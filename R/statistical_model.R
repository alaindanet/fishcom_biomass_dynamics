#' Get linear model station
#'
#'
get_lm_station <- function (.data = NULL, var_names = NULL, rhs = NULL) {

  if (! "data" %in% colnames(.data)) {
    .data %<>%
      dplyr::group_by(station) %>%
      tidyr::nest()
  }

  # Linear model
  for (i in seq_along(var_names)) {
  
  .data[[var_names[i]]] <-
    purrr::map(.data$data, compute_linear_model, formulas = paste0(var_names[i], rhs))
  
  }
  # Coefficients

  return(ungroup(.data))
}

#' Compute a linear model 
compute_linear_model <- function (.data = NULL, formulas = NULL) {
  
  model <- lm(
    formula = as.formula(formulas),
    data = .data
  )
  return(model)
} 

get_lm_coeff <- function(.data = NULL, col_names = NULL) {
  
  output <- .data
  if ("station" %in% names(.data)) {
    output %<>% select(station)
  }

  for (i in seq_along(col_names)) {
    output[[col_names[i]]] <- purrr::map(.data[[col_names[i]]], broom::tidy) 
  }
  output %<>%
    tidyr::pivot_longer(cols = all_of(col_names), names_to = "coeff", values_to = "value") %>%
    tidyr::unnest(cols = value)

  output$term <- stringr::str_replace_all(output$term, c("[//(//)]" = "",
      "nb_year" = "slope")
    ) %>% 
  tolower

  return(output)
  
}

extract_xy_slope_from_coeff <- function (y = NULL, x = NULL, .data = NULL) {

  prep_data <- .data %>%
    dplyr::filter(
      coeff %in% c(rlang::as_string(sym(x)), rlang::as_string(sym(y))),
      term == "slope") %>%
    dplyr::mutate(rel_std.error = std.error / abs(estimate)) %>%
    dplyr::select(station, coeff, estimate, rel_std.error) %>%
    tidyr::pivot_wider(names_from = coeff, names_sep = "_", values_from = c(estimate, rel_std.error)) %>%
    dplyr::mutate(weight = 1 / (.data[[paste0("rel_std.error_", x)]] * .data[[paste0("rel_std.error_", y)]]))

  colnames(prep_data) %<>% 
    str_replace_all(., pattern = "estimate_", "")

  output <- prep_data[, names(prep_data) %in% c("station", x, y , "weight")] 
    
  return(output)
}

compute_lm_temporal_trends <- function (.data = NULL, y = "biomass", x = "connectance", group = NULL) {
  
  group_var <- enquo(group)
  group_var_chr <- quo_name(group_var)


  if (rlang::quo_is_null(group_var)) {
    output <- list(y = y, x = x)
  } else {
    output <- list(y = y, x = x, group = unique(.data[[group_var_chr]]))
  }
  output <- 
    output %>%
    expand.grid() %>%
    mutate_all(as.character) %>%
    distinct() %>%
    filter_at(vars(x, y), any_vars((. != x))) %>%
    as_tibble

  if (rlang::quo_is_null(group_var)) {
    output$data <- purrr::map2(output$y, output$x, extract_xy_slope_from_coeff, .data = .data)
  } else {
    output$data <- purrr::pmap(
      list(output$y, output$x, output$group),
      ~extract_xy_slope_from_coeff(y = ..1, x = ..2, .data = filter(.data, !!group_var == ..3)))
  }

  output %<>%
    mutate(
      formulas = paste(y, "~", x ),
      model = purrr::map2(data, formulas, ~lm(formula = .y, weights = weight, data =.x))
      ) %>%
    select(-formulas)
  return(output)
}

#' Linear models bm versus network 

compute_my_lm_vs_net_model <- function (
  .df = NULL,
  var_to_group = c("variable", "protocol_type"),
  var_to_scale = NULL
  ) {

  .df %<>%
    mutate(
      increasing = linear_slope > 0,
      inc_f = as.factor(increasing),
      log_bm = log(bm)
    ) 

  if (!is.null(var_to_scale)) {
    .df %<>%
      mutate_at(vars(all_of(var_to_scale)), scale)
  }
  
  .df %<>%
  group_by_at(vars(all_of(var_to_group))) %>%
  nest() %>%
    mutate(
      #mod_all_group = map(data, ~lm(linear_slope ~ bm_slope*inc_f*bm_group, .x, weights = reg_weight)),
      #mod_medium_group = map(data, ~lm(
	   #linear_slope~ bm_slope + bm_slope:inc_f + bm_slope:bm_group +
	    #bm_slope:inc_f:bm_group,
	  #.x, weights = reg_weight)),
     mod_medium_bm = map(data, ~lm(
	   linear_slope ~ bm_slope + bm + bm_slope:inc_f + bm_slope:bm +
	    bm_slope:inc_f:bm,
	  .x, weights = reg_weight)),
      #mod_min_bm = map(data, ~lm(
	  #linear_slope ~ bm_slope:inc_f + bm_slope:bm +
	    #bm_slope:inc_f:bm,
	  #.x, weights = reg_weight)),
      mod_all_bm = map(data, ~lm(linear_slope ~ bm_slope*inc_f*bm, .x, weights = reg_weight)),
      #mod_all_log_bm = map(data, ~lm(linear_slope ~ bm_slope*inc_f*log_bm, .x, weights = reg_weight)),
      #mod_bm = map(data, ~lm(linear_slope ~ bm_slope*bm, .x, weights = reg_weight)),
      mod_bm_quad = map(data, ~lm(linear_slope ~ bm_slope*bm + I(bm_slope^2), .x, weights = reg_weight)),
      mod_bm_quad2 = map(data, ~lm(linear_slope ~ bm_slope*bm + I(bm_slope^2)*bm, .x, weights = reg_weight)),
      #mod_log_bm_quad = map(data, ~lm(linear_slope ~ bm_slope*log_bm + I(bm_slope^2), .x, weights = reg_weight)),
      #mod_log_bm_quad2 = map(data, ~lm(linear_slope ~ bm_slope*log_bm + I(bm_slope^2)*log_bm, .x, weights = reg_weight)),
      #mod_bm_inc = map(data, ~lm(linear_slope ~ bm_slope*bm + inc_f, .x, weights = reg_weight)),
      #mod_log_bm = map(data, ~lm(linear_slope ~ bm_slope*log_bm, .x, weights = reg_weight)),
      #mod_log_bm_inc = map(data, ~lm(linear_slope ~ bm_slope*log_bm + inc_f, .x, weights = reg_weight)),
      #mod_inc = map(data, ~lm(linear_slope ~ bm_slope*inc_f, .x, weights = reg_weight)),
      #mod_inc_bm = map(data, ~lm(linear_slope ~ bm_slope*inc_f + bm, .x, weights = reg_weight)),
      #mod_inc_log_bm = map(data, ~lm(linear_slope ~ bm_slope*inc_f + log_bm, .x, weights = reg_weight)),
    )

  return(ungroup(.df))
}

model_summary <- function (.df) {

  model_var <- tidyselect::vars_select(names(.df), starts_with("mod"))

  .df %<>%
    select(-data) %>%
    pivot_longer(cols = c(all_of(model_var)),
      names_to = "model", values_to = "model_obj") %>%
    mutate(
      resume = map(model_obj, summary),
      anova = map(model_obj, anova)
    )

  return(.df)

}

get_mod_pred <- function (summary_mod = NULL) {
  summary_mod %>%
    mutate(
      term = map(model_obj,
	function (x) {
	  term <- attr(x$terms , "term.labels") %>%
	    .[. %in% get_model_terms()]
	  paste0(term, c(rep("", length(term) - 1), " [quart2]"))
	}
      ),
      pred = map2(model_obj, term, ~ggpredict(.x,
	  terms = .y)),
      pred_df = map(pred, as_tibble),
      plot = map(pred, ~plot(.x, add.data = FALSE))
    )
}


pval2lgl <- function (x = NULL, thld = 0.05) {

  x <- ifelse(x <= thld, TRUE, FALSE)
  return(as.logical(x))
}

make_hyp_table <- function (hyp_table = NULL, mod = NULL) {

  combin <- expand.grid(
    variable = get_com_str_var(),
    mod_bm_quad2 = unique(hyp_table$mod_bm_quad2), 
    stat = c("estimate", "p.value")) %>%
  left_join(select(hyp_table, 2:3)) %>%
  pivot_longer(
    cols = c(mod_bm_quad2, mod_medium_bm),
    names_to = "model", values_to = "term"
    ) %>%
  mutate_all(as.character)

  comparison_table <- distinct(combin) %>%
    mutate(
      coeff = pmap_dbl(list(variable, stat, model, term),
	~get_coef_from_terms(mydata = mod,
	  variable = ..1, model = ..3, term = ..4, type = ..2
	  ))) %>%
  arrange(variable)

test2 <- comparison_table %>%
  left_join(
    pivot_longer(hyp_table, cols = c(mod_bm_quad2, mod_medium_bm),
      names_to = "model", values_to = "term")) %>%
  filter(stat == "p.value") %>%
  select(variable:model, coeff, effect) %>%
  pivot_wider(names_from = stat, values_from = coeff) %>%
  mutate(p.value = pval2lgl(p.value, thld = 0.05)) %>%
  pivot_wider(names_from = model, values_from = c(p.value)) 

return(test2)
}
