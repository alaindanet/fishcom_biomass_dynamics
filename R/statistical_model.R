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
    purrr::map(.data$data, ~try(compute_linear_model(.x, formulas = paste0(var_names[i], rhs))))
  
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
  var_to_scale = NULL,
  x = "bm_slope",
  covar = NULL
  ) {

  x_sym <- x

  # Extract var name of x slope 
  mask <- str_detect(colnames(.df), "slope") &
    !colnames(.df) %in% c("linear_slope", "linear_slope_strd_error")
  x_slope_var <- colnames(.df)[mask]
  x_slope_var_sym <- sym(x_slope_var)
  x_var <- str_remove(x_slope_var, "_slope")
  x_var_sym <- sym(x_var) #needed for tidy selection 

  # 
  .df %<>%
    mutate(
      increasing := {{x_slope_var_sym}} > 0,
      inc_f = as.factor(increasing),
      #"log_{{x_var_sym}}" := log({{x_var_sym}})
    ) 

  #return(.df)

  if (!is.null(var_to_scale)) {
    .df %<>%
      mutate_at(vars(all_of(var_to_scale)), scale)
  }
  

  # Define formulas:

  if (is.null(covar)) {
    # Covariable is the x raw variable, so the formula is basically:
    #y ~ x_slope + x
    covar <- x_var
  } 
  formula_medium <- 
    paste0("linear_slope ~ ", x_slope_var, " + ", covar," + ", x_slope_var,
      ":inc_f + ",x_slope_var, ":", covar, " + ", x_slope_var,":inc_f:", covar)
  formula_all <- paste0("linear_slope ~ ", x_slope_var, "* inc_f *", covar)

  .df %<>%
  group_by_at(vars(all_of(var_to_group))) %>%
  nest() %>%
    mutate(
     mod_medium_bm = map(data, ~lm(as.formula(formula_medium), .x, weights = reg_weight)),
     mod_all_bm = map(data, ~lm(as.formula(formula_all), .x, weights = reg_weight)),
      #mod_bm_quad = map(data, ~lm(as.formula(paste("linear_slope ~ ", !!(x_sym), "*bm + I(", !!(x_sym), "^2)")), .x, weights = reg_weight)),
      #mod_bm_quad2 = map(data, ~lm(as.formula(paste("linear_slope ~ ", !!(x_sym),"*bm + I(", !!(x_sym), "^2)*bm")), .x, weights = reg_weight))
    )

  return(ungroup(.df))
}

get_model_summary <- function (.df) {

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

get_model_bm_rich_no_covar_no_inc <- function (.data = NULL) {

  list(
    log_bm_std = lm(
      log_bm_std ~ log_rich_std,
      data = {{.data}},
      weight = 1 / (log_rich_std_strd_error + log_bm_std_strd_error)
      ),
    log_rich_std = lm(
      log_rich_std ~ log_bm_std,
      data = {{.data}},
      weight = 1 / (log_rich_std_strd_error + log_bm_std_strd_error)
      ),
    ct_ff = lm(
      ct_ff ~ log_rich_std  + log_bm_std,
      data = {{.data}},
      weight = 1 / (log_rich_std_strd_error + log_bm_std_strd_error + ct_ff_strd_error)
      ),
    w_trph_lvl_avg = lm(
      w_trph_lvl_avg ~  log_rich_std + log_bm_std,
      data = {{.data}},
      weight = 1 / (log_rich_std_strd_error + log_bm_std_strd_error + w_trph_lvl_avg_strd_error)
      ),
    piel_bm = lm(
      piel_bm ~ log_rich_std + log_bm_std,
      data = {{.data}},
      weight = 1 / (log_rich_std_strd_error + log_bm_std_strd_error + piel_bm_strd_error)),
    piel_nind = lm(
      piel_nind ~ log_rich_std + log_bm_std,
      data = {{.data}},
      weight = 1 / (log_rich_std_strd_error + log_bm_std_strd_error + piel_nind_strd_error))
  )

}

get_model_bm_rich_int <- function (.data = NULL) {

  list(
    log_bm_std = lm(
      log_bm_std ~ log_rich_std,
      data = {{.data}},
      weight = 1 / log_rich_std_strd_error * log_bm_std_strd_error),
    log_rich_std = lm(
      log_rich_std ~ log_bm_std,
      data = {{.data}},
      weight = 1 / log_rich_std_strd_error * log_bm_std_strd_error),
    ct_ff = lm(
      ct_ff ~ log_rich_std  * log_bm_std,
      data = {{.data}},
      weight = 1 / log_rich_std_strd_error * log_bm_std_strd_error * ct_ff_strd_error),
    w_trph_lvl_avg = lm(
      w_trph_lvl_avg ~  log_rich_std * log_bm_std,
      data = {{.data}},
      weight = 1 / log_rich_std_strd_error * log_bm_std_strd_error * w_trph_lvl_avg_strd_error),
    piel_bm = lm(
      piel_bm ~ log_rich_std * log_bm_std,
      data = {{.data}},
      weight = 1 / log_rich_std_strd_error * log_bm_std_strd_error * piel_bm_strd_error),
    piel_nind = lm(
      piel_nind ~ log_rich_std * log_bm_std,
      data = {{.data}},
      weight = 1 / log_rich_std_strd_error * log_bm_std_strd_error * piel_nind_strd_error)
  )

}

#' Compute spatial relationships

get_mod_list <- function(.data = NULL, st_by_var = NULL) {
  rlang::eval_tidy(rlang::quo(
      list(
        log_bm_std = nlme::lme(log_bm_std ~ log_RC1 + log_RC2 + log_rich_std,
          random = ~ 1 | basin, data = {{.data}}),

        log_rich_std = nlme::lme(log_rich_std ~ log_RC1 + log_RC2 + log_bm_std,
          random = ~ 1 | basin, data = {{.data}}),

        ct_ff = nlme::lme(ct_ff ~ log_RC1 + log_RC2 + log_rich_std + log_bm_std,
          random = ~ 1 | basin, data = {{.data}}),

        w_trph_lvl_avg = nlme::lme(
          w_trph_lvl_avg ~ log_RC1 + log_RC2 + log_rich_std + log_bm_std,
          random = ~ 1 | basin, data = {{.data}}),

        piel_bm = nlme::lme(
          piel_bm ~ log_RC1 + log_RC2 + log_rich_std + log_bm_std,
          random = ~ 1 | basin, data = {{.data}}),

        piel_nind = nlme::lme(
          piel_nind ~ log_RC1 + log_RC2 + log_rich_std + log_bm_std,
          random = ~ 1 | basin, data = {{.data}})
      )
      ))
}

get_mod_list_lme4 <- function(.data = NULL) {
  rlang::eval_tidy(rlang::quo(
      list(
        log_bm_std = lme4::lmer(
          log_bm_std ~ log_RC1 + log_RC2 + log_rich_std + (1 | basin),
          data = {{.data}}),

        log_rich_std = lme4::lmer(
          log_rich_std ~ log_RC1 + log_RC2 + log_bm_std + (1 | basin),
          data = {{.data}}),

        ct_ff = lme4::lmer(
          ct_ff ~ log_RC1 + log_RC2 + log_rich_std + log_bm_std + (1 | basin),
          data = {{.data}}),

        w_trph_lvl_avg = lme4::lmer(
          w_trph_lvl_avg ~ log_RC1 + log_RC2 + log_rich_std + log_bm_std + (1 | basin),
          data = {{.data}}),

        piel_bm = lme4::lmer(
          piel_bm ~ log_RC1 + log_RC2 + log_rich_std + log_bm_std + (1 | basin),
          data = {{.data}}),

        piel_nind = lme4::lmer(
          piel_nind ~ log_RC1 + log_RC2 + log_rich_std + log_bm_std +
          (1 | basin),
          data = {{.data}})
      )
      ))
}

stdCoef.merMod <- function(object) {
  sdy <- sd(lme4::getME(object, "y"))
  sdx <- apply(lme4::getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx / sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx / sdy
  return(data.frame(std_estimate = sc, stdse = se))
}

stdse.lm <- function(model) {

  ti <- get_data_from_lm_model(model)
  x <- attr(model$terms, "term.labels")
  y <- names(attr(model$terms, "dataClasses")) %>%
    .[! . %in% c("(weights)", x)]
  sdx <- apply(ti[, colnames(ti) %in% x], 2, sd, na.rm = T)
  sdy <- sd(ti[[y]], na.rm = T)
  coef.fixef <- coef(summary(model))[!row.names(coef(summary(model))) %in% "(Intercept)", "Estimate"] * sdx / sdy
  se.fixef <- coef(summary(model))[!row.names(coef(summary(model))) %in% "(Intercept)", "Std. Error"] * sdx / sdy
  #TODO: recompute std coef
  return(data.frame(std_estimate = coef.fixef, stdse = se.fixef))

}

#' Compute PCA

get_pca_environment <- function (.data = data_for_pca) {

  x <- as.data.frame(.data[, colnames(.data) != "station"])
  row.names(x) <- .data$station
  # Perform PCA
  pca <- compute_rotated_pca(x)
  # Inverse the second axis to have a increasing gradient of temperature 
  pca$rotated$loadings[, 2] <- pca$rotated$loadings[, 2] * (-1) 
  pca$rotated$scores[, 1] <- pca$rotated$scores[, 1] * -1

  return(pca)
}

compute_rotated_pca <- function(.data = NULL, naxis = 2) {
      .data %<>% na.omit
      pca <- ade4::dudi.pca(as.data.frame(.data), scannf = FALSE, nf = naxis, center = TRUE, scale = TRUE)
      pca_rotated <- psych::principal(.data, rotate="varimax", nfactors = naxis,
	scores=TRUE)

      return(list(normal = pca, rotated = pca_rotated))
}


#' Compute plots from pca_rotated 
#'
#' @param pca_rotated list. The output of compute_rotated_pca.
#' @param axis integer vector of length 2 (x and y resp.).
#'
plot_rotated_pca <- function (pca_rotated = NULL, axis = c(1,2)) {

 {
   adegraphics::adegpar()$plabels
   ade4::s.corcircle(
    pca_rotated$rotated$loadings[1:nrow(pca_rotated$rotated$loadings),], xax = axis[1],
    yax = axis[2], box = TRUE)
  mtext(paste0("RC ", axis[1]), side = 1, adj = .5, line = -1)
  mtext(paste0("RC ", axis[2]), side = 2, adj = .5, line = -5)
  rotated_plot <- grDevices::recordPlot()
 }
  pca_plot <- factoextra::fviz_pca_var(pca_rotated$normal,
    axes = axis,
    col.var = "contrib", # Color by contributions to the PC
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE # Avoid text overlapping
  )
  return(list(rotated = rotated_plot, normal = pca_plot))
}
