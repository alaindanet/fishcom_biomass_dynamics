#' Add number of year since start
#'
#' @param .data a df e.g. generated by get_full_data()
add_nb_year_station <- function (.data = NULL) {
  
  output <- .data %>%
    group_by(station) %>%
    mutate(nb_year = year - min(year)) %>%
    ungroup()
  return(output)
}


#' Add biomass value, relative to the first year 
#'
#' @param .data a df e.g. generated by get_full_data()
add_relative_biomass_to_start <- function (.data = NULL) {
  
  output <- .data %>%
    dplyr::group_by(station) %>%
    dplyr::arrange(nb_year) %>%
    dplyr::mutate(rel_bm = (biomass - first(biomass)) / first(biomass)) 
  
  if ("log_bm" %in% names(output)) {

    output %<>% 
      dplyr::mutate(rel_log_bm = (log_bm - first(log_bm)) / first(log_bm))
  
  }

  return(ungroup(output))
}

#' Compute derivate variables
#'
#' @inheritParams add_relative_biomass_to_start 
add_to_full_data <- function (.data = NULL) {

  output <- add_nb_year_station(.data)
  output <- output %>%
    mutate(
      log_bm = log(biomass),
      log_rich = log(richness),
      bm_std = biomass / surface,
      rich_std = richness / surface,
      log_bm_std = log(bm_std),
      log_rich_std = log(rich_std),
      nbnode_std = nbnode / surface,
      log_nbnode_std = log(nbnode_std),
      nb_pisc_node_std = nb_pisc_node / surface,
      nb_non_pisc_node_std = nb_non_pisc_node / surface,
      log_nb_pisc_node_std = log(nb_pisc_node_std),
      nb_pisc_rich_std = nb_pisc_rich / surface,
      nb_non_pisc_rich_std = nb_non_pisc_rich / surface,
      log_nb_pisc_rich_std = log(nb_pisc_rich_std)
    )
  output <- add_relative_biomass_to_start(output)

  return(output)
}

#' Add group to data according to station
#'
#'
add_group_station <- function (.data = NULL, group = NULL) {

  output <- .data %>%
    left_join(group, by = "station")

  return(output)

}

get_bm_vs_network_trends <- function (classif = NULL, bm_var = NULL,
  coeff = c("connectance", "w_trph_lvl_avg", "richness", "weighted_connectance")
  ) {

  var_to_keep <- c("station", "linear_slope", "linear_slope_strd_error")

  bm <- classif[classif$variable == bm_var,] %>%
    unnest(classif) %>%
    select(!!!var_to_keep) %>%
    rename(bm_slope = linear_slope, bm_slope_strd_error = linear_slope_strd_error)

  network <- classif %>%
    filter(!variable %in% get_biomass_var()) %>%
    unnest(classif) %>%
    select(!!!c("variable", var_to_keep))

  output <- bm %>%
    left_join(network, by = "station")

  # Add regression weight
  output %<>%
    mutate( reg_weight = (1 / bm_slope_strd_error +
	1 / linear_slope_strd_error) / 2)

  return(output)
}

get_rich_vs_network_trends <- function (classif = NULL, rich_var = NULL,
  coeff = c("connectance", "w_trph_lvl_avg", "weighted_connectance")
  ) {

  var_to_keep <- c("station", "linear_slope", "linear_slope_strd_error")

  rich <- classif[classif$variable == rich_var,] %>%
    unnest(classif) %>%
    select(!!!var_to_keep) %>%
    rename(rich_slope = linear_slope, rich_slope_strd_error = linear_slope_strd_error)

  network <- classif %>%
    filter(!variable %in% get_richness_var()) %>%
    unnest(classif) %>%
    select(!!!c("variable", var_to_keep))

  output <- rich %>%
    left_join(network, by = "station")

  # Add regression weight
  output %<>%
    mutate( reg_weight = (1 / rich_slope_strd_error +
	1 / linear_slope_strd_error) / 2)

  return(output)
}

get_y_versus_x_trends <- function(
  classif = NULL,
  x_var = "log_rich_std",
  y_var = c("connectance", "w_trph_lvl_avg", "weighted_connectance")) {

  var_to_keep <- c("station", "linear_slope", "linear_slope_strd_error")

  x_data <- classif[classif$variable == x_var,] %>%
    unnest(classif) %>%
    select(!!!var_to_keep)
  
  # Rename according to the variable
  colnames(x_data)[colnames(x_data) %in% var_to_keep[2:3]] <- paste0(x_var, c("_slope", "_strd_error")) 

  #return(x_data)

  y_data <- classif %>%
    filter(variable %in% y_var) %>%
    unnest(classif) %>%
    select(!!!c("variable", var_to_keep))

  output <- x_data %>%
    left_join(y_data, by = "station")

  # Add regression weight
  output %<>%
    mutate(reg_weight = (1 / !!sym(paste0(x_var, "_strd_error")) +
	1 / linear_slope_strd_error) / 2)

  return(output)

} 

get_station_biomass_summary <- function (.data = NULL, bm_var = c("bm_std", "log_bm_std")) {

  var_to_keep <- c("station", "nb_year", bm_var)
  # Prepare
  .data %<>%
    select(!!!var_to_keep) %>%
    pivot_longer(cols = -c(station, nb_year), names_to = "bm_var", values_to = "bm") %>%
    group_by(station, bm_var)

  # Summarise and make it tidy
  bm_summary <- .data %>% 
    summarise(
      first_year = bm[nb_year == 0],
      last_year = bm[nb_year == max(nb_year)],
      first_3_year = mean(bm[nb_year %in% c(0:2)], na.rm = TRUE),
      last_3_year = mean(bm[nb_year %in% c(max(nb_year) - 2:max(nb_year))], na.rm = TRUE),
      median = median(bm, na.rm = TRUE),
      avg = mean(bm, na.rm = TRUE)) %>%
    pivot_longer(cols = c(first_year:avg), names_to = "summary_type", values_to = "bm")

  # Make station group and make it tidy
  bm_group <- bm_summary %>%
    group_by(bm_var, summary_type) %>%
    mutate(
      median = ifelse(bm < median(bm, na.rm = TRUE), "little", "big"),
      avg = ifelse(bm < mean(bm, na.rm = TRUE), "little", "big"),
      quartile = map_chr(bm, function (x, y) {
	if (is.na(x)) return(NA)

	if (x <= y[1])
	  output <- "first_quartile"
	else if (x > y[1] & x <= y[2])
	  output <- "second_quartile"
	else if (x > y[2] & x <= y[3])
	  output <- "third_quartile"
	else if (x > y[3])
	  output <- "fourth_quartile"
	else
	  output <- NA
	return(output)
}, y = quantile(bm, probs = c(.25, .50, .75), na.rm = TRUE)
)) %>%
    pivot_longer(cols = c(median:quartile), names_to = "group_type", values_to = "group") %>%
    ungroup()


}

get_station_com_summary <- function (.data = NULL, myvar = c("bm_std",
    "log_bm_std"), var_cat = "bm",
  summary_type = "all", group = TRUE) {

  stopifnot(summary_type %in% c("all", "median", "first_3_year"))

  var_to_keep <- c("station", "nb_year", myvar)
  var_cat_sym <- rlang::sym(var_cat)
  cat_var <- paste0(var_cat, "_var")
  cat_var_sym <- rlang::sym(cat_var)
  # Prepare
  .data %<>%
    select(!!!var_to_keep) %>%
    pivot_longer(cols = -c(station, nb_year),
      names_to = cat_var,
      values_to = var_cat) %>%
    group_by(station, !!cat_var_sym)

  
  nb_year_sym <- rlang::sym("nb_year")
  # Summarise and make it tidy

  if (summary_type == "all") {
  bm_summary <- .data %>% 
    summarise(
      first_year = (!!var_cat_sym)[!!nb_year_sym == 0],
      last_year = (!!var_cat_sym)[!!nb_year_sym == max(!!nb_year_sym)],
      first_3_year = mean((!!var_cat_sym)[!!nb_year_sym %in% c(0:2)], na.rm = TRUE),
      last_3_year = mean((!!var_cat_sym)[!!nb_year_sym %in% c(max(!!nb_year_sym) - 2:max(!!nb_year_sym))], na.rm = TRUE),
      median = median(!!var_cat_sym, na.rm = TRUE),
      avg = mean(!!var_cat_sym, na.rm = TRUE)) %>%
    pivot_longer(cols = c(first_year:avg),
      names_to = "summary_type",
      values_to = var_cat)
  } else if (summary_type == "median") {
    bm_summary <- .data %>% 
      summarise(
	median = median(!!var_cat_sym, na.rm = TRUE)
	)
  
  } else if (summary_type == "first_3_year") {
    bm_summary <- .data %>% 
      summarise(
	first_3_year = mean((!!var_cat_sym)[!!nb_year_sym %in% c(0:2)], na.rm = TRUE)
      )
  }

  if (group) {
  # Make station group and make it tidy
  bm_summary %<>%
    group_by(!!cat_var_sym, summary_type) %>%
    mutate(
      median = ifelse(!!var_cat_sym < median(!!var_cat_sym, na.rm = TRUE), "little", "big"),
      avg = ifelse(!!var_cat_sym < mean(!!var_cat_sym, na.rm = TRUE), "little", "big"),
      quartile = map_chr(!!var_cat_sym, function (x, y) {
	if (is.na(x)) return(NA)

	if (x <= y[1])
	  output <- "first_quartile"
	else if (x > y[1] & x <= y[2])
	  output <- "second_quartile"
	else if (x > y[2] & x <= y[3])
	  output <- "third_quartile"
	else if (x > y[3])
	  output <- "fourth_quartile"
	else
	  output <- NA
	return(output)
}, y = quantile(!!var_cat_sym, probs = c(.25, .50, .75), na.rm = TRUE)
)) %>%
    pivot_longer(cols = c(median:quartile), names_to = "group_type", values_to = "group") %>%
    ungroup()
  }

  return(bm_summary)
}

add_stream_bm_caract_to_model <- function (
  bm_vs_network_df = NULL,
  stream_caract = NULL,
  bm_caract = NULL, 
  bm_group_var = "bm_std",
  bm_summary_type = NULL,
  group_type_caract = NULL 
  ) {

  if (!is.null(stream_caract)) {

  stream_caract %<>%
    filter(group_type == group_type_caract) %>%
    rename(stream_group = group) %>%
    select(-group_type)

    bm_vs_network_df %<>%
    left_join(stream_caract, by = "station")
  }


  stopifnot(any(!is.null(bm_summary_type), !is.null(bm_summary_type)))
  if (!is.null(bm_summary_type) & !is.null(bm_summary_type)) {

    bm_caract %<>%
      filter(
	group_type == group_type_caract,
	bm_var == bm_group_var,
	summary_type == bm_summary_type
	) %>%
    rename(bm_group = group) %>%
    select(-bm_var, -summary_type, -group_type, -bm_var)
  }

  output <- bm_vs_network_df %>% 
    left_join(bm_caract, by = "station")
    
  return(output)
}

get_st_mono_trends <- function(.df = NULL, xvar = NULL, stable = FALSE) {

  if (!stable) {
    shp_filter <- c("increase_constant", "decrease_constant")
  } else {
    shp_filter <- c("increase_constant", "stable_constant", "decrease_constant")
  }

  output <- .df %>%
    unnest(classif) %>%
    filter(
      variable == xvar,
      shape_class %in% shp_filter
    ) %>%
    select(station, shape_class)

  return(output)

}

get_st_all_trends <- function(.df = NULL, xvar = NULL, stable = FALSE) {

  if (!stable) {
    dir_filter <- c("increase", "decrease")
  } else {
    dir_filter <- c("increase", "stable", "decrease")
  }

  output <- .df %>%
    unnest(classif) %>%
    filter(
      variable == xvar,
      direction %in% dir_filter
      ) %>%
    select(station, shape_class)

  return(output)
}


get_slope_x_bound <- function(classif = NULL, st_mono = NULL, type = "min_max") {
  
   classif %<>%
    filter(variable %in% st_mono$variable) %>%
    left_join(st_mono, by = "variable") %>%
    mutate(
      classif = map2(classif, station, ~.x %>% filter(station %in% .y)),
      min_max = map2(classif, variable,
	~c(
	  min(.x[["linear_slope"]], na.rm = TRUE), max(.x[[ "linear_slope"]], na.rm = TRUE
      )
	)
	),
    )

    output <- classif[[type]]
    names(output) <- classif[["variable"]]

    return(output)
}

#' Get VIF

get_vif <- function (.df = NULL, model_cols = c("mod1", "mod2", "mod3", "mod5")) {

  .df %<>% 
    select(-data) %>%
    pivot_longer(cols = all_of(model_cols), names_to = "model_type", values_to = "model") %>%
    mutate(
      vif_tmp = map(model, ~try(car::vif(.x))),
      vif = map(vif_tmp, ~try(enframe(.x, name = "term", value = "vif")))
    )

  .df %<>%
    mutate(
      check = map2_lgl(vif, vif_tmp,
	~all(class(.x) != "try-error" & class(.y) != "try-error"))) %>%
    filter(check) %>%
    select(-check)

  .df %<>% 
    filter(!any(class(vif) %in% "try-error")) %>%
    unnest(cols = vif)
  return(.df)

}

#' get regression table from model summary
#'
#' @param .data
#' @param model chr name of a model 
#' @param protocol chr 
#' @return list of length two
get_table_from_summary <- function (
  .data = NULL,
  model = NULL,
  protocol = NULL,
  variable = NULL
  ) {

  model_chr <- model 
  variable_chr <- variable 

  if (is.null(protocol)) {
    .data %<>% filter(model == model_chr)
  } else {
    .data %<>% filter(model == model_chr, protocol_type == protocol)
  }

  if (!is.null(variable_chr)) {
    .data %<>% filter(variable %in% variable_chr)
  }

  .data %<>%
    mutate(
      reg_table = map(model_obj, broom::tidy),
      anova_table = map(anova, broom::tidy)
    )

  nb_coeff <- length(coefficients(.data$model_obj[[1]]))
  nb_coeff <- length(coefficients(.data$model_obj[[1]]))

  make_summary_table <- function (.df, type, nb_rep) {
      .df[[type]] %>% 
	bind_rows(.) %>%
	mutate(variable = rep(.df[["variable"]], each = nb_rep)) %>%
	select(variable, everything())
    }

  tables <- c("reg_table", "anova_table")

  output <- purrr::map(tables,
  ~make_summary_table(.df = .data, nb_rep = nb_coeff, type = .x)
  )
  names(output) <- tables

  return(output)
} 

get_coef_from_terms <- function (
  mydata = NULL,
  variable = NULL,
  model = NULL,
  term = NULL, type = "coeff") {

  var_chr <- variable
  term_chr <- term 

  mydata %<>% filter(variable == var_chr)

  ml <- mydata[mydata$variable == var_chr, ][[model]][[1]]
  ml_summary <- broom::tidy(ml)

  vec <- deframe(ml_summary[, c("term", type)])
  vec[term_chr]
  
}


#' Misc

merge.all <- function(x, y) {
  dplyr::left_join(x, y, by="station")
}
