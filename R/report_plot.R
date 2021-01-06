plot_bm_vs_bm_slope <- function (tps_dyn_coef = NULL, bm_group = NULL) {

  tps_dyn_coef %>%
    filter(term %in% "slope", coeff %in% c("bm_std", "log_bm_std")) %>%
    select(station:estimate) %>%
    pivot_wider(names_from = "term", values_from = estimate) %>%
    left_join(bm_group, by = c("station", "coeff" = "bm_var")) %>%
    filter(group_type == "median", summary_type %in% c("first_3_year", "median")) %>%
    ggplot(aes(x = bm, y = slope)) +
    geom_point(aes(color = group)) +
    facet_wrap(summary_type~coeff, scales = "free") +
    labs(x = "Summary of biomass (g)", y = "Slope (g.y^-1)")

}

plot_hist_med_bm <- function(.data = full_data2) {

  .data %>%
    group_by(station) %>%
    summarise(med_bm = median(biomass)) %>%
    ggplot(aes(x = med_bm)) +
    geom_histogram() +
    labs(x = "Median total biomass over time by station (g)")
}

get_pred_model <- function (
  summary_model = NULL,
  model_data = NULL,
  model_name = NULL,
  term = NULL,
  selected_variable = get_com_str_var()
  ) {

  stopifnot(model_name %in% c("mod_bm_quad2", "mod_medium_bm"))

  if (is.null(term)) {
    if (model_name == "mod_bm_quad2") {
      term <- c("bm_slope", "bm [quart2]")
    }
    if (model_name == "mod_medium_bm") {
      term <- c("bm_slope [-0.2,-0.00001,0.00001,.2]", "inc_f", "bm [quart2]")
    }
    stopifnot(!is.null(term))
  }

  pred <- summary_model %>%
    filter(model == model_name, variable %in% selected_variable) %>%
    mutate(
      pred = map(model_obj, ~ggpredict(.x, terms = term)),
      pred_df = map(pred, as_tibble)
    )

  model_data %<>% filter(variable %in% selected_variable)

  if (model_name == "mod_medium_bm") {
    pred_plot <- map2(model_data$data, pred$pred_df,
      ~plot_final_model(ggpred = .y, rawdata = .x))
  } else if (model_name == "mod_bm_quad2") {
    pred_plot <- map2(model_data$data, pred$pred_df,
      plot_pred_data)
  }

  names(pred_plot) <- pred$variable 
  return(pred_plot)

}

get_model_plot_from_signif_term <- function (
  anova_table = NULL,
  model = NULL 
  ) {

  term_model <- anova_table %>%
    mutate(signf = p.value <= 0.05) %>%
    filter(signf) %>%
    #select(variable, term) %>%
    group_by(variable) %>%
    summarise( signif_term = paste(term, collapse = "+"),
      indiv_term = list(term)) %>%
    mutate( single_var_term = map(indiv_term, get_mod_single_term),
      predict_term = map(single_var_term, from_term_to_predict_term))

    model_test <- model %>%
      filter(variable %in% term_model$variable) %>%
      select(variable, data) %>%
      left_join(term_model, by = "variable")%>%
      mutate(model_formula = paste0("linear_slope ~", signif_term)) %>%
      mutate(
	mod = map2(data, model_formula,
	  ~try(compute_linear_model(formulas = .y, .data = .x)))
      )
      model_test %<>%
	mutate(pred = pmap(list(mod, predict_term),
	    function (model, pred_term){
	      ggpredict(model, terms = pred_term) %>% as_tibble
	  }),
	  myplot = pmap(list(pred, data),
	    function (pred, .data) {
	    try(
	      plot_final_model(ggpred = pred,
		rawdata = .data, std_error = TRUE))
	    })
	)
    return(model_test)

} 
