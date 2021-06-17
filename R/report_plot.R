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

get_predict_from_signif <- function (mod_summary = NULL) {

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

plot_matrix_bm_rich_cross_classif <- function (classif = NULL) {

  ti <- classif %>%
    filter(variable %in% c("log_bm_std", "log_rich_std")) %>%
    unnest(classif) %>%
    select(variable, station, shape_class) %>%
    pivot_wider(names_from = variable, values_from = shape_class)

  ti2 <- ti %>% 
    group_by(log_bm_std, log_rich_std) %>%
    summarise(n = n())

  ggplot(data = ti2, aes(x=log_bm_std, y = log_rich_std, fill=log(n))) + 
    geom_tile() +
    viridis::scale_fill_viridis() +
    geom_text(aes(x=log_bm_std, y = log_rich_std, label = n), color = "black", size = 10)

}

get_plot_fig1_2 <- function(
  predict_plot = predict_plot2,
  get_list = FALSE,
  rm_legend = FALSE,
  bm_x = "log_bm_std",
  bm_y = c(get_com_str_var(), "log_rich_std", "piel_nind", "piel_bm"),
  rich_y = c(get_com_str_var(), "log_bm_std", "piel_nind", "piel_bm"),
  rich_x = "log_rich_std"
  ) {

  pred_plot_signif_bm <- predict_plot %>%
    filter(y %in% bm_y, !y %in% bm_x, x == bm_x)

  pred_plot_signif_rich <- predict_plot %>%
    filter(y %in% rich_y, x == rich_x) %>%
    filter(!y %in% rich_x)

  pred_plot_other <- predict_plot %>%
    filter(
      y %in% c("ct_ff", "w_trph_lvl_avg"),
      x %in% c("ct_ff", "w_trph_lvl_avg")
    )

  pred_plot_ind <- predict_plot %>%
    filter(
      y %in% c(get_com_str_var(), "log_bm_std", "piel_nind", "piel_bm"),
      x == "nind_std") %>%
    filter(!y %in% c("nind_std"))

  the_plot <- map(
    list(pred_plot_signif_bm,
      pred_plot_signif_rich,
      pred_plot_other,
      pred_plot_ind),
    function(x) {
      legend_signif <- get_legend(
        x$pred_plot[[1]] +
          theme(legend.position = "bottom")
      )
      x %<>%
        mutate(
          pred_plot = pmap(list(pred_plot, y, x),
            function(p, y, x) {
              # Replace label
              label <- str_replace_all(c(x, y), var_replacement(slope = TRUE))
              p +
                labs(x = label[1], y = label[2]) +
                theme(legend.position = "none")
            })
        )

      list_p <- x$pred_plot
      list_p[[length(list_p) + 1]] <- legend_signif

      pred_plot <- plot_grid(
        plotlist = list_p,
        labels = "AUTO",
        nrow = 2
      )

      if (get_list) {
        return(list_p)
      }

      return(pred_plot)

    }
  )

  names(the_plot) <- c("bm", "rich", "other", "nind")

  return(the_plot)

}

get_plot_rich_bm <- function(
  predict_plot = pred_bm_rich_mono_stable_trends,
  get_list = FALSE,
  rm_legend = FALSE,
  bm_x = "log_bm_std",
  bm_y = c(get_com_str_var(), "log_rich_std", "piel_nind", "piel_bm"),
  rich_y = c(get_com_str_var(), "log_bm_std", "piel_nind", "piel_bm"),
  rich_x = "log_rich_std",
  temporal_label = TRUE
  ) {

  pred_plot_signif_bm <- predict_plot %>%
    filter(
      response %in% bm_y,
      !response %in% bm_x, term == bm_x
    )

  pred_plot_signif_rich <- predict_plot %>%
    filter(
      response %in% rich_y,
      term == rich_x
      ) %>%
    filter(!response %in% rich_x)


  the_plot <- map(
    list(
      pred_plot_signif_bm,
      pred_plot_signif_rich
      ),
    function(x) {
      legend_signif <- get_legend(
        x$pred_plot[[1]] +
          theme(legend.position = "bottom")
      )
      x %<>%
        mutate(
          pred_plot = pmap(list(pred_plot, response, term),
            function(p, y, x) {
              # Replace label
              label <- str_replace_all(c(x, y), var_replacement(slope = temporal_label))
              p +
                labs(x = label[1], y = label[2]) +
                theme(legend.position = "none")
            })
        )

      list_p <- x$pred_plot
      list_p[[length(list_p) + 1]] <- legend_signif

      pred_plot <- plot_grid(
        plotlist = list_p,
        labels = "AUTO",
        nrow = 2
      )

      if (get_list) {
        return(list_p)
      }
      return(pred_plot)
    }
  )

  names(the_plot) <- c("bm", "rich")

  return(the_plot)
}

plot_raw_data_new_model <- function(.df = NULL, x_var = NULL, y_var = NULL, covar = NULL, std_error = FALSE) {

  col_df <- colnames(.df)
  if (paste0(x_var, "_slope") %in% col_df) {
    x_var_error <- paste0(x_var, "_strd_error")
    y_var_error <- paste0(y_var, "_strd_error")
    x_var <- col_df[str_detect(col_df, paste0(x_var, "_slope")) &
      !str_detect(col_df, paste0(x_var, "_strd_error"))]
    y_var <- col_df[str_detect(col_df, paste0(y_var, "_slope")) &
      !str_detect(col_df, paste0(y_var, "_strd_error"))]
  } else {
    x_var_error <- paste0(x_var, "_strd_error")
    y_var_error <- paste0(y_var, "_strd_error")
    x_var <- col_df[str_detect(col_df, x_var) & !str_detect(col_df, paste0(x_var, "_strd_error"))]
    y_var <- col_df[str_detect(col_df, y_var) & !str_detect(col_df, paste0(y_var, "_strd_error"))]
  }
  #return(c(x_var, x_var_error, y_var, y_var_error))
  if (!is.null(covar)) {
    p <- .df %>%
      ggplot(aes_string(x = x_var, y = y_var, color = covar)) +
      viridis::scale_color_viridis() +
      geom_point()
  } else {
    p <- .df %>% ggplot(aes_string(x = x_var, y = y_var)) +
      viridis::scale_color_viridis() +
      geom_point()
  }
  if (std_error) {
    p <- p +
      geom_errorbar(
        data = .df,
        aes_string(
          xmin = paste0(x_var, " - ", x_var_error),
          xmax = paste0(x_var, " + ", x_var_error)
          ),
        color = "gray50") +
      geom_errorbar(
        data = .df,
        aes_string(
          ymin = paste0(y_var, " - ", y_var_error),
          ymax = paste0(y_var, " + ", y_var_error)
          ),
        color = "gray50") +
      geom_point()

  }
  return(p)
}

#' Get spatial report plot

get_y_x_comb_from_model <- function(mod = NULL) {

  x_terms <- labels(terms(mod))
  all_terms <- dimnames(attr(terms(mod), "factors"))[[1]]

  y_term <- all_terms[! all_terms %in% x_terms] 

  tibble(
    y = y_term, 
    x = x_terms 
  )
}

get_predict_from_model_x <- function (mod = NULL, term = NULL) {
  ggpredict(mod, terms = c(term)) %>%
    as_tibble()
}

get_predict_plot_from_model_x <- function (mod = NULL, x = NULL) {

  stopifnot(is.character(x))

  pred <- get_predict_from_model_x(mod = mod, term = x)

  y <- unique(get_y_x_comb_from_model(mod = mod)$y)

  dataset <- mod$data

  # In case of classic lm model:
  if (is.null(dataset)) {
    dataset <- mod$model
  }

  p <- dataset %>%
    ggplot(aes_string(y = y, x = x)) +
    geom_point()

  p <- p +
    geom_line(data = pred, aes(y = predicted, x = x))

  # Axis label
  label_tmp <- var_replacement()
  label <- str_replace_all(label_tmp, "\n", "") 
  names(label) <- names(label_tmp)
  p +
    labs(
      x = label[x],
      y = label[y]
    )
}

get_sp_model_plot <- function(model = sp_models) {
    tibble(model = model,
      comb_term = map(model, ~try(get_y_x_comb_from_model(.x)))) %>%
    unnest(comb_term) %>%
    mutate(
      gg = map2(x, model, ~try(get_predict_plot_from_model_x(mod = .y, x = .x)))
    )
}

get_predict_from_new_model <- function(model = NULL, x_bound = slope_x_bound) {

  x_bound <- tibble::enframe(x_bound, name = "term", value = "min_max")

  # Build the df with term of the model and p.value of Anova
  ti <- tibble(
    response = names(model),
    model = model,
    aov_mod = map(model, car::Anova),
    aov_tab = map(aov_mod, ~broom::tidy(.x) %>%
      filter(term != "Residuals") %>%
      select(term, p.value)
    )) %>%
  unnest(aov_tab) %>%
  left_join(x_bound, by = "term")

# get prediction terms and predictions
ti %>%
  mutate(
    pred_term = map2(term, min_max,
      ~paste0(.x, " [", .y[1], ", -0.000000001, 0.000000001, ", .y[2], "]")
      ),
    prediction = map2(model, pred_term,
      ~try(ggpredict(model = .x, terms = .y) %>%
        as_tibble()
      )
    )
  )

}

get_pred_plot_from_new_model <- function(
  model = NULL,
  dataset = NULL,
  x_bound = slope_x_bound, std_error_bar = TRUE) {

  # Get significativity from anova and make prediction
  aov_pred <- get_predict_from_new_model(
    model = model,
    x_bound = x_bound
  )

  # build raw plot and add prediction
  aov_pred %>%
    mutate(
      raw_plot = map2(term, response,
        ~plot_raw_data_new_model(
          .df = dataset,
          x_var = .x, y_var = .y,
          std_error = std_error_bar, covar = NULL
          )),
      pred_plot = pmap(list(prediction, raw_plot, p.value),
        function(pred, raw_p, s) {
          plot_add_pred_data(pred = pred, gg = raw_p, signif = s)
        }
      )
    )

}

get_corr_plot_tps_trends <- function(
  classif = NULL,
  st = st_mono_trends_stable_rich_bm,
  var_to_keep = c(get_com_str_var(),
    "nbnode_std", "nb_pisc_rich_std",
    "nb_pisc_node_std", "prop_pisc_node",
    "prop_pisc_rich", "bm_std", "log_bm_std")) {

  ti <- classif %>%
    unnest(classif) %>%
    select(station, variable, linear_slope, linear_slope_strd_error) %>%
    filter(station %in% st) %>%
    filter(variable %in% var_to_keep)

    var_slope <- ti %>%
      select(station, variable, linear_slope) %>%
      pivot_wider(names_from = variable, values_from = linear_slope) %>%
      na.omit

    var_slope_error <- ti %>%
      select(station, variable, linear_slope_strd_error) %>%
      pivot_wider(names_from = variable, values_from = linear_slope_strd_error) %>%
      filter(station %in% var_slope$station) %>%
      select(-station) %>%
      rowMeans(., na.rm = TRUE)

    weighted_corr <- cov.wt(
      var_slope[, !colnames(var_slope) %in% "station"],
      wt = var_slope_error, cor = TRUE)$cor

    #g <- corrplot::corrplot(weighted_corr,  type = "upper")
    # does not bc it is a base plot 
    return(weighted_corr)
}

get_range_variable_plot <- function(
  full_data = NULL,
  sp_data = NULL,
  st = NULL,
  var_to_keep = c("ct_ff", "w_trph_lvl_avg", "log_rich_std", "log_bm_std",
    "piel_bm", "piel_nind")
  ) {

  range_spatial_variable <- sp_data %>%
    filter(station %in% st) %>%
    select(c("station", all_of(var_to_keep))) %>%
    pivot_longer(!station, names_to = "variable", values_to = "values") %>%
    mutate(variable = str_replace_all(variable, var_replacement())) %>%
    group_by(variable) %>%
    summarise(
      min = min(values, na.rm = TRUE),
      max = max(values, na.rm = TRUE)) %>%
    mutate(range = max - min)

  range_variable_station <- full_data %>%
    filter(station %in% st) %>%
    select(c("station", all_of(var_to_keep))) %>%
    pivot_longer(!station, names_to = "variable", values_to = "values") %>%
    mutate(variable = str_replace_all(variable, var_replacement())) %>%
    group_by(station, variable) %>%
    summarise(
      min = min(values, na.rm = TRUE),
      max = max(values, na.rm = TRUE),
      .groups = "drop"
    )

  p_min_max <- range_variable_station %>%
    pivot_longer(
      cols = c(min, max),
      names_to = "minmax",
      values_to = "values") %>%
    ggplot(aes(x = values, fill = minmax)) +
    geom_histogram() +
    geom_vline(data = range_spatial_variable %>%
      pivot_longer(
        cols = c(min, max),
        names_to = "minmax",
        values_to = "values"),
    aes(xintercept = values, color = minmax), size = 2
    ) +
  facet_wrap(~variable, scale = "free_x")


  p_range <- range_variable_station %>%
    mutate(range = abs(max - min)) %>%
    ggplot(aes(x = range)) +
    geom_histogram() +
    geom_vline(data = range_spatial_variable, aes(xintercept = range), size = 2) +
    facet_wrap(~variable, scale = "free")

  return(list(min_max = p_min_max, range = p_range))
}
