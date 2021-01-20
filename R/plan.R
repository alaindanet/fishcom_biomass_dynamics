# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

# Variable:
model_x_var <- c("log_bm_std", "rich_std", "log_rich_std", "ct_ff", "w_trph_lvl_avg")
#model_y_var <- c("ct_ff", "w_trph_lvl_avg", "rich_std", "log_rich_std", "w_trph_lvl_avg")

plan <- drake_plan(
  net_analysis_data = target(
    get_network_analysis_data(
    path = file_in(!!get_mypath("data", "classes", "network_analysis.rda"))
    ), resources = list(cores = 3, gpus = 0)),
  net_data = get_network_data(path = file_in(!!get_mypath("data", "classes", "network_metrics.rda"))),
  com_data = get_community_data(path = file_in(!!get_mypath("data", "community_metrics.rda"))),
  com_analysis_data = get_community_analysis_data( path =
    file_in(!!get_mypath("data", "community_analysis.rda")), op = op_data),
  op_data = get_op_data(path = file_in(!!get_mypath("data", "op_analysis.rda"))),
  habitat_press = get_data(obj_name = "habitat_press", dir = get_mypath("data")),
  habitat_pressure = get_data(obj_name = "habitat_pressure", dir = get_mypath("data")),
  region_polygon = get_data(obj_name = "region_polygon", dir = get_mypath("data")),
  station_analysis = get_data(obj_name = "station_analysis",
    dir = "~/Documents/post-these/mnhn/fishcom/data"),
  net_l_ld_IS = get_l_ld_IS(net = net_analysis_data),
  metrics_fishfish_only = get_network_metric_fish_only(net = net_analysis_data),
  net_data2 = left_join(net_data, net_l_ld_IS, by = "opcod") %>%
    left_join(select(metrics_fishfish_only, opcod, ct_ff, l_ff, ld_ff), by = "opcod"),
  piel = get_piel_nind_bm(com = com_analysis_data), 
  com_data2 = com_data %>%
    left_join(piel, by = "opcod"),
  full_data = get_full_data(net = net_data2, com = com_data2, op = op_data),
  full_data2 = add_to_full_data(.data = full_data),
  species_full_data = get_species_full_data(com = com_analysis_data, op =
    op_data),
  monotonous_data = get_monotonous_station(.data = full_data2),
  temporal_dynamics = get_lm_station(.data = monotonous_data, 
    var_name = c(get_biomass_var(), get_com_str_var(all = TRUE)),
    rhs = " ~ nb_year"),
  rigal_classification = compute_rigal_classif(data = full_data2, 
    variable = c(get_biomass_var(), get_com_str_var(all = TRUE))),
  rigal_species = compute_rigal_classif(
  data = species_full_data,
  variable = colnames(species_full_data)[!is.na(str_extract(colnames(species_full_data), "[A-Z]{3}"))]  
  ),

  biomass_group = get_station_biomass_summary(.data = full_data2, bm_var = get_biomass_var()),
  richness_group = get_station_com_summary(
  .data = full_data2,
  myvar = get_richness_var(),
  var_cat = "rich"),
  summary_var = full_data2 %>%
    select(-opcod, -surface, -rel_bm, -rel_log_bm) %>%
    pivot_longer(
      cols = biomass:log_nb_pisc_rich_std,
      names_to = "variable",
      values_to = "value") %>%
    group_by(station, variable) %>%
    arrange(year) %>%
    summarise(med = median(value), f3y = median(value[1:3])) %>%
    ungroup(),
  summary_var_med = summary_var %>% 
    select(-f3y) %>%
    pivot_wider(names_from = "variable", values_from = "med"),
  summary_var_f3y = summary_var %>% 
    select(-med) %>%
    pivot_wider(names_from = variable, values_from = "f3y"),
  stream_group = get_stream_group(
    op_analysis_path = file_in(!!get_mypath("data", "op_analysis.rda")),
    habitat_press_path = file_in(!!get_mypath("data", "habitat_press.rda"))),
  bm_vs_net_trend = target(
    get_bm_vs_network_trends(classif = rigal_classification, bm_var = y),
    transform = map(
      y = !!get_biomass_var(),
      .names = c("bm_vs_net_trends", "log_bm_vs_net_trends")
    )
  ),
  com_vs_net_trend = target(
    get_rich_vs_network_trends(classif = rigal_classification, rich_var = y),
    transform = map(
      y = !!get_richness_var(),
      .names = c("rich_vs_net_trends", "log_rich_vs_net_trends") 
    )
  ),
  st_mono_trends_combined_list = 
    rigal_classification %>%
    mutate(
      classif = map(classif, ~ .x %>%
	filter(shape_class %in% c("increase_constant", "decrease_constant")) %>%
	select(station, shape_class)),
      station = map(classif, ~.x$station)) %>%
    select(-classif)
    ,
  slope_x_bound = get_slope_x_bound(
    classif = rigal_classification,
    st_mono = st_mono_trends_combined_list, type = "min_max"),

  # 3. Modelling
  comb = list(
    y = c(get_com_str_var(all = TRUE), "bm_std", "log_bm_std"),
    x = model_x_var
  ) %>% 
  expand.grid(., stringsAsFactors = FALSE) %>%
  as_tibble %>%
  # filtering combination: 
  filter(y != x) %>%
  mutate(
    covar = ifelse(str_detect(x, "log_"), str_remove(x, "log_"), x)
  ),
  data4model = comb %>%
    mutate(
      data_model = map2(x, y, function (myx, myy, classif) {
	get_y_versus_x_trends(classif = classif, x_var = myx, y_var = myy) %>%
	  filter(station %in% get_st_mono_trends(.df = classif, xvar = myx)$station)
      }, classif = rigal_classification),
      data_model = map2(data_model, covar,
	~left_join(.x, summary_var_f3y[, colnames(summary_var_f3y) %in% c("station", .y)], by = "station")
      )
    ),
  model = data4model %>% 
    mutate(
      mods = pmap(list(.df = data_model, x = x, covar = covar), compute_my_lm_vs_net_model,
	var_to_group = "variable")
      ) %>%
    unnest(c(mods)) %>%
    select(-data_model, -variable) %>%
    select(-mod_all_bm),
  model_vif = get_vif(model, model_cols = all_of(tidyselect::vars_select(names(model),
	starts_with("mod")))
    ),
  model_summary = get_model_summary(model),

  #temporal_dynamics_plot = get_temporal_dynamics_plot(temporal_dynamics = temporal_dynamics),
  temporal_dynamics_coef = get_lm_coeff(
    .data = temporal_dynamics,
    col_names = c("bm_std", "log_bm_std", "connectance", "w_trph_lvl_avg", "richness", "weighted_connectance", "nbnode_std", "nb_pisc_rich_std", "nb_pisc_node_std", "prop_pisc_node", "prop_pisc_rich")),
  dyn_group = add_group_station(.data = temporal_dynamics_coef, group = biomass_group),
  net_dyn_lm = compute_lm_temporal_trends(
    .data = dyn_group,
    x = c("bm_std", "log_bm_std", "connectance", "w_trph_lvl_avg", "richness", "weighted_connectance", "nbnode_std", "nb_pisc_rich_std", "nb_pisc_node_std", "prop_pisc_node", "prop_pisc_rich"), 
    y = c("weighted_connectance", "connectance", "w_trph_lvl_avg", "richness", "nbnode_std", "nb_pisc_rich_std", "nb_pisc_node_std", "prop_pisc_node", "prop_pisc_rich"), group = com_size),
  net_dyn_lm_plot = get_net_dyn_lm_plot(net_dyn_lm = net_dyn_lm),
  net_dyn_lm_coeff = get_lm_coeff(.data = net_dyn_lm, col_names = "model"),

  report = callr::r(
    function(...) rmarkdown::render(...),
    args = list(
      input = drake::knitr_in("report.Rmd"),
      output_file = drake::file_out("report.html")
    )
  ),
  #4. The plots
  p_bm_vs_bm_slope = plot_bm_vs_bm_slope(
    tps_dyn_coef = temporal_dynamics_coef,
    bm_group = biomass_group  
    ),
  p_hist_med_bm = plot_hist_med_bm(.data = full_data2),

  #5. Tables 
  effect_quad2_piece = tibble(
    effect = c("Dynamic of biomass", "disassembly/assembly", "com size", "bm dyn dep on com size?", "bm dyn dep on disassembly/assembly?", "bm dyn dep on both dis/assembly and com size ?"),
    mod_bm_quad2 = c("bm_slope", "I(bm_slope^2)", "bm", "bm_slope:bm", "I(bm_slope^2)", "bm:I(bm_slope^2)"),
    mod_medium_bm = c("bm_slope", "bm_slope:inc_fTRUE", "bm", "bm_slope:bm", "bm_slope:inc_fTRUE", "bm_slope:bm:inc_fTRUE")
    ),
  #hyp_table_f3y = make_hyp_table(hyp_table = effect_quad2_piece, mod = model_log_bm_std),
  #hyp_table = make_hyp_table(hyp_table = effect_quad2_piece, mod = model_log_bm_std),
  summary_table = model_summary %>%
      mutate(
	reg_table = map(model_obj, broom::tidy),
	anova_table = map(anova, broom::tidy)
	) %>%
      select(any_of(c("x", "y", "covar", "model", "model_obj", "reg_table", "anova_table"))),
  predict_table = summary_table %>%
     mutate(
	ggpred_term = map(anova_table, 
	  ~get_ggpredict_term_from_anova(aov_tab = .x,
	    bound = slope_x_bound)),
	tmp_pred_table = map2(model_obj, ggpred_term, ~ ggpredict(.x, .y) %>% as_tibble),
	pred_table = map2(tmp_pred_table, ggpred_term, ~rename_pred_table(pred_table = .x, term = .y)),
	) %>%
      select(any_of(c("x", "y", "covar", "model", "model_obj", "ggpred_term", "tmp_pred_table", "pred_table"))),
  predict_plot = predict_table %>%
      left_join(select(model, x, y, covar, data), by = c("x", "y", "covar")) %>%
      mutate(
	raw_plot = map2(data, covar,
	  ~plot_raw_data(.df = .x, covar = .y, std_error = TRUE)
	  ),
	 pred_plot = map2(pred_table, raw_plot, 
	  ~plot_add_pred_data(pred = .x, gg = .y)
	  )
	) %>%
      select(any_of(c("x", "y", "covar", "model", "pred_table", "raw_plot", "pred_plot"))),
  #model_log_bm_f3y_table = get_table_from_summary(
    #.data = summary_log_bm_f3y,
    #model =  "mod_bm_quad2",
    #variable = NULL 
  #),
  model_log_bm_f3y_table_medium = list(
    reg_table = unnest(summary_table, reg_table),
    anova_table = unnest(summary_table, anova_table)
    ) %>%
    map(., ~ .x %>% filter(x == "log_bm_std") %>%
      select(-any_of(c("x", "covar", "model", "model_obj", "reg_table", "anova_table")))),
  #model_log_rich_f3y_table_medium = get_table_from_summary(
    #.data = summary_log_rich_f3y,
    #model =  "mod_medium_bm",
    #variable = NULL
    #),
  trace = TRUE
)

