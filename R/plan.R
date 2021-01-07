# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html
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
  full_data = get_full_data(net = net_data2, com = com_data, op = op_data),
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
  summary_var = monotonous_data %>%
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

  st_decrease_increase = target(
    rigal_classification %>%
      unnest(classif) %>%
      filter(
	variable == y,
	shape_class %in% c("increase_constant", "decrease_constant")
	) %>%
      select(station, shape_class),
    transform = map(
      y = c("bm_std", "log_bm_std", "rich_std", "log_rich_std"),
      .names = paste0(c("bm_std", "log_bm_std", "rich_std", "log_rich_std"), "_st_decrease_increase")
    )
  ),
  model = target(
    compute_my_lm_vs_net_model(
      .df = filter(my_bm_net_group, !is.na(protocol_type)),
      var_to_group = grouped_var,
      x = ifelse("bm_slope" %in% colnames(my_bm_net_group), "bm_slope", "rich_slope")
      ),
    transform = cross(
      my_bm_net_group = list(
	bm_net_group_median, bm_net_group_f3y,
	log_bm_net_group_median, log_bm_net_group_f3y,
	rich_net_group_median, rich_net_group_f3y,
	log_rich_net_group_median, log_rich_net_group_f3y),
      grouped_var = list(c("variable"), c("variable", "protocol_type")),
      .names = model_type_var(add_protocol = TRUE, add_rich = TRUE) #add rich
    )
    ),
  vif = target(
    get_vif(model, model_cols =
      all_of(tidyselect::vars_select(names(model), starts_with("mod")))),
    transform = map(model,
      .names = paste0("vif_",
	model_type_var(cut_prefix = TRUE, add_protocol = TRUE, add_rich = TRUE)
      )
    )
    ),
  model_summary = target(
    model_summary(model),
    transform = map(model,
    .names = paste0("summary_",
	model_type_var(cut_prefix = TRUE, add_protocol = TRUE, add_rich = TRUE)
	)
    )
    ),
  model_pred = target(
    get_mod_pred(model_summary),
    transform = map(model_summary,
    .names = paste0("pred_",
	model_type_var(cut_prefix = TRUE, add_protocol = TRUE, add_rich = TRUE)
	)
    )
    ),

  bm_group = target(
    add_stream_bm_caract_to_model(bm_vs_network_df = dataset,
      stream_caract = stream_group, bm_caract = biomass_group,
      bm_group_var = "bm_std", group_type_caract = "median",
      bm_summary_type = bm_summary_var) %>% 
    filter(station %in% st),
  transform = map(
    dataset = list(bm_vs_net_trends, log_bm_vs_net_trends, rich_vs_net_trends, log_rich_vs_net_trends, bm_vs_net_trends, log_bm_vs_net_trends, rich_vs_net_trends, log_rich_vs_net_trends),
    st = list(bm_std_st_decrease_increase$station,
      log_bm_std_st_decrease_increase$station,
      rich_std_st_decrease_increase$station,
      log_rich_std_st_decrease_increase$station,
      bm_std_st_decrease_increase$station,
      log_bm_std_st_decrease_increase$station,
      rich_std_st_decrease_increase$station,
      log_rich_std_st_decrease_increase$station
      ),
    bm_summary_var = c("first_3_year", "first_3_year", "first_3_year", "first_3_year", "median", "median", "median", "median"),
    .names = c("bm_net_group_f3y", "log_bm_net_group_f3y", "rich_net_group_f3y", "log_rich_net_group_f3y", "bm_net_group_median", "log_bm_net_group_median", "rich_net_group_median", "log_rich_net_group_median")
  ) 
  ),

  temporal_dynamics_plot = get_temporal_dynamics_plot(temporal_dynamics = temporal_dynamics),
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
  p_pred_medium_f3y = get_pred_model(
    summary_model = summary_log_bm_f3y,
    model_name = "mod_medium_bm",
    model_data = model_log_bm_f3y
  ),
  p_pred_quad2_f3y = get_pred_model(
    summary_model = summary_log_bm_f3y,
    model_name = "mod_bm_quad2",
    model_data = model_log_bm_f3y
  ),
  pred_plot_signif = get_model_plot_from_signif_term(
    anova_table = model_log_bm_f3y_table_medium[["anova_table"]],
    model = model_log_bm_f3y 
    ),
  pred_plot_signif_log_rich = get_model_plot_from_signif_term(
    anova_table = model_log_rich_f3y_table_medium[["anova_table"]],
    model = model_log_rich_f3y
    ),

  #5. Tables 
  effect_quad2_piece = tibble(
    effect = c("Dynamic of biomass", "disassembly/assembly", "com size", "bm dyn dep on com size?", "bm dyn dep on disassembly/assembly?", "bm dyn dep on both dis/assembly and com size ?"),
    mod_bm_quad2 = c("bm_slope", "I(bm_slope^2)", "bm", "bm_slope:bm", "I(bm_slope^2)", "bm:I(bm_slope^2)"),
    mod_medium_bm = c("bm_slope", "bm_slope:inc_fTRUE", "bm", "bm_slope:bm", "bm_slope:inc_fTRUE", "bm_slope:bm:inc_fTRUE")
    ),
  hyp_table_f3y = make_hyp_table(hyp_table = effect_quad2_piece, mod = model_log_bm_f3y),
  hyp_table = make_hyp_table(hyp_table = effect_quad2_piece, mod = model_log_bm),
  model_log_bm_f3y_table = get_table_from_summary(
    .data = summary_log_bm_f3y,
    model =  "mod_bm_quad2",
    variable = NULL 
  ),
  model_log_bm_f3y_table_medium = get_table_from_summary(
    .data = summary_log_bm_f3y,
    model =  "mod_medium_bm",
    variable = NULL
  ),
  model_log_rich_f3y_table_medium = get_table_from_summary(
    .data = summary_log_rich_f3y,
    model =  "mod_medium_bm",
    variable = NULL
    ),

)

