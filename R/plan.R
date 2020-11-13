# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html
plan <- drake_plan(
  net_data = get_network_data(path = file_in(!!get_mypath("data", "classes", "network_metrics.rda"))),
  com_data = get_community_data(path = file_in(!!get_mypath("data", "community_metrics.rda"))),
  op_data = get_op_data(path = file_in(!!get_mypath("data", "op_analysis.rda"))),
  habitat_press = get_data(obj_name = "habitat_press", dir = get_mypath("data")),
  habitat_pressure = get_data(obj_name = "habitat_pressure", dir = get_mypath("data")),
  region_polygon = get_data(obj_name = "region_polygon", dir = get_mypath("data")),
  station_analysis = get_data(obj_name = "station_analysis",
    dir = "~/Documents/post-these/mnhn/fishcom/data"),

  full_data = get_full_data(net = net_data, com = com_data, op = op_data),
  full_data2 = add_to_full_data(.data = full_data),
  monotonous_data = get_monotonous_station(.data = full_data2),
  temporal_dynamics = get_lm_station(.data = monotonous_data, 
    var_name = c(get_biomass_var(), get_com_str_var(all = TRUE)),
    rhs = " ~ nb_year"),
  rigal_classification = compute_rigal_classif(data = full_data2, 
    variable = c(get_biomass_var(), get_com_str_var(all = TRUE))),
  biomass_group = get_station_biomass_summary(.data = full_data2, bm_var = get_biomass_var()),
  richness_group = get_station_com_summary(
  .data = full_data2,
  myvar = get_richness_var(),
  var_cat = "rich"),
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
      var_to_group = grouped_var),
    transform = cross(
      my_bm_net_group = list(bm_net_group_median, bm_net_group_f3y,
	log_bm_net_group_median, log_bm_net_group_f3y),
      grouped_var = list(c("variable"), c("variable", "protocol_type")),
      .names = model_type_var(add_protocol = TRUE)
    )
    ),
  vif = target(
    get_vif(model, model_cols =
      all_of(tidyselect::vars_select(names(model), starts_with("mod")))),
    transform = map(model,
      .names = paste0("vif_",
	model_type_var(cut_prefix = TRUE, add_protocol = TRUE)
      )
    )
    ),
  model_summary = target(
    model_summary(model),
    transform = map(model,
    .names = paste0("summary_",
	model_type_var(cut_prefix = TRUE, add_protocol = TRUE)
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
      dataset = list(bm_vs_net_trends, log_bm_vs_net_trends, bm_vs_net_trends, log_bm_vs_net_trends),
      st = list(bm_std_st_decrease_increase$station,
	log_bm_std_st_decrease_increase$station, 
	bm_std_st_decrease_increase$station,
	log_bm_std_st_decrease_increase$station),
      bm_summary_var = c("first_3_year", "first_3_year", "median", "median"),
      .names = c("bm_net_group_f3y", "log_bm_net_group_f3y", "bm_net_group_median", "log_bm_net_group_median")
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
  ) 
)

