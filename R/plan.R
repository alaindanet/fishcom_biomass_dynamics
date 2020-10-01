# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html
plan <- drake_plan(
  net_data = get_network_data(path = file_in(!!get_mypath("data", "classes", "network_metrics.rda"))),
  com_data = get_community_data(path = file_in(!!get_mypath("data", "community_metrics.rda"))),
  op_data = get_op_data(path = file_in(!!get_mypath("data", "op_analysis.rda"))),
  full_data = get_full_data(net = net_data, com = com_data, op = op_data),
  full_data2 = add_to_full_data(.data = full_data),
  monotonous_data = get_monotonous_station(.data = full_data2),
  temporal_dynamics = get_lm_station(.data = monotonous_data, 
    var_name = c("bm_std", "log_bm_std", "connectance", "w_trph_lvl_avg", "richness", "weighted_connectance"),
    rhs = " ~ nb_year"),
  rigal_classification = compute_rigal_classif(data = full_data2, 
    variable = c("bm_std", "log_bm_std", "connectance", "w_trph_lvl_avg", "richness", "weighted_connectance")),
  biomass_group = get_station_biomass_summary(.data = full_data2, bm_var = c("bm_std", "log_bm_std")),
  stream_group = get_stream_group(
    op_analysis_path = file_in(!!get_mypath("data", "op_analysis.rda")),
    habitat_press_path = file_in(!!get_mypath("data", "habitat_press.rda"))),
  bm_vs_net_trends = get_bm_vs_network_trends(classif = rigal_classification, bm_var = "bm_std"),
  log_bm_vs_net_trends = get_bm_vs_network_trends(classif = rigal_classification, bm_var = "log_bm_std"),
  bm_net_group = 
    add_stream_bm_caract_to_model(bm_vs_network_df = bm_vs_net_trends,
      stream_caract = stream_group, bm_caract = biomass_group,
      bm_group_var = "bm_std", group_type_caract = "median") %>% 
  filter(station %in% bm_std_st_decrease_increase$station),
  log_bm_net_group = 
    add_stream_bm_caract_to_model( bm_vs_network_df = log_bm_vs_net_trends,
      stream_caract = stream_group, bm_caract = biomass_group,
      bm_group_var = "bm_std", group_type_caract = "median") %>%
  filter(station %in% log_bm_std_st_decrease_increase$station),
  , 
  bm_std_st_decrease_increase = rigal_classification %>%
    unnest(classif) %>%
    filter(variable == "bm_std", shape_class %in% c("increase_constant", "decrease_constant")) %>%
    select(station, shape_class),
  log_bm_std_st_decrease_increase = rigal_classification %>%
    unnest(classif) %>%
    filter(variable == "log_bm_std", shape_class %in% c("increase_constant", "decrease_constant")) %>%
    select(station, shape_class),
  temporal_dynamics_plot = get_temporal_dynamics_plot(temporal_dynamics = temporal_dynamics),
  temporal_dynamics_coef = get_lm_coeff(
    .data = temporal_dynamics,
    col_names = c("bm_std", "log_bm_std", "connectance", "w_trph_lvl_avg", "richness", "weighted_connectance")),
  dyn_group = add_group_station(.data = temporal_dynamics_coef, group = biomass_group),
  net_dyn_lm = compute_lm_temporal_trends(
    .data = dyn_group,
    x = c("bm_std", "log_bm_std", "connectance", "w_trph_lvl_avg", "richness", "weighted_connectance"), 
    y = c("weighted_connectance", "connectance", "w_trph_lvl_avg", "richness"), group = com_size),
  net_dyn_lm_plot = get_net_dyn_lm_plot(net_dyn_lm = net_dyn_lm),
  net_dyn_lm_coeff = get_lm_coeff(.data = net_dyn_lm, col_names = "model"),
  report = rmarkdown::render(
    input = knitr_in("report.Rmd"),
    output_file = file_out("report.html")
  )
)
