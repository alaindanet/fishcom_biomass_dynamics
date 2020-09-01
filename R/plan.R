# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html
plan <- drake_plan(
  net_data = get_network_data(path = file_in(!!get_mypath("data", "classes", "network_metrics.rda"))),
  com_data = get_community_data(path = file_in(!!get_mypath("data", "community_metrics.rda"))),
  op_data = get_op_data(path = file_in(!!get_mypath("data", "op_analysis.rda"))),
  full_data = get_full_data(net = net_data, com = com_data, op = op_data),
  filtered_time_series = get_monotonous_station(.data = full_data),
  data = add_to_full_data(.data = filtered_time_series),
  biomass_group = get_biomass_group(.data = data),
  temporal_dynamics = get_lm_station(.data = data, 
    var_name = c("biomass", "log_bm", "connectance", "w_trph_lvl_avg", "richness", "weighted_connectance"),
    rhs = " ~ nb_year + surface"),
  temporal_dynamics_plot = get_temporal_dynamics_plot(temporal_dynamics = temporal_dynamics),
  temporal_dynamics_coef = get_lm_coeff(
    .data = temporal_dynamics,
    col_names = c("biomass", "log_bm", "connectance", "w_trph_lvl_avg", "richness", "weighted_connectance")),
  dyn_group = add_group_station(.data = temporal_dynamics_coef, group = biomass_group),
  net_dyn_lm = compute_lm_temporal_trends(
    .data = dyn_group,
    x = c("biomass", "log_bm", "connectance", "w_trph_lvl_avg", "richness", "weighted_connectance"), 
    y = c("weighted_connectance", "connectance", "w_trph_lvl_avg", "richness"), group = com_size),
  net_dyn_lm_plot = get_net_dyn_lm_plot(net_dyn_lm = net_dyn_lm),
  net_dyn_lm_coeff = get_lm_coeff(.data = net_dyn_lm, col_names = "model"),
  report = rmarkdown::render(
    input = knitr_in("report.Rmd"),
    output_file = file_out("report.html")
  )
)

