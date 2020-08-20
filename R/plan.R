# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  net_data = get_network_data(),
  com_data = get_community_data(),
  op_data = get_op_data(),
  full_data = get_full_data(net = net_data, com = com_data, op = op_data),
  filtered_time_series = get_monotonous_station(.data = full_data),
  data = add_to_full_data(.data = filtered_time_series),
  temporal_dynamics = get_lm_station(.data = data, 
    var_name = c("biomass", "log_bm", "rel_bm", "rel_log_bm", "connectance", "w_trph_lvl_avg", "richness"),
    rhs = " ~ nb_year + surface"),
  temporal_dynamics_plot = get_temporal_dynamics_plot(temporal_dynamics = temporal_dynamics),
  temporal_dynamics_coef = get_lm_coeff(.data = temporal_dynamics,
    col_names = c("biomass", "log_bm", "rel_bm", "rel_log_bm", "connectance", "w_trph_lvl_avg", "richness")),
  net_dyn_lm = compute_lm_temporal_trends(.data = temporal_dynamics_coef,
    y = c("biomass", "log_bm", "rel_bm", "rel_log_bm"), x = c("connectance", "w_trph_lvl_avg", "richness")),
  net_dyn_lm_plot = get_net_dyn_lm_plot(net_dyn_lm = net_dyn_lm),
  net_dyn_lm_coeff = get_lm_coeff(.data = net_dyn_lm, col_names = "model"),
  report = rmarkdown::render(input = knitr_in("report/report.Rmd"),
    output_file = file_out("report/report.html")
  )  
)

