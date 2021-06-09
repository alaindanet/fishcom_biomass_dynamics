# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

# Variable:
model_x_var <- c("log_rich_std", "log_bm_std", "bm_std", "rich_std", "ct_ff", "w_trph_lvl_avg", "nind_std")
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
  betadiv = map_dfr(
    c("biomass", "bm_std", "nind_std", "nind"),
    ~compute_temporal_betadiv(
      .op = op_data,
      com = filter(com_analysis_data, !is.na(surface)),
      variable = .x) %>%
    select(-data, -com) %>%
    mutate(variable = .x)
  ) %>%
  pivot_longer(cols = c(betadiv:betadiv_bin_diag)) %>%
  mutate(name = str_c(name, variable, sep = "_")) %>%
  select(-variable) %>%
  pivot_wider(names_from = "name", values_from = "value")
  ,
  betapart_bin = 
    get_com_mat_station(
      com = com_analysis_data,
      .op = op_data,
      variable = "biomass",
      presence_absence = TRUE) %>%
  get_temporal_betapart_from_com_mat_station(com = .) %>%
  select(-betapart) %>%
  ungroup() %>%
  unnest(be),
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

  # 2.2 Environment
  st_analysis_by_var = get_station_analysis_by_var(
    classif = rigal_classification),
  st_analysis = get_all_station_analysis(classif = rigal_classification),
  data_for_pca = habitat_press %>%
    filter(station %in% st_analysis) %>%
    select(all_of(c("station", get_var_for_pca()))),
  pca = get_pca_environment(.data = data_for_pca),
  environment_pca = data_for_pca %>%
    mutate(
      RC1 = pca$rotated$scores[, 1],
      log_RC1 = log(RC1 + abs(min(RC1)) + 1),
      RC2 = pca$rotated$scores[, 2],
      log_RC2 = log(RC2 + abs(min(RC2)) + 1)
      ) %>%
    select(RC1, log_RC1, RC2, log_RC2, everything()),

  # 3. Modelling
  comb = list(
    y = c(get_com_str_var(all = TRUE), get_biomass_var()),
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
	~left_join(
	  .x,
	  summary_var_f3y[, colnames(summary_var_f3y) %in% c("station", .y)],
	  by = "station"
	)
      )
    ),
  model = data4model %>%
    mutate(
      mods = pmap(list(.df = data_model, x = x, covar = covar),
	compute_my_lm_vs_net_model,
	var_to_group = "variable")
      ) %>%
    unnest(c(mods)) %>%
    select(-data_model, -variable) %>%
    select(-mod_all_bm),
  model_vif = get_vif(model, model_cols = all_of(tidyselect::vars_select(names(model),
	starts_with("mod")))
    ),
  model_summary = get_model_summary(model),
  model_summary_scale = model %>%
  mutate(
    scaled_mod = map(mod_medium_bm, lm.beta::lm.beta),
    resume = map(scaled_mod, ~summary.lm.beta(.x, standardized = TRUE)),
    anova = map(scaled_mod, ~anova(.x))
  ),

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

  # 3.2 Analysis for the spatial models
  basin_station = get_basin_station(),
  sp_st_data = summary_var_med %>%
    left_join(environment_pca, by = "station") %>%
    left_join(basin_station, by = "station") %>%
    filter(station %in% st_analysis) %>%
    select(all_of(c("station", "basin", "log_RC1", "log_RC2", get_all_var_analysis()))) %>%
    na.omit,
  st_sp = st4sp_model(my_x_var = model_x_var, .data4model = data4model),
  sp_models = map(st_sp,
    ~get_mod_list(.data = as.data.frame(sp_st_data[sp_st_data$station %in% .x, ]))),
  colin_sp_models = map(sp_models, ~map(.x, car::vif)),
  est_sp_models = map(sp_models, ~map(.x, broom.mixed::tidy)),

  #4. The plots
  p_bm_vs_bm_slope = plot_bm_vs_bm_slope(
    tps_dyn_coef = temporal_dynamics_coef,
    bm_group = biomass_group
    ),
  p_hist_med_bm = plot_hist_med_bm(.data = full_data2),
  p_cross_classif_bm_rich = plot_matrix_bm_rich_cross_classif(
    classif = rigal_classification),
  p_fig1_2 = get_plot_fig1_2(predict_plot = predict_plot2,
    bm_x = "log_bm_std", rich_x = "log_rich_std"
    ),
  p_pca = my_pca_plot(.data = pca$rotated,
    xaxis = "RC1", yaxis = "RC2", ctb_thld = .4, 
    label_size = 4, force_pull = 0.01, force = 10,
    seed = 1
  ),
  sp_relation_plot = map(sp_models, get_sp_model_plot),
  sp_relation_plot2 = rbind(
    sp_relation_plot$rich_std %>%
      filter(x == "log_rich_std"),
    sp_relation_plot$bm_std %>%
      filter(x == "log_bm_std")
  ),


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
      mod_summary_table = map(model_obj, broom::glance),
      anova_table = map(anova, broom::tidy)
      ) %>%
  select(any_of(c("x", "y", "covar", "model", "model_obj", "reg_table", "mod_summary_table", "anova_table"))),
  summary_table_scale = model_summary_scale %>%
    mutate(
      reg_table = map(scaled_mod, broom::tidy),
      mod_summary_table = map(scaled_mod, broom::glance),
      anova_table = map(anova, broom::tidy)
      ) %>%
  select(any_of(c("x", "y", "covar", "model", "model_obj", "reg_table", "mod_summary_table", "anova_table"))),
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

    predict_table2 = summary_table %>%
  select(x, y, covar, anova_table) %>%
  left_join(data4model, by = c("x", "y", "covar")) %>%
  mutate(
    model_obj = furrr::future_map2(anova_table, data_model,
      ~compute_lm_from_signif_anova(aov_tab = .x, .df = .y)
      ),
    anova = map(model_obj, anova),
    anova_table = map(anova, broom::tidy),
    ggpred_term = map(anova_table,
      ~get_ggpredict_term_from_anova(
	aov_tab = .x,
	bound = slope_x_bound,
	pval_threshold = 1
	)),
  tmp_pred_table = map2(model_obj, ggpred_term, 
    ~ggpredict(.x, .y) %>%
      as_tibble),
    pred_table = map2(tmp_pred_table, ggpred_term,
      ~rename_pred_table(pred_table = .x, term = .y))) %>%
      select(any_of(c("x", "y", "covar", "model", "model_obj", "ggpred_term", "tmp_pred_table", "pred_table", "anova_table"))),

    predict_plot2 = predict_table2 %>%
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
  model_log_bm_f3y_table_medium = list(
    reg_table = unnest(summary_table, reg_table),
    anova_table = unnest(summary_table, anova_table)
    ) %>%
    map(., ~ .x %>% filter(x == "log_bm_std") %>%
      select(-any_of(c("x", "covar", "model", "model_obj", "reg_table", "anova_table")))),
  model_log_rich_f3y_table_medium = list(
    reg_table = unnest(summary_table, reg_table),
    anova_table = unnest(summary_table, anova_table)
    ) %>%
    map(., ~ .x %>% filter(x == "log_rich_std") %>%
      select(-any_of(c("x", "covar", "model", "model_obj", "reg_table", "anova_table")))),

    #5.2 Table for spatial analysis
    anova_table_sp = map(sp_models, ~map(.x, ~anova(.x))),
    tab_sp_anova = map(anova_table_sp, get_anova_table_from_named_list),
    reg_table_sp = map(sp_models, ~map(.x, ~broom::tidy(.x))),
    mod_summary_table_sp = map(sp_models, ~map(.x, ~broom::glance(.x))),
    rsq_sp_mod = map(
      sp_models,
      ~piecewiseSEM::rsquared(modelList = .x, method = NULL)
      ),

    # Reporting

  report = callr::r(
    function(...) rmarkdown::render(...),
    args = list(
      input = drake::knitr_in("report.Rmd"),
      output_file = drake::file_out("report.html")
    )
  ),
  wordstack = callr::r(
    function(...) rmarkdown::render(...),
    args = list(
      input = drake::knitr_in("paper/wordstack.Rmd"),
      output_file = drake::file_out("paper/wordstack.html"),
      output_dir = "paper"
    )
  ),
  manuscript = callr::r(
    function(...) rmarkdown::render(...),
    args = list(
      input = drake::knitr_in("paper/manuscript.Rmd"),
      output_file = drake::file_out("paper/manuscript.pdf"),
      output_dir = "paper"
    )
    ),
  appendix = callr::r(
    function(...) rmarkdown::render(...),
    args = list(
      input = drake::knitr_in("paper/appendix.Rmd"),
      output_file = drake::file_out("paper/appendix.pdf"),
      output_dir = "paper"
    )
    ),

  # Supplementary analysis

  ## Make the temporal analysis with together log_bm_std and log_rich_std

  ### Get both slopes
  var_slope_to_keep = c("station", "linear_slope", "linear_slope_strd_error"),
  slope_com_var = map(
    unique(c(get_com_str_var(), "log_bm_std", "log_rich_std")),
    function(x, var_to_keep, classif_var) {

      x_data <- classif_var[classif_var$variable == x, ] %>%
	unnest(classif)
      x_data <- x_data[, colnames(x_data) %in% var_to_keep]

      # Rename according to the variable
      colnames(x_data)[colnames(x_data) %in% var_to_keep[2:3]] <-
	paste0(x, c("_slope", "_strd_error"))
      return(x_data)

    },
    var_to_keep = c("station", "linear_slope", "linear_slope_strd_error"),
  classif_var = rigal_classification) %>%
  Reduce(merge.all, .) %>%
  left_join(summary_var_f3y[, c("station", "bm_std", "rich_std")]) %>%
  mutate(
    inc_f_log_rich = ifelse(log_rich_std_slope > 0, TRUE, FALSE),
    inc_f_log_bm = ifelse(log_bm_std_slope > 0, TRUE, FALSE)
    ),
  ### Monotonuous station
  st_mono_trends_rich_bm = map(c("log_rich_std", "log_bm_std"),
    ~get_st_mono_trends(rigal_classification, .x)$station) %>%
    Reduce(intersect, .),
  slope_com_var_no_covar = {
    slope_com_var_no_covar <- slope_com_var[,
      !colnames(slope_com_var) %in% c("bm_std", "rich_std")
      ]
    colnames(slope_com_var_no_covar) <-
      str_remove(colnames(slope_com_var_no_covar), "_slope")
    slope_com_var_no_covar
  },
  #### Monotonuous station but with stable
  st_mono_trends_stable_rich_bm = map(c("log_rich_std", "log_bm_std"),
    ~get_st_mono_trends(rigal_classification, .x, stable = TRUE)$station) %>%
    Reduce(intersect, .),
  ### Non monotonous station
  st_trends_rich_bm = map(c("log_rich_std", "log_bm_std"),
    ~get_st_all_trends(rigal_classification, .x)$station) %>%
    Reduce(intersect, .),
  ### Non monotonous stations with stable = all stations


  ### Modelling
  model_bm_rich_mono_trends = get_model_bm_rich_no_covar_no_inc(
    .data = filter(slope_com_var_no_covar, station %in% st_mono_trends_rich_bm)
    ) %>%
  map(., lm.beta::lm.beta),
  model_bm_rich_mono_stable_trends = get_model_bm_rich_no_covar_no_inc(
    .data = filter(slope_com_var_no_covar,
      station %in% st_mono_trends_stable_rich_bm)
    ) %>%
  map(., lm.beta::lm.beta),
  model_bm_rich_trends = get_model_bm_rich_no_covar_no_inc(
    .data = filter(slope_com_var_no_covar,
      station %in% st_trends_rich_bm)
    ) %>%
  map(., lm.beta::lm.beta),
  model_bm_rich = get_model_bm_rich_no_covar_no_inc(
    .data = slope_com_var_no_covar
    ) %>%
  map(., lm.beta::lm.beta),
  vif_bm_rich_mod_mono_trends = map(
    model_bm_rich_mono_trends,
    ~try(car::vif(.x))),
  vif_bm_rich_mod_mono_stable_trends = map(
    model_bm_rich_mono_stable_trends,
    ~try(car::vif(.x))),
  vif_bm_rich_mod_trends = map(
    model_bm_rich_trends,
    ~try(car::vif(.x))),
  vif_bm_rich_mod = map(
    model_bm_rich,
    ~try(car::vif(.x))),
  resume_bm_rich_mod_mono_trends = map(
    model_bm_rich_mono_trends,
    ~summary(.x,  standardized = TRUE)),
  resume_bm_rich_mod_mono_stable_trends = map(
    model_bm_rich_mono_stable_trends,
    ~summary(.x,  standardized = TRUE)),
  resume_bm_rich_mod_trends = map(
    model_bm_rich_trends,
    ~summary(.x,  standardized = TRUE)),
  resume_bm_rich_mod = map(
    model_bm_rich,
    ~summary(.x,  standardized = TRUE)),
  anova_bm_rich_mod_mono_trends = map(
    model_bm_rich_mono_trends,
    car::Anova),
  anova_bm_rich_mod_mono_stable_trends = map(
    model_bm_rich_mono_stable_trends,
    car::Anova),
  anova_bm_rich_mod_trends = map(
    model_bm_rich_trends,
    car::Anova),
  anova_bm_rich_mod = map(model_bm_rich,
    car::Anova),

  ### Plot
  pred_bm_rich_trends = get_pred_plot_from_new_model(
    model = model_bm_rich_trends,
    dataset = filter(slope_com_var_no_covar, station %in% st_trends_rich_bm),
    x_bound = slope_x_bound
  ),
  pred_bm_rich_mono_trends = get_pred_plot_from_new_model(
    model = model_bm_rich_mono_trends,
    dataset = filter(
      slope_com_var_no_covar,
      station %in% st_mono_trends_rich_bm),
    x_bound = slope_x_bound
  ),
  pred_bm_rich_mono_stable_trends = get_pred_plot_from_new_model(
    model = model_bm_rich_mono_stable_trends,
    dataset = filter(
      slope_com_var_no_covar,
      station %in% st_mono_trends_stable_rich_bm),
    x_bound = slope_x_bound
  ),
  pred_bm_rich = get_pred_plot_from_new_model(
    model = model_bm_rich,
    dataset = slope_com_var_no_covar,
    x_bound = slope_x_bound
  ),

  ## Keep station with trends only (i.e. non monotonic)

  ### Table
  reg_table_bm_rich_mono_trends = map2(
    model_bm_rich_mono_trends,
    names(model_bm_rich_mono_trends),
    function(model, name) {
      ml <- broom::tidy(model) %>%
	mutate(response = name) %>%
	select(response, everything())
    }
    ) %>%
  do.call(rbind, .),
  reg_table_bm_rich_mono_stable_trends = map2(
    model_bm_rich_mono_stable_trends,
    names(model_bm_rich_mono_stable_trends),
    function(model, name) {
      ml <- broom::tidy(model) %>%
	mutate(response = name) %>%
	select(response, everything())
    }
    ) %>%
  do.call(rbind, .),
  reg_table_bm_rich_trends = map2(
    model_bm_rich_trends,
    names(model_bm_rich_trends),
    function(model, name) {
      ml <- broom::tidy(model) %>%
	mutate(response = name) %>%
	select(response, everything())
    }
    ) %>%
  do.call(rbind, .),
  reg_table_bm_rich = map2(
    model_bm_rich,
    names(model_bm_rich),
    function(model, name) {
      ml <- broom::tidy(model) %>%
	mutate(response = name) %>%
	select(response, everything())
    }
    ) %>%
  do.call(rbind, .),
  anova_table_bm_rich_mono_trends = map2(
    anova_bm_rich_mod_mono_trends,
    names(anova_bm_rich_mod_mono_trends),
    function(model, name) {
      ml <- broom::tidy(model) %>%
	mutate(response = name) %>%
	select(response, everything())
    }
    ) %>%
  do.call(rbind, .),
  anova_table_bm_rich_mono_stable_trends = map2(
    anova_bm_rich_mod_mono_stable_trends,
    names(anova_bm_rich_mod_mono_stable_trends),
    function(model, name) {
      ml <- broom::tidy(model) %>%
	mutate(response = name) %>%
	select(response, everything())
    }
    ) %>%
  do.call(rbind, .),
  anova_table_bm_rich_trends = map2(
    anova_bm_rich_mod_trends,
    names(anova_bm_rich_mod_trends),
    function(model, name) {
      ml <- broom::tidy(model) %>%
	mutate(response = name) %>%
	select(response, everything())
    }
    ) %>%
  do.call(rbind, .),
  anova_table_bm_rich = map2(
    anova_bm_rich_mod,
    names(anova_bm_rich_mod),
    function(model, name) {
      ml <- broom::tidy(model) %>%
	mutate(response = name) %>%
	select(response, everything())
    }
    ) %>%
  do.call(rbind, .),

  trace = TRUE
)
