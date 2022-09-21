library(targets)
lapply(list.files(here::here("R"), full.names = TRUE), source)
tar_option_set(packages = c(
    "targets", "tarchetypes", "tidyverse", "magrittr",
    "lubridate", "here", "kableExtra", "scales", "rmarkdown", "sf",
    "piecewiseSEM", "semEff", "piecewiseSEM", "DiagrammeR", "cowplot", "future", "INLA",
    "inlatools", "ggeffects"))

library(future.callr)
plan(callr)
source("./packages.R")

list(
  # Get fishcom data:
  tar_target(net_analysis_data,
    get_network_analysis_data(
      path = file_in(!!get_mypath("data", "classes", "network_analysis.rda"))
      )),
  tar_target(net_data, get_network_data(path = file_in(!!get_mypath("data", "classes", "network_metrics.rda")))),
  tar_target(com_data, get_community_data(path = file_in(!!get_mypath("data", "community_metrics.rda")))),
  tar_target(com_analysis_data, get_community_analysis_data( path =
      file_in(!!get_mypath("data", "community_analysis.rda")), op = op_data)),
  tar_target(op_data, get_op_data(path = file_in(!!get_mypath("data", "op_analysis.rda")))),
  tar_target(habitat_press, get_data(obj_name = "habitat_press", dir = get_mypath("data"))),
  tar_target(habitat_pressure, get_data(obj_name = "habitat_pressure", dir = get_mypath("data"))),
  tar_target(region_polygon, get_data(obj_name = "region_polygon", dir = get_mypath("data"))),
  tar_target(station_analysis, get_data(obj_name = "station_analysis",
      dir = "~/Documents/post-these/mnhn/fishcom/data")),
  tar_target(piel, get_piel_nind_bm(com = com_analysis_data)),

  # Compute dataset:
  tar_target(net_l_ld_IS, get_l_ld_IS(net = net_analysis_data)),
  tar_target(metrics_fishfish_only, get_network_metric_fish_only(net = net_analysis_data)),
  tar_target(net_data2,
    left_join(net_data, net_l_ld_IS, by = "opcod") %>%
    left_join(select(metrics_fishfish_only, opcod, ct_ff, l_ff, ld_ff), by = "opcod")),
  tar_target(com_data2, com_data %>%
    left_join(piel, by = "opcod")),
  tar_target(full_data, get_full_data(net = net_data2, com = com_data2, op = op_data)),
  tar_target(full_data2, add_to_full_data(.data = full_data)),
  ## Environment:
  tar_target(data_for_pca, habitat_press %>%
    select(all_of(c("station", get_var_for_pca())))),
  tar_target(pca, get_pca_environment(.data = data_for_pca)),
  tar_target(environment_pca,
    data_for_pca %>%
      mutate(
        RC1 = pca$rotated$scores[, 1],
        log_RC1 = log(RC1 + abs(min(RC1)) + 1),
        RC2 = pca$rotated$scores[, 2],
        log_RC2 = log(RC2 + abs(min(RC2)) + 1)
        ) %>%
    select(RC1, log_RC1, RC2, log_RC2, everything())),
  tar_target(basin_station, get_basin_station()),

  # Data analysis:
  tar_target(facet_var, c(get_biomass_var(), get_com_str_var())),
  tar_target(data_inla, full_data2 %>%
    left_join(basin_station, by = "station") %>%
    mutate(
      station = as.character(station),
      basin = as.character(basin),
      basin1 = as.character(basin),
      station1 = as.character(station),
      intercept_basin = as.integer(as.factor(basin)),
      intercept_basin_station = as.integer(as.factor(paste0("basin", basin, ":", "station", station)))
      )),
  tar_target(sp_for_sem, data_inla %>%
    group_by(station) %>%
    summarise(across(facet_var, median)) %>%
    left_join(environment_pca %>%
      mutate(station = as.character(station)) %>%
      select(c(station, log_RC1, log_RC2)),
    by = "station") %>%
    left_join(mutate(basin_station, station = as.character(station)), by =
      "station")),
  # Analysis 
  ## Temporal trends
  tar_target(gaussian_inla_no_drivers, tibble(
      response = facet_var,
      mod = purrr::map(facet_var, ~try(inla(
            formula = get_formula_inla(resp = .x, tau_prior = FALSE),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE,
              return.marginals=TRUE, return.marginals.predictor=TRUE),
            control.predictor = list(link = 1, compute = T),
            verbose = F,
            data = data_inla
            ))))),

  tar_target(gaussian_inla_no_drivers_effects,
    format_inla_model_list(x = gaussian_inla_no_drivers)),
  tar_target(glob_tps_trends_decade_no_drivers,
    gaussian_inla_no_drivers_effects %>%
      filter(term == "nb_year") %>%
      mutate(
        mean = map2_dbl(response, mean,
          ~compute_trends_meaningful_units(x = .y, resp = .x,
            time = 10)),
        low = map2_dbl(response, low,
          ~compute_trends_meaningful_units(x = .y, resp = .x),
          time = 10),
        high = map2_dbl(response, high,
          ~compute_trends_meaningful_units(x = .y, resp = .x),
          time = 10))),
    tar_target(gaussian_pred_station_tps, gaussian_inla_no_drivers %>%
      mutate(
        station_tps = map(mod, ~get_re_prediction_inla(.x, effect = "station1"))
        ) %>%
      select(-mod) %>%
      unnest(station_tps)),
    tar_target(gaussian_pred_station_tps10, gaussian_pred_station_tps %>%
      mutate(
        mean = map2_dbl(response, mean,
          ~compute_trends_meaningful_units(x = .y, resp = .x,
            time = 10)),
        quant0.025 = map2_dbl(response, quant0.025,
          ~compute_trends_meaningful_units(x = .y, resp = .x),
          time = 10),
        quant0.975 = map2_dbl(response, quant0.975,
          ~compute_trends_meaningful_units(x = .y, resp = .x),
          time = 10)
        )),
    tar_target(tps_for_sem, gaussian_pred_station_tps10 %>%
      select(response, station, mean, sd) %>%
      pivot_wider(everything(),
        names_from = "response", values_from = c("mean", "sd")
        )),

    ### SEM:
    tar_target(sem, list(
        lm(mean_ct_ff ~ mean_log_bm_std + mean_log_rich_std, data = tps_for_sem,
          weight = 1 / (sd_log_rich_std + sd_log_bm_std + sd_ct_ff)),
        lm(mean_w_trph_lvl_avg ~ mean_log_bm_std + mean_log_rich_std, data = tps_for_sem,
          weight = 1 / (sd_log_rich_std + sd_log_bm_std + sd_w_trph_lvl_avg)),
        lm(mean_log_bm_std ~ mean_log_rich_std, data = tps_for_sem,
          weight = 1 / (sd_log_rich_std + sd_log_bm_std)
        )
      )
    ),
  tar_target(semeff_perc_inla, get_tps_semeff(sem = sem, sem_data = tps_for_sem, ci_type = "perc")),
  tar_target(sp_sem, list(
      log_rich_std = lmer(log_rich_std ~ log_RC1 + log_RC2 + (1 | basin), data = sp_for_sem),
      log_bm_std = lmer(log_bm_std ~ log_rich_std + log_RC1 + log_RC2 + (1 | basin), data = sp_for_sem),
      ct_ff = lmer(ct_ff ~ log_rich_std + log_bm_std + log_RC1 + log_RC2 + (1 | basin), data = sp_for_sem),
      w_trph_lvl_avg = lmer(w_trph_lvl_avg ~ log_rich_std + log_bm_std + log_RC1 + log_RC2 +
        (1 | basin), data = sp_for_sem)
      )),
  tar_target(sp_semeff_perc, get_sp_semeff(sem = sp_sem, sem_data = sp_for_sem, ci_type = "perc")),

  # Model performance:

  tar_target(gaussian_inla_rand,
    gaussian_inla_no_drivers %>%
      mutate(
        hpd_random = map(
          mod,
          ~get_hpdmarginal_inla(
            inla_mod = .x,
            type = "rand"
          )
        )
        ) %>%
    select(-mod) %>%
    unnest(hpd_random)),
  tar_target(gaussian_inla_var_fitted,
    gaussian_inla_no_drivers %>%
    mutate(
      var_pred = purrr::map(mod, function(y) {
        map_dbl(y$marginals.fitted.values,
          ~var(inla.rmarginal(100, .x))
        )
        }
      )
      ) %>%
    select(-mod)),
  tar_target(r2_inla, gaussian_inla_var_fitted %>%
      left_join(get_std_inla_from_rand(inla_rand_tab = gaussian_inla_rand), by = "response") %>%
      mutate(
        r2_mvp_marg = pmap(
          list(var_pred, intercept_basin, intercept_basin_station, epsilon),
          ~r2_mvp(var_pred = ..1, std_intercept = c(..2, ..3), epsilon = ..4, type = "marginal")
          ),
        r2_mvp_cond = pmap(
          list(var_pred, intercept_basin, intercept_basin_station, epsilon),
          ~r2_mvp(var_pred = ..1, std_intercept = c(..2, ..3), epsilon = ..4, type = "conditional")
          ),
        r2_mvp_marg_mean = map_dbl(r2_mvp_marg, mean),
        r2_mvp_marg_med = map_dbl(r2_mvp_marg, median),
        r2_mvp_marg_sd = map_dbl(r2_mvp_marg, sd),
        r2_mvp_cond_mean = map_dbl(r2_mvp_cond, mean),
        r2_mvp_cond_med = map_dbl(r2_mvp_cond, median),
        r2_mvp_cond_sd = map_dbl(r2_mvp_cond, sd),
        r2_mvp_marg95hpd = map(r2_mvp_marg, ~coda::HPDinterval(coda::as.mcmc(.x))),
        r2_mvp_marg95hpdci = map2_chr(r2_mvp_marg95hpd, r2_mvp_marg_mean,
          ~paste0(
            format(round(.y, 2), nsmall = 2),
            " [",
            format(round(.x[, "lower"], 2), nsmall = 2),
            ",",
            format(round(.x[, "upper"], 2), nsmall = 2),
            "]")
          ),
        r2_mvp_cond95hpd = map(r2_mvp_cond, ~coda::HPDinterval(coda::as.mcmc(.x))),
        r2_mvp_cond95hpdci = map2_chr(r2_mvp_cond95hpd, r2_mvp_cond_mean,
          ~paste0(
            format(round(.y, 2), nsmall = 2),
            " [",
            format(round(.x[, "lower"], 2), nsmall = 2),
            ",",
            format(round(.x[, "upper"], 2), nsmall = 2),
            "]")
          ),
      )
    ),
  tar_target(r2_sem, rbind(
      rsquared(as.psem(sp_sem)) %>%
        select(Response, Marginal, Conditional) %>%
        mutate(type = "spatial"),
      rsquared(as.psem(sem)) %>%
        mutate(Marginal = R.squared, Conditional = NA, type = "temporal") %>%
        select(Response, Marginal, Conditional, type)
      ))


  #tar_target(map_st, get_station_map(st = st_mono_trends_stable_rich_bm))
  )
  #betadiv = map_dfr(
    #c("biomass", "bm_std", "nind_std", "nind"),
    #~compute_temporal_betadiv(
      #.op = op_data,
      #com = filter(com_analysis_data, !is.na(surface)),
      #variable = .x) %>%
    #select(-data, -com) %>%
    #mutate(variable = .x)
  #) %>%
  #pivot_longer(cols = c(betadiv:betadiv_bin_diag)) %>%
  #mutate(name = str_c(name, variable, sep = "_")) %>%
  #select(-variable) %>%
  #pivot_wider(names_from = "name", values_from = "value"),
  #betapart_bin =
    #get_com_mat_station(
      #com = com_analysis_data,
      #.op = op_data,
      #variable = "biomass",
      #presence_absence = TRUE) %>%
  #get_temporal_betapart_from_com_mat_station(com = .) %>%
  #select(-betapart) %>%
  #ungroup() %>%
  #unnest(be),
