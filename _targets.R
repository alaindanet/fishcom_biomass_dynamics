library(targets)
lapply(list.files(here::here("R"), full.names = TRUE), source)
tar_option_set(packages = c(
    "targets", "tarchetypes", "tidyverse", "magrittr",
    "lubridate", "here", "kableExtra", "scales", "rmarkdown", "sf", "igraph",
    "piecewiseSEM", "semEff", "piecewiseSEM", "DiagrammeR", "cowplot", "future", "INLA",
    "inlatools", "ggeffects", "corrplot", "arrow"))

library(future.callr)
plan(callr)
source("./packages.R")
theme_set(
  theme_half_open() +
    background_grid()
)

list(
  # Get fishcom data:
  tar_target(metaweb_analysis,
    get_data(obj_name = "metaweb_analysis", dir = here("data"))
    ),
  tar_target(basin_dce, {
    basin <- read_sf(here::here("data-raw", "basin_dce", "BassinDCE.shp"))
    basin %<>% st_transform(crs = 2154)
    basin %<>% filter(CdBassinDC %in%
      c("B1", "B2", "A", "D", "F", "C", "G", "H")
    )
    basin <- rmapshaper::ms_simplify(input = basin) %>%
      st_as_sf()
    basin_inner <- basin %>%
      rmapshaper::ms_innerlines() %>%
      as_tibble() %>%
      st_as_sf()
    list(basin_inner = basin_inner, basin_tot = basin)
    }),
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
  tar_target(extra_net_metrics,
    net_analysis_data %>%
      select(opcod, network) %>%
      mutate(
        node_fish_top_nb = map_int(network, get_nb_fish_node_top_consumers),
        redundancy_metrics = map_dfr(network, get_redundancy)
        ) %>%
      select(-network) %>%
      unnest(redundancy_metrics)
  ),
  tar_target(net_data2,
    left_join(net_data, net_l_ld_IS, by = "opcod") %>%
    left_join(extra_net_metrics, by = "opcod") %>%
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

  ## Bioenergetic model simulation
  tar_target(sim, open_dataset(here::here("data-raw",
        "sim_richness_carrying_capacity.arrow"),
      format = "arrow") %>%
    collect() %>%
    filter(!is.na(final_richness)) %>%
    mutate(
      consumer_biomass = total_bm - producer_bm,
      consumer_richness = final_richness - producer_richness,
      K_std = K / producer_richness
      ) %>%
    mutate(across(c(
          total_bm, final_richness,
          connectance_final, weighted_average_trophic_level),
        ~log(.x),
        .names = "log_{.col}")
      ) %>%
    mutate(
      top_consumer_nb = map_int(A, get_nb_species_top_consumers_sim),
      redundancy_metrics = map_dfr(A, get_redundancy_sim)) %>%
    unnest(redundancy_metrics) %>%
    select(-A)
    ),
  tar_target(sim_param,
    open_dataset(here::here("data-raw", "sim_richness_carrying_capacity.arrow"),
      format = "arrow") %>%
    collect() %>%
    select(sim_id, fw_id, S, C, K)
    ),
  tar_target(sim_std,
    sim %>%
      mutate(across(where(is.double), ~scale(.x)[, 1])) %>%
      mutate(across(ends_with("richness"), ~scale(.x)[, 1])) %>%
      mutate(across(c(S, K), ~scale(.x)[, 1]))
    ),
  tar_target(sim_cor_mat,
    cor(sim %>% select(is.double | ends_with("richness") | K | S)
      )),
  tar_target(sim_cor_plot, corrplot(sim_cor_mat, method = "number")),

  # Data analysis:
  tar_target(facet_var, c(get_biomass_var(), get_com_str_var())),
  tar_target(data_inla, full_data2 %>%
    select(-starts_with("piel"),
      -c(surface, biomass, richness, nbnode, weighted_connectance),
      -c(l:avg_IS),
      -c(l_ff, ld_ff),
      -c(functional_links, total_links, redundant_links, prop_functional_links)
    ) %>%
    left_join(basin_station, by = "station") %>%
    na.omit() %>%
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
        ) %>%
      rename_with(~str_remove(.x, "mean_"))
      ),

    ### SEM:
    tar_target(sem, list(
      prop_redundant_links = lm(prop_redundant_links ~ log_bm_std + log_rich_std, data = tps_for_sem,
        weight = 1 / (sd_log_rich_std + sd_log_bm_std + sd_prop_redundant_links)),
      connectance = lm(connectance ~ log_bm_std + log_rich_std, data = tps_for_sem,
        weight = 1 / (sd_log_rich_std + sd_log_bm_std + sd_connectance)),
      w_trph_lvl_avg = lm(w_trph_lvl_avg ~ log_bm_std + log_rich_std, data = tps_for_sem,
        weight = 1 / (sd_log_rich_std + sd_log_bm_std + sd_w_trph_lvl_avg)),
      log_bm_std = lm(log_bm_std ~ log_rich_std, data = tps_for_sem,
          weight = 1 / (sd_log_rich_std + sd_log_bm_std)
        )
      )
    ),
  tar_target(semeff_perc_inla, get_tps_semeff(sem = sem, sem_data = tps_for_sem, ci_type = "perc")),
  tar_target(sp_sem, list(
      #log_rich_std = lmer(log_rich_std ~ log_RC1 + log_RC2 + (1 | basin), data = sp_for_sem),
      prop_redundant_links = lmer(prop_redundant_links ~ log_rich_std + log_bm_std + (1 | basin), data = sp_for_sem),
      log_bm_std = lmer(log_bm_std ~ log_rich_std  + (1 | basin), data = sp_for_sem), #+ log_RC1 + log_RC2
      connectance = lmer(connectance ~ log_rich_std + log_bm_std + (1 | basin), data = sp_for_sem), #+ log_RC1 + log_RC2
      w_trph_lvl_avg = lmer(w_trph_lvl_avg ~ log_rich_std + log_bm_std  +
        (1 | basin), data = sp_for_sem) #+ log_RC1 + log_RC2
      )),
  tar_target(sp_semeff_perc, get_sp_semeff(sem = sp_sem, sem_data = sp_for_sem, ci_type = "perc")),
  tar_target(sp_semeff_tab, from_semEff_to_table(sp_semeff_perc)),
  tar_target(semeff_tab, from_semEff_to_table(semeff_perc_inla)),
  tar_target(semeff_tot, rbind(tibble(semeff_tab, model = "temporal"),
      tibble(sp_semeff_tab, model = "spatial")
      )),

    ### Bioenergetic model
  tar_target(bioener_ks_mod_list,
    list(
      log_total_bm = lm(log_total_bm ~ log_final_richness + S + K, sim),
      log_weighted_average_trophic_level = lm(log_weighted_average_trophic_level ~ log_total_bm + log_final_richness + S + K, sim),
      log_connectance_final = lm(log_connectance_final ~ log_total_bm + log_final_richness + S + K, sim)
      )),
  tar_target(sem_bioener_ks, as.psem(bioener_ks_mod_list)),
  tar_target(semeff_bioener_ks,
    semEff(sem_bioener_ks, R = 100, ci.type = "perc", data = sim)
    ),
  tar_target(semeff_bioener_ks_tab,
    from_semEff_to_table(x = semeff_bioener_ks)
    ),
  tar_target(bioener_bm_rich_mod_list,
    list(
      log_total_bm = lm(log_total_bm ~ log_final_richness, sim),
      log_weighted_average_trophic_level = lm(log_weighted_average_trophic_level ~ log_total_bm + log_final_richness, sim),
      log_connectance_final = lm(log_connectance_final ~ log_total_bm + log_final_richness, sim),
      prop_redundant_links = lm(prop_redundant_links ~ log_total_bm + log_final_richness, sim)
      )
    ),
  tar_target(sem_bioener_bm_rich, as.psem(bioener_bm_rich_mod_list)),
  tar_target(semeff_bioener_bm_rich,
    semEff(sem_bioener_bm_rich, R = 100, ci.type = "perc", data = sim)
    ),
  tar_target(semeff_bioener_bm_rich_tab,
    from_semEff_to_table(x = semeff_bioener_bm_rich)
    ),

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
        select(Response, Marginal, Conditional, type),
      rsquared(as.psem(sem_bioener_bm_rich)) %>%
        mutate(Marginal = R.squared, Conditional = NA, type = "theoretical model") %>%
        select(Response, Marginal, Conditional, type)
      )),
  tar_target(r2_sem_tab,
    r2_sem %>%
      select(
        Type = type,
        Response,
        `Marg. R2` = Marginal,
        `Cond. R2` = Conditional
        ) %>%
    arrange(Type, Response) %>%
    mutate(
      Response = str_remove_all(Response, "mean_"),
      Response = var_replacement()[Response],
      Type = str_to_sentence(Type)
      ) %>%
    mutate(across(c(`Marg. R2`, `Cond. R2`), ~round(., 2)))
  ),
  tar_target(tabvif,{
    vif_tps_sem <- car::vif(sem[[1]])
    vif_tps_sem <- setNames(round(vif_tps_sem, 2), names(vif_tps_sem))
    vif_sp_sem <- round(car::vif(sp_sem[["connectance"]]), 2)
    vif_bioener_sem <- round(car::vif(bioener_bm_rich_mod_list[["log_connectance_final"]]), 2)
    rbind(
      tibble(Model = "temporal", enframe(vif_tps_sem)),
      tibble(Model = "spatial", enframe(vif_sp_sem)),
      tibble(Model = "theoretical model", enframe(vif_bioener_sem))
      ) %>%
    mutate(
      Model = str_to_sentence(Model),
      name = var_replacement()[name]
      ) %>%
    pivot_wider(names_from = "name", values_from = "value")
  }
  ),

  # Figure
  ## Direct effect SEM temporal
  tar_target(p_list_sem_tps,
    list(
      p_bm_rich = tps_for_sem %>%
        ggplot(aes(
            x = log_rich_std,
            y = log_bm_std
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(intercept = coef(sem[["log_bm_std"]])[1], slope = coef(sem[["log_bm_std"]])[2], size = 1) +
        labs(x = "Species richness trend\n(% by decade)", y = "Biomass trend\n(% by decade)"),
      p_ct_rich = tps_for_sem %>%
        ggplot(aes(
            x = log_rich_std,
            y = connectance,
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(intercept = coef(sem[["connectance"]])["(Intercept)"] + mean(tps_for_sem$log_bm_std) * coef(sem[["connectance"]])["log_bm_std"],
          slope = coef(sem[["connectance"]])["log_rich_std"], size = 1) +
        labs(x = "Species richness trend\n(% by decade)", y = "Connectance trend\n(by decade)"),
      p_ct_bm = tps_for_sem %>%
        ggplot(aes(
            x = log_bm_std,
            y = connectance,
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = coef(sem[["connectance"]])["(Intercept)"] + mean(tps_for_sem$log_rich_std) * coef(sem[["connectance"]])["log_rich_std"],
          slope = coef(sem[["connectance"]])["log_bm_std"], size = 1) +
        labs(x = "Biomass trend\n(% by decade)", y = "Connectance trend\n(by decade)"),
      p_tlvl_rich = tps_for_sem %>%
        ggplot(aes(
            x = log_rich_std,
            y = w_trph_lvl_avg,
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(intercept = coef(sem[["w_trph_lvl_avg"]])["(Intercept)"] + mean(tps_for_sem$log_bm_std) * coef(sem[["w_trph_lvl_avg"]])["log_bm_std"],
          slope = coef(sem[["w_trph_lvl_avg"]])["log_rich_std"], size = 1) +
        labs(x = "Species richness trend\n(% by decade)", y = "Avg trophic level trend\n(by decade)"),
      p_tlvl_bm = tps_for_sem %>%
        ggplot(aes(
            x = log_bm_std,
            y = w_trph_lvl_avg,
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = coef(sem[["w_trph_lvl_avg"]])["(Intercept)"] + mean(tps_for_sem$log_rich_std) * coef(sem[["w_trph_lvl_avg"]])["log_rich_std"],
          slope = coef(sem[["w_trph_lvl_avg"]])["log_bm_std"], size = 1) +
        labs(
          x = "Biomass trend\n(% by decade)",
          y = "Avg trophic level trend\n(by decade)"),
      p_redundant_rich = tps_for_sem %>%
        ggplot(aes(
            x = log_rich_std,
            y = prop_redundant_links,
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(intercept = coef(sem[["prop_redundant_links"]])["(Intercept)"] + mean(tps_for_sem$log_bm_std) * coef(sem[["prop_redundant_links"]])["log_bm_std"],
          slope = coef(sem[["prop_redundant_links"]])["log_rich_std"], size = 1) +
        labs(x = "Species richness trend\n(% by decade)", y = "Redundancy trend"),
      p_redundant_bm = tps_for_sem %>%
        ggplot(aes(
            x = log_bm_std,
            y = prop_redundant_links
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = coef(sem[["prop_redundant_links"]])["(Intercept)"] + mean(tps_for_sem$log_rich_std) * coef(sem[["prop_redundant_links"]])["log_rich_std"],
          slope = coef(sem[["prop_redundant_links"]])["log_bm_std"], size = 1) +
        labs(
          x = "Biomass trend\n(% by decade)",
          y = "Redundancy trend")
    )
    ),
  tar_target(p_sem_tps_right, {

    theme_set(theme_half_open() + background_grid())

    del_x <- function(x) {
      theme(axis.title.x = element_blank(), axis.text.x = element_blank())
    }

    plot_grid(
      NULL, p_list_sem_tps$p_bm_rich + del_x(),
      p_list_sem_tps$p_ct_bm + del_x(), p_list_sem_tps$p_ct_rich + del_x(),
      p_list_sem_tps$p_tlvl_bm + del_x(), p_list_sem_tps$p_tlvl_rich + del_x(),
      p_list_sem_tps$p_redundant_bm, p_list_sem_tps$p_redundant_rich,
      ncol = 2,
      labels = c("", letters[2:8]),
      align = "v"
    )

    }),
  tar_target(p_sem_tps_biplot, {
    theme_set(theme_half_open() + background_grid())
    plot_grid(
      NULL, p_list_sem_tps$p_ct_bm, p_list_sem_tps$p_tlvl_bm, p_list_sem_tps$p_redundant_bm,
      p_list_sem_tps$p_bm_rich, p_list_sem_tps$p_ct_rich, p_list_sem_tps$p_tlvl_rich,
      p_list_sem_tps$p_redundant_rich,
      ncol = 4, byrow = TRUE,
      labels = c("", letters[2:8]),
      align = "v",
      label_x = 0.1
      ) }),
  tar_target(p_sem_tps,
    plot_grid(
      ggdraw() + draw_image(here::here("figures", "sem_tps2.png"), scale = .9),
      p_sem_tps_biplot,
      nrow = 2, rel_heights = c(1, 1), labels = c("a", "")
      )),
  tar_target(p_sem_tps_file,
    ggsave(
      filename = here::here("figures", "fig_sem_tps.png"),
      p_sem_tps,
      scale = 2.6,
      units = c("mm"),
      width = 120,
      height = 120 * .8),
    format = "file"
    ),
  ## SEM spatial temporal theoretical
  tar_target(semeff_tot_ok,
    rbind(
      semeff_tot %>%
        filter(effect_type %in% c("direct", "total")),
      semeff_bioener_bm_rich_tab %>%
        filter(effect_type %in% c("direct", "total")) %>%
        mutate(model = "Theoretical model")
      ) %>%
    mutate(across(c(response, predictor),
        ~var_replacement()[str_remove(.x, "mean_")]
        )) %>%
    mutate(
      effect_type = str_to_sentence(effect_type),
      model = str_to_sentence(model)
    )
    ),
  tar_target(p_semeff_tot_ok,
    semeff_tot_ok %>%
      mutate(
        response = recode_factor(response, !!!var_replacement()),
        model = factor(model, levels = c("Temporal", "Spatial", "Theoretical model"))) %>%
    ggplot(aes(x = predictor, y = effect, ymin = lower_ci, ymax = upper_ci, color = model, shape = effect_type)) +
    geom_pointrange(position = position_dodge(width = 0.4)) +
    facet_grid(cols = vars(response), switch = "y") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Predictor",
      y = expression(paste("Standardized slope coefficients ", r[delta])),
      color = "Model",
      shape = "Effect") +
    theme_cowplot() +
    background_grid(minor = "y") +
    theme(
      legend.position = "bottom",
      strip.placement = "outside"
      )),
  tar_target(p_sem_tps_sp,
    semeff_tot %>%
      filter(effect_type != "mediators",
        !predictor %in% c("log_RC1", "log_RC2")) %>%
      mutate(
        response = str_remove(response, "mean_"),
        response = var_replacement()[response],
        predictor = str_remove(predictor, "mean_"),
        predictor = var_replacement()[predictor],
        effect_type = str_to_sentence(effect_type),
        model = str_to_sentence(model)
        ) %>%
      ggplot(aes(y = predictor, x = effect, colour = response, shape = model)) +
      geom_point(
        position = position_dodge(width = 0.4), size = 4) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_linerange(
        aes(xmin = lower_ci, xmax = upper_ci),
        size = 1,
        position = position_dodge(width = 0.4)
        ) +
      facet_grid(cols = vars(effect_type)) +
      labs(
        y = "Predictor",
        x = expression(paste("Standardized slope coefficients ", r[delta])),
        color = "Foodweb metric", shape = "Model") +
      theme_cowplot() +
      background_grid() +
      theme(legend.position = "bottom", strip.background = element_blank())
    ),
  tar_target(p_semeff_tot_ok_file,
    ggsave(
      filename = here::here("figures", "p_semeff_tot_ok.png"),
      p_semeff_tot_ok,
      scale = 2.6,
      units = c("mm"),
      width = 120,
      height = 120 * .4),
    format = "file"
    ),

  # Supplementary material
  tar_target(obs_fit,
      map2_dfr(gaussian_inla_no_drivers$mod, gaussian_inla_no_drivers$response,
        ~plot_obs_fitted_inla(
          mod_inla = .x,
          dataset = data_inla,
          resp = .y,
          pred_nrows = NULL,
          return_df = TRUE
        )
      )
    ),
  tar_target(p_obs_fit,
    obs_fit %>%
      filter(
        response %in% c("log_bm_std", "ct_ff", "w_trph_lvl_avg", "log_rich_std", "prop_redundant_links")
        ) %>%
      mutate(response = var_replacement()[response]) %>%
      ggplot(aes(x = obs, y = fit)) +
      geom_point(alpha = .3, color = "gray70") +
      geom_abline(slope = 1, intercept = 0, size = 1) +
      geom_text(data = r2_obs_fit,
        aes(label = lab), parse = TRUE,
        vjust = 1, hjust = 0
        ) +
      facet_wrap(vars(response), scales = "free", ncol = 2) +
      labs(x = "Observed values", y = "Fitted values") +
      geom_rug(alpha = .05) +
      theme_half_open() +
      theme(strip.background = element_blank())
    ),
  tar_target(r2_obs_fit,
    obs_fit %>%
      filter(
        response %in% c("log_bm_std", "ct_ff", "w_trph_lvl_avg", "log_rich_std", "prop_redundant_links")
        ) %>%
      mutate(response = var_replacement()[response]) %>%
      group_by(response) %>%
      summarise(
        cor = round(cor(y = fit, x = obs, use = "complete.obs"), 2),
        lab = paste0("~~~~rho == ", cor),
        obs = -Inf,#(max(obs) - min(obs)) * .10,
        fit = Inf#(max(fit) - min(fit)) * .90
        )
      ),
  tar_target(p_hist_site_tps10, {
    sum_tps_10 <- gaussian_pred_station_tps10 %>%
      filter(response %in% c("log_bm_std", "connectance",
          "w_trph_lvl_avg", "log_rich_std", "prop_redundant_links")) %>%
      mutate(response = var_replacement()[response]) %>%
      group_by(response) %>%
      summarise(avg = mean(mean), med = median(mean)) %>%
      pivot_longer(-response, names_to = "metric", values_to = "values")

    gaussian_pred_station_tps10 %>%
      filter(response %in% c("log_bm_std", "connectance", "w_trph_lvl_avg", "log_rich_std",
          "prop_redundant_links")) %>%
      mutate(response = var_replacement()[response]) %>%
      ggplot(aes(x = mean)) +
      geom_histogram(fill = "lightblue", bins = 60) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
      geom_vline(data = filter(sum_tps_10, metric == "med"), aes(xintercept = values)) + #, color = metric
      labs(x = "Temporal trends by decade", y = "Number of sites") +
        scale_x_continuous(n.breaks = 10) +
        facet_wrap(vars(response), scales = "free_x",
          ncol = 2, strip.position = "top") +
        theme_half_open() +
        theme(strip.background = element_blank())
    }),
  tar_target(r2_inla_tab,
    r2_inla %>%
      filter(
        response %in% c("log_bm_std", "connectance", "w_trph_lvl_avg", "log_rich_std", "prop_redundant_links")
        ) %>%
    mutate(response = var_replacement()[response]) %>%
    select(
      Response = response,
      `Marg. Rsq` = r2_mvp_marg95hpdci,
      `Cond. Rsq` = r2_mvp_cond95hpdci
    )
    ),
  tar_target(tab_inla_rand,
    gaussian_inla_rand %>%
      filter(
        ci_level == "level:0.95",
        response %in% c("log_bm_std", "connectance", "w_trph_lvl_avg", "log_rich_std", "prop_redundant_links")
        ) %>%
    select(-ci_level) %>%
    mutate(across(c(mean, low, high), ~round(., 3))) %>%
    mutate(
      response = var_replacement()[response],
      term = str_remove(term, "Precision for the |Precision for |"),
      term = replacement_random_term()[term],
      ci = paste0(mean, " [", high,",", low,"]")
      ) %>%
    arrange(response, desc(term)) %>%
    select(
      `Response` = response,
      `Term` = term,
      `S.D.` = ci
      )
    ),
  tar_target(tab_inla_r2_rand,
    gaussian_inla_rand %>%
      filter(
        ci_level == "level:0.95",
        response %in% c("log_bm_std", "connectance", "w_trph_lvl_avg", "log_rich_std", "prop_redundant_links")
        ) %>%
    select(-ci_level) %>%
    mutate(across(c(mean, low, high), ~round(., 3))) %>%
    mutate(
      response = var_replacement()[response],
      term = str_remove(term, "Precision for the |Precision for |"),
      term = replacement_random_term()[term],
      ci = paste0(mean, " [", high,",", low,"]")
      ) %>%
    arrange(response, desc(term)) %>%
    select(
      `Response` = response,
      `Term` = term,
      `S.D.` = ci
      ) %>%
    left_join(r2_inla_tab, by = "Response") %>%
    select(1, 4, 5, 2, 3)
    ),
  tar_target(p_corr, {
    cor_tps <- tps_for_sem %>%
      select(c(log_bm_std, connectance, w_trph_lvl_avg, log_rich_std,
          prop_pisc_node, prop_pisc_rich, prop_redundant_links)) %>%
      rename_with(~var_replacement()[.x]) %>%
      cor(., use = "complete.obs")

    cor_sp <- sp_for_sem %>%
      select(c(log_bm_std, connectance, w_trph_lvl_avg, log_rich_std,
          prop_pisc_node, prop_pisc_rich, prop_redundant_links)) %>%
    rename_with(~var_replacement()[.x]) %>%
    cor(., use = "complete.obs")

    cor_model <- sim %>%
      select(c(log_total_bm, log_connectance_final,
          log_weighted_average_trophic_level, log_final_richness,
          prop_redundant_links)) %>%
    rename_with(~var_replacement()[.x]) %>%
    cor(., use = "complete.obs")

    p_cor_tps <- ggcorrplot(cor_tps,
      hc.order = FALSE,
      type = "upper",
      tl.srt = 20,
      lab = TRUE)

    p_cor_sp <- ggcorrplot(cor_sp,
      hc.order = FALSE,
      type = "upper",
      tl.srt = 20,
      lab = TRUE)

    p_cor_model <- ggcorrplot(cor_model,
      hc.order = FALSE,
      type = "upper",
      tl.srt = 20,
      lab = TRUE)

    p_corr <- plot_grid(
      p_cor_tps + theme(legend.position = "none"),
      p_cor_sp + theme(legend.position = "none"),
      p_cor_model + theme(legend.position = "none"),
      get_legend(p_cor_tps),
      ncol = 2,
      rel_widths = c(1, 1, 1,.1),
      labels = c("a", "b", "c", ""))
  }),
tar_target(semeff_tot_tab,
  semeff_tot_ok %>%
    mutate(across(c(effect, lower_ci, upper_ci), ~round(., 2))) %>%
    mutate(
      effect = paste0(effect, " [", lower_ci,"; ", upper_ci,"]")
      ) %>%
    mutate(
      model = recode_factor(model, !!!effect_type_var())) %>%
    select(model, response, `Effect type` = effect_type, predictor, effect) %>%
    rename_with(~str_to_sentence(.x))
  ),
  ## Random network extinction
  tar_target(node_tlvl,
    setNames(NetIndices::TrophInd(metaweb_analysis$metaweb)$TL,
    colnames(metaweb_analysis$metaweb))
    ),
  tar_target(node_seq, {
    node_dec_tlvl <- sort(node_tlvl, decreasing = TRUE)
    node_inc_tlvl <- sort(node_tlvl, decreasing = FALSE)
    list(
      node_dec_tlvl = node_dec_tlvl,
      node_inc_tlvl = node_inc_tlvl,
      node_fish_dec_tlvl = node_dec_tlvl[fish(names(node_dec_tlvl))],
      node_fish_inc_tlvl = node_inc_tlvl[fish(names(node_inc_tlvl))]
    )
    }),
  tar_target(p_ext_node,
    extinction_simulation_node_inc_dec_lg %>%
      filter(metric %in% c("ct", "avg_tl", "prop_redundant_links")) %>%
      mutate(
        metric = recode_factor(metric, !!!var_replacement()),
        type = str_to_sentence(type)
        ) %>%
      ggplot(aes(x = nb_extinct_node, y = value, color = type, alpha = type)) +
      geom_line() +
      geom_point(
        data = extinction_simulation_node_random_lg %>%
          filter(metric %in% c("ct", "avg_tl", "prop_redundant_links")) %>%
          mutate(
            metric = recode_factor(metric, !!!var_replacement()),
            type = str_to_sentence(type)
            )) +
      geom_smooth(data = extinction_simulation_node_random_lg %>%
          filter(metric %in% c("ct", "avg_tl", "prop_redundant_links")) %>%
          mutate(
            metric = recode_factor(metric, !!!var_replacement()),
            type = str_to_sentence(type)
            ), color = "black") +
      scale_color_manual(values = c("Increasing" = "#FDE725FF", "Decreasing" = "#482677FF", "Random" = "grey")) +
      scale_alpha_manual(values = c("Increasing" = 1, "Decreasing" = 1, "Random" = 0.12), guide = "none") +
      facet_wrap(~metric, scale = "free_y", strip.position = "left") +
      labs(y = "Network metric values", x = "Number of node removed",
        color = "Trophic level scenario") +
      cowplot::theme_half_open() +
      theme(
        legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.y = element_blank()
      )
    ),
  tar_target(fish_species_tlvl,
    fish_species_tlvl <- node_tlvl %>%
      enframe(name = "node", value = "tlvl") %>%
      mutate(species = get_species(node)[, 1]) %>%
      filter(str_detect(species, "[A-Z]+")) %>%
      group_by(species) %>%
      summarise(tlvl_med = median(tlvl)) %>%
      arrange(desc(tlvl_med)) %>%
      deframe()
    ),
  tar_target(extinction_simulation_species_increasing,
    extinction_simulation(
      metaweb_analysis$metaweb,
      species_names = names(sort(fish_species_tlvl, decreasing = FALSE))
      ) %>%
    bind_rows()
    ),
  tar_target(extinction_simulation_species_decreasing,
    extinction_simulation(
      metaweb_analysis$metaweb,
      species_names = names(sort(fish_species_tlvl, decreasing = TRUE))
      ) %>%
    bind_rows()),
  tar_target(extinction_simulation_species_inc_dec_lg,
    rbind(
      extinction_simulation_species_increasing %>%
        arrange(iteration) %>%
        mutate(nb_extinct_species = cumsum(nb_species_extinction),
          nb_extinct_node = cumsum(nb_node_extinction),
          type = "increasing"),
      extinction_simulation_species_decreasing %>%
        arrange(iteration) %>%
        mutate(nb_extinct_species = cumsum(nb_species_extinction),
          nb_extinct_node = cumsum(nb_node_extinction),
          type = "decreasing")) %>%
    pivot_longer(
      cols = -c(nb_node_extinction, nb_species_extinction,
        iteration, nb_extinct_species,
        nb_extinct_node, type),
      names_to = "metric",
      values_to = "value")
    ),
  tar_target(extinction_simulation_species_random,
    furrr::future_map_dfr(1:50, function (id_sim) {
      extinction_simulation(metaweb_analysis$metaweb,
        species_names = names(sort(fish_species_tlvl, decreasing = TRUE)),
        random = TRUE, seed = id_sim) %>%
      bind_rows() %>%
      mutate(id_sim = id_sim, seed = id_sim)
  })),
  tar_target(extinction_simulation_species_random_lg,
    extinction_simulation_species_random %>%
      group_by(id_sim) %>%
      arrange(iteration) %>%
      mutate(
        nb_extinct_species = cumsum(nb_species_extinction),
        nb_extinct_node = cumsum(nb_node_extinction),
        type = "random"
        ) %>%
      pivot_longer(
        cols = -c(nb_node_extinction, nb_species_extinction,
          iteration, nb_extinct_species, nb_extinct_node,
          type, id_sim, seed),
        names_to = "metric", values_to = "value")),
  tar_target(extinction_simulation_node_increasing,
    extinction_simulation(
      metaweb_analysis$metaweb,
      species_names = names(sort(node_seq$node_fish_inc_tlvl, decreasing = FALSE))
      ) %>%
    bind_rows()
    ),
  tar_target(extinction_simulation_node_decreasing,
    extinction_simulation(
      metaweb_analysis$metaweb,
      species_names = names(sort(node_seq$node_fish_dec_tlvl, decreasing = TRUE))
      ) %>%
    bind_rows()
    ),
  tar_target(extinction_simulation_node_inc_dec_lg,
    rbind(
      extinction_simulation_node_increasing %>%
        arrange(iteration) %>%
        mutate(nb_extinct_species = cumsum(nb_species_extinction),
          nb_extinct_node = cumsum(nb_node_extinction),
          type = "increasing"),
      extinction_simulation_node_decreasing %>%
        arrange(iteration) %>%
        mutate(nb_extinct_species = cumsum(nb_species_extinction),
          nb_extinct_node = cumsum(nb_node_extinction),
          type = "decreasing")
        ) %>%
    pivot_longer(
      cols = -c(nb_node_extinction, nb_species_extinction,
        iteration, nb_extinct_species,
        nb_extinct_node, type),
      names_to = "metric",
      values_to = "value")
    ),
  tar_target(extinction_simulation_node_random,
    furrr::future_map_dfr(1:50, function (id_sim) {
      extinction_simulation(metaweb_analysis$metaweb,
        species_names = names(sort(node_seq$node_fish_dec_tlvl, decreasing = TRUE)),
        random = TRUE, seed = id_sim) %>%
      bind_rows() %>%
      mutate(id_sim = id_sim, seed = id_sim)
  })

    ),
  tar_target(extinction_simulation_node_random_lg,
    extinction_simulation_node_random %>%
      group_by(id_sim) %>%
      arrange(iteration) %>%
      mutate(
        nb_extinct_species = cumsum(nb_species_extinction),
        nb_extinct_node = cumsum(nb_node_extinction),
        type = "random"
        ) %>%
      pivot_longer(
        cols = -c(nb_node_extinction, nb_species_extinction,
          iteration, nb_extinct_species, nb_extinct_node,
          type, id_sim, seed),
        names_to = "metric", values_to = "value")),
  tar_target(p_ext_sp,
    extinction_simulation_species_inc_dec_lg %>%
      filter(metric %in% c("ct", "avg_tl", "prop_redundant_links")) %>%
      mutate(
        metric = recode_factor(metric, !!!var_replacement()),
        type = str_to_sentence(type)
        ) %>%
      ggplot(aes(x = nb_extinct_species, y = value, color = type, alpha = type)) +
      geom_line() +
      geom_point(
        data = extinction_simulation_species_random_lg %>%
          filter(metric %in% c("ct", "avg_tl", "prop_redundant_links")) %>%
          mutate(
            metric = recode_factor(metric, !!!var_replacement()),
            type = str_to_sentence(type)
            )) +
      geom_smooth(data = extinction_simulation_species_random_lg %>%
        filter(metric %in% c("ct", "avg_tl", "prop_redundant_links")) %>%
        mutate(
          metric = recode_factor(metric, !!!var_replacement()),
          type = str_to_sentence(type)
          ), color = "black") +
      scale_color_manual(values = c("Increasing" = "#FDE725FF", "Decreasing" = "#482677FF", "Random" = "grey")) +
      scale_alpha_manual(values = c("Increasing" = 1, "Decreasing" = 1, "Random" = 0.12), guide = "none") +
      facet_wrap(~metric, scale = "free_y", strip.position = "left") +
      labs(y = "Network metric values", x = "Number of species removed",
        color = "Trophic level scenario") +
      cowplot::theme_half_open() +
      theme(
        legend.position = "bottom",
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.y = element_blank()
      )
    ),
  tar_target(p_tot_ext_sim,
    plot_grid(
      p_ext_sp + theme(legend.position = "none"),
      p_ext_node + theme(legend.position = "none"),
      get_legend(p_ext_node + theme(legend.position = "bottom")),
      nrow = 3,
      labels = c("a", "b", ""),
      rel_heights = c(1, 1, .1)
    )
    ),
  tar_target(p_sem_tot_ext_sp,
    plot_grid(
      p_semeff_tot_ok,
      p_ext_sp,
      rel_heights = c(1, .8),
      nrow = 2, labels = "auto"
    )),
  tar_target(figSext,
    ggsave_multiple(
      paste0("p_ext_node", c(".png", ".pdf")),
      plot = p_ext_node,
      path = here::here("figures"),
      scale = 2.4,
      units = c("mm"),
      width = 120,
      height = 120 * .35)
    ),
  ## Bioenergetic model
  tar_target(p_sem_list_sim,
    list(
      p_bm_rich = sim %>%
        ggplot(aes(
            x = log_final_richness,
            y = log_total_bm)) +
        geom_point(shape = 1, size = 2) +
        #geom_boxplot() +
        geom_abline(
          intercept = coef(bioener_bm_rich_mod_list[["bm"]])["(Intercept)"],
          slope = coef(bioener_bm_rich_mod_list[["bm"]])["log_final_richness"], size = 1) +
        labs(x = "Species richness", y = "Total Biomass"),
      p_tlvl_rich = sim %>%
        ggplot(aes(
            x = log_final_richness,
            y = log_weighted_average_trophic_level
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = coef(bioener_bm_rich_mod_list[["tlvl"]])["(Intercept)"] +
            mean(sim$log_total_bm) * coef(bioener_bm_rich_mod_list[["tlvl"]])["log_total_bm"],
          slope = coef(bioener_bm_rich_mod_list[["tlvl"]])["log_final_richness"], size = 1) +
        labs(x = "Species richness", y = "Avg trophic level"),
      p_tlvl_bm = sim %>%
        ggplot(aes(
            x = log_total_bm,
            y = log_weighted_average_trophic_level
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = coef(bioener_bm_rich_mod_list[["tlvl"]])["(Intercept)"] +
            mean(sim$log_final_richness) * coef(bioener_bm_rich_mod_list[["tlvl"]])["log_final_richness"],
          slope = coef(bioener_bm_rich_mod_list[["tlvl"]])["log_total_bm"], size = 1) +
        labs( x = "Total biomass", y = "Avg trophic_level"),
      p_ct_rich = sim %>%
        ggplot(aes(
            x = log_final_richness,
            y = log_connectance_final
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = coef(bioener_bm_rich_mod_list[["ct"]])["(Intercept)"] +
            mean(sim$log_total_bm) * coef(bioener_bm_rich_mod_list[["ct"]])["log_total_bm"],
          slope = coef(bioener_bm_rich_mod_list[["ct"]])["log_final_richness"], size
          = 1) +
        labs(x = "Species richness", y = "Connectance"),
      p_ct_bm = sim %>%
        ggplot(aes(
            x = log_total_bm,
            y = log_connectance_final
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = coef(bioener_bm_rich_mod_list[["ct"]])["(Intercept)"] +
            mean(sim$log_final_richness) * coef(bioener_bm_rich_mod_list[["ct"]])["log_final_richness"],
          slope = coef(bioener_bm_rich_mod_list[["ct"]])["log_total_bm"], size = 1) +
        labs(x = "Total biomass", y = "Connectance"),
      p_redundant_bm = sim %>%
        ggplot(aes(
            x = log_total_bm,
            y = prop_redundant_links
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = coef(bioener_bm_rich_mod_list[["prop_redundant_links"]])["(Intercept)"] +
            mean(sim$log_final_richness) * coef(bioener_bm_rich_mod_list[["prop_redundant_links"]])["log_final_richness"],
          slope = coef(bioener_bm_rich_mod_list[["prop_redundant_links"]])["log_total_bm"], size
          = 1) +
        labs(x = "Total biomass", y = "Redundancy"),
      p_redundant_rich = sim %>%
        ggplot(aes(
            x = log_final_richness,
            y = prop_redundant_links
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = coef(bioener_bm_rich_mod_list[["prop_redundant_links"]])["(Intercept)"] +
            mean(sim$log_total_bm) * coef(bioener_bm_rich_mod_list[["prop_redundant_links"]])["log_total_bm"],
          slope = coef(bioener_bm_rich_mod_list[["prop_redundant_links"]])["log_final_richness"], size = 1) +
        labs(x = "Species richness", y = "Redundancy")
      ) %>%
    map(., ~.x + theme_half_open() + background_grid())
  ),
  tar_target(p_sem_sim, {
    theme_set(theme_half_open() + background_grid())
    plot_grid(
      NULL, p_sem_list_sim$p_ct_bm, p_sem_list_sim$p_tlvl_bm, p_sem_list_sim$p_redundant_bm,
      p_sem_list_sim$p_bm_rich, p_sem_list_sim$p_ct_rich, p_sem_list_sim$p_tlvl_rich, p_sem_list_sim$p_redundant_rich,
      nrow = 2, byrow = TRUE,
      align = "v",
      label_x = 0.1
    )
  }
    ),
  tar_target(fig_bioener_sem,
    ggsave(
      filename = here::here("figures", "p_sem_sim.png"),
      p_sem_sim,
      scale = 2.6,
      units = c("mm"),
      width = 120,
      height = 120 * .4),
    format = "file"
  ),
  ## Spatial
  tar_target(p_sem_sp_biplot, {
    avg_coef_sp <- function(d = sp_sem,
      y = "w_trph_lvl_avg",
      x = "(Intercept)") {
      mean(coef(d[[y]])$basin[[x]])
    }
    list(
      p_bm_rich = sp_for_sem %>%
        ggplot(aes(
            x = log_rich_std,
            y = log_bm_std)) +
        geom_point(shape = 1, size = 2) +
        #geom_boxplot() +
        geom_abline(
          intercept = avg_coef_sp(y = "log_bm_std", x = "(Intercept)"),
          slope = avg_coef_sp(y = "log_bm_std", x = "log_rich_std"), size = 1) +
        labs(x = "Species richness", y = "Total Biomass"),
      p_tlvl_rich = sp_for_sem %>%
        ggplot(aes(
            x = log_rich_std,
            y = w_trph_lvl_avg
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = avg_coef_sp(y = "w_trph_lvl_avg", x = "(Intercept)") +
            mean(sp_for_sem$log_bm_std) *
            avg_coef_sp(y = "w_trph_lvl_avg", x = "log_bm_std"),
          slope = avg_coef_sp(y = "w_trph_lvl_avg", x = "log_rich_std"), size = 1) +
        labs(x = "Species richness", y = "Avg trophic level"),
      p_tlvl_bm = sp_for_sem %>%
        ggplot(aes(
            x = log_bm_std,
            y = w_trph_lvl_avg
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = avg_coef_sp(y = "w_trph_lvl_avg", x = "(Intercept)") +
            mean(sp_for_sem$log_rich_std) *
            avg_coef_sp(y = "w_trph_lvl_avg", x = "log_rich_std"),
          slope = avg_coef_sp(y = "w_trph_lvl_avg", x = "log_bm_std"), size = 1) +
        labs(x = "Total biomass", y = "Avg trophic_level"),
      p_ct_rich = sp_for_sem %>%
        ggplot(aes(
            x = log_rich_std,
            y = connectance
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = avg_coef_sp(y = "connectance", x = "(Intercept)") +
            mean(sp_for_sem$log_bm_std) *
            avg_coef_sp(y = "connectance", x = "log_bm_std"),
          slope = avg_coef_sp(y = "connectance", x = "log_rich_std"), size
          = 1) +
        labs(x = "Species richness", y = "Connectance"),
      p_ct_bm = sp_for_sem %>%
        ggplot(aes(
            x = log_bm_std,
            y = connectance
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = avg_coef_sp(y = "connectance", x = "(Intercept)") +
            mean(sp_for_sem$log_rich_std) *
            avg_coef_sp(y = "connectance", x = "log_rich_std"),
          slope = avg_coef_sp(y = "connectance", x = "log_bm_std"), size = 1) +
        labs(x = "Total biomass", y = "Connectance"),
      p_redundant_bm = sp_for_sem %>%
        ggplot(aes(
            x = log_bm_std,
            y = prop_redundant_links
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = avg_coef_sp(y = "prop_redundant_links", x = "(Intercept)") +
            mean(sp_for_sem$log_rich_std) *
            avg_coef_sp(y = "prop_redundant_links", x = "log_rich_std"),
          slope = avg_coef_sp(y = "prop_redundant_links", x = "log_bm_std"), size
          = 1) +
        labs(x = "Total biomass", y = "Redundancy"),
      p_redundant_rich = sp_for_sem %>%
        ggplot(aes(
            x = log_rich_std,
            y = prop_redundant_links
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = avg_coef_sp(y = "prop_redundant_links", x = "(Intercept)") +
            mean(sp_for_sem$log_bm_std) *
            avg_coef_sp(y = "prop_redundant_links", x = "log_bm_std"),
          slope = avg_coef_sp(y = "prop_redundant_links", x = "log_rich_std"), size = 1) +
        labs(x = "Species richness", y = "Redundancy")
      ) %>%
    map(., ~.x + theme_half_open() + background_grid())
  }
  ),
  tar_target(p_sem_sp, {
    theme_set(theme_half_open() + background_grid())
    plot_grid(
      NULL, p_sem_sp_biplot$p_ct_bm, p_sem_sp_biplot$p_tlvl_bm, p_sem_sp_biplot$p_redundant_bm,
      p_sem_sp_biplot$p_bm_rich, p_sem_sp_biplot$p_ct_rich, p_sem_sp_biplot$p_tlvl_rich, p_sem_sp_biplot$p_redundant_rich,
      nrow = 2, byrow = TRUE,
      align = "v",
      label_x = 0.1
    )
  }
    ),
  tar_target(fig_sp_sem,
    ggsave(
      filename = here::here("figures", "p_sem_sp.png"),
      p_sem_sp,
      scale = 2.6,
      units = c("mm"),
      width = 120,
      height = 120 * .4),
    format = "file"
    ),
  tar_target(p_sem_sp_sim,
    plot_grid(p_sem_sp, p_sem_sim,
      nrow = 2, labels = c("a", "b")
      )),
  tar_target(p_sem_sp_sim_file,
    ggsave(
      filename = here::here("figures", "p_sem_sp_sim.png"),
      p_sem_sp_sim,
      scale = 2.6,
      units = c("mm"),
      width = 120,
      height = 120 * .7),
    format = "file"
    ),
  # Main figure parts:
  tar_target(fig2_right,
    save_plot(
      filename = here("figures", "fig2_right.png"),
      p_sem_tps_right,
      ncol = 2, nrow = 3),
    format = "file"
    ),
  tar_target(p_map_fr,
    ggplot() +
      geom_sf(data = st_geometry(basin_dce$basin_tot), fill = NA) +
      geom_sf(
        data = st_geometry(station_analysis),
        shape = 21, alpha = 1,
        color = "white", fill = "black", size = 3, stroke = 1
        ) +
      geom_sf(data = st_geometry(
          station_analysis %>%
            filter(id == 9561)
          ),
        shape = 21,
        color = "white", fill = "red", size = 3, stroke = 1) +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank()
        )
    ),
  tar_target(map_plot,
    ggsave_multiple(
      paste0("map_fr", c(".png", ".pdf")),
      plot = p_map_fr,
      path = here::here("figures"),
      scale = 2.4,
      units = c("mm"),
      width = 80,
      height = 80 * 1)
    ),
  tar_target(ex_tps_sp_net,{
    v_colors <- viridisLite::viridis(n = 2, begin = 0.5, end = 1)
    names(v_colors) <- c("tps", "sp")

    tps_dec_inc <- map(c("log_bm_std", "log_rich_std", "connectance", "w_trph_lvl_avg", "prop_redundant_links"), #
      function(x) {
        tmp <- plot_temporal_variable_lm(
          .df = full_data2,
          st = "9561",
          var = x,
          add_median = TRUE,
          col_median = v_colors["sp"],
          col_trend = v_colors["tps"]
          ) +
        theme_half_open() +
        theme(axis.title = element_text(size = 12))
      tmp$layers[[1]]$aes_params$size <- 4
      tmp
      }
    )
    plot_grid(plotlist = tps_dec_inc)
    }),
  tar_target(figmetC,
    ggsave_multiple(
      paste0("example_tps_sp_net", c(".png", ".pdf")),
      plot = ex_tps_sp_net,
      path = here::here("figures"),
      scale = 1.8,
      width = 120,
      height = 120 * .6,
      units = "mm")
    ),
  tar_target(figSobsfit,
      save_plot(
        filename = here::here("figures", "p_obs_fit.pdf"),
        plot = p_obs_fit,
        nrow = 2,
        ncol = 2
        ),
      format = "file"
      ),
    tar_target(figStps10,
      save_plot(
        filename = here::here("figures", "p_hist_site_tps10.pdf"),
        plot = p_hist_site_tps10,
        nrow = 2,
        ncol = 2
        ),
      format = "file"
      ),
    tar_target(figstd,
      save_plot(
        filename = here::here("figures", "p_sem_tps_sp.pdf"),
        plot = p_sem_tps_sp,
        nrow = 1,
        ncol = 2,
        base_height = 5,
        base_asp = 1.618
      )
      ,
      format = "file"
    ),
  tar_target(figcorr,
    ggsave(
      here::here("figures", "p_corr.pdf"),
      p_corr,
      units = "mm",
      scale = 2.8,
      width = 80, height = 80 * .75),
    format = "file"
    ),
  tar_target(p_sem_tot_ext_sp_file,
    ggsave_multiple(
      paste0("p_sem_tot_ext_sp", c(".png", ".pdf")),
      plot = tar_read(p_sem_tot_ext_sp),
      path = here::here("figures"),
      scale = 2.4,
      units = c("mm"),
      width = 120,
      height = 120 * .65)
    ),
  # Tables:
  tar_target(pca_table,
    data_for_pca %>%
      pivot_longer(-station, names_to = "metric", values_to = "values") %>%
      group_by(metric) %>%
      summarise(summ = list(enframe(summary_distribution(values, na.rm = TRUE)))) %>%
      unnest(summ) %>%
      pivot_wider(names_from = "name", values_from = "value") %>%
      mutate(
        median = format(round(median, 2), nsmall = 2),
        `Median (Q1, Q3)` = paste0(median, " (",
          format(round(`1st_quart`, 2), nsmall = 2), ",",
          format(round(`2nd_quart`, 2), nsmall = 2),
          ")"),
        `(Min, Max)` = paste0("(",
          format(round(min, 2), nsmall = 2),
          ",",
          format(round(max, 2), nsmall = 2),
          ")")
        ) %>%
      arrange(metric) %>%
      mutate(metric = replacement_pca_var()[metric]) %>%
      select(`PCA variable` = metric, `Median (Q1, Q3)`, `(Min, Max)`)
    ),
  tar_target(op_table,
    op_data %>%
      filter(opcod %in% unique(data_inla$opcod)) %>%
      ungroup() %>%
      group_by(station) %>%
      summarise(
        span = max(year) - min(year) + 1,
        completeness = n() / (span),
        baseline_year = min(year),
        nb_op = length(year)
        ) %>%
      pivot_longer(-station, names_to = "metric", values_to = "values") %>%
      group_by(metric) %>%
      summarise(summ = list(enframe(summary_distribution(values, na.rm = TRUE)))) %>%
      unnest(summ) %>%
      pivot_wider(names_from = "name", values_from = "value") %>%
      mutate(
        median = format(round(median, 2), nsmall = 2),
        `Median (Q1, Q3)` = paste0(median, " (",
          format(round(`1st_quart`, 2), nsmall = 2), ",",
          format(round(`2nd_quart`, 2), nsmall = 2),
          ")"),
        `(Min, Max)` = paste0("(",
          format(round(min, 2), nsmall = 2),
          ",",
          format(round(max, 2), nsmall = 2),
          ")")
        ) %>%
      arrange(metric) %>%
      mutate(metric = replacement_op_data()[metric]) %>%
      select(`Sampling` = metric, `Median (Q1, Q3)`, `(Min, Max)`, median) %>%
      select(-median)
    ),
  tar_target(com_metric_tab,
    full_data2 %>%
      left_join(basin_station, by = "station") %>%
      select(-basin) %>%
      mutate(biomass_kg = biomass * 10^-3) %>%
    pivot_longer(-c(opcod, station), names_to = "metric", values_to = "values") %>%
    filter(metric %in% c("richness", "biomass_kg", "nbnode", "nind", "w_trph_lvl_avg",
        "connectance", "prop_pisc_node", "prop_pisc_rich", "prop_redundant_links")) %>%
    group_by(metric) %>%
    summarise(summ = list(enframe(summary_distribution(values, na.rm = TRUE)))) %>%
    unnest(summ) %>%
    pivot_wider(names_from = "name", values_from = "value") %>%
    mutate(
      median = format(round(median, 2), nsmall = 2),
      `Median (Q1, Q3)` = paste0(median, " (",
        format(round(`1st_quart`, 2), nsmall = 2), ",",
        format(round(`2nd_quart`, 2), nsmall = 2),
        ")"),
      `(Min, Max)` = paste0("(",
        format(round(min, 2), nsmall = 2),
        ",",
        format(round(max, 2), nsmall = 2),
        ")")
      ) %>%
    mutate(metric = var_replacement()[metric]) %>%
    select(`Community metric` = metric, `Median (Q1, Q3)`, `(Min, Max)`, median) %>%
    arrange(median) %>%
    select(-median)
  ),
  tar_target(summary_pred_station_tps10,
    gaussian_pred_station_tps10 %>%
      filter(response %in% c("log_bm_std", "connectance", "w_trph_lvl_avg", "log_rich_std", "prop_redundant_links")) %>%
      group_by(response) %>%
      summarise(summ = list(enframe(summary_distribution(mean, na.rm = TRUE)))) %>%
      unnest(summ) %>%
      pivot_wider(names_from = "name", values_from = "value")
    ),
  #tar_render(main_text, here("paper", "manuscript.Rmd")),
  tar_render(supplementary, here("paper", "appendix.Rmd"))

)
