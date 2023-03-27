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
  tar_target(sp_semeff_tab, from_semEff_to_table(sp_semeff_perc)),
  tar_target(semeff_tab, from_semEff_to_table(semeff_perc_inla)),
  tar_target(semeff_tot, rbind(tibble(semeff_tab, model = "temporal"),
      tibble(sp_semeff_tab, model = "spatial")
      )),

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
    vif_tps_sem <- setNames(round(vif_tps_sem, 2), str_remove(names(vif_tps_sem), "mean_"))

    vif_sp_sem <- round(car::vif(sp_sem[["ct_ff"]]), 2)

    rbind(
      tibble(Model = "temporal", enframe(vif_tps_sem)),
      tibble(Model = "spatial", enframe(vif_sp_sem))
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
            x = mean_log_rich_std,
            y = mean_log_bm_std
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(intercept = coef(sem[[3]])[1], slope = coef(sem[[3]])[2], size = 1) +
        labs(x = "Species richness trend\n(% by decade)", y = "Biomass trend\n(% by decade)"),

      p_ct_rich = tps_for_sem %>%
        ggplot(aes(
            x = mean_log_rich_std,
            y = mean_ct_ff,
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(intercept = coef(sem[[1]])["(Intercept)"], slope = coef(sem[[1]])["mean_log_rich_std"], size = 1) +
        labs(x = "Species richness trend\n(% by decade)", y = "Connectance trend\n(by decade)"),
      p_ct_bm = tps_for_sem %>%
        ggplot(aes(
            x = mean_log_bm_std,
            y = mean_ct_ff,
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(intercept = coef(sem[[1]])["(Intercept)"], slope = coef(sem[[1]])["mean_log_bm_std"], size = 1) +
        labs(x = "Biomass trend\n(% by decade)", y = "Connectance trend\n(by decade)"),

      p_tlvl_rich = tps_for_sem %>%
        ggplot(aes(
            x = mean_log_rich_std,
            y = mean_w_trph_lvl_avg,
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(intercept = coef(sem[[2]])["(Intercept)"], slope = coef(sem[[2]])["mean_log_rich_std"], size = 1) +
        labs(x = "Species richness trend\n(% by decade)", y = "Average trophic level trend\n(by decade)"),

      p_tlvl_bm = tps_for_sem %>%
        ggplot(aes(
            x = mean_log_bm_std,
            y = mean_w_trph_lvl_avg,
            )) +
        geom_point(shape = 1, size = 2) +
        geom_abline(
          intercept = coef(sem[[2]])["(Intercept)"],
          slope = coef(sem[[2]])["mean_log_bm_std"], size = 1) +
        labs(
          x = "Biomass trend\n(% by decade)",
          y = "Average trophic level trend\n(by decade)")
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
      p_list_sem_tps$p_tlvl_bm, p_list_sem_tps$p_tlvl_rich,
      ncol = 2,
      labels = c("", letters[2:6]),
      align = "v"
    )

    }),
  ## SEM spatial temporal
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
        response %in% c("log_bm_std", "ct_ff", "w_trph_lvl_avg", "log_rich_std")
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
        response %in% c("log_bm_std", "ct_ff", "w_trph_lvl_avg", "log_rich_std")
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
      filter(response %in% c("log_bm_std", "ct_ff",
          "w_trph_lvl_avg", "log_rich_std")) %>%
      mutate(response = var_replacement()[response]) %>%
      group_by(response) %>%
      summarise(avg = mean(mean), med = median(mean)) %>%
      pivot_longer(-response, names_to = "metric", values_to = "values")

    gaussian_pred_station_tps10 %>%
      filter(response %in% c("log_bm_std", "ct_ff", "w_trph_lvl_avg", "log_rich_std")) %>%
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
        response %in% c("log_bm_std", "ct_ff", "w_trph_lvl_avg", "log_rich_std")
        ) %>%
    mutate(response = var_replacement()[response]) %>%
    select(
      Response = response,
      `Marg. Rsq` = r2_mvp_marg95hpdci,
      `Cond. Rsq` = r2_mvp_cond95hpdci
    )
    ),
  tar_target(tab_inla_r2_rand,
    gaussian_inla_rand %>%
      filter(
        ci_level == "level:0.95",
        response %in% c("log_bm_std", "ct_ff", "w_trph_lvl_avg", "log_rich_std")
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
      select(c(matches("mean"))) %>%
      rename_with(~str_remove(.x, "mean_")) %>%
      select(-c(matches("piel"))) %>%
      rename_with(~var_replacement()[.x]) %>%
      cor(.)

    cor_sp <- sp_for_sem %>%
      select(c(log_bm_std, ct_ff, w_trph_lvl_avg,
          log_rich_std, prop_pisc_node, prop_pisc_rich)) %>%
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

    p_corr <- plot_grid(
      p_cor_tps + theme(legend.position = "none"),
      p_cor_sp + theme(legend.position = "none"),
      get_legend(p_cor_tps),
      ncol = 3,
      rel_widths = c(1, 1, .1),
      labels = c("a", "b", ""))
  }),

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
  tar_target(sim_dec_tlvl,
    get_sim_net(net = metaweb_analysis$metaweb, ext_seq = node_seq$node_fish_dec_tlvl,
      keep_metaweb = FALSE)
    ),
  tar_target(sim_inc_tlvl,
    get_sim_net(net = metaweb_analysis$metaweb, ext_seq = node_seq$node_fish_inc_tlvl,
      keep_metaweb = FALSE)
    ),
  tar_target(sim_random_tlvl, {
    nsim <- 50
    set.seed(123)
    tibble(
      repl = seq_len(nsim),
      sim = furrr::future_map(repl,
        ~get_sim_net(net = metaweb_analysis$metaweb, ext_seq = sample(node_seq$node_fish_inc_tlvl),
          keep_metaweb = FALSE)
      )
    )
    }),
  tar_target(node_sim_metrics,
    list(
      dec = sim_dec_tlvl %>%
        mutate(met = map(metrics, ~enframe(unlist(.x[net_ext_met_to_keep()])))) %>%
        select(-metrics) %>%
        unnest(met) %>%
        pivot_wider(names_from = "name", values_from = "value"),
      inc = sim_inc_tlvl %>%
        mutate(met = map(metrics, ~enframe(unlist(.x[net_ext_met_to_keep()])))) %>%
        select(-metrics) %>%
        unnest(met) %>%
        pivot_wider(names_from = "name", values_from = "value"),
      random = sim_random_tlvl %>%
        mutate(res = map(sim, function(x) {
            x %>%
              mutate(met = map(metrics, ~enframe(unlist(.x[net_ext_met_to_keep()])))) %>%
              unnest(met) %>%
              pivot_wider(names_from = "name", values_from = "value")
          })) %>%
      select(-sim) %>%
      unnest(res)
    )
    ),
  tar_target(rand_node_tmp,
    sim_random_tlvl %>%
      unnest(sim) %>%
      mutate(
        met = map(metrics, ~enframe(unlist(.x[net_ext_met_to_keep()]))),
        nb_ext = map_int(node_rm_tot, length),
        type = "random",
        extinct = "node"
        ) %>%
      select(-metrics, - node_rm_tot) %>%
      unnest(met) %>%
      rename(metric = name)
    ),
  tar_target(sim_node_dec_inc,

    rbind(
      node_sim_metrics$dec %>%
        pivot_longer(!!net_ext_met_to_keep(), names_to = "metric", values_to = "value") %>%
        mutate(
          type = "decreasing",
          extinct = "node"
          ),
      node_sim_metrics$inc %>%
          pivot_longer(!!net_ext_met_to_keep(), names_to = "metric", values_to = "value") %>%
          mutate(
            type = "increasing",
            extinct = "node"
          )
    )
    ),
  tar_target(p_ext_node,
    sim_node_dec_inc %>%
      filter(str_detect(metric, "_ff")) %>%
      mutate(
        nb_ext = map_int(node_rm_tot, length),
        metric = var_replacement()[metric],
        type = str_to_sentence(type)
        ) %>%
      ggplot(aes(x = nb_ext, y = value, color = type)) +
      geom_line() +
      geom_point(data = rand_node_tmp %>%
        filter(str_detect(metric, "_ff")) %>%
        mutate(
          metric = var_replacement()[metric],
          type = str_to_sentence(type)
          )) +
      geom_smooth(data = rand_node_tmp %>%
        filter(str_detect(metric, "_ff")) %>%
        mutate(
          metric = var_replacement()[metric],
          type = str_to_sentence(type)
          ), color = "black") +
      facet_wrap(~metric, scale = "free_y") +
      labs(y = "Network metric values",
        x = "Number of node removed",
        color = "Trophic level scenario") +
      theme(legend.position = "bottom")
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
  tar_target(fish_sim_species, {
    sim_dec_tlvl_species <- vector(mode ="list", length = length(fish_species_tlvl))
    sim_inc_tlvl_species <- vector(mode = "list", length = length(fish_species_tlvl))
    inc_species_tlvl <- sort(fish_species_tlvl, decreasing = FALSE)

    for (i in seq_along(fish_species_tlvl)) {
      decreasing_tlvl_metaweb <- metaweb_analysis$metaweb[
        !str_detect(
          rownames(metaweb_analysis$metaweb),
          paste0(names(fish_species_tlvl)[1:i], collapse = "|")
          ),
        !str_detect(
          colnames(metaweb_analysis$metaweb),
          paste0(names(fish_species_tlvl)[1:i], collapse = "|")
        )
        ]

      # Remove without feeding links
      mask_link <- colSums(decreasing_tlvl_metaweb) != 0
      decreasing_tlvl_metaweb <- decreasing_tlvl_metaweb[mask_link, mask_link]
      sim_dec_tlvl_species[[i]] <- get_fw_metrics2(decreasing_tlvl_metaweb,
        keep_metaweb = FALSE)

      increasing_tlvl_metaweb <- metaweb_analysis$metaweb[
        !str_detect(
          rownames(metaweb_analysis$metaweb),
          paste0(names(inc_species_tlvl)[1:i], collapse = "|")
          ),
        !str_detect(
          colnames(metaweb_analysis$metaweb),
          paste0(names(inc_species_tlvl)[1:i], collapse = "|")
        )
        ]

      # Remove without feeding links
      mask_link <- colSums(increasing_tlvl_metaweb) != 0
      increasing_tlvl_metaweb <- increasing_tlvl_metaweb[mask_link, mask_link]
      sim_inc_tlvl_species[[i]] <- get_fw_metrics2(increasing_tlvl_metaweb,
        keep_metaweb = FALSE)
    }
    list(
      dec =  tibble(
        one_sp_rm = names(fish_species_tlvl),
        species_removed = map(seq_along(fish_species_tlvl),
          ~names(fish_species_tlvl)[1:.x]),
        metrics = sim_dec_tlvl_species
        ),
      inc = tibble(
        one_sp_rm = names(inc_species_tlvl),
        species_removed = map(seq_along(inc_species_tlvl),
          ~names(inc_species_tlvl)[1:.x]),
        metrics = sim_inc_tlvl_species
      )
    )
    }
    ),
  tar_target(sim_rand_species, {
    nsim <- 50
    set.seed(123)

    sim_random_tlvl_species <- vector(mode = "list", length = length(fish_species_tlvl))
    sim_random_tlvl_species <- map(sim_random_tlvl_species, ~vector(mode = "list", length = nsim))
    sp_to_rm <- vector(mode = "list", length = length(fish_species_tlvl))
    sp_to_rm <- map(sp_to_rm, ~vector(mode = "list", length = nsim))

    for (i in seq_along(fish_species_tlvl)) {
      for (j in seq_len(nsim)) {
        sp_to_rm[[i]][[j]] <- sample(names(fish_species_tlvl), size = i, replace = FALSE)

        tmp_metaweb <- metaweb_analysis$metaweb[
          !str_detect(
            rownames(metaweb_analysis$metaweb),
            paste0(sp_to_rm[[i]][[j]], collapse = "|")
            ),
          !str_detect(
            colnames(metaweb_analysis$metaweb),
            paste0(sp_to_rm[[i]][[j]], collapse = "|")
          )
          ]
        # Remove without feeding links
        mask_link <- colSums(tmp_metaweb) != 0
        tmp_metaweb <- tmp_metaweb[mask_link, mask_link]
        sim_random_tlvl_species[[i]][[j]] <- get_fw_metrics2(
          tmp_metaweb,
          keep_metaweb = FALSE)
      }
    }
    tibble(
      nsp_rm = seq_along(fish_species_tlvl),
      species_removed = sp_to_rm,
      metrics = sim_random_tlvl_species
    )
    }
    ),
  tar_target(sim_sp_dec_inc,
    rbind(
      fish_sim_species$inc %>%
        mutate(
          met = map(metrics, ~enframe(unlist(.x[net_ext_met_to_keep()]))),
          nb_ext = map_int(species_removed, length),
          type = "increasing",
          extinct = "species"
          ) %>%
      unnest(met) %>%
      rename(metric = name),
    fish_sim_species$dec %>%
      mutate(
        met = map(metrics, ~enframe(unlist(.x[net_ext_met_to_keep()]))),
        nb_ext = map_int(species_removed, length),
        type = "decreasing",
        extinct = "species"
        ) %>%
    unnest(met) %>%
    rename(metric = name)
    )
    ),
  tar_target(rand_sp_met,
    sim_rand_species %>%
      unnest(metrics) %>%
      mutate(
        met = map(metrics, ~enframe(unlist(.x[net_ext_met_to_keep()]))),
        nb_ext = nsp_rm,
        type = "random",
        extinct = "species"
        ) %>%
      unnest(met) %>%
      rename(metric = name)
    ),
  tar_target(p_ext_sp,
    sim_sp_dec_inc %>%
      filter(str_detect(metric, "_ff")) %>%
      mutate(
        metric = var_replacement()[metric],
        type = str_to_sentence(type)
        ) %>%
      ggplot(aes(x = nb_ext, y = value, color = type)) +
      geom_line() +
      geom_point(
        data = rand_sp_met %>%
          filter(str_detect(metric, "_ff")) %>%
          mutate(
            metric = var_replacement()[metric],
            type = str_to_sentence(type)
            )) +
        geom_smooth(data = rand_sp_met %>%
          filter(str_detect(metric, "_ff")) %>%
          mutate(
            metric = var_replacement()[metric],
            type = str_to_sentence(type)
            ), color = "black") +
        facet_wrap(~metric, scale = "free_y") +
        labs(y = "Network metric values", x = "Number of species removed",
          color = "Trophic level scenario") +
        theme(legend.position = "bottom")
    ),
  tar_target(p_tot_ext_sim,
    plot_grid(
      p_ext_sp +
        theme_half_open() +
        background_grid() +
        theme(
          strip.background = element_blank(),
          legend.position = "none"
          ),
        p_ext_node +
          theme_half_open() +
          background_grid() +
          theme(
            strip.background = element_blank(),
            legend.position = "none"
            ),
          get_legend(p_ext_node + theme(legend.position = "bottom")),
          nrow = 3,
          labels = c("a", "b", ""),
          rel_heights = c(1, 1, .1)
    )
    ),
  tar_target(figSext,
    save_plot(
      filename = here::here("figures", "tot_ext_sim.pdf"),
      plot = p_tot_ext_sim,
      nrow = 2,
      ncol = 3
    ),
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
  tar_target(p_station,
    ggplot() +
      geom_sf(data = st_geometry(basin_dce$basin_tot), fill = "grey90") +
      geom_sf(
        data = st_geometry(station_analysis),
        shape = 21,
        color = "white", fill = "black", size = 1, stroke = 1) +
      geom_sf(data = st_geometry(
          station_analysis %>%
            filter(id == 9561)
          ),
        shape = 21,
        color = "white", fill = "red", size = 1, stroke = 1) +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
      )
      ),
    tar_target(map_plot,
      save_plot(filename = here::here("figures", "map_fr.pdf"), p_station,
        base_height = 5.71,
        base_asp = 1
        ),
      format = "file"
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
    save_plot(here("figures", "p_corr.pdf"), p_corr, ncol = 2),
    format = "file"
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
    data_inla %>%
      select(
        -c(basin, basin1, station1, intercept_basin, intercept_basin_station)
        ) %>%
    mutate(biomass_kg = biomass * 10^-3) %>%
    pivot_longer(-c(opcod, station), names_to = "metric", values_to = "values") %>%
    filter(metric %in% c("richness", "biomass_kg", "nbnode", "nind", "w_trph_lvl_avg",
        "ct_ff", "prop_pisc_node", "prop_pisc_rich")) %>%
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
  tar_render(main_text, here("paper", "manuscript.Rmd")),
  tar_render(supplementary, here("paper", "appendix.Rmd"))

)
