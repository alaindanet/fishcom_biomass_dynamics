
get_formula_inla <- function(resp = NULL, tau_prior = NULL) {

  fixed_min <- "nb_year"

  fixed_part <- fixed_min

  if (!is.null(tau_prior)) {
    rand_part <-
      paste0(
        "f(intercept_basin, model = 'iid', hyper = ", tau_prior, ") +
        f(basin1, nb_year, model = 'iid', hyper = ", tau_prior, ") +
        f(intercept_basin_station, model = 'iid', hyper = ", tau_prior, ") +
        f(station1, nb_year, model = 'iid', hyper = ", tau_prior, ")"
      )
  } else {
    rand_part <-
      paste0(
        "f(intercept_basin, model = 'iid') +
        f(basin1, nb_year, model = 'iid') +
        f(intercept_basin_station, model = 'iid') +
        f(station1, nb_year, model = 'iid')")
  }

  form <- paste0(resp, " ~\n", fixed_part, " +\n", rand_part)
  as.formula(form)
}

get_re_prediction_inla <- function(
  inla_mod = NULL,
  effect = "siteid1",
  trend_class = TRUE,
  exponentiate = FALSE,
  modelling_data = NULL) {

  re <- inla_mod$summary.random[[effect]] %>%
    as_tibble() %>%
    rename(
      quant0.025 = `0.025quant`,
      quant0.975 = `0.975quant`,
      quant0.5 = `0.5quant`
    ) %>% 
    select(-kld)

  re_name <- str_extract(effect, "station|basin")
  colnames(re)[colnames(re) == "ID"] <- re_name

  if(trend_class) {
    re <- re %>%
      mutate(
        trend_class = case_when(
          quant0.025 > 0 & quant0.975 > 0 ~ "increase",
          quant0.025 < 0 & quant0.975 < 0 ~ "decrease",
          sign(quant0.025) * sign(quant0.975) == -1 ~ "stable",
          TRUE ~ "NA"
        )
      )

  }


  if (exponentiate) {
    re <- re %>%
      mutate(across(where(is.double), ~exp(. - 1)))
  }

  return(re)
}

get_hpdmarginal_inla <- function(
  inla_mod = NULL,
  type = "fixed",
  p = c(.80, .90, 0.95)) {

  if (type == "fixed") {
    m <- inla_mod$marginals.fixed
    mi <- inla_mod$summary.fixed
  } else if (type == "rand") {
    m <- inla_mod$marginals.hyperpar
    mi <- inla_mod$summary.hyperpar
  }
  output <- map_dfr(m, function(x) {
    if (!any(x == "Inf")) {

     inla.hpdmarginal(marginal = x, p = c(.80, .90, 0.95)) %>%
    as.data.frame() %>%
    rownames_to_column("ci_level")
    } else {
      tibble(
        ci_level = c("level:0.80", "level:0.90", "level:0.95"),
        low = NA,
        high = NA)
    }

  }
,
  .id = "term"
  )

  ## Add mean
  output <- output %>%
    left_join(
      mi %>%
        rownames_to_column("term") %>%
        as_tibble %>%
        select(term, mean),
      by = "term"
    )
 

  if (type == "rand") {
    output[c("low", "mean", "high")] <- map(output[c("low", "mean", "high")], tau_to_sigma)
  }

  return(output)
}

plot_uniform_quantile_inla <- function(mod_inla = NULL) {
  pit <- sort(mod_inla$cpo$pit)

  tb <- tibble(
    pit = pit,
    uniquant = (seq_along(pit)) / (length(pit)+1)
  )

  tb %>%
  ggplot(aes(x = uniquant, y = pit)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = "uniform quantiles", y = "Sorted PIT values")
}

tau_to_sigma <- function(x) {
  1 / sqrt(x)
}

sigma_to_tau <- function(x) {
  1 / (x^2)
}

r2_mvp <- function(
  var_pred = NULL,
  epsilon = NULL,
  std_intercept = NULL,
  type = "all"
  ) {

  # from std to var
  var_interp <- sum(map_dbl(std_intercept, ~.x^2))

  out <- list(
    marginal = var_pred / (var_pred + var_interp + epsilon^2),
    conditional = (var_pred  + var_interp) / (var_pred + var_interp + epsilon^2)
    )

  if (type == "all") {
    return(out)
  } else if (type == "marginal") {
    return(out[["marginal"]])
  } else if (type == "conditional") {
    return(out[["conditional"]])
  }

}

get_global_effect <- function (
  effect = inla_no_drivers_effects,
  resp = NULL,
  ci_lvl = "level:0.95"
  ) {

  out <- effect %>% 
    filter(
      ci_level == ci_lvl,
      term == "log1_year_nb",
      response == resp 
    )

  out[, c("low", "high", "mean")] %>%
    pivot_longer(everything()) %>%
    deframe()
}

p_ci <- function(x, r = 2, p = TRUE) {
  out <- format(round(x, r), nsmall = r)

  if (p) {
    paste0(out["mean"], "%", " [", out["low"],"%,", out["high"],"%]")
  } else {
    paste0(out["mean"], " [", out["low"],",", out["high"],"]")
  }

}

format_inla_model_list <- function(
  x = gaussian_inla_prior_std,
  response_to_skip = c(
    "species_nb", "log_species_nb", "species_nb_tps_scaled",
    "chao_richness_tps_scaled", "total_abundance",
    "total_abundance_scaled", "total_abundance_tps"),
  prob = c(.80, .90, .95)
  ) {

  x %>%
    filter(!response %in% response_to_skip) %>%
    mutate(
      hpd_fixed = map(
        mod,
        ~get_hpdmarginal_inla(
          inla_mod = .x,
          type = "fixed",
          p = prob
          )
        ),
      hpd_random = map(
        mod,
        ~get_hpdmarginal_inla(
          inla_mod = .x,
          type = "rand",
          p = prob
        )
      )
      ) %>%
    select(-mod) %>%
    select(-hpd_random) %>%
    unnest(hpd_fixed)
}

compute_trends_meaningful_units <- function (
  x = NULL, resp = NULL, time = 10) {

  if (resp %in% c("log_rich_std", "log_bm_std")) {
    return(log_beta_to_perc_rate(x) * time)

  } else if (resp %in% c("ct_ff", "w_trph_lvl_avg", "log_rich_std", "piel_nind",
      "piel_bm", "prop_pisc_node", "prop_pisc_rich")) {
    return(x * time)
  } else {
    stop("undefined resp")
  }

}

get_std_inla_from_rand <- function (
  inla_rand_tab = NULL
  ) {
  x <- inla_rand_tab %>%
    filter(ci_level == "level:0.95") %>%
    mutate(
      term = str_remove(term, "Precision for "),
      term = str_replace(term, "the Gaussian observations", "epsilon")
      ) %>%
    distinct(response, term, mean)
  x %>%
    pivot_wider(names_from = "term", values_from = "mean")
}

r2_mvp <- function(
  var_pred = NULL,
  epsilon = NULL,
  std_intercept = NULL,
  type = "all"
  ) {

  # from std to var
  var_interp <- sum(map_dbl(std_intercept, ~.x^2))

  out <- list(
    marginal = var_pred / (var_pred + var_interp + epsilon^2),
    conditional = (var_pred  + var_interp) / (var_pred + var_interp + epsilon^2)
    )

  if (type == "all") {
    return(out)
  } else if (type == "marginal") {
    return(out[["marginal"]])
  } else if (type == "conditional") {
    return(out[["conditional"]])
  }

}

log_beta_to_perc_rate <- function (x) {
  (exp(x) - 1) * 100
}

get_global_effect <- function (
  effect = glob_tps_trends_decade_no_drivers,
  resp = NULL,
  ci_lvl = "level:0.95"
  ) {

  out <- effect %>% 
    filter(
      ci_level == ci_lvl,
      term == "nb_year",
      response == resp 
    )

  out[, c("low", "high", "mean")] %>%
    pivot_longer(everything()) %>%
    deframe()
}

p_ci <- function(x, r = 2, p = TRUE) {
  out <- format(round(x, r), nsmall = r)

  if (p) {
    paste0(out["mean"], "%", " [", out["low"],"%,", out["high"],"%]")
  } else {
    paste0(out["mean"], " [", out["low"],",", out["high"],"]")
  }

}

get_effect_ci <- function(
  effect = glob_tps_trends_decade_no_drivers,
  resp = NULL,
  term = "nb_year",
  ci_lvl = "level:0.95",
  r = 2,
  p = FALSE
  ) {
  term1 <- term

  out <- effect %>%
    filter(
      ci_level == ci_lvl,
      term == term1,
      response == resp
    )

  out <- out[, c("low", "high", "mean")] %>%
    pivot_longer(everything()) %>%
    deframe()

  p_ci(x = out, r = r, p = p)
}
