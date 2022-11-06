get_biomass_var <- function (all = FALSE) {

  x <- c("log_bm_std")
  if (all) {
  x <- c(x, "bm_std", "biomass", "rel_bm", "log_bm", "rel_log_bm")
  }
  return(x)
}

get_richness_var <- function (all = FALSE) {

  x <- c("log_rich_std")
  if (all) {
    x <- c(x, "rich_std", "richness", "log_rich") 
  }
  return(x)
}

get_species <- function(node_list) {

  species_resource_list <- stringr::str_extract_all(
    node_list, "[A-Za-z]+", simplify = TRUE)

  species_resource_list

}

model_type_var <- function(
  cut_prefix = FALSE,
  add_protocol = FALSE,
  add_rich = FALSE) {

  x <- c("model_bm", "model_bm_f3y", "model_log_bm", "model_log_bm_f3y")

  if (cut_prefix) {
    x %<>% str_remove_all("model_")
  }

  if (add_rich) {
    x <- c(x, str_replace(x, "bm", "rich"))
  }

  if (add_protocol) {
    x <- map(c("", "_by_protocol"), ~paste0(x, .x)) %>%
      unlist
  }

  return(x)
}

get_com_str_var <- function (all = FALSE) {

  x <- c(
    "ct_ff", "w_trph_lvl_avg", "log_rich_std", "piel_nind", "piel_bm",
    "prop_pisc_node", "prop_pisc_rich"
  )

  if (all) {
    x <- c(x, "rich_std", "connectance", "weighted_connectance", "richness",
      "nbnode_std", "nb_pisc_rich_std", "nb_non_pisc_rich_std",
      "nb_pisc_node_std", "nb_non_pisc_node_std",
      "prop_non_pisc_node",  "prop_non_pisc_rich", "l", "ld",
      "m", "A", "avg_IS", "gini", "ct_ff", "l_ff", "ld_ff", "piel_nind",
      "piel_bm", "nind_std") }

  x <- unique(x)

  return(x)
}

get_model_terms <- function ( ) {
  c("bm_slope", "inc_f", "bm", "log_bm")

} 

var_replacement <- function (slope = FALSE, several_lines = FALSE) {
  x <- c(
    log_RC1 = "PCA Avg stream size",
    log_RC2 = "PCA Avg temperature",
    ct_ff = "Connectance",
    avg_tl_ff = "Avg trophic level",
    max_tl_ff = "Max trophic level",
    w_trph_lvl_avg = "Avg trophic level",
    #connectance ,weighted_connectance,
    log_rich_std = "Species richness",
    log_bm_std = "Biomass",
    bm_std = "Biomass (g.m^-2)",
    rich_std = "Species richness (m^-2)",
    piel_nind = "Evenness (abundance)",
    piel_bm = "Evenness (biomass)",
    prop_pisc_node = "Piscivorous node prop.",
    prop_pisc_rich = "Piscivorous rich prop.",
    biomass = "Biomass (g)",
    biomass_kg = "Biomass (kg)",
    richness = "Species richness",
    nbnode = "Nb of nodes",
    nind = "Nb of individuals"
  )

  if (several_lines) {
    x <- c(
      log_RC1 = "PCA\nAvg stream size",
      log_RC2 = "PCA\n Avg temperature",
      ct_ff = "Connectance",
      w_trph_lvl_avg = "Avg\ntrophic level",
      #connectance ,weighted_connectance,
      log_rich_std = "Log\nSpecies richness",
      log_bm_std = "Log Biomass",
      bm_std = "Biomass",
      rich_std = "Species\nrichness",
      piel_nind = "Evenness\n(abundance)",
      piel_bm = "Evenness\n(biomass)"
    )
  }

  if (slope) {
    tmp <- names(x)
    x %<>% paste0(., " (change per year)")
    names(x) <- tmp
  }

  return(x)
}

get_var_for_pca <- function() {
  c(
    "slope", "alt", "d_source", "strahler", "width_river_mean", "avg_depth_station_mean",
    "width_river_cv", "avg_depth_station_cv", "DBO_med", "flow_med", "temperature_med",
    "DBO_cv", "flow_cv", "temperature_cv"
  )
}

get_all_var_analysis <- function () {
  c(get_com_str_var(), get_biomass_var(), get_richness_var()) %>% unique
}

net_ext_met_to_keep <- function() {
    c("ct", "ct_ff", "avg_tl", "avg_tl_ff", "max_tl", "max_tl_ff")
}

replacement_random_term <- function() {
  c(
    "Gaussian observations" = "Error",
    "intercept_basin" = "Intercept (basin)",
    "basin1" = "Time (basin)",
    "intercept_basin_station" = "Intercept (site nested in basin)",
    "station1" = "Time (site nested in basin)"
  )
}


replacement_pca_var <- function() {
  c(
    "alt"                    = "Altitude (m)",
    "avg_depth_station_cv"   = "River depth CV",
    "avg_depth_station_mean" = "River depth (m)",
    "d_source"               = "Distance from source (km)",
    "DBO_cv"                 = "BOD CV",
    "DBO_med"                = "BOD",
    "flow_cv"                = "Water flow CV",
    "flow_med"               = "Water flow (m3/s)",
    "slope"                  = "Slope (deg.)",
    "strahler"               = "Strahler order",
    "temperature_cv"         = "Temperature CV",
    "temperature_med"        = "Temperature (°C)",
    "width_river_cv"         = "River width CV",
    "width_river_mean"       = "River width (m)"
  )
}

replacement_op_data <- function() {
  c(
    "baseline_year" = "Baseline year",
    "completeness"  = "Completeness",
    "nb_op"         = "Sampling number",
    "span"          = "Year span"
  )
}
