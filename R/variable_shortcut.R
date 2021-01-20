get_biomass_var <- function (all = FALSE) {

  x <- c("bm_std", "log_bm_std")
  if (all) {
  x <- c(x, "biomass", "rel_bm", "log_bm", "rel_log_bm")
  }
  return(x)
}

get_richness_var <- function (all = FALSE) {

  x <- c("rich_std", "log_rich_std")
  if (all) {
    x <- c(x, "richness", "log_rich") 
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

x <- c("ct_ff", "w_trph_lvl_avg", #"connectance" ,"weighted_connectance",
 "rich_std")

if (all) {
  x <- c(x, "connectance", "weighted_connectance", "richness", "log_rich_std", "nbnode_std" ,"nb_pisc_rich_std", "nb_pisc_node_std", "prop_pisc_node", "prop_pisc_rich", "l", "ld", "m", "A", "avg_IS", "gini", "ct_ff", "l_ff", "ld_ff", "piel_nind", "piel_bm")
}
return(x)
}

get_model_terms <- function ( ) {
  c("bm_slope", "inc_f", "bm", "log_bm")

} 
