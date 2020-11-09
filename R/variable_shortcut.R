get_biomass_var <- function () {
  c("biomass", "rel_bm", "log_bm", "rel_log_bm", "bm_std", "log_bm_std")
}

get_richness_var <- function () {
  c("richness", "rich_std", "log_rich", "log_rich_std")
}

get_species <- function(node_list) {

  species_resource_list <- stringr::str_extract_all(
    node_list, "[A-Za-z]+", simplify = TRUE)

  species_resource_list

}

model_type_var <- function() {
  c("model_bm", "model_bm_f3y", "model_log_bm", "model_log_bm_f3y")
}
