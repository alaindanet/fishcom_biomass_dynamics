#' Get community metrics
#'
#' @param dir path to the data
get_community_data <- function (
  path = get_mypath("data", "community_metrics.rda"),
  community_metrics = NULL
  ) {

  load(path, envir = environment())

  com <- community_metrics %>% 
    dplyr::select(opcod, biomass, richness) %>%
    dplyr::filter(!is.na(opcod))

  return(com)
  
}

#' Get network metrics
#'
#' @param dir path to the data
get_network_data <- function (network_metrics = NULL, 
  path = get_mypath("data", "classes", "network_metrics.rda"), 
metrics = NULL) {
  
  load(path, envir = environment())

  network_metrics %<>%
    mutate(
      pisc_stat = map(network, ~try(get_piscivory_stat_from_network(net = .x)))
    )

  if (is.null(metrics)) {
    metrics <- c(
      "connectance", "weighted_connectance",
      "nbnode", "w_trph_lvl_avg", "pisc_stat")
  }

  col_to_keep <- c("opcod", metrics)
  net <- network_metrics[, colnames(network_metrics) %in% col_to_keep]

  net %<>%
    unnest(cols = pisc_stat)

  net %<>%
    dplyr::filter(!is.na(opcod))

  return(net)
}

#' Get fishing operation data
#'
#'
get_op_data <- function (path = get_mypath("data", "op_analysis.rda")) {

  load(path, envir = environment())

  col_to_keep <- c("opcod", "station", "year", "surface")
  op <- op_analysis[, colnames(op_analysis) %in% col_to_keep]

  op %<>%
    dplyr::filter(!is.na(opcod) | !is.na(station))

  return(op)
}


#' Get fishing operation data
#'
#' @details input are data from get_* op, community, network _data functions
#' 
#' @param net df
#' @param com df
#' @param op df 
#'
get_full_data <- function (net = NULL, com = NULL, op = NULL) {

  full <- op %>% 
    dplyr::left_join(com, by = "opcod") %>%
    dplyr::left_join(net, by = "opcod") %>%
    dplyr::filter(!is.na(opcod) | !is.na(station))

  return(full)

}

#' Get group of community based on median biomass 
#'
#'
get_biomass_group <- function(.data = data) {
 
  .data %>%
  select(station, biomass, log_bm) %>%
  pivot_longer(-station, names_to = "coeff", values_to = "biomass") %>%
  group_by(station, coeff) %>%
  summarise(bm_med = median(biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(coeff == "biomass") %>%
  mutate(com_size = ifelse(bm_med < median(bm_med, na.rm = TRUE), "little", "big")) %>%
  ungroup() %>%
  select(station, com_size)
}

get_stream_group <- function (op_analysis_path = NULL, habitat_press_path = NULL) {
  load(file = op_analysis_path, envir = environment())
  load(file = habitat_press_path, envir = environment())

  width_classif_mean <- habitat_press %>%
    mutate(
      avg = if_else(width_river_mean < mean(width_river_mean, na.rm = TRUE),
	"little", "big"),
      median = if_else(width_river_mean < median(
	  width_river_mean, na.rm = TRUE), "little", "big")) %>%
  pivot_longer(cols = c(avg, median), names_to = "group_type", values_to =
    "group") %>%
  select(station, width_river_mean, group_type, group)

protocol_station <- op_analysis %>%
  group_by(station) %>%
  summarise(protocol_type = unique(protocol_type))

output <- width_classif_mean %>% 
  left_join(protocol_station, by = "station")

return(output)

}

update_files <- function () {

  folder <- "~/Documents/post-these/mnhn/fishcom" 
  data_folder <- paste0(folder, "/data") 
  net_folder <- paste0(folder, "/data/classes") 

  file.copy(
    from = c(
      paste0(data_folder, "/op_analysis.rda"), 
      paste0(data_folder, "/community_metrics.rda")
      ),
    to = get_mypath("data"),
   overwrite = TRUE,
   copy.date = TRUE
  )

  file.copy(
    from = c(paste0(net_folder, "/network_metrics.rda")),
    to = get_mypath("data", "classes"),
   overwrite = TRUE,
   copy.date = TRUE
   )
}
