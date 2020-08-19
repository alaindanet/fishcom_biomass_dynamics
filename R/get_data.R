#' Get community metrics
#'
#' @param dir path to the data
get_community_data <- function (dir = get_mypath("data")) {

  myload(community_metrics, dir = dir, envir = environment())

  com <- community_metrics %>% 
    dplyr::select(opcod, biomass, richness) %>%
    dplyr::filter(!is.na(opcod))

  return(com)
  
}

#' Get network metrics
#'
#' @param dir path to the data
get_network_data <- function (dir = get_mypath("data", "classes"), metrics = NULL) {

  
  myload(network_metrics, dir = dir, envir = environment())

  if (is.null(metrics)) {
    metrics <- c(
      "connectance", "weighted_connectance",
      "nbnode", "w_trph_lvl_avg")
  }

  col_to_keep <- c("opcod", metrics)
  net <- network_metrics[, colnames(network_metrics) %in% col_to_keep]

  net %<>%
    dplyr::filter(!is.na(opcod))

  return(net)
}

#' Get fishing operation data
#'
#'
get_op_data <- function (dir = get_mypath("data")) {

  myload(op_analysis, dir = dir, envir = environment())

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

