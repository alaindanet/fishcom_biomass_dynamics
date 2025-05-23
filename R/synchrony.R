################################################################################
#                        Functions to compute synchrony                        #
################################################################################

compute_sp_nb_from_com_mat <- function (com_mat = NULL) {

  rowSums(com_mat != 0)

}

compute_avg_cv_sp <- function(biomass, variance) {
  #Check that the species are in the same order in the vector:
  stopifnot(names(biomass) == names(variance))

  rel_biomass <- biomass / sum(biomass)
  rel_sdt <- sqrt(variance) / biomass

  cv_avg <- sum(rel_biomass * rel_sdt)
  return(cv_avg)
}

compute_cv_com <- function(synchrony, cv_sp) {
  sqrt(synchrony) * cv_sp
}

compute_synchrony <- function(cov_mat) {
  com_var <- sum(cov_mat)
  var_intra_sp <- sum(sqrt(diag(cov_mat)))
  phi <- com_var / (var_intra_sp^2)
  return(phi)
}

get_sync_cv_mat <- function(com_analysis = NULL, op_analysis = NULL, presence_threshold = 0.1) {

  # If it is network data, change the "species" colum
  stopifnot(any(c("species", "sp_class") %in% names(com_analysis)))
  if ("sp_class" %in% names(com_analysis)) {
    com_analysis %<>% dplyr::rename(species = sp_class)
    troph <- TRUE
  } else {
    troph <- FALSE 
  }

  com <- dplyr::ungroup(com_analysis) %>%
    dplyr::left_join(dplyr::select(op_analysis, opcod, station, year), by = "opcod") %>%
    dplyr::filter(!is.na(station))

  if (troph) {
    com %<>% dplyr::select(station, year, species, biomass, troph_level,troph_group)
  } else {
    com %<>% dplyr::select(station, year, species, biomass)
  }
  

  if (!is.na(presence_threshold)) {
    # We will compute the variance of each species in each station, which
    # require that there is some obsversation
    filter_na <- com %>%
      dplyr::group_by(station, species) %>%
      dplyr::summarise(n = sum(!is.na(biomass)) / dplyr::n()) %>%
      dplyr::mutate(to_rm = ifelse(n < presence_threshold, TRUE, FALSE)) %>%
      dplyr::select(-n)

    com %<>%
      dplyr::left_join(filter_na) %>%
      dplyr::filter(!to_rm) %>%
      dplyr::select(-to_rm)
  }

  # Get 0 biomass when the species is absent of station
  # By station replicate complete species list observed:
  complete_com <- com %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(
      species = list(unique(species)),
      year = list(unique(year))) %>%
    dplyr::mutate(comb = purrr::map2(species, year, function(sp, year){
	test <- expand.grid(species = sp, year = year)
	return(test)
})
      ) %>%
    dplyr::select(station, comb) %>%
    tidyr::unnest(comb)

  # Join and put biomass to 0 when no observed:
  complete_com %<>% dplyr::left_join(com, by = c("station", "species", "year")) %>%
    dplyr::mutate(biomass = ifelse(is.na(biomass), 0, biomass))

  if (troph) {
    troph_data <- complete_com[, c("station", "species", "year", "biomass", "troph_level", "troph_group")] %>%
      dplyr::group_by(station) %>%
      tidyr::nest()
    complete_com %<>%
      dplyr::select(-troph_level, -troph_group)
  }
  complete_com %<>%
    dplyr::group_by(station) %>%
    tidyr::nest()

  complete_com %<>%
    dplyr::mutate(
      com_mat_year = purrr::map(data, function(x) tidyr::spread(x, species, biomass)),
      com_mat = purrr::map(com_mat_year, function(x) dplyr::select(x, -year)),
      com_mat = purrr::map(com_mat, as.matrix)
    )

  synchrony <- complete_com %>%
    dplyr::mutate(
      richness = purrr::map(com_mat, compute_sp_nb_from_com_mat),
      richness_med = purrr::map_dbl(richness, median),
      avg_sp = purrr::map(com_mat, colMeans),
      cov_mat = purrr::map(com_mat, cov),
      var_sp = purrr::map(cov_mat, diag),
      synchrony = purrr::map_dbl(cov_mat, compute_synchrony),
      cv_sp = purrr::map2_dbl(avg_sp, var_sp, compute_avg_cv_sp),
      cv_com = compute_cv_com(synchrony = synchrony, cv_sp = cv_sp),
      cv_classic = purrr::map2_dbl(cov_mat, com_mat, function(variance, biomass) {
	sqrt(sum(variance)) / mean(rowSums(biomass))
})
    )

  # To get biomass and troph information:
  if (troph) {
    synchrony$data <- troph_data$data
  }

  return(synchrony)

}

#' Compute species contribution
#'
#' @param mat matrix of species biomass across time. The species are in column
#' and the sampling date in rows 
#' @param cv_com_tot dbl CV of the community with all the species 
#' @param cv_sp_tot dbl Average CV of the species with all the species 
#' @param sync_tot dbl synchrony of the species with all the species 
#' @details contribution are computed as follow: C_k = M_{-t} -M 
#' @value return a data.frame with species contribution to total cv, species cv
#' and synchrony
compute_sp_contrib <- function (
  mat = NULL,
  cv_com_tot = NULL,
  cv_sp_tot = NULL,
  sync_tot = NULL) {

  stopifnot(is.matrix(mat))

  if (ncol(mat) < 2) {
   return(
     data.frame(species = NA,
       cv_com = NA,
       cv_sp = NA,
       sync = NA, 
       cbt_cv_com = NA,
       cbt_cv_sp = NA, 
       cbt_sync = NA
     ) 
     )
  }

  sp <- colnames(mat)
  # Loop over species
  leave_one_out <- purrr::map_dfr(sp, function(x, mat) {
    # Remove one species
    tmp_mat <- mat[, colnames(mat) != x, drop = FALSE]
    # Compute cv and synchrony
    avg_sp <- colMeans(tmp_mat)
    cov_mat <- cov(tmp_mat)
    var_sp <- diag(cov_mat)
    synchrony <- compute_synchrony(cov_mat)
    cv_sp <- compute_avg_cv_sp(avg_sp, var_sp)
    cv_com <- compute_cv_com(synchrony = synchrony, cv_sp = cv_sp)
    # Save sp name, cv and synchrony
    list(species = x, cv_com_sp = cv_com, cv_sp_sp = cv_sp, sync_sp = synchrony)
  }, mat = mat)

  # Compute contribution
  contrib <- leave_one_out %>%
    mutate(
      cbt_cv_com = cv_com_tot - cv_com_sp,
      cbt_cv_sp = cv_sp_tot - cv_sp_sp,
      cbt_sync = sync_tot - sync_sp
    )
  contrib
}

