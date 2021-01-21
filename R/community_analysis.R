# Turn community analyze steps into functions 

#' Remove species from community and network
#' 
#' @param 
#' @param com community_analysis
rm_sp_from_com <- function (com = NULL, sp_to_rm = NULL) {

  com %<>%
    filter(!species %in% sp_to_rm)

  return(com)

}

#' Summarise network metrics over time
#'
#' Compute the median and the CV of network metrics through time 
#'
#' @param op data.frame containing opcod and station 
#' @param network data.frame containing opcod and network metrics 
#'
#'
summarise_network_over_time <- function (
  op = NULL,
  class_network = NULL,
  species_network = NULL,
  metrics = NULL,
  type_metrics = "species") {

  op %<>%
    dplyr::select(opcod, station, year, surface)

  # Metrics are computed with a type of network
  stopifnot(type_metrics %in% c("species", "classes"))
  if (type_metrics == "classes") {
   com <- op %>%
    dplyr::left_join(class_network, by = "opcod") 
    
  } else {
   com <- op %>%
    dplyr::left_join(species_network, by = "opcod") 
  }
   com %<>%
    dplyr::group_by(station) %>%
    dplyr::summarise_at(metrics,
      list(cv = ~sd(.) / mean(.), med = median, stab = ~mean(.) / sd(.)))

    ##  Trophic group metrics are computed with class network
    # Get total richness, biomass_med,  by station and trophic group 
  composition <- class_network %>%
    dplyr::left_join(op, by = "opcod") %>%
    dplyr::select(station, composition) %>%
    tidyr::unnest(composition) %>%
    dplyr::mutate(species = str_extract_all(sp_class, "[A-Z]{3}"))

  sum_surface <- op %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(sum_surface = sum(surface))

  richness_tot <- composition %>%
    dplyr::group_by(station, troph_group) %>%
    dplyr::summarise(richness_tot = length(unique(species))) %>%
    dplyr::left_join(sum_surface, by = "station") %>%
    dplyr::mutate(rich_tot_std = richness_tot / sum_surface) %>%
    dplyr::select(-sum_surface)

  ## Biomass
  troph_group <- class_network %>%
    dplyr::left_join(op, by = "opcod") %>%
    dplyr::select(station, opcod, troph_group) %>%
    unnest(troph_group) %>%
    dplyr::group_by(station, troph_group) %>%
    dplyr::summarise_at(c("biomass", "bm_std", "richness", "rich_std",
	"nbnode"), list(cv = ~sd(.) / mean(.), med = median, avg = mean, stab =
	~mean(.) / sd(.)))

  # Merge trophic group metrics
  troph_group_metrics <- troph_group %>%
    dplyr::left_join(richness_tot, by = c("station", "troph_group")) %>%
    tidyr::nest(troph_group = -station)

  com %<>% dplyr::left_join(troph_group_metrics, by = "station")

  return(com)
}

#' Summarise Biomass and species richness by trophic group over time 
#'
#' This function computes biomass stability, species richness average and
#' median, etc, ... by trophic group (i.e. group 2 and 3).
#'
#' @inheritParams summarise_network_over_time
#'
summarise_bm_troph_over_time <- function (
  op = NULL,
  network = NULL
) {

  ## Get sum_surface: 
  sum_surface <- op %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(sum_surface = sum(surface))

  op %<>% dplyr::select(opcod, station, year)
  # Match network to fish operation 
  net <- network %>%
    dplyr::left_join(op, by = "opcod") %>%
    dplyr::ungroup() %>%
    filter(!is.na(station) & !is.na(opcod)) %>%
    dplyr::select(station, troph_group) %>%
    tidyr::unnest(troph_group)

  # Summarise biomass and community characteristics
  biomass_variation <- net %>%
    dplyr::group_by(station, troph_group) %>%
    dplyr::summarise_all(
      list(
	med = median,
	avg = mean,
	cv = ~sd(.) / mean(.),
	stab = ~mean(.) / sd(.))
    )

  # Get total richness:
  tot_richness <- network %>% 
    dplyr::left_join(op, by = "opcod") %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(station) & !is.na(opcod)) %>%
    dplyr::select(opcod, station, composition) %>%
    tidyr::unnest(composition) %>%
    dplyr::mutate(species = str_extract_all(sp_class, "[A-Z]{3}")) %>%
    dplyr::group_by(station, troph_group) %>%
    dplyr::summarise(richness_tot = length(unique(species))) %>%
    dplyr::left_join(sum_surface, by = "station") %>%
    dplyr::mutate(rich_tot_std = richness_tot / sum_surface) %>%
    dplyr::select(-sum_surface)

  # Select variables to keep:
  var_to_keep <- "biomass|bm_std|rich_std|richness_avg|richness_med|richness_tot|rich_tot_std|station|troph_group"
  biomass_variation %<>%
    dplyr::left_join(tot_richness, by = c("station", "troph_group")) %>%
    select_at(vars(dplyr::matches(var_to_keep)))

  # Merge with temporal_network_metrics
  biomass_variation %<>%
    dplyr::group_by(station) %>%
    tidyr::nest(troph_group = c(troph_group, matches(gsub(x = var_to_keep, "\\|station", "")))) #bm_std, rich_std, richness_avg, richness_med, richness_tot, rich_tot_std

  return(biomass_variation)

}

#' Summarise community metrics over time 
#'
#' @inheritParams summarise_network_over_time
#' @param com data.frame containing opcod and community description metrics 
#'
summarise_com_over_time <- function (
  op = NULL,
  com = NULL
) {

    if ("station" %in% names(com)) {
     com$station <- NULL
    }

var_to_summarise <- c("richness", "rich_std", "nind", "nind_std", "biomass", "bm_std", "pielou", "simpson")

  output <- com %>%
    dplyr::left_join(op, by = c("opcod")) %>%
    dplyr::filter(!(is.na(station) | is.na(opcod))) %>%
    dplyr::select_at(c("station", var_to_summarise)) %>%
    dplyr::group_by(station) %>%
    dplyr::summarise_at(var_to_summarise,
      list(avg = mean, cv =~ sd(.) / mean(.), med = median, stab = ~mean(.) / sd(.)))
  return(output)
}

get_temporal_betadiv_from_com_mat_year <- function (com = NULL) {

com_mat_year_nested$beta <- c(
  NA,
  map(com_mat_year_nested$year[-1],
    function(y, .df) {

      year_n <- .df[.df$year == y, ]$data[[1]]
      year_n_1 <- .df[.df$year == 1995, ]$data[[1]]

      out <- betapart::beta.temp(
	year_n_1[, !colnames(year_n_1) %in% "station"],
	year_n[, !colnames(year_n) %in% "station"]
      )
      output <- tibble(
	station = year_n$station,
	out
      )
      return(output)
    
    }, .df = com_mat_year_nested)
)

}

get_temporal_betapart_from_com_mat_station <- function (com = NULL) {

  com$betapart <- map(com$data,
    ~ beta.multi(.x[, !colnames(.x) %in% c("year")])
  )
  com$be = map(com$betapart,
    function (b) {
      enframe(b) %>%
	unnest() %>%
	pivot_wider(names_from = "name", values_from = "value")
    }
  )
 
  com %>%
    select(station, betapart, be)

}

get_com_mat_station <- function(com = NULL, .op = NULL, variable = "biomass", presence_absence = FALSE) {

  var_sym <- sym("biomass")
  tmp_com_op <- com %>%
    left_join(select(.op, opcod, station, year), by = "opcod") %>%
    filter(!is.na(station), !is.na(year)) 

  tmp_com_mat <- tmp_com_op %>%
    select(station, year, species, {{var_sym}}) %>%
    group_by(station) %>%
    arrange(year) %>%
    nest()

  com_mat_station <- tmp_com_mat %>%
    mutate(
      data = map(data,
	~.x %>% 
	  pivot_wider(names_from = species, values_from = {{var_sym}}) %>%
	  mutate_at(vars(matches("[A-Z]{3}", ignore.case = FALSE)), ~ifelse(is.na(.), 0, .))
      )
    )

  if (presence_absence) {
    com_mat_station$data <-
      map(com_mat_station$data,
	~ .x %>%
	  mutate_at(vars(matches("[A-Z]{3}", ignore.case = FALSE)), ~ifelse(is.na(.), 0, .)) %>%
	  mutate_at(vars(matches("[A-Z]", ignore.case = FALSE)), ~ifelse(. != 0, 1, .))
    )
  }

  return(com_mat_station)

}
get_com_mat_year <- function(com = NULL, .op = NULL, variable = "biomass") {

  var_sym <- sym("biomass")
  tmp_com_op <- com %>%
    left_join(select(.op, opcod, station, year), by = "opcod") %>%
    filter(!is.na(station), !is.na(year)) 

  tmp_com_mat <- tmp_com_op %>%
    select(station, year, species, {{var_sym}}) %>%
    pivot_wider(names_from = species, values_from = {{var_sym}}) %>%
    mutate_all(~ifelse(is.na(.), 0, .)) 

  tmp_com_mat %>%
    group_by(year) %>%
    arrange(station) %>%
    nest()

  comb <- expand.grid(
    station = unique(tmp_com_mat$station),
    year = unique(tmp_com_mat$year)) %>%
    as_tibble()

  com_mat_year <- comb %>%
    left_join(tmp_com_mat, by = c("station", "year")) %>%
    mutate_at(vars(matches("[A-Z]{3}", ignore.case = FALSE)), ~ifelse(is.na(.), 0, .)) %>%
    mutate_at(vars(matches("[A-Z]", ignore.case = FALSE)), ~ifelse(. != 0, 1, .)) %>%
    group_by(year) %>%
    arrange(year, station) 

  return(com_mat_year)

}

#' Compute temporal beta-diversity
#'
#' @param com data.frame community_analysis 
#'
#'
compute_temporal_betadiv <- function (
  .op = NULL, com = NULL, variable = "biomass") {

  var_sym <- sym(variable)

  com %<>%
    dplyr::ungroup() %>%
    dplyr::left_join(.op, by = "opcod") %>%
    dplyr::filter(!(is.na(station) | is.na(opcod))) %>%
    dplyr::select(station, year, species, {{var_sym}}) %>%
    dplyr::group_by(station, year, species) %>%
    dplyr::summarise({{var_sym}} := sum({{var_sym}})) %>%
    dplyr::group_by(station) %>%
    dplyr::arrange(year) %>%
    tidyr::nest() %>%
    dplyr::ungroup()
  ## build community matrices
  com %<>%
    dplyr::mutate(
      com = furrr::future_map2(data, station, function(x, y) {

	x %<>%
	  tidyr::spread(species, {{var_sym}}) %>%
	  dplyr::mutate_at(vars(-year), list(~replace(.,is.na(.), as.integer(0)))) %>%
	  dplyr::arrange(year) %>%
	  dplyr::select(-year)
	x
	}
      )
    )
  # Here I take the overall mean of differences between years but could be an idea
  # to consider the mean of dissimilarity year to year (i.e. the diagonal values)
  betadiv <- com %>%
    dplyr::mutate(
      betadiv = furrr::future_map_dbl(com, compute_betadiv),
      betadiv_bin = furrr::future_map_dbl(com, compute_betadiv, binary = TRUE),
      betadiv_diag = furrr::future_map_dbl(com, compute_betadiv, time_to_time = TRUE),
      betadiv_bin_diag = furrr::future_map_dbl(com, compute_betadiv, binary = TRUE, time_to_time = TRUE)
    )

} 

#' Compute betadiv
#'
#'
#'@export
compute_betadiv <- function(com, binary = FALSE, time_to_time = FALSE) {

  if (binary) {
    com %<>% replace(., . != 0, 1)
  }

  dist_mat <- vegan::vegdist(com, method = "bray", binary = binary)

  if (time_to_time) {
    dist_mat <- diag(dist_mat)
  }

  mean(dist_mat)
}

#' Compute synchrony with species
#'
#' 
#' @param com community_analysis
#'
#'
compute_com_synchrony <- function (.op = NULL, com = NULL, ...) {

  synchrony <- get_sync_cv_mat(com_analysis = com,
    op_analysis = .op, presence_threshold = NA)

  synchrony %<>%
    dplyr::mutate(
      contrib = purrr::pmap(
	list(mat = com_mat, cv_com_tot = cv_com, cv_sp_tot = cv_sp, sync_tot = synchrony),
	~compute_sp_contrib(mat = ..1, cv_com_tot = ..2, cv_sp_tot = ..3, sync_tot = ..4)),
      richness_tot = map_dbl(com_mat, ~ncol(.x))
    )

  # Total number of of species by unit of surface: 
  sum_surface <- .op %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(sum_surface = sum(surface))
  synchrony %<>%
    dplyr::left_join(sum_surface, by = "station") %>%
    dplyr::mutate(rich_tot_std = richness_tot / sum_surface) %>%
    dplyr::select(station, richness_med,  synchrony, cv_sp, cv_com, cv_classic, contrib, richness_tot, rich_tot_std)

  return(synchrony)
} 
#'
#'
#'
compute_community_temporal_analysis <- function(.op = NULL,
  type_network_metrics = "species", msg = FALSE) {

  output <- vector("list", length = 4)
  names(output) <- c("tps_net", "tps_com", "tps_bm_troph", "sync")

  # Network ---------------------------------------------------------- 
  myload(network_metrics, dir = mypath("data", "classes"))
  class_network_metrics <- network_metrics
  myload(network_metrics, dir = mypath("data", "species"))
  species_network_metrics <- network_metrics
  rm(network_metrics)

  output[["tps_net"]] <- summarise_network_over_time(
    op = .op,
    species_network = species_network_metrics,
    class_network = class_network_metrics,
    metrics = c("weighted_connectance", "connectance", "w_trph_lvl_avg"),
    type_metrics = type_network_metrics
  )

  if (msg) {
    cat("Network done (1/4)\n")
  }

  # Beta-diversity ---------------------------------------------------
  if(!exists("community_analysis")) {
    myload(community_analysis, dir = mypath("data"))
  }
  betadiv <- compute_temporal_betadiv(.op = .op, com = community_analysis)

  if (msg) {
    cat("Beta-diversity done (1.5/4)\n")
  }

  # Synchrony of species ---------------------------------------------
  output[["sync"]] <- compute_com_synchrony(.op = .op, com = community_analysis)

  if (msg) {
    cat("Synchrony done (2/4)\n")
  }
  # 
  com_std <- community_analysis %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::select(.op, opcod, surface), by = "opcod") %>%
    dplyr::mutate(
      nind = nind / surface, 
      length = length / surface, 
      biomass = biomass / surface
    )

  output[["sync_std"]] <- compute_com_synchrony(.op = .op, com = com_std)

  if (msg) {
    cat("Synchrony standardized done (2.5/4)\n")
  }

  # Biomass ---------------------------------------------------------- 
  if(!exists("community_metrics")) {
    myload(community_metrics, dir = mypath("data"))
  }
  output[["tps_com"]] <- summarise_com_over_time(
    op = .op,
    com = community_metrics
  )
  output[["tps_com"]] %<>%
    dplyr::left_join(dplyr::select(betadiv, -data, -com), by = "station")

  if (msg) {
    cat("Community description done (3/4)\n")
  }

  # Biomass by trophic group -----------------------------------------
  myload(network_analysis, dir = mypath("data", "classes"))
  class_network_analysis <- network_analysis
  rm(network_analysis)

  #output[["tps_bm_troph"]] <- summarise_bm_troph_over_time(
    #op = .op,
    #network = class_network_analysis
  #)
  #output[["tps_bm_troph"]] %<>% 
    #tidyr::unnest(troph_group) %>%
    #dplyr::ungroup()

  if (msg) {
    cat("Biomass by trophic group done (4/4)\n")
  }
  return(output)
}


#' Compute community matrix 
#'
#' 
#' @param com community_analysis
#'
#'
compute_com_mat <- function (.op = NULL, com = NULL, variable = "bm_std",
  summarise_by_station = TRUE 
  ) {

  #var_autorized <- c("bm_std", "nind_std", "biomass", "nind", )
  #stopifnot(variable %in% var_autorized)
  var_sym <- rlang::sym(variable)

  #if (!all(var_autorized %in% colnames(com))) {
    #com %<>%
      #dplyr::mutate(
	#bm_std = biomass / surface,
	#nind_std = nind /surface
      #) 
  #}

  com %<>%
    dplyr::filter(!is.na(surface)) %>%
    dplyr::select(opcod, species, {{var_sym}}) %>%
    dplyr::left_join(.op[, c("opcod", "station")], by = "opcod") %>%
    dplyr::filter(!is.na(station)) %>%
    dplyr::group_by(station) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(data, function (.data, summarise_data) {
	.data %<>%
	  tidyr::spread(species, {{var_sym}}) %>%
	  dplyr::mutate_all(list(~ ifelse(is.na(.), 0, .))) 
	if(summarise_data) {
	.data %<>%
	  dplyr::summarise_all(list(median))
	}

	return(.data)
    }, summarise_data = summarise_by_station))
  
  com %<>%
    tidyr::unnest(data) %>%
    dplyr::mutate_all(list(~ ifelse(is.na(.), 0, .))) %>%
    ungroup()

  return(com)
} 

get_pielou_from_com_analysis <- function (com = NULL, variable = NULL) {

  var_sym <- sym(variable)

  test_mat <- com %>%
    ungroup() %>%
    select(opcod, species, {{var_sym}}) %>%
    spread(species, {{var_sym}}) %>%
    mutate_all(list(~ ifelse(is.na(.), 0, .)))

  richness <- vegan::specnumber(test_mat[,-1])
  piel <- vegan::diversity(test_mat[,-1]) / log(richness)

  # Because log(1) = 0
  piel[richness == 1] <- 0 

  output <- tibble(opcod = test_mat[["opcod"]], pielou = piel)
  return(output)

} 

get_piel_nind_bm <- function (com = NULL) {

  piel_nind <- 
    get_pielou_from_com_analysis(
      com = com,
      variable = "nind"
      ) %>%
    select(opcod, piel_nind = pielou)

  piel_bm <- 
    get_pielou_from_com_analysis(
      com = com,
      variable = "biomass"
      ) %>%
    select(opcod, piel_bm = pielou)

  piel_bm %>%
    left_join(piel_nind, by = "opcod")
}

#' Compute Pielou and simpson
#'
#' @param .data a vector of species abundance/biomass 
#'
compute_pielou_simpson <- function(.data) {
  richness <- vegan::specnumber(.data)
  if (richness == 1) {
    pielou <- 0
    simpson <- 0
  } else {
    pielou <- vegan::diversity(.data) / log(richness)
    simpson <- vegan::diversity(.data, index = "simpson")
  }
  out <- tibble( pielou = pielou, simpson = simpson)
  return(out)
}

#' Compute synchrony over basin
#'
#'
#'
compute_synchrony_over_basin <- function(com = NULL, basin = NULL, .op = NULL) {

  com %<>%
    dplyr::left_join(dplyr::select(.op, opcod, station, year), by = "opcod") %>%
    dplyr::left_join(basin, by = "station")

  # Get mean biomass by basin, species and by year
  complete_com <- com %>%
    dplyr::group_by(basin) %>%
    dplyr::summarise(
      species = list(unique(species)),
      year = list(unique(year))) %>%
    dplyr::mutate(comb = purrr::map2(species, year, function(sp, date){
        test <- expand.grid(species = sp, year = date)
        return(test)
    })
      ) %>%
    tidyr::unnest(comb)

  # Join and put biomass to 0 when no observed:
  complete_com %<>% dplyr::left_join(com, by = c("basin", "species", "year")) %>%
    dplyr::mutate(biomass = ifelse(is.na(biomass), 0, biomass)) %>%
    group_by(basin, species, year) %>%
    summarise(biomass = mean(biomass))

  complete_com %<>%
    group_by(basin) %>%
    nest()

  complete_com %<>%
    dplyr::mutate(
      com_mat_date = purrr::map(data, function(x) tidyr::spread(x, species, biomass)),
      com_mat = purrr::map(com_mat_date, function(x) dplyr::select(x, -year)),
      com_mat = purrr::map(com_mat, as.matrix)
    )

  synchrony <- complete_com %>%
    dplyr::mutate(
      richness = purrr::map(com_mat, compute_sp_nb_from_com_mat),
      richness_med = purrr::map_dbl(richness, median),
      richness_tot = map_dbl(com_mat, ~ncol(.x)),
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
  synchrony

}

#' Compute fish body mass 
#' @param length in mm numeric 
#' @param gram logical TRUE (default), miligram if not
calc_fish_body_mass <- function (length = NULL, unit = "gram") {

  weight  <- 0.01 * (length ^ 3.03)

  if (unit == "gram") {
    weight <- weight * 10 ^ -3 #in grams
  }

  return(weight)

}
