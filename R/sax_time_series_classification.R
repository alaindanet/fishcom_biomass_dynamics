get_sax_data <- function (dir = get_mypath("data")) {

  myload(biomass_ts_sax, dir = dir, envir = environment())

  return(biomass_ts_sax)
}

get_monotonous_station <- function (.data = NULL, sax = NULL) {

  if (is.null(sax)) {
    sax <- get_sax_data()
  }

  station_monotonous <- sax[sax$sax %in% c("cba", "bbb", "abc"), ]$station

  output <- .data %>%
    dplyr::filter(station %in% station_monotonous)
  
  return(output)
  
}
