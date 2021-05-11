get_basin_station <- function (sf_obj = FALSE) {

  mypath <- 
    myload(the_8_hydrologic_basin, station_analysis,
      envir = environment(), dir = get_mypath("data"))

  station_analysis %<>%
    sf::st_transform(crs = 2154)
  the_8_hydrologic_basin %<>%
    dplyr::mutate(NomDistric = ifelse(CdBassinDC == "B2", "SAMBRE", NomDistric))

  station_basin <- sf::st_intersects(station_analysis, the_8_hydrologic_basin)
  station_analysis$basin <- purrr::map_chr(station_basin, function(x){
    if (length(x) == 0) {
      return(NA)
    }
    the_8_hydrologic_basin[["NomDistric"]][x]
    })
  station_analysis %<>%
    #filter(!is.na(basin)) %>%
    dplyr::mutate(station = id) %>%
    dplyr::select(station, basin)

  if (!sf_obj) {
    sf::st_geometry(station_analysis) <- NULL
  }

  return(station_analysis)

}
