get_station_map <- function(st = st_mono_trends_stable_rich_bm) {

  library("rnaturalearth")
  library("rnaturalearthdata")
  library(sf)
  library("ggspatial")

  # Basin data
  basin <- read_sf(get_mypath("data-raw", "basin_dce", "BassinDCE.shp"))

  if (!dir.exists(get_mypath("data-raw", "basin_dce"))) {
    dir.create(get_mypath("data-raw", "basin_dce"))
  }
  if (!file.exists(get_mypath("data-raw", "basin_dce", "basin_dce.zip"))) {
    download.file(
      url = "https://www.data.gouv.fr/fr/datasets/r/7c3a45a7-518a-4415-80aa-c59e427592c5",
      destfile = get_mypath("data-raw", "basin_dce", "basin_dce.zip")
    )
    unzip(zipfile = get_mypath("data-raw", "basin_dce", "basin_dce.zip"))
  }
  basin <- read_sf(get_mypath("data-raw", "basin_dce", "BassinDCE.shp"))
  basin %<>% st_transform(crs = 2154)
  basin %<>% filter(CdBassinDC %in% c("B1", "B2", "A", "D", "F", "C", "G", "H"))
  basin <- rmapshaper::ms_simplify(input = basin) %>%
    st_as_sf()
  basin_inner <- basin %>%
    rmapshaper::ms_innerlines() %>%
    as_tibble() %>%
    st_as_sf()

  # World data
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_transform(crs = 2154)
  country_points <- st_centroid(world) %>%
    filter(name %in% c("France"))
  country_points <- cbind(country_points, st_coordinates(country_points$geometry))
  country_points[country_points$name == "Germany", c("X", "Y")] <- c(1200000, 7000000) 
  country_points[country_points$name == "Spain", c("X", "Y")] <- c(300000, 6150000) 
  country_points[country_points$name == "Italy", c("X", "Y")] <- c(1300000, 6450000) 
  ocean <- data.frame(X = 250000, Y = 6500000, name = "Atlantic\nOcean")
  sea <- data.frame(X = c(450000, 900000), Y = c(7025000, 6180000), name = c("English \n channel", "Mediterranean \n sea"))

  # Station
  loadd(station_analysis)
  st <- station_analysis %>%
    filter(id %in% st) %>%
    st_transform(crs = 2154)

  #
  # Font size
  font_size <- 10
  size_label <- 3.5

  p <- ggplot() +
    geom_sf(data = st_geometry(world), fill = "antiquewhite") +
    # Coutry
    geom_text(data= country_points,aes(x=X, y=Y, label=toupper(name)),
      color = "black", fontface = "bold", size = size_label, check_overlap = FALSE) +
    # Ocean + sea
    geom_text(data= ocean,aes(x=X, y=Y, label= name),
      color = "darkblue", fontface = "bold", check_overlap = FALSE, size = size_label) +
    geom_text(data= sea,aes(x=X, y=Y, label=name),
      color = "darkblue", fontface = "bold", check_overlap = FALSE, size = size_label) +
    #theme_map() +
    # Station
    geom_sf(data = st_geometry(st)) +
    # Plot basin
    geom_sf(data = basin_inner) +
    coord_sf(
      xlim = c(60000, 1100000),
      ylim = c(6100000, 7122216),
      datum = sf::st_crs(2154),
      crs = 2154,
      expand = FALSE
    )

    p +
      annotation_north_arrow(
        location = "bl", which_north = "true",
        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
        style = north_arrow_fancy_orienteering) +
      annotation_scale(location = "bl", width_hint = 0.25) +
      xlab("Longitude") +
      ylab("Latitude") +
      theme(
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

}
