#####################
#  Plot by station  #
#####################

mypath <- rprojroot::find_package_root_file
library(tidyverse)
library(magrittr)
library(cowplot)
theme_set(theme_cowplot())

load(paste0(mypath(), "/biomass_cba.RData"))

plot_station <- biomass_cba %>%
  group_by(station) %>%
  nest() %>%
  mutate(p = map2(data, station,
      function (.data, titre) {
	.data %>% 
	  ggplot(aes(x = year, y = biomass)) +
	  geom_point() +
	  ggtitle(paste0("Plot ", titre)) +
	  geom_smooth(method = lm)
      }
      ))
plot_station$p[[1]]

index_station <- seq_along(plot_station$station)
nb_station_by_plot <- 4
index_plot_station <- split(index_station, ceiling(index_station / nb_station_by_plot))

if (!dir.exists(mypath("figures"))) {
  dir.create(mypath("figures"))
}

for (i in seq_along(index_plot_station)) {

  tmp_grid <- plot_grid(plotlist = plot_station$p[index_plot_station[[i]]])

  file_name <- mypath("figures", paste0("plot_bm_station_", i, ".pdf"))
  save_plot(filename = file_name, plot = tmp_grid)
}
