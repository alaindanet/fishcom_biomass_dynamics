##Data frame connectances totales

#Importation des packages
library(dplyr)
library(magrittr)
library(tidyverse)

#Importation des fichiers
load("~/Desktop/Stage M2 CESCO/fishcom/data/biomass_ts_sax.rda")
load("~/Desktop/Stage M2 CESCO/fishcom/data/community_metrics.rda")
load("~/Desktop/Stage M2 CESCO/fishcom/data/op_analysis.rda")
load("~/Desktop/fishcom_biomass_dynamics/data/classes/network_metrics.rda")

### Data frame : all_connectances (opcod, station, connectance, year)
op <- dplyr::select(op_analysis, station, opcod, year, surface)
net <- dplyr::select(network_metrics, opcod, connectance)
op_net <- left_join(net, op, by = "opcod") %>%
  filter(!is.na(station) & !is.na(opcod))

# Ajout de la classification cba
classification <- dplyr::select(biomass_ts_sax, station, sax)
# S'assurer que la variable station soit en character dans les deux data frame
op_station <- dplyr::select(op_analysis, station, opcod, year, surface) %>%
  mutate(station = as.character(station))

net_class <- net %>%
  # ajouter les stations à community analysis
  left_join(op_station, by = "opcod") %>%
  # ajouter les classifications
  left_join(classification, by = "station")
# Ne garder que les cba
connectance_cba <- filter(net_class, sax == "cba") %>%
  dplyr::select(opcod, station, connectance, year, sax, surface)
# Ne garder que les abc
connectance_abc <- filter(net_class, sax == "abc") %>%
  dplyr::select(opcod, station, connectance, year, sax, surface)
# Ne garder que les bbb
connectance_bbb <- filter(net_class, sax == "bbb") %>%
  dplyr::select(opcod, station, connectance, year, sax, surface)

all_connectances <- bind_rows(connectance_abc, connectance_bbb, connectance_cba)

