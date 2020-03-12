#Importation des packages
library(dplyr)
library(magrittr)
library(tidyverse)

#Importation des fichiers
load("~/Desktop/Stage M2 CESCO/fishcom/data/biomass_ts_sax.rda")
load("~/Desktop/Stage M2 CESCO/fishcom/data/community_metrics.rda")
load("~/Desktop/Stage M2 CESCO/fishcom/data/op_analysis.rda")
load("~/Desktop/Stage M2 CESCO/network_metrics.rda")

### Data frame : biomass_cba (opcod, station, biomass, year)
op_cba <- dplyr::select(op_analysis, station, opcod, year)
com_cba <- dplyr::select(community_metrics, opcod, biomass)
op_com_cba <- left_join(com_cba, op_cba, by = "opcod") %>%
  filter(!is.na(station) & !is.na(opcod))

# Ajout de la classification cba
classification_cba <- dplyr::select(biomass_ts_sax, station, sax)
# S'assurer que la variable station soit en character dans les deux data frame
op_station <- dplyr::select(op_analysis, station, opcod, year) %>%
  mutate(station = as.character(station))

com_class <- com_cba %>%
# ajouter les stations à community analysis
  left_join(op_station, by = "opcod") %>%
# ajouter les classifications
  left_join(classification_cba, by = "station")
# Ne garder que les cba
biomass_cba <- filter(com_class, sax == "cba") %>%
  dplyr::select(opcod, station, biomass, year)
#############################################################
### Data frame : richness_cba (opcod, station, richness, year)
op_rich <- dplyr::select(op_analysis, station, opcod, year)
com_rich <- dplyr::select(community_metrics, opcod, richness)
op_com_rich <- left_join(com_rich, op_rich, by = "opcod") %>%
  filter(!is.na(station) & !is.na(opcod))

# Ajout de la classification cba
classification_cba <- dplyr::select(biomass_ts_sax, station, sax)
# S'assurer que la variable station soit en character dans les deux data frame
op_station_rich <- dplyr::select(op_analysis, station, opcod, year) %>%
  mutate(station = as.character(station))

com_class_rich <- com_rich %>%
  # ajouter les stations à community analysis
  left_join(op_station_rich, by = "opcod") %>%
  # ajouter les classifications
  left_join(classification_cba, by = "station")
# Ne garder que les cba
richness_cba <- filter(com_class_rich, sax == "cba") %>%
  dplyr::select(opcod, station, richness, year)
#############################################################
### Data frame : trphlvl_cba (opcod, station, troph_lvl, year)
op_trph <- dplyr::select(op_analysis, station, opcod, year)
net_trph <- dplyr::select(network_metrics, opcod, w_trph_lvl_avg)
op_net_trph <- left_join(net_trph, op_trph, by = "opcod") %>%
  filter(!is.na(station) & !is.na(opcod))

# Ajout de la classification cba
classification_cba <- dplyr::select(biomass_ts_sax, station, sax)
# S'assurer que la variable station soit en character dans les deux data frame
op_station_trph <- dplyr::select(op_analysis, station, opcod, year) %>%
  mutate(station = as.character(station))

net_class_trph <- net_trph %>%
  # ajouter les stations à community analysis
  left_join(op_station_trph, by = "opcod") %>%
  # ajouter les classifications
  left_join(classification_cba, by = "station")
# Ne garder que les cba
trphlvl_cba <- filter(net_class_trph, sax == "cba") %>%
  dplyr::select(opcod, station, w_trph_lvl_avg, year)
