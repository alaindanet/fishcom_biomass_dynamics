##Data frame richesses totales

#Importation des packages
library(dplyr)
library(magrittr)
library(tidyverse)

#Importation des fichiers
load("~/Desktop/Stage M2 CESCO/fishcom/data/biomass_ts_sax.rda")
load("~/Desktop/Stage M2 CESCO/fishcom/data/community_metrics.rda")
load("~/Desktop/Stage M2 CESCO/fishcom/data/op_analysis.rda")

### Data frame : all_richnesses (opcod, station, biomass, year)
op <- dplyr::select(op_analysis, station, opcod, year, surface)
com <- dplyr::select(community_metrics, opcod, biomass)
op_com <- left_join(com, op, by = "opcod") %>%
  filter(!is.na(station) & !is.na(opcod))

# Ajout de la classification cba
classification <- dplyr::select(biomass_ts_sax, station, sax)
# S'assurer que la variable station soit en character dans les deux data frame
op_station <- dplyr::select(op_analysis, station, opcod, year, surface) %>%
  mutate(station = as.character(station))

com_class <- com %>%
  # ajouter les stations à community analysis
  left_join(op_station, by = "opcod") %>%
  # ajouter les classifications
  left_join(classification, by = "station")
# Ne garder que les cba
biomass_cba <- filter(com_class, sax == "cba") %>%
  dplyr::select(opcod, station, biomass, year, sax, surface)
# Ne garder que les abc
biomass_abc <- filter(com_class, sax == "abc") %>%
  dplyr::select(opcod, station, biomass, year, sax, surface)
# Ne garder que les bbb
biomass_bbb <- filter(com_class, sax == "bbb") %>%
  dplyr::select(opcod, station, biomass, year, sax, surface)

all_biomasses <- bind_rows(biomass_abc, biomass_bbb, biomass_cba)

#Ajout de la biomasse initiale
#Fonction me donne 51k obs, utiliser un left join 
all_biomasses_bi <- dplyr::left_join(all_biomasses,initial_biomass)
