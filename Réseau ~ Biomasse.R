# Importation des packages
library (tidyverse)
library (dplyr)
library (magrittr)
#Import des fichiers
load("~/Desktop/fishcom_biomass_dynamics/dt_trphlvl.RData")
load("~/Desktop/fishcom_biomass_dynamics/dt_richness.RData")
load("~/Desktop/fishcom_biomass_dynamics/dt_biomass.RData")

#Création d'un df avec les pentes de : biomasse, richesse, niveau trophique moyen 
biom <- dplyr::select(dt_biomass, station, Pente, p.value_pente) %>%
  transmute(Pente_Biomass = Pente, p.value_pente_Biomass = p.value_pente)
rich <- dplyr::select(dt_richness, station, Pente, p.value_pente) %>%
  transmute(Pente_Richness = Pente, p.value_pente_Richness = p.value_pente)
troph <- dplyr::select(dt_trphlvl, station, Pente, p.value_pente) %>%
  transmute(Pente_Trophlvl = Pente, p.value_pente_Trophlvl = p.value_pente)

inter <- left_join (biom, rich, by = "station")
df_dt <- left_join (inter, troph, by = "station")

#Dynamique temporelle richesse ~ Dynamique temporelle biomasse
plot(df_dt$Pente_Biomass,df_dt$Pente_Richness)

#Dynamique temporelle niveau trophique moyen ~ Dynamique temporelle biomasse
plot(df_dt$Pente_Biomass,df_dt$Pente_Trophlvl)
