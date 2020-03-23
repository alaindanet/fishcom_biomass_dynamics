# Importation des packages
library (tidyverse)
library (dplyr)
library (magrittr)
#Import des fichiers
load("~/Desktop/fishcom_biomass_dynamics/dt_trphlvl.RData")
load("~/Desktop/fishcom_biomass_dynamics/dt_richness.RData")
load("~/Desktop/fishcom_biomass_dynamics/dt_biomass.RData")

#Création d'un df avec les nb_years de : biomasse, richesse, niveau trophique moyen 
biom <- dplyr::select(dt_biomass, station, nb_year, p.value_pente) %>%
  transmute(Pente_Biomass = nb_year, p.value_pente_Biomass = p.value_pente)
rich <- dplyr::select(dt_richness, station, nb_year, p.value_pente) %>%
  transmute(Pente_Richness = nb_year, p.value_pente_Richness = p.value_pente)
troph <- dplyr::select(dt_trphlvl, station, nb_year, p.value_pente) %>%
  transmute(Pente_Trophlvl = nb_year, p.value_pente_Trophlvl = p.value_pente)

df_brich <- left_join (biom, rich, by = "station") %>%
  dplyr::filter(p.value_pente_Richness >= 0 & p.value_pente_Richness <= 0.05) %>%
  mutate(p.value_pente_Biomass = as.numeric(p.value_pente_Biomass)) %>%
  mutate(p.value_pente_Richness = as.numeric(p.value_pente_Richness))

df_btroph <- left_join (biom, troph, by = "station") %>%
  dplyr::filter(p.value_pente_Trophlvl >= 0 & p.value_pente_Trophlvl <= 0.05) %>%
  mutate(p.value_pente_Biomass = as.numeric(p.value_pente_Biomass)) %>%
  mutate(p.value_pente_Trophlvl = as.numeric(p.value_pente_Trophlvl))

#Dynamique temporelle richesse ~ Dynamique temporelle biomasse
plot(df_dt$Pente_Biomass,df_dt$Pente_Richness)

#Colorer en fonction de la p value des nb_years
graphe_richness <- ggplot(df_brich, aes(x=Pente_Biomass, y=Pente_Richness, color=p.value_pente_Richness)) + geom_point()
graphe_richness
graphe_richness + geom_hline(yintercept=0) 

#Colore en fonction de la valeur de biomasse initiale
biomass_init <- read.table("//Users//Loubnaem//Desktop//biomass_init.txt",header=TRUE,dec=",") %>%
  mutate(station=as.character(station))
df_brich_bi <- left_join(df_brich, biomass_init, by = "station")
df_btroph_bi <- left_join(df_btroph, biomass_init, by = "station")

graphe_richness_bi <- ggplot(df_brich_bi, aes(x=Pente_Biomass, y=Pente_Richness, color=log(biomass_init))) + geom_point()
graphe_richness_bi
graphe_richness_bi + geom_hline(yintercept=0) 

#YES!!!!!
#Dynamique temporelle niveau trophique moyen ~ Dynamique temporelle biomasse
plot(df_btroph_bi$Pente_Biomass,df_btroph_bi$Pente_Trophlvl)

graphe_trphlvl <- ggplot(df_btroph_bi, aes(x=Pente_Biomass, y=Pente_Trophlvl, color=p.value_pente_Trophlvl)) + geom_point()
graphe_trphlvl
graphe_trphlvl + geom_hline(yintercept=0) 

graphe_troph_bi <- ggplot(df_btroph_bi, aes(x=Pente_Biomass, y=Pente_Trophlvl, color=log(biomass_init))) + geom_point()
graphe_troph_bi
graphe_troph_bi + geom_hline(yintercept=0) 

