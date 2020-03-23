# Importation des packages
library (tidyverse)
library (dplyr)
library (magrittr)
#Import des fichiers
load("~/Desktop/fishcom_biomass_dynamics/richness_cba.RData")

#Ajout du nb d'années
richness_cba_nb <- richness_cba %>% 
  group_by(station)%>%
  mutate(nb_year = year - min(year))

#Un modèle linéaire par station
bystation_richness <-richness_cba_nb%>%
  group_by(station)%>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(richness ~ nb_year, .x)),
    coeff = map(model, broom::tidy)
  )
#Tableau des coefficients richness
richness_unnest <-unnest(bystation_richness, coeff)

#Suppression des colonnes inutiles
richness_coeff <- richness_unnest[ , - c(2:3)]

#Suppression des parenthèses: https://stackoverflow.com/a/53622690
datar <- richness_coeff %>%
  mutate(term = str_replace_all(term, "[//(//)]", ""))
str(datar)
#Division en deux du data frame
datar_part1 <- dplyr::select(datar, station, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

#Supprimer les lignes intercept du data_part2
datar_part2 <- dplyr::select(datar, station, term, std.error, statistic, p.value)
datar_part3 <- datar_part2[seq(2, nrow(datar_part2), 2),] %>% 
  transmute(std.error_pente = std.error, statistic_pente = statistic, p.value_pente = p.value)

#Fusionner les deux jeux de données
dt_richness <- left_join(datar_part1, datar_part3, by = "station") %>%
  dplyr::filter(p.value_pente >= 0 & p.value_pente <= 0.05)
plot(dt_richness$Intercept,dt_richness$nb_year)
graphe_richness <-ggplot(dt_richness, aes(x=Intercept, y=nb_year, color=p.value_pente)) + geom_point()
graphe_richness

#Ajout de la biomasse initiale
biomass_init <- read.table("//Users//Loubnaem//Desktop//biomass_init.txt",header=TRUE,dec=",") %>%
  mutate(station=as.character(station))
dt_richness_biomass_init <- left_join(dt_richness, biomass_init, by = "station")

graphe_richness_biomass_init <-ggplot(dt_richness_biomass_init, aes(x=Intercept, y=nb_year, color=log(biomass_init))) + geom_point()
graphe_richness_biomass_init

