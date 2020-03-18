# Importation des packages
library (tidyverse)
library (dplyr)
library (magrittr)
#Import des fichiers
# Ajout pour Alain:
if (Sys.info()["nodename"] == "Andy") {
  mypath <- rprojroot::find_package_root_file
  source(mypath("R", "misc.R")) # pour myload and mysave 
  load(mypath("biomass_cba.RData"))

} else {
  load("~/Desktop/fishcom_biomass_dynamics/biomass_cba.RData")
}

#Ajout du log(biomass) et du nb d'années
logbiomass_cba<-dplyr::mutate(biomass_cba,log_biomass=log10(biomass)) %>%
  group_by(station)%>%
  mutate(nb_year = year - min(year))

#Un modèle linéaire par station
bystation_biomass <-logbiomass_cba%>%
  group_by(station)%>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(log_biomass ~ nb_year, .x)),
    coeff = map(model, broom::tidy)
  )
#Tableau des coefficients
biomass_coeff <- bystation_biomass %>%
  select(station, coeff) %>%
  unnest(coeff)
str(biomass_coeff)
##OK
#Suppression des parenthèses: https://stackoverflow.com/a/53622690
data <- biomass_coeff %>%
  mutate(term = str_replace_all(term, "[//(//)]", ""))
str(data)
#Division en deux du data frame
data_part1 <- dplyr::select(data, station, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)
str(data_part1)

#Supprimer les lignes intercept du data_part2
data_part2 <- dplyr::select(data, station, term, std.error, statistic, p.value)
data_part3 <- data_part2[seq(2, nrow(data_part2), 2),] %>% 
  transmute(std.error_pente = std.error, statistic_pente = statistic, p.value_pente = p.value)

#Fusionner les deux jeux de données
dt_biomass <- left_join(data_part1, data_part3, by = "station")
#ungroup
plot(dt_biomass$Intercept,dt_biomass$nb_year)
graphe_biomass <-ggplot(dt_biomass, aes(x=Intercept, y=nb_year, color=p.value_pente)) + geom_point()
graphe_biomass

#Ajout de la biomasse initiale
biomass_init <- read.table("//Users//Loubnaem//Desktop//biomass_init.txt",header=TRUE,dec=",") %>%
  mutate(station=as.character(station))
dt_biomass_init <- left_join(dt_biomass, biomass_init, by = "station")

graphe_biomass_init <-ggplot(dt_biomass_init, aes(x=Intercept, y=nb_year, color=log(biomass_init))) + geom_point()
graphe_biomass_init

