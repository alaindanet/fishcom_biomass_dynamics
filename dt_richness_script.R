# Importation des packages
mypath <- rprojroot::find_package_root_file
library (tidyverse)
library (dplyr)
library (magrittr)
#Import des fichiers
load("~/Desktop/fishcom_biomass_dynamics/richness_cba.RData")
load(paste0(mypath(), "/richness_cba.RData"))

add_nb_year_station <- function (.data = NULL) {

  output <- .data %>%
    group_by(station) %>%
    mutate(nb_year = year - min(year)) %>%
    ungroup()
  return(output)
} 
add_nb_year_station(.data = richness_cba)

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

#' Just compute a linear model with data.frame and a formula 
compute_linear_model <- function (.data = NULL, formulas = NULL) {

  model <- lm(
    formula = as.formula(formulas),
    data = .data
  )
  return(model)
} 

get_lm_coeff <- function(.data = NULL, formulas = NULL) {


  data_for_model <- .data %>%
    group_by(station) %>%
    nest()

  # Run the model
  ## Here I used lapply to show you an another method than purrr::map
  ## But they do the same thing
  data_for_model$model <- lapply( 
    X = data_for_model$data, #
    FUN = compute_linear_model,
    formulas = formulas
    )

  # Get the coefficients
  ## Here I could use purrr::map or lapply but I show you with for loop 

  ## Prepare the output
  data_for_model$coeff <- vector(mode = "list", length = nrow(data_for_model))
  for (i in seq_along(data_for_model$coeff)) {
  
    data_for_model$coeff[[i]] <-
      broom::tidy(data_for_model$model[[i]])
  }

  return(data_for_model)

}
test <- 
  get_lm_coeff(
    .data = add_nb_year_station(.data = richness_cba),
    formulas = "richness ~ nb_year"
  )
unnest(test, coeff)

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

