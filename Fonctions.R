# Importation des packages
mypath <- rprojroot::find_package_root_file
library (tidyverse)
library (dplyr)
library (magrittr)
#Import des fichiers
load("~/Desktop/fishcom_biomass_dynamics/richness_cba.RData")
load(paste0(mypath(), "/richness_cba.RData"))

#Ajout du nombre d'années
add_nb_year_station <- function (.data = NULL) {
  
  output <- .data %>%
    group_by(station) %>%
    mutate(nb_year = year - min(year)) %>%
    ungroup()
  return(output)
} 
add_nb_year_station(.data = richness_cba)

#Un modèle linéaire par station 

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

# Unnest = unnest (df, colonne)

#Suppression des colonnes inutiles
erase_useless_columns <- function (.data = NULL ) {
  output <- .data[ , - c(2:3)]
return(output)
}

#Suppression des parenthèses
erase_brackets <- function (.data = NULL) {
  output <- .data %>%
  mutate(term = str_replace_all(term, "[//(//)]", ""))
return(output)
}

#Division du data frame
df_part1 <- function (.data = NULL) {
  output <- .data %>%
    select(station, term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    mutate(Pente = nb_year) %>%
    select(station, Intercept, Pente)
return(output)
}

essai_bis <- df_part1(.data = essai)

df_part2 <- function (.data = NULL) {
  output <- .data %>%
    select(station, term, p.value) %>%
    pivot_wider(names_from = term, values_from = p.value) %>%
    mutate(p.value_pente = nb_year) %>%
    select(station, p.value_pente)
  return(output)
}

essai_ter <- df_part2(.data=essai)

#Fusion pour obtenir le df final avec : station, Intercept, pente, p.value pente
merge_df <- function (.data = NULL, data_to_join = NULL) {
  output <- .data %>%
    left_join(data_to_join, by = "station")
  return(output)
}
df <- merge_df(.data=df_part1, data_to_join = df_part2)
