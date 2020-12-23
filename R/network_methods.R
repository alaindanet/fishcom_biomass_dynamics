
#'
calc_weighted_connectance <- function (
  network = NULL,
  intermediate_results = FALSE) {

  if (!(class(network) %in% c("dgCMatrix", "matrix"))) {
    if (!intermediate_results) {
      return(NA)
    } else {
      return(c(ct = NA, A = NA, m = NA))
    }
  }

  if (class(network) != "matrix") {
    network <- as.matrix(network)
    network[network == 0] <- NA # To manage log
  }

  # Shannon
  total_fluxes <- sum(network, na.rm = TRUE)
  weighted_fluxes <- network / total_fluxes
  shannon <- - sum(weighted_fluxes * log(weighted_fluxes), na.rm = TRUE)
  #return(list(total_fluxes, weighted_fluxes, shannon))

  # Average mutual information
  Ti <- rowSums(network, na.rm = TRUE)
  Ti[Ti == 0] <- NA # To manage log
  Tj <- colSums(network, na.rm = TRUE)
  Tj[Tj == 0] <- NA # To manage log
  A  <- sum(weighted_fluxes * log( (network * total_fluxes) / (Ti * Tj) ), na.rm = TRUE)

  # Effective connectance per node
  m <- exp((shannon - A) / 2)

  connectance <- m / nrow(network)
  if(!intermediate_results) {
    return(connectance)
  } else {
    return(c(ct = connectance, A = A, m = m))
  }
}

get_network_metric_fish_only <- function (net = NULL) {
  out <- 
    net %>%
    select(opcod, network) %>%
    mutate(
      fish_fish_net = map(network, get_fish_fish_matrix),
      ff_metrics = map(fish_fish_net, NetIndices::GenInd),
      ct_ff = map_dbl(ff_metrics, "C"),
      l_ff =  map_dbl(ff_metrics, "Ltot"),
      ld_ff = map_dbl(ff_metrics, "LD"),
    ) %>%
    select(-network)

    return(out)
}

get_fish_fish_matrix <- function (net = NULL) {
  fish_fish <- net[fish(rownames(net)), fish(colnames(net))] 
  if (length(fish_fish) == 1) {
    fish_fish %<>% as.matrix() 
    colnames(fish_fish) <-
      row.names(fish_fish) <-
	colnames(net)[fish(colnames(net))]
  }
  return(fish_fish)
}

#' Get information about piscivorous composition in network
get_piscivory_stat_from_network <- function (net) {

  fish_fish <- get_fish_fish_matrix(net)

  fish_fish %<>%
    colSums()

  tibble(
    nb_pisc_node = length(fish_fish[fish_fish > 0]),
    prop_pisc_node = nb_pisc_node / length(fish_fish),
    nb_pisc_rich =  str_extract(names(fish_fish[fish_fish > 0]), "[A-Z]+") %>%
      unique() %>% length(),
    prop_pisc_rich = nb_pisc_rich / str_extract(names(fish_fish), "[A-Z]+") %>%
      unique() %>% length()
  )

}

#' Extract trophic species string by excluding resource
fish <- function(string) {
  resource <- str_extract(string, "[a-z]+") %>%
    na.omit()
  return(!string %in% resource)
} 

get_l_ld_IS <- function(net = NULL) {
  
  net_l_ld <-
    net %>%
    mutate(
      l =  map_dbl(metrics, "Ltot"),
      ld = map_dbl(metrics, "LD"),
      weigted_stat_tmp = map(weighted_fish_fish_net,
	~ calc_weighted_connectance(.x, intermediate_results = TRUE)),
      A = map_dbl(weigted_stat_tmp, "A"),
      m = map_dbl(weigted_stat_tmp, "m"),
      avg_IS = map_dbl(weighted_fish_fish_net, ~ as.matrix(.x) %>% mean),
      gini = map_dbl(weighted_fish_fish_net, ~ifelse(all(is.na(.x)), NA, compute_gini(.x)))
      ) %>%
  select(opcod, l, ld, A, m, gini, avg_IS)

  return(net_l_ld)

}


compute_gini <- function (mat) {
  #https://en.wikipedia.org/wiki/Gini_coefficient
  diag(mat) <- NA
  v <- mat %>% as.vector()
  v <- na.omit(v)

  N <- length(v)
  v <- sort(v)
  i <- 1:N
  gini <- 1 - (2 / (N - 1)) * (N - (sum(i * v) / sum(v)) )
  return(gini)
}
