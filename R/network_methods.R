
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
    nb_non_pisc_node = length(fish_fish) - nb_pisc_node,
    prop_pisc_node = nb_pisc_node / length(fish_fish),
    prop_non_pisc_node = 1 - prop_pisc_node,
    nb_pisc_rich =  str_extract(names(fish_fish[fish_fish > 0]), "[A-Z]+") %>%
      unique() %>% length(),
    nb_non_pisc_rich =  str_extract(names(fish_fish), "[A-Z]+") %>%
      unique() %>% length() - nb_pisc_rich,
    prop_pisc_rich = nb_pisc_rich / str_extract(names(fish_fish), "[A-Z]+") %>%
      unique() %>% length(),
    prop_non_pisc_rich = 1 - prop_pisc_rich
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

#' Get redundancy in food-webs
#'
#' Computes redundant and functional links.
#'
#' @examples
#' The example from Allesina et al. (2009) https://doi.org/10.1098/rstb.2008.0214
#' tu_names <- c("R", "Sediment_1", "POC", "Benthos", "Zooplankton")
#' tu <- matrix(
#'   c(
#'     0, 1, 1, 0, 0,
#'     0, 0, 0, 1, 0,
#'     0, 1, 0, 0, 1,
#'     0, 1, 0, 0, 0,
#'     0, 1, 0, 0, 0
#'   ), nrow = 5, byrow = TRUE,
#'   dimnames = list(tu_names, tu_names)
#' )
#' g <- graph_from_adjacency_matrix(tu, mode = "directed", diag = FALSE)
#' V(g)
#' dtree <- dominator_tree(g, root="R")
#' layout <- layout_as_tree(dtree$domtree, root="R")
#' layout[,2] <- -layout[,2]
#' plot(dtree$domtree, layout=layout, vertex.label=V(dtree$domtree)$name)
get_redundancy <- function(x = NULL) {
  # Get a root "R" to compute dominant tree
  root_names <- c("R","det", "biof", "phytopl")
  root_mat <- matrix(
    c(
      0, 1, 1, 1,
      0, 0, 0, 0,
      0, 0, 0, 0,
      0, 0, 0, 0
      ), nrow = 4, byrow = TRUE,
    dimnames = list(root_names, root_names)
  )
  root_graph <- graph_from_adjacency_matrix(root_mat, mode = "directed", diag = FALSE)

  # Check for starving fish nodes
  no_int <- colnames(x)[colSums(x) == 0]
  if  (any(!no_int %in% root_names)) {
    out <-  list(
      functional_links = NA,
      total_links = NA,
      redundant_links = NA,
      prop_redundant_links = NA,
      prop_functional_links = NA
    )
  } else {

    mygraph <- graph_from_adjacency_matrix(x, mode = "directed", diag = FALSE)
    rooted_graph <- mygraph + root_graph
    dtree <- dominator_tree(rooted_graph, root="R", mode = "out")
    functional_links <- length(E(dtree$domtree))
    total_links <- length(E(rooted_graph))
    redundant_links  <- total_links - functional_links

    out <- list(
      functional_links = functional_links,
      total_links = total_links,
      redundant_links = redundant_links,
      prop_redundant_links = redundant_links / total_links,
      prop_functional_links = functional_links / total_links
    )
  }
  out
}

get_nb_fish_node_top_consumers <- function(x = NULL) {
  ti <- get_fish_fish_matrix(net = x)
  eating <- colSums(ti) > 0
  being_eaten <-  rowSums(ti) > 0
  sum(eating & !being_eaten)
}

get_nb_species_top_consumers_sim <- function(x = NULL) {

  n_species <- sqrt(length(x))
  species_names <- paste0("s", 1:n_species)
  mat <- matrix(
    x,
    nrow = n_species,
    dimnames = list(species_names, species_names)
  )
  eating <- colSums(mat) > 0
  being_eaten <-  rowSums(mat) > 0
  sum(eating & !being_eaten)
}

get_redundancy_sim <- function(x = NULL) {

  n_species <- sqrt(length(x))
  species_names <- paste0("s", 1:n_species)
  mat <- matrix(
    x,
    nrow = n_species,
    dimnames = list(species_names, species_names)
  )
  mygraph <- graph_from_adjacency_matrix(mat)

  producer <- which(colSums(mat) == 0)
  resource_names <- c("R", species_names[producer])
  add_resource <- matrix(
    rep(0, (length(producer) + 1) * (length(producer) + 1)),
    nrow = length(producer) + 1,
    dimnames = list(resource_names, resource_names)
  )
  add_resource[rownames(add_resource) == "R", colnames(add_resource) != "R"] <- 1

  rooted_graph <- mygraph + graph_from_adjacency_matrix(add_resource)

  dtree <- dominator_tree(rooted_graph, root="R", mode = "out")
  functional_links <- length(E(dtree$domtree))
  total_links <- length(E(rooted_graph))
  redundant_links  <- total_links - functional_links
  list(
    functional_links = functional_links,
    total_links = total_links,
    redundant_links = redundant_links,
    prop_redundant_links = redundant_links / total_links,
    prop_functional_links = functional_links / total_links
  )

}
