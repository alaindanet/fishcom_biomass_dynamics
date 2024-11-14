get_fw_metrics <- function(x) {

  ff_metaweb <- get_fish_fish_matrix(x)

  if (ncol(ff_metaweb) == 0) {
    l_in_ff <- NA
    l_out_ff <- NA
    ff_ind <- NA
    ct_ff <- NA
    tl_ff <- NA
    avg_tl_ff <- NA
    max_tl_ff <- NA
  } else {
    l_in_ff <- colSums(ff_metaweb)
    l_out_ff <- rowSums(ff_metaweb)
    ff_ind <- NetIndices::GenInd(ff_metaweb)
    ct_ff <- ff_ind["C"]
    tl_ff <- NetIndices::TrophInd(ff_metaweb)$TL
    avg_tl_ff <- mean(tl_ff)
    max_tl_ff <- max(tl_ff)
  }

  ind <- NetIndices::GenInd(x)
  ct <- ind["C"]
  tl <- NetIndices::TrophInd(x)$TL
  avg_tl <- mean(tl)
  max_tl <- max(tl)

  list(
    ff_metaweb = ff_metaweb,
    ind = ind,
    ff_ind = ff_ind,
    l_in_ff = l_in_ff,
    l_out_ff = l_out_ff,
    ct = ct,
    ct_ff = ct_ff,
    tl = tl,
    tl_ff = tl_ff,
    avg_tl = avg_tl,
    avg_tl_ff = avg_tl_ff,
    max_tl = max_tl,
    max_tl_ff = max_tl_ff
  )
}

#' Compute FW metrics from adjacency metrics (for extinction sequence)
#'
#' @return Food web metrics computed with all the food web or the fish species
#' only
#'
get_fw_metrics2 <- function(x, keep_metaweb = TRUE) {

  ## ff_metaweb
  ff_metaweb <- get_fish_fish_matrix(x)

  if (ncol(ff_metaweb) == 0) {
    l_in_ff <- NA
    l_out_ff <- NA
    ff_ind <- NA
    ct_ff <- NA
    tl_ff <- NA
    avg_tl_ff <- NA
    max_tl_ff <- NA
    prop_redundant_links_ff <- NA
  } else {
    l_in_ff <- colSums(ff_metaweb)
    l_out_ff <- rowSums(ff_metaweb)
    ff_ind <- NA
    ct_ff <- sum(ff_metaweb) / (ncol(ff_metaweb) - 1)^2
    tl_ff <- NetIndices::TrophInd(ff_metaweb)$TL
    avg_tl_ff <- mean(tl_ff)
    max_tl_ff <- max(tl_ff)
    prop_redundant_links_ff <- get_redundancy(ff_metaweb)[["prop_redundant_links"]]
  }

  if (ncol(x) == 0) {
    l_in <- NA
    l_out <- NA
    ind <- NA
    ct <- NA
    tl <- NA
    avg_tl <- NA
    max_tl <- NA
    prop_redundant_links <- NA
  } else {
    l_in <- colSums(x)
    l_out <- rowSums(x)
    ind <- NA
    ct <- sum(x) / (ncol(x) - 1)^2
    tl <- NetIndices::TrophInd(x)$TL
    avg_tl <- mean(tl)
    max_tl <- max(tl)
    prop_redundant_links <- get_redundancy(x)[["prop_redundant_links"]]
  }

  if (!keep_metaweb) {
    ff_metaweb <- NA
  }

  list(
    ff_metaweb = ff_metaweb,
    ind = ind,
    ff_ind = ff_ind,
    l_in = l_in,
    l_in_ff = l_in_ff,
    l_out = l_out,
    l_out_ff = l_out_ff,
    ct = ct,
    ct_ff = ct_ff,
    tl = tl,
    tl_ff = tl_ff,
    avg_tl = avg_tl,
    avg_tl_ff = avg_tl_ff,
    max_tl = max_tl,
    max_tl_ff = max_tl_ff,
    prop_redundant_links = prop_redundant_links,
    prop_redundant_links_ff = prop_redundant_links_ff
  )


}

#' Simulation extinctions
#'
#' @example
#' tu <- matrix(
#'   c(0, 1, 0, 0,
#'     0, 0, 1, 0,
#'     0, 0, 0, 1,
#'     0, 0, 0, 0
#'   ), byrow = TRUE, nrow = 4,
#'   dimnames = list(
#'     c("plant", "HERBIVORE_1", "CARNIVORE_1", "DINO_1"),
#'     c("plant", "HERBIVORE_1", "CARNIVORE_1", "DINO_1")
#'   )
#' )
#' # Extinction cascade, only the plant survived
#' ti <- get_sim_net(net = tu, ext_seq = c("HERBIVORE_1" = 1),
#'       keep_metaweb = TRUE
#' )
#' ncol(ti$metaweb[[1]]) == 1
#'
#' # Extinction cascade, only the plant and the herbivore survived
#' ti <- get_sim_net(net = tu, ext_seq = c("CARNIVORE_1" = 1),
#'       keep_metaweb = TRUE
#' )
#' ncol(ti$metaweb[[1]]) == 2
#' ti$metrics
#'
#' # No Extinction cascade, only the dino died
#' ti <- get_sim_net(net = tu, ext_seq = c("DINO_1" = 1),
#'       keep_metaweb = TRUE
#' )
#' ncol(ti$metaweb[[1]]) == 3
#' # Extinction cascade, everybody dies
#' ti <- get_sim_net(net = tu, ext_seq = c("plant" = 1),
#'       keep_metaweb = TRUE
#' )
#' ncol(ti$metaweb[[1]]) == 0
get_sim_net <- function(net = NULL, ext_seq = NULL, keep_metaweb = TRUE) {

  stopifnot(!is.null(names(ext_seq)))
  node_extinction <- tibble(
    node_rm_single = names(ext_seq),
    node_tlvl = ext_seq,
    node_rm_tot = map(seq_along(ext_seq), ~ext_seq[1:.x]),
    metaweb = furrr::future_map(
      seq_along(ext_seq),
      function(x) {
        tmp <- net[
          !rownames(net) %in% names(ext_seq)[1:x],
          !colnames(net) %in% names(ext_seq)[1:x]
          ]
        # Keep only the trophic species that have feeding links
        # and keep also the resources
        mask_link <- colSums(tmp) != 0 | !fish(colnames(tmp))
        to_remove <- any(!mask_link)
        while (to_remove) {
          tmp <- tmp[mask_link, mask_link, drop = FALSE]
          mask_link <- colSums(tmp) != 0 | !fish(colnames(tmp))
          to_remove <- any(!mask_link)
        }
        return(tmp)
      }
    )
  )

  meta_keep <- keep_metaweb
  out <- node_extinction %>%
    mutate(
      metrics = furrr::future_map(
        metaweb, ~try(get_fw_metrics2(.x, keep_metaweb = meta_keep))
      )
    )

  if (!keep_metaweb) {
    out$metaweb <- NULL
  }

  return(out)
}

#' Extinct a species
#'
#' @example
#' tu <- matrix(
#'   c(0, 1, 0, 0,
#'     0, 0, 1, 0,
#'     0, 0, 0, 1,
#'     0, 0, 0, 0
#'     ), byrow = TRUE, nrow = 4,
#'   dimnames = list(
#'     c("plant", "HERBIVORE_1", "CARNIVORE_1", "DINO_1"),
#'     c("plant", "HERBIVORE_1", "CARNIVORE_1", "DINO_1")
#'   )
#' )
#' # Extinction cascade, only the plant survived
#' ti <- make_extinction(tu, "HERBIVORE_1")
#' ncol(ti) == 1
#'
#' # Extinction cascade, only two species  survived
#' ti <- make_extinction(tu, "CARNIVORE_1")
#' ncol(ti) == 2
make_extinction <- function(network = NULL, species_to_remove = NULL) {
  stopifnot(!any(map_lgl(list(network, species_to_remove), is.null)))
  tmp <- network[
    !rownames(network) %in% species_to_remove,
    !colnames(network) %in% species_to_remove,
    drop = FALSE
    ]
  # Keep only the trophic species that have feeding links
  # and keep also the resources
  mask_link <- colSums(tmp) != 0 | !fish(colnames(tmp))
  to_remove <- any(!mask_link)
  while (to_remove) {
    tmp <- tmp[mask_link, mask_link, drop = FALSE]
    mask_link <- colSums(tmp) != 0 | !fish(colnames(tmp))
    to_remove <- any(!mask_link)
  }
  return(tmp)
}

#' Simulate an extinction sequence
#'
#' @example
#' tu <- matrix(
#'   c(0, 1, 0, 0,
#'     0, 0, 1, 0,
#'     0, 0, 0, 1,
#'     0, 0, 0, 0
#'     ), byrow = TRUE, nrow = 4,
#'   dimnames = list(
#'     c("plant", "HERBIVORE_1", "CARNIVORE_1", "DINO_1"),
#'     c("plant", "HERBIVORE_1", "CARNIVORE_1", "DINO_1")
#'   )
#' )
#' extinction_simulation(tu, species_names = c("CARNIVORE_1"))
#' ti <- extinction_simulation(tu, random = TRUE, seed = 123, species_names = c("DINO_1", "CARNIVORE_1", "HERBIVORE_1"))
#' bind_rows(ti)
#' ti <- extinction_simulation(tu, random = TRUE, seed = 1, species_names = c("DINO_1", "CARNIVORE_1", "HERBIVORE_1"))
#' bind_rows(ti)
extinction_simulation <- function(
  metaweb = NULL,
  species_names = NULL,
  random = FALSE,
  seed = 123
  ) {
  set.seed(seed)
  nb_extinction <- vector(mode = "integer", length = length(species_names))
  metrics_list <- vector(mode = "list", length = length(species_names))
  species <- species_names
  i <- 1
  # Sanitize species_list
  mask_species_in_metaweb <- map_lgl(species,
    ~any(str_detect(colnames(metaweb), .x)))
  if (any(!mask_species_in_metaweb)) {
    message("species ", cat(species[!mask_species_in_metaweb], sep = ","), " not in metaweb. Been removed.")
    species <- species[mask_species_in_metaweb]
  }
  while (length(species) > 0) {
    # Take a species to remove among remaining species
    ## Match species names or trophic species to metaweb names
    if (random) {
      species <- sample(species, replace = FALSE)
    }
    mask_metaweb_species <- str_detect(colnames(metaweb), species[1])
    species_to_remove <- colnames(metaweb)[mask_metaweb_species]
    # Remove it & Remove disconnected
    tmp_metaweb <- make_extinction(metaweb, species_to_remove)
    # Compute network metrics
    metrics_list[[i]] <- get_fw_metrics2(tmp_metaweb, keep_metaweb = FALSE)
    ## Remove unnecessary matrices and metrics
    long_met <- map_lgl(metrics_list[[i]], ~length(.x) > 1)
    metrics_list[[i]] <- metrics_list[[i]][!long_met]
    nb_node_extinction <- ncol(metaweb) - ncol(tmp_metaweb)
    get_nb_species <- function(x) length(unique(get_species(x)[, 1] %>% .[str_detect(., "[A-Z]{3}")]))
    nb_species_extinction  <- get_nb_species(colnames(metaweb)) - get_nb_species(colnames(tmp_metaweb))
    metrics_list[[i]] <- append(metrics_list[[i]], c(
        nb_node_extinction = nb_node_extinction,
        nb_species_extinction = nb_species_extinction,
        iteration = i))
    # Compute nb extinction
    # Update metaweb and species list
    metaweb <- tmp_metaweb
    species <- species[
      map_lgl(species, ~any(str_detect(colnames(metaweb), .x)))
      ]
    i <- i + 1
  }
  return(metrics_list)
}
