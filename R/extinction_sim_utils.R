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

get_fw_metrics2 <- function(x, keep_metaweb = TRUE) {

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
    ff_ind <- NA
    ct_ff <- sum(ff_metaweb) / (ncol(ff_metaweb) - 1)^2
    tl_ff <- NetIndices::TrophInd(ff_metaweb)$TL
    avg_tl_ff <- mean(tl_ff)
    max_tl_ff <- max(tl_ff)
  }

  if (ncol(x) == 0) {
    l_in <- NA
    l_out <- NA
    ind <- NA
    ct <- NA
    tl <- NA
    avg_tl <- NA
    max_tl <- NA
  } else {
    l_in <- colSums(x)
    l_out <- rowSums(x)
    ind <- NA
    ct <- sum(x) / (ncol(x) - 1)^2
    tl <- NetIndices::TrophInd(x)$TL
    avg_tl <- mean(tl)
    max_tl <- max(tl)
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
    max_tl_ff = max_tl_ff
  )


}

get_sim_net <- function(net = NULL, ext_seq = NULL, keep_metaweb = TRUE) {

  node_extinction <- tibble(
    node_rm_single = names(ext_seq),
    node_tlvl = ext_seq,
    node_rm_tot = map(seq_along(ext_seq),
      ~ext_seq[1:.x]),
    metaweb = furrr::future_map(seq_along(ext_seq),
      function(x) {
      tmp <- net[
        !rownames(net) %in% names(ext_seq)[1:x],
        !colnames(net) %in% names(ext_seq)[1:x]
        ]

      mask_link <- colSums(tmp) != 0
      return(tmp[mask_link, mask_link])
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
