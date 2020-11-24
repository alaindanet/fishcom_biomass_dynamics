#' Get temporal dynamics plot
#'
get_temporal_dynamics_plot <- function (temporal_dynamics = NULL) {

  temporal_dynamics %>%
    select(-data) %>%
    pivot_longer(-station, names_to = "variable", values_to = "model") %>%
    mutate(plot = map(model, plot_data_slope_lm)) %>%
    select(-model)


}
#'Get relationship plot 
#'
get_net_dyn_lm_plot <- function (net_dyn_lm = NULL) {

  net_dyn_lm %>%
    mutate(plot = map(model, plot_data_slope_lm)) %>%
    select(-model)
  
}

#' Plot data and slope from lm object
#'
plot_data_slope_lm <- function (fit) {

  prediction <- ggeffects::ggpredict(fit, terms = names(fit$model)[2])
  colnames(prediction) %<>% str_replace_all(., c("x" = names(fit$model)[2], "predicted" = names(fit$model)[1]))


  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    geom_line(data = prediction) +
    geom_ribbon(data = prediction, aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
    #geom_line(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 2),
	"Intercept =",signif(fit$coef[[1]],2),
	" Slope =",signif(fit$coef[[2]], 2),
	" P =",signif(summary(fit)$coef[2,4], 3)))
}

plot_final_model <- function (ggpred = NULL, rawdata = NULL, facet = FALSE) {
  ggpred %<>%
    as_tibble %>%
    rename(increasing = group, group = facet)

  ggpred %<>%
    filter(increasing == FALSE & x < 0 | increasing == TRUE & x > 0)

  if (is.null(rawdata)) {
  rawdata <- attr(ggpred, "rawdata")
  }

  p <- rawdata %>%
    ggplot(aes(x = bm_slope, y = linear_slope, color = log(bm))) +
    scale_color_viridis() +
    geom_point() +
    geom_line(
      data = ggpred %>%
	rename(bm_f = group, bm_slope = x, linear_slope = predicted),
      aes(y = linear_slope, x = bm_slope, linetype = bm_f),
      inherit.aes = FALSE)
    return(p)
  

  if (!facet) {
  p <- ggplot(ggpred, aes(x = x , y = predicted, linetype = group)) +
    #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha =
      #.1) +
    geom_line()
  #p
  p + geom_point(
    data = rawdata, aes(y = response, x = x))
  } else {
    p <- ggplot(ggpred, aes(x = x , y = predicted)) +
      #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha =
	#.1) +
      geom_line()
    p +
      #geom_point(data = rawdata, aes(y = bm_slope, x = linear_slope)) +
      facet_grid(cols = vars(group), scales = "free_x")
  
  }

}

plot_pred_data <- function(.data = NULL, pred = NULL, std_error = TRUE) {

  p <- .data %>%
    ggplot(aes(x = bm_slope, y = linear_slope, color = log(bm))) +
    scale_color_viridis() +
    geom_point() +
    geom_line(
      data = pred %>%
	rename(bm_f = group, bm_slope = x, linear_slope = predicted),
      aes(y = linear_slope, x = bm_slope, linetype = bm_f),
      inherit.aes = FALSE)

    if (std_error) {
    
      p <- p +
	geom_errorbar(data = .data, 
	  aes(xmin = bm_slope - bm_slope_strd_error,
	    xmax = bm_slope + bm_slope_strd_error),
	  alpha = .3
	  ) +
	geom_errorbar(data = .data, 
	  aes(ymin = linear_slope - linear_slope_strd_error,
	    ymax = linear_slope + linear_slope_strd_error), alpha = .3)

    
    }

  return(p)

}

labs_log_bm_net_trends <- function ( ) {
  labs(
    x = paste("Trend of log of biomass per surface (g.m-2.y-1)"),
    y = paste("Trend of Networks (U.y-1)")
    )
}

labs_bm_net_trends <- function ( ) {
  labs(
    x = paste("Trend of biomass per surface (g.m-2.y-1)"),
    y = paste("Trend of Networks (U.y-1)")
    )
}

xylabs <- function (...) {
  dots <- pryr::named_dots(...)
  dots <- map(dots, eval) %>% unlist

  lab_list <- list(
    del = expression(bold(paste("Fraction of global dispersal (", delta, ")"))),
    nbnode = paste("Number of nodes"),
    bm = paste("Biomass (g)"),
    bm_std = paste("Biomass per surface (g.m-2)"),
    bm_slope = paste("Trend of biomass per surface (g.m-2.y-1)"),
    linear_slope = paste("Trend of Networks (U.y-1)"),
    richness = paste("Species richness"),
    richness_cv = paste("CV of richness"),
    richness_avg = paste("Average richness"),
    richness_avg = paste("Median richness"),
    connectance = paste("Connectance"),
    connectance_avg = paste("Average connectance"),
    connectance_med = paste("Median connectance"),
    connectance_cv = paste("CV of connectance"),
    diversity = paste("Diversity")
    )
  
  lab_used <- lab_list[dots]
  names(lab_used) <- names(dots)

  labs(
    x = lab_used["x"][[1]],
    y = lab_used["y"][[1]]
    )
}


