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

plot_final_model <- function (ggpred = NULL, rawdata = NULL, facet = FALSE, std_error = FALSE) {

  ggpred %<>% as_tibble

  if ("facet" %in% names(ggpred)) {
  ggpred %<>% rename(increasing = group, group = facet)
  ggpred %<>%
    filter(increasing == FALSE & x < 0 | increasing == TRUE & x > 0)
  } else if (is.factor(ggpred$group) & !any(c("TRUE", "FALSE") %in% levels(ggpred$group))) {

  } else {
    ggpred %<>% rename(increasing = group)
    ggpred %<>%
      filter(increasing == FALSE & x < 0 | increasing == TRUE & x > 0)
  }

  if (is.null(rawdata)) {
    rawdata <- attr(ggpred, "rawdata")
  }

  if ("bm_slope" %in% colnames(rawdata)) {
    x_var <- "bm_slope"
    x_var_error <- "bm_slope_strd_error"
  } else if ("rich_slope" %in% colnames(rawdata)) {
    x_var <- "rich_slope" 
    x_var_error <- "rich_slope_strd_error"
  } else {
    stop("X var cannot be defined")
  }
  
  p <- rawdata %>%
    ggplot(aes_string(x = x_var, y = "linear_slope", color = "log(bm)")) +
    viridis::scale_color_viridis() +
    geom_point() 

  renaming_vector <- c( "x", "predicted")
  names(renaming_vector) <- c(x_var, "linear_slope")

  if ("group" %in% colnames(ggpred)) {
    renaming_vector <- c(renaming_vector, bm_f = "group")
    p <- p +
      geom_line(
	data = ggpred %>%
	  rename(!!!renaming_vector),
	aes_string(y = "linear_slope", x = x_var, linetype = "bm_f"),
	inherit.aes = FALSE)
  } else {
    p <- p +
      geom_line(
	data = ggpred %>% rename(!!!renaming_vector),
	aes_string(y = "linear_slope", x = x_var), inherit.aes = FALSE)
  }

  if (std_error) {
    p <- p +
      geom_errorbar(data = rawdata, 
	aes_string(
	  xmin = paste0(x_var, " - ", x_var_error),
	  xmax = paste0(x_var, " + ", x_var_error)),
	alpha = .3
	) +
      geom_errorbar(data = rawdata, 
	aes(
	  ymin = linear_slope - linear_slope_strd_error,
	  ymax = linear_slope + linear_slope_strd_error),
	alpha = .3)
  }

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
    viridis::scale_color_viridis() +
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


model_single_term <- function () {
  c("rich_slope", "bm_slope", "inc_f", "bm")
}
get_mod_single_term <- function (model_term = NULL) {
  single_term <- model_single_term()

 out <- str_extract_all( string = model_term,
    pattern = paste0(single_term,collapse = "|"),
    simplify = TRUE) %>% as.vector(.) %>% unique()

 out[out %in% single_term]
}

from_term_to_predict_term <- function (term = NULL) {

  # Needed for predict term to be in order for ggpredict
  term <- term[order(match(term, model_single_term()))]

  if ("bm_slope" %in% term) {
    term[term == "bm_slope"] <- "bm_slope [-0.2,-0.00001,0.00001,.2]"
  }
  if ("rich_slope" %in% term) {
    term[term == "rich_slope"] <- "rich_slope [-0.1,-0.00001,0.00001,.2]"
  }
  if ("bm" %in% term) {
    term[term == "bm"] <- "bm [quart2]"
  }
  term
}
