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
  rawdata %<>%
    rename(group = bm)

  ggpred %<>%
    filter(increasing == FALSE & x < 0 | increasing == TRUE & x > 0)

  if (is.null(rawdata)) {
  rawdata <- attr(ggpred, "rawdata")
  }


  if (!facet) {
  p <- ggplot(ggpred, aes(x = x , y = predicted, color = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha =
      .1) +
    geom_line()
  p + geom_point(
    data = rawdata, aes(y = bm_slope, x = linear_slope, color = group))
  } else {
    p <- ggplot(ggpred, aes(x = x , y = predicted)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha =
	.1) +
      geom_line() 
    p + geom_point(data = rawdata, aes(y = bm_slope, x = linear_slope)) +
      facet_grid(cols = vars(group), scales = "free_x")
  
  }

}
