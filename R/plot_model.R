plot_obs_fitted_inla <- function(
  mod_inla = NULL,
  dataset = NULL,
  resp = NULL,
  pred_nrows = NULL,
  return_df = FALSE
  ) {

  fitted_values <- mod_inla$summary.fitted.values[["mean"]]

  if (!is.null(pred_nrows)) {
    row_selection <- length(fitted_values) - pred_nrows
    fitted_values <-
      fitted_values[1:row_selection]
  }

  obs_fit <- tibble(
    response = resp,
    station = dataset[["station"]],
    year = dataset[["year"]],
    fit = fitted_values,
    obs = dataset[[resp]]
  )

  if (return_df) {
    return(obs_fit)
  }

  obs_fit %>%
    ggplot(aes(x = fit, y = obs)) +
    geom_point(alpha = .5, size = 2) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    labs(x = "Fitted values", y = "Observed values")
}
