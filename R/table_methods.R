
get_anova_table_from_named_list <- function(x = NULL) {

  out <- map2(names(x), x,
    ~mutate(.y, y = .x) %>%
      rownames_to_column(var = "term") %>% 
      as_tibble() %>%
      select(y, term, everything())
  )
  do.call(rbind, out)

}

format_reg_table <- function(x = NULL) {

  x %>%
    select(y, term, estimate, std.error, statistic, p.value) %>%
    rename(
      Response = y, Term = term,
      Estimate = estimate,
      `Std error` = std.error,
      `Statistic` = statistic,
      `P-value` = p.value
    ) %>%
    mutate(
      Response = str_replace_all(Response, var_replacement()),
      Term = str_replace_all(Term, var_replacement())
    )

}

get_std_coef_from_reg_tab <- function(tab = NULL) {
  tab %>%
    select(response, term, estimate, std.error, std_estimate, stdse, statistic)
}

summary_distribution <-
  function(x = NULL, na.rm = FALSE) {

    quant <- quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = na.rm)
    names(quant) <- c("min", "1st_quart", "median", "2nd_quart", "max")

    other_desc <- c(
      mean = mean(x, na.rm = na.rm),
      sd = sd(x, na.rm = na.rm),
      n = length(x),
      n_na = length(x[is.na(x)]),
      frac_na = length(x[is.na(x)]) / length(x)
    )

    output <- c(quant, other_desc)
    return(output)
}
