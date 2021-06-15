
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
    select(response, term, estimate, std_estimate, std.error, statistic)
}
