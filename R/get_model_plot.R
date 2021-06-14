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

get_lm_single_term <- function (model_term = NULL) {
  sep_string <- str_split(model_term, pattern = "[:|+]") %>% unlist
  str_remove_all(string = sep_string, pattern = "\\s*") %>% unique
}

from_term_to_predict_term <- function (term = NULL) {

  # Needed for predict term to be in order for ggpredict
  #term <- term[order(match(term, model_single_term()))]
  ## first x (i.e. slope), followed by bm and inc_f 
  term <- c(
    term[str_detect(term, "_slope")],
    term[str_detect(term, "inc_f")],
    term[!str_detect(term, "_slope") & !str_detect(term, "inc_f")]
  )
  term %<>% na.omit
  return(term)

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

get_ggpredict_term_from_anova <- function (aov_tab = NULL, bound = NULL, pval_threshold = 0.05) {
  tmp <- aov_tab %>%
    mutate(signf = p.value <= pval_threshold) %>%
    filter(signf) %>%
    summarise(
      signif_term = paste(term, collapse = "+"),
      indiv_term = list(term)
      ) %>%
    mutate(
      single_var_term = map(signif_term, get_lm_single_term),
      predict_term = map(single_var_term, from_term_to_predict_term)
    )
    pred_term <- tmp$predict_term[[1]]
    #tmp$predict_term
    # Get x var to get bounds 
    term <- aov_tab$term
    x_var_slope <-  term[str_ends(term, "_slope")]
    x_var <- str_remove(x_var_slope, "_slope")
    min_max <- bound[[x_var]]

    ggpred_tmp <- paste0(x_var_slope, " [",min_max[1],", -0.000000001, 0.000000001, ", min_max[2], "]")

    # If no signicant effect, return x slope, may be to change
    if (str_length(pred_term) == 0) {
      return(ggpred_tmp)
    }

    if (any(str_detect(pred_term, "inc_f"))) {
     ggpred_tmp <- c(ggpred_tmp, "inc_f") 
    }

    if (any(!str_detect(pred_term, "inc_f|_slope"))) {
      tmp <- 
      ggpred_tmp <- c(ggpred_tmp,
	paste0(pred_term[!str_detect(pred_term, "inc_f|_slope")], " [quart2]")
      )
    }

    return(ggpred_tmp)

}

get_ggpredict_term_from_anova_new_model <- function (
  aov_tab = NULL,
  bound = NULL,
  pval_threshold = 0.05) {

  tmp <- aov_tab %>%
    mutate(signf = p.value <= pval_threshold) %>%
    filter(signf) %>%
    summarise(
      signif_term = paste(term, collapse = "+"),
      indiv_term = list(term)
      ) %>%
    mutate(
      single_var_term = map(signif_term, get_lm_single_term),
      predict_term = map(single_var_term, from_term_to_predict_term)
    )
    pred_term <- tmp$predict_term[[1]]
    #tmp$predict_term
    # Get x var to get bounds
    term <- aov_tab$term
    x_var_slope <-  term[str_ends(term, "_slope")]
    x_var <- str_remove(x_var_slope, "_slope")
    min_max <- bound[[x_var]]

    ggpred_tmp <- paste0(x_var_slope, " [",min_max[1],", -0.000000001, 0.000000001, ", min_max[2], "]")

    # If no signicant effect, return x slope, may be to change
    if (str_length(pred_term) == 0) {
      return(ggpred_tmp)
    }

    if (any(str_detect(pred_term, "inc_f"))) {
     ggpred_tmp <- c(ggpred_tmp, "inc_f") 
    }

    if (any(!str_detect(pred_term, "inc_f|_slope"))) {
      tmp <- 
      ggpred_tmp <- c(ggpred_tmp,
	paste0(pred_term[!str_detect(pred_term, "inc_f|_slope")], " [quart2]")
      )
    }

    return(ggpred_tmp)

}

compute_lm_from_signif_anova <- function (aov_tab = NULL , .df = NULL) {
  tmp <- aov_tab %>%
    mutate(signf = p.value <= 0.05) %>%
    filter(signf) %>%
    summarise(
      signif_term = paste(term, collapse = "+"), 
      indiv_term = list(term)
      ) %>%
    mutate(single_var_term = map(signif_term, get_lm_single_term))

  #return(tmp)
  

  term <- aov_tab$term
  x_var_slope <- term[str_ends(term, "_slope")]
  x_var_slope_sym <- sym(x_var_slope)

  .df %<>%
    mutate(
      increasing := {{x_var_slope_sym}} > 0,
      inc_f = as.factor(increasing),
      #"log_{{x_var_sym}}" := log({{x_var_sym}})
    ) 

  signif_term <- tmp$signif_term[[1]]

  # If no significant term, put slope
  if (str_length(signif_term) == 0 ) {
    signif_term <- paste0(x_var_slope)
  }
  # If no slope in significant terms, i.e. put it
  if (!str_detect(signif_term, "_slope")) {
    signif_term <- paste0(x_var_slope, "+", signif_term)
  }

  # Get main effet if the interaction is significant 
  single_terms <- tmp$indiv_term[[1]]
  if (any(str_detect(single_terms, ":"))) {
    # left term
    left_term_inter <- map_chr(
      single_terms[str_detect(single_terms, ":")],
	~str_split(.x, ":")[[1]][1]
      )
    
    signif_term <- paste0(left_term_inter, "+", signif_term, collapse = "+") 
  }
  #signif_term

  formulas <- paste0("linear_slope ~ ", signif_term)
  lm(formula = as.formula(formulas), data = .df, weights = reg_weight)

}

rename_pred_table <- function (pred_table = NULL, term = NULL) {
  
  # Case one: all the term are present
  if (length(term) == 1) {
    pred_table %<>%
      select(-group)
  } else if (length(term) == 3) {
    pred_table %<>%
      rename(increasing = group, covar = facet) %>% 
      filter(increasing == FALSE & x < 0 | increasing == TRUE & x > 0)
  } else if (length(term) == 2) {
    if (any(str_detect(term, "inc_f"))) {
      pred_table %<>%
	rename(increasing = group) %>%
	filter(increasing == FALSE & x < 0 | increasing == TRUE & x > 0)
    } else {
      pred_table %<>%
	rename(covar = group)
    }
  }

  return(pred_table)

}

plot_raw_data  <- function(.df = NULL, covar = NULL, std_error = TRUE) {

  col_df <- colnames(.df)
  x_var <- col_df[str_detect(col_df, "_slope") & !str_detect(col_df, "_slope_strd_error") & !str_detect(col_df, "linear_slope")]
  x_var_error <- str_replace(x_var, "_slope","_strd_error")

  #return(c(x_var, covar, x_var_error))

  if (!is.null(covar)) {
    p <- .df %>%
      ggplot(aes_string(x = x_var, y = "linear_slope", color = covar)) +
      viridis::scale_color_viridis() +
      geom_point() 
  } else {
    p <- .df %>%
      ggplot(aes_string(x = x_var, y = "linear_slope")) +
      viridis::scale_color_viridis() +
      geom_point() 
  }

  if (std_error) {
    p <- p +
      geom_errorbar(data = .df, 
	aes_string(
	  xmin = paste0(x_var, " - ", x_var_error),
	  xmax = paste0(x_var, " + ", x_var_error)),
	alpha = .3
	) +
      geom_errorbar(data = .df, 
	aes(
	  ymin = linear_slope - linear_slope_strd_error,
	  ymax = linear_slope + linear_slope_strd_error),
	alpha = .3)
  }
  return(p)
}

plot_add_pred_data <- function (pred = NULL, gg = NULL, signif = FALSE) {

  signif <- ifelse(signif <= 0.05, TRUE, FALSE)

  if (is.null(gg)) {
    p <- pred %>%
      ggplot(aes(y = predicted, x = x))
  } else {
    p <- gg
  }

  if ("covar" %in% colnames(pred)) {
    p <- p +
      geom_line(
        data = pred,
        aes(y = predicted, x = x, linetype = covar),
        inherit.aes = FALSE,
        color = ifelse(signif, "red", "black")
      )
  } else {
    p <- p +
      geom_line(data = pred,
        aes(y = predicted, x = x),
        inherit.aes = FALSE,
        color = ifelse(signif, "red", "black")
      )
  }

  return(p)
}
