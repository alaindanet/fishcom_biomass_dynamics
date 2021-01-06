# This minimal drake workflow demonstrates the file structure from
# https://books.ropensci.org/drake/projects.html#code-files.
# It is not the only way to organize R scripts for drake,
# but this pattern is helpful.

source("R/packages.R")  # Load your packages, e.g. library(drake).
source("R/misc.R") # Define your custom code as a bunch of functions.
source("R/variable_shortcut.R") # Define your custom code as a bunch of functions.
source_dir("R") # Define your custom code as a bunch of functions.
source("R/plan.R")      # Create your drake plan.
# Call make() to run your work.
# Your targets will be stored in a hidden .drake/ cache,
#drake::r_make(plan)
drake::make(plan, keep_going = TRUE)
drake::make(plan)
drake::vis_drake_graph(plan)

#make(plan, parallelism = "future", jobs = 3)

# If you do not change any code or data,
# subsequent make()'s do not build targets.

# Load your targets back into your session with loadd() and readd().
plan
print(plan, n = 40)
loadd(my_bm_net_group)
rep(list(c(2,6)),8)
loadd(log_rich_net_group_f3y, log_bm_net_group_f3y)
loadd(model_log_rich_f3y)

debugonce(get_model_plot_from_signif_term)
make(plan)

debugonce(from_term_to_predict_term)

term_model$indiv_term
term_model$predict_term

filter(model_test, variable == "A")
filter(term_model, variable == "A")

loadd(pred_plot_signif_log_rich)
loadd(pred_plot_signif)
pred_plot_signif$pred[[3]]
pred_plot_signif$data[[3]]
pred_plot_signif$predict_term[[3]]
pred_plot_signif_log_rich$predict_term[[3]]
plot_final_model(
  ggpred = pred_plot_signif_log_rich$pred[[3]],
  rawdata = pred_plot_signif_log_rich$data[[3]],
  facet = FALSE,
  std_error = TRUE)
pred_plot_signif_log_rich$myplot[[1]]
pred_plot_signif_log_rich$pred[[7]]
select(pred_plot_signif_log_rich, variable, model_formula, mod)
