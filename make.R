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

loadd(pred_plot_signif_log_rich)
loadd(pred_plot_signif)
pred_plot_signif[4, ]$pred
pred_plot_signif[3, ]$predict_term
pred_plot_signif[4, ]$predict_term
pred_plot_signif[4, ]$myplot

debugonce(plot_final_model)
plot_final_model(
  rawdata = pred_plot_signif[4, ]$data[[1]],
  ggpred = pred_plot_signif[4, ]$pred[[1]]
)
