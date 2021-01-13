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

loadd(rigal_classification)
loadd(full_data2)
test <- get_station_com_summary(
  .data = full_data2,
  myvar = c("bm_std", "log_bm_std", "rich_std", "log_rich_std"),
  var_cat = "com", group = FALSE, summary_type = "median")
test <- get_station_com_summary(
  .data = full_data2,
  myvar = c("bm_std", "log_bm_std", "rich_std", "log_rich_std"),
  var_cat = "com", group = FALSE, summary_type = "first_3_year")

loadd(rich_vs_net_trends, stream_group, biomass_group, bm_vs_net_trends, bm_std_st_decrease_increase, rich_std_st_decrease_increase)
test2 <- add_stream_bm_caract_to_model(
  bm_vs_network_df = bm_vs_net_trends,
  stream_caract = NULL,
  bm_caract = filter(biomass_group, bm_var == "bm_std",
    group_type == "median", summary_type == "first_3_year"),
  bm_group_var = "bm_std", group_type_caract = "median", 
  bm_summary_type = "first_3_year") %>% 
filter(station %in% bm_std_st_decrease_increase$station)

add_stream_bm_caract_to_model(bm_vs_network_df = rich_vs_net_trends,
    stream_caract = stream_group, bm_caract = biomass_group,
    bm_group_var = "bm_std", group_type_caract = "median", bm_summary_type
= "first_3_year") %>%
    filter(station %in% rich_std_st_decrease_increase$station)


filter(biomass_group, bm_var == "bm_std", group_type == "median", summary_type ==
  "first_3_year")



loadd(summary_var_f3y, rich_vs_net_trends, bm_net_group_f3y)
left_join(rich_vs_net_trends, summary_var_f3y, by = "station")

test <- get_y_versus_x_trends(classif = rigal_classification,
  x_var = "log_rich_std") %>%
left_join(., summary_var_f3y, by = "station")

mask <- str_detect(colnames(test), "slope") &
  !colnames(test) %in% c("linear_slope", "linear_slope_strd_error")
colnames(test)[mask]

compute_my_lm_vs_net_model(
      .df = filter(bm_net_group_f3y, !is.na(protocol_type)),
      var_to_group = "variable",
      x = ifelse("bm_slope" %in% colnames(bm_net_group_f3y), "bm_slope", "rich_slope")
      )


