## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----load_data, eval=FALSE----------------------------------------------------
# library(manureshed)
# 
# # Load built-in data for HUC8 watersheds
# nugis_data <- load_builtin_nugis("huc8", 2016)
# boundaries <- load_builtin_boundaries("huc8")

## ----nitrogen_sensitivity, eval=FALSE-----------------------------------------
# # Standard analysis (50% efficiency)
# results_standard <- agri_classify_complete_custom(
#   nugis_data,
#   scale = "huc8",
#   n_efficiency = 0.5,
#   p_efficiency = 1.0
# )
# 
# # Improved management scenario (70% efficiency)
# results_high <- agri_classify_complete_custom(
#   nugis_data,
#   scale = "huc8",
#   n_efficiency = 0.7,
#   p_efficiency = 1.0
# )
# 
# # Conservative scenario (30% efficiency)
# results_low <- agri_classify_complete_custom(
#   nugis_data,
#   scale = "huc8",
#   n_efficiency = 0.3,
#   p_efficiency = 1.0
# )
# 
# # Compare nitrogen classifications
# message("Standard (50% efficiency):\n")
# print(table(results_standard$N_class))
# 
# message("\nImproved Management (70% efficiency):\n")
# print(table(results_high$N_class))
# 
# message("\nConservative (30% efficiency):\n")
# print(table(results_low$N_class))

## ----visualize_sensitivity, eval=FALSE----------------------------------------
# library(ggplot2)
# library(dplyr)
# 
# # Create comparison data frame
# efficiency_scenarios <- data.frame(
#   Efficiency = c(0.3, 0.5, 0.7),
#   Scenario = c("Conservative", "Standard", "Improved"),
#   Source = c(
#     sum(results_low$N_class == "Source", na.rm = TRUE),
#     sum(results_standard$N_class == "Source", na.rm = TRUE),
#     sum(results_high$N_class == "Source", na.rm = TRUE)
#   ),
#   Sink_Deficit = c(
#     sum(results_low$N_class == "Sink_Deficit", na.rm = TRUE),
#     sum(results_standard$N_class == "Sink_Deficit", na.rm = TRUE),
#     sum(results_high$N_class == "Sink_Deficit", na.rm = TRUE)
#   )
# )
# 
# # Plot source watersheds across scenarios
# ggplot(efficiency_scenarios, aes(x = Efficiency, y = Source)) +
#   geom_line(size = 1.2, color = "#2166ac") +
#   geom_point(size = 3, color = "#2166ac") +
#   labs(
#     title = "Impact of Nitrogen Efficiency on Source Watersheds",
#     x = "Nitrogen Efficiency Factor",
#     y = "Number of Source Watersheds"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 14, face = "bold")
#   )

## ----phosphorus_sensitivity, eval=FALSE---------------------------------------
# # Standard phosphorus analysis
# results_p_standard <- agri_classify_complete_custom(
#   nugis_data,
#   scale = "huc8",
#   n_efficiency = 0.5,
#   p_efficiency = 1.0
# )
# 
# # Reduced phosphorus availability scenario
# results_p_reduced <- agri_classify_complete_custom(
#   nugis_data,
#   scale = "huc8",
#   n_efficiency = 0.5,
#   p_efficiency = 0.8
# )
# 
# # Compare phosphorus classifications
# message("Standard (100% P efficiency):\n")
# print(table(results_p_standard$P_class))
# 
# message("\nReduced Availability (80% P efficiency):\n")
# print(table(results_p_reduced$P_class))

## ----combined_sensitivity, eval=FALSE-----------------------------------------
# # Create multiple scenarios
# scenarios <- list(
#   baseline = list(n = 0.5, p = 1.0),
#   improved_n = list(n = 0.7, p = 1.0),
#   reduced_p = list(n = 0.5, p = 0.8),
#   both_modified = list(n = 0.6, p = 0.9)
# )
# 
# # Run all scenarios
# results_list <- lapply(names(scenarios), function(scenario_name) {
#   scenario <- scenarios[[scenario_name]]
# 
#   results <- agri_classify_complete_custom(
#     nugis_data,
#     scale = "huc8",
#     n_efficiency = scenario$n,
#     p_efficiency = scenario$p
#   )
# 
#   # Return summary statistics
#   data.frame(
#     Scenario = scenario_name,
#     N_Efficiency = scenario$n,
#     P_Efficiency = scenario$p,
#     N_Source = sum(results$N_class == "Source", na.rm = TRUE),
#     P_Source = sum(results$P_class == "Source", na.rm = TRUE),
#     N_Sink_Deficit = sum(results$N_class == "Sink_Deficit", na.rm = TRUE),
#     P_Sink_Deficit = sum(results$P_class == "Sink_Deficit", na.rm = TRUE)
#   )
# })
# 
# # Combine results
# sensitivity_summary <- do.call(rbind, results_list)
# print(sensitivity_summary)

## ----map_comparison, eval=FALSE-----------------------------------------------
# # Join with spatial boundaries
# spatial_standard <- boundaries %>%
#   left_join(results_standard, by = c("huc8" = "ID"))
# 
# spatial_high <- boundaries %>%
#   left_join(results_high, by = c("huc8" = "ID"))
# 
# # Create side-by-side maps
# map_standard <- map_agricultural_classification(
#   spatial_standard,
#   "nitrogen",
#   "N_class",
#   "Standard Efficiency (50%)"
# )
# 
# map_high <- map_agricultural_classification(
#   spatial_high,
#   "nitrogen",
#   "N_class",
#   "Improved Efficiency (70%)"
# )
# 
# # Display maps (requires cowplot or similar)
# # cowplot::plot_grid(map_standard, map_high, ncol = 2)

## ----complete_workflow, eval=FALSE--------------------------------------------
# # Define efficiency range for nitrogen
# n_efficiency_values <- seq(0.3, 0.7, by = 0.1)
# 
# # Run sensitivity analysis
# sensitivity_results <- lapply(n_efficiency_values, function(eff) {
#   results <- agri_classify_complete_custom(
#     nugis_data,
#     scale = "huc8",
#     n_efficiency = eff,
#     p_efficiency = 1.0
#   )
# 
#   data.frame(
#     N_Efficiency = eff,
#     N_Source = sum(results$N_class == "Source", na.rm = TRUE),
#     N_Sink_Deficit = sum(results$N_class == "Sink_Deficit", na.rm = TRUE),
#     N_Within_Watershed = sum(results$N_class == "Within_Watershed", na.rm = TRUE)
#   )
# })
# 
# # Combine and visualize
# sensitivity_df <- do.call(rbind, sensitivity_results)
# 
# # Create sensitivity plot
# library(tidyr)
# sensitivity_long <- sensitivity_df %>%
#   pivot_longer(
#     cols = c(N_Source, N_Sink_Deficit, N_Within_Watershed),
#     names_to = "Classification",
#     values_to = "Count"
#   ) %>%
#   mutate(
#     Classification = gsub("N_", "", Classification),
#     Classification = gsub("_", " ", Classification)
#   )
# 
# ggplot(sensitivity_long, aes(x = N_Efficiency, y = Count, color = Classification)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 3) +
#   labs(
#     title = "Nitrogen Classification Sensitivity to Efficiency Factor",
#     x = "Nitrogen Efficiency Factor",
#     y = "Number of Watersheds",
#     color = "Classification"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 14, face = "bold"),
#     legend.position = "bottom"
#   )

