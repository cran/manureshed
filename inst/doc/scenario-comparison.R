## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 10,
  fig.height = 6
)

## ----basic_comparison, eval=FALSE---------------------------------------------
# library(manureshed)
# 
# # Run analysis without WWTP
# base_scenario <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = FALSE
# )
# 
# # Run analysis with WWTP
# wwtp_scenario <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# # Compare the two scenarios
# comparison <- compare_scenarios(list(
#   "Agricultural Only" = base_scenario,
#   "Agricultural + WWTP" = wwtp_scenario
# ))

## ----view_data, eval=FALSE----------------------------------------------------
# # View the comparison data
# print(comparison$comparison_data)

## ----view_summary, eval=FALSE-------------------------------------------------
# # View summary
# print(comparison$summary)
# 
# # See specific differences
# print(comparison$summary$differences)

## ----view_plots, eval=FALSE---------------------------------------------------
# # Bar chart comparing classification counts
# comparison$plots$bar_chart
# 
# # Surplus/deficit comparison
# comparison$plots$surplus_deficit
# 
# # Percent change from base scenario
# comparison$plots$percent_change

## ----multiple_scenarios, eval=FALSE-------------------------------------------
# # Create three different scenarios
# conservative <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = FALSE,
#   cropland_threshold = 2000  # More restrictive
# )
# 
# moderate <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE,
#   cropland_threshold = 1234  # Default
# )
# 
# liberal <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE,
#   cropland_threshold = 500   # Less restrictive
# )
# 
# # Compare all three
# multi_comparison <- compare_scenarios(list(
#   "Conservative (No WWTP, High Threshold)" = conservative,
#   "Moderate (WWTP, Default Threshold)" = moderate,
#   "Liberal (WWTP, Low Threshold)" = liberal
# ))
# 
# # View results
# print(multi_comparison$comparison_data)
# multi_comparison$plots$bar_chart

## ----temporal_comparison, eval=FALSE------------------------------------------
# # Analyze multiple years
# year_2010 <- run_builtin_analysis(
#   scale = "county",
#   year = 2010,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# year_2013 <- run_builtin_analysis(
#   scale = "county",
#   year = 2013,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# year_2016 <- run_builtin_analysis(
#   scale = "county",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# # Compare temporal trends
# temporal <- compare_scenarios(list(
#   "2010" = year_2010,
#   "2013" = year_2013,
#   "2016" = year_2016
# ))
# 
# # See how classifications changed over time
# temporal$plots$bar_chart
# temporal$plots$percent_change

## ----scale_comparison, eval=FALSE---------------------------------------------
# county_scale <- run_builtin_analysis(
#   scale = "county",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# huc8_scale <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# huc2_scale <- run_builtin_analysis(
#   scale = "huc2",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# # Compare scales
# scale_comp <- compare_scenarios(list(
#   "County (n=~3000)" = county_scale,
#   "HUC8 (n=~2000)" = huc8_scale,
#   "HUC2 (n=18)" = huc2_scale
# ))
# 
# print(scale_comp$comparison_data)

## ----save_plots, eval=FALSE---------------------------------------------------
# # Create output directory
# output_dir <- "scenario_comparison_results"
# dir.create(output_dir, showWarnings = FALSE)
# 
# # Save all plots
# save_plot(
#   comparison$plots$bar_chart,
#   file.path(output_dir, "classification_comparison.png"),
#   width = 12, height = 8
# )
# 
# save_plot(
#   comparison$plots$surplus_deficit,
#   file.path(output_dir, "surplus_deficit_comparison.png"),
#   width = 12, height = 8
# )
# 
# save_plot(
#   comparison$plots$percent_change,
#   file.path(output_dir, "percent_change.png"),
#   width = 12, height = 8
# )

## ----save_data, eval=FALSE----------------------------------------------------
# # Save comparison data
# write.csv(
#   comparison$comparison_data,
#   file.path(output_dir, "scenario_comparison_data.csv"),
#   row.names = FALSE
# )
# 
# # Save summary statistics
# write.csv(
#   comparison$summary$differences,
#   file.path(output_dir, "scenario_differences.csv"),
#   row.names = FALSE
# )

## ----auto_save, eval=FALSE----------------------------------------------------
# # Comparison with automatic plot saving
# comparison <- compare_scenarios(
#   scenario_list = list(
#     "Base" = base_scenario,
#     "WWTP" = wwtp_scenario
#   ),
#   create_plots = TRUE,
#   output_dir = "my_comparison_results"  # Plots saved here
# )

## ----interpret_deltas, eval=FALSE---------------------------------------------
# # Extract differences
# diffs <- comparison$summary$differences
# 
# # Interpret changes
# if (diffs$delta_sources > 0) {
#   cat("Adding WWTP increased sources by", diffs$delta_sources, "units\n")
# } else if (diffs$delta_sources < 0) {
#   cat("Adding WWTP decreased sources by", abs(diffs$delta_sources), "units\n")
# }
# 
# # Percent change
# cat("This represents a", round(diffs$pct_change_sources, 1), "% change\n")

## ----management_example, eval=FALSE-------------------------------------------
# # Current state (no WWTP recovery)
# current <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = FALSE
# )
# 
# # With WWTP recovery management
# with_policy <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# # Compare
# policy_impact <- compare_scenarios(list(
#   "Current (No WWTP)" = current,
#   "With WWTP Recovery" = with_policy
# ))
# 
# # Key question: How many sink areas could benefit?
# sinks_helped <- policy_impact$summary$differences$delta_sinks
# cat("WWTP recovery could help", abs(sinks_helped), "deficit areas\n")

## ----sensitivity_example, eval=FALSE------------------------------------------
# thresholds <- c(500, 1000, 1234, 1500, 2000)
# results <- list()
# 
# for (thresh in thresholds) {
#   results[[paste0("Threshold_", thresh)]] <- run_builtin_analysis(
#     scale = "huc8",
#     year = 2016,
#     nutrients = "nitrogen",
#     include_wwtp = TRUE,
#     cropland_threshold = thresh
#   )
# }
# 
# # Compare all thresholds
# sensitivity <- compare_scenarios(results)
# 
# # See how excluded areas change
# excluded_counts <- sensitivity$comparison_data$n_excluded
# names(excluded_counts) <- names(results)
# print(excluded_counts)

## ----regional_example, eval=FALSE---------------------------------------------
# # Iowa
# iowa <- run_state_analysis(
#   state = "IA",
#   scale = "county",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# # Nebraska
# nebraska <- run_state_analysis(
#   state = "NE",
#   scale = "county",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# # Compare states
# state_comp <- compare_scenarios(list(
#   "Iowa" = iowa,
#   "Nebraska" = nebraska
# ))
# 
# state_comp$plots$bar_chart

## ----meaningful_comparison, eval=FALSE----------------------------------------
# # GOOD: Only WWTP inclusion changes
# compare_scenarios(list(
#   "No WWTP" = run_builtin_analysis(year=2016, include_wwtp=FALSE),
#   "With WWTP" = run_builtin_analysis(year=2016, include_wwtp=TRUE)
# ))
# 
# # LESS CLEAR: Multiple things change at once
# compare_scenarios(list(
#   "Base" = run_builtin_analysis(year=2016, scale="county", include_wwtp=FALSE),
#   "Alternative" = run_builtin_analysis(year=2015, scale="huc8", include_wwtp=TRUE)
# ))

## ----naming, eval=FALSE-------------------------------------------------------
# # GOOD names
# compare_scenarios(list(
#   "2016 Agricultural Only" = scenario1,
#   "2016 Agricultural + WWTP" = scenario2
# ))
# 
# # Less clear names
# compare_scenarios(list(
#   "Scenario 1" = scenario1,
#   "Scenario 2" = scenario2
# ))

## ----document, eval=FALSE-----------------------------------------------------
# # Create a metadata file
# metadata <- data.frame(
#   scenario = c("Base", "WWTP"),
#   year = c(2016, 2016),
#   scale = c("huc8", "huc8"),
#   include_wwtp = c(FALSE, TRUE),
#   cropland_threshold = c(1234, 1234)
# )
# 
# write.csv(metadata, "scenario_metadata.csv", row.names = FALSE)

## ----significance, eval=FALSE-------------------------------------------------
# # Small changes might not be meaningful
# diffs <- comparison$summary$differences
# 
# if (abs(diffs$pct_change_sources) < 5) {
#   cat("Change is less than 5% - may not be substantial\n")
# }

## ----troubleshooting, eval=FALSE----------------------------------------------
# # This comparison has limited value
# compare_scenarios(list(
#   "County" = county_results,  # ~3000 units
#   "HUC2" = huc2_results       # ~18 units
# ))
# 
# # Better: Compare percentages instead
# # Use the comparison data to calculate percentages yourself

## ----missing_nutrients, eval=FALSE--------------------------------------------
# # One has nitrogen, other has phosphorus - won't compare well
# # Make sure both scenarios analyze the same nutrient

