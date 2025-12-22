## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 6
)

## ----setup--------------------------------------------------------------------
library(manureshed)

## ----quick_start, eval=FALSE--------------------------------------------------
# # Complete analysis with maps and plots
# results <- quick_analysis(
#   scale = "huc8",           # Choose: "county", "huc8", or "huc2"
#   year = 2016,              # Any year 1987-2016
#   nutrients = "nitrogen",   # Choose: "nitrogen", "phosphorus", or both
#   include_wwtp = TRUE       # Include wastewater plants (2007-2016 only)
# )

## ----check_data, eval=FALSE---------------------------------------------------
# # See what data is available
# check_builtin_data()
# 
# # Download all data (optional, ~40MB)
# download_all_data()

## ----basic_analysis, eval=FALSE-----------------------------------------------
# # Analyze just agricultural data
# results <- run_builtin_analysis(
#   scale = "county",
#   year = 2010,
#   nutrients = "nitrogen",
#   include_wwtp = FALSE    # No WWTP data
# )
# 
# # Quick summary
# summarize_results(results)

## ----with_wwtp, eval=FALSE----------------------------------------------------
# # Analysis with wastewater plants (2007-2016 available)
# results_wwtp <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2016,
#   nutrients = c("nitrogen", "phosphorus"),
#   include_wwtp = TRUE
# )
# 
# # See the difference WWTP makes
# comparison <- compare_analyses(results, results_wwtp, "nitrogen")
# print(comparison)

## ----access_results, eval=FALSE-----------------------------------------------
# # Agricultural data with classifications
# agri_data <- results$agricultural
# 
# # WWTP facility data
# wwtp_facilities <- results$wwtp$nitrogen$facility_data
# 
# # Combined results (agricultural + WWTP)
# combined_data <- results$integrated$nitrogen
# 
# # Analysis settings
# parameters <- results$parameters

## ----maps, eval=FALSE---------------------------------------------------------
# # Basic nitrogen map
# n_map <- map_agricultural_classification(
#   data = results$agricultural,
#   nutrient = "nitrogen",
#   classification_col = "N_class",
#   title = "Nitrogen Classifications"
# )
# 
# # Save the map
# save_plot(n_map, "nitrogen_map.png", width = 10, height = 8)

## ----wwtp_maps, eval=FALSE----------------------------------------------------
# # Map WWTP facilities
# facility_map <- map_wwtp_points(
#   results$wwtp$nitrogen$spatial_data,
#   nutrient = "nitrogen",
#   title = "Nitrogen WWTP Facilities"
# )
# 
# # Map WWTP influence on agricultural areas
# influence_map <- map_wwtp_influence(
#   results$integrated$nitrogen,
#   nutrient = "nitrogen",
#   title = "WWTP Influence on Nitrogen"
# )

## ----single_year, eval=FALSE--------------------------------------------------
# # Any year 1987-2016 for agricultural data
# results_1990 <- run_builtin_analysis(scale = "county", year = 1990,
#                                      nutrients = "nitrogen", include_wwtp = FALSE)
# 
# results_2005 <- run_builtin_analysis(scale = "huc8", year = 2005,
#                                      nutrients = "phosphorus", include_wwtp = FALSE)
# 
# # WWTP data available 2007-2016
# results_2012 <- run_builtin_analysis(scale = "huc8", year = 2012,
#                                      nutrients = "nitrogen", include_wwtp = TRUE)

## ----multiple_years, eval=FALSE-----------------------------------------------
# # Analyze several years at once
# batch_results <- batch_analysis_years(
#   years = 2014:2016,
#   scale = "county",
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )

## ----custom_wwtp, eval=FALSE--------------------------------------------------
# # Use your own WWTP files
# results_2020 <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2020,  # Agricultural data available
#   nutrients = "nitrogen",
#   include_wwtp = TRUE,
#   custom_wwtp_nitrogen = "my_wwtp_data_2020.csv",
#   wwtp_load_units = "lbs"  # Handle different units
# )

## ----state_analysis, eval=FALSE-----------------------------------------------
# # Analyze a specific state
# texas_results <- run_state_analysis(
#   state = "TX",
#   scale = "county",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# # Quick state analysis with maps
# ohio_quick <- quick_state_analysis(
#   state = "OH",
#   scale = "huc8",
#   year = 2015,
#   nutrients = "phosphorus"
# )

## ----individual_data, eval=FALSE----------------------------------------------
# # Load specific datasets
# county_2016 <- load_builtin_nugis("county", 2016)
# huc8_boundaries <- load_builtin_boundaries("huc8")
# wwtp_nitrogen <- load_builtin_wwtp("nitrogen", 2012)
# 
# # Check what years are available
# list_available_years()

## ----memory, eval=FALSE-------------------------------------------------------
# # For large analyses, clear cache if needed
# clear_data_cache()
# 
# # Check package health
# health_check()

## ----quality, eval=FALSE------------------------------------------------------
# # Always validate your results
# quick_check(results)
# 
# # Get package citation
# citation_info()

## ----help, eval=FALSE---------------------------------------------------------
# # Function documentation
# ?run_builtin_analysis
# ?quick_analysis
# ?map_agricultural_classification
# 
# # Package overview
# ?manureshed
# 
# # Check if everything is working
# health_check()

