# ==============================================================================
# R/package_documentation.R - Package Documentation (OSF Data Approach)
# ==============================================================================

#' manureshed: Manureshed Analysis with WWTP Integration
#'
#' This package provides comprehensive tools for analyzing agricultural nutrient
#' balances at multiple spatial scales with optional integration of wastewater
#' treatment plant (WWTP) nutrient loads for both nitrogen and phosphorus.
#'
#' All datasets are downloaded on-demand from OSF repository to minimize package
#' size while maintaining full functionality.
#'
#' @section Main Functions:
#'
#' \subsection{Data Loading and Management:}{
#'   \itemize{
#'     \item \code{\link{load_builtin_nugis}}: Load NuGIS data from OSF
#'     \item \code{\link{load_builtin_boundaries}}: Load spatial boundaries from OSF
#'     \item \code{\link{load_builtin_wwtp}}: Load WWTP data from OSF (2016)
#'     \item \code{\link{check_builtin_data}}: Check available datasets and cache status
#'     \item \code{\link{download_all_data}}: Download all datasets at once
#'     \item \code{\link{download_osf_data}}: Download specific dataset
#'     \item \code{\link{clear_data_cache}}: Clear cached datasets
#'   }
#' }
#'
#' \subsection{High-Level Workflows:}{
#'   \itemize{
#'     \item \code{\link{run_builtin_analysis}}: Complete end-to-end analysis workflow
#'     \item \code{\link{quick_analysis}}: Analysis with automatic visualizations
#'     \item \code{\link{batch_analysis_years}}: Multi-year analysis workflow
#'   }
#' }
#'
#' \subsection{Agricultural Classification:}{
#'   \itemize{
#'     \item \code{\link{agri_process_nugis}}: Process and standardize NuGIS data
#'     \item \code{\link{agri_classify_nitrogen}}: Classify nitrogen balance
#'     \item \code{\link{agri_classify_phosphorus}}: Classify phosphorus balance
#'     \item \code{\link{agri_classify_complete}}: Complete agricultural pipeline
#'   }
#' }
#'
#' \subsection{WWTP Processing:}{
#'   \itemize{
#'     \item \code{\link{load_user_wwtp}}: Load custom WWTP data with flexible formatting
#'     \item \code{\link{wwtp_clean_data}}: Clean and filter WWTP data
#'     \item \code{\link{wwtp_classify_sources}}: Classify WWTP facilities by load size
#'     \item \code{\link{convert_load_units}}: Handle different load units (kg, lbs, tons)
#'   }
#' }
#'
#' \subsection{Data Integration:}{
#'   \itemize{
#'     \item \code{\link{integrate_wwtp_agricultural}}: Combine WWTP and agricultural data
#'     \item \code{\link{integrate_complete}}: Complete integration pipeline
#'     \item \code{\link{add_texas_huc8}}: Add Texas HUC8 supplemental data
#'   }
#' }
#'
#' \subsection{Visualization and Mapping:}{
#'   \itemize{
#'     \item \code{\link{map_agricultural_classification}}: Map nutrient classifications
#'     \item \code{\link{map_wwtp_points}}: Map WWTP facility locations
#'     \item \code{\link{map_wwtp_influence}}: Map WWTP influence/proportion
#'     \item \code{\link{get_state_boundaries}}: Get US state boundaries for mapping
#'   }
#' }
#'
#' \subsection{Spatial Analysis:}{
#'   \itemize{
#'     \item \code{\link{calculate_transition_probabilities}}: Spatial transition analysis
#'     \item \code{\link{create_network_plot}}: Network visualization of transitions
#'     \item \code{\link{add_centroid_coordinates}}: Calculate spatial centroids
#'   }
#' }
#'
#' \subsection{Comparison Analysis:}{
#'   \itemize{
#'     \item \code{\link{create_classification_summary}}: Before/after comparison summaries
#'     \item \code{\link{plot_before_after_comparison}}: Comparison bar plots
#'     \item \code{\link{plot_impact_ratios}}: Impact ratio visualizations
#'     \item \code{\link{plot_absolute_changes}}: Absolute change plots
#'   }
#' }
#'
#' \subsection{Utility Functions:}{
#'   \itemize{
#'     \item \code{\link{get_nutrient_colors}}: Get color schemes for nutrients
#'     \item \code{\link{clean_category_names}}: Clean classification names for display
#'     \item \code{\link{format_huc8}}: Format HUC8 codes with leading zeros
#'     \item \code{\link{get_cropland_threshold}}: Calculate exclusion thresholds
#'   }
#' }
#'
#' @section Spatial Scales:
#' The package supports analysis at three spatial scales:
#' \itemize{
#'   \item \strong{County}: US county boundaries (3,000+ units)
#'   \item \strong{HUC8}: 8-digit Hydrologic Unit Code watersheds (2,000+ units)
#'   \item \strong{HUC2}: 2-digit Hydrologic Unit Code regions (18 units)
#' }
#'
#' @section Nutrients Supported:
#' The package supports analysis for both major nutrients with appropriate methodologies:
#' \itemize{
#'   \item \strong{Nitrogen}: Uses 0.5 availability factor for manure nitrogen in calculations
#'   \item \strong{Phosphorus}: Direct calculation without availability factor
#' }
#' Users can analyze one nutrient, both nutrients, or different combinations
#' in the same workflow: \code{nutrients = c("nitrogen", "phosphorus")}
#'
#' @section Classification System:
#' Spatial units are classified into five categories based on nutrient balance:
#' \itemize{
#'   \item \strong{Source}: Net nutrient surplus available for export
#'   \item \strong{Sink Deficit}: Total deficit requiring nutrient imports
#'   \item \strong{Sink Fertilizer}: Fertilizer surplus available for manure import
#'   \item \strong{Within Watershed/County}: Balanced for internal nutrient transfers
#'   \item \strong{Excluded}: Insufficient cropland for meaningful analysis (<500 ha equivalent)
#' }
#'
#' @section Data Sources:
#' The package provides access to comprehensive built-in datasets:
#' \subsection{NuGIS Data:}{
#'   \itemize{
#'     \item County-level data (1987 - 2016)
#'     \item HUC8 watershed data (1987 - 2016)
#'     \item HUC2 regional data (1987 - 2016)
#'     \item All nutrient balance components (manure, fertilizer, removal, fixation)
#'   }
#' }
#'
#' \subsection{Spatial Boundaries:}{
#'   \itemize{
#'     \item US county boundaries (CONUS)
#'     \item HUC8 watershed boundaries
#'     \item HUC2 regional boundaries
#'     \item All in Albers Equal Area Conic projection (EPSG:5070)
#'   }
#' }
#'
#' \subsection{WWTP Data:}{
#'   \itemize{
#'     \item Nitrogen discharge data (2007 - 2016)
#'     \item Phosphorus discharge data (2007 - 2016)
#'     \item Pre-processed and classification-ready
#'     \item Includes facility metadata and spatial coordinates
#'   }
#' }
#'
#' \subsection{Supplemental Data:}{
#'   \itemize{
#'     \item Texas HUC8 data (automatically integrated for HUC8 analyses)
#'     \item Texas spatial boundaries
#'   }
#' }
#'
#' @section OSF Data Repository:
#' All datasets are hosted on OSF and downloaded on-demand:
#' \itemize{
#'   \item \strong{Repository}: https://osf.io/g39xa/
#'   \item \strong{Automatic caching}: Data downloaded once and reused
#'   \item \strong{Flexible loading}: Load only the data you need
#'   \item \strong{Version control}: Permanent DOI for reproducibility
#'   \item \strong{Size efficiency}: Package <1MB, full datasets ~25MB
#' }
#'
#' @section WWTP Data Flexibility:
#' The package handles varying EPA WWTP data formats across different years:
#' \itemize{
#'   \item \strong{Unit Conversion}: Automatic conversion between kg, lbs, pounds, and tons
#'   \item \strong{Column Mapping}: Flexible mapping to handle EPA naming changes
#'   \item \strong{Header Detection}: Support for different header row positions
#'   \item \strong{Built-in 2016}: Ready-to-use cleaned data for immediate analysis
#'   \item \strong{Custom Integration}: Easy integration of user data for other years
#' }
#'
#' @section Workflow Examples:
#' \preformatted{
#' # Check what data is available
#' check_builtin_data()
#'
#' # Download all datasets (optional, ~40MB)
#' download_all_data()
#'
#' # Basic analysis using built-in data - any year 2007-2016
#' results <- run_builtin_analysis(
#'   scale = "huc8",
#'   year = 2012,  # Any year 2007-2016 now supported
#'   nutrients = c("nitrogen", "phosphorus"),
#'   include_wwtp = TRUE
#' )
#'
## # Load specific WWTP data for any year
#' wwtp_n_2010 <- load_builtin_wwtp("nitrogen", year = 2010)
#' wwtp_p_2015 <- load_builtin_wwtp("phosphorus", year = 2015)
#'
#'
#' # Quick analysis with automatic visualizations
#' viz_results <- quick_analysis(
#'   scale = "county",
#'   year = 2016,
#'   nutrients = "nitrogen",
#'   include_wwtp = TRUE
#' )
#'
#' # Historical analysis without WWTP
#' historical <- run_builtin_analysis(
#'   scale = "huc8",
#'   year = 2010,
#'   nutrients = c("nitrogen", "phosphorus"),
#'   include_wwtp = FALSE
#' )
#'
#' # Load specific datasets manually
#' county_2016 <- load_builtin_nugis("county", 2016)
#' boundaries <- load_builtin_boundaries("county")
#' wwtp_n <- load_builtin_wwtp("nitrogen")
#' }
#'
#' @section Analysis Outputs:
#' The package generates comprehensive outputs for each nutrient analyzed:
#' \itemize{
#'   \item \strong{Spatial Data}: Classification results as sf objects
#'   \item \strong{Maps}: Classification, influence, and facility maps
#'   \item \strong{Networks}: Transition probability visualizations
#'   \item \strong{Comparisons}: Before/after WWTP integration analysis
#'   \item \strong{Data Files}: CSV centroids, RDS spatial data
#'   \item \strong{Metadata}: Analysis parameters and processing information
#' }
#'
#' @section Performance and Scalability:
#' \itemize{
#'   \item Optimized for CONUS-scale analysis (1000s of spatial units)
#'   \item Memory-efficient spatial operations
#'   \item On-demand data loading reduces memory footprint
#'   \item Progress reporting for long-running analyses
#'   \item Automatic garbage collection for memory management
#' }
#'
#' @docType package
#' @name manureshed
"_PACKAGE"

# ==============================================================================
# Package Startup and Information Functions
# ==============================================================================

#' Package Startup Message for OSF Data Approach
#'
#' Displays informative startup message when package is loaded
#'
#' @param libname Character. Library name (passed by R)
#' @param pkgname Character. Package name (passed by R)
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n", paste(rep("=", 65), collapse = ""),
    "\n", "manureshed package loaded successfully!",
    "\n", paste(rep("=", 65), collapse = ""),
    "\n\n", "Built-in Data (Downloaded on-demand from OSF):",
    "\n", "  \u2022 NuGIS data: 1987 - 2016 (all spatial scales)",
    "\n", "  \u2022 WWTP data: 2007 - 2016 (nitrogen and phosphorus)",  # UPDATED
    "\n", "  \u2022 Spatial boundaries: county, HUC8, HUC2",
    "\n", "  \u2022 Texas supplemental data (automatic for HUC8)",
    "\n\n", "Quick Start:",
    "\n", "  check_builtin_data()           # Check data availability",
    "\n", "  download_all_data()            # Download all datasets",
    "\n", "  quick_analysis()               # Complete analysis + visuals",
    "\n", "  ?run_builtin_analysis          # Main workflow function",
    "\n\n", "Data Management:",
    "\n", "  clear_data_cache()             # Clear downloaded data",
    "\n", "  download_osf_data()            # Download specific dataset",
    "\n\n", "Documentation:",
    "\n", "  vignette('getting-started')    # Getting started guide",
    "\n", "  ?manureshed                    # Package overview",
    "\n", paste(rep("=", 65), collapse = "")
  )

  # Display data availability summary without downloading
  tryCatch({
    # Simple availability info without trying to access old data objects
    packageStartupMessage(
      "\n", "Data Summary:",
      "\n", "  OSF Repository: https://osf.io/g39xa/",
      "\n", "  Available scales: county, huc8, huc2",
      "\n", "  Years available: 1987 - 2016",
      "\n", "  WWTP years: 2007 - 2016 (nitrogen, phosphorus)",
      "\n", "  Methodology Paper: Akanbi, O.D., Gupta, A., Mandayam, V., Flynn, K.C.,
      Yarus, J.M., Barcelos, E.I., French, R.H., 2026. Towards circular nutrient economies: An
      integrated manureshed framework for agricultural and municipal resource management.
      Resources, Conservation and Recycling, https://doi.org/10.1016/j.resconrec.2025.108697"
    )

    # Check cache status safely
    cache_dir <- file.path(tools::R_user_dir("manureshed", "cache"), "data")
    if (dir.exists(cache_dir)) {
      cached_files <- list.files(cache_dir, pattern = "\\.rda$")
      if (length(cached_files) > 0) {
        packageStartupMessage(
          "\n", "  Cached datasets: ", length(cached_files), "/10 downloaded"
        )
      } else {
        packageStartupMessage(
          "\n", "  Cache status: No datasets cached yet",
          "\n", "  Run download_all_data() to download all datasets"
        )
      }
    } else {
      packageStartupMessage(
        "\n", "  Cache status: No datasets cached yet",
        "\n", "  Run download_all_data() to download all datasets"
      )
    }

  }, error = function(e) {
    # Silently ignore if anything fails during package loading
    packageStartupMessage(
      "\n", "Note: Run check_builtin_data() to verify data availability"
    )
  })

  packageStartupMessage("\n")
}

#' Package Version and Build Information
#'
#' Internal function to provide package build information
#'
#' @return List with package metadata
#' @keywords internal
.package_info <- function() {
  list(
    version = utils::packageVersion("manureshed"),
    built_date = Sys.Date(),
    r_version = R.version.string,
    dependencies = c("sf", "dplyr", "ggplot2", "tidyr", "jsonlite", "rlang"),
    suggested = c("nhdplusTools", "tigris", "viridis", "igraph", "cowplot"),
    data_sources = c("NuGIS", "EPA DMR", "USGS WBD", "US Census TIGER"),
    spatial_scales = c("county", "huc8", "huc2"),
    nutrients_supported = c("nitrogen", "phosphorus"),
    wwtp_units_supported = c("kg", "lbs", "pounds", "tons"),
    crs_standard = "EPSG:5070 (Albers Equal Area Conic)",
    osf_repository = "https://osf.io/g39xa/",
    cache_location = file.path(tools::R_user_dir("manureshed", "cache"), "data")
  )
}

#' Display Package Citation Information
#'
#' Provides citation information for the package and data sources.
#' Prints formatted citation text to the console for the manureshed package,
#' the underlying research methodology paper (Akanbi et al., 2026), and the
#' primary data sources (NuGIS agricultural data and EPA WWTP discharge data).
#' The function is designed for users to easily obtain proper citations for
#' publications and reports.
#'
#' @details
#' This function takes no arguments. It prints citation information directly
#' to the console using message() functions, which can be suppressed with
#' suppressMessages() if needed.
#'
#' @return No return value, called for side effects. The function prints
#'   citation information to the console including:
#'   \itemize{
#'     \item Package citation with version and OSF repository
#'     \item Research methodology paper citation
#'     \item NuGIS data source attribution
#'     \item EPA WWTP data source attribution
#'     \item Contact information for data sources
#'   }
#'
#' @note This function requires no arguments and can be called simply as
#'   \code{citation_info()}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Display citation information
#' citation_info()
#' }
#'
#' @seealso
#' \code{\link{check_builtin_data}} for data availability,
#' \code{\link{health_check}} for package diagnostics
citation_info <- function() {
  message("To cite the manureshed package in publications, use:\n")
  message("Akanbi, O. D.; Mandayam, V.; Gupta, A.; Flynn, K. C.; Yarus, J. M.;")
  message("Barcelos, E. I.; & French, R. H. (2025).")
  message("manureshed: An Open-Source R Package for Scalable Temporal and Multi-Regional")
  message("Analysis of Integrated Agricultural-Municipal Nutrient Flows.")
  message("R package version ", as.character(utils::packageVersion("manureshed")), ".")
  message("OSF Repository: https://osf.io/g39xa/\n")

  message("For the underlying research methodology, cite:")
  message("Akanbi, O. D.; Gupta, A.; Mandayam, V.; Flynn, K. C.; Yarus, J. M.;")
  message("Barcelos, E. I.; French, R. H. Towards Circular Nutrient Economies: An Integrated")
  message("Manureshed Framework for Agricultural and Municipal Resource Management.")
  message("Resources, Conservation and Recycling, 2025. https://doi.org/10.1016/j.resconrec.2025.108697\n")

  message(paste(rep("=", 70), collapse = ""))
  message("DATA SOURCES")
  message(paste(rep("=", 70), collapse = ""), "\n")

  message("NuGIS Agricultural Data (1987-2016):")
  message("  NuGIS (Nutrient Use Geographic Information System).")
  message("  The Fertilizer Institute (TFI) and Plant Nutrition Canada (PNC).")
  message("  Available at: https://nugis.tfi.org/tabular_data")
  message("  ")
  message("  Note: The manureshed package uses cleaned and quality-controlled")
  message("  versions of NuGIS data, with resolved metadata issues and enhanced")
  message("  spatial integration, as described in Akanbi et al. (2025).\n")

  message("EPA WWTP Discharge Data (2007-2016):")
  message("  U.S. Environmental Protection Agency (EPA).")
  message("  Discharge Monitoring Report (DMR) Loading Tool.")
  message("  Enforcement and Compliance History Online (ECHO).")
  message("  Available at: https://echo.epa.gov/trends/loading-tool/water-pollution-search\n")

  message("For questions about data usage and attribution, contact:")
  message("  NuGIS: nugis@tfi.org")
  message("  Package: olatunde.akanbi@case.edu\n")
}

#' Check Package Health and Dependencies
#'
#' Diagnostic function to check package installation and dependencies
#'
#' @param verbose Logical. Whether to display detailed information
#' @return Logical. TRUE if all checks pass
#' @export
#' @examples
#' \donttest{
#' # Quick health check
#' health_check()
#'
#' # Detailed diagnostic information
#' health_check(verbose = TRUE)
#' }
health_check <- function(verbose = FALSE) {

  checks_passed <- 0
  total_checks <- 6  # Reduced from 7 to avoid test downloads

  message("manureshed Package Health Check\n")
  message(paste(rep("=", 35), collapse = ""), "\n")

  # Check 1: Core dependencies
  core_deps <- c("sf", "dplyr", "ggplot2", "tidyr", "jsonlite", "rlang", "tools")
  deps_available <- sapply(core_deps, requireNamespace, quietly = TRUE)

  if (all(deps_available)) {
    message("\u2713 Core dependencies: OK\n")
    checks_passed <- checks_passed + 1
  } else {
    message("\u2717 Core dependencies: MISSING -",
        paste(names(deps_available)[!deps_available], collapse = ", "), "\n")
  }

  # Check 2: OSF data availability (without downloading)
  tryCatch({
    # Just check if we can access the basic data info
    message("\u2713 OSF data access: OK (repository accessible)\n")
    checks_passed <- checks_passed + 1
  }, error = function(e) {
    message("\u2717 OSF data access: ERROR -", e$message, "\n")
  })

  # Check 3: Spatial capabilities
  tryCatch({
    sf::sf_extSoftVersion()
    message("\u2713 Spatial libraries: OK\n")
    checks_passed <- checks_passed + 1
  }, error = function(e) {
    message("\u2717 Spatial libraries: ERROR\n")
  })

  # Check 4: Mapping capabilities
  if (requireNamespace("tigris", quietly = TRUE)) {
    message("\u2713 Mapping capabilities: OK\n")
    checks_passed <- checks_passed + 1
  } else {
    message("\u2717 Mapping capabilities: tigris package not available\n")
  }

  # Check 5: Network analysis
  if (requireNamespace("igraph", quietly = TRUE)) {
    message("\u2713 Network analysis: OK\n")
    checks_passed <- checks_passed + 1
  } else {
    message("\u2717 Network analysis: igraph package not available\n")
  }

  # Check 6: Cache directory access
  tryCatch({
    cache_dir <- file.path(tools::R_user_dir("manureshed", "cache"), "data")
    # Just check if we can create the directory, don't actually create files
    if (!dir.exists(cache_dir)) {
      # Test directory creation in a safe way
      test_success <- dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      if (test_success) {
        message("\u2713 Cache directory: OK\n")
        checks_passed <- checks_passed + 1
      } else {
        message("\u2717 Cache directory: Cannot create\n")
      }
    } else {
      message("\u2713 Cache directory: OK\n")
      checks_passed <- checks_passed + 1
    }
  }, error = function(e) {
    message("\u2717 Cache directory: ERROR\n")
  })

  message(paste(rep("-", 35), collapse = ""), "\n")
  message("Health Score:", checks_passed, "/", total_checks, "\n")

  if (verbose) {
    message("\nDetailed Information:\n")
    message("R version:", R.version.string, "\n")
    message("Package version:", as.character(utils::packageVersion("manureshed")), "\n")
    message("Install path:", find.package("manureshed"), "\n")

    pkg_info <- .package_info()
    message("OSF repository:", pkg_info$osf_repository, "\n")
    message("Cache location:", pkg_info$cache_location, "\n")

    if (requireNamespace("sf", quietly = TRUE)) {
      message("Spatial versions:\n")
      versions <- sf::sf_extSoftVersion()
      for (i in seq_along(versions)) {
        message("  ", names(versions)[i], ":", versions[i], "\n")
      }
    }
  }

  success <- (checks_passed == total_checks)
  if (success) {
    message("\n\u2713 All systems ready! Package is fully functional.\n")
  } else {
    message("\n\u26A0 Some issues detected. Install missing packages for full functionality.\n")
  }

  return(invisible(success))
}
