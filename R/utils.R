#' @importFrom stats sd median ecdf quantile setNames
#' @importFrom utils read.csv write.csv
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select left_join right_join group_by summarise case_when if_else bind_rows
#' @importFrom rlang sym :=
#' @importFrom stats ecdf quantile setNames
#' @importFrom utils read.csv write.csv
NULL

# Add these to your globalVariables to suppress R CMD check notes
utils::globalVariables(c(
  # General data manipulation variables
  ".", "geometry", "State", "STUSPS",

  # WWTP data column names (various EPA formats)
  "Facility Latitude", "Facility Longitude",
  "Total Facility Design Flow (MGD)", "Actual Average Facility Flow (MGD)",
  "Wastewater Flow (MGal/yr)", "Pollutant Load (kg/yr)",
  "Average Daily Load (kg/day)", "Average Concentration (mg/L)",
  "Facility Name", "NPDES Permit Number",
  "Facility Type Indicator", "Major/Non-Major Status",

  # Standardized column names after processing
  "facility_name", "latitude", "longitude", "pollutant_load",
  "state", "npdes", "county", "facility_type", "major_status",
  "design_flow", "actual_flow", "wastewater_flow",
  "avg_daily_load", "avg_concentration",
  "Facility_Name", "Lat", "Long",  # <-- ADD THESE

  # Analysis result variables
  "wwtp_n_load", "wwtp_p_load", "wwtp_count",
  "combined_N_surplus", "combined_P_surplus",
  "combined_N_class", "combined_P_class",
  "wwtp_proportion_N", "wwtp_proportion_P",
  "source_class",

  # Nutrient balance components
  "manure_N", "manure_P", "fertilizer_N", "fertilizer_P",
  "N_fixation", "crop_removal_N", "crop_removal_P",
  "N_surplus", "P_surplus", "N_class", "P_class",
  "cropland",

  # Geographic and administrative variables
  "ID", "NAME", "FIPS", "HUC_8", "HUC_NAME",
  "longitude", "latitude",
  "boundary_id",  # <-- ADD THIS

  # Load variables in different units
  "N_Load_kg", "N_Load_tons", "P_Load_kg", "P_Load_tons",

  # Analysis metadata
  "Category", "Agricultural", "WWTP_Combined",
  "Absolute_Change", "Percent_Change", "Impact_Ratio",
  "Type", "Count",

  # Additional variables from check warnings
  "Year", "Classification", "Lat", "Long",

  # Aggregation variables
  "facility_index", "boundary_index", "total_load", "facility_count",

  "YoY_Change", "YoY_Percent",
  "use_builtin_wwtp"
))


#' CONUS States
#'
#' Vector of Continental United States state abbreviations
#'
#' @export
CONUS_STATES <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
                  "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
                  "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
                  "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD",
                  "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

#' Standard CRS for Manureshed Analysis
#'
#' Albers Equal Area Conic projection (EPSG:5070)
#'
#' @export
MANURESHED_CRS <- 5070

#' Conversion Factor: Kilograms to US Tons
#'
#' @export
KG_TO_TONS <- 907.185

#' Conversion Factor: Pounds to US Tons
#'
#' @export
LBS_TO_TONS <- 2000

#' Conversion Factor: Pounds to Kilograms
#'
#' @export
LBS_TO_KG <- 2.20462

#' Conversion Factor: P2O5 to P
#'
#' @export
P2O5_TO_P <- 0.436

#' Convert Load Units to Tons
#'
#' Convert loads from various units to US tons
#'
#' @param load_values Numeric vector of load values
#' @param from_unit Character. Input unit: "kg", "lbs", "pounds", "tons"
#' @return Numeric vector of loads in US tons
#' @export
#' @examples
#' # Convert from kilograms to tons
#' kg_loads <- c(1000, 2000, 3000)
#' tons_loads <- convert_load_units(kg_loads, "kg")
#'
#' # Convert from pounds to tons
#' lbs_loads <- c(5000, 10000, 15000)
#' tons_loads <- convert_load_units(lbs_loads, "lbs")
convert_load_units <- function(load_values, from_unit) {
  from_unit <- tolower(from_unit)

  converted_loads <- switch(from_unit,
                            "kg" = load_values / KG_TO_TONS,
                            "lbs" = load_values / LBS_TO_TONS,
                            "pounds" = load_values / LBS_TO_TONS,
                            "tons" = load_values,
                            stop("Unsupported unit: ", from_unit,
                                 ". Supported units: kg, lbs, pounds, tons"))

  return(converted_loads)
}

#' Default Color Schemes for Nutrient Classifications
#'
#' @param nutrient Character. Either "nitrogen" or "phosphorus"
#' @return Named vector of colors for classification categories
#' @export
get_nutrient_colors <- function(nutrient = "nitrogen") {
  # UPDATED: Slightly adjusted nitrogen colors for publication differentiation
  # Still maintains colorblind-friendly palette
  nitrogen_colors <- c(
    "Sink_Deficit" = "#a66",
    "Sink_Fertilizer" = "#dfc27d",   # Kept same (tan)
    "Within_Watershed" = "#8c6bb1",  # CHANGED: Slightly different purple (was #9467bd)
    "Within_County" = "#8c6bb1",     # CHANGED: Slightly different purple (was #9467bd)
    "Source" = "#80cdc1",            # Kept same (teal)
    "Excluded" = "#018571"           # Kept same (dark teal)
  )

  # Phosphorus colors remain unchanged
  phosphorus_colors <- c(
    "Sink_Deficit" = "#b2abd2",
    "Sink_Fertilizer" = "#f1b",
    "Within_Watershed" = "#d01c8b",
    "Within_County" = "#d01c8b",
    "Source" = "#b8e186",
    "Excluded" = "#4dac26"
  )

  switch(nutrient,
         "nitrogen" = nitrogen_colors,
         "phosphorus" = phosphorus_colors,
         stop("Nutrient must be 'nitrogen' or 'phosphorus'"))
}

#' Clean Category Names for Display
#'
#' @param names Character vector of category names to clean
#' @return Character vector of cleaned names
#' @export
clean_category_names <- function(names) {
  cleaned <- gsub("_", " ", names)
  cleaned <- gsub("Sink Deficit", "sink deficit", cleaned)
  cleaned <- gsub("Sink Fertilizer", "sink fertilizer", cleaned)
  cleaned <- gsub("Within Watershed", "within watershed", cleaned)
  cleaned <- gsub("Within County", "within county", cleaned)
  cleaned <- tolower(cleaned)
  return(cleaned)
}

#' Validate Required Columns
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param data_type Character description of data type for error messages
#' @return Logical. TRUE if all columns present, stops with error otherwise
validate_columns <- function(data, required_cols, data_type = "data") {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in", data_type, ":",
               paste(missing_cols, collapse = ", ")))
  }
  return(TRUE)
}

#' Format HUC8 Codes
#'
#' Add leading zeros to 7-digit HUC8 codes to make them 8-digit
#'
#' @param huc_codes Character or numeric vector of HUC codes
#' @return Character vector of properly formatted 8-digit HUC codes
#' @export
format_huc8 <- function(huc_codes) {
  huc_codes <- as.character(huc_codes)
  ifelse(nchar(huc_codes) == 7, paste0("0", huc_codes), huc_codes)
}

#' Clean Text Data
#'
#' Remove extra quotes and whitespace from text fields
#'
#' @param text Character vector to clean
#' @return Character vector of cleaned text
clean_text <- function(text) {
  # Remove extra single quotes at start and end
  text <- gsub("^''\\s*|\\s*''$", "", text)
  # Remove any remaining single quotes
  text <- gsub("'", "", text)
  # Remove extra spaces
  trimws(text)
}



#' Benchmark Analysis Performance
#'
#' Test analysis speed and memory usage
#'
#' @param scale Character. Spatial scale
#' @param year Numeric. Year to test
#' @param nutrients Character vector. Nutrients to analyze
#' @param n_runs Integer. Number of benchmark runs (default: 3)
#' @param include_wwtp Logical. Include WWTP processing
#' @return List with timing statistics and memory usage
#' @export
#' @examples
#' \donttest{
#' # Benchmark HUC8 analysis - use smaller scale for faster testing
#' benchmark <- benchmark_analysis(
#'   scale = "county",  # Use county for faster testing
#'   year = 2016,
#'   nutrients = "nitrogen",
#'   n_runs = 2  # Reduce runs for faster testing
#' )
#' print(benchmark)
#' }
benchmark_analysis <- function(scale = "huc8", year = 2016,
                               nutrients = "nitrogen", n_runs = 3,
                               include_wwtp = TRUE) {

  message("Benchmarking analysis performance...")
  message("  Scale: ", scale)
  message("  Year: ", year)
  message("  Runs: ", n_runs)

  timings <- numeric(n_runs)
  memory_used <- numeric(n_runs)

  for (i in 1:n_runs) {
    message("\nRun ", i, "/", n_runs)

    # Force garbage collection before run
    gc(verbose = FALSE, full = TRUE)

    # Record start memory
    mem_before <- sum(gc()[,2])
    start_time <- Sys.time()

    # Run analysis
    result <- run_builtin_analysis(
      scale = scale,
      year = year,
      nutrients = nutrients,
      include_wwtp = include_wwtp,
      save_outputs = FALSE,
      verbose = FALSE
    )

    # Record timing
    timings[i] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    # Record memory
    mem_after <- sum(gc()[,2])
    memory_used[i] <- mem_after - mem_before

    # Clean up
    rm(result)
    gc(verbose = FALSE)
  }

  results <- list(
    scale = scale,
    year = year,
    nutrients = paste(nutrients, collapse = ", "),
    include_wwtp = include_wwtp,
    n_runs = n_runs,
    timing = list(
      mean = mean(timings),
      sd = sd(timings),
      min = min(timings),
      max = max(timings),
      median = median(timings),
      all_runs = timings
    ),
    memory_mb = list(
      mean = mean(memory_used),
      max = max(memory_used),
      all_runs = memory_used
    ),
    timestamp = Sys.time()
  )

  class(results) <- c("manureshed_benchmark", "list")
  return(results)
}

#' Print Summary of Analysis Results
#'
#' Print formatted summary of manureshed analysis results to the console.
#' The summary includes analysis configuration parameters (scale, year, nutrients,
#' WWTP inclusion), spatial coverage statistics, agricultural nutrient classifications
#' with counts and percentages, WWTP integration metrics (if applicable), integrated
#' classifications (if available), output file information, and processing time.
#'
#' @param results List. Analysis results from \code{\link{run_builtin_analysis}} or
#'   \code{\link{run_state_analysis}}. Must contain at minimum:
#'   \itemize{
#'     \item \code{parameters}: List with scale, year, nutrients, include_wwtp
#'     \item \code{agricultural}: sf data frame with classification columns
#'   }
#'   Optional components:
#'   \itemize{
#'     \item \code{wwtp}: WWTP analysis results
#'     \item \code{integrated}: Integrated classification results
#'     \item \code{created_files} or \code{saved_files}: Output file paths
#'   }
#'
#' @param detailed Logical. If TRUE, includes additional breakdown of integrated
#'   classifications showing combined agricultural-WWTP nutrient classes. If FALSE
#'   (default), shows only agricultural classifications and basic WWTP statistics.
#'
#' @return Invisibly returns the input \code{results} list unchanged. The function
#'   is called primarily for its side effect of printing a formatted summary to the
#'   console. The invisible return allows for piping operations while displaying
#'   the summary.
#'
#' @details
#' The summary output is organized into sections:
#' \describe{
#'   \item{Analysis Configuration}{Scale, year, nutrients analyzed, WWTP inclusion, state (if applicable)}
#'   \item{Spatial Coverage}{Total number of spatial units analyzed}
#'   \item{Agricultural Classifications}{Nitrogen and phosphorus classification counts and percentages}
#'   \item{WWTP Integration}{Number of facilities and total loads by nutrient (if applicable)}
#'   \item{Integrated Classifications}{Combined agricultural-WWTP classes (if detailed = TRUE)}
#'   \item{Output Files}{Number and types of created files (if saved)}
#'   \item{Processing Time}{Analysis duration in minutes (if available)}
#' }
#'
#' Classification names are cleaned for display (underscores replaced with spaces,
#' line breaks removed). Percentages are rounded to one decimal place. All console
#' output uses \code{\link{message}} and can be suppressed with
#' \code{\link{suppressMessages}}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Basic summary
#' results <- run_builtin_analysis(scale = "county", year = 2016)
#' summarize_results(results)
#'
#' # Detailed summary with integrated classifications
#' results <- run_builtin_analysis(
#'   scale = "huc8",
#'   year = 2012,
#'   include_wwtp = TRUE
#' )
#' summarize_results(results, detailed = TRUE)
#' }

#' \dontrun{
#'   # This requires magrittr - never auto-run
#'   library(magrittr)
#'   results <- run_builtin_analysis(scale = "huc2", year = 2015) %>%
#'     summarize_results() %>%
#'     export_for_gis(output_dir = tempdir())
#' }
#'
#' @seealso
#' \code{\link{run_builtin_analysis}} for generating analysis results,
#' \code{\link{quick_check}} for quick validation,
#' \code{\link{compare_analyses}} for comparing two result sets
summarize_results <- function(results, detailed = FALSE) {
  message("\n", paste(rep("=", 60), collapse = ""))
  message("MANURESHED ANALYSIS SUMMARY")
  message(paste(rep("=", 60), collapse = ""), "\n")

  # Basic parameters
  message("Analysis Configuration:")
  message("  Scale:     ", results$parameters$scale)
  message("  Year:      ", results$parameters$year)
  message("  Nutrients: ", paste(results$parameters$nutrients, collapse = ", "))
  message("  WWTP:      ", if(results$parameters$include_wwtp) "Yes" else "No")
  if (!is.null(results$parameters$state)) {
    message("  State:     ", results$parameters$state)
  }
  message("")

  # Spatial units
  message("Spatial Coverage:")
  message("  Total units: ", nrow(results$agricultural), "\n")

  # Classification summaries
  message("Agricultural Classifications:")

  if ("N_class" %in% names(results$agricultural)) {
    message("  Nitrogen:")
    n_table <- table(results$agricultural$N_class)
    for (class in names(n_table)) {
      pct <- round(n_table[class] / sum(n_table) * 100, 1)
      message(sprintf("    %-20s %5d (%5.1f%%)",
                      clean_category_names(class), n_table[class], pct))
    }
    message("")
  }

  if ("P_class" %in% names(results$agricultural)) {
    message("  Phosphorus:")
    p_table <- table(results$agricultural$P_class)
    for (class in names(p_table)) {
      pct <- round(p_table[class] / sum(p_table) * 100, 1)
      message(sprintf("    %-20s %5d (%5.1f%%)",
                      clean_category_names(class), p_table[class], pct))
    }
    message("")
  }

  # WWTP summary if included
  if ("wwtp" %in% names(results)) {
    message("WWTP Integration:")
    for (nutrient in names(results$wwtp)) {
      if ("facility_data" %in% names(results$wwtp[[nutrient]])) {
        n_facilities <- nrow(results$wwtp[[nutrient]]$facility_data)
        load_col <- if (nutrient == "nitrogen") "N_Load_tons" else "P_Load_tons"
        total_load <- sum(results$wwtp[[nutrient]]$facility_data[[load_col]], na.rm = TRUE)

        message("  ", tools::toTitleCase(nutrient), ":")
        message("    Facilities:  ", n_facilities)
        message("    Total load:  ", sprintf("%.1f", total_load), " tons/year")
      }
    }
    message("")
  }

  # Combined classification if integrated
  if ("integrated" %in% names(results) && detailed) {
    message("Integrated Classifications (with WWTP):")

    for (nutrient in names(results$integrated)) {
      combined_col <- if (nutrient == "nitrogen") "combined_N_class" else "combined_P_class"

      if (combined_col %in% names(results$integrated[[nutrient]])) {
        message("  ", tools::toTitleCase(nutrient), ":")
        combined_table <- table(results$integrated[[nutrient]][[combined_col]])

        for (class in names(combined_table)) {
          pct <- round(combined_table[class] / sum(combined_table) * 100, 1)
          message(sprintf("    %-20s %5d (%5.1f%%)",
                          clean_category_names(class), combined_table[class], pct))
        }
      }
    }
    message("")
  }

  # Output files if saved
  if ("created_files" %in% names(results) || "saved_files" %in% names(results)) {
    all_files <- c(results$created_files, results$saved_files)
    if (length(all_files) > 0) {
      message("Output Files: ", length(all_files), " files created")
      if (detailed) {
        for (file_type in names(all_files)) {
          message("  ", file_type, ": ", basename(all_files[[file_type]]))
        }
      }
      message("")
    }
  }

  # Processing time
  if ("processing_time_minutes" %in% names(results$parameters)) {
    message("Processing Time: ", round(results$parameters$processing_time_minutes, 2), " minutes")
  }

  message(paste(rep("=", 60), collapse = ""), "\n")

  invisible(results)
}

#' Compare Two Analysis Results
#'
#' Compare classifications between two analysis results
#'
#' @param results1 First analysis results
#' @param results2 Second analysis results
#' @param nutrient Character. Nutrient to compare ("nitrogen" or "phosphorus")
#' @return Data frame with comparison
#' @export
#' @examples
#' \donttest{
#' results_2010 <- run_builtin_analysis(scale = "county", year = 2010)
#' results_2016 <- run_builtin_analysis(scale = "county", year = 2016)
#' comparison <- compare_analyses(results_2010, results_2016, "nitrogen")
#' }
compare_analyses <- function(results1, results2, nutrient = "nitrogen") {

  class_col <- if (nutrient == "nitrogen") "N_class" else "P_class"

  # Get classifications from both results
  classes1 <- table(results1$agricultural[[class_col]])
  classes2 <- table(results2$agricultural[[class_col]])

  # Get all unique classes
  all_classes <- unique(c(names(classes1), names(classes2)))

  # Create comparison data frame
  comparison <- data.frame(
    Classification = all_classes,
    Year1_Count = sapply(all_classes, function(x)
      ifelse(x %in% names(classes1), classes1[x], 0)),
    Year2_Count = sapply(all_classes, function(x)
      ifelse(x %in% names(classes2), classes2[x], 0)),
    stringsAsFactors = FALSE
  )

  # Add change columns
  comparison$Absolute_Change <- comparison$Year2_Count - comparison$Year1_Count
  comparison$Percent_Change <- round(
    (comparison$Year2_Count - comparison$Year1_Count) / comparison$Year1_Count * 100, 1
  )

  # Add years to column names
  year1 <- results1$parameters$year
  year2 <- results2$parameters$year
  names(comparison)[2:3] <- paste0(c(year1, year2), "_Count")

  # Print summary
  message("\nComparison of", tools::toTitleCase(nutrient), "Classifications")
  message(paste(rep("=", 50), collapse = ""))
  message("Year 1:", year1)
  message("Year 2:", year2, "\n\n")

  print(comparison)

  return(comparison)
}



#' Quick Data Check
#'
#' Perform quick validation checks on analysis results
#'
#' @param results Analysis results object
#' @param verbose Logical. Print detailed messages
#' @return Logical. TRUE if all checks pass
#' @export
quick_check <- function(results, verbose = TRUE) {

  checks_passed <- TRUE
  issues <- character()

  # Check 1: Results structure
  required_elements <- c("agricultural", "parameters")
  missing <- setdiff(required_elements, names(results))
  if (length(missing) > 0) {
    issues <- c(issues, paste("Missing elements:", paste(missing, collapse = ", ")))
    checks_passed <- FALSE
  }

  # Check 2: NA values in classifications
  if ("agricultural" %in% names(results)) {
    if ("N_class" %in% names(results$agricultural)) {
      na_count <- sum(is.na(results$agricultural$N_class))
      if (na_count > 0) {
        issues <- c(issues, paste("NA values in N_class:", na_count))
      }
    }

    if ("P_class" %in% names(results$agricultural)) {
      na_count <- sum(is.na(results$agricultural$P_class))
      if (na_count > 0) {
        issues <- c(issues, paste("NA values in P_class:", na_count))
      }
    }
  }

  # Check 3: Excluded percentage
  if ("N_class" %in% names(results$agricultural)) {
    n_excluded <- sum(results$agricultural$N_class == "Excluded", na.rm = TRUE)
    pct_excluded <- n_excluded / nrow(results$agricultural) * 100

    if (pct_excluded > 50) {
      issues <- c(issues, sprintf("%.1f%% of units excluded - check threshold", pct_excluded))
    }
  }

  # Check 4: Spatial validity
  if (inherits(results$agricultural, "sf")) {
    invalid <- !sf::st_is_valid(results$agricultural)
    if (any(invalid)) {
      issues <- c(issues, paste(sum(invalid), "invalid geometries"))
      checks_passed <- FALSE
    }
  }

  # Print results
  if (verbose) {
    message("\nData Quality Check\n")
    message(paste(rep("=", 40), collapse = ""), "\n")

    if (checks_passed && length(issues) == 0) {
      message("All checks passed\n")
    } else {
      if (length(issues) > 0) {
        message("Issues found:\n")
        for (issue in issues) {
          message(" ", issue, "\n")
        }
      }
    }
    message(paste(rep("=", 40), collapse = ""), "\n\n")
  }

  return(checks_passed)
}

#' List Available Built-in Years
#'
#' Show available years for each data type
#'
#' @param scale Character. Spatial scale (optional)
#' @return Data frame with available years by data type
#' @export
list_available_years <- function(scale = NULL) {

  available <- data.frame(
    Data_Type = c("NuGIS", "WWTP"),
    Start_Year = c(1987, 2007),
    End_Year = c(2016, 2016),
    Total_Years = c(30, 10),
    stringsAsFactors = FALSE
  )

  message("\nAvailable Data Years\n")
  message(paste(rep("=", 50), collapse = ""))
  print(available, row.names = FALSE)
  message("\n")

  if (!is.null(scale)) {
    message("For scale '", scale, "':\n", sep = "")
    message("  NuGIS: 1987-2016 (30 years)\n")
    message("  WWTP:  2007-2016 (10 years)\n\n")
  }

  message("Note: WWTP analysis requires year 2007-2016\n")
  message("      Custom WWTP data can be used for other years\n\n")

  invisible(available)
}
