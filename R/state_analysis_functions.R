# ==============================================================================
# R/state_analysis_functions.R - State-Level Analysis Support
# ==============================================================================

#' Get State FIPS Code
#'
#' Convert state abbreviation to FIPS code
#'
#' @param state_abbr Character. Two-letter state abbreviation (e.g., "OH", "TX")
#' @return Character. Two-digit state FIPS code
#' @export
get_state_fips <- function(state_abbr) {
  state_fips_map <- c(
    "AL" = "01", "AK" = "02", "AZ" = "04", "AR" = "05", "CA" = "06",
    "CO" = "08", "CT" = "09", "DE" = "10", "DC" = "11", "FL" = "12",
    "GA" = "13", "HI" = "15", "ID" = "16", "IL" = "17", "IN" = "18",
    "IA" = "19", "KS" = "20", "KY" = "21", "LA" = "22", "ME" = "23",
    "MD" = "24", "MA" = "25", "MI" = "26", "MN" = "27", "MS" = "28",
    "MO" = "29", "MT" = "30", "NE" = "31", "NV" = "32", "NH" = "33",
    "NJ" = "34", "NM" = "35", "NY" = "36", "NC" = "37", "ND" = "38",
    "OH" = "39", "OK" = "40", "OR" = "41", "PA" = "42", "RI" = "44",
    "SC" = "45", "SD" = "46", "TN" = "47", "TX" = "48", "UT" = "49",
    "VT" = "50", "VA" = "51", "WA" = "53", "WV" = "54", "WI" = "55",
    "WY" = "56"
  )

  state_abbr <- toupper(state_abbr)
  if (!state_abbr %in% names(state_fips_map)) {
    stop("Invalid state abbreviation: ", state_abbr)
  }

  return(state_fips_map[state_abbr])
}

#' Filter Data by State
#'
#' Filter spatial data to a specific state
#'
#' @param data Data frame or sf object. Spatial data with FIPS or HUC codes
#' @param state Character. Two-letter state abbreviation
#' @param scale Character. Spatial scale: "county", "huc8", or "huc2"
#' @param boundaries sf object. Spatial boundaries (optional, for HUC scales)
#' @return Filtered data for the specified state
#' @export
filter_by_state <- function(data, state, scale, boundaries = NULL) {

  state <- toupper(state)
  state_fips <- get_state_fips(state)

  # Map state abbreviations to full state names
  state_name_map <- c(
    "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas", "CA" = "California",
    "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware", "DC" = "District of Columbia",
    "FL" = "Florida", "GA" = "Georgia", "HI" = "Hawaii", "ID" = "Idaho", "IL" = "Illinois",
    "IN" = "Indiana", "IA" = "Iowa", "KS" = "Kansas", "KY" = "Kentucky", "LA" = "Louisiana",
    "ME" = "Maine", "MD" = "Maryland", "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota",
    "MS" = "Mississippi", "MO" = "Missouri", "MT" = "Montana", "NE" = "Nebraska", "NV" = "Nevada",
    "NH" = "New Hampshire", "NJ" = "New Jersey", "NM" = "New Mexico", "NY" = "New York",
    "NC" = "North Carolina", "ND" = "North Dakota", "OH" = "Ohio", "OK" = "Oklahoma",
    "OR" = "Oregon", "PA" = "Pennsylvania", "RI" = "Rhode Island", "SC" = "South Carolina",
    "SD" = "South Dakota", "TN" = "Tennessee", "TX" = "Texas", "UT" = "Utah", "VT" = "Vermont",
    "VA" = "Virginia", "WA" = "Washington", "WV" = "West Virginia", "WI" = "Wisconsin", "WY" = "Wyoming"
  )

  state_full_name <- state_name_map[state]

  if (scale == "county") {
    # For county data, try multiple filtering approaches
    filtered_data <- NULL

    # Method 1: Filter by full state name in "state" column
    if ("state" %in% names(data)) {
      filtered_data <- data[data$state == state_full_name, ]
      if (nrow(filtered_data) > 0) {
        message("Filtered to state ", state, ": ", nrow(filtered_data), " spatial units")
        return(filtered_data)
      }
    }

    # Method 2: Filter by state abbreviation in "state" column
    if ("state" %in% names(data)) {
      filtered_data <- data[toupper(data$state) == state, ]
      if (nrow(filtered_data) > 0) {
        message("Filtered to state ", state, ": ", nrow(filtered_data), " spatial units")
        return(filtered_data)
      }
    }

    # Method 3: Filter by FIPS prefix
    if ("FIPS" %in% names(data)) {
      filtered_data <- data[substr(data$FIPS, 1, 2) == state_fips, ]
      if (nrow(filtered_data) > 0) {
        message("Filtered to state ", state, ": ", nrow(filtered_data), " spatial units")
        return(filtered_data)
      }
    }

    # Method 4: Filter by ID (assuming it's FIPS)
    if ("ID" %in% names(data)) {
      filtered_data <- data[substr(data$ID, 1, 2) == state_fips, ]
      if (nrow(filtered_data) > 0) {
        message("Filtered to state ", state, ": ", nrow(filtered_data), " spatial units")
        return(filtered_data)
      }
    }

    # If we get here, no filtering method worked
    stop("Could not filter county data for state ", state,
         ". Available columns: ", paste(names(data), collapse = ", "),
         ". Sample state values: ", paste(unique(data$state)[1:min(5, length(unique(data$state)))], collapse = ", "))

  } else if (scale %in% c("huc8", "huc2")) {
    # For HUC scales, we need spatial intersection
    if (is.null(boundaries)) {
      stop("Boundaries required for filtering HUC data by state")
    }

    # Get state boundary - use tigris if available, otherwise error
    if (!requireNamespace("tigris", quietly = TRUE)) {
      stop("Package 'tigris' required for HUC state filtering. Install with: install.packages('tigris')")
    }

    state_boundary <- tigris::states(cb = TRUE) %>%
      dplyr::filter(STUSPS == state) %>%
      sf::st_transform(MANURESHED_CRS)

    # Spatial intersection
    if (inherits(boundaries, "sf")) {
      intersects <- sf::st_intersects(boundaries, state_boundary, sparse = FALSE)
      state_boundaries <- boundaries[intersects[,1], ]

      # Filter data based on spatial units in state
      # FIXED: Use the actual column names from your data
      boundary_id_col <- if (scale == "huc8") "huc8" else "huc2"  # These match your actual columns

      # Check if the boundary ID column exists
      if (!boundary_id_col %in% names(state_boundaries)) {
        stop("Boundary ID column '", boundary_id_col, "' not found in boundaries. Available columns: ",
             paste(names(state_boundaries), collapse = ", "))
      }

      state_ids <- state_boundaries[[boundary_id_col]]

      # Try different possible ID column names in the data
      data_id_col <- NULL
      possible_id_cols <- c("ID", "HUC_8", "HUC_2", "huc8", "huc2")

      for (col in possible_id_cols) {
        if (col %in% names(data)) {
          data_id_col <- col
          break
        }
      }

      if (is.null(data_id_col)) {
        stop("No suitable ID column found in HUC data for filtering. Available columns: ",
             paste(names(data), collapse = ", "))
      }

      message("Using data ID column: ", data_id_col)
      message("Using boundary ID column: ", boundary_id_col)

      filtered_data <- data[data[[data_id_col]] %in% state_ids, ]

    } else {
      stop("Boundaries must be an sf object for spatial filtering")
    }

  } else {
    stop("Unsupported scale: ", scale)
  }

  message("Filtered to state ", state, ": ", nrow(filtered_data), " spatial units")
  return(filtered_data)
}

#' Run State-Level Analysis
#'
#' Run manureshed analysis for a specific state
#'
#' @param state Character. Two-letter state abbreviation (e.g., "OH", "TX")
#' @param scale Character. Spatial scale: "county", "huc8", or "huc2"
#' @param year Numeric. Year to analyze
#' @param nutrients Character vector. Nutrients to analyze
#' @param include_wwtp Logical. Whether to include WWTP analysis
#' @param output_dir Character. Output directory
#' @param verbose Logical. Show progress messages
#' @param ... Additional arguments passed to run_builtin_analysis
#' @return List with analysis results for the state
#' @export
#' @examples
#' \donttest{
#' # Use Texas which has more data
#' texas_results <- run_state_analysis(
#'   state = "TX",
#'   scale = "county",  # Use county for faster processing
#'   year = 2016,
#'   nutrients = "nitrogen",  # Single nutrient for speed
#'   include_wwtp = TRUE
#' )
#'
#' # California county-level analysis
#' ca_results <- run_state_analysis(
#'   state = "CA",
#'   scale = "county",
#'   year = 2010,
#'   nutrients = "nitrogen"
#' )
#' }
run_state_analysis <- function(state, scale = "huc8", year = 2016,
                               nutrients = c("nitrogen", "phosphorus"),
                               include_wwtp = TRUE,
                               output_dir = file.path(tempdir(), paste0("state_", tolower(state), "_results")),
                               verbose = TRUE, ...) {

  if (verbose) {
    message("\n", paste(rep("=", 70), collapse = ""))
    message("STATE-LEVEL MANURESHED ANALYSIS")
    message(paste(rep("=", 70), collapse = ""))
    message("State:", state)
    message("Scale:", scale)
    message("Year:", year)
    message(paste(rep("-", 70), collapse = ""))
  }

  # Load full data first
  if (verbose) message("Loading national data...")
  nugis_data <- load_builtin_nugis(scale, year, verbose = FALSE)
  spatial_boundaries <- load_builtin_boundaries(scale, verbose = FALSE)

  # Filter to state - pass boundaries for HUC scales
  if (verbose) message("Filtering to state", state, "...")
  if (scale == "county") {
    # For county, we don't need boundaries
    state_nugis <- filter_by_state(nugis_data, state, scale)
    state_boundaries <- filter_by_state(spatial_boundaries, state, scale)
  } else {
    # For HUC scales, pass boundaries to the filter function
    state_nugis <- filter_by_state(nugis_data, state, scale, spatial_boundaries)
    state_boundaries <- filter_by_state(spatial_boundaries, state, scale, spatial_boundaries)
  }

  # Check if we have any data after filtering
  if (nrow(state_nugis) == 0) {
    warning("No data found for state ", state, " at ", scale, " scale for year ", year)
    return(list(
      agricultural = NULL,
      parameters = list(
        state = state,
        scale = scale,
        year = year,
        nutrients = nutrients,
        include_wwtp = include_wwtp,
        analysis_timestamp = Sys.time(),
        error = paste("No data found for state", state)
      )
    ))
  }

  # Get cropland threshold
  if (scale == "county") {
    cropland_threshold <- 500 * 2.47105
  } else {
    county_data <- load_builtin_nugis("county", year, verbose = FALSE)
    state_county <- filter_by_state(county_data, state, "county")

    # Check if we have county data for threshold calculation
    if (nrow(state_county) == 0) {
      warning("No county data found for state ", state, " - using default threshold")
      cropland_threshold <- 500 * 2.47105
    } else {
      cropland_threshold <- get_cropland_threshold(scale, state_county, state_nugis)
    }
  }

  # Process agricultural data
  if (verbose) message("Processing agricultural classifications...")
  agri_data <- agri_classify_complete(state_nugis, scale, cropland_threshold)

  # Join with spatial boundaries
  boundary_id_col <- switch(scale,
                            "county" = "FIPS",
                            "huc8" = "huc8",
                            "huc2" = "huc2")

  join_spec <- setNames("ID", boundary_id_col)
  agri_spatial <- state_boundaries %>%
    dplyr::left_join(agri_data, by = join_spec)

  # Initialize results
  results <- list(
    agricultural = agri_spatial,
    parameters = list(
      state = state,
      scale = scale,
      year = year,
      nutrients = nutrients,
      cropland_threshold = cropland_threshold,
      include_wwtp = include_wwtp,
      analysis_timestamp = Sys.time()
    )
  )

  # Process WWTP if requested and we have data
  if (include_wwtp && nrow(agri_data) > 0) {
    if (verbose) message("Processing state WWTP data...")

    wwtp_processed <- list()

    for (nutrient in nutrients) {
      # Load WWTP data
      wwtp_data <- load_builtin_wwtp(nutrient, year, verbose = FALSE)

      # Filter to state - WWTP data should have State column
      state_col <- NULL
      if ("State" %in% names(wwtp_data)) {
        state_col <- "State"
      } else if ("STATE" %in% names(wwtp_data)) {
        state_col <- "STATE"
      } else if ("state" %in% names(wwtp_data)) {
        state_col <- "state"
      }

      if (!is.null(state_col)) {
        wwtp_state <- wwtp_data[toupper(wwtp_data[[state_col]]) == state, ]
      } else {
        # Spatial filtering as fallback
        wwtp_sf <- wwtp_to_spatial(wwtp_data)

        if (!requireNamespace("tigris", quietly = TRUE)) {
          warning("Package 'tigris' required for WWTP spatial filtering. Skipping WWTP analysis.")
          next
        }

        state_boundary <- tigris::states(cb = TRUE) %>%
          dplyr::filter(STUSPS == state) %>%
          sf::st_transform(4326)

        intersects <- sf::st_intersects(wwtp_sf, state_boundary, sparse = FALSE)
        wwtp_state <- wwtp_data[intersects[,1], ]
      }

      if (nrow(wwtp_state) > 0) {
        wwtp_clean <- wwtp_filter_positive_loads(wwtp_state, nutrient) %>%
          wwtp_classify_sources(nutrient)

        wwtp_sf <- wwtp_to_spatial(wwtp_clean)
        wwtp_aggregated <- wwtp_aggregate_by_boundaries(
          wwtp_sf, state_boundaries, nutrient, boundary_id_col
        )

        wwtp_processed[[nutrient]] <- list(
          facility_data = wwtp_clean,
          spatial_data = wwtp_sf,
          aggregated_data = wwtp_aggregated
        )
      }
    }

    if (length(wwtp_processed) > 0) {
      results$wwtp <- wwtp_processed

      # Integration
      if (verbose) message("Integrating WWTP and agricultural data...")
      integrated_data <- list()

      for (nutrient in names(wwtp_processed)) {
        integrated_data[[nutrient]] <- integrate_wwtp_agricultural(
          agri_spatial,
          wwtp_processed[[nutrient]]$aggregated_data,
          nutrient, cropland_threshold, scale
        )
      }

      results$integrated <- integrated_data
    }
  }

  if (verbose) {
    message("\n", paste(rep("=", 70), collapse = ""))
    message("STATE ANALYSIS COMPLETE\n")
    message(paste(rep("=", 70), collapse = ""))
    message("State:", state)
    message("Spatial units:", nrow(results$agricultural))
    if ("wwtp" %in% names(results)) {
      total_facilities <- sum(sapply(names(results$wwtp), function(n) {
        nrow(results$wwtp[[n]]$facility_data)
      }))
      message("WWTP facilities:", total_facilities)
    }
    message(paste(rep("=", 70), collapse = ""))
  }

  return(results)
}

#' Quick State Analysis with Visualization
#'
#' Run state-level analysis with automatic visualizations
#'
#' @param state Character. Two-letter state abbreviation
#' @param scale Character. Spatial scale
#' @param year Numeric. Year to analyze
#' @param nutrients Character vector. Nutrients to analyze
#' @param include_wwtp Logical. Include WWTP analysis
#' @param output_dir Character. Output directory
#' @param create_maps Logical. Create maps
#' @param create_networks Logical. Create network plots
#' @param create_comparisons Logical. Create comparison plots
#' @param verbose Logical. Show progress
#' @param ... Additional arguments
#' @return List with results and visualizations
#' @export
#' @examples
#' \donttest{
#' # Quick state analysis - use states with good data coverage
#' results <- quick_state_analysis(
#'   state = "TX",  # Texas has good data coverage
#'   scale = "county",
#'   year = 2016,
#'   nutrients = "nitrogen",
#'   include_wwtp = TRUE
#' )
#' }
quick_state_analysis <- function(state, scale = "huc8", year = 2016,
                                 nutrients = c("nitrogen", "phosphorus"),
                                 include_wwtp = TRUE,
                                 output_dir = file.path(tempdir(), paste0("state_", tolower(state), "_results")),
                                 create_maps = TRUE,
                                 create_networks = TRUE,
                                 create_comparisons = TRUE,
                                 verbose = TRUE, ...) {

  # Run state analysis
  results <- run_state_analysis(
    state = state,
    scale = scale,
    year = year,
    nutrients = nutrients,
    include_wwtp = include_wwtp,
    output_dir = output_dir,
    verbose = verbose,
    ...
  )

  # Check if analysis was successful
  if (is.null(results$agricultural)) {
    warning("State analysis failed - no visualizations created")
    return(results)
  }

  # Create visualizations (similar to quick_analysis but state-focused)
  created_files <- list()

  if (create_maps || create_networks || create_comparisons) {
    if (verbose) message("\nGenerating state visualizations...")

    # Create output directory
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    for (nutrient in nutrients) {
      if (create_maps) {
        # Agricultural map
        agri_class_col <- if (nutrient == "nitrogen") "N_class" else "P_class"
        agri_map <- map_agricultural_classification(
          results$agricultural, nutrient, agri_class_col,
          paste(state, "Agricultural", tools::toTitleCase(nutrient), "- Year", year)
        )

        agri_map_file <- file.path(output_dir,
                                   paste0("map_", tolower(state), "_agricultural_",
                                          nutrient, "_", year, ".png"))
        save_plot(agri_map, agri_map_file, width = 10, height = 8, dpi = 300)
        created_files[[paste0("agricultural_", nutrient, "_map")]] <- agri_map_file
      }

      # Add combined maps and other visualizations if WWTP included...
      if (include_wwtp && "integrated" %in% names(results) &&
          nutrient %in% names(results$integrated)) {

        # Combined map
        combined_class_col <- if (nutrient == "nitrogen") "combined_N_class" else "combined_P_class"
        combined_map <- map_agricultural_classification(
          results$integrated[[nutrient]], nutrient, combined_class_col,
          paste(state, "Combined", tools::toTitleCase(nutrient), "- Year", year)
        )

        combined_map_file <- file.path(output_dir,
                                       paste0("map_", tolower(state), "_combined_",
                                              nutrient, "_", year, ".png"))
        save_plot(combined_map, combined_map_file, width = 10, height = 8, dpi = 300)
        created_files[[paste0("combined_", nutrient, "_map")]] <- combined_map_file
      }
    }
  }

  results$visualization <- list(
    created_files = created_files,
    state = state
  )

  return(results)
}
