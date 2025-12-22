#' Complete Manureshed Analysis Workflow (Built-in Data)
#'
#' Run complete manureshed analysis using built-in NuGIS data (start-2016)
#' and optional WWTP data. For WWTP analysis beyond 2016, users must provide their own data.
#' Supports analysis of nitrogen, phosphorus, or both nutrients simultaneously.
#'
#' @param scale Character. Spatial scale: "county", "huc8", or "huc2"
#' @param year Numeric. Year to analyze (available: start-2016 for NuGIS, 2016 for built-in WWTP)
#' @param nutrients Character vector. Nutrients to analyze: c("nitrogen", "phosphorus") or subset
#' @param output_dir Character. Output directory for results (default: "manureshed_results")
#' @param include_wwtp Logical. Whether to include WWTP analysis (default: TRUE)
#' @param wwtp_year Numeric. Year for WWTP data (default: same as year, only 2016 available built-in)
#' @param custom_wwtp_nitrogen Character. Path to custom WWTP nitrogen file (for non-2016 years)
#' @param custom_wwtp_phosphorus Character. Path to custom WWTP phosphorus file (for non-2016 years)
#' @param wwtp_column_mapping Named list. Custom column mapping for WWTP data
#' @param wwtp_skip_rows Numeric. Rows to skip in custom WWTP files (default: 0)
#' @param wwtp_header_row Numeric. Header row in custom WWTP files (default: 1)
#' @param wwtp_load_units Character. Units of WWTP loads: "kg", "lbs", "pounds", "tons" (default: "kg")
#' @param add_texas Logical. Whether to add Texas HUC8 data (only for HUC8 scale, default: FALSE)
#' @param save_outputs Logical. Whether to save results to files (default: TRUE)
#' @param cropland_threshold Numeric. Custom cropland threshold for exclusion (optional)
#' @param verbose Logical. Whether to print detailed progress messages (default: TRUE)
#' @return List with all analysis results for specified nutrients
#' @export
#' @examples
#' \donttest{
#' # Basic analysis using built-in data (2007-2016 WWTP available)
#' results_2016 <- run_builtin_analysis(
#'   scale = "huc8",
#'   year = 2016,
#'   nutrients = c("nitrogen", "phosphorus"),
#'   include_wwtp = TRUE
#' )
#'
#' # Analysis for earlier year (no WWTP available) - nitrogen only
#' results_2010 <- run_builtin_analysis(
#'   scale = "county",
#'   year = 2010,
#'   nutrients = "nitrogen",
#'   include_wwtp = FALSE
#' )
#'
#' # Analysis for earlier year with WWTP now available
#' results_2010 <- run_builtin_analysis(
#'   scale = "county",
#'   year = 2010,
#'   nutrients = "nitrogen",
#'   include_wwtp = TRUE  # Now supported for 2010!
#' )
#'
#' # Analysis for year before WWTP availability
#' results_2005 <- run_builtin_analysis(
#'   scale = "huc8",
#'   year = 2005,
#'   nutrients = "phosphorus",
#'   include_wwtp = FALSE  # No WWTP data before 2007
#' )
#' }
run_builtin_analysis <- function(scale = "huc8", year = 2016,
                                 nutrients = c("nitrogen", "phosphorus"),
                                 output_dir = tempdir(),
                                 include_wwtp = TRUE, wwtp_year = NULL,
                                 custom_wwtp_nitrogen = NULL,
                                 custom_wwtp_phosphorus = NULL,
                                 wwtp_column_mapping = NULL,
                                 wwtp_skip_rows = 0, wwtp_header_row = 1,
                                 wwtp_load_units = "kg",
                                 add_texas = FALSE, save_outputs = TRUE,
                                 cropland_threshold = NULL, verbose = TRUE) {

  start_time <- Sys.time()

  # Validate inputs
  if (!all(nutrients %in% c("nitrogen", "phosphorus"))) {
    stop("nutrients must be 'nitrogen', 'phosphorus', or both")
  }

  if (!scale %in% c("county", "huc8", "huc2")) {
    stop("scale must be 'county', 'huc8', or 'huc2'")
  }

  if (!wwtp_load_units %in% c("kg", "lbs", "pounds", "tons")) {
    stop("wwtp_load_units must be 'kg', 'lbs', 'pounds', or 'tons'")
  }

  # Create output directory
  if (save_outputs && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) message("Created output directory: ", output_dir)
  }

  if (verbose) {
    message("\n", paste(rep("=", 70), collapse = ""))
    message("BATCH MANURESHED ANALYSIS")
    message(paste(rep("=", 70), collapse = ""))
    message("Year: ", year)
    message("Scale: ", scale)
    message("Nutrients: ", paste(nutrients, collapse = ", "))
    message(paste(rep("-", 70), collapse = ""), "\n")
  }

  # Set WWTP year if not specified
  if (is.null(wwtp_year)) {
    wwtp_year <- year
  }

  # Check data availability
  if (verbose) message("Checking data availability...")
  available_data <- check_builtin_data()

  # Validate scale availability
  if (!scale %in% available_data$nugis_scales) {
    stop("Built-in NuGIS data not available for scale: ", scale,
         "\nAvailable scales: ", paste(available_data$nugis_scales, collapse = ", "))
  }

  # Validate year availability for NuGIS
  if (!year %in% available_data$nugis_years[[scale]]) {
    stop("Built-in NuGIS data not available for year: ", year, " at scale: ", scale,
         "\nAvailable years: ", paste(available_data$nugis_years[[scale]], collapse = ", "))
  }

  if (verbose) {
    message("Data availability confirmed\n")
    message("  Available scales:", paste(available_data$nugis_scales, collapse = ", "), "\n")
    message("  Available years for", scale, ":",
        paste(range(available_data$nugis_years[[scale]]), collapse = "-"), "\n")
    message("  Built-in WWTP data:", if(available_data$wwtp_available) "Available (2007-2016)" else "Not available", "\n\n")
  }

  # Load built-in NuGIS data and boundaries
  if (verbose) message("Loading built-in NuGIS data...\n")
  nugis_data <- load_builtin_nugis(scale, year)

  if (verbose) message("Loading built-in spatial boundaries...\n")
  spatial_boundaries <- load_builtin_boundaries(scale)

  # Get boundary ID column name based on scale
  boundary_id_col <- switch(scale,
                            "county" = "FIPS",
                            "huc8" = "huc8",
                            "huc2" = "huc2",
                            stop("Unsupported scale: ", scale))

  # Calculate cropland threshold
  if (verbose) message("Calculating cropland threshold...\n")
  if (is.null(cropland_threshold)) {
    if (scale == "county") {
      cropland_threshold <- 500 * 2.47105  # 500 ha in acres
    } else {
      # Load county data for threshold calculation
      county_data <- load_builtin_nugis("county", year)
      cropland_threshold <- get_cropland_threshold(scale, county_data, nugis_data)
    }
  }

  if (verbose) {
    message("Cropland threshold:", round(cropland_threshold, 2), "acres\n\n")
  }

  # Process agricultural data
  if (verbose) message("Processing agricultural classifications...\n")
  agri_data <- agri_classify_complete(nugis_data, scale, cropland_threshold)

  # Join with spatial boundaries (fixed dynamic column joining)
  join_spec <- setNames("ID", boundary_id_col)
  agri_spatial <- spatial_boundaries %>%
    dplyr::left_join(agri_data, by = join_spec) %>%
    dplyr::mutate(
      # Handle units without agricultural data (territories, independent cities, etc.)
      N_class = dplyr::if_else(is.na(N_class), "Excluded", N_class),
      P_class = dplyr::if_else(is.na(P_class), "Excluded", P_class),
      manure_N = dplyr::if_else(is.na(manure_N), 0, manure_N),
      manure_P = dplyr::if_else(is.na(manure_P), 0, manure_P),
      fertilizer_N = dplyr::if_else(is.na(fertilizer_N), 0, fertilizer_N),
      fertilizer_P = dplyr::if_else(is.na(fertilizer_P), 0, fertilizer_P),
      N_fixation = dplyr::if_else(is.na(N_fixation), 0, N_fixation),
      crop_removal_N = dplyr::if_else(is.na(crop_removal_N), 0, crop_removal_N),
      crop_removal_P = dplyr::if_else(is.na(crop_removal_P), 0, crop_removal_P),
      cropland = dplyr::if_else(is.na(cropland), 0, cropland),
      N_surplus = dplyr::if_else(is.na(N_surplus),
                                 0.5 * manure_N - (crop_removal_N - N_fixation),
                                 N_surplus),
      P_surplus = dplyr::if_else(is.na(P_surplus),
                                 manure_P - crop_removal_P,
                                 P_surplus)
    )

  if (verbose) {
    message("Agricultural classification complete\n")
    message("  Spatial units processed:", nrow(agri_spatial), "\n")
    if ("N_class" %in% names(agri_spatial)) {
      n_classes <- table(agri_spatial$N_class, useNA = "ifany")
      message("  Nitrogen classes:", paste(names(n_classes), "(", n_classes, ")", collapse = ", "), "\n")
    }
    if ("P_class" %in% names(agri_spatial)) {
      p_classes <- table(agri_spatial$P_class, useNA = "ifany")
      message("  Phosphorus classes:", paste(names(p_classes), "(", p_classes, ")", collapse = ", "), "\n")
    }
    message("\n")
  }

  # Initialize results structure
  results <- list(
    agricultural = agri_spatial,
    parameters = list(
      scale = scale,
      year = year,
      nutrients = nutrients,
      cropland_threshold = cropland_threshold,
      include_wwtp = include_wwtp,
      analysis_timestamp = Sys.time()
    )
  )

  # Process WWTP data if requested
  if (include_wwtp) {
    if (verbose) {
      message("Processing WWTP data...\n")
      message("  Nutrients:", paste(nutrients, collapse = ", "), "\n")
      message("  WWTP year:", wwtp_year, "\n")
      message("  Load units:", wwtp_load_units, "\n")
    }

    # Determine WWTP data source
    use_builtin_wwtp <- (wwtp_year %in% 2007:2016 &&  # UPDATED year range
                           is.null(custom_wwtp_nitrogen) &&
                           is.null(custom_wwtp_phosphorus) &&
                           available_data$wwtp_available)

    if (verbose) {
      message("  Data source:", if(use_builtin_wwtp) paste0("Built-in (", wwtp_year, ")") else "Custom files", "\n")
    }

    # Initialize WWTP data containers
    wwtp_nitrogen_data <- NULL
    wwtp_phosphorus_data <- NULL

    # Load nitrogen data if needed
    if ("nitrogen" %in% nutrients) {
      if (use_builtin_wwtp) {
        if (verbose) message("  Loading built-in nitrogen WWTP data for", wwtp_year, "...\n")
        wwtp_nitrogen_data <- load_builtin_wwtp("nitrogen", year = wwtp_year)  # PASS YEAR
      } else {
        if (is.null(custom_wwtp_nitrogen)) {
          stop("Custom WWTP nitrogen data path required for year ", wwtp_year,
               " (built-in data only available for 2007-2016)")  # UPDATED
        }
        if (verbose) message("  Loading custom nitrogen WWTP data...\n")
        wwtp_nitrogen_raw <- load_user_wwtp(custom_wwtp_nitrogen, "nitrogen",
                                            wwtp_column_mapping, wwtp_skip_rows, wwtp_header_row,
                                            wwtp_load_units)
        wwtp_nitrogen_data <- wwtp_clean_data(wwtp_nitrogen_raw, "nitrogen")
      }
    }

    # Load phosphorus data if needed
    if ("phosphorus" %in% nutrients) {
      if (use_builtin_wwtp) {
        if (verbose) message("  Loading built-in phosphorus WWTP data for", wwtp_year, "...\n")
        wwtp_phosphorus_data <- load_builtin_wwtp("phosphorus", year = wwtp_year)  # PASS YEAR
      } else {
        if (is.null(custom_wwtp_phosphorus)) {
          stop("Custom WWTP phosphorus data path required for year ", wwtp_year,
               " (built-in data only available for 2007-2016)")  # UPDATED
        }
        if (verbose) message("  Loading custom phosphorus WWTP data...\n")
        wwtp_phosphorus_raw <- load_user_wwtp(custom_wwtp_phosphorus, "phosphorus",
                                              wwtp_column_mapping, wwtp_skip_rows, wwtp_header_row,
                                              wwtp_load_units)
        wwtp_phosphorus_data <- wwtp_clean_data(wwtp_phosphorus_raw, "phosphorus")
      }
    }

    # Process WWTP data for each nutrient
    wwtp_processed <- list()

    if ("nitrogen" %in% nutrients && !is.null(wwtp_nitrogen_data)) {
      if (verbose) message("  Processing nitrogen WWTP facilities...\n")
      wwtp_n_clean <- wwtp_nitrogen_data %>%
        wwtp_filter_positive_loads("nitrogen") %>%
        wwtp_classify_sources("nitrogen")

      wwtp_n_sf <- wwtp_to_spatial(wwtp_n_clean)
      wwtp_n_aggregated <- wwtp_aggregate_by_boundaries(wwtp_n_sf, spatial_boundaries,
                                                        "nitrogen", boundary_id_col)

      wwtp_processed$nitrogen <- list(
        facility_data = wwtp_n_clean,
        spatial_data = wwtp_n_sf,
        aggregated_data = wwtp_n_aggregated
      )
    }

    if ("phosphorus" %in% nutrients && !is.null(wwtp_phosphorus_data)) {
      if (verbose) message("  Processing phosphorus WWTP facilities...\n")
      wwtp_p_clean <- wwtp_phosphorus_data %>%
        wwtp_filter_positive_loads("phosphorus") %>%
        wwtp_classify_sources("phosphorus")

      wwtp_p_sf <- wwtp_to_spatial(wwtp_p_clean)
      wwtp_p_aggregated <- wwtp_aggregate_by_boundaries(wwtp_p_sf, spatial_boundaries,
                                                        "phosphorus", boundary_id_col)

      wwtp_processed$phosphorus <- list(
        facility_data = wwtp_p_clean,
        spatial_data = wwtp_p_sf,
        aggregated_data = wwtp_p_aggregated
      )
    }

    # Store WWTP results
    results$wwtp <- wwtp_processed

    if (verbose) {
      message("WWTP data processing complete\n")
      for (nutrient in names(wwtp_processed)) {
        n_facilities <- nrow(wwtp_processed[[nutrient]]$facility_data)
        n_spatial_units <- nrow(wwtp_processed[[nutrient]]$aggregated_data)
        message("  ", nutrient, ":", n_facilities, "facilities in", n_spatial_units, "spatial units\n")
      }
      message("\n")
    }

    # Integrate WWTP with agricultural data
    if (verbose) message("Integrating WWTP and agricultural data...\n")

    integrated_data <- list()

    # Integrate nitrogen if available
    if ("nitrogen" %in% nutrients && "nitrogen" %in% names(wwtp_processed)) {
      nitrogen_integrated <- integrate_wwtp_agricultural(
        agri_spatial,
        wwtp_processed$nitrogen$aggregated_data,
        "nitrogen", cropland_threshold, scale
      )
      integrated_data$nitrogen <- nitrogen_integrated
    }

    # Integrate phosphorus if available
    if ("phosphorus" %in% nutrients && "phosphorus" %in% names(wwtp_processed)) {
      phosphorus_integrated <- integrate_wwtp_agricultural(
        agri_spatial,
        wwtp_processed$phosphorus$aggregated_data,
        "phosphorus", cropland_threshold, scale
      )
      integrated_data$phosphorus <- phosphorus_integrated
    }

    # Add Texas data if requested (applies to both nutrients)
    if (add_texas && scale == "huc8") {
      if (verbose) message("Adding Texas HUC8 supplemental data...\n")
      if ("nitrogen" %in% names(integrated_data)) {
        integrated_data$nitrogen <- add_texas_huc8(integrated_data$nitrogen, year, cropland_threshold)
      }
      if ("phosphorus" %in% names(integrated_data)) {
        integrated_data$phosphorus <- add_texas_huc8(integrated_data$phosphorus, year, cropland_threshold)
      }
    }

    results$integrated <- integrated_data
    results$parameters$wwtp_year <- wwtp_year
    results$parameters$wwtp_source <- if (use_builtin_wwtp) "built-in" else "custom"
    results$parameters$wwtp_load_units <- wwtp_load_units
    results$parameters$add_texas <- add_texas

    if (verbose) {
      message(" Integration complete\n")
      for (nutrient in names(integrated_data)) {
        if (paste0("combined_", substr(nutrient, 1, 1), "_class") %in% names(integrated_data[[nutrient]])) {
          combined_col <- paste0("combined_", toupper(substr(nutrient, 1, 1)), "_class")
          combined_classes <- table(integrated_data[[nutrient]][[combined_col]], useNA = "ifany")
          message("  ", nutrient, "combined classes:",
              paste(names(combined_classes), "(", combined_classes, ")", collapse = ", "), "\n")
        }
      }
      message("\n")
    }

  } else {
    if (verbose) message("WWTP analysis skipped\n\n")
  }

  # Save outputs if requested
  if (save_outputs) {
    if (verbose) message("Saving results...\n")

    # Save agricultural results
    agri_file <- save_spatial_data(
      results$agricultural,
      file.path(output_dir, paste0(scale, "_agricultural_", year, ".rds")),
      scale = scale, nutrient = "both", analysis_type = "agricultural", year = year
    )

    if (include_wwtp && "integrated" %in% names(results)) {
      # Save integrated results for each nutrient
      saved_files <- list()
      for (nutrient in nutrients) {
        if (nutrient %in% names(results$integrated)) {
          # Save integrated spatial data
          integrated_file <- save_spatial_data(
            results$integrated[[nutrient]],
            file.path(output_dir, paste0(scale, "_", nutrient, "_integrated_", year, ".rds")),
            scale = scale, nutrient = nutrient, analysis_type = "integrated", year = year
          )
          saved_files[[paste0(nutrient, "_integrated")]] <- integrated_file

          # Save centroid data for transition analysis
          centroids <- add_centroid_coordinates(results$integrated[[nutrient]])
          centroid_file <- save_centroid_data(
            centroids,
            file.path(output_dir, paste0(scale, "_", nutrient, "_centroids_", year, ".csv")),
            scale = scale, nutrient = nutrient, year = year
          )
          saved_files[[paste0(nutrient, "_centroids")]] <- centroid_file
        }
      }
      results$saved_files <- saved_files
    }

    # Save analysis summary
    summary_file <- save_analysis_summary(
      results,
      file.path(output_dir, paste0("analysis_summary_", year, ".rds"))
    )

    if (verbose) {
      message(" Results saved to:", output_dir, "\n")
      message("  Files created:", length(c(agri_file, unlist(results$saved_files), summary_file)), "\n\n")
    }
  }

  # Final summary
  end_time <- Sys.time()
  processing_time <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)

  results$parameters$processing_time_minutes <- processing_time

  if (verbose) {
    message(paste(rep("=", 70), collapse = ""), "\n")
    message("ANALYSIS COMPLETE\n")
    message(paste(rep("=", 70), collapse = ""), "\n")
    message("Processing time:", processing_time, "minutes\n")
    message("Scale:", scale, "\n")
    message("Year:", year, "\n")
    message("Nutrients analyzed:", paste(nutrients, collapse = ", "), "\n")
    message("Spatial units:", nrow(results$agricultural), "\n")
    if (include_wwtp) {
      total_facilities <- sum(sapply(names(results$wwtp), function(n) {
        nrow(results$wwtp[[n]]$facility_data)
      }))
      message("WWTP facilities:", total_facilities, "\n")
    }
    if (save_outputs) {
      message("Output directory:", output_dir, "\n")
    }
    message(paste(rep("=", 70), collapse = ""), "\n")
  }

  return(results)
}

#' Quick Analysis with Visualization
#'
#' Run analysis and automatically generate key visualizations for specified nutrients.
#' This is a convenience function that combines run_builtin_analysis with automatic
#' visualization generation.
#'
#' @param scale Character. Spatial scale: "county", "huc8", or "huc2"
#' @param year Numeric. Year to analyze
#' @param nutrients Character vector. Nutrients to analyze: c("nitrogen", "phosphorus") or subset
#' @param include_wwtp Logical. Whether to include WWTP analysis (default: TRUE)
#' @param output_dir Character. Output directory (default: tempdir())
#' @param create_maps Logical. Whether to create classification maps (default: TRUE)
#' @param create_networks Logical. Whether to create network plots (default: TRUE)
#' @param create_comparisons Logical. Whether to create comparison plots (default: TRUE)
#' @param create_wwtp_maps Logical. Whether to create WWTP facility maps (default: TRUE)
#' @param wwtp_load_units Character. Units for WWTP loads if using custom data (default: "kg")
#' @param map_resolution Character. Map resolution: "low", "medium", "high" (default: "medium")
#' @param generate_report Logical. Whether to generate HTML report (default: FALSE)
#' @param verbose Logical. Whether to print progress messages (default: TRUE)
#' @param ... Additional arguments passed to run_builtin_analysis
#' @return List with results and file paths of created visualizations
#' @export
#' @examples
#' \donttest{
#' # Quick analysis with all visualizations (2007-2016 WWTP available)
#' results <- quick_analysis(
#'   scale = "huc8",
#'   year = 2012,  # Use valid year
#'   nutrients = c("nitrogen", "phosphorus"),
#'   include_wwtp = TRUE,
#'   generate_report = TRUE
#' )
#'
#' # Agricultural only analysis for pre-WWTP year
#' results <- quick_analysis(
#'   scale = "county",
#'   year = 2005,  # Before WWTP data
#'   nutrients = "nitrogen",
#'   include_wwtp = FALSE,
#'   create_networks = FALSE
#' )
#'
#' # High-resolution analysis with expanded year range
#' results <- quick_analysis(
#'   scale = "huc8",
#'   year = 2008,  # Use valid WWTP year
#'   nutrients = "phosphorus",
#'   include_wwtp = TRUE,
#'   map_resolution = "high"
#' )
#' }
quick_analysis <- function(scale = "huc8", year = 2016,
                           nutrients = c("nitrogen", "phosphorus"),
                           include_wwtp = TRUE, output_dir = tempdir(),
                           create_maps = TRUE, create_networks = TRUE,
                           create_comparisons = TRUE, create_wwtp_maps = TRUE,
                           wwtp_load_units = "kg", map_resolution = "medium",
                           generate_report = FALSE, verbose = TRUE, ...) {

  start_time <- Sys.time()

  # Validate nutrients
  if (!all(nutrients %in% c("nitrogen", "phosphorus"))) {
    stop("nutrients must be 'nitrogen', 'phosphorus', or both")
  }

  if (verbose) {
    message("\n", paste(rep("=", 70), collapse = ""), "\n")
    message("QUICK MANURESHED ANALYSIS WITH VISUALIZATION\n")
    message(paste(rep("=", 70), collapse = ""), "\n")
    message("Scale:", scale, "\n")
    message("Year:", year, "\n")
    message("Nutrients:", paste(nutrients, collapse = ", "), "\n")
    message("Visualizations: Maps =", create_maps, ", Networks =", create_networks,
        ", Comparisons =", create_comparisons, "\n")
    message(paste(rep("-", 70), collapse = ""), "\n\n")
  }

  # Run main analysis
  results <- run_builtin_analysis(
    scale = scale, year = year, nutrients = nutrients,
    output_dir = output_dir, include_wwtp = include_wwtp,
    save_outputs = TRUE, wwtp_load_units = wwtp_load_units,
    verbose = verbose, ...
  )

  # Set visualization parameters based on resolution
  viz_params <- switch(map_resolution,
                       "low" = list(width = 8, height = 5, dpi = 150),
                       "medium" = list(width = 11, height = 6, dpi = 300),
                       "high" = list(width = 16, height = 9, dpi = 450),
                       list(width = 11, height = 6, dpi = 300)
  )

  created_files <- list()

  if (verbose) message("Generating visualizations...\n")

  for (nutrient in nutrients) {
    if (verbose) message("  Creating", nutrient, "visualizations...\n")

    if (create_maps) {
      # Agricultural classification map
      agri_class_col <- if (nutrient == "nitrogen") "N_class" else "P_class"
      agri_map <- map_agricultural_classification(
        results$agricultural, nutrient, agri_class_col,
        paste("Agricultural", tools::toTitleCase(nutrient), "Classifications")
      )

      agri_map_file <- file.path(output_dir, paste0("map_agricultural_", nutrient, "_", year, ".png"))
      save_plot(agri_map, agri_map_file,
                width = viz_params$width, height = viz_params$height, dpi = viz_params$dpi)
      created_files[[paste0("agricultural_", nutrient, "_map")]] <- agri_map_file

      # Combined map (if WWTP included and nutrient was processed)
      if (include_wwtp && "integrated" %in% names(results) && nutrient %in% names(results$integrated)) {
        combined_class_col <- if (nutrient == "nitrogen") "combined_N_class" else "combined_P_class"
        combined_map <- map_agricultural_classification(
          results$integrated[[nutrient]], nutrient, combined_class_col,
          paste("Combined Agricultural + WWTP", tools::toTitleCase(nutrient), "Classifications")
        )

        combined_map_file <- file.path(output_dir, paste0("map_combined_", nutrient, "_", year, ".png"))
        save_plot(combined_map, combined_map_file,
                  width = viz_params$width, height = viz_params$height, dpi = viz_params$dpi)
        created_files[[paste0("combined_", nutrient, "_map")]] <- combined_map_file

        # WWTP influence map
        influence_map <- map_wwtp_influence(
          results$integrated[[nutrient]], nutrient,
          paste("WWTP", tools::toTitleCase(nutrient), "Influence")
        )

        influence_map_file <- file.path(output_dir, paste0("map_wwtp_influence_", nutrient, "_", year, ".png"))
        save_plot(influence_map, influence_map_file,
                  width = viz_params$width, height = viz_params$height, dpi = viz_params$dpi)
        created_files[[paste0("wwtp_influence_", nutrient, "_map")]] <- influence_map_file

        # WWTP facility points map
        if (create_wwtp_maps && "wwtp" %in% names(results) && nutrient %in% names(results$wwtp)) {
          facility_map <- map_wwtp_points(
            results$wwtp[[nutrient]]$spatial_data, nutrient,
            paste("WWTP", tools::toTitleCase(nutrient), "Facilities")
          )

          facility_map_file <- file.path(output_dir, paste0("map_wwtp_facilities_", nutrient, "_", year, ".png"))
          save_plot(facility_map, facility_map_file,
                    width = viz_params$width, height = viz_params$height, dpi = viz_params$dpi)
          created_files[[paste0("wwtp_facilities_", nutrient, "_map")]] <- facility_map_file
        }
      }
    }

    if (create_networks && include_wwtp && "integrated" %in% names(results) &&
        nutrient %in% names(results$integrated)) {
      # Create transition probability networks
      centroids <- add_centroid_coordinates(results$integrated[[nutrient]])

      # Agricultural transitions
      agri_class_col <- if (nutrient == "nitrogen") "N_class" else "P_class"
      agri_transitions <- calculate_transition_probabilities(centroids, agri_class_col)

      agri_network_file <- file.path(output_dir, paste0("network_agricultural_", nutrient, "_", year, ".png"))
      create_network_plot(agri_transitions, nutrient,
                          paste("Agricultural", tools::toTitleCase(nutrient)),
                          agri_network_file)
      created_files[[paste0("agricultural_", nutrient, "_network")]] <- agri_network_file

      # Combined transitions
      combined_class_col <- if (nutrient == "nitrogen") "combined_N_class" else "combined_P_class"
      combined_transitions <- calculate_transition_probabilities(centroids, combined_class_col)

      combined_network_file <- file.path(output_dir, paste0("network_combined_", nutrient, "_", year, ".png"))
      create_network_plot(combined_transitions, nutrient,
                          paste("WWTP and Agricultural", tools::toTitleCase(nutrient)),
                          combined_network_file)
      created_files[[paste0("combined_", nutrient, "_network")]] <- combined_network_file

      # Save transition matrices for further analysis
      agri_matrix_file <- file.path(output_dir, paste0("transitions_agricultural_", nutrient, "_", year, ".csv"))
      save_transition_matrix(agri_transitions, agri_matrix_file, nutrient, "agricultural")
      created_files[[paste0("agricultural_", nutrient, "_transitions")]] <- agri_matrix_file

      combined_matrix_file <- file.path(output_dir, paste0("transitions_combined_", nutrient, "_", year, ".csv"))
      save_transition_matrix(combined_transitions, combined_matrix_file, nutrient, "combined")
      created_files[[paste0("combined_", nutrient, "_transitions")]] <- combined_matrix_file
    }

    if (create_comparisons && include_wwtp && "integrated" %in% names(results) &&
        nutrient %in% names(results$integrated)) {
      # Create comparison plots
      agri_col <- if (nutrient == "nitrogen") "N_class" else "P_class"
      combined_col <- if (nutrient == "nitrogen") "combined_N_class" else "combined_P_class"

      summary_data <- create_classification_summary(
        results$integrated[[nutrient]], agri_col, combined_col
      )

      # Before/after comparison
      comparison_plot <- plot_before_after_comparison(
        summary_data, nutrient,
        paste(tools::toTitleCase(nutrient), ": Agricultural vs WWTP + Agricultural")
      )

      comparison_file <- file.path(output_dir, paste0("comparison_", nutrient, "_", year, ".png"))
      save_plot(comparison_plot, comparison_file,
                width = viz_params$width, height = viz_params$height, dpi = viz_params$dpi)
      created_files[[paste0("comparison_", nutrient)]] <- comparison_file

      # Impact ratios
      impact_plot <- plot_impact_ratios(
        summary_data,
        paste("Impact of WWTP Addition on", tools::toTitleCase(nutrient), "Classification")
      )

      impact_file <- file.path(output_dir, paste0("impact_", nutrient, "_", year, ".png"))
      save_plot(impact_plot, impact_file,
                width = viz_params$width, height = viz_params$height, dpi = viz_params$dpi)
      created_files[[paste0("impact_", nutrient)]] <- impact_file

      # Absolute changes
      change_plot <- plot_absolute_changes(
        summary_data,
        paste("Absolute Change in", tools::toTitleCase(nutrient), "Classifications")
      )

      change_file <- file.path(output_dir, paste0("changes_", nutrient, "_", year, ".png"))
      save_plot(change_plot, change_file,
                width = viz_params$width, height = viz_params$height, dpi = viz_params$dpi)
      created_files[[paste0("changes_", nutrient)]] <- change_file

      # Save comparison data
      comparison_data_file <- file.path(output_dir, paste0("comparison_data_", nutrient, "_", year, ".csv"))
      write.csv(summary_data, comparison_data_file, row.names = FALSE)
      created_files[[paste0("comparison_data_", nutrient)]] <- comparison_data_file
    }
  }

  # Generate comprehensive report if requested
  if (generate_report) {
    if (verbose) message("  Generating analysis report...\n")

    report_file <- file.path(output_dir, paste0("analysis_report_", year, ".html"))
    tryCatch({
      create_analysis_report(results, report_file, format = "html",
                             title = paste("Manureshed Analysis Report -", year))
      created_files[["analysis_report"]] <- report_file
    }, error = function(e) {
      if (verbose) {
        warning("Failed to generate report: ", e$message,
                "\nInstall 'rmarkdown' and 'knitr' packages for report generation")
      }
    })
  }

  # Create visualization summary
  viz_summary <- list(
    total_files = length(created_files),
    files_by_type = table(gsub("^[^_]*_", "", names(created_files))),
    files_by_nutrient = table(sapply(strsplit(names(created_files), "_"), function(x) {
      if (length(x) >= 2 && x[2] %in% c("nitrogen", "phosphorus")) x[2] else "general"
    })),
    output_directory = output_dir,
    visualization_parameters = viz_params
  )

  end_time <- Sys.time()
  total_time <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)

  if (verbose) {
    message(" Visualization complete\n")
    message("  Files created:", length(created_files), "\n")
    message("  By type:", paste(names(viz_summary$files_by_type), "(", viz_summary$files_by_type, ")", collapse = ", "), "\n")
    message("  Resolution:", map_resolution, paste0("(", viz_params$width, "x", viz_params$height, " @ ", viz_params$dpi, " DPI)"), "\n")
    message("  Total time:", total_time, "minutes\n\n")

    message(paste(rep("=", 70), collapse = ""), "\n")
    message("QUICK ANALYSIS COMPLETE\n")
    message(paste(rep("=", 70), collapse = ""), "\n")
    message("Analysis + Visualization time:", total_time, "minutes\n")
    message("Output files:", length(created_files) + length(unlist(results$saved_files)), "\n")
    message("Output directory:", output_dir, "\n")
    message("Nutrients analyzed:", paste(nutrients, collapse = ", "), "\n")
    if (generate_report && "analysis_report" %in% names(created_files)) {
      message("Report generated:", basename(created_files[["analysis_report"]]), "\n")
    }
    message(paste(rep("=", 70), collapse = ""), "\n")
  }

  # Add visualization info to results
  results$visualization <- list(
    created_files = created_files,
    summary = viz_summary,
    parameters = list(
      create_maps = create_maps,
      create_networks = create_networks,
      create_comparisons = create_comparisons,
      create_wwtp_maps = create_wwtp_maps,
      map_resolution = map_resolution,
      generate_report = generate_report
    ),
    total_visualization_time_minutes = round(as.numeric(difftime(end_time, start_time, units = "mins")) -
                                               results$parameters$processing_time_minutes, 2)
  )

  return(results)
}

#' Batch Analysis Across Multiple Years
#'
#' Run manureshed analysis across multiple years with consistent parameters
#'
#' @param years Numeric vector. Years to analyze
#' @param scale Character. Spatial scale: "county", "huc8", or "huc2"
#' @param nutrients Character vector. Nutrients to analyze
#' @param include_wwtp Logical. Whether to include WWTP (only available for 2007-2016 built-in)
#' @param output_base_dir Character. Base output directory
#' @param create_comparative_plots Logical. Whether to create year-over-year comparisons
#' @param verbose Logical. Whether to print progress
#' @param ... Additional arguments passed to run_builtin_analysis
#' @return List of results for each year
#' @export
#' @examples
#' \donttest{
#' # Analyze trends with WWTP for subset of supported range
#' batch_results <- batch_analysis_years(
#'   years = 2010:2012,  # Use smaller range for examples
#'   scale = "huc8",
#'   nutrients = "nitrogen",
#'   include_wwtp = TRUE
#' )
#'
#' # Historical analysis without WWTP
#' historical_results <- batch_analysis_years(
#'   years = 1990:1992,  # Use smaller range
#'   scale = "county",
#'   nutrients = c("nitrogen", "phosphorus"),
#'   include_wwtp = FALSE
#' )
#'
#' # Mixed analysis: some years with WWTP, some without
#' mixed_results <- batch_analysis_years(
#'   years = c(2005, 2010, 2015),  # 2010,2015 will have WWTP
#'   scale = "huc8",
#'   nutrients = "nitrogen",
#'   include_wwtp = TRUE  # Will only apply to 2010,2015
#' )
#' }
batch_analysis_years <- function(years, scale = "huc8",
                                 nutrients = c("nitrogen", "phosphorus"),
                                 include_wwtp = TRUE,
                                 output_base_dir = tempdir(),
                                 create_comparative_plots = TRUE,
                                 verbose = TRUE, ...) {

  if (verbose) {
    message("\n", paste(rep("=", 70), collapse = ""), "\n")
    message("BATCH MANURESHED ANALYSIS\n")
    message(paste(rep("=", 70), collapse = ""), "\n")
    message("Years:", paste(range(years), collapse = "-"), "(", length(years), "years)\n")
    message("Scale:", scale, "\n")
    message("Nutrients:", paste(nutrients, collapse = ", "), "\n")
    message(paste(rep("-", 70), collapse = ""), "\n\n")
  }

  # Check data availability
  available_data <- check_builtin_data()
  available_years <- available_data$nugis_years[[scale]]

  # Validate years
  invalid_years <- setdiff(years, available_years)
  if (length(invalid_years) > 0) {
    stop("Years not available for ", scale, " scale: ", paste(invalid_years, collapse = ", "),
         "\nAvailable years: ", paste(range(available_years), collapse = "-"))
  }

  # Create base output directory
  if (!dir.exists(output_base_dir)) {
    dir.create(output_base_dir, recursive = TRUE)
  }

  batch_results <- list()
  year_summaries <- list()

  # Process each year
  for (year in years) {
    if (verbose) {
      message("Processing year", year, "...\n")
    }

    year_output_dir <- file.path(output_base_dir, paste0("year_", year))

    # Determine if WWTP should be included (only for 2007-2016 built-in)
    use_wwtp <- include_wwtp && (year %in% 2007:2016) && available_data$wwtp_available

    if (include_wwtp && !year %in% 2007:2016) {
      if (verbose) {
        message("  Note: WWTP analysis skipped for", year, "(built-in data only available for 2007-2016)\n")
      }
    }
    # Run analysis for this year
    tryCatch({
      year_results <- run_builtin_analysis(
        scale = scale,
        year = year,
        nutrients = nutrients,
        output_dir = year_output_dir,
        include_wwtp = use_wwtp,
        verbose = FALSE,  # Suppress individual year verbosity
        ...
      )

      batch_results[[as.character(year)]] <- year_results

      # Extract summary for comparative analysis
      year_summary <- list(
        year = year,
        n_units = nrow(year_results$agricultural),
        include_wwtp = use_wwtp
      )

      # Add classification summaries for each nutrient
      for (nutrient in nutrients) {
        class_col <- if (nutrient == "nitrogen") "N_class" else "P_class"
        if (class_col %in% names(year_results$agricultural)) {
          year_summary[[paste0(nutrient, "_classes")]] <- table(year_results$agricultural[[class_col]], useNA = "ifany")
        }
      }

      year_summaries[[as.character(year)]] <- year_summary

      if (verbose) {
        message("  Year", year, "complete (", nrow(year_results$agricultural), " units)\n")
      }

    }, error = function(e) {
      if (verbose) {
        message("  Year", year, "failed:", e$message, "\n")
      }
      batch_results[[as.character(year)]] <- NULL
    })
  }

  # Create comparative visualizations if requested
  if (create_comparative_plots && length(batch_results) > 1) {
    if (verbose) message("\nCreating comparative visualizations...\n")

    comparative_plots <- create_year_comparison_plots(
      year_summaries, nutrients, output_base_dir, verbose
    )
  }

  # Create batch summary
  batch_summary <- list(
    years_processed = names(batch_results),
    years_failed = setdiff(as.character(years), names(batch_results)),
    parameters = list(
      scale = scale,
      nutrients = nutrients,
      include_wwtp = include_wwtp,
      total_years = length(years),
      successful_years = length(batch_results)
    ),
    processing_timestamp = Sys.time(),
    year_summaries = year_summaries
  )

  # Save batch summary
  batch_summary_file <- file.path(output_base_dir, "batch_summary.rds")
  saveRDS(batch_summary, batch_summary_file)

  if (verbose) {
    message("\n", paste(rep("=", 70), collapse = ""), "\n")
    message("BATCH ANALYSIS COMPLETE\n")
    message(paste(rep("=", 70), collapse = ""), "\n")
    message("Years processed:", length(batch_results), "/", length(years), "\n")
    message("Scale:", scale, "\n")
    message("Output directory:", output_base_dir, "\n")
    if (create_comparative_plots && length(batch_results) > 1) {
      message("Comparative plots: Created\n")
    }
    message("Batch summary:", basename(batch_summary_file), "\n")
    message(paste(rep("=", 70), collapse = ""), "\n")
  }

  return(list(
    results = batch_results,
    summary = batch_summary,
    comparative_plots = if (exists("comparative_plots")) comparative_plots else NULL
  ))
}

#' Create Year-over-Year Comparison Plots
#'
#' Internal function to create comparative visualizations across years
#'
#' @param year_summaries List. Summary data for each year
#' @param nutrients Character vector. Nutrients to analyze
#' @param output_dir Character. Output directory
#' @param verbose Logical. Progress messages
#' @return List of created plot files
#' @keywords internal
create_year_comparison_plots <- function(year_summaries, nutrients, output_dir, verbose) {

  created_plots <- list()

  # Extract years and create trend data
  years <- as.numeric(names(year_summaries))

  for (nutrient in nutrients) {
    class_col <- paste0(nutrient, "_classes")

    # Extract classification data across years
    trend_data <- list()
    all_classes <- c()

    for (year in names(year_summaries)) {
      if (class_col %in% names(year_summaries[[year]])) {
        year_classes <- year_summaries[[year]][[class_col]]
        trend_data[[year]] <- year_classes
        all_classes <- union(all_classes, names(year_classes))
      }
    }

    if (length(trend_data) > 1) {
      # Create trend plot
      trend_df <- data.frame()
      for (year in names(trend_data)) {
        year_data <- trend_data[[year]]
        for (class in all_classes) {
          count <- if (class %in% names(year_data)) year_data[[class]] else 0
          trend_df <- rbind(trend_df, data.frame(
            Year = as.numeric(year),
            Classification = class,
            Count = count,
            Nutrient = nutrient
          ))
        }
      }

      # Create the plot
      trend_plot <- ggplot2::ggplot(trend_df, ggplot2::aes(x = Year, y = Count, color = Classification)) +
        ggplot2::geom_line(size = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::scale_color_manual(values = get_nutrient_colors(nutrient)) +
        ggplot2::scale_x_continuous(breaks = years) +
        ggplot2::labs(
          title = paste(tools::toTitleCase(nutrient), "Classification Trends Over Time"),
          subtitle = paste("Years:", paste(range(years), collapse = " - ")),
          x = "Year",
          y = "Number of Spatial Units",
          color = "Classification"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold"),
          legend.position = "bottom"
        )

      # Save the plot
      trend_file <- file.path(output_dir, paste0("trend_", nutrient, "_", min(years), "_", max(years), ".png"))
      save_plot(trend_plot, trend_file, width = 12, height = 8)
      created_plots[[paste0(nutrient, "_trend")]] <- trend_file

      if (verbose) {
        message("  Created", nutrient, "trend plot\n")
      }
    }
  }

  return(created_plots)
}

#' Enhanced Batch Analysis with Full Visualizations
#'
#' Run batch analysis with comprehensive visualization output for each year
#'
#' @param years Numeric vector. Years to analyze
#' @param scale Character. Spatial scale
#' @param nutrients Character vector. Nutrients to analyze
#' @param include_wwtp Logical. Include WWTP analysis
#' @param output_base_dir Character. Base output directory
#' @param create_all_visualizations Logical. Create all maps, networks, and comparisons
#' @param create_comparative_plots Logical. Create year-over-year comparisons
#' @param show_progress Logical. Display progress bar (requires 'progress' package)
#' @param verbose Logical. Show progress
#' @param ... Additional arguments
#' @return List of results with comprehensive outputs
#' @export
#' @examples
#' \dontrun{
#' # This function is computationally intensive
#' # See vignette("advanced-features") for examples
#' results <- batch_analysis_enhanced(years = 2015:2016)
#' }
batch_analysis_enhanced <- function(years, scale = "huc8",
                                    nutrients = c("nitrogen", "phosphorus"),
                                    include_wwtp = TRUE,
                                    output_base_dir = tempdir(),
                                    create_all_visualizations = TRUE,
                                    create_comparative_plots = TRUE,
                                    show_progress = TRUE,
                                    verbose = TRUE, ...) {

  # Progress bar setup
  if (show_progress && length(years) > 1) {
    if (requireNamespace("progress", quietly = TRUE)) {
      pb <- progress::progress_bar$new(
        format = "  Processing [:bar] :percent | Year :current/:total | ETA: :eta",
        total = length(years),
        clear = FALSE,
        width = 80
      )
      use_progress <- TRUE
    } else {
      message("Install 'progress' package for progress bars: install.packages('progress')")
      use_progress <- FALSE
    }
  } else {
    use_progress <- FALSE
  }

  if (verbose) {
    message("\n", paste(rep("=", 70), collapse = ""), "\n")
    message("ENHANCED BATCH MANURESHED ANALYSIS\n")
    message(paste(rep("=", 70), collapse = ""), "\n")
    message("Years:", paste(range(years), collapse = "-"), "(", length(years), "years)\n")
    message("Full visualizations:", create_all_visualizations, "\n")
    message(paste(rep("-", 70), collapse = ""), "\n\n")
  }

  # Create base output directory
  if (!dir.exists(output_base_dir)) {
    dir.create(output_base_dir, recursive = TRUE)
  }

  batch_results <- list()
  all_viz_files <- list()

  for (i in seq_along(years)) {
    year <- years[i]

    if (use_progress) pb$tick(tokens = list(current = i, total = length(years)))
    if (verbose) {
      message("\n", paste(rep("-", 50), collapse = ""), "\n")
      message("Processing year", year, "...\n")
      message(paste(rep("-", 50), collapse = ""), "\n")
    }

    year_output_dir <- file.path(output_base_dir, paste0("year_", year))

    tryCatch({
      if (create_all_visualizations) {
        # Use quick_analysis for full visualization suite
        year_results <- quick_analysis(
          scale = scale,
          year = year,
          nutrients = nutrients,
          include_wwtp = include_wwtp,
          output_dir = year_output_dir,
          create_maps = TRUE,
          create_networks = TRUE,
          create_comparisons = TRUE,
          create_wwtp_maps = TRUE,
          verbose = FALSE,
          ...
        )
      } else {
        # Standard analysis without visualizations
        year_results <- run_builtin_analysis(
          scale = scale,
          year = year,
          nutrients = nutrients,
          include_wwtp = include_wwtp,
          output_dir = year_output_dir,
          verbose = FALSE,
          ...
        )
      }

      batch_results[[as.character(year)]] <- year_results

      # Collect visualization files
      if ("visualization" %in% names(year_results)) {
        all_viz_files[[as.character(year)]] <- year_results$visualization$created_files
      }

      if (verbose) {
        message("Year", year, "complete\n")
        if (create_all_visualizations && "visualization" %in% names(year_results)) {
          message("  Visualizations created:",
              length(year_results$visualization$created_files), "\n")
        }
      }

    }, error = function(e) {
      if (verbose) {
        message("Year", year, "failed:", e$message, "\n")
      }
    })
  }

  # Create comprehensive comparative visualizations
  if (create_comparative_plots && length(batch_results) > 1) {
    if (verbose) {
      message("\n", paste(rep("-", 50), collapse = ""), "\n")
      message("Creating comparative visualizations...\n")
      message(paste(rep("-", 50), collapse = ""), "\n")
    }

    comparative_files <- create_comprehensive_comparisons(
      batch_results, nutrients, output_base_dir, verbose
    )
    all_viz_files$comparative <- comparative_files
  }

  # Create batch summary report
  batch_summary <- create_batch_summary_report(
    batch_results, years, nutrients, output_base_dir, verbose
  )

  if (verbose) {
    message("\n", paste(rep("=", 70), collapse = ""), "\n")
    message("ENHANCED BATCH ANALYSIS COMPLETE\n")
    message(paste(rep("=", 70), collapse = ""), "\n")
    message("Total years processed:", length(batch_results), "\n")
    message("Total visualizations:", sum(sapply(all_viz_files, length)), "\n")
    message(paste(rep("=", 70), collapse = ""), "\n")
  }

  return(list(
    results = batch_results,
    visualizations = all_viz_files,
    summary = batch_summary
  ))
}

#' Batch Analysis with Parallel Processing
#'
#' Run batch analysis using multiple cores for faster processing
#'
#' @param years Numeric vector. Years to analyze
#' @param n_cores Integer. Number of cores (default: detectCores() - 1)
#' @param ... Arguments passed to run_builtin_analysis
#' @return List of results
#' @export
#' @examples
#' \donttest{
#' results <- batch_analysis_parallel(
#'   years = 2015:2016,  # Use valid years only
#'   n_cores = 2,        # Max 2 cores for CRAN
#'   scale = "county",   # Use county for faster processing
#'   nutrients = "nitrogen"
#' )
#' }
batch_analysis_parallel <- function(years, n_cores = NULL, ...) {

  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package 'parallel' required for parallel processing")
  }

  # Determine number of cores
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
  }

  # CRAN safety: limit to 2 cores max for package checks
  max_cores <- min(2, parallel::detectCores() - 1)
  if (n_cores > max_cores) {
    message("Limiting cores to ", max_cores, " for CRAN compatibility")
    n_cores <- max_cores
  }

  message("Starting parallel batch analysis with ", n_cores, " cores")
  message("Processing ", length(years), " years")

  # Create cluster
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Export necessary objects to cluster
  parallel::clusterEvalQ(cl, {
    library(manureshed)
  })

  # Process years in parallel
  results <- parallel::parLapply(cl, years, function(year) {
    tryCatch({
      run_builtin_analysis(
        year = year,
        save_outputs = TRUE,
        verbose = FALSE,
        ...
      )
    }, error = function(e) {
      list(error = e$message, year = year)
    })
  })

  names(results) <- as.character(years)

  # Count successes and failures
  n_success <- sum(!sapply(results, function(x) "error" %in% names(x)))
  n_failed <- length(years) - n_success

  message("\nParallel processing complete:")
  message("  Successful: ", n_success, "/", length(years))
  if (n_failed > 0) {
    failed_years <- years[sapply(results, function(x) "error" %in% names(x))]
    message("  Failed years: ", paste(failed_years, collapse = ", "))
  }

  return(results)
}






#' Create Comprehensive Batch Comparisons
#'
#' Generate detailed comparative visualizations across years
#'
#' @param batch_results List of analysis results by year
#' @param nutrients Character vector of nutrients
#' @param output_dir Character output directory
#' @param verbose Logical show progress
#' @return List of created comparison files
#' @keywords internal
create_comprehensive_comparisons <- function(batch_results, nutrients,
                                             output_dir, verbose) {

  created_files <- list()
  years <- as.numeric(names(batch_results))

  # Check if WWTP is included by looking at first result
  has_wwtp <- "integrated" %in% names(batch_results[[1]])

  for (nutrient in nutrients) {
    # Extract data across years
    trend_data <- extract_trend_data(batch_results, nutrient)

    if (nrow(trend_data) > 0) {
      # 1. Classification trends over time
      trend_plot <- create_classification_trend_plot(trend_data, nutrient)
      trend_file <- file.path(output_dir,
                              paste0("comparison_trend_", nutrient, "_",
                                     min(years), "_", max(years), ".png"))
      save_plot(trend_plot, trend_file, width = 12, height = 8)
      created_files[[paste0(nutrient, "_trend")]] <- trend_file

      # 2. Stacked area chart
      stacked_plot <- create_stacked_area_plot(trend_data, nutrient)
      stacked_file <- file.path(output_dir,
                                paste0("comparison_stacked_", nutrient, "_",
                                       min(years), "_", max(years), ".png"))
      save_plot(stacked_plot, stacked_file, width = 12, height = 8)
      created_files[[paste0(nutrient, "_stacked")]] <- stacked_file

      # 3. Year-over-year change analysis (only if WWTP included)
      if (has_wwtp && length(years) > 1) {
        change_plot <- create_yoy_change_plot(trend_data, nutrient)
        change_file <- file.path(output_dir,
                                 paste0("comparison_yoy_change_", nutrient, "_",
                                        min(years), "_", max(years), ".png"))
        save_plot(change_plot, change_file, width = 12, height = 8)
        created_files[[paste0(nutrient, "_yoy_change")]] <- change_file
      }

      if (verbose) {
        message("  Created", nutrient, "comparison plots\n")
      }
    }
  }

  # 4. Summary statistics table
  summary_table <- create_batch_summary_table(batch_results, nutrients)
  summary_file <- file.path(output_dir, "batch_summary_statistics.csv")
  write.csv(summary_table, summary_file, row.names = FALSE)
  created_files$summary_table <- summary_file

  return(created_files)
}


create_batch_summary_table <- function(batch_results, nutrients) {

  summary_data <- data.frame()

  for (year in names(batch_results)) {
    result <- batch_results[[year]]

    year_row <- data.frame(
      Year = as.numeric(year),
      Total_Units = nrow(result$agricultural),
      stringsAsFactors = FALSE
    )

    for (nutrient in nutrients) {
      agri_col <- if (nutrient == "nitrogen") "N_class" else "P_class"

      if (agri_col %in% names(result$agricultural)) {
        # Count each classification
        classes <- table(result$agricultural[[agri_col]])

        for (class_name in names(classes)) {
          col_name <- paste0(nutrient, "_", gsub("_", "", class_name))
          year_row[[col_name]] <- as.numeric(classes[class_name])
        }
      }
    }

    summary_data <- rbind(summary_data, year_row)
  }

  return(summary_data)
}











#' Extract Trend Data from Batch Results
#'
#' @param batch_results List of results
#' @param nutrient Character nutrient type
#' @return Data frame with trend data
#' @keywords internal
extract_trend_data <- function(batch_results, nutrient) {

  trend_df <- data.frame()

  for (year in names(batch_results)) {
    result <- batch_results[[year]]

    # Get agricultural classifications
    agri_col <- if (nutrient == "nitrogen") "N_class" else "P_class"
    if (agri_col %in% names(result$agricultural)) {
      agri_counts <- table(result$agricultural[[agri_col]])

      for (class in names(agri_counts)) {
        trend_df <- rbind(trend_df, data.frame(
          Year = as.numeric(year),
          Classification = class,
          Count = as.numeric(agri_counts[class]),
          Type = "Agricultural",
          Nutrient = nutrient
        ))
      }
    }

    # Get combined classifications if available
    if ("integrated" %in% names(result) && nutrient %in% names(result$integrated)) {
      combined_col <- if (nutrient == "nitrogen") "combined_N_class" else "combined_P_class"
      if (combined_col %in% names(result$integrated[[nutrient]])) {
        combined_counts <- table(result$integrated[[nutrient]][[combined_col]])

        for (class in names(combined_counts)) {
          trend_df <- rbind(trend_df, data.frame(
            Year = as.numeric(year),
            Classification = class,
            Count = as.numeric(combined_counts[class]),
            Type = "WWTP_Combined",
            Nutrient = nutrient
          ))
        }
      }
    }
  }

  return(trend_df)
}

#' Create Batch Summary Report
#'
#' Generate comprehensive summary report for batch analysis
#'
#' @param batch_results List of results
#' @param years Vector of years
#' @param nutrients Vector of nutrients
#' @param output_dir Output directory
#' @param verbose Show progress
#' @return Summary data frame
#' @keywords internal
create_batch_summary_report <- function(batch_results, years, nutrients,
                                        output_dir, verbose) {

  summary_data <- data.frame()

  for (year in names(batch_results)) {
    result <- batch_results[[year]]

    for (nutrient in nutrients) {
      year_summary <- data.frame(
        Year = as.numeric(year),
        Nutrient = nutrient,
        Total_Units = nrow(result$agricultural),
        WWTP_Included = "integrated" %in% names(result)
      )

      # Add classification counts
      agri_col <- if (nutrient == "nitrogen") "N_class" else "P_class"
      if (agri_col %in% names(result$agricultural)) {
        counts <- table(result$agricultural[[agri_col]])
        for (class in names(counts)) {
          year_summary[[paste0("Agri_", gsub("_", "", class))]] <- as.numeric(counts[class])
        }
      }

      # Add combined counts if available
      if ("integrated" %in% names(result) && nutrient %in% names(result$integrated)) {
        combined_col <- if (nutrient == "nitrogen") "combined_N_class" else "combined_P_class"
        if (combined_col %in% names(result$integrated[[nutrient]])) {
          counts <- table(result$integrated[[nutrient]][[combined_col]])
          for (class in names(counts)) {
            year_summary[[paste0("Combined_", gsub("_", "", class))]] <- as.numeric(counts[class])
          }
        }

        # Add WWTP statistics
        if ("wwtp" %in% names(result) && nutrient %in% names(result$wwtp)) {
          year_summary$WWTP_Facilities <- nrow(result$wwtp[[nutrient]]$facility_data)
          load_col <- if (nutrient == "nitrogen") "N_Load_tons" else "P_Load_tons"
          year_summary$WWTP_Total_Load <- sum(
            result$wwtp[[nutrient]]$facility_data[[load_col]], na.rm = TRUE
          )
        }
      }

      summary_data <- rbind(summary_data, year_summary)
    }
  }

  # Save summary report
  summary_file <- file.path(output_dir, "batch_analysis_summary.csv")
  write.csv(summary_data, summary_file, row.names = FALSE)

  if (verbose) {
    message("  Created batch summary report:", basename(summary_file), "\n")
  }

  return(summary_data)
}

#' Create Classification Trend Plot
#'
#' @param trend_data Data frame with trend data
#' @param nutrient Character nutrient type
#' @return ggplot object
#' @keywords internal
create_classification_trend_plot <- function(trend_data, nutrient) {

  colors <- get_nutrient_colors(nutrient)

  plot_data <- trend_data %>%
    dplyr::filter(Type == "Agricultural") %>%
    dplyr::filter(Classification != "Excluded")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = Count,
                                          color = Classification,
                                          group = Classification)) +
    ggplot2::geom_line(linewidth = 1.2) +  # Changed from size to linewidth
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_manual(values = colors,
                                labels = clean_category_names) +
    ggplot2::scale_x_continuous(breaks = unique(plot_data$Year)) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title = paste(tools::toTitleCase(nutrient), "Classification Trends"),
      subtitle = paste("Years:", min(plot_data$Year), "-", max(plot_data$Year)),
      x = "Year",
      y = "Number of Spatial Units",
      color = "Classification"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 11)
    )
}

#' Create Stacked Area Plot
#'
#' @param trend_data Data frame with trend data
#' @param nutrient Character nutrient type
#' @return ggplot object
#' @keywords internal
create_stacked_area_plot <- function(trend_data, nutrient) {

  colors <- get_nutrient_colors(nutrient)

  plot_data <- trend_data %>%
    dplyr::filter(Type == "Agricultural") %>%
    dplyr::filter(Classification != "Excluded")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = Count,
                                          fill = Classification)) +
    ggplot2::geom_area(alpha = 0.8, position = "stack") +
    ggplot2::scale_fill_manual(values = colors,
                               labels = clean_category_names) +
    ggplot2::scale_x_continuous(breaks = unique(plot_data$Year)) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title = paste(tools::toTitleCase(nutrient), "Classification Distribution Over Time"),
      subtitle = "Stacked Area Chart",
      x = "Year",
      y = "Number of Spatial Units",
      fill = "Classification"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 11)
    )
}

#' Create Year-over-Year Change Plot
#'
#' @param trend_data Data frame with trend data
#' @param nutrient Character nutrient type
#' @return ggplot object
#' @keywords internal
create_yoy_change_plot <- function(trend_data, nutrient) {

  # Calculate year-over-year changes
  change_data <- trend_data %>%
    dplyr::filter(Classification != "Excluded") %>%
    dplyr::arrange(Year, Classification, Type) %>%
    dplyr::group_by(Classification, Type) %>%
    dplyr::mutate(
      YoY_Change = Count - dplyr::lag(Count),
      YoY_Percent = (Count - dplyr::lag(Count)) / dplyr::lag(Count) * 100
    ) %>%
    dplyr::filter(!is.na(YoY_Change))

  ggplot2::ggplot(change_data, ggplot2::aes(x = Year, y = YoY_Percent,
                                            fill = Classification)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::facet_wrap(~Type, ncol = 1) +
    ggplot2::scale_fill_manual(values = get_nutrient_colors(nutrient),
                               labels = clean_category_names) +
    ggplot2::labs(
      title = paste(tools::toTitleCase(nutrient), "Year-over-Year Classification Changes"),
      x = "Year",
      y = "Percent Change (%)",
      fill = "Classification"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "bottom"
    )
}
