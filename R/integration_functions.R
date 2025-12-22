#' Load Texas HUC8 Data from OSF
#'
#' Load Texas HUC8 supplemental data from OSF repository
#'
#' @param year Numeric. Year to extract from Texas data
#' @param verbose Logical. Show loading progress
#' @return Data frame with Texas HUC8 data for specified year
#' @keywords internal
load_texas_huc8_data <- function(year = 2016, verbose = FALSE) {

  # Download/load Texas data from OSF
  tryCatch({
    data_file <- download_osf_data("texas_huc8_data", verbose = verbose)

    # Load the .rda file
    load_env <- new.env()
    load(data_file, envir = load_env)

    # Extract the data object
    if (exists("texas_huc8_data", envir = load_env)) {
      tex_data <- get("texas_huc8_data", envir = load_env)
    } else {
      objects <- ls(load_env)
      if (length(objects) == 1) {
        tex_data <- get(objects[1], envir = load_env)
      } else {
        stop("Could not find Texas HUC8 data in downloaded file")
      }
    }

    # Filter for specified year
    if ("Year" %in% names(tex_data)) {
      tex_data_year <- tex_data[tex_data$Year == year, ]

      if (nrow(tex_data_year) == 0) {
        warning("No Texas data found for year ", year)
        return(NULL)
      }

      return(tex_data_year)
    } else {
      warning("Year column not found in Texas data")
      return(tex_data)
    }

  }, error = function(e) {
    warning("Failed to load Texas HUC8 data: ", e$message)
    return(NULL)
  })
}

#' Load Texas HUC8 Boundaries from OSF
#'
#' Load Texas HUC8 spatial boundaries from OSF repository
#'
#' @param verbose Logical. Show loading progress
#' @return sf object with Texas HUC8 boundaries
#' @keywords internal
load_texas_huc8_boundaries <- function(verbose = FALSE) {

  tryCatch({
    data_file <- download_osf_data("texas_huc8_boundaries", verbose = verbose)

    # Load the .rda file
    load_env <- new.env()
    load(data_file, envir = load_env)

    # Extract the data object
    if (exists("texas_huc8_boundaries", envir = load_env)) {
      tex_boundaries <- get("texas_huc8_boundaries", envir = load_env)
    } else {
      objects <- ls(load_env)
      if (length(objects) == 1) {
        tex_boundaries <- get(objects[1], envir = load_env)
      } else {
        stop("Could not find Texas HUC8 boundaries in downloaded file")
      }
    }

    # Ensure proper CRS
    tex_boundaries <- sf::st_transform(tex_boundaries, crs = MANURESHED_CRS)

    return(tex_boundaries)

  }, error = function(e) {
    warning("Failed to load Texas HUC8 boundaries: ", e$message)
    return(NULL)
  })
}

#' Add Texas HUC8 Data (Updated for OSF)
#'
#' Add manually supplied Texas HUC8 data for missing watersheds
#' Uses OSF data loading instead of built-in data
#'
#' @param huc8_data sf object. Existing HUC8 agricultural data
#' @param year Numeric. Year to extract from Texas data
#' @param cropland_threshold Numeric. Threshold for classification
#' @param verbose Logical. Show progress messages
#' @return sf object with Texas data added
#' @export
add_texas_huc8 <- function(huc8_data, year = 2016, cropland_threshold, verbose = TRUE) {

  if (verbose) {
    message("Attempting to add Texas HUC8 data for year ", year, "...")
  }

  # Load Texas data from OSF
  tex_data_year <- load_texas_huc8_data(year, verbose = verbose)

  if (is.null(tex_data_year)) {
    if (verbose) {
      warning("No Texas HUC8 data available for year ", year)
    }
    return(huc8_data)
  }

  if (verbose) {
    message("Processing Texas HUC8 data (", nrow(tex_data_year), " watersheds)...")
  }

  # Process Texas data to match standard format
  tex_processed <- data.frame(
    ID = format_huc8(tex_data_year$HUC8),         # Your column: HUC8
    NAME = tex_data_year$HUC.Name,                # Your column: HUC.Name
    manure_N = tex_data_year$manure_N,            # Already standardized
    manure_P = tex_data_year$manure_P2O5 * P2O5_TO_P,  # Convert P2O5 to P
    fertilizer_N = tex_data_year$fertilizer_N,    # Already standardized
    fertilizer_P = tex_data_year$fertilizer_P2O5 * P2O5_TO_P,  # Convert P2O5 to P
    N_fixation = tex_data_year$N_fixation,        # Already standardized
    crop_removal_N = tex_data_year$crop_removal_N,    # Already standardized
    crop_removal_P = tex_data_year$crop_removal_P2O5 * P2O5_TO_P,  # Convert P2O5 to P
    cropland = tex_data_year$cropland,            # Already standardized
    stringsAsFactors = FALSE
  )

  # Apply classifications
  tex_classified <- tex_processed %>%
    agri_classify_nitrogen(cropland_threshold, "huc8") %>%
    agri_classify_phosphorus(cropland_threshold, "huc8")

  # Load Texas spatial boundaries from OSF
  tex_spatial <- load_texas_huc8_boundaries(verbose = verbose)

  if (!is.null(tex_spatial)) {
    # Join with processed data
    tex_final <- tex_classified %>%
      dplyr::left_join(tex_spatial, by = c("ID" = "huc8")) %>%
      sf::st_as_sf()

    # Bind with existing data
    combined_data <- dplyr::bind_rows(huc8_data, tex_final)

    if (verbose) {
      message("Successfully added Texas HUC8 data with spatial boundaries")
      message("Total watersheds: ", nrow(combined_data), " (added ", nrow(tex_final), " from Texas)")
    }

  } else {
    if (verbose) {
      warning("Texas spatial boundaries not available - adding data without geometries")
    }

    # Add data without spatial component
    tex_final <- tex_classified

    # For non-spatial joining, we need to be more careful
    combined_data <- dplyr::bind_rows(
      huc8_data %>% sf::st_drop_geometry(),
      tex_final
    ) %>%
      dplyr::left_join(
        huc8_data %>% dplyr::select(ID, geometry),
        by = "ID"
      ) %>%
      sf::st_as_sf()
  }

  return(combined_data)
}

#' Integrate WWTP Data with Agricultural Classifications (Updated)
#'
#' Combine WWTP loads with agricultural nutrient balance classifications
#'
#' @param agri_data Data frame. Agricultural classification data
#' @param wwtp_aggregated Data frame. Aggregated WWTP loads by spatial unit
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @param cropland_threshold Numeric. Threshold for exclusion classification
#' @param scale Character. Spatial scale for within-unit classification
#' @return Data frame with combined WWTP + agricultural classifications
#' @export
integrate_wwtp_agricultural <- function(agri_data, wwtp_aggregated, nutrient,
                                        cropland_threshold, scale = "huc8") {

  if (!nutrient %in% c("nitrogen", "phosphorus")) {
    stop("Nutrient must be 'nitrogen' or 'phosphorus'")
  }

  message("Integrating WWTP ", nutrient, " data with agricultural classifications...")

  # Join WWTP data with agricultural data
  # Determine the correct ID column in agricultural data
  agri_id_col <- if ("FIPS" %in% names(agri_data)) {
    "FIPS"
  } else if ("ID" %in% names(agri_data)) {
    "ID"
  } else {
    # Look for other possible ID columns
    possible_cols <- c("huc8", "huc2", "HUC_8", "HUC_2")
    match_col <- intersect(possible_cols, names(agri_data))
    if (length(match_col) > 0) {
      match_col[1]
    } else {
      stop("No suitable ID column found in agricultural data")
    }
  }

  message("Using agricultural ID column: ", agri_id_col)

  # Join WWTP data with agricultural data using correct column names
  combined_data <- agri_data %>%
    dplyr::left_join(wwtp_aggregated, by = setNames("ID", agri_id_col)) %>%
    dplyr::mutate(
      # Replace NA WWTP loads with 0
      wwtp_n_load = dplyr::if_else(is.na(wwtp_n_load), 0, wwtp_n_load),
      wwtp_p_load = dplyr::if_else(is.na(wwtp_p_load), 0, wwtp_p_load),
      wwtp_count = dplyr::if_else(is.na(wwtp_count), 0L, wwtp_count)
    )

  if (nutrient == "nitrogen") {
    combined_data <- combined_data %>%
      dplyr::mutate(
        # Combined surplus (WWTP adds to manure with 0.5 factor)
        combined_N_surplus = 0.5 * (manure_N + wwtp_n_load) -
          (crop_removal_N - N_fixation),

        # New classification including WWTP
        combined_N_class = dplyr::case_when(
          cropland < cropland_threshold ~ "Excluded",
          combined_N_surplus > 0 ~ "Source",
          0.5 * (manure_N + wwtp_n_load + fertilizer_N) -
            (crop_removal_N - N_fixation) < 0 ~ "Sink_Deficit",
          0.5 * fertilizer_N - (crop_removal_N - N_fixation) > 0 ~ "Sink_Fertilizer",
          TRUE ~ dplyr::if_else(scale == "county", "Within_County", "Within_Watershed")
        ),

        # Calculate WWTP contribution proportion
        wwtp_proportion_N = dplyr::if_else(
          (wwtp_n_load + manure_N) > 0,
          wwtp_n_load / (wwtp_n_load + manure_N),
          0
        )
      )
  } else if (nutrient == "phosphorus") {
    combined_data <- combined_data %>%
      dplyr::mutate(
        # Combined surplus (WWTP adds to manure, no 0.5 factor for P)
        combined_P_surplus = (manure_P + wwtp_p_load) - crop_removal_P,

        # New classification including WWTP
        combined_P_class = dplyr::case_when(
          cropland < cropland_threshold ~ "Excluded",
          combined_P_surplus > 0 ~ "Source",
          (manure_P + wwtp_p_load + fertilizer_P - crop_removal_P) < 0 ~ "Sink_Deficit",
          (fertilizer_P - crop_removal_P) > 0 ~ "Sink_Fertilizer",
          TRUE ~ dplyr::if_else(scale == "county", "Within_County", "Within_Watershed")

        ),

        # Calculate WWTP contribution proportion
        wwtp_proportion_P = dplyr::if_else(
          (wwtp_p_load + manure_P) > 0,
          wwtp_p_load / (wwtp_p_load + manure_P),
          0
        )
      )
  }

  # Summary of integration
  combined_col <- if (nutrient == "nitrogen") "combined_N_class" else "combined_P_class"
  class_summary <- table(combined_data[[combined_col]])
  message("Combined ", nutrient, " classification summary:")
  for (i in 1:length(class_summary)) {
    message("  ", names(class_summary)[i], ": ", class_summary[i], " units")
  }

  return(combined_data)
}

#' Complete Integration Pipeline (Updated)
#'
#' Run complete integration of WWTP and agricultural data for both nutrients
#'
#' @param agri_data Data frame or sf object. Agricultural classification data
#' @param wwtp_nitrogen_aggregated Data frame. Aggregated nitrogen WWTP data
#' @param wwtp_phosphorus_aggregated Data frame. Aggregated phosphorus WWTP data
#' @param cropland_threshold Numeric. Threshold for exclusion
#' @param scale Character. Spatial scale
#' @param add_texas Logical. Whether to add Texas HUC8 data (only for HUC8 scale)
#' @param year Numeric. Year for Texas data
#' @return List with integrated nitrogen and phosphorus data
#' @export
integrate_complete <- function(agri_data, wwtp_nitrogen_aggregated,
                               wwtp_phosphorus_aggregated, cropland_threshold,
                               scale = "huc8", add_texas = FALSE, year = 2016) {

  message("Starting complete integration pipeline...")

  # Add Texas data if requested and scale is HUC8
  if (add_texas && scale == "huc8") {
    message("Adding Texas HUC8 data...")
    agri_data <- add_texas_huc8(agri_data, year, cropland_threshold, verbose = TRUE)
  }

  # Integrate nitrogen data
  nitrogen_integrated <- integrate_wwtp_agricultural(
    agri_data, wwtp_nitrogen_aggregated, "nitrogen", cropland_threshold, scale
  )

  # Integrate phosphorus data
  phosphorus_integrated <- integrate_wwtp_agricultural(
    agri_data, wwtp_phosphorus_aggregated, "phosphorus", cropland_threshold, scale
  )

  message("Integration pipeline complete!")

  return(list(
    nitrogen = nitrogen_integrated,
    phosphorus = phosphorus_integrated
  ))
}
