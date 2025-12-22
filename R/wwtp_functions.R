# ==============================================================================
# R/wwtp_functions.R - WWTP Processing Functions (OSF Version)
# ==============================================================================

#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select left_join right_join group_by summarise case_when if_else bind_rows
#' @importFrom rlang sym :=
#' @importFrom sf st_as_sf st_transform st_intersects st_within
NULL

#' Map WWTP Column Names to Standard Format
#'
#' Create mapping between EPA WWTP data column names and standardized format.
#' Handles various EPA data formats across different years.
#'
#' @param raw_data Data frame. Raw WWTP data
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @param custom_mapping Named list. Custom column mappings (optional)
#' @return Named list with column mappings
#' @export
#' @examples
#' \dontrun{
# # Will need raw data from user. Hence, dont run.
#' mapping <- map_wwtp_columns(raw_wwtp_data, "nitrogen")
#'
#' # Custom mapping for different format
#' custom_map <- list(facility_name = "Plant_Name",
#'                   pollutant_load = "Annual_Load_kg")
#' mapping <- map_wwtp_columns(raw_data, "nitrogen", custom_map)
#' }
#'
map_wwtp_columns <- function(raw_data, nutrient, custom_mapping = NULL) {

  if (!nutrient %in% c("nitrogen", "phosphorus")) {
    stop("Nutrient must be 'nitrogen' or 'phosphorus'")
  }

  # If custom mapping provided, use it
  if (!is.null(custom_mapping)) {
    return(custom_mapping)
  }

  # Auto-detect EPA format based on column names
  col_names <- names(raw_data)

  # Standard mapping attempts for different EPA formats
  standard_mapping <- list(
    facility_name = c("Facility Name", "Facility_Name", "FACILITY_NAME", "Plant_Name"),
    npdes = c("NPDES Permit Number", "NPDES_ID", "NPDES", "Permit_Number"),
    state = c("State", "STATE", "State_Code"),
    county = c("County", "COUNTY", "County_Name"),
    facility_type = c("Facility Type Indicator", "Facility_Type", "Type"),
    major_status = c("Major/Non-Major Status", "Major_Minor", "Status"),
    latitude = c("Facility Latitude", "Latitude", "LAT", "Lat", "Facility_Latitude"),
    longitude = c("Facility Longitude", "Longitude", "LON", "Long", "Facility_Longitude"),
    design_flow = c("Total Facility Design Flow (MGD)", "Design_Flow", "Design_Flow_MGD"),
    actual_flow = c("Actual Average Facility Flow (MGD)", "Actual_Flow", "Avg_Flow_MGD"),
    wastewater_flow = c("Wastewater Flow (MGal/yr)", "Flow_MGal_yr", "Annual_Flow"),
    pollutant_load = c("Pollutant Load (kg/yr)", "Load_kg_yr", "Annual_Load_kg",
                       "Load (kg/yr)", "Pollutant_Load"),
    avg_daily_load = c("Average Daily Load (kg/day)", "Daily_Load", "Avg_Daily_Load"),
    avg_concentration = c("Average Concentration (mg/L)", "Concentration", "Avg_Conc_mg_L")
  )

  # Find matching columns
  column_mapping <- list()

  for (std_name in names(standard_mapping)) {
    possible_names <- standard_mapping[[std_name]]

    # Find first matching column name
    matched_col <- NULL
    for (possible_name in possible_names) {
      if (possible_name %in% col_names) {
        matched_col <- possible_name
        break
      }
    }

    if (!is.null(matched_col)) {
      column_mapping[[std_name]] <- matched_col
    } else {
      # If no exact match, try case-insensitive partial matching
      for (possible_name in possible_names) {
        partial_matches <- grep(possible_name, col_names, ignore.case = TRUE, value = TRUE)
        if (length(partial_matches) > 0) {
          column_mapping[[std_name]] <- partial_matches[1]
          break
        }
      }
    }
  }

  # Validate essential columns are found
  essential_cols <- c("facility_name", "pollutant_load", "latitude", "longitude")
  missing_essential <- setdiff(essential_cols, names(column_mapping))

  if (length(missing_essential) > 0) {
    stop("Could not find essential columns: ", paste(missing_essential, collapse = ", "),
         "\nAvailable columns: ", paste(col_names, collapse = ", "),
         "\nProvide custom_mapping for non-standard formats")
  }

  return(column_mapping)
}

#' Load User WWTP Data
#'
#' Load and standardize user-provided WWTP data with flexible formatting
#'
#' @param file_path Character. Path to WWTP data file
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @param column_mapping Named list. Custom column mapping (optional)
#' @param skip_rows Numeric. Number of rows to skip (default: 0)
#' @param header_row Numeric. Row containing headers (default: 1)
#' @param load_units Character. Units of pollutant loads: "kg", "lbs", "pounds", "tons"
#' @return Data frame with standardized WWTP data
#' @export
#' @examples
#' \dontrun{
#' # Standard EPA format but will not run because data needs to be supplied as custom
#' # Load custom WWTP data (for years outside 2007-2016)
#' wwtp_data <- load_user_wwtp("nitrogen_2020.csv", "nitrogen")
#'
#' # For years 2007-2016, consider using built-in data:
#' # wwtp_builtin <- load_builtin_wwtp("nitrogen", 2015)
#'
#' # Custom format with different units
#' wwtp_data <- load_user_wwtp("custom_wwtp.csv", "phosphorus",
#'                           load_units = "lbs", skip_rows = 3)
#'
#'
#' # Custom column mapping
#' custom_map <- list(facility_name = "Plant_Name",
#'                   pollutant_load = "Load_lbs_per_year")
#' wwtp_data <- load_user_wwtp("custom.csv", "nitrogen", custom_map)
#' }
load_user_wwtp <- function(file_path, nutrient, column_mapping = NULL,
                           skip_rows = 0, header_row = 1, load_units = "kg") {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  if (!nutrient %in% c("nitrogen", "phosphorus")) {
    stop("Nutrient must be 'nitrogen' or 'phosphorus'")
  }

  if (!load_units %in% c("kg", "lbs", "pounds", "tons")) {
    stop("load_units must be 'kg', 'lbs', 'pounds', or 'tons'")
  }

  message("Loading user WWTP ", nutrient, " data from: ", basename(file_path))

  # Read the file
  if (skip_rows > 0) {
    # Skip rows and read from header_row
    raw_data <- utils::read.csv(file_path, skip = skip_rows + header_row - 1,
                                stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    raw_data <- utils::read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  }

  message("Read ", nrow(raw_data), " rows, ", ncol(raw_data), " columns")

  # Get column mapping
  col_mapping <- map_wwtp_columns(raw_data, nutrient, column_mapping)

  # Create standardized data frame
  standardized_data <- data.frame(
    Facility_Name = raw_data[[col_mapping$facility_name]],
    stringsAsFactors = FALSE
  )

  # Add optional columns if available
  if ("npdes" %in% names(col_mapping)) {
    standardized_data$NPDES <- raw_data[[col_mapping$npdes]]
  }

  if ("state" %in% names(col_mapping)) {
    standardized_data$State <- raw_data[[col_mapping$state]]
  }

  if ("county" %in% names(col_mapping)) {
    standardized_data$County <- raw_data[[col_mapping$county]]
  }

  if ("facility_type" %in% names(col_mapping)) {
    standardized_data$Facility_Type <- raw_data[[col_mapping$facility_type]]
  }

  if ("major_status" %in% names(col_mapping)) {
    standardized_data$Major_Status <- raw_data[[col_mapping$major_status]]
  }

  # Add essential columns
  standardized_data$Lat <- as.numeric(raw_data[[col_mapping$latitude]])
  standardized_data$Long <- as.numeric(raw_data[[col_mapping$longitude]])

  # Add flow data if available
  if ("design_flow" %in% names(col_mapping)) {
    standardized_data$Design_Flow <- as.numeric(raw_data[[col_mapping$design_flow]])
  }

  if ("actual_flow" %in% names(col_mapping)) {
    standardized_data$Actual_Flow <- as.numeric(raw_data[[col_mapping$actual_flow]])
  }

  if ("wastewater_flow" %in% names(col_mapping)) {
    standardized_data$Wastewater_Flow <- as.numeric(raw_data[[col_mapping$wastewater_flow]])
  }

  # Add load data with unit conversion
  load_raw <- as.numeric(raw_data[[col_mapping$pollutant_load]])

  # Convert to standard units (kg and tons)
  load_kg <- convert_load_units(load_raw, load_units)
  load_tons <- load_kg / KG_TO_TONS

  # Set column names based on nutrient
  if (nutrient == "nitrogen") {
    standardized_data$N_Load_kg <- load_kg
    standardized_data$N_Load_tons <- load_tons
  } else {
    standardized_data$P_Load_kg <- load_kg
    standardized_data$P_Load_tons <- load_tons
  }

  # Add daily load and concentration if available
  if ("avg_daily_load" %in% names(col_mapping)) {
    standardized_data$Avg_Daily_Load <- as.numeric(raw_data[[col_mapping$avg_daily_load]])
  } else {
    # Calculate from annual load
    standardized_data$Avg_Daily_Load <- load_kg / 365
  }

  if ("avg_concentration" %in% names(col_mapping)) {
    standardized_data$Avg_Conc <- as.numeric(raw_data[[col_mapping$avg_concentration]])
  }

  message("Standardized WWTP data:")
  message("  Original units: ", load_units)
  message("  Facilities: ", nrow(standardized_data))
  message("  Mean load: ", round(mean(load_tons, na.rm = TRUE), 2), " tons/year")

  return(standardized_data)
}

#' Clean WWTP Data
#'
#' Clean and validate WWTP data for analysis
#'
#' @param wwtp_data Data frame. Raw or standardized WWTP data
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @return Data frame with cleaned WWTP data
#' @export
#' @examples
#' \dontrun{
#' # Clean user-loaded data will not run. They need to be supplied by users
#' clean_data <- wwtp_clean_data(raw_wwtp_data, "nitrogen")
#'
#' # Clean OSF data (usually already clean, available 2007-2016)
#' osf_data <- load_builtin_wwtp("phosphorus", 2012)
#' clean_data <- wwtp_clean_data(osf_data, "phosphorus")
#' }
wwtp_clean_data <- function(wwtp_data, nutrient) {

  if (!nutrient %in% c("nitrogen", "phosphorus")) {
    stop("Nutrient must be 'nitrogen' or 'phosphorus'")
  }

  message("Cleaning WWTP ", nutrient, " data...")

  original_count <- nrow(wwtp_data)

  # Remove rows with missing essential data
  cleaned_data <- wwtp_data %>%
    dplyr::filter(
      !is.na(Lat) & !is.na(Long) &
        Lat != 0 & Long != 0 &
        !is.na(Facility_Name) &
        Facility_Name != "" & Facility_Name != "NA"
    )

  # Filter by load column based on nutrient
  load_col <- if (nutrient == "nitrogen") "N_Load_tons" else "P_Load_tons"

  if (load_col %in% names(cleaned_data)) {
    cleaned_data <- cleaned_data %>%
      dplyr::filter(!is.na(!!rlang::sym(load_col)) & !!rlang::sym(load_col) > 0)
  }

  # Filter to CONUS (Continental US coordinates)
  cleaned_data <- cleaned_data %>%
    dplyr::filter(
      Lat >= 24.5 & Lat <= 49.5 &   # Continental US latitude range
        Long >= -125 & Long <= -66     # Continental US longitude range
    )

  # Remove duplicate facilities (same coordinates and name)
  cleaned_data <- cleaned_data %>%
    dplyr::distinct(Facility_Name, Lat, Long, .keep_all = TRUE)

  final_count <- nrow(cleaned_data)
  removed_count <- original_count - final_count

  message("Cleaning complete:")
  message("  Original facilities: ", original_count)
  message("  Removed: ", removed_count, " (missing data, duplicates, or outside CONUS)")
  message("  Final facilities: ", final_count)

  if (final_count == 0) {
    warning("No valid facilities remaining after cleaning")
  }

  return(cleaned_data)
}

#' Filter WWTP Data for Positive Loads
#'
#' Filter WWTP data to include only facilities with positive nutrient loads
#'
#' @param wwtp_data Data frame. WWTP data
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @return Data frame with facilities having positive loads
#' @export
wwtp_filter_positive_loads <- function(wwtp_data, nutrient) {

  if (!nutrient %in% c("nitrogen", "phosphorus")) {
    stop("Nutrient must be 'nitrogen' or 'phosphorus'")
  }

  load_col <- if (nutrient == "nitrogen") "N_Load_tons" else "P_Load_tons"

  if (!load_col %in% names(wwtp_data)) {
    stop("Load column '", load_col, "' not found in data")
  }

  original_count <- nrow(wwtp_data)

  filtered_data <- wwtp_data %>%
    dplyr::filter(!!rlang::sym(load_col) > 0)

  final_count <- nrow(filtered_data)

  message("Filtered for positive ", nutrient, " loads:")
  message("  Original: ", original_count, " facilities")
  message("  With positive loads: ", final_count, " facilities")

  return(filtered_data)
}

#' Classify WWTP Sources by Load Size
#'
#' Classify WWTP facilities into size categories based on annual nutrient loads
#'
#' @param wwtp_data Data frame. WWTP data with load information
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @return Data frame with source_class column added
#' @export
#' @examples
#' \donttest{
#' # Load WWTP data first
#' wwtp_data <- load_builtin_wwtp("nitrogen", 2016)
#'
#' # Classify nitrogen sources
#' classified_data <- wwtp_classify_sources(wwtp_data, "nitrogen")
#' table(classified_data$source_class)
#' }
wwtp_classify_sources <- function(wwtp_data, nutrient) {

  if (!nutrient %in% c("nitrogen", "phosphorus")) {
    stop("Nutrient must be 'nitrogen' or 'phosphorus'")
  }

  load_col <- if (nutrient == "nitrogen") "N_Load_tons" else "P_Load_tons"

  if (!load_col %in% names(wwtp_data)) {
    stop("Load column '", load_col, "' not found in data")
  }

  # Define thresholds based on nutrient
  if (nutrient == "nitrogen") {
    # Nitrogen thresholds (tons/year)
    thresholds <- c(10, 50, 150, 1000)
    labels <- c("Minor Source", "Small Source", "Medium Source", "Large Source", "Very Large Source")
  } else {
    # Phosphorus thresholds (tons/year) - typically lower than nitrogen
    thresholds <- c(1, 5, 15, 100)
    labels <- c("Minor Source", "Small Source", "Medium Source", "Large Source", "Very Large Source")
  }

  # Classify facilities
  classified_data <- wwtp_data %>%
    dplyr::mutate(
      source_class = cut(!!rlang::sym(load_col),
                         breaks = c(0, thresholds, Inf),
                         labels = labels,
                         include.lowest = TRUE)
    )

  # Summary of classifications
  class_summary <- table(classified_data$source_class)
  message("WWTP ", nutrient, " source classification:")
  for (i in 1:length(class_summary)) {
    message("  ", names(class_summary)[i], ": ", class_summary[i], " facilities")
  }

  return(classified_data)
}

#' Load User WWTP Data
#'
#' Load and standardize user-provided WWTP data with flexible formatting
#'
#' @param file_path Character. Path to WWTP data file
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @param column_mapping Named list. Custom column mapping (optional)
#' @param skip_rows Numeric. Number of rows to skip (default: 0)
#' @param header_row Numeric. Row containing headers (default: 1)
#' @param load_units Character. Units of pollutant loads: "kg", "lbs", "pounds", "tons"
#' @return Data frame with standardized WWTP data
#' @export
#' @examples
#' \dontrun{
# # These examples require user data files - dont run in package check
#'
#' # Standard EPA format
#' wwtp_data <- load_user_wwtp("nitrogen_2020.csv", "nitrogen")
#'
#' # Custom format with different units
#' wwtp_data <- load_user_wwtp("custom_wwtp.csv", "phosphorus",
#'                           load_units = "lbs", skip_rows = 3)
#'
#' # Custom column mapping
#' custom_map <- list(facility_name = "Plant_Name",
#'                   pollutant_load = "Load_lbs_per_year")
#' wwtp_data <- load_user_wwtp("custom.csv", "nitrogen", custom_map)
#' }
load_user_wwtp <- function(file_path, nutrient, column_mapping = NULL,
                           skip_rows = 0, header_row = 1, load_units = "kg") {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  if (!nutrient %in% c("nitrogen", "phosphorus")) {
    stop("Nutrient must be 'nitrogen' or 'phosphorus'")
  }

  if (!load_units %in% c("kg", "lbs", "pounds", "tons")) {
    stop("load_units must be 'kg', 'lbs', 'pounds', or 'tons'")
  }

  message("Loading user WWTP ", nutrient, " data from: ", basename(file_path))

  # Read the file
  if (skip_rows > 0) {
    # Skip rows and read from header_row
    raw_data <- utils::read.csv(file_path, skip = skip_rows + header_row - 1,
                                stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    raw_data <- utils::read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  }

  message("Read ", nrow(raw_data), " rows, ", ncol(raw_data), " columns")

  # Get column mapping
  col_mapping <- map_wwtp_columns(raw_data, nutrient, column_mapping)

  # Create standardized data frame
  standardized_data <- data.frame(
    Facility_Name = raw_data[[col_mapping$facility_name]],
    stringsAsFactors = FALSE
  )

  # Add optional columns if available
  if ("npdes" %in% names(col_mapping)) {
    standardized_data$NPDES <- raw_data[[col_mapping$npdes]]
  }

  if ("state" %in% names(col_mapping)) {
    standardized_data$State <- raw_data[[col_mapping$state]]
  }

  if ("county" %in% names(col_mapping)) {
    standardized_data$County <- raw_data[[col_mapping$county]]
  }

  if ("facility_type" %in% names(col_mapping)) {
    standardized_data$Facility_Type <- raw_data[[col_mapping$facility_type]]
  }

  if ("major_status" %in% names(col_mapping)) {
    standardized_data$Major_Status <- raw_data[[col_mapping$major_status]]
  }

  # Add essential columns
  standardized_data$Lat <- as.numeric(raw_data[[col_mapping$latitude]])
  standardized_data$Long <- as.numeric(raw_data[[col_mapping$longitude]])

  # Add flow data if available
  if ("design_flow" %in% names(col_mapping)) {
    standardized_data$Design_Flow <- as.numeric(raw_data[[col_mapping$design_flow]])
  }

  if ("actual_flow" %in% names(col_mapping)) {
    standardized_data$Actual_Flow <- as.numeric(raw_data[[col_mapping$actual_flow]])
  }

  if ("wastewater_flow" %in% names(col_mapping)) {
    standardized_data$Wastewater_Flow <- as.numeric(raw_data[[col_mapping$wastewater_flow]])
  }

  # Add load data with unit conversion
  load_raw <- as.numeric(raw_data[[col_mapping$pollutant_load]])

  # Convert to standard units (kg and tons)
  load_kg <- convert_load_units(load_raw, load_units)
  load_tons <- load_kg / KG_TO_TONS

  # Set column names based on nutrient
  if (nutrient == "nitrogen") {
    standardized_data$N_Load_kg <- load_kg
    standardized_data$N_Load_tons <- load_tons
  } else {
    standardized_data$P_Load_kg <- load_kg
    standardized_data$P_Load_tons <- load_tons
  }

  # Add daily load and concentration if available
  if ("avg_daily_load" %in% names(col_mapping)) {
    standardized_data$Avg_Daily_Load <- as.numeric(raw_data[[col_mapping$avg_daily_load]])
  } else {
    # Calculate from annual load
    standardized_data$Avg_Daily_Load <- load_kg / 365
  }

  if ("avg_concentration" %in% names(col_mapping)) {
    standardized_data$Avg_Conc <- as.numeric(raw_data[[col_mapping$avg_concentration]])
  }

  message("Standardized WWTP data:")
  message("  Original units: ", load_units)
  message("  Facilities: ", nrow(standardized_data))
  message("  Mean load: ", round(mean(load_tons, na.rm = TRUE), 2), " tons/year")

  return(standardized_data)
}

#' Clean WWTP Data
#'
#' Clean and validate WWTP data for analysis
#'
#' @param wwtp_data Data frame. Raw or standardized WWTP data
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @return Data frame with cleaned WWTP data
#' @export
#' @examples
#' \donttest{
#' # Load and clean WWTP data
#' raw_wwtp_data <- load_builtin_wwtp("nitrogen", 2016)
#' clean_data <- wwtp_clean_data(raw_wwtp_data, "nitrogen")
#' }
wwtp_clean_data <- function(wwtp_data, nutrient) {

  if (!nutrient %in% c("nitrogen", "phosphorus")) {
    stop("Nutrient must be 'nitrogen' or 'phosphorus'")
  }

  message("Cleaning WWTP ", nutrient, " data...")

  original_count <- nrow(wwtp_data)

  # Remove rows with missing essential data
  cleaned_data <- wwtp_data %>%
    dplyr::filter(
      !is.na(Lat) & !is.na(Long) &
        Lat != 0 & Long != 0 &
        !is.na(Facility_Name) &
        Facility_Name != "" & Facility_Name != "NA"
    )

  # Filter by load column based on nutrient
  load_col <- if (nutrient == "nitrogen") "N_Load_tons" else "P_Load_tons"

  if (load_col %in% names(cleaned_data)) {
    cleaned_data <- cleaned_data %>%
      dplyr::filter(!is.na(!!rlang::sym(load_col)) & !!rlang::sym(load_col) > 0)
  }

  # Filter to CONUS (Continental US coordinates)
  cleaned_data <- cleaned_data %>%
    dplyr::filter(
      Lat >= 24.5 & Lat <= 49.5 &   # Continental US latitude range
        Long >= -125 & Long <= -66     # Continental US longitude range
    )

  # Remove duplicate facilities (same coordinates and name)
  cleaned_data <- cleaned_data %>%
    dplyr::distinct(Facility_Name, Lat, Long, .keep_all = TRUE)

  final_count <- nrow(cleaned_data)
  removed_count <- original_count - final_count

  message("Cleaning complete:")
  message("  Original facilities: ", original_count)
  message("  Removed: ", removed_count, " (missing data, duplicates, or outside CONUS)")
  message("  Final facilities: ", final_count)

  if (final_count == 0) {
    warning("No valid facilities remaining after cleaning")
  }

  return(cleaned_data)
}

#' Convert WWTP Data to Spatial Format
#'
#' Convert WWTP data frame to sf spatial object
#'
#' @param wwtp_data Data frame. WWTP data with Lat/Long coordinates
#' @param crs Numeric. Coordinate reference system (default: 4326 for WGS84)
#' @return sf object with WWTP facilities as point geometries
#' @export
#' @examples
#' \donttest{
#' # Load and convert to spatial format
#' wwtp_data <- load_builtin_wwtp("nitrogen", 2016)
#' wwtp_clean_data <- wwtp_clean_data(wwtp_data, "nitrogen")
#' wwtp_sf <- wwtp_to_spatial(wwtp_clean_data)
#'
#' # Convert and transform to analysis CRS (without using pipe operator)
#' wwtp_sf <- wwtp_to_spatial(wwtp_clean_data)
#' wwtp_sf_transformed <- sf::st_transform(wwtp_sf, 5070)  # Albers Equal Area Conic
#' }
wwtp_to_spatial <- function(wwtp_data, crs = 4326) {

  # Validate coordinate columns
  if (!all(c("Lat", "Long") %in% names(wwtp_data))) {
    stop("Lat and Long columns required for spatial conversion")
  }

  # Check for valid coordinates
  valid_coords <- !is.na(wwtp_data$Lat) & !is.na(wwtp_data$Long) &
    wwtp_data$Lat != 0 & wwtp_data$Long != 0

  if (sum(valid_coords) == 0) {
    stop("No valid coordinates found in data")
  }

  if (sum(!valid_coords) > 0) {
    message("Removing ", sum(!valid_coords), " facilities with invalid coordinates")
    wwtp_data <- wwtp_data[valid_coords, ]
  }

  # Convert to sf object
  wwtp_sf <- sf::st_as_sf(wwtp_data,
                          coords = c("Long", "Lat"),
                          crs = crs)

  message("Created spatial WWTP data with ", nrow(wwtp_sf), " facilities")

  return(wwtp_sf)
}

#' Aggregate WWTP Data by Spatial Boundaries
#'
#' Aggregate WWTP loads by spatial units (counties, HUC8s, etc.)
#'
#' @param wwtp_sf sf object. Spatial WWTP data
#' @param boundaries sf object. Spatial boundaries for aggregation
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @param boundary_id_col Character. Name of boundary ID column
#' @return Data frame with aggregated loads by spatial unit
#' @export
wwtp_aggregate_by_boundaries <- function(wwtp_sf, boundaries, nutrient, boundary_id_col) {

  if (!nutrient %in% c("nitrogen", "phosphorus")) {
    stop("Nutrient must be 'nitrogen' or 'phosphorus'")
  }

  # DEBUG: Print what we're looking for
  message("Looking for boundary ID column: '", boundary_id_col, "'")
  message("Available columns in boundaries: ", paste(names(boundaries), collapse = ", "))

  if (!boundary_id_col %in% names(boundaries)) {
    stop("Boundary ID column '", boundary_id_col, "' not found in boundaries.",
         "\nAvailable columns: ", paste(names(boundaries), collapse = ", "))
  }

  load_col <- if (nutrient == "nitrogen") "N_Load_tons" else "P_Load_tons"

  if (!load_col %in% names(wwtp_sf)) {
    stop("Load column '", load_col, "' not found in WWTP data")
  }

  message("Aggregating WWTP ", nutrient, " loads by spatial boundaries...")

  # Ensure both datasets have same CRS
  if (sf::st_crs(wwtp_sf) != sf::st_crs(boundaries)) {
    wwtp_sf <- sf::st_transform(wwtp_sf, sf::st_crs(boundaries))
  }

  # Spatial intersection to assign facilities to boundaries
  intersections <- sf::st_intersects(wwtp_sf, boundaries)

  # Create facility-boundary assignments
  facility_assignments <- data.frame(
    facility_index = rep(1:nrow(wwtp_sf), lengths(intersections)),
    boundary_index = unlist(intersections)
  )

  if (nrow(facility_assignments) == 0) {
    warning("No WWTP facilities found within provided boundaries")
    # Return empty aggregation with proper structure
    empty_result <- data.frame(
      ID = character(0),
      wwtp_n_load = numeric(0),
      wwtp_p_load = numeric(0),
      wwtp_count = integer(0)
    )
    return(empty_result)
  }

  # FIXED: Use proper indexing with the boundary_id_col
  # Extract the boundary ID column values using the correct column name
  boundary_ids <- boundaries[[boundary_id_col]]
  facility_assignments$boundary_id <- boundary_ids[facility_assignments$boundary_index]

  # Extract load values
  load_values <- as.numeric(sf::st_drop_geometry(wwtp_sf)[[load_col]])
  facility_assignments$load <- load_values[facility_assignments$facility_index]

  # Aggregate by boundary
  aggregated <- facility_assignments %>%
    dplyr::group_by(boundary_id) %>%
    dplyr::summarise(
      total_load = sum(load, na.rm = TRUE),
      facility_count = dplyr::n(),
      .groups = 'drop'
    )

  # Create final result with standardized column names
  result <- data.frame(
    ID = aggregated$boundary_id,
    wwtp_n_load = if (nutrient == "nitrogen") aggregated$total_load else 0,
    wwtp_p_load = if (nutrient == "phosphorus") aggregated$total_load else 0,
    wwtp_count = aggregated$facility_count
  )

  message("Aggregation complete:")
  message("  WWTP facilities: ", nrow(wwtp_sf))
  message("  Spatial units with facilities: ", nrow(result))
  message("  Total ", nutrient, " load: ", round(sum(result[[paste0("wwtp_", substr(nutrient, 1, 1), "_load")]]), 2), " tons/year")

  return(result)
}

#' Complete WWTP Processing Pipeline
#'
#' Run complete WWTP processing pipeline for both nutrients
#'
#' @param nitrogen_path Character. Path to nitrogen WWTP data (if NULL, loads from OSF)
#' @param phosphorus_path Character. Path to phosphorus WWTP data (if NULL, loads from OSF)
#' @param column_mapping Named list. Custom column mapping for user data
#' @param skip_rows Numeric. Rows to skip in user files
#' @param header_row Numeric. Header row in user files
#' @param load_units Character. Units of loads in user files
#' @param verbose Logical. Show processing messages
#' @return List with processed nitrogen and phosphorus WWTP data
#' @export
#' @examples
#' \donttest{
#' # Process built-in OSF data (2016 default)
#' wwtp_results_builtin <- wwtp_process_complete(
#'   nitrogen_path = NULL,     # Use built-in data
#'   phosphorus_path = NULL,   # Use built-in data
#'   verbose = TRUE
#' )
#'
#' # Process custom user data
#' # wwtp_results_custom <- wwtp_process_complete(
#' #   nitrogen_path = "nitrogen_2020.csv",
#' #   phosphorus_path = "phosphorus_2020.csv",
#' #   load_units = "lbs"
#' # )
#'
#' # Mixed: OSF for one nutrient, custom for another
#' # wwtp_results_mixed <- wwtp_process_complete(
#' #   nitrogen_path = NULL,           # Use OSF built-in
#' #   phosphorus_path = "custom_P.csv" # Use custom
#' # )
#' }
wwtp_process_complete <- function(nitrogen_path = NULL, phosphorus_path = NULL,
                                  column_mapping = NULL, skip_rows = 0,
                                  header_row = 1, load_units = "kg",
                                  verbose = TRUE) {

  if (verbose) {
    message("Starting complete WWTP processing pipeline...")
  }

  results <- list()

  # Process nitrogen data
  if (!is.null(nitrogen_path)) {
    # User-provided nitrogen data
    if (verbose) message("Processing user nitrogen WWTP data...")
    wwtp_n_raw <- load_user_wwtp(nitrogen_path, "nitrogen", column_mapping,
                                 skip_rows, header_row, load_units)
    wwtp_n_clean <- wwtp_clean_data(wwtp_n_raw, "nitrogen")
  } else {
    # Load from OSF
    if (verbose) message("Loading nitrogen WWTP data from OSF...")
    wwtp_n_clean <- load_builtin_wwtp("nitrogen", verbose = verbose) %>%
      wwtp_filter_positive_loads("nitrogen") %>%
      wwtp_classify_sources("nitrogen")
  }

  results$nitrogen <- list(
    raw_data = if (!is.null(nitrogen_path)) wwtp_n_raw else NULL,
    clean_data = wwtp_n_clean,
    spatial_data = wwtp_to_spatial(wwtp_n_clean)
  )

  # Process phosphorus data
  if (!is.null(phosphorus_path)) {
    # User-provided phosphorus data
    if (verbose) message("Processing user phosphorus WWTP data...")
    wwtp_p_raw <- load_user_wwtp(phosphorus_path, "phosphorus", column_mapping,
                                 skip_rows, header_row, load_units)
    wwtp_p_clean <- wwtp_clean_data(wwtp_p_raw, "phosphorus")
  } else {
    # Load from OSF
    if (verbose) message("Loading phosphorus WWTP data from OSF...")
    wwtp_p_clean <- load_builtin_wwtp("phosphorus", verbose = verbose) %>%
      wwtp_filter_positive_loads("phosphorus") %>%
      wwtp_classify_sources("phosphorus")
  }

  results$phosphorus <- list(
    raw_data = if (!is.null(phosphorus_path)) wwtp_p_raw else NULL,
    clean_data = wwtp_p_clean,
    spatial_data = wwtp_to_spatial(wwtp_p_clean)
  )

  if (verbose) {
    message("WWTP processing complete!")
    message("  Nitrogen facilities: ", nrow(results$nitrogen$clean_data))
    message("  Phosphorus facilities: ", nrow(results$phosphorus$clean_data))
  }

  return(results)
}
