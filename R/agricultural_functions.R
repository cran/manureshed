# ==============================================================================
# R/agricultural_functions.R - Agricultural Classification Functions
# ==============================================================================

#' Process NuGIS Data for Manureshed Analysis
#'
#' Clean and standardize NuGIS data for agricultural nutrient analysis
#'
#' @param nugis_data Data frame. Raw NuGIS data for specified year
#' @param scale Character. Spatial scale: "county", "huc8", or "huc2"
#' @return Data frame with standardized columns for analysis
#' @export
agri_process_nugis <- function(nugis_data, scale) {

  # Define column mappings based on scale - UPDATED for your actual data structure
  if (scale == "county") {
    col_mapping <- list(
      id_col = "FIPS",
      name_col = "county",
      manure_n = "manure_N",              # Already standardized in preprocessing
      manure_p = "manure_P2O5",           # Will convert P2O5 to P
      fertilizer_n = "fertilizer_N",      # Already standardized
      fertilizer_p = "fertilizer_P2O5",   # Will convert P2O5 to P
      n_fixation = "N_fixation",          # Already standardized
      crop_removal_n = "crop_removal_N",  # Already standardized
      crop_removal_p = "crop_removal_P2O5", # Will convert P2O5 to P
      cropland = "cropland"               # Already standardized
    )
  } else if (scale == "huc8") {
    col_mapping <- list(
      id_col = "HUC_8",
      name_col = "HUC_NAME",
      manure_n = "manure_N",              # Already standardized
      manure_p = "manure_P2O5",           # Will convert P2O5 to P
      fertilizer_n = "fertilizer_N",      # Already standardized
      fertilizer_p = "fertilizer_P2O5",   # Will convert P2O5 to P
      n_fixation = "N_fixation",          # Already standardized
      crop_removal_n = "crop_removal_N",  # Already standardized
      crop_removal_p = "crop_removal_P2O5", # Will convert P2O5 to P
      cropland = "cropland"               # Already standardized
    )
  } else if (scale == "huc2") {
    col_mapping <- list(
      id_col = "HUC_2",
      name_col = "huc_name",              # Note: different column name for HUC2
      manure_n = "manure_N",              # Already standardized
      manure_p = "manure_P2O5",           # Will convert P2O5 to P
      fertilizer_n = "fertilizer_N",      # Already standardized
      fertilizer_p = "fertilizer_P2O5",   # Will convert P2O5 to P
      n_fixation = "N_fixation",          # Already standardized
      crop_removal_n = "crop_removal_N",  # Already standardized
      crop_removal_p = "crop_removal_P2O5", # Will convert P2O5 to P
      cropland = "cropland"               # Already standardized
    )
  } else {
    stop("Unsupported scale: ", scale)
  }
  # Validate required columns exist
  required_cols <- unlist(col_mapping)
  validate_columns(nugis_data, required_cols, paste(scale, "NuGIS data"))

  # Create standardized data frame
  clean_data <- data.frame(
    ID = nugis_data[[col_mapping$id_col]],
    NAME = nugis_data[[col_mapping$name_col]],
    manure_N = nugis_data[[col_mapping$manure_n]],
    manure_P = nugis_data[[col_mapping$manure_p]] * P2O5_TO_P,  # Convert P2O5 to P
    fertilizer_N = nugis_data[[col_mapping$fertilizer_n]],
    fertilizer_P = nugis_data[[col_mapping$fertilizer_p]] * P2O5_TO_P,  # Convert P2O5 to P
    N_fixation = nugis_data[[col_mapping$n_fixation]],
    crop_removal_N = nugis_data[[col_mapping$crop_removal_n]],
    crop_removal_P = nugis_data[[col_mapping$crop_removal_p]] * P2O5_TO_P,  # Convert P2O5 to P
    cropland = nugis_data[[col_mapping$cropland]],
    stringsAsFactors = FALSE
  )

  # Standardize ID codes based on scale
  if (scale == "county") {
    # FIPS codes should be 5 digits
    clean_data$ID <- sprintf("%05d", as.numeric(clean_data$ID))
  } else if (scale %in% c("huc8", "huc2")) {
    # HUC codes - use existing format_huc8 function for consistency
    clean_data$ID <- format_huc8(clean_data$ID)
  }

  # Clean name column
  clean_data$NAME <- clean_text(clean_data$NAME)

  message("Processed NuGIS data for ", scale, " scale:")
  message("  Spatial units: ", nrow(clean_data))
  message("  Converted P2O5 to P using factor: ", P2O5_TO_P)

  return(clean_data)
}

#' Classify Agricultural Nitrogen Status
#'
#' Classify spatial units based on nitrogen balance using standard 0.5 efficiency factor
#'
#' @param data Data frame with processed agricultural data
#' @param cropland_threshold Numeric. Threshold for excluding small cropland areas
#' @param scale Character. Spatial scale for within-unit classification
#' @return Data frame with nitrogen classification added
#' @export
agri_classify_nitrogen <- function(data, cropland_threshold, scale = "huc8") {
  # Calculate nitrogen surplus (using 0.5 factor for available manure N)
  data$N_surplus <- 0.5 * data$manure_N - (data$crop_removal_N - data$N_fixation)

  # Initialize classification column
  data$N_class <- NA_character_

  # 1. Source units (surplus > 0)
  data$N_class[data$N_surplus > 0] <- "Source"

  # 2. Exclude small cropland units
  data$N_class[data$cropland < cropland_threshold] <- "Excluded"

  # 3. Sink units with deficit (total inputs < crop removal)
  deficit_condition <- is.na(data$N_class) &
    (0.5 * (data$manure_N + data$fertilizer_N) -
       (data$crop_removal_N - data$N_fixation) < 0)
  data$N_class[deficit_condition] <- "Sink_Deficit"

  # 4. Sink units with fertilizer surplus
  fert_surplus_condition <- is.na(data$N_class) &
    (0.5 * data$fertilizer_N - (data$crop_removal_N - data$N_fixation) > 0)
  data$N_class[fert_surplus_condition] <- "Sink_Fertilizer"

  # 5. Within-unit transfer candidates
  within_condition <- is.na(data$N_class) &
    (0.5 * data$manure_N - (data$crop_removal_N - data$N_fixation) <= 0) &
    (0.5 * data$fertilizer_N - (data$crop_removal_N - data$N_fixation) <= 0) &
    (0.5 * (data$manure_N + data$fertilizer_N) -
       (data$crop_removal_N - data$N_fixation) >= 0)

  # Set appropriate within-unit classification based on scale
  within_class <- if (scale == "county") "Within_County" else "Within_Watershed"
  data$N_class[within_condition] <- within_class

  # Summary
  class_summary <- table(data$N_class)
  message("Nitrogen classification summary:")
  for (i in 1:length(class_summary)) {
    message("  ", names(class_summary)[i], ": ", class_summary[i], " units")
  }

  return(data)
}

#' Classify Agricultural Nitrogen Status with Custom Efficiency Factor
#'
#' Classify spatial units based on nitrogen balance with user-specified efficiency factor.
#' This function allows sensitivity analysis by varying the nitrogen efficiency assumption.
#' The default value of 0.5 represents typical losses during nutrient cycling, uptake, and
#' application, but regional conditions may warrant different values.
#'
#' @param data Data frame with processed agricultural data
#' @param cropland_threshold Numeric. Threshold for excluding small cropland areas
#' @param scale Character. Spatial scale for within-unit classification
#' @param n_efficiency Numeric. Nitrogen efficiency factor (default: 0.5, range: 0-1)
#' @return Data frame with nitrogen classification added
#' @export
#' @examples
#' \donttest{
#' # Load and process data first
#' nugis_data <- load_builtin_nugis("county", 2016)
#' processed_data <- agri_process_nugis(nugis_data, "county")
#' cropland_threshold <- 500 * 2.47105  # 500 ha in acres
#'
#' # Standard analysis with default 0.5 efficiency
#' results_default <- agri_classify_nitrogen_custom(
#'   processed_data, cropland_threshold = cropland_threshold, n_efficiency = 0.5
#' )
#'
#' # Sensitivity analysis with higher efficiency (e.g., improved management)
#' results_high <- agri_classify_nitrogen_custom(
#'   processed_data, cropland_threshold = cropland_threshold, n_efficiency = 0.7
#' )
#'
#' # Conservative analysis with lower efficiency
#' results_low <- agri_classify_nitrogen_custom(
#'   processed_data, cropland_threshold = cropland_threshold, n_efficiency = 0.3
#' )
#'
#' # Compare classification changes across efficiency scenarios
#' table(results_default$N_class)
#' table(results_high$N_class)
#' table(results_low$N_class)
#' }
agri_classify_nitrogen_custom <- function(data, cropland_threshold, scale = "huc8",
                                          n_efficiency = 0.5) {
  # Validate efficiency factor
  if (!is.numeric(n_efficiency) || n_efficiency < 0 || n_efficiency > 1) {
    stop("Nitrogen efficiency factor must be numeric between 0 and 1. Provided: ", n_efficiency)
  }

  if (n_efficiency != 0.5) {
    message("Using custom nitrogen efficiency factor: ", n_efficiency)
    message("  Standard factor is 0.5 representing typical losses")
    message("  Your factor represents ", n_efficiency * 100, "% efficiency")
  }

  # Calculate nitrogen surplus with custom efficiency
  data$N_surplus <- n_efficiency * data$manure_N - (data$crop_removal_N - data$N_fixation)

  # Initialize classification column
  data$N_class <- NA_character_

  # 1. Source units (surplus > 0)
  data$N_class[data$N_surplus > 0] <- "Source"

  # 2. Exclude small cropland units
  data$N_class[data$cropland < cropland_threshold] <- "Excluded"

  # 3. Sink units with deficit (total inputs < crop removal)
  deficit_condition <- is.na(data$N_class) &
    (n_efficiency * (data$manure_N + data$fertilizer_N) -
       (data$crop_removal_N - data$N_fixation) < 0)
  data$N_class[deficit_condition] <- "Sink_Deficit"

  # 4. Sink units with fertilizer surplus
  fert_surplus_condition <- is.na(data$N_class) &
    (n_efficiency * data$fertilizer_N - (data$crop_removal_N - data$N_fixation) > 0)
  data$N_class[fert_surplus_condition] <- "Sink_Fertilizer"

  # 5. Within-unit transfer candidates
  within_condition <- is.na(data$N_class) &
    (n_efficiency * data$manure_N - (data$crop_removal_N - data$N_fixation) <= 0) &
    (n_efficiency * data$fertilizer_N - (data$crop_removal_N - data$N_fixation) <= 0) &
    (n_efficiency * (data$manure_N + data$fertilizer_N) -
       (data$crop_removal_N - data$N_fixation) >= 0)

  # Set appropriate within-unit classification based on scale
  within_class <- if (scale == "county") "Within_County" else "Within_Watershed"
  data$N_class[within_condition] <- within_class

  # Summary
  class_summary <- table(data$N_class)
  message("Nitrogen classification summary (efficiency = ", n_efficiency, "):")
  for (i in 1:length(class_summary)) {
    message("  ", names(class_summary)[i], ": ", class_summary[i], " units")
  }

  return(data)
}

#' Classify Agricultural Phosphorus Status
#'
#' Classify spatial units based on phosphorus balance (no efficiency factor for P)
#'
#' @param data Data frame with processed agricultural data
#' @param cropland_threshold Numeric. Threshold for excluding small cropland areas
#' @param scale Character. Spatial scale for within-unit classification
#' @return Data frame with phosphorus classification added
#' @export
agri_classify_phosphorus <- function(data, cropland_threshold, scale = "huc8") {
  # Calculate phosphorus surplus (no 0.5 factor for P)
  data$P_surplus <- data$manure_P - data$crop_removal_P

  # Initialize classification column
  data$P_class <- NA_character_

  # 1. Source units (surplus > 0)
  data$P_class[data$P_surplus > 0] <- "Source"

  # 2. Exclude small cropland units
  data$P_class[data$cropland < cropland_threshold] <- "Excluded"

  # 3. Sink units with deficit
  deficit_condition <- is.na(data$P_class) &
    (data$manure_P + data$fertilizer_P - data$crop_removal_P < 0)
  data$P_class[deficit_condition] <- "Sink_Deficit"

  # 4. Sink units with fertilizer surplus
  fert_surplus_condition <- is.na(data$P_class) &
    (data$fertilizer_P - data$crop_removal_P > 0)
  data$P_class[fert_surplus_condition] <- "Sink_Fertilizer"

  # 5. Within-unit transfer candidates
  within_condition <- is.na(data$P_class) &
    (data$manure_P - data$crop_removal_P <= 0) &
    (data$fertilizer_P - data$crop_removal_P <= 0) &
    (data$manure_P + data$fertilizer_P - data$crop_removal_P >= 0)

  # Set appropriate within-unit classification based on scale
  within_class <- if (scale == "county") "Within_County" else "Within_Watershed"
  data$P_class[within_condition] <- within_class

  # Summary
  class_summary <- table(data$P_class)
  message("Phosphorus classification summary:")
  for (i in 1:length(class_summary)) {
    message("  ", names(class_summary)[i], ": ", class_summary[i], " units")
  }

  return(data)
}

#' Classify Agricultural Phosphorus Status with Custom Efficiency Factor
#'
#' Classify spatial units based on phosphorus balance with user-specified efficiency factor.
#' While standard phosphorus classification uses 100\% efficiency (factor = 1.0), this function
#' allows sensitivity analysis by varying the phosphorus efficiency assumption for different
#' management scenarios or application methods.
#'
#' @param data Data frame with processed agricultural data
#' @param cropland_threshold Numeric. Threshold for excluding small cropland areas
#' @param scale Character. Spatial scale for within-unit classification
#' @param p_efficiency Numeric. Phosphorus efficiency factor (default: 1.0, range: 0-1)
#' @return Data frame with phosphorus classification added
#' @export
#' @examples
#' \donttest{
#' # Load and process data first
#' nugis_data <- load_builtin_nugis("county", 2016)
#' processed_data <- agri_process_nugis(nugis_data, "county")
#' cropland_threshold <- 500 * 2.47105  # 500 ha in acres
#'
#' # Standard analysis with default 1.0 efficiency (100%)
#' results_default <- agri_classify_phosphorus_custom(
#'   processed_data, cropland_threshold = cropland_threshold, p_efficiency = 1.0
#' )
#'
#' # Analysis with reduced efficiency (e.g., accounting for losses)
#' results_reduced <- agri_classify_phosphorus_custom(
#'   processed_data, cropland_threshold = cropland_threshold, p_efficiency = 0.8
#' )
#'
#' # Conservative analysis with lower efficiency
#' results_conservative <- agri_classify_phosphorus_custom(
#'   processed_data, cropland_threshold = cropland_threshold, p_efficiency = 0.6
#' )
#'
#' # Compare classification changes across efficiency scenarios
#' table(results_default$P_class)
#' table(results_reduced$P_class)
#' table(results_conservative$P_class)
#' }
agri_classify_phosphorus_custom <- function(data, cropland_threshold, scale = "huc8",
                                            p_efficiency = 1.0) {
  # Validate efficiency factor
  if (!is.numeric(p_efficiency) || p_efficiency < 0 || p_efficiency > 1) {
    stop("Phosphorus efficiency factor must be numeric between 0 and 1. Provided: ", p_efficiency)
  }

  if (p_efficiency != 1.0) {
    message("Using custom phosphorus efficiency factor: ", p_efficiency)
    message("  Standard factor is 1.0 (100% efficiency)")
    message("  Your factor represents ", p_efficiency * 100, "% efficiency")
  }

  # Calculate phosphorus surplus with custom efficiency
  data$P_surplus <- p_efficiency * data$manure_P - data$crop_removal_P

  # Initialize classification column
  data$P_class <- NA_character_

  # 1. Source units (surplus > 0)
  data$P_class[data$P_surplus > 0] <- "Source"

  # 2. Exclude small cropland units
  data$P_class[data$cropland < cropland_threshold] <- "Excluded"

  # 3. Sink units with deficit
  deficit_condition <- is.na(data$P_class) &
    (p_efficiency * (data$manure_P + data$fertilizer_P) - data$crop_removal_P < 0)
  data$P_class[deficit_condition] <- "Sink_Deficit"

  # 4. Sink units with fertilizer surplus
  fert_surplus_condition <- is.na(data$P_class) &
    (p_efficiency * data$fertilizer_P - data$crop_removal_P > 0)
  data$P_class[fert_surplus_condition] <- "Sink_Fertilizer"

  # 5. Within-unit transfer candidates
  within_condition <- is.na(data$P_class) &
    (p_efficiency * data$manure_P - data$crop_removal_P <= 0) &
    (p_efficiency * data$fertilizer_P - data$crop_removal_P <= 0) &
    (p_efficiency * (data$manure_P + data$fertilizer_P) - data$crop_removal_P >= 0)

  # Set appropriate within-unit classification based on scale
  within_class <- if (scale == "county") "Within_County" else "Within_Watershed"
  data$P_class[within_condition] <- within_class

  # Summary
  class_summary <- table(data$P_class)
  message("Phosphorus classification summary (efficiency = ", p_efficiency, "):")
  for (i in 1:length(class_summary)) {
    message("  ", names(class_summary)[i], ": ", class_summary[i], " units")
  }

  return(data)
}

#' Complete Agricultural Classification Pipeline
#'
#' Run complete agricultural nutrient classification analysis for both N and P
#'
#' @param nugis_data Data frame. Raw NuGIS data
#' @param scale Character. Spatial scale: "county", "huc8", or "huc2"
#' @param cropland_threshold Numeric. Optional custom threshold
#' @param county_data Data frame. County data for threshold calculation (if needed)
#' @return Data frame with complete agricultural classifications for both nutrients
#' @export
agri_classify_complete <- function(nugis_data, scale, cropland_threshold = NULL,
                                   county_data = NULL) {

  message("Starting complete agricultural classification for ", scale, " scale...")

  # Process NuGIS data
  processed_data <- agri_process_nugis(nugis_data, scale)

  # Calculate threshold if not provided
  if (is.null(cropland_threshold)) {
    if (scale == "county") {
      cropland_threshold <- 500 * 2.47105  # 500 ha in acres
    } else {
      if (is.null(county_data)) {
        stop("County data required for threshold calculation for ", scale)
      }
      cropland_threshold <- get_cropland_threshold(scale, county_data, nugis_data)
    }
  }

  # Apply classifications for both nutrients
  classified_data <- processed_data %>%
    agri_classify_nitrogen(cropland_threshold, scale) %>%
    agri_classify_phosphorus(cropland_threshold, scale)

  message("Agricultural classification complete!")
  message("Applied threshold: ", round(cropland_threshold, 2), " acres")

  return(classified_data)
}

#' Complete Agricultural Classification Pipeline with Custom Efficiency Factors
#'
#' Run complete agricultural nutrient classification analysis for both N and P with
#' user-specified efficiency factors for sensitivity analysis.
#'
#' @param nugis_data Data frame. Raw NuGIS data
#' @param scale Character. Spatial scale: "county", "huc8", or "huc2"
#' @param cropland_threshold Numeric. Optional custom threshold
#' @param county_data Data frame. County data for threshold calculation (if needed)
#' @param n_efficiency Numeric. Nitrogen efficiency factor (default: 0.5)
#' @param p_efficiency Numeric. Phosphorus efficiency factor (default: 1.0)
#' @return Data frame with complete agricultural classifications for both nutrients
#' @export
#' @examples
#' \donttest{
#' # Load county data
#' nugis_data <- load_builtin_nugis("county", 2016)
#'
#' # Standard analysis
#' results_standard <- agri_classify_complete_custom(
#'   nugis_data, "county"
#' )
#'
#' # Sensitivity analysis with varied nitrogen efficiency
#' results_high_n <- agri_classify_complete_custom(
#'   nugis_data, "county",
#'   n_efficiency = 0.7
#' )
#'
#' # Analysis with both custom efficiencies
#' results_custom <- agri_classify_complete_custom(
#'   nugis_data, "county",
#'   n_efficiency = 0.6,
#'   p_efficiency = 0.9
#' )
#' }
agri_classify_complete_custom <- function(nugis_data, scale,
                                          cropland_threshold = NULL,
                                          county_data = NULL,
                                          n_efficiency = 0.5,
                                          p_efficiency = 1.0) {

  message("Starting complete agricultural classification with custom efficiency factors...")
  message("  Nitrogen efficiency: ", n_efficiency)
  message("  Phosphorus efficiency: ", p_efficiency)

  # Process NuGIS data
  processed_data <- agri_process_nugis(nugis_data, scale)

  # Calculate threshold if not provided
  if (is.null(cropland_threshold)) {
    if (scale == "county") {
      cropland_threshold <- 500 * 2.47105  # 500 ha in acres
    } else {
      if (is.null(county_data)) {
        stop("County data required for threshold calculation for ", scale)
      }
      cropland_threshold <- get_cropland_threshold(scale, county_data, nugis_data)
    }
  }

  # Apply classifications for both nutrients with custom efficiencies
  classified_data <- processed_data %>%
    agri_classify_nitrogen_custom(cropland_threshold, scale, n_efficiency) %>%
    agri_classify_phosphorus_custom(cropland_threshold, scale, p_efficiency)

  message("Agricultural classification complete!")
  message("Applied threshold: ", round(cropland_threshold, 2), " acres")

  return(classified_data)
}
