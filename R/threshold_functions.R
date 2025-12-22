#' Calculate Cropland Threshold for Exclusion
#'
#' Calculate cropland threshold for excluding small agricultural areas
#' Uses county 500ha baseline to determine percentile for other scales
#'
#' @param county_data Data frame. County-level NuGIS data with cropland column
#' @param target_data Data frame. Target scale data (HUC8, HUC2) with cropland column
#' @param county_cropland_col Character. Name of cropland column in county data
#' @param target_cropland_col Character. Name of cropland column in target data
#' @param baseline_ha Numeric. Baseline cropland in hectares for exclusion (default: 500)
#' @return Numeric. Threshold value for target scale
#' @export
calculate_cropland_threshold <- function(county_data, target_data,
                                         county_cropland_col, target_cropland_col,
                                         baseline_ha = 500) {
  # Convert baseline hectares to acres
  baseline_acres <- baseline_ha * 2.47105

  # Calculate percentile in county data
  county_cropland_percentile <- ecdf(county_data[[county_cropland_col]])
  percentile_baseline <- county_cropland_percentile(baseline_acres)

  # Apply same percentile to target scale data
  threshold <- quantile(target_data[[target_cropland_col]],
                        probs = percentile_baseline, na.rm = TRUE)

  message("Calculated cropland threshold:")
  message("  County baseline: ", baseline_ha, " ha (", round(baseline_acres, 2), " acres)")
  message("  Percentile in county data: ", round(percentile_baseline * 100, 2), "%")
  message("  Threshold for target scale: ", round(threshold, 2), " acres")

  return(threshold)
}

#' Get Cropland Threshold by Scale
#'
#' Get appropriate cropland threshold based on spatial scale
#'
#' @param scale Character. Spatial scale: "county", "huc8", or "huc2"
#' @param county_data Data frame. County-level data (required for huc8/huc2)
#' @param target_data Data frame. Target scale data (required for huc8/huc2)
#' @param baseline_ha Numeric. Baseline for county exclusion (default: 500)
#' @return Numeric. Threshold value
#' @export
get_cropland_threshold <- function(scale, county_data = NULL, target_data = NULL,
                                   baseline_ha = 500) {
  if (scale == "county") {
    threshold <- baseline_ha * 2.47105  # Convert to acres
    message("Using county baseline threshold: ", round(threshold, 2), " acres")
    return(threshold)
  } else if (scale %in% c("huc8", "huc2")) {
    if (is.null(county_data) || is.null(target_data)) {
      stop("County and target data required for ", scale, " threshold calculation")
    }

    # Determine column names based on scale
    county_col <- "cropland"
    target_col <- "cropland"

    return(calculate_cropland_threshold(county_data, target_data,
                                        county_col, target_col, baseline_ha))
  } else {
    stop("Scale must be 'county', 'huc8', or 'huc2'")
  }
}
