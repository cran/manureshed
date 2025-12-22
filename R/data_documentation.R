#' NuGIS and EPA WWTP Data Sources
#'
#' @description
#' This package uses two primary data sources:
#'
#' @section NuGIS Agricultural Data:
#' The Nutrient Use Geographic Information System (NuGIS) presents cropland
#' nutrient balances for the conterminous United States from 1987-2016.
#'
#' \strong{Source:} The Fertilizer Institute (TFI) and Plant Nutrition Canada (PNC)
#'
#' \strong{Website:} \url{https://nugis.tfi.org/tabular_data}
#'
#' \strong{Contact:} nugis@@tfi.org
#'
#' \strong{Components:}
#' \itemize{
#'   \item County-level crop and livestock data from USDA Census of Agriculture
#'   \item Fertilizer use data from AAPFCO
#'   \item Geospatial nutrient balance estimates
#'   \item Available for counties, HUC8, and HUC2 watersheds
#' }
#'
#' \strong{Data Processing:} The manureshed package uses cleaned versions of
#' NuGIS data with resolved metadata issues and enhanced spatial integration,
#' as detailed in the manureshed methodology paper (Akanbi et al., 2026).
#'
#' @section EPA WWTP Data:
# Wastewater Treatment Plant discharge data from EPAs ECHO system.
#'
#' \strong{Source:} U.S. Environmental Protection Agency
#'
#' \strong{System:} Discharge Monitoring Report (DMR) Loading Tool via ECHO
#'
#' \strong{Website:} \url{https://echo.epa.gov/trends/loading-tool/water-pollution-search}
#'
#' \strong{Data Years:} 2007-2016 (nitrogen and phosphorus loads)
#'
#' \strong{License:} Public domain (U.S. Government work)
#'
#' @section Data Attribution:
#' When using this package, please cite both the package, methodology paper, and the underlying
#' data sources. Use \code{citation_info()} to display full citation information.
#'
#' @name data_sources
#' @docType data
#' @keywords datasets
NULL
