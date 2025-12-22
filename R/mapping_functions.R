# ==============================================================================
# R/mapping_functions.R - Enhanced Mapping Functions with Improved Spacing
# ==============================================================================

#' Get State Boundaries for Mapping
#'
#' Get US state boundaries excluding non-CONUS states
#'
#' @return sf object with state boundaries
#' @export
get_state_boundaries <- function() {
  states_sf <- tigris::states(cb = TRUE) %>%
    dplyr::filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "MP", "GU", "AS")) %>%
    sf::st_transform(crs = MANURESHED_CRS)

  return(states_sf)
}

#' Create Agricultural Classification Map
#'
#' Create map showing agricultural nutrient classifications
#'
#' @param data sf object. Spatial data with classifications
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @param classification_col Character. Name of classification column
#' @param title Character. Map title
#' @return ggplot object
#' @export
map_agricultural_classification <- function(data, nutrient, classification_col, title) {

  # Get appropriate colors
  colors <- get_nutrient_colors(nutrient)

  # Get state boundaries for context
  states_sf <- get_state_boundaries()

  # Create labels
  labels <- clean_category_names(names(colors))
  names(labels) <- names(colors)

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = states_sf,
                     fill = "white",
                     color = "gray80",
                     size = 0.2) +
    ggplot2::geom_sf(data = data,
                     ggplot2::aes(fill = !!rlang::sym(classification_col)),
                     color = "white",
                     size = 0.1) +
    ggplot2::scale_fill_manual(
      values = colors,
      labels = labels,
      name = "Classification"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5,
                                         vjust = -5, margin = ggplot2::margin(b = 20)),  # FIXED: Added bottom margin
      legend.position = "right",
      legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 13, face = "bold",
                                           margin = ggplot2::margin(r = 15)),  # FIXED: Added right margin
      legend.box.spacing = ggplot2::unit(-60, "pt"),  # or even -40 for more space
      plot.margin = ggplot2::margin(t = 30, r = 10, b = 10, l = 10)  # FIXED: Added top margin for title
    ) +
    ggplot2::labs(title = title)

  return(map)
}

#' Create WWTP Point Map
#'
#' Create map showing WWTP locations classified by load size
#'
#' @param wwtp_sf sf object. Spatial WWTP data with classifications
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @param title Character. Map title
#' @return ggplot object
#' @export
map_wwtp_points <- function(wwtp_sf, nutrient, title) {

  # Get state boundaries
  states_sf <- get_state_boundaries()

  # Order factor levels for consistent display
  wwtp_sf$source_class <- factor(wwtp_sf$source_class,
                                 levels = c("Minor Source", "Small Source", "Medium Source",
                                            "Large Source", "Very Large Source"))

  # Define load ranges for labels based on nutrient
  if (nutrient == "nitrogen") {
    labels <- c("minor source (<10)", "small source (10-50)", "medium source (50-150)",
                "large source (150-1000)", "very large source (1000+)")
  } else {
    labels <- c("minor source (<1)", "small source (1-5)", "medium source (5-15)",
                "large source (15-100)", "very large source (100+)")
  }

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = states_sf,
                     fill = "white",
                     color = "gray80",
                     size = 0.2) +
    ggplot2::geom_sf(data = wwtp_sf,
                     ggplot2::aes(color = source_class),
                     alpha = 0.7) +
    ggplot2::scale_color_viridis_d(
      option = if (nutrient == "nitrogen") "mako" else "viridis",
      direction = -1,
      name = "Source Classification\n(tons/yr)",  # FIXED: Added line break for better spacing
      labels = labels
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 17, face = "bold", hjust = 0.5,
                                         vjust = -10, margin = ggplot2::margin(b = 25)),  # FIXED: Increased spacing
      plot.subtitle = ggplot2::element_text(size = 15, hjust = 0.5,
                                            vjust = -11, margin = ggplot2::margin(b = 20)),  # FIXED: Added margin
      legend.position = "right",
      legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 13, face = "bold",
                                           margin = ggplot2::margin(r = 15, b = 10)),  # FIXED: Added margins
      legend.box.spacing = ggplot2::unit(-40, "pt"),  # or even -40 for more space
      plot.margin = ggplot2::margin(t = 40, r = 10, b = 10, l = 10)  # FIXED: Increased top margin
    ) +
    ggplot2::labs(title = title)

  return(map)
}

#' Create WWTP Influence Map
#'
#' Create map showing WWTP contribution as proportion of total nutrient load
#'
#' @param data sf object. Integrated data with WWTP proportions
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @param title Character. Map title
#' @return ggplot object
#' @export
map_wwtp_influence <- function(data, nutrient, title) {

  # Get state boundaries
  states_sf <- get_state_boundaries()

  # Determine proportion column
  prop_col <- if (nutrient == "nitrogen") "wwtp_proportion_N" else "wwtp_proportion_P"

  # Choose appropriate viridis option
  viridis_option <- if (nutrient == "nitrogen") "turbo" else "cividis"

  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = states_sf,
                     fill = "white",
                     color = "gray80",
                     size = 0.2) +
    ggplot2::geom_sf(data = data,
                     ggplot2::aes(fill = !!rlang::sym(prop_col)),
                     color = "white",
                     size = 0.1) +
    ggplot2::scale_fill_viridis_c(option = viridis_option,
                                  name = "WWTP\nProportion",  # FIXED: Added line break
                                  labels = scales::percent_format(accuracy = 1)) +  # FIXED: Better formatting
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 17, face = "bold", hjust = 0.5,
                                         vjust = -5, margin = ggplot2::margin(b = 20)),  # FIXED: Added margin
      legend.position = "right",
      legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 13, face = "bold",
                                           margin = ggplot2::margin(r = 15, b = 10)),  # FIXED: Added margins
      legend.box.spacing = ggplot2::unit(-40, "pt"),  # or even -40 for more space
      plot.margin = ggplot2::margin(t = 30, r = 10, b = 10, l = 10)  # FIXED: Added top margin
    ) +
    ggplot2::labs(title = title)

  return(map)
}
