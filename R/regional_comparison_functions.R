#' Get Built-in Region Definitions
#'
#' @return List of region definitions
#' @export
get_region_definitions <- function() {
  list(
    corn_belt = c("IA", "IL", "IN", "OH", "MO"),
    dairy_belt = c("WI", "MN", "MI", "NY", "PA"),
    great_plains = c("ND", "SD", "NE", "KS", "OK", "TX"),
    southeast = c("NC", "SC", "GA", "FL", "AL", "MS", "LA", "AR"),
    northeast = c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "PA", "NJ"),
    pacific = c("WA", "OR", "CA"),
    mountain = c("MT", "ID", "WY", "NV", "UT", "CO", "AZ", "NM")
  )
}

#' Compare Multiple Regions
#'
#' @param regions Named list of state vectors, or character vector of built-in region names
#' @param scale Spatial scale
#' @param year Year to analyze
#' @param nutrients Nutrients to analyze
#' @param include_wwtp Include WWTP?
#' @param verbose Show messages?
#' @return List with regional comparison results
#' @export
#' @examples
#' \dontrun{
#' # Compare built-in regions
#' comparison <- compare_regions(
#'   regions = c("corn_belt", "dairy_belt"),
#'   scale = "county",
#'   year = 2016,
#'   nutrients = "nitrogen"
#' )
#'
#' # Compare custom regions
#' comparison <- compare_regions(
#'   regions = list(
#'     "Upper Midwest" = c("WI", "MN", "MI"),
#'     "Lower Midwest" = c("IL", "IN", "OH")
#'   ),
#'   scale = "county",
#'   year = 2016
#' )
#'
#' # View comparison
#' print(comparison$summary)
#' comparison$plots$bar_chart
#' }
compare_regions <- function(regions,
                            scale = "county",
                            year = 2016,
                            nutrients = "nitrogen",
                            include_wwtp = TRUE,
                            verbose = TRUE) {

  # Get built-in regions if character vector provided
  if (is.character(regions)) {
    region_defs <- get_region_definitions()
    regions <- region_defs[regions]

    # Check all requested regions exist
    invalid <- names(regions)[sapply(regions, is.null)]
    if (length(invalid) > 0) {
      stop(
        "Unknown region(s): ", paste(invalid, collapse = ", "), "\n\n",
        "Available regions: ", paste(names(region_defs), collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (verbose) message("Analyzing ", length(regions), " regions...\n")

  # Run analysis for each region
  regional_results <- list()

  for (region_name in names(regions)) {
    if (verbose) message("Processing region: ", region_name)

    states <- regions[[region_name]]

    # Combine states
    state_data <- list()
    for (state in states) {
      state_result <- run_state_analysis(
        state = state,
        scale = scale,
        year = year,
        nutrients = nutrients,
        include_wwtp = include_wwtp,
        verbose = FALSE
      )
      state_data[[state]] <- state_result$agricultural
    }

    # Combine state data
    combined_data <- do.call(rbind, state_data)

    regional_results[[region_name]] <- list(
      data = combined_data,
      states = states
    )
  }

  # Create comparison summary
  summary_data <- create_regional_summary(regional_results, nutrients)

  # Create comparison plots
  plots <- create_regional_plots(summary_data, nutrients)

  return(list(
    results = regional_results,
    summary = summary_data,
    plots = plots,
    parameters = list(
      regions = names(regions),
      scale = scale,
      year = year,
      nutrients = nutrients
    )
  ))
}

#' Create Regional Summary
#'
#' @keywords internal
create_regional_summary <- function(regional_results, nutrients) {

  summary_list <- list()

  for (region_name in names(regional_results)) {
    data <- regional_results[[region_name]]$data

    for (nutrient in nutrients) {
      class_col <- paste0(toupper(substr(nutrient, 1, 1)), "_class")
      surplus_col <- paste0(toupper(substr(nutrient, 1, 1)), "_surplus")

      summary_list[[length(summary_list) + 1]] <- data.frame(
        region = region_name,
        nutrient = nutrient,
        n_total = nrow(data),
        n_sources = sum(data[[class_col]] == "Source", na.rm = TRUE),
        n_sinks = sum(data[[class_col]] %in% c("Sink_Deficit", "Sink_Fertilizer"),
                      na.rm = TRUE),
        n_balanced = sum(data[[class_col]] %in% c("Balanced", "Within_Watershed",
                                                  "Within_County"), na.rm = TRUE),
        total_surplus = sum(data[[surplus_col]][data[[class_col]] == "Source"],
                            na.rm = TRUE),
        total_deficit = abs(sum(data[[surplus_col]][
          data[[class_col]] %in% c("Sink_Deficit", "Sink_Fertilizer")], na.rm = TRUE)),
        avg_cropland = mean(data$cropland, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }

  summary_df <- do.call(rbind, summary_list)

  # Add percentages
  summary_df$pct_sources <- summary_df$n_sources / summary_df$n_total * 100
  summary_df$pct_sinks <- summary_df$n_sinks / summary_df$n_total * 100

  return(summary_df)
}

#' Create Regional Comparison Plots
#'
#' @keywords internal
create_regional_plots <- function(summary_data, nutrients) {

  plots <- list()

  # Bar chart of sources vs sinks
  plot_data <- summary_data %>%
    tidyr::pivot_longer(
      cols = c("n_sources", "n_sinks", "n_balanced"),
      names_to = "type",
      values_to = "count"
    )

  plots$bar_chart <- ggplot2::ggplot(plot_data,
                                     ggplot2::aes(x = region, y = count, fill = type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_manual(
      values = c("n_sources" = "#d62728", "n_sinks" = "#1f77b4",
                 "n_balanced" = "#2ca02c"),
      labels = c("Sources", "Sinks", "Balanced")
    ) +
    ggplot2::labs(
      title = "Regional Comparison of Nutrient Classifications",
      x = "Region",
      y = "Number of Units",
      fill = "Classification"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Surplus/Deficit comparison
  plots$surplus_deficit <- ggplot2::ggplot(summary_data,
                                           ggplot2::aes(x = region)) +
    ggplot2::geom_bar(ggplot2::aes(y = total_surplus/1000),
                      stat = "identity", fill = "#d62728", alpha = 0.7) +
    ggplot2::geom_bar(ggplot2::aes(y = -total_deficit/1000),
                      stat = "identity", fill = "#1f77b4", alpha = 0.7) +
    ggplot2::labs(
      title = "Regional Surplus and Deficit",
      x = "Region",
      y = "Amount (Metric Tons)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(plots)
}

#' List Available Regions
#'
#' @export
list_regions <- function() {
  regions <- get_region_definitions()

  cat("Built-in Regions:\n")
  cat("==================\n\n")

  for (region_name in names(regions)) {
    states <- regions[[region_name]]
    cat(sprintf("%-15s: %s\n", region_name, paste(states, collapse = ", ")))
  }

  cat("\nUsage:\n")
  cat('  compare_regions(regions = c("corn_belt", "dairy_belt"), ...)\n')
  cat("\nOr define custom regions:\n")
  cat('  compare_regions(regions = list("My Region" = c("OH", "IN")), ...)\n')
}
