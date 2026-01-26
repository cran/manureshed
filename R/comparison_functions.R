#' Create Classification Summary Table
#'
#' Create summary table of classification counts for both nutrients
#'
#' @param data Data frame. Data with classification columns
#' @param agricultural_col Character. Name of agricultural classification column
#' @param combined_col Character. Name of combined (with WWTP) classification column
#' @return Data frame with classification counts and changes
#' @export
create_classification_summary <- function(data, agricultural_col, combined_col) {

  # Filter out "Excluded" areas before creating summary
  data_clean <- data %>%
    dplyr::filter(
      !is.na(!!rlang::sym(agricultural_col)) &
        !is.na(!!rlang::sym(combined_col)) &
        !!rlang::sym(agricultural_col) != "Excluded" &
        !!rlang::sym(combined_col) != "Excluded"
    )

  # Get counts for each classification (excluding "Excluded")
  agri_counts <- table(data_clean[[agricultural_col]])
  combined_counts <- table(data_clean[[combined_col]])
  # Create comprehensive summary
  all_categories <- unique(c(names(agri_counts), names(combined_counts)))

  summary_df <- data.frame(
    Category = all_categories,
    Agricultural = as.numeric(agri_counts[all_categories]),
    WWTP_Combined = as.numeric(combined_counts[all_categories]),
    stringsAsFactors = FALSE
  )

  # Replace NA with 0
  summary_df[is.na(summary_df)] <- 0

  # Calculate changes
  summary_df <- summary_df %>%
    dplyr::mutate(
      Absolute_Change = WWTP_Combined - Agricultural,
      Percent_Change = ifelse(Agricultural > 0,
                              (WWTP_Combined - Agricultural) / Agricultural * 100,
                              NA),
      Impact_Ratio = ifelse(Agricultural > 0, WWTP_Combined / Agricultural, NA)
    )

  return(summary_df)
}

#' Create Before/After Comparison Plot
#'
#' Create side-by-side comparison of agricultural vs WWTP+agricultural classifications
#'
#' @param data Data frame. Summary data from create_classification_summary
#' @param nutrient Character. "nitrogen" or "phosphorus" for coloring
#' @param title Character. Plot title
#' @return ggplot object
#' @export
plot_before_after_comparison <- function(data, nutrient, title) {

  # Prepare data for plotting
  plot_data <- data %>%
    dplyr::select(Category, Agricultural, WWTP_Combined) %>%
    tidyr::pivot_longer(cols = c("Agricultural", "WWTP_Combined"),
                        names_to = "Type", values_to = "Count") %>%
    dplyr::mutate(
      Type = dplyr::case_when(
        Type == "Agricultural" ~ "Agricultural",
        Type == "WWTP_Combined" ~ "WWTP + Agricultural"
      ),
      Category = factor(Category, levels = c("Sink_Deficit", "Sink_Fertilizer",
                                             "Within_Watershed", "Within_County", "Source"))
    )

  # Set colors based on nutrient
  colors <- if (nutrient == "nitrogen") {
    c("Agricultural" = "#86C7DF", "WWTP + Agricultural" = "#5A9BD4")
  } else {
    c("Agricultural" = "#DFA086", "WWTP + Agricultural" = "#D4825A")
  }

  # Clean category names for display
  plot_data$Category <- clean_category_names(plot_data$Category)

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Category, y = Count, fill = Type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = Count),
                       position = ggplot2::position_dodge(width = 0.7),
                       vjust = -0.5, size = 2.8, fontface = "bold") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::labs(
      title = title,
      x = "",
      y = "Number of Spatial Units",
      fill = "Type: "
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = 13, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5),
      legend.position = "top",
      legend.title = ggplot2::element_text(face = "bold")
    )

  return(plot)
}

#' Create Impact Ratio Plot
#'
#' Create plot showing impact of WWTP addition as ratios
#'
#' @param data Data frame. Summary data with impact ratios
#' @param title Character. Plot title
#' @return ggplot object
#' @export
plot_impact_ratios <- function(data, title) {

  # Prepare data for impact plot
  impact_data <- data %>%
    dplyr::select(Category, Impact_Ratio) %>%
    dplyr::filter(!is.na(Impact_Ratio)) %>%
    dplyr::mutate(Category = factor(Category, levels = c("Sink_Deficit", "Sink_Fertilizer",
                                                         "Within_Watershed", "Within_County", "Source")))

  # Clean category names
  impact_data$Category <- clean_category_names(impact_data$Category)

  plot <- ggplot2::ggplot(impact_data, ggplot2::aes(x = Category, y = Impact_Ratio)) +
    ggplot2::geom_bar(stat = "identity", fill = "#66c2a5", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = round(Impact_Ratio, 2)),
                       vjust = -0.5, size = 2.8, fontface = "bold") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = title,
      subtitle = "Ratio of (WWTP + Agricultural) to (Agricultural alone)",
      x = "",
      y = "Impact Ratio"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = 13, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5)
    )

  return(plot)
}

#' Create Absolute Change Plot
#'
#' Create plot showing absolute changes in classification counts
#'
#' @param data Data frame. Summary data with absolute changes
#' @param title Character. Plot title
#' @return ggplot object
#' @export
plot_absolute_changes <- function(data, title) {

  # Prepare data for change plot
  change_data <- data %>%
    dplyr::select(Category, Absolute_Change) %>%
    dplyr::mutate(Category = factor(Category, levels = c("Sink_Deficit", "Sink_Fertilizer",
                                                         "Within_Watershed", "Within_County", "Source")))

  # Clean category names
  change_data$Category <- clean_category_names(change_data$Category)

  plot <- ggplot2::ggplot(change_data, ggplot2::aes(x = Category, y = Absolute_Change)) +
    ggplot2::geom_bar(stat = "identity",
                      fill = ifelse(change_data$Absolute_Change >= 0, "#2166ac", "#d73027"),
                      width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = Absolute_Change),
                       vjust = ifelse(change_data$Absolute_Change >= 0, -0.5, 1.2),
                       size = 2.8, fontface = "bold") +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    ggplot2::labs(
      title = title,
      subtitle = "Positive values indicate increases, negative values indicate decreases",
      x = "",
      y = "Change in Number of Spatial Units"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = 13, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5)
    )

  return(plot)
}



#' Compare Multiple Analysis Scenarios
#'
#' Compare results from multiple analysis runs side-by-side with visualizations
#' and summary statistics.
#'
#' @param scenario_list Named list of analysis results from run_builtin_analysis()
#' @param metrics Character vector of metrics to compare. Options: "sources",
#'   "sinks", "balanced", "excluded", "total_surplus", "total_deficit"
#' @param create_plots Logical. Create comparison plots? (default: TRUE)
#' @param output_dir Character. Directory for saving plots (default: NULL, no save)
#'
#' @return List containing:
#'   \item{comparison_data}{Data frame with metrics for each scenario}
#'   \item{plots}{List of ggplot objects (if create_plots = TRUE)}
#'   \item{summary}{Summary statistics}
#'
#' @export
#' @examples
#' \donttest{
#' # Create multiple scenarios
#' base <- run_builtin_analysis(year = 2016, include_wwtp = FALSE,
#'                               scale = "county", nutrients = "nitrogen")
#' wwtp <- run_builtin_analysis(year = 2016, include_wwtp = TRUE,
#'                               scale = "county", nutrients = "nitrogen")
#'
#' # Compare scenarios
#' comparison <- compare_scenarios(list(
#'   "Base (Agricultural Only)" = base,
#'   "With WWTP" = wwtp
#' ))
#'
#' # View comparison data
#' print(comparison$comparison_data)
#'
#' # View plots
#' print(comparison$plots$bar_chart)
#' }
compare_scenarios <- function(scenario_list,
                              metrics = c("sources", "sinks", "balanced", "excluded"),
                              create_plots = TRUE,
                              output_dir = NULL) {

  # Validate inputs
  if (!is.list(scenario_list) || length(scenario_list) < 2) {
    stop("scenario_list must be a named list with at least 2 scenarios", call. = FALSE)
  }

  if (is.null(names(scenario_list))) {
    names(scenario_list) <- paste("Scenario", seq_along(scenario_list))
  }

  # Extract metrics from each scenario
  comparison_data <- data.frame()

  for (scenario_name in names(scenario_list)) {
    result <- scenario_list[[scenario_name]]

    # Extract metrics
    scenario_metrics <- extract_scenario_metrics(result)
    scenario_metrics$scenario <- scenario_name

    comparison_data <- rbind(comparison_data, scenario_metrics)
  }

  # Create summary
  summary_stats <- create_comparison_summary(comparison_data, metrics)

  # Create plots if requested
  plots <- NULL
  if (create_plots) {
    plots <- create_comparison_plots(comparison_data, metrics)

    # Save plots if output directory provided
    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      for (plot_name in names(plots)) {
        plot_file <- file.path(output_dir, paste0("comparison_", plot_name, ".png"))
        save_plot(plots[[plot_name]], plot_file, width = 12, height = 8)
      }
    }
  }

  # Return results
  result_list <- list(
    comparison_data = comparison_data,
    summary = summary_stats,
    plots = plots
  )

  # Print summary

  message("Number of scenarios:", length(scenario_list), "\n")

  for (name in names(scenario_list)) {
    message("  -", name, "\n")
  }
  message("\nKey differences:\n")
  print(summary_stats$differences)


  return(result_list)
}


#' Extract Metrics from Analysis Results
#'
#' Internal function to extract comparison metrics from a single analysis result
#'
#' @param result Analysis result from run_builtin_analysis()
#' @return Data frame with metrics
#' @keywords internal
extract_scenario_metrics <- function(result) {

  # Check if WWTP is included - if so, use integrated data for primary metrics
  has_wwtp <- "integrated" %in% names(result)

  if (has_wwtp && "nitrogen" %in% names(result$integrated)) {
    # Use combined data as primary metrics
    primary_data <- result$integrated$nitrogen
    n_sources <- sum(primary_data$combined_N_class == "Source", na.rm = TRUE)
    n_sinks <- sum(primary_data$combined_N_class %in% c("Sink_Deficit", "Sink_Fertilizer"), na.rm = TRUE)
    n_balanced <- sum(primary_data$combined_N_class == "Balanced", na.rm = TRUE)
    n_excluded <- sum(primary_data$combined_N_class == "Excluded", na.rm = TRUE)
    total_surplus <- sum(primary_data$combined_N_surplus[primary_data$combined_N_class == "Source"], na.rm = TRUE)
    total_deficit <- abs(sum(primary_data$combined_N_surplus[primary_data$combined_N_class %in%
                                                               c("Sink_Deficit", "Sink_Fertilizer")], na.rm = TRUE))
  } else {
    # Use agricultural data as primary metrics
    ag_data <- result$agricultural
    n_sources <- sum(ag_data$N_class == "Source", na.rm = TRUE)
    n_sinks <- sum(ag_data$N_class %in% c("Sink_Deficit", "Sink_Fertilizer"), na.rm = TRUE)
    n_balanced <- sum(ag_data$N_class == "Balanced", na.rm = TRUE)
    n_excluded <- sum(ag_data$N_class == "Excluded", na.rm = TRUE)
    total_surplus <- sum(ag_data$N_surplus[ag_data$N_class == "Source"], na.rm = TRUE)
    total_deficit <- abs(sum(ag_data$N_surplus[ag_data$N_class %in%
                                                 c("Sink_Deficit", "Sink_Fertilizer")], na.rm = TRUE))
  }

  # Keep agricultural data separate for reference
  ag_data <- result$agricultural

  # Store agricultural-only metrics for reference
  n_sources_ag <- sum(ag_data$N_class == "Source", na.rm = TRUE)
  n_sinks_ag <- sum(ag_data$N_class %in% c("Sink_Deficit", "Sink_Fertilizer"), na.rm = TRUE)
  n_balanced_ag <- sum(ag_data$N_class == "Balanced", na.rm = TRUE)
  total_surplus_ag <- sum(ag_data$N_surplus[ag_data$N_class == "Source"], na.rm = TRUE)
  total_deficit_ag <- abs(sum(ag_data$N_surplus[ag_data$N_class %in%
                                                  c("Sink_Deficit", "Sink_Fertilizer")], na.rm = TRUE))

  if (has_wwtp && "nitrogen" %in% names(result$integrated)) {
    int_data <- result$integrated$nitrogen

    n_sources_combined <- sum(int_data$combined_N_class == "Source", na.rm = TRUE)
    n_sinks_combined <- sum(int_data$combined_N_class %in%
                              c("Sink_Deficit", "Sink_Fertilizer"), na.rm = TRUE)
    n_balanced_combined <- sum(int_data$combined_N_class == "Balanced", na.rm = TRUE)

    total_surplus_combined <- sum(int_data$combined_N_surplus[
      int_data$combined_N_class == "Source"], na.rm = TRUE)
    total_deficit_combined <- abs(sum(int_data$combined_N_surplus[
      int_data$combined_N_class %in% c("Sink_Deficit", "Sink_Fertilizer")], na.rm = TRUE))
  } else {
    n_sources_combined <- NA
    n_sinks_combined <- NA
    n_balanced_combined <- NA
    total_surplus_combined <- NA
    total_deficit_combined <- NA
  }

  # Create metrics data frame
  # Create metrics data frame
  metrics <- data.frame(
    n_sources = n_sources,
    n_sinks = n_sinks,
    n_balanced = n_balanced,
    n_excluded = n_excluded,
    total_surplus_kg = total_surplus,
    total_deficit_kg = total_deficit,
    n_sources_ag = n_sources_ag,          # ADD
    n_sinks_ag = n_sinks_ag,              # ADD
    total_surplus_ag_kg = total_surplus_ag,  # ADD
    total_deficit_ag_kg = total_deficit_ag,  # ADD
    has_wwtp = has_wwtp,
    total_units = nrow(ag_data),
    scale = result$parameters$scale,
    year = result$parameters$year,
    stringsAsFactors = FALSE
  )

  return(metrics)
}


#' Create Comparison Summary Statistics
#'
#' @param comparison_data Data frame with scenario metrics
#' @param metrics Character vector of metrics to summarize
#' @return List with summary statistics
#' @keywords internal
create_comparison_summary <- function(comparison_data, metrics) {

  # Calculate differences between first and other scenarios
  base_scenario <- comparison_data[1, ]

  differences <- data.frame()

  for (i in 2:nrow(comparison_data)) {
    scenario <- comparison_data[i, ]

    diff_row <- data.frame(
      scenario = scenario$scenario,
      delta_sources = scenario$n_sources - base_scenario$n_sources,
      delta_sinks = scenario$n_sinks - base_scenario$n_sinks,
      delta_surplus = scenario$total_surplus_kg - base_scenario$total_surplus_kg,
      delta_deficit = scenario$total_deficit_kg - base_scenario$total_deficit_kg,
      pct_change_sources = (scenario$n_sources - base_scenario$n_sources) /
        base_scenario$n_sources * 100,
      stringsAsFactors = FALSE
    )

    differences <- rbind(differences, diff_row)
  }

  return(list(
    base_scenario = base_scenario$scenario,
    differences = differences,
    total_scenarios = nrow(comparison_data)
  ))
}


#' Create Comparison Plots
#'
#' @param comparison_data Data frame with scenario metrics
#' @param metrics Character vector of metrics to plot
#' @return List of ggplot objects
#' @keywords internal
create_comparison_plots <- function(comparison_data, metrics) {

  plots <- list()

  # 1. Bar chart of classification counts
  class_data <- comparison_data[, c("scenario", "n_sources", "n_sinks",
                                    "n_balanced", "n_excluded")]
  class_long <- tidyr::pivot_longer(
    class_data,
    cols = c("n_sources", "n_sinks", "n_balanced", "n_excluded"),
    names_to = "classification",
    values_to = "count"
  )

  class_long$classification <- factor(
    class_long$classification,
    levels = c("n_sources", "n_sinks", "n_balanced", "n_excluded"),
    labels = c("Sources", "Sinks", "Balanced", "Excluded")
  )

  plots$bar_chart <- ggplot2::ggplot(class_long,
                                     ggplot2::aes(x = scenario, y = count,
                                                  fill = classification)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_manual(values = c(
      "Sources" = "#d62728",
      "Sinks" = "#1f77b4",
      "Balanced" = "#2ca02c",
      "Excluded" = "#7f7f7f"
    )) +
    ggplot2::labs(
      title = "Classification Counts by Scenario",
      x = "Scenario",
      y = "Number of Units",
      fill = "Classification"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  # 2. Surplus/Deficit comparison
  surplus_data <- comparison_data[, c("scenario", "total_surplus_kg", "total_deficit_kg")]
  surplus_long <- tidyr::pivot_longer(
    surplus_data,
    cols = c("total_surplus_kg", "total_deficit_kg"),
    names_to = "type",
    values_to = "amount_kg"
  )

  surplus_long$type <- factor(
    surplus_long$type,
    levels = c("total_surplus_kg", "total_deficit_kg"),
    labels = c("Total Surplus", "Total Deficit")
  )

  plots$surplus_deficit <- ggplot2::ggplot(surplus_long,
                                           ggplot2::aes(x = scenario, y = amount_kg/1000,
                                                        fill = type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_manual(values = c(
      "Total Surplus" = "#d62728",
      "Total Deficit" = "#1f77b4"
    )) +
    ggplot2::labs(
      title = "Total Nitrogen Surplus and Deficit by Scenario",
      x = "Scenario",
      y = "Amount (Metric Tons)",
      fill = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )

  # 3. Percentage change plot (if > 2 scenarios)
  if (nrow(comparison_data) > 1) {
    base_sources <- comparison_data$n_sources[1]
    base_sinks <- comparison_data$n_sinks[1]

    pct_change <- data.frame(
      scenario = comparison_data$scenario[-1],
      sources_pct = (comparison_data$n_sources[-1] - base_sources) / base_sources * 100,
      sinks_pct = (comparison_data$n_sinks[-1] - base_sinks) / base_sinks * 100
    )

    pct_long <- tidyr::pivot_longer(
      pct_change,
      cols = c("sources_pct", "sinks_pct"),
      names_to = "type",
      values_to = "pct_change"
    )

    pct_long$type <- factor(
      pct_long$type,
      levels = c("sources_pct", "sinks_pct"),
      labels = c("Sources", "Sinks")
    )

    plots$percent_change <- ggplot2::ggplot(pct_long,
                                            ggplot2::aes(x = scenario, y = pct_change,
                                                         fill = type)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::scale_fill_manual(values = c(
        "Sources" = "#d62728",
        "Sinks" = "#1f77b4"
      )) +
      ggplot2::labs(
        title = paste("Percent Change from", comparison_data$scenario[1]),
        x = "Scenario",
        y = "Percent Change (%)",
        fill = ""
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        legend.position = "bottom"
      )
  }

  return(plots)
}
