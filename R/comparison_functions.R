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
