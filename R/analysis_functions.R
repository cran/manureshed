#' Calculate Spatial Transition Probabilities
#'
#' Calculate transition probabilities between adjacent spatial units
#'
#' @param data Data frame. Data with classification and coordinate columns
#' @param class_column Character. Name of classification column
#' @param longitude_col Character. Name of longitude column (default: "longitude")
#' @param latitude_col Character. Name of latitude column (default: "latitude")
#' @return Data frame with transition probabilities as percentages
#' @export
calculate_transition_probabilities <- function(data, class_column,
                                               longitude_col = "longitude",
                                               latitude_col = "latitude") {

  # Filter out excluded areas
  data_filtered <- data[data[[class_column]] != "Excluded", ]

  # Extract unique categories and convert to numeric
  categories <- sort(unique(data_filtered[[class_column]]))
  num_categories <- length(categories)
  final_categories <- as.numeric(factor(data_filtered[[class_column]], levels = categories))

  # Initialize transition matrix
  transition_matrix <- matrix(0, nrow = num_categories, ncol = num_categories)

  # Get grid dimensions
  nx <- length(unique(data_filtered[[longitude_col]]))
  ny <- length(unique(data_filtered[[latitude_col]]))

  # Compute transitions between adjacent cells
  for (i in 1:(nx - 1)) {
    for (j in 1:(ny - 1)) {
      index <- (j - 1) * nx + i
      if (index > length(final_categories)) next

      current_cat <- final_categories[index]
      if (is.na(current_cat)) next

      if (current_cat >= 1 && current_cat <= num_categories) {
        # Check right neighbor
        right_index <- index + 1
        if (right_index <= length(final_categories)) {
          right_cat <- final_categories[right_index]
          if (!is.na(right_cat) && right_cat >= 1 && right_cat <= num_categories &&
              right_cat != current_cat) {
            transition_matrix[current_cat, right_cat] <- transition_matrix[current_cat, right_cat] + 1
            transition_matrix[right_cat, current_cat] <- transition_matrix[right_cat, current_cat] + 1
          }
        }

        # Check below neighbor
        below_index <- index + nx
        if (below_index <= length(final_categories)) {
          below_cat <- final_categories[below_index]
          if (!is.na(below_cat) && below_cat >= 1 && below_cat <= num_categories &&
              below_cat != current_cat) {
            transition_matrix[current_cat, below_cat] <- transition_matrix[current_cat, below_cat] + 1
            transition_matrix[below_cat, current_cat] <- transition_matrix[below_cat, current_cat] + 1
          }
        }
      }
    }
  }

  # Normalize to percentages
  row_sums <- rowSums(transition_matrix)
  row_sums[row_sums == 0] <- 1
  transition_probs <- transition_matrix / row_sums
  transition_percentages <- round(transition_probs * 100, 2)

  # Convert to data frame with proper names
  transition_df <- as.data.frame(transition_percentages)
  names(transition_df) <- categories
  row.names(transition_df) <- categories

  return(transition_df)
}

#' Create Network Plot from Transition Probabilities
#'
#' Create network visualization of spatial transition probabilities
#'
#' @param transition_df Data frame. Transition probability matrix
#' @param nutrient Character. "nitrogen" or "phosphorus" for coloring
#' @param analysis_type Character. Description of analysis type
#' @param output_path Character. Path to save PNG file
#' @param highlight_transitions Logical. Whether to highlight specific transitions
#' @return NULL (saves plot to file)
#' @export
create_network_plot <- function(transition_df, nutrient, analysis_type, output_path,
                                highlight_transitions = TRUE) {

  # Clean category names for display
  clean_categories <- sapply(colnames(transition_df), function(name) {
    if (name != "Source") {
      name <- gsub("_", " ", name)
    }
    return(name)
  })

  # Format for network display
  clean_categories <- gsub("Sink Deficit", "Sink\nDeficit", clean_categories)
  clean_categories <- gsub("Sink Fertilizer", "Sink\nFertilizer", clean_categories)
  clean_categories <- gsub("Within Watershed", "Within\nWatershed", clean_categories)
  clean_categories <- gsub("Within County", "Within\nCounty", clean_categories)

  # Create network
  net <- igraph::graph_from_adjacency_matrix(
    as.matrix(transition_df),
    mode = "directed",
    weighted = TRUE
  )

  # Set vertex names
  igraph::V(net)$name <- clean_categories

  # Initialize edge properties
  igraph::E(net)$width <- 0.8
  igraph::E(net)$color <- "gray50"
  igraph::E(net)$label <- paste0(round(igraph::E(net)$weight, 1), "%")
  igraph::E(net)$label.color <- "gray30"
  igraph::E(net)$label.cex <- 0.9

  # Highlight specific transitions if requested
  if (highlight_transitions) {
    source_index <- which(clean_categories == "Source")
    sink_deficit_indices <- which(clean_categories %in% c("Sink\nDeficit"))
    sink_fertilizer_indices <- which(clean_categories %in% c("Sink\nFertilizer"))

    all_edges <- igraph::get.edges(net, igraph::E(net))

    for (i in 1:length(igraph::E(net))) {
      edge <- all_edges[i,]
      is_target_transition <- FALSE

      # Check for Source <-> Sink transitions
      if (length(source_index) > 0 && length(sink_deficit_indices) > 0) {
        if ((edge[1] == source_index && edge[2] %in% sink_deficit_indices) ||
            (edge[1] %in% sink_deficit_indices && edge[2] == source_index)) {
          is_target_transition <- TRUE
        }
      }

      if (length(source_index) > 0 && length(sink_fertilizer_indices) > 0) {
        if ((edge[1] == source_index && edge[2] %in% sink_fertilizer_indices) ||
            (edge[1] %in% sink_fertilizer_indices && edge[2] == source_index)) {
          is_target_transition <- TRUE
        }
      }

      if (is_target_transition) {
        igraph::E(net)$width[i] <- 1.2
        igraph::E(net)$color[i] <- "#D73027"
        igraph::E(net)$label.color[i] <- "#D73027"
        igraph::E(net)$label.cex[i] <- 1.5
      }
    }
  }

  # Simplify network
  net <- igraph::simplify(net, remove.multiple = FALSE, remove.loops = TRUE)

  # Set colors based on nutrient
  vertex_color <- if (nutrient == "nitrogen") "#415fb8" else "#8338bb"

  # Create titles
  main_title <- "Manureshed Spatial Transition Probability Network"
  subtitle <- paste("Focus:", analysis_type)

  # Create high-resolution plot
  grDevices::png(output_path, width = 3000, height = 3000, res = 300)

  # Save and restore par settings IMMEDIATELY after change
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar), add = TRUE)

  # Now set the new parameters
  graphics::par(mar = c(0.5, 0.5, 2.5, 0.5))

  plot(net,
       layout = igraph::layout_in_circle(net),
       edge.arrow.size = 1.8,
       edge.curved = 0.25,
       vertex.color = vertex_color,
       vertex.size = 60,
       vertex.label.color = "white",
       vertex.label.cex = 2.0,
       vertex.label.font = 2,
       vertex.frame.color = "gray20",
       vertex.frame.width = 2,
       vertex.label.family = "Arial",
       edge.label = igraph::E(net)$label,
       edge.label.cex = igraph::E(net)$label.cex,
       edge.label.color = igraph::E(net)$label.color,
       edge.label.font = 2,
       edge.label.family = "Arial",
       edge.color = igraph::E(net)$color,
       edge.width = igraph::E(net)$width,
       main = ""
  )

  # Add titles
  graphics::title(main = main_title, line = 0.3, cex.main = 1.8, font.main = 2, family = "Arial")
  graphics::title(main = subtitle, line = -1.0, cex.main = 1.3, font.main = 1, family = "Arial")

  grDevices::dev.off()

  message("Created network plot: ", output_path)  # Also fixed cat() to message()
}


#' Add Centroid Coordinates to Spatial Data
#'
#' Calculate centroid coordinates for spatial units
#'
#' @param spatial_data sf object. Spatial data
#' @return Data frame with centroid coordinates added
#' @export
add_centroid_coordinates <- function(spatial_data) {
  centroids <- sf::st_centroid(spatial_data$geometry)
  centroids_4326 <- sf::st_transform(centroids, 4326)
  coords <- sf::st_coordinates(centroids_4326)

  result <- spatial_data %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      longitude = coords[,1],
      latitude = coords[,2]
    )

  return(result)
}
