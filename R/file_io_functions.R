#' Save Spatial Data
#'
#' Save spatial data to RDS file with standardized naming
#'
#' @param data sf object. Spatial data to save
#' @param file_path Character. Output file path (should end in .rds). If NULL, auto-generated
#' @param scale Character. Spatial scale for file naming
#' @param nutrient Character. Nutrient type for file naming ("nitrogen", "phosphorus", or "both")
#' @param analysis_type Character. Analysis type for file naming
#' @param year Numeric. Year for file naming
#' @return Character. Path to saved file
#' @export
#' @examples
#' \donttest{
#' # Create some example results first
#' results <- run_builtin_analysis(scale = "county", year = 2016)
#'
#' # Save with auto-generated filename
#' save_spatial_data(results$agricultural, scale = "county", year = 2016)
#'
#' # Save with custom filename
#' save_spatial_data(results$agricultural,
#'                   file.path(tempdir(), "my_results.rds"))
#' }
save_spatial_data <- function(data, file_path = NULL, scale = "huc8",
                              nutrient = "both", analysis_type = "combined",
                              year = format(Sys.Date(), "%Y")) {

  if (is.null(file_path)) {
    # Auto-generate filename with timestamp
    timestamp <- format(Sys.Date(), "%Y%m%d")
    if (nutrient == "both") {
      file_path <- file.path(tempdir(), paste0(scale, "_", analysis_type, "_", year, "_", timestamp, ".rds"))
    } else {
      file_path <- file.path(tempdir(), paste0(scale, "_", nutrient, "_", analysis_type, "_", year, "_", timestamp, ".rds"))
    }
  }

  # Ensure .rds extension
  if (!grepl("\\.rds$", file_path, ignore.case = TRUE)) {
    file_path <- paste0(file_path, ".rds")
  }

  # Create directory if it doesn't exist
  dir_path <- dirname(file_path)
  if (dir_path != "." && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created directory: ", dir_path)
  }

  # Save the data
  tryCatch({
    saveRDS(data, file_path)
    file_size <- round(file.size(file_path) / 1024 / 1024, 2) # Size in MB
    message("Saved spatial data to: ", file_path)
    message("File size: ", file_size, " MB")
    message("Rows: ", nrow(data), ", Columns: ", ncol(data))

    if (inherits(data, "sf")) {
      message("Geometry type: ", unique(sf::st_geometry_type(data))[1])
      message("CRS: ", sf::st_crs(data)$input)
    }

  }, error = function(e) {
    stop("Failed to save spatial data: ", e$message, call. = FALSE)
  })

  return(normalizePath(file_path))
}

#' Save Centroid Data
#'
#' Save centroid data to CSV file for transition probability analysis
#'
#' @param data Data frame. Data with centroid coordinates
#' @param file_path Character. Output file path (should end in .csv). If NULL, auto-generated
#' @param scale Character. Spatial scale for file naming
#' @param nutrient Character. Nutrient type for file naming
#' @param analysis_type Character. Analysis type for file naming
#' @param year Numeric. Year for file naming
#' @return Character. Path to saved file
#' @export
#' @examples
#' \donttest{
#' # Create some example data first
#' results <- run_builtin_analysis(scale = "county", year = 2016, include_wwtp = TRUE)
#'
#' # Save centroids for transition analysis
#' if ("integrated" %in% names(results) && "nitrogen" %in% names(results$integrated)) {
#'   centroids <- add_centroid_coordinates(results$integrated$nitrogen)
#'   save_centroid_data(centroids, scale = "county", nutrient = "nitrogen")
#' }
#' }
save_centroid_data <- function(data, file_path = NULL, scale = "huc8",
                               nutrient = "nitrogen", analysis_type = "centroids",
                               year = format(Sys.Date(), "%Y")) {

  if (is.null(file_path)) {
    # Auto-generate filename with timestamp
    timestamp <- format(Sys.Date(), "%Y%m%d")
    file_path <- file.path(tempdir(), paste0(scale, "_", nutrient, "_", analysis_type, "_", year, "_", timestamp, ".csv"))
  }

  # Ensure .csv extension
  if (!grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    file_path <- paste0(file_path, ".csv")
  }

  # Create directory if it doesn't exist
  dir_path <- dirname(file_path)
  if (dir_path != "." && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created directory: ", dir_path)
  }

  # Validate data has required columns for centroids
  required_cols <- c("longitude", "latitude")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    warning("Missing coordinate columns: ", paste(missing_cols, collapse = ", "))
  }

  # Save the data
  tryCatch({
    write.csv(data, file_path, row.names = FALSE)
    file_size <- round(file.size(file_path) / 1024, 2) # Size in KB
    message("Saved centroid data to: ", file_path)
    message("File size: ", file_size, " KB")
    message("Rows: ", nrow(data), ", Columns: ", ncol(data))

    if (all(required_cols %in% names(data))) {
      coord_range_lon <- range(data$longitude, na.rm = TRUE)
      coord_range_lat <- range(data$latitude, na.rm = TRUE)
      message("Longitude range: [", round(coord_range_lon[1], 3), ", ", round(coord_range_lon[2], 3), "]")
      message("Latitude range: [", round(coord_range_lat[1], 3), ", ", round(coord_range_lat[2], 3), "]")
    }

  }, error = function(e) {
    stop("Failed to save centroid data: ", e$message, call. = FALSE)
  })

  return(normalizePath(file_path))
}

#' Save Plot
#'
#' Save ggplot object to file with publication-quality settings
#'
#' @param plot ggplot object. Plot to save
#' @param file_path Character. Output file path
#' @param width Numeric. Plot width in inches (default: 11)
#' @param height Numeric. Plot height in inches (default: 6)
#' @param dpi Numeric. Resolution in dots per inch (default: 300)
#' @param units Character. Units for width and height (default: "in")
#' @param device Character. Output device (auto-detected from file extension)
#' @return Character. Path to saved file
#' @export
#' @examples
#' \donttest{
#' # Create a simple plot for demonstration
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
#'
#' # Save with default settings (300 DPI, 11x6 inches)
#' save_plot(p, file.path(tempdir(), "test_plot.png"))
#'
#' # Save with custom dimensions for presentation
#' save_plot(p, file.path(tempdir(), "presentation_plot.png"), width = 16, height = 9)
#'
#' # Save as PDF for publication
#' save_plot(p, file.path(tempdir(), "publication_figure.pdf"), width = 8, height = 6)
#' }
save_plot <- function(plot, file_path, width = 11, height = 6, dpi = 300,
                      units = "in", device = NULL) {

  # Validate inputs
  if (!inherits(plot, "ggplot")) {
    stop("Object must be a ggplot object", call. = FALSE)
  }

  if (missing(file_path)) {
    stop("file_path is required", call. = FALSE)
  }

  # Create directory if it doesn't exist
  dir_path <- dirname(file_path)
  if (dir_path != "." && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created directory: ", dir_path)
  }

  # Auto-detect device from file extension if not specified
  if (is.null(device)) {
    ext <- tolower(tools::file_ext(file_path))
    device <- switch(ext,
                     "png" = "png",
                     "jpg" = "jpeg",
                     "jpeg" = "jpeg",
                     "pdf" = "pdf",
                     "svg" = "svg",
                     "tiff" = "tiff",
                     "eps" = "ps",
                     "png") # Default to PNG
  }

  # Set device-specific parameters
  extra_params <- list()
  if (device %in% c("png", "jpeg", "tiff")) {
    extra_params$type = "cairo"  # Better quality for raster formats
  }
  if (device == "pdf") {
    extra_params$useDingbats = FALSE  # Better compatibility
  }

  # Save the plot
  tryCatch({
    do.call(ggplot2::ggsave, c(list(
      filename = file_path,
      plot = plot,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      device = device
    ), extra_params))

    # Report success with details
    file_size <- round(file.size(file_path) / 1024, 2) # Size in KB
    message("Saved plot to: ", file_path)
    message("Dimensions: ", width, " x ", height, " ", units, " at ", dpi, " DPI")
    message("File size: ", file_size, " KB")
    message("Device: ", device)

  }, error = function(e) {
    stop("Failed to save plot: ", e$message, call. = FALSE)
  })

  return(normalizePath(file_path))
}

#' Save Analysis Summary
#'
#' Save comprehensive summary of analysis parameters and results
#'
#' @param results List. Complete analysis results from workflow functions
#' @param file_path Character. Output file path. If NULL, auto-generated
#' @param format Character. Output format: "rds", "json", or "txt"
#' @return Character. Path to saved file
#' @export
#' @examples
#' \donttest{
#' # Create analysis results first
#' results <- run_builtin_analysis(scale = "county", year = 2016)
#'
#' # Save complete analysis summary
#' summary_path <- file.path(tempdir(), "analysis_summary_2016.json")
#' save_analysis_summary(results, summary_path, format = "json")
#' }
save_analysis_summary <- function(results, file_path = NULL, format = "rds") {

  format <- match.arg(format, c("rds", "json", "txt"))

  # Auto-generate filename if not provided
  if (is.null(file_path)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    extension <- switch(format,
                        "rds" = ".rds",
                        "json" = ".json",
                        "txt" = ".txt")
    file_path <- file.path(tempdir(), paste0("analysis_summary_", timestamp, extension))
  }

  # Create directory if needed
  dir_path <- dirname(file_path)
  if (dir_path != "." && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # Extract summary information
  summary_info <- list(
    analysis_timestamp = Sys.time(),
    parameters = results$parameters,
    data_summary = list(),
    file_info = list()
  )

  # Add data summaries
  if ("agricultural" %in% names(results)) {
    summary_info$data_summary$agricultural <- list(
      n_units = nrow(results$agricultural),
      n_columns = ncol(results$agricultural),
      has_geometry = inherits(results$agricultural, "sf")
    )

    # Add nutrient classification summaries
    if ("N_class" %in% names(results$agricultural)) {
      summary_info$data_summary$nitrogen_classes <- table(results$agricultural$N_class, useNA = "ifany")
    }
    if ("P_class" %in% names(results$agricultural)) {
      summary_info$data_summary$phosphorus_classes <- table(results$agricultural$P_class, useNA = "ifany")
    }
  }

  if ("wwtp" %in% names(results)) {
    wwtp_summary <- list()
    for (nutrient in names(results$wwtp)) {
      if ("facility_data" %in% names(results$wwtp[[nutrient]])) {
        facility_data <- results$wwtp[[nutrient]]$facility_data
        wwtp_summary[[nutrient]] <- list(
          n_facilities = nrow(facility_data),
          source_classes = if ("source_class" %in% names(facility_data)) table(facility_data$source_class) else NULL,
          total_load = if (paste0(toupper(substring(nutrient, 1, 1)), "_Load_tons") %in% names(facility_data)) {
            sum(facility_data[[paste0(toupper(substring(nutrient, 1, 1)), "_Load_tons")]], na.rm = TRUE)
          } else NULL
        )
      }
    }
    summary_info$data_summary$wwtp <- wwtp_summary
  }

  if ("integrated" %in% names(results)) {
    integrated_summary <- list()
    for (nutrient in names(results$integrated)) {
      integrated_data <- results$integrated[[nutrient]]
      combined_col <- if (nutrient == "nitrogen") "combined_N_class" else "combined_P_class"

      integrated_summary[[nutrient]] <- list(
        n_units = nrow(integrated_data),
        combined_classes = if (combined_col %in% names(integrated_data)) {
          table(integrated_data[[combined_col]], useNA = "ifany")
        } else NULL
      )
    }
    summary_info$data_summary$integrated <- integrated_summary
  }

  # Add created files info if available
  if ("created_files" %in% names(results)) {
    summary_info$file_info$created_files <- results$created_files
    summary_info$file_info$n_files_created <- length(results$created_files)
  }

  # Save in requested format
  tryCatch({
    if (format == "rds") {
      saveRDS(summary_info, file_path)

    } else if (format == "json") {
      # Convert to JSON-friendly format (no complex objects)
      json_summary <- summary_info
      # Convert tables to named lists
      json_summary <- rapply(json_summary, function(x) {
        if (inherits(x, "table")) {
          as.list(as.numeric(x))
        } else {
          x
        }
      }, how = "replace")

      jsonlite::write_json(json_summary, file_path, auto_unbox = TRUE, pretty = TRUE)

    } else if (format == "txt") {
      # Create human-readable text summary
      sink(file_path)
      message("MANURESHED ANALYSIS SUMMARY\n")
      message("============================\n\n")

      message("Analysis Date:", format(summary_info$analysis_timestamp), "\n\n")

      message("PARAMETERS:\n")
      message("-----------\n")
      if (!is.null(summary_info$parameters)) {
        for (param in names(summary_info$parameters)) {
          message(param, ":", summary_info$parameters[[param]], "\n")
        }
      }
      message("\n")

      if (!is.null(summary_info$data_summary)) {
        message("DATA SUMMARY:\n")
        message("-------------\n")

        if ("agricultural" %in% names(summary_info$data_summary)) {
          message("Agricultural Data:", summary_info$data_summary$agricultural$n_units, "spatial units\n")
        }

        if ("wwtp" %in% names(summary_info$data_summary)) {
          message("WWTP Data:\n")
          for (nutrient in names(summary_info$data_summary$wwtp)) {
            n_fac <- summary_info$data_summary$wwtp[[nutrient]]$n_facilities
            message("  ", nutrient, ":", n_fac, "facilities\n")
          }
        }

        if ("nitrogen_classes" %in% names(summary_info$data_summary)) {
          message("Nitrogen Classifications:\n")
          n_classes <- summary_info$data_summary$nitrogen_classes
          for (i in 1:length(n_classes)) {
            message("  ", names(n_classes)[i], ":", n_classes[i], "\n")
          }
        }

        if ("phosphorus_classes" %in% names(summary_info$data_summary)) {
          message("Phosphorus Classifications:\n")
          p_classes <- summary_info$data_summary$phosphorus_classes
          for (i in 1:length(p_classes)) {
            message("  ", names(p_classes)[i], ":", p_classes[i], "\n")
          }
        }
      }

      if (!is.null(summary_info$file_info$created_files)) {
        message("\nCREATED FILES:\n")
        message("--------------\n")
        for (file_type in names(summary_info$file_info$created_files)) {
          message(file_type, ":", summary_info$file_info$created_files[[file_type]], "\n")
        }
      }

      sink()
    }

    file_size <- round(file.size(file_path) / 1024, 2)
    message("Saved analysis summary to: ", file_path)
    message("Format: ", toupper(format))
    message("File size: ", file_size, " KB")

  }, error = function(e) {
    stop("Failed to save analysis summary: ", e$message, call. = FALSE)
  })

  return(normalizePath(file_path))
}

#' Save Transition Probability Matrix
#'
#' Save transition probability matrix to CSV with metadata
#'
#' @param transition_df Data frame. Transition probability matrix
#' @param file_path Character. Output file path
#' @param nutrient Character. Nutrient type
#' @param analysis_type Character. Type of analysis
#' @param metadata List. Additional metadata to include
#' @return Character. Path to saved file
#' @export
#' @examples
#' \donttest{
#' # Create example analysis results first
#' results <- run_builtin_analysis(scale = "county", year = 2016, include_wwtp = TRUE)
#'
#' # Save transition probabilities (only if integrated results exist)
#' if ("integrated" %in% names(results) && "nitrogen" %in% names(results$integrated)) {
#'   centroids <- add_centroid_coordinates(results$integrated$nitrogen)
#'   transitions <- calculate_transition_probabilities(centroids, "combined_N_class")
#'   save_transition_matrix(transitions,
#'                         file.path(tempdir(), "transitions_nitrogen.csv"),
#'                         "nitrogen")
#' }
#' }
save_transition_matrix <- function(transition_df, file_path, nutrient,
                                   analysis_type = "combined", metadata = NULL) {

  # Create directory if needed
  dir_path <- dirname(file_path)
  if (dir_path != "." && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # Prepare metadata
  meta_info <- list(
    created_date = Sys.time(),
    nutrient = nutrient,
    analysis_type = analysis_type,
    n_categories = nrow(transition_df),
    categories = rownames(transition_df)
  )

  if (!is.null(metadata)) {
    meta_info <- c(meta_info, metadata)
  }

  tryCatch({
    # Save main transition matrix
    write.csv(transition_df, file_path, row.names = TRUE)

    # Save metadata to companion file
    meta_file <- sub("\\.csv$", "_metadata.txt", file_path)
    sink(meta_file)
    message("TRANSITION PROBABILITY MATRIX METADATA\n")
    message("======================================\n\n")
    for (item in names(meta_info)) {
      if (length(meta_info[[item]]) > 1) {
        message(item, ":\n")
        for (val in meta_info[[item]]) {
          message("  ", val, "\n")
        }
      } else {
        message(item, ":", meta_info[[item]], "\n")
      }
    }
    sink()

    file_size <- round(file.size(file_path) / 1024, 2)
    message("Saved transition matrix to: ", file_path)
    message("Saved metadata to: ", meta_file)
    message("File size: ", file_size, " KB")
    message("Matrix dimensions: ", nrow(transition_df), " x ", ncol(transition_df))

  }, error = function(e) {
    stop("Failed to save transition matrix: ", e$message, call. = FALSE)
  })

  return(normalizePath(file_path))
}

#' Create Analysis Report
#'
#' Generate comprehensive HTML or PDF report of analysis results
#'
#' @param results List. Complete analysis results
#' @param output_path Character. Path for output report
#' @param format Character. Report format: "html" or "pdf"
#' @param title Character. Report title
#' @param include_maps Logical. Whether to include maps in report
#' @return Character. Path to generated report
#' @export
#' @examples
#' \donttest{
#' # Generate HTML report - use tempdir to avoid check directory pollution
#' results <- run_builtin_analysis(scale = "county", year = 2016)
#' report_path <- file.path(tempdir(), "analysis_report.html")
#' create_analysis_report(results, report_path)
#' }
create_analysis_report <- function(results, output_path, format = "html",
                                   title = "Manureshed Analysis Report",
                                   include_maps = TRUE) {

  format <- match.arg(format, c("html", "pdf"))

  # Check if required packages are available using requireNamespace
  required_pkgs <- c("rmarkdown", "knitr")
  missing_pkgs <- character(0)

  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_pkgs <- c(missing_pkgs, pkg)
    }
  }

  if (length(missing_pkgs) > 0) {
    stop("Required packages not installed: ", paste(missing_pkgs, collapse = ", "),
         "\nInstall with: install.packages(c(",
         paste0("'", missing_pkgs, "'", collapse = ", "), "))",
         call. = FALSE)
  }

  # Create temporary Rmd file
  temp_rmd <- tempfile(fileext = ".Rmd")

  # Generate Rmd content
  rmd_content <- generate_report_content(results, title, include_maps)

  # Write Rmd content
  writeLines(rmd_content, temp_rmd)

  # Render the report
  tryCatch({
    rmarkdown::render(
      input = temp_rmd,
      output_file = basename(output_path),
      output_dir = dirname(output_path),
      output_format = if (format == "html") "html_document" else "pdf_document",
      quiet = TRUE
    )

    message("Generated analysis report: ", output_path)
    message("Format: ", toupper(format))

    # Clean up temporary file
    unlink(temp_rmd)

  }, error = function(e) {
    unlink(temp_rmd)
    stop("Failed to generate report: ", e$message, call. = FALSE)
  })

  return(normalizePath(output_path))
}

#' Generate Report Content
#'
#' Internal function to generate R Markdown content for analysis report
#'
#' @param results List. Analysis results
#' @param title Character. Report title
#' @param include_maps Logical. Whether to include maps
#' @return Character vector. R Markdown content
#' @keywords internal
generate_report_content <- function(results, title, include_maps) {

  content <- c(
    paste0("---"),
    paste0('title: "', title, '"'),
    paste0('date: "', Sys.Date(), '"'),
    paste0('output: '),
    paste0('  html_document:'),
    paste0('    toc: true'),
    paste0('    toc_float: true'),
    paste0('    theme: flatly'),
    paste0('---'),
    paste0(''),
    paste0('```{r setup, include=FALSE}'),
    paste0('knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)'),
    paste0('library(manureshed)'),
    paste0('library(dplyr)'),
    paste0('library(knitr)'),
    paste0('```'),
    paste0(''),
    paste0('# Analysis Summary'),
    paste0('')
  )

  # Add parameter summary
  if ("parameters" %in% names(results)) {
    content <- c(content,
                 paste0('## Analysis Parameters'),
                 paste0(''),
                 paste0('```{r}'),
                 paste0('params <- results$parameters'),
                 paste0('param_df <- data.frame('),
                 paste0('  Parameter = names(params),'),
                 paste0('  Value = sapply(params, function(x) paste(x, collapse = ", "))'),
                 paste0(')'),
                 paste0('kable(param_df, caption = "Analysis Parameters")'),
                 paste0('```'),
                 paste0('')
    )
  }

  # Add data summaries
  content <- c(content,
               paste0('## Data Summary'),
               paste0('')
  )

  if ("agricultural" %in% names(results)) {
    content <- c(content,
                 paste0('### Agricultural Classifications'),
                 paste0(''),
                 paste0('```{r}'),
                 paste0('if ("N_class" %in% names(results$agricultural)) {'),
                 paste0('  n_summary <- table(results$agricultural$N_class)'),
                 paste0('  kable(data.frame(Classification = names(n_summary), Count = as.numeric(n_summary)),'),
                 paste0('        caption = "Nitrogen Classifications")'),
                 paste0('}'),
                 paste0(''),
                 paste0('if ("P_class" %in% names(results$agricultural)) {'),
                 paste0('  p_summary <- table(results$agricultural$P_class)'),
                 paste0('  kable(data.frame(Classification = names(p_summary), Count = as.numeric(p_summary)),'),
                 paste0('        caption = "Phosphorus Classifications")'),
                 paste0('}'),
                 paste0('```'),
                 paste0('')
    )
  }

  # Add WWTP summary if available
  if ("wwtp" %in% names(results)) {
    content <- c(content,
                 paste0('### WWTP Facility Summary'),
                 paste0(''),
                 paste0('```{r}'),
                 paste0('wwtp_summary <- list()'),
                 paste0('for (nutrient in names(results$wwtp)) {'),
                 paste0('  if ("facility_data" %in% names(results$wwtp[[nutrient]])) {'),
                 paste0('    fac_data <- results$wwtp[[nutrient]]$facility_data'),
                 paste0('    wwtp_summary[[nutrient]] <- list('),
                 paste0('      Facilities = nrow(fac_data),'),
                 paste0('      States = length(unique(fac_data$State))'),
                 paste0('    )'),
                 paste0('  }'),
                 paste0('}'),
                 paste0('if (length(wwtp_summary) > 0) {'),
                 paste0('  wwtp_df <- do.call(rbind, lapply(names(wwtp_summary), function(n) {'),
                 paste0('    data.frame(Nutrient = n, wwtp_summary[[n]])'),
                 paste0('  }))'),
                 paste0('  kable(wwtp_df, caption = "WWTP Facility Summary")'),
                 paste0('}'),
                 paste0('```'),
                 paste0('')
    )
  }

  # Add visualization placeholder if maps requested
  if (include_maps && "created_files" %in% names(results)) {
    content <- c(content,
                 paste0('## Visualizations'),
                 paste0(''),
                 paste0('The following visualizations were created:'),
                 paste0(''),
                 paste0('```{r}'),
                 paste0('if ("created_files" %in% names(results)) {'),
                 paste0('  files_df <- data.frame('),
                 paste0('    Type = names(results$created_files),'),
                 paste0('    File = basename(unlist(results$created_files))'),
                 paste0('  )'),
                 paste0('  kable(files_df, caption = "Created Visualization Files")'),
                 paste0('}'),
                 paste0('```'),
                 paste0('')
    )
  }

  # Add footer
  content <- c(content,
               paste0('---'),
               paste0(''),
               paste0('*Report generated with the manureshed R package*')
  )

  return(content)
}
