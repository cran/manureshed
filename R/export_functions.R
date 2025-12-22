# ==============================================================================
# R/export_functions.R - Export Functions for Different Use Cases
# ==============================================================================

#' Export Results for GIS Applications
#'
#' Export spatial results in GIS-ready formats
#'
#' @param results Analysis results object
#' @param output_dir Output directory
#' @param formats Character vector of formats: "shapefile", "geojson", "kml", "gpkg"
#' @return List of created files
#' @export
#' @examples
#' \donttest{
#' # Use tempdir to avoid polluting check directory
#' results <- run_builtin_analysis(scale = "county", year = 2016)
#' output_dir <- file.path(tempdir(), "gis_outputs")
#' gis_files <- export_for_gis(results, output_dir)
#' }
export_for_gis <- function(results, output_dir = file.path(tempdir(), "gis_export"),
                           formats = c("shapefile", "geojson")) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  created_files <- list()

  # Export agricultural results
  if ("agricultural" %in% names(results) && inherits(results$agricultural, "sf")) {

    # Prepare data for export - handle duplicate columns
    data_to_export <- results$agricultural

    # Get column names and find duplicates
    col_names <- names(data_to_export)

    # Remove geometry column temporarily for processing
    geom <- sf::st_geometry(data_to_export)
    data_df <- sf::st_drop_geometry(data_to_export)

    # Keep only unique column names (first occurrence)
    unique_cols <- !duplicated(names(data_df))
    data_df <- data_df[, unique_cols, drop = FALSE]

    # Reconstruct sf object
    data_to_export <- sf::st_sf(data_df,
                                geometry = sf::st_geometry(results$agricultural),
                                crs = sf::st_crs(results$agricultural))  # ADD CRS

    # Now export in requested formats
    if ("shapefile" %in% formats) {
      tryCatch({
        shp_file <- file.path(output_dir, "agricultural_results.shp")
        sf::st_write(data_to_export, shp_file, delete_dsn = TRUE, quiet = TRUE)
        created_files$agricultural_shp <- shp_file
        message("Created shapefile: ", shp_file)
      }, error = function(e) {
        warning("Shapefile export failed: ", e$message)
      })
    }

    if ("geojson" %in% formats) {
      tryCatch({
        json_file <- file.path(output_dir, "agricultural_results.geojson")
        sf::st_write(data_to_export, json_file, delete_dsn = TRUE, quiet = TRUE)
        created_files$agricultural_geojson <- json_file
        message("Created GeoJSON: ", json_file)
      }, error = function(e) {
        warning("GeoJSON export failed: ", e$message)
      })
    }

    if ("kml" %in% formats) {
      tryCatch({
        kml_file <- file.path(output_dir, "agricultural_results.kml")
        sf::st_write(data_to_export, kml_file, delete_dsn = TRUE,
                     driver = "KML", quiet = TRUE)
        created_files$agricultural_kml <- kml_file
        message("Created KML: ", kml_file)
      }, error = function(e) {
        warning("KML export failed: ", e$message)
      })
    }

    if ("gpkg" %in% formats) {
      tryCatch({
        gpkg_file <- file.path(output_dir, "agricultural_results.gpkg")
        sf::st_write(data_to_export, gpkg_file, delete_dsn = TRUE, quiet = TRUE)
        created_files$agricultural_gpkg <- gpkg_file
        message("Created GeoPackage: ", gpkg_file)
      }, error = function(e) {
        warning("GeoPackage export failed: ", e$message)
      })
    }
  }

  # Export integrated results if available
  if ("integrated" %in% names(results)) {
    for (nutrient in names(results$integrated)) {
      if (inherits(results$integrated[[nutrient]], "sf")) {

        # Handle duplicates for integrated data too
        data_to_export <- results$integrated[[nutrient]]
        geom <- sf::st_geometry(data_to_export)
        data_df <- sf::st_drop_geometry(data_to_export)
        unique_cols <- !duplicated(names(data_df))
        data_df <- data_df[, unique_cols, drop = FALSE]
        data_to_export <- sf::st_sf(data_df, geometry = geom)

        if ("shapefile" %in% formats) {
          tryCatch({
            shp_file <- file.path(output_dir, paste0(nutrient, "_integrated.shp"))
            sf::st_write(data_to_export, shp_file, delete_dsn = TRUE, quiet = TRUE)
            created_files[[paste0(nutrient, "_shp")]] <- shp_file
          }, error = function(e) {
            warning("Shapefile export failed for ", nutrient, ": ", e$message)
          })
        }

        if ("geojson" %in% formats) {
          tryCatch({
            json_file <- file.path(output_dir, paste0(nutrient, "_integrated.geojson"))
            sf::st_write(data_to_export, json_file, delete_dsn = TRUE, quiet = TRUE)
            created_files[[paste0(nutrient, "_geojson")]] <- json_file
          }, error = function(e) {
            warning("GeoJSON export failed for ", nutrient, ": ", e$message)
          })
        }
      }
    }
  }

  return(created_files)
}

#' Export for Publication
#'
#' Create high-resolution outputs suitable for publication
#'
#' @param results Analysis results
#' @param output_dir Output directory
#' @param dpi Resolution (default: 600 for publication quality)
#' @return List of created files
#' @export
export_for_publication <- function(results, output_dir = file.path(tempdir(), "publication_export"), dpi = 600) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  created_files <- list()

  # High-resolution maps
  for (nutrient in results$parameters$nutrients) {
    # Agricultural map
    agri_col <- if (nutrient == "nitrogen") "N_class" else "P_class"
    map <- map_agricultural_classification(
      results$agricultural, nutrient, agri_col,
      paste(toupper(nutrient), "Classifications")
    )

    map_file <- file.path(output_dir, paste0("figure_", nutrient, "_agricultural.png"))
    save_plot(map, map_file, width = 10, height = 8, dpi = dpi)
    created_files[[paste0(nutrient, "_map")]] <- map_file
  }

  # Summary statistics table (LaTeX format)
  summary_file <- file.path(output_dir, "table_summary_statistics.tex")
  create_publication_table(results, summary_file)
  created_files$summary_table <- summary_file

  message("Publication files created in: ", output_dir)
  message("  Resolution: ", dpi, " DPI")

  return(created_files)
}

#' Export for Policy Briefs
#'
#' Create simplified outputs for policy makers
#'
#' @param results Analysis results
#' @param output_dir Output directory
#' @return List of created files
#' @export
export_for_policy <-  function(results, output_dir = file.path(tempdir(), "policy_export")) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  created_files <- list()

  # Executive summary
  summary_file <- file.path(output_dir, "executive_summary.txt")
  create_executive_summary(results, summary_file)
  created_files$executive_summary <- summary_file

  # Key statistics
  stats_file <- file.path(output_dir, "key_statistics.csv")
  create_key_statistics(results, stats_file)
  created_files$key_statistics <- stats_file

  # Simplified maps (lower resolution, clearer labels)
  for (nutrient in results$parameters$nutrients) {
    agri_col <- if (nutrient == "nitrogen") "N_class" else "P_class"
    map <- map_agricultural_classification(
      results$agricultural, nutrient, agri_col,
      paste(toupper(nutrient), "Balance")
    )

    map_file <- file.path(output_dir, paste0(nutrient, "_balance_map.png"))
    save_plot(map, map_file, width = 11, height = 8, dpi = 150)
    created_files[[paste0(nutrient, "_map")]] <- map_file
  }

  message("Policy brief materials created in: ", output_dir)

  return(created_files)
}

# Helper functions (internal)

create_publication_table <- function(results, output_file) {
  # Create LaTeX table
  sink(output_file)
  message("\\begin{table}[h]\n")
  message("\\caption{Analysis Summary Statistics}\n")
  message("\\begin{tabular}{lrrr}\n")
  message("\\hline\n")
  message("Classification & Count & Percent & Area (km2) \\\\\n")
  message("\\hline\n")
  # Add table rows based on results
  message("\\hline\n")
  message("\\end{tabular}\n")
  message("\\end{table}\n")
  sink()
}

create_executive_summary <- function(results, output_file) {
  sink(output_file)
  message("EXECUTIVE SUMMARY\n")
  message(paste(rep("=", 50), collapse = ""), "\n\n")
  message("Analysis Date:", format(Sys.Date(), "%B %d, %Y"), "\n")
  message("Spatial Scale:", results$parameters$scale, "\n")
  message("Year:", results$parameters$year, "\n\n")
  message("KEY FINDINGS:\n\n")
  # Add key findings based on results
  message("Total spatial units analyzed:", nrow(results$agricultural), "\n")
  # Add more summary points
  sink()
}

create_key_statistics <- function(results, output_file) {
  stats_df <- data.frame(
    Metric = character(),
    Value = character(),
    stringsAsFactors = FALSE
  )

  stats_df <- rbind(stats_df, data.frame(
    Metric = "Total Spatial Units",
    Value = as.character(nrow(results$agricultural))
  ))

  # Add more statistics

  write.csv(stats_df, output_file, row.names = FALSE)
}
