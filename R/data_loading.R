# ==============================================================================
# R/data_loading.R - OSF Data Loading Functions with Configured File IDs
# ==============================================================================

#' Download and Cache Data from OSF
#'
#' Download built-in datasets from OSF repository using Files API
#'
#' @param dataset_name Character. Name of dataset to download
#' @param force_download Logical. Force re-download even if cached version exists
#' @param cache_dir Character. Directory to cache downloaded data (default: user data dir)
#' @param verbose Logical. Show download progress
#' @return Path to cached data file
#' @export
download_osf_data <- function(dataset_name, force_download = FALSE,
                              cache_dir = NULL, verbose = TRUE) {

  # OSF project ID for manureshed
  osf_project_id <- "g39xa"

  # OSF permanent file IDs - CONFIGURED WITH YOUR ACTUAL IDs
  dataset_file_ids <- list(
    # NuGIS datasets
    "nugis_county_data" = "689a80e81a020593fca5e8b4",
    "nugis_huc8_data" = "689a80e66880a2c5318935f0",
    "nugis_huc2_data" = "689a80e4ea0587a8dea0aeac",

    # Spatial boundaries
    "county_boundaries" = "689a80dbfcf05a1374a5ea29",
    "huc8_boundaries" = "689a80de8688b85a3e893403",
    "huc2_boundaries" = "689a80df2582efb84a921b41",

    # WWTP data - UPDATED FOR MULTI-YEAR
    "wwtp_nitrogen_combined" = "689c97483efa05a3a93ccdd2",  # NEW
    "wwtp_phosphorus_combined" = "689c97404cae9f37ea13c731", # NEW

    # Texas supplemental data
    "texas_huc8_data" = "689a80e3bd495653dc921bc0",
    "texas_huc8_boundaries" = "689a80d9aa71910946921ade"
  )

  # Also update the corresponding filenames
  dataset_filenames <- list(
    "nugis_county_data" = "nugis_county_data.rda",
    "nugis_huc8_data" = "nugis_huc8_data.rda",
    "nugis_huc2_data" = "nugis_huc2_data.rda",
    "county_boundaries" = "county_boundaries.rda",
    "huc8_boundaries" = "huc8_boundaries.rda",
    "huc2_boundaries" = "huc2_boundaries.rda",
    "wwtp_nitrogen_combined" = "wwtp_nitrogen_combined.rda",    # UPDATED
    "wwtp_phosphorus_combined" = "wwtp_phosphorus_combined.rda", # UPDATED
    "texas_huc8_data" = "texas_huc8_data.rda",
    "texas_huc8_boundaries" = "texas_huc8_boundaries.rda"
  )

  if (!dataset_name %in% names(dataset_file_ids)) {
    stop("Unknown dataset: ", dataset_name,
         "\nAvailable datasets: ", paste(names(dataset_file_ids), collapse = ", "))
  }

  # Get file ID
  file_id <- dataset_file_ids[[dataset_name]]

  # Set up cache directory
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tools::R_user_dir("manureshed", "cache"), "data")
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    if (verbose) message("Created cache directory: ", cache_dir)
  }

  # Define local file path
  local_file <- file.path(cache_dir, dataset_filenames[[dataset_name]])

  # Check if we need to download
  needs_download <- force_download || !file.exists(local_file)

  if (needs_download) {
    if (verbose) {
      message("Downloading ", dataset_name, " from OSF...")
    }

    # Use the working Files API URL pattern
    download_url <- paste0("https://files.osf.io/v1/resources/", osf_project_id,
                           "/providers/osfstorage/", file_id)

    if (verbose) {
      message("  File ID: ", file_id)
    }

    tryCatch({
      utils::download.file(
        url = download_url,
        destfile = local_file,
        mode = "wb",
        method = "auto",
        quiet = !verbose
      )

      # Validate download
      if (file.exists(local_file) && file.size(local_file) > 1000) {  # At least 1KB
        if (verbose) {
          file_size <- round(file.size(local_file) / 1024 / 1024, 2)
          message("  Downloaded successfully (", file_size, " MB)")
        }
      } else {
        if (file.exists(local_file)) {
          unlink(local_file) # Clean up failed download
        }
        stop("Download failed - file not created or too small")
      }

    }, error = function(e) {
      if (file.exists(local_file)) {
        unlink(local_file) # Clean up partial download
      }
      stop("Failed to download ", dataset_name, " from OSF: ", e$message,
           "\nURL: ", download_url)
    })

  } else {
    if (verbose) {
      message("Using cached version of ", dataset_name)
    }
  }

  return(local_file)
}

#' Load Built-in NuGIS Data from OSF
#'
#' Load built-in NuGIS data from OSF repository for specified year and spatial scale.
#' Data includes all years from 1987 through 2016.
#'
#' @param scale Character. Spatial scale: "county", "huc8", or "huc2"
#' @param year Numeric. Year to filter data (available: 1987-2016)
#' @param force_download Logical. Force re-download even if cached
#' @param verbose Logical. Show download progress
#' @return Data frame of NuGIS data for specified scale and year
#' @export
#' @examples
#' \donttest{
#' # Load county data for 2016
#' county_2016 <- load_builtin_nugis("county", 2016)
#'
#' # Load HUC8 data for 2010
#' huc8_2010 <- load_builtin_nugis("huc8", 2010)
#'
#' # Load county data for 2010, force fresh download
#' county_2010 <- load_builtin_nugis("county", 2010, force_download = TRUE)
#' }
load_builtin_nugis <- function(scale, year = 2016, force_download = FALSE, verbose = TRUE) {

  # Validate inputs
  if (!scale %in% c("county", "huc8", "huc2")) {
    stop("Scale must be 'county', 'huc8', or 'huc2'")
  }

  # Get dataset name
  dataset_name <- paste0("nugis_", scale, "_data")

  # Download/load data from OSF
  data_file <- download_osf_data(dataset_name, force_download, verbose = verbose)

  # Load the .rda file
  load_env <- new.env()
  load(data_file, envir = load_env)

  # Extract the data object (should have same name as dataset_name)
  if (exists(dataset_name, envir = load_env)) {
    data <- get(dataset_name, envir = load_env)
  } else {
    # Try to find any object in the loaded environment
    objects <- ls(load_env)
    if (length(objects) == 1) {
      data <- get(objects[1], envir = load_env)
    } else {
      stop("Could not find expected data object in ", data_file)
    }
  }

  # Validate data structure
  if (!"Year" %in% names(data)) {
    stop("Year column not found in NuGIS data")
  }

  # Filter for specified year
  data_year <- data[data$Year == year, ]

  if (nrow(data_year) == 0) {
    available_years <- sort(unique(data$Year))
    stop("No data found for year: ", year,
         ". Available years: ", paste(available_years, collapse = ", "))
  }

  if (verbose) {
    message("Loaded NuGIS ", scale, " data for year ", year)
    message("Number of spatial units: ", nrow(data_year))
  }

  return(data_year)
}

#' Load Built-in WWTP Data from OSF
#'
#' Load built-in WWTP data for specified year from OSF repository (2007-2016 available)
#'
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @param year Numeric. Year to filter data (available: 2007-2016)
#' @param force_download Logical. Force re-download even if cached
#' @param verbose Logical. Show download progress
#' @return Data frame with cleaned WWTP data for specified year
#' @export
#' @examples
#' \donttest{
#' # Load WWTP data for different years (2007-2016 available)
#' wwtp_n_2016 <- load_builtin_wwtp("nitrogen", 2016)
#' wwtp_n_2012 <- load_builtin_wwtp("nitrogen", 2012)
#' wwtp_n_2007 <- load_builtin_wwtp("nitrogen", 2007)
#'
#' # Load phosphorus data
#' wwtp_p_2015 <- load_builtin_wwtp("phosphorus", 2015)
#' wwtp_p_2010 <- load_builtin_wwtp("phosphorus", 2010)
#'
#' # Force re-download
#' wwtp_fresh <- load_builtin_wwtp("nitrogen", 2014, force_download = TRUE)
#' }
load_builtin_wwtp <- function(nutrient, year = 2016, force_download = FALSE, verbose = TRUE) {

  if (!nutrient %in% c("nitrogen", "phosphorus")) {
    stop("Nutrient must be 'nitrogen' or 'phosphorus'")
  }

  # Validate year range
  if (!year %in% 2007:2016) {
    stop("Year must be between 2007 and 2016. Available years: 2007-2016")
  }

  dataset_name <- paste0("wwtp_", nutrient, "_combined")

  # Download/load data from OSF
  data_file <- download_osf_data(dataset_name, force_download, verbose = verbose)

  # Load the .rda file
  load_env <- new.env()
  load(data_file, envir = load_env)

  # Extract the data object
  if (exists(dataset_name, envir = load_env)) {
    data <- get(dataset_name, envir = load_env)
  } else {
    objects <- ls(load_env)
    if (length(objects) == 1) {
      data <- get(objects[1], envir = load_env)
    } else {
      stop("Could not find expected WWTP data in ", data_file)
    }
  }

  # Validate data structure
  if (!"Year" %in% names(data)) {
    stop("Year column not found in WWTP data")
  }

  # Filter for specified year
  data_year <- data[data$Year == year, ]

  if (nrow(data_year) == 0) {
    available_years <- sort(unique(data$Year))
    stop("No WWTP data found for year: ", year,
         ". Available years: ", paste(available_years, collapse = ", "))
  }

  if (verbose) {
    message("Loaded WWTP ", nutrient, " data for year ", year)
    message("Number of facilities: ", nrow(data_year))
  }

  return(data_year)
}

#' Load Built-in Spatial Boundaries from OSF
#'
#' Load built-in spatial boundary data for specified scale from OSF repository
#'
#' @param scale Character. Spatial scale: "county", "huc8", or "huc2"
#' @param force_download Logical. Force re-download even if cached
#' @param verbose Logical. Show download progress
#' @return sf object with spatial boundaries
#' @export
load_builtin_boundaries <- function(scale, force_download = FALSE, verbose = TRUE) {

  if (!scale %in% c("county", "huc8", "huc2")) {
    stop("Scale must be 'county', 'huc8', or 'huc2'")
  }

  # Get dataset name
  dataset_name <- paste0(scale, "_boundaries")

  # Download/load data from OSF
  data_file <- download_osf_data(dataset_name, force_download, verbose = verbose)

  # Load the .rda file
  load_env <- new.env()
  load(data_file, envir = load_env)

  # Extract the data object
  if (exists(dataset_name, envir = load_env)) {
    boundaries <- get(dataset_name, envir = load_env)
  } else {
    objects <- ls(load_env)
    if (length(objects) == 1) {
      boundaries <- get(objects[1], envir = load_env)
    } else {
      stop("Could not find expected boundary data in ", data_file)
    }
  }

  # Ensure proper CRS
  boundaries <- sf::st_transform(boundaries, crs = MANURESHED_CRS)

  if (verbose) {
    message("Loaded ", scale, " boundaries")
    message("Number of spatial units: ", nrow(boundaries))
  }

  return(boundaries)
}

#' Load Built-in WWTP Data from OSF
#'
#' Load built-in WWTP data for specified year from OSF repository (2007-2016 available)
#'
#' @param nutrient Character. "nitrogen" or "phosphorus"
#' @param year Numeric. Year to filter data (available: 2007-2016)
#' @param force_download Logical. Force re-download even if cached
#' @param verbose Logical. Show download progress
#' @return Data frame with cleaned WWTP data for specified year
#' @export
load_builtin_wwtp <- function(nutrient, year = 2016, force_download = FALSE, verbose = TRUE) {

  if (!nutrient %in% c("nitrogen", "phosphorus")) {
    stop("Nutrient must be 'nitrogen' or 'phosphorus'")
  }

  # Validate year range
  if (!year %in% 2007:2016) {
    stop("Year must be between 2007 and 2016. Available years: 2007-2016")
  }

  dataset_name <- paste0("wwtp_", nutrient, "_combined")

  # Download/load data from OSF
  data_file <- download_osf_data(dataset_name, force_download, verbose = verbose)

  # Load the .rda file
  load_env <- new.env()
  load(data_file, envir = load_env)

  # Extract the data object
  if (exists(dataset_name, envir = load_env)) {
    data <- get(dataset_name, envir = load_env)
  } else {
    objects <- ls(load_env)
    if (length(objects) == 1) {
      data <- get(objects[1], envir = load_env)
    } else {
      stop("Could not find expected WWTP data in ", data_file)
    }
  }

  # Validate data structure
  if (!"Year" %in% names(data)) {
    stop("Year column not found in WWTP data")
  }

  # Filter for specified year
  data_year <- data[data$Year == year, ]

  if (nrow(data_year) == 0) {
    available_years <- sort(unique(data$Year))
    stop("No WWTP data found for year: ", year,
         ". Available years: ", paste(available_years, collapse = ", "))
  }

  if (verbose) {
    message("Loaded WWTP ", nutrient, " data for year ", year)
    message("Number of facilities: ", nrow(data_year))
  }

  return(data_year)
}

#' Check Data Availability from OSF
#'
# Check what datasets are available from OSF repository and whats cached locally
#'
#' @param verbose Logical. Show detailed information about cache status
#' @return List showing available datasets and cache status
#' @export
check_builtin_data <- function(verbose = FALSE) {

  # Updated dataset list - removed old single-year WWTP files
  all_datasets <- c(
    "nugis_county_data", "nugis_huc8_data", "nugis_huc2_data",
    "county_boundaries", "huc8_boundaries", "huc2_boundaries",
    "wwtp_nitrogen_combined", "wwtp_phosphorus_combined",  # UPDATED
    "texas_huc8_data", "texas_huc8_boundaries"
  )

  # Check cache directory
  cache_dir <- file.path(tools::R_user_dir("manureshed", "cache"), "data")

  available_data <- list(
    nugis_scales = c("county", "huc8", "huc2"),
    boundary_scales = c("county", "huc8", "huc2"),
    wwtp_available = TRUE,
    wwtp_years = 2007:2016,  # UPDATED
    texas_data_available = TRUE,
    osf_project = "https://osf.io/g39xa/",
    cache_directory = cache_dir,
    cached_datasets = character(0),
    missing_datasets = character(0),
    nugis_years = list(
      county = 1987:2016,
      huc8 = 1987:2016,
      huc2 = 1987:2016
    )
  )

  # Check which datasets are cached
  if (dir.exists(cache_dir)) {
    cached_files <- list.files(cache_dir, pattern = "\\.rda$", full.names = FALSE)
    cached_datasets <- gsub("\\.rda$", "", cached_files)

    available_data$cached_datasets <- intersect(cached_datasets, all_datasets)
    available_data$missing_datasets <- setdiff(all_datasets, cached_datasets)
  } else {
    available_data$missing_datasets <- all_datasets
  }

  if (verbose) {
    message("manureshed Data Availability")
    message("============================")
    message("OSF Repository: https://osf.io/g39xa/")
    message("Cache Directory: ", cache_dir, "\n")

    message("Cache Status:")
    message("- Cached locally: ", length(available_data$cached_datasets), "/", length(all_datasets))
    message("- Not yet downloaded: ", length(available_data$missing_datasets), "/", length(all_datasets), "\n")
    if (length(available_data$cached_datasets) > 0) {
      message("Cached Datasets:\n")
      for (dataset in available_data$cached_datasets) {
        message("  OK", dataset, "\n")
      }
      message("\n")
    }

    if (length(available_data$missing_datasets) > 0) {
      message("Not Yet Downloaded:\n")
      for (dataset in available_data$missing_datasets) {
        message("  --", dataset, "\n")
      }
      message("\nRun download_all_data() to download all datasets\n\n")
    }

    message("Available Data:\n")
    message("- NuGIS scales:", paste(available_data$nugis_scales, collapse = ", "), "\n")
    message("- NuGIS years: 1987-2016\n")
    message("- WWTP data: 2007-2016 (nitrogen, phosphorus)\n")  # UPDATED
    message("- Total datasets:", length(all_datasets), "\n")
  }

  return(available_data)
}

#' Download All Datasets from OSF
#'
#' Convenience function to download all available datasets from OSF
#'
#' @param force_download Logical. Re-download even if files exist in cache
#' @param verbose Logical. Show progress for each download
#' @return Logical. TRUE if all downloads successful
#' @export
download_all_data <- function(force_download = FALSE, verbose = TRUE) {

  # UPDATED dataset list
  all_datasets <- c(
    "nugis_county_data", "nugis_huc8_data", "nugis_huc2_data",
    "county_boundaries", "huc8_boundaries", "huc2_boundaries",
    "wwtp_nitrogen_combined", "wwtp_phosphorus_combined",  # UPDATED
    "texas_huc8_data", "texas_huc8_boundaries"
  )

  if (verbose) {
    message("Downloading manureshed datasets from OSF...")
    message("OSF Project: https://osf.io/g39xa/")
  }

  failed_downloads <- character(0)
  start_time <- Sys.time()

  for (i in seq_along(all_datasets)) {
    dataset <- all_datasets[i]

    if (verbose) {
      message("\n[", i, "/", length(all_datasets), "] ", dataset, "...")
    }

    tryCatch({
      download_osf_data(dataset, force_download, verbose = verbose)
    }, error = function(e) {
      failed_downloads <<- c(failed_downloads, dataset)
      if (verbose) {
        message("  FAILED: ", e$message)
      }
    })
  }

  end_time <- Sys.time()
  download_time <- as.numeric(difftime(end_time, start_time, units = "mins"))

  success <- length(failed_downloads) == 0

  if (verbose) {
    message("\n", paste(rep("=", 50), collapse = ""))
    message("Download Summary:")
    message("Time: ", round(download_time, 1), " minutes")
    message("Successful: ", length(all_datasets) - length(failed_downloads),
            "/", length(all_datasets), " datasets")

    if (length(failed_downloads) > 0) {
      message("Failed: ", paste(failed_downloads, collapse = ", "))
    }

    if (success) {
      message("All datasets downloaded successfully!")
    }
  }

  return(success)
}

#' Clear Data Cache
#'
#' Remove cached datasets to free up disk space
#'
#' @param confirm Logical. Require confirmation before deleting
#' @param verbose Logical. Show what's being deleted
#' @return Logical. TRUE if successful
#' @export
clear_data_cache <- function(confirm = TRUE, verbose = TRUE) {

  cache_dir <- file.path(tools::R_user_dir("manureshed", "cache"), "data")

  if (!dir.exists(cache_dir)) {
    if (verbose) message("No cache directory found")
    return(TRUE)
  }

  cache_files <- list.files(cache_dir, pattern = "\\.rda$", full.names = TRUE)

  if (length(cache_files) == 0) {
    if (verbose) message("Cache is already empty")
    return(TRUE)
  }

  total_size <- sum(file.size(cache_files), na.rm = TRUE) / 1024 / 1024

  if (confirm) {
    message("Clear", length(cache_files), "cached files (",
        round(total_size, 1), "MB)? (y/N): ")
    response <- readline()
    if (!tolower(response) %in% c("y", "yes")) {
      message("Cancelled")
      return(FALSE)
    }
  }

  tryCatch({
    unlink(cache_files)
    if (verbose) {
      message("Cleared ", length(cache_files), " files (",
              round(total_size, 1), "MB)")
    }
    return(TRUE)
  }, error = function(e) {
    warning("Failed to clear cache: ", e$message)
    return(FALSE)
  })
}

#' Test OSF Connection
#'
#' Test downloading a small dataset to verify OSF connectivity
#'
#' @param verbose Logical. Show detailed test results
#' @return Logical. TRUE if test successful
#' @export
test_osf_connection <- function(verbose = TRUE) {

  if (verbose) {
    message("Testing OSF connection...")
  }

  # Test with Texas boundaries (should be smallest file)
  tryCatch({
    temp_file <- download_osf_data("texas_huc8_boundaries",
                                   force_download = TRUE, verbose = verbose)

    if (file.exists(temp_file) && file.size(temp_file) > 0) {
      if (verbose) {
        message("OSF connection test: SUCCESS")
        message("Downloaded file size: ", round(file.size(temp_file)/1024, 2), " KB")
      }
      return(TRUE)
    } else {
      if (verbose) {
        message("OSF connection test: FAILED - no file created")
      }
      return(FALSE)
    }

  }, error = function(e) {
    if (verbose) {
      message("OSF connection test: FAILED")
      message("Error: ", e$message)
    }
    return(FALSE)
  })
}
