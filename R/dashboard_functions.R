#' Launch Interactive Manureshed Dashboard
#'
#' Opens an interactive Shiny dashboard for exploring nutrient balance data
#' without writing R code. Users can select analysis parameters, view interactive
#' maps, explore statistics, and download results.
#'
#' @param port Integer. Port number for the Shiny server (default: random available port)
#' @param launch.browser Logical. Open browser automatically? (default: TRUE)
#'
#' @details
#' This function launches an interactive dashboard with the following features:
#' \itemize{
#'   \item Select analysis parameters (year, scale, nutrients, WWTP inclusion)
#'   \item View interactive maps of nutrient classifications
#'   \item Explore summary statistics and distributions
#'   \item Browse results in interactive data tables
#'   \item Download results as CSV files
#'   \item Generate and download PDF reports
#' }
#'
#' The dashboard requires several suggested packages. If not installed, run:
#' \code{install.packages(c("shiny", "shinydashboard", "leaflet", "plotly", "DT"))}
#'
#' @return NULL (invisibly). Launches the Shiny application.
#'
#' @export
#' @examples
#' \dontrun{
#' # Launch the dashboard (requires suggested packages)
#' launch_dashboard()
#'
#' # Launch on specific port
#' launch_dashboard(port = 3838)
#' }
launch_dashboard <- function(port = NULL, launch.browser = TRUE) {

  # Check if shiny is installed
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required for the dashboard.\n",
      "Install it with: install.packages('shiny')",
      call. = FALSE
    )
  }

  # Check other required packages
  required_pkgs <- c("shinydashboard", "leaflet", "plotly", "DT")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      "Dashboard requires additional packages: ",
      paste(missing_pkgs, collapse = ", "), "\n",
      "Install with: install.packages(c('",
      paste(missing_pkgs, collapse = "', '"), "'))",
      call. = FALSE
    )
  }

  # Get app directory
  app_dir <- system.file("shiny", "dashboard", package = "manureshed")

  if (app_dir == "" || !dir.exists(app_dir)) {
    stop(
      "Could not find dashboard files. Please reinstall manureshed:\n",
      "devtools::install()",
      call. = FALSE
    )
  }

  # Launch app
  message("Launching Manureshed Dashboard...")
  message("Dashboard running at http://localhost:",
          if(is.null(port)) "random_port" else port)
  message("Press Ctrl+C or Esc to stop the dashboard")

  if (is.null(port)) {
    shiny::runApp(app_dir, launch.browser = launch.browser)
  } else {
    shiny::runApp(app_dir, port = port, launch.browser = launch.browser)
  }
}
