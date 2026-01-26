## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----install_deps, eval=FALSE-------------------------------------------------
# # Install dashboard dependencies
# install.packages(c(
#   "shiny",
#   "shinydashboard",
#   "leaflet",
#   "plotly",
#   "DT"
# ))

## ----launch, eval=FALSE-------------------------------------------------------
# library(manureshed)
# 
# # Launch the dashboard
# launch_dashboard()

## ----launch_port, eval=FALSE--------------------------------------------------
# # Launch on specific port
# launch_dashboard(port = 3838)

## ----import_excel, eval=FALSE-------------------------------------------------
# # In Excel or Google Sheets:
# # File > Import > CSV
# # Select the downloaded file

## ----import_r, eval=FALSE-----------------------------------------------------
# # Read downloaded data
# data <- read.csv("manureshed_huc8_2016_nitrogen.csv")
# 
# # Filter to sources only
# sources <- data[data$N_class == "Source", ]
# 
# # Calculate statistics
# mean(sources$N_surplus)

## ----fix_shiny, eval=FALSE----------------------------------------------------
# install.packages(c("shiny", "shinydashboard", "leaflet", "plotly", "DT"))

## ----reinstall, eval=FALSE----------------------------------------------------
# devtools::install()  # Reinstall package

## ----hybrid, eval=FALSE-------------------------------------------------------
# # 1. Explore with dashboard
# launch_dashboard()
# # Identify interesting patterns
# 
# # 2. Reproduce in code for publication
# results <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )
# 
# # 3. Do advanced analysis
# comparison <- compare_scenarios(...)

## ----share_code, eval=FALSE---------------------------------------------------
# # Colleagues can reproduce with:
# library(manureshed)
# results <- run_builtin_analysis(
#   scale = "huc8",
#   year = 2016,
#   nutrients = "nitrogen",
#   include_wwtp = TRUE
# )

## ----deploy_cloud, eval=FALSE-------------------------------------------------
# library(rsconnect)
# 
# # Configure account (one time)
# setAccountInfo(name="your-account", token="...", secret="...")
# 
# # Deploy
# deployApp(
#   appDir = system.file("shiny", "dashboard", package = "manureshed"),
#   appName = "manureshed-dashboard"
# )

## ----deploy_connect, eval=FALSE-----------------------------------------------
# # Follow RStudio Connect deployment guide
# # Requires RStudio Connect server

## ----get_help, eval=FALSE-----------------------------------------------------
# ?launch_dashboard
# ?run_builtin_analysis

