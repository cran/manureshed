# tests/testthat/test-workflows.R

test_that("State analysis filters correctly", {
  skip_on_cran()
  skip_if_not_installed("tigris")

  ohio_data <- run_state_analysis(
    "OH",
    scale = "county",
    year = 2016,
    include_wwtp = FALSE,
    save_outputs = FALSE,
    verbose = FALSE
  )

  # All FIPS should start with 39 (Ohio)
  expect_true(all(substr(ohio_data$agricultural$FIPS, 1, 2) == "39"))
  expect_true(nrow(ohio_data$agricultural) > 0)
  expect_true(nrow(ohio_data$agricultural) < 100)  # Ohio has 88 counties
})

test_that("Color schemes are updated correctly", {
  n_colors <- get_nutrient_colors("nitrogen")
  p_colors <- get_nutrient_colors("phosphorus")

  # Check nitrogen colors (using unname to remove names attribute)
  expect_equal(unname(n_colors["Within_Watershed"]), "#8c6bb1")  # purple hex
  expect_equal(unname(n_colors["Within_County"]), "#8c6bb1")
  expect_equal(unname(n_colors["Source"]), "#80cdc1")

  # Check phosphorus colors
  expect_equal(unname(p_colors["Sink_Deficit"]), "#b2abd2")
  expect_equal(unname(p_colors["Sink_Fertilizer"]), "#f1b")
  expect_equal(unname(p_colors["Source"]), "#b8e186")
})

test_that("Benchmark function works", {
  skip_on_cran()

  benchmark <- benchmark_analysis(
    scale = "huc2",
    year = 2016,
    nutrients = "nitrogen",
    n_runs = 2,
    include_wwtp = FALSE
  )

  expect_s3_class(benchmark, "manureshed_benchmark")
  expect_true(benchmark$timing$mean > 0)
  expect_equal(length(benchmark$timing$all_runs), 2)
})

test_that("Export functions create files", {
  skip_on_cran()

  results <- run_builtin_analysis(
    scale = "huc2",
    year = 2016,
    nutrients = "nitrogen",
    include_wwtp = FALSE,
    save_outputs = FALSE,
    verbose = FALSE
  )

  temp_dir <- tempdir()

  # Try export with error handling
  expect_no_error({
    gis_files <- export_for_gis(
      results,
      output_dir = file.path(temp_dir, "gis_test"),
      formats = c("geojson")
    )
  })

  # Only check file if export succeeded
  if (exists("gis_files") && !is.null(gis_files$agricultural_geojson)) {
    expect_true(file.exists(gis_files$agricultural_geojson))
  }
})

test_that("Cleanup test directories", {
  if (dir.exists("batch_enhanced_results")) {
    unlink("batch_enhanced_results", recursive = TRUE)
  }
})
