

test_that("stops if no local.directory provided", {
  expect_error(extract_coords_combine(
    varnames = c(
      "EVI_7_post_mean",
      "LST_Day_1km_7_post_mean",
      "Fpar_20_prior_max"
    )
  ))
})

test_that("stops if no varnames provided", {
  expect_error(extract_coords_combine(local.directory = paste0(testthat::test_path(), "/test-files")))
})

test_that("stops if local.directory provided does not exist", {
  expect_error(extract_coords_combine(
    local.directory = "not a real folder",
    varnames = c(
      "EVI_7_post_mean",
      "LST_Day_1km_7_post_mean",
      "Fpar_20_prior_max"
    )
  ))
})

test_that("stops if no matching files to combine", {
  expect_error(extract_coords_combine(
    local.directory = paste0(testthat::test_path(), "/test-files"),
    varnames = c("landcover_7_post_mean")
  ))
})

test_that("returns data.frame", {
  results <- extract_coords_combine(
      local.directory = paste0(testthat::test_path(), "/test-files"),
      varnames = c(
        "EVI_7_post_mean",
        "LST_Day_1km_7_post_mean",
        "Fpar_20_prior_max"
      )
    )
  expect_equal(class(results), "data.frame")
})

