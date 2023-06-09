dates <- c("2010-01-01", "2011-01-01")
Extent <- terra::ext(c(12, 36, -35, -12))
data("sample_extent_data")

test_that("Stops if dates missing", {
  expect_error(
    dynamic_proj_covariates(
      varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
      local.directory = testthat::test_path("test-files"),
      spatial.ext = Extent,
      spatial.res.degrees = 0.05,
      resample.method = "bilinear",
      save.directory = tempdir()
    )
  )
})

test_that("Stops if varnames missing", {
  expect_error(
    dynamic_proj_covariates(
      dates = dates,
      local.directory = testthat::test_path("test-files"),
      spatial.ext = Extent,
      spatial.res.degrees = 0.05,
      resample.method = "bilinear",
      save.directory = tempdir()
    )
  )
})

test_that("Stops if spatial.res.degrees given and resample.method missing",
          {
            expect_error(
              dynamic_proj_covariates(
                dates = dates,
                varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
                local.directory = testthat::test_path("test-files"),
                spatial.ext = Extent,
                spatial.res.degrees = 0.05,
                save.directory = tempdir()
              )
            )
          })

test_that("Stops if save.directory and save.drive.folder missing", {
  expect_error(
    dynamic_proj_covariates(
      dates = dates,
      varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
      local.directory = testthat::test_path("test-files"),
      spatial.ext = Extent,
      spatial.res.degrees = 0.05,
      resample.method = "bilinear"
    )
  )
})

test_that("Stops if wrong length of resample.method (not 1 or equal to n. variables)",
          {
            expect_error(
              dynamic_proj_covariates(
                dates = dates,
                varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
                local.directory = testthat::test_path("test-files"),
                spatial.ext = Extent,
                spatial.res.degrees = 0.05,
                resample.method = c("method1", "method2", "method9"),
                save.directory = tempdir()
              )
            )
          })

test_that("Stops if wrong class spatial.ext", {
  expect_error(
    dynamic_proj_covariates(
      dates = dates,
      varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
      local.directory = testthat::test_path("test-files"),
      spatial.ext = "Extent",
      spatial.res.degrees = 0.05,
      resample.method = "bilinear",
      save.directory = tempdir()
    )
  )
})

test_that("Works if spatial.ext = numeric", {
  dates <- c("2010-01-01")
  results <- dynamic_proj_covariates(
      dates = dates,
      varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
      local.directory = testthat::test_path("test-files"),
      spatial.ext = c(12, 36, -35, -12),
      spatial.res.degrees = 10,
      resample.method = "bilinear",
      cov.file.type = "csv",
      save.directory = tempdir()
    )
  expect_equal(length(results), length(dates))
})


test_that("Works if spatial.ext = Extent", {
  dates <- c("2010-01-01")
  Extent <- terra::ext(c(12, 36, -35, -12))
  results <- dynamic_proj_covariates(
      dates = dates,
      varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
      local.directory = testthat::test_path("test-files"),
      spatial.ext = Extent,
      spatial.res.degrees = 10,
      resample.method = "bilinear",
      cov.file.type = "csv",
      save.directory = tempdir()
    )
  expect_equal(length(results), length(dates))
})


test_that("Works if spatial.ext = RasterLayer", {
  dates <- c("2010-01-01")
  raster <- terra::rast(terra::ext(c(12, 36, -35, -12)))
  results <- dynamic_proj_covariates(
      dates = dates,
      varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
      local.directory = testthat::test_path("test-files"),
      spatial.ext = raster,
      spatial.res.degrees = 10,
      resample.method = "bilinear",
      cov.file.type = "csv",
      spatial.mask = sample_extent_data,
      save.directory = tempdir()
    )
  expect_equal(length(results), length(dates))
})



test_that("Works if spatial.ext = polygon", {
  dates <- c("2010-01-01")
  polygon <- sp::Polygon(cbind(c(12, 12, 36, 36), c(-35, -12, -35, -12)))
  results <- dynamic_proj_covariates(
      dates = dates,
      varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
      local.directory = testthat::test_path("test-files"),
      spatial.ext = polygon,
      spatial.res.degrees = 10,
      resample.method = "bilinear",
      cov.file.type = "csv",
      save.directory = tempdir()
    )
  expect_equal(length(results), length(dates))
})



test_that("Works if spatial.ext = sf", {
  dates <- c("2010-01-01")
  results <- dynamic_proj_covariates(
      dates = dates,
      varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
      local.directory = testthat::test_path("test-files"),
      spatial.ext = sample_extent_data,
      spatial.res.degrees = 10,
      resample.method = "bilinear",
      cov.file.type = "csv",
      save.directory = tempdir()
    )
  expect_equal(length(results), length(dates))
})


test_that("Works if method is tif", {
  dates = c("2010-01-01")
  results <- dynamic_proj_covariates(
      dates = dates,
      varnames = c("Temperaturemean_ra",
                   "TemperatureEightmean_ra",
                   "Precipitationsum_ra",
                   "Precipitation8Wsum_ra"),
      local.directory = testthat::test_path("test-files"),
      spatial.ext = sample_extent_data,
      spatial.res.degrees = 10,
      resample.method = "bilinear",
      cov.file.type = "tif",
      save.directory = tempdir()
    )
  expect_equal(length(results), length(dates))
})

data("sample_extent_data")
test_that("Works if method is tif and spatial mask", {
  dates = c("2010-01-01")
  results <- dynamic_proj_covariates(
      dates = dates,
      varnames = c("Temperaturemean_ra",
                   "TemperatureEightmean_ra",
                   "Precipitationsum_ra",
                   "Precipitation8Wsum_ra"),
      local.directory = testthat::test_path("test-files"),
      spatial.ext = sample_extent_data,
      spatial.res.degrees = 10,
      resample.method = "bilinear",
      cov.file.type = "tif",
      spatial.mask = sample_extent_data,
      save.directory = tempdir()
    )
  expect_equal(length(results), length(dates))
})


