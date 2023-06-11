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


test_that("Works if static rasters", {
  dates <- c("2010-01-01")
  raster <- terra::rast(terra::ext(c(12, 36, -35, -12)))
  raster <-terra::setValues(raster, 1:terra::ncell(raster) )
  results <- dynamic_proj_covariates(
      dates = dates,
      varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
      local.directory = testthat::test_path("test-files"),
      spatial.ext = raster,
      spatial.res.degrees = 10,
      resample.method = "bilinear",
      cov.file.type = "csv",
      static.rasters = raster,
      static.varnames = "mystatic",
      static.resample.method = "bilinear",
      spatial.mask = sample_extent_data,
      save.directory = tempdir()
    )
  expect_equal(length(results), length(dates))
})


test_that("Works if static rasters x2", {
  dates <- c("2010-01-01")
  raster <- terra::rast(terra::ext(c(12, 36, -35, -12)))
  raster <-terra::setValues(raster, 1:terra::ncell(raster) )
  raster <-c(raster,raster)
  results <- dynamic_proj_covariates(
    dates = dates,
    varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
    local.directory = testthat::test_path("test-files"),
    spatial.ext = raster,
    spatial.res.degrees = 10,
    resample.method = "bilinear",
    cov.file.type = "csv",
    static.rasters = raster,
    static.varnames = c("mystatic","mystatic2"),
    static.resample.method = "bilinear",
    spatial.mask = sample_extent_data,
    save.directory = tempdir()
  )
  expect_equal(length(results), length(dates))
})

test_that("Works if static rasters and buffering", {
  dates <- c("2010-01-01")
  raster <- terra::rast(terra::ext(c(12, 36, -35, -12)))
  raster <-terra::setValues(raster, 1:terra::ncell(raster) )
  raster <-c(raster,raster)
  results <- dynamic_proj_covariates(
    dates = dates,
    varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
    local.directory = testthat::test_path("test-files"),
    spatial.ext = raster,
    spatial.res.degrees = 10,
    resample.method = "bilinear",
    cov.file.type = "csv",
    static.rasters = raster,
    static.varnames = c("mystatic","mystatic2"),
    static.moving.window.matrix = matrix(1,3,3),
    static.GEE.math.fun = "mean",
    static.resample.method = "bilinear",
    spatial.mask = sample_extent_data,
    save.directory = tempdir()
  )
  expect_equal(length(results), length(dates))
})










test_that("Works if spatial.ext = polygon", {
  dates <- c("2010-01-01")
  polygon <-  sf::st_polygon(list(cbind(c(20, 12, 36, 36,20), c(-35, -12, -35, -12,-35))))
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


test_that("Works if spatial.ext = sf geom", {
  dates <- c("2010-01-01")
  results <- dynamic_proj_covariates(
    dates = dates,
    varnames = c("precipitation_10_prior_sum", "NDVI_5_post_max"),
    local.directory = testthat::test_path("test-files"),
    spatial.ext = sample_extent_data$geometry,
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


