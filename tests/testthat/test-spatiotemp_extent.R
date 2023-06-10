

data("sample_explan_data")

test_that("stops if no occcurence data provided", {
  expect_error(spatiotemp_extent())
})

test_that("stop if temporal.ext wrong class", {
  expect_error(spatiotemp_extent(occ.data = sample_explan_data, temporal.ext =
                                   1234))
})

test_that("stop if temporal.ext length not equal 2", {
  expect_error(spatiotemp_extent(
    occ.data = sample_explan_data,
    temporal.ext = c("2011-01-02")
  ))
})

test_that("stop if temporal.ext contains one invalid date", {
  expect_error(spatiotemp_extent(
    occ.data = sample_explan_data,
    temporal.ext = c("2011-01-02", "2011-14-02")
  ))
})

test_that("stop if temporal.ext contains two invalid date", {
  expect_error(spatiotemp_extent(
    occ.data = sample_explan_data,
    temporal.ext = c("2011-91-02", "2011-14-02")
  ))
})

test_that("stop if temporal.ext not two sequential dates", {
  expect_error(spatiotemp_extent(
    occ.data = sample_explan_data,
    temporal.ext = c("2011-01-02", "2010-01-02")
  ))
})

test_that("Message if occurrence data contains invalid dates", {
  testdata <- sample_explan_data
  testdata[4, "day"] <- 56
  expect_warning(spatiotemp_extent(
    occ.data = testdata,
    temporal.ext = c("2001-01-02", "2020-02-02")
  ))
})

test_that("Filters by temporal.ext: all within dates", {
  results <- spatiotemp_extent(occ.data = sample_explan_data,
                      temporal.ext = c("2000-01-01", "2020-01-01"))
  expect_equal(nrow(results), nrow(sample_explan_data))
})

test_that("Filters by temporal.ext: some not within dates", {
  results <- spatiotemp_extent(occ.data = sample_explan_data,
                      temporal.ext = c("2013-01-01", "2020-01-01"))
  expect_equal(nrow(results), 148)
})

data(sample_extent_data)

test_that("Filters by spatial.ext but all within extent, numeric ", {
  numeric <- c(min(sample_explan_data$x)+1,
               max(sample_explan_data$x)-1,
               min(sample_explan_data$y)+1,
               max(sample_explan_data$y)-1
    )
  results <- spatiotemp_extent(occ.data = sample_explan_data, spatial.ext = numeric)
  expect_equal(nrow(results), 316)
})

test_that("Filters by spatial.ext but all within extent, Extent ", {
  Extent <- terra::ext(c(
      min(sample_explan_data$x)+1,
      max(sample_explan_data$x)-1,
      min(sample_explan_data$y)+1,
      max(sample_explan_data$y)-1
    ))
  results <- spatiotemp_extent(occ.data = sample_explan_data, spatial.ext = Extent)
  expect_equal(nrow(results), 316)
})

test_that("Filters by spatial.ext but all within extent, raster ", {
  numeric <- c(min(sample_explan_data$x)+1,
      max(sample_explan_data$x)-1,
      min(sample_explan_data$y)+1,
      max(sample_explan_data$y)-1
    )
  raster <- terra::rast(terra::ext(numeric))
  results <- spatiotemp_extent(occ.data = sample_explan_data, spatial.ext = raster)
  expect_equal(nrow(results), 316)
})

test_that("Filters by spatial.ext polygon ", {
  polygon <-  sf::st_polygon(list(cbind(c(20, 12, 36, 36,20), c(-35, -12, -35, -12,-35))))
  results <- spatiotemp_extent(occ.data = sample_explan_data, spatial.ext = polygon)
  expect_equal(nrow(results), 327)
})

test_that("Filters by spatial.ext but not all within extent, numeric ", {
  numeric <- c(24, 36, -35, -12)
  results <- spatiotemp_extent(occ.data = sample_explan_data, spatial.ext = numeric)
  expect_equal(nrow(results), 280)
})

test_that("Filters by spatial.ext but not all within extent, Extent ", {
  Extent <- terra::ext(c(24, 36, -35, -12))
  results <- spatiotemp_extent(occ.data = sample_explan_data, spatial.ext = Extent)
  expect_equal(nrow(results), 280)
})

test_that("Filters by spatial.ext but not all within extent, raster ", {
  numeric <- c(24, 36, -35, -12)
  raster <- terra::rast(terra::ext(numeric))
  raster <- terra::setValues(raster, values = 1:terra::ncell(raster))
  results <- spatiotemp_extent(occ.data = sample_explan_data, spatial.ext = raster)
  expect_equal(nrow(results), 280)
})

test_that("Filters by spatial.ext but all within extent, sf ", {
  results <- spatiotemp_extent(occ.data = sample_explan_data, spatial.ext = sample_extent_data)
  expect_equal(nrow(results), 328)
})



