
test_that("stops if no spatial.res.degrees and no spatial.res.metres", {expect_error(get_moving_window(radial.distance=100, spatial.ext = c(11.72083,40.84583,-46.9625 ,-4.425 )))})

test_that("stops if no radial.distance", {expect_error(get_moving_window(spatial.res.degrees=0.3,spatial.res.metres = 1000, spatial.ext = c(11.72083,40.84583,-46.9625 ,-4.425 )))})

test_that("stops if spatial.res.degrees class is not numeric", {expect_error(get_moving_window(radial.distance=100,spatial.res.degrees="zero point five degrees",spatial.res.metres = 1000, spatial.ext = c(11.72083,40.84583,-46.9625 ,-4.425 )))})

test_that("stops if spatial.res.degrees given but spatial.ext not given", {expect_error(get_moving_window(radial.distance=100,spatial.res.degrees=0.5))})

test_that("stops if spatial.res.degrees and spatial.ext class invalid", {expect_error(get_moving_window(radial.distance=100,spatial.res.degrees=0.5,spatial.ext="southernafrica"))})

test_that("stops if spatial.res.degrees and spatial.ext class numeric and incorrect length invalid", {
    expect_error(get_moving_window(radial.distance=100,spatial.res.degrees=0.5,spatial.ext=c(2,3,4)))})

test_that("stops if spatial.res.degrees and spatial.ext class numeric and incorrect length invalid", {
  expect_error(get_moving_window(radial.distance=100,spatial.res.degrees=0.5,spatial.ext=c(2,3,4)))})

test_that("works with spatial.ext as numeric", {
  numeric<-c(12,36,-35,-12)
  results<-get_moving_window(radial.distance=100,spatial.res.degrees=0.5,spatial.ext=numeric)
  expect_equal(class(results),"matrix")})

test_that("works with spatial.ext as Extent", {
  Extent<-raster::extent(c(12,36,-35,-12))
  results<-get_moving_window(radial.distance=100,spatial.res.degrees=0.5,spatial.ext=Extent)
  expect_equal(class(results),"matrix")})

test_that("works with spatial.ext as raster", {
  numeric<-c(12,36,-35,-12)
  raster<-raster::raster(raster::extent(numeric))
  results<-get_moving_window(radial.distance=100000,spatial.res.degrees=0.5,spatial.ext=raster)
  expect_equal(class(results),"matrix")})

test_that("works with spatial.ext as polygon", {
  polygon<-sp::Polygon(cbind(c(12,12,36,36),c(-35,-12,-35,-12)))
  results<-get_moving_window(radial.distance=100000,spatial.res.degrees=0.5,spatial.ext=polygon)
  expect_equal(class(results),"matrix")})

test_that("stops if spatial.res.degrees and spatial.ext class invalid", {
  expect_error(get_moving_window(radial.distance=100000,spatial.res.degrees=0.5,spatial.ext="southernafrica"))})

test_that("Error if wrong class spatial.res.metres", {
  expect_error(get_moving_window(radial.distance=100000,spatial.res.metres="300m"))})





