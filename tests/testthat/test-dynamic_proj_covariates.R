dates<-c("2010-01-01","2011-01-01")
Extent<-raster::extent(c(12,36,-35,-12))

test_that("Stops if dates missing", {
    expect_error(dynamic_proj_covariates(varnames=c("precipitation_10_prior_sum","NDVI_5_post_max"),local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                               spatial.ext=Extent,spatial.res.degrees=0.05,resample.method="bilinear",
                                               save.directory=tempdir()))})

test_that("Stops if varnames missing", {
  expect_error(dynamic_proj_covariates(dates=dates,local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                             spatial.ext=Extent,spatial.res.degrees=0.05,resample.method="bilinear",
                                             save.directory=tempdir()))})

test_that("Stops if spatial.res.degrees given and resample.method missing", {
  expect_error(dynamic_proj_covariates(dates=dates,varnames=c("precipitation_10_prior_sum","NDVI_5_post_max"),local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                             spatial.ext=Extent,spatial.res.degrees=0.05,
                                             save.directory=tempdir()))})

test_that("Stops if save.directory and save.drive.folder missing", {
  expect_error(dynamic_proj_covariates(dates=dates,varnames=c("precipitation_10_prior_sum","NDVI_5_post_max"),local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                             spatial.ext=Extent,spatial.res.degrees=0.05,resample.method="bilinear"))})

test_that("Stops if save.directory doesn't exist", {
  expect_error(dynamic_proj_covariates(dates=dates,varnames=c("precipitation_10_prior_sum","NDVI_5_post_max"),local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                             spatial.ext=Extent,spatial.res.degrees=0.05,resample.method="bilinear",
                                             save.directory="tempdir"))})

test_that("Stops if wrong length of resample.method (not 1 or equal to n. variables)", {
  expect_error(dynamic_proj_covariates(dates=dates,varnames=c("precipitation_10_prior_sum","NDVI_5_post_max"),local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                             spatial.ext=Extent,spatial.res.degrees=0.05,resample.method=c("method1","method2"),save.directory=tempdir()))})

test_that("Stops if wrong class spatial.ext", {
  expect_error(dynamic_proj_covariates(dates=dates,varnames=c("precipitation_10_prior_sum","NDVI_5_post_max"),local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                             spatial.ext="Extent",spatial.res.degrees=0.05,resample.method="bilinear",
                                             save.directory=tempdir()))})

test_that("Works if spatial.ext = numeric", {
  dates<-c("2010-01-01","2011-01-01")
 results<-dynamic_proj_covariates(dates=dates,varnames=c("precipitation_10_prior_sum","NDVI_5_post_max"),local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                            spatial.ext=c(12,36,-35,-12),spatial.res.degrees=0.05,resample.method="bilinear",
                            save.directory=tempdir())
 expect_equal(length(results),length(dates))})


test_that("Works if spatial.ext = Extent", {
  dates<-c("2010-01-01","2011-01-01")
  Extent<-raster::extent(c(12,36,-35,-12))
  results<-dynamic_proj_covariates(dates=dates,varnames=c("precipitation_10_prior_sum","NDVI_5_post_max"),local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                         spatial.ext=Extent,spatial.res.degrees=0.05,resample.method="bilinear",
                                         save.directory=tempdir())
  expect_equal(length(results),length(dates))})


test_that("Works if spatial.ext = RasterLayer", {
  dates<-c("2010-01-01","2011-01-01")
  raster<-raster::raster(raster::extent(c(12,36,-35,-12)))
  results<-dynamic_proj_covariates(dates=dates,varnames=c("precipitation_10_prior_sum","NDVI_5_post_max"),local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                         spatial.ext=raster,spatial.res.degrees=0.05,resample.method="bilinear",
                                         save.directory=tempdir())
  expect_equal(length(results),length(dates))})


test_that("Works if spatial.ext = polygon", {
  dates<-c("2010-01-01","2011-01-01")
  polygon<-sp::Polygon(cbind(c(12,12,36,36),c(-35,-12,-35,-12)))
  results<-dynamic_proj_covariates(dates=dates,varnames=c("precipitation_10_prior_sum","NDVI_5_post_max"),local.directory="C:/Users/eerdo/Documents/dynamicSDM/tests/testthat/test-files",
                                         spatial.ext=polygon,spatial.res.degrees=0.05,resample.method="bilinear",
                                         save.directory=tempdir())
  expect_equal(length(results),length(dates))})



test_that("Works if spatial.ext = polygon and Google Drive used", {
  skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  dates=c("2010-01-01","2010-04-01")
  polygon<-sp::Polygon(cbind(c(12,12,36,36),c(-35,-12,-35,-12)))
  results<-dynamic_proj_covariates(dates=dates,varnames=c("Temperaturemean","TemperatureEightmean","Precipitationsum","Precipitation8Wsum"),user.email=user.email,drive.folder="temporary_folder_buffered_extraction",
                                   spatial.ext=polygon,spatial.res.degrees=0.05,resample.method="bilinear",
                                   save.drive.folder = "testfiles")

  expect_equal(length(results),length(dates))})
