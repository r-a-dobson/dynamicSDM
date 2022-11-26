
test_bandname<-"LST_Day_1km"
test_datasetname<-"MODIS/006/MOD11A1"
test.spatial.res.metres<-111320
user.email<-as.character(gargle::gargle_oauth_sitrep()$email)

test_that("stops if no spatial.ext provided", {
 expect_error(extract_dynamic_raster(dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no dates provided", {
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no temporal.res provided", {
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no temporal.direction provided", {
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no datasetname provided", {
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                        bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no bandname provided", {
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no spatial.res.metres provided", {
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no GEE.math.fun provided", {
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no save.drive.folder provided", {
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email))})

test_that("Success if extent = numeric used", {
  datelist<-c("2010-01-01","2011-01-01")
  results<-extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                                      datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                                      GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction")
    expect_equal(length(results),length(datelist))})


test_that("Success if extent = Extent object used", {
  datelist<-c("2010-01-01","2011-01-01")
  Extent<-raster::extent(c(12,36,-35,-12))
  results<-extract_dynamic_raster(spatial.ext=Extent,dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                                  datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                                  GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction")
  expect_equal(length(results),length(datelist))})


test_that("Success if extent = RasterLayer object used", {
  datelist<-c("2010-01-01","2011-01-01")
  numeric<-c(12,36,-35,-12)
  raster<-raster::raster(raster::extent(numeric))
  results<-extract_dynamic_raster(spatial.ext=raster,dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                                  datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                                  GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction")
  expect_equal(length(results),length(datelist))})

test_that("Success if extent = polygon object used", {
  datelist<-c("2010-01-01","2011-01-01")
  polygon<-sp::Polygon(cbind(c(12,12,36,36),c(-35,-12,-35,-12)))
  results<-extract_dynamic_raster(spatial.ext=polygon,dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                                  datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                                  GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction")
  expect_equal(length(results),length(datelist))})

test_that("Success if temporal.direction post used", {
  datelist<-c("2010-01-01","2011-01-01")
  results<-extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="post",
                                  datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                                  GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction")
  expect_equal(length(results),length(datelist))})



