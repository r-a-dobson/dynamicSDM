
test_bandname<-"LST_Day_1km"
test_datasetname<-"MODIS/006/MOD11A1"
test.spatial.res.metres<-111320
user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
test_that("stops if no spatial.ext provided", {
  expect_error(extract_buffered_raster(dates=c("2010-01-01","2013-01-01"),temporal.res=9,
                          temporal.direction="prior",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                          bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                          GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email))})

test_that("stops if no dates provided", {
  expect_error(extract_buffered_raster(spatial.ext=c(12,36,-35,-12),temporal.res=9,
                                       temporal.direction="prior",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                       GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email))})

test_that("stops if no datasetname provided", {
  expect_error(extract_buffered_raster(spatial.ext=c(12,36,-35,-12),dates=c("2010-01-01","2013-01-01"),temporal.res=9,
                                       temporal.direction="prior",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       bandname=test_bandname,spatial.res.metres=test.spatial.res.metres,
                                       GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email))})

test_that("stops if no bandname provided", {
  expect_error(extract_buffered_raster(spatial.ext=c(12,36,-35,-12),dates=c("2010-01-01","2013-01-01"),temporal.res=9,
                                       temporal.direction="prior",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                        datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                       GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email))})

test_that("stops if no spatial.res.metres provided", {
  expect_error(extract_buffered_raster(spatial.ext=c(12,36,-35,-12),dates=c("2010-01-01","2013-01-01"),temporal.res=9,
                                       temporal.direction="prior",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       bandname=test_bandname,datasetname=test_datasetname,
                                       GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email))})

test_that("stops if no GEE.math.fun provided", {
  expect_error(extract_buffered_raster(spatial.ext=c(12,36,-35,-12),dates=c("2010-01-01","2013-01-01"),temporal.res=9,
                                       temporal.direction="prior",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                      save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email))})

test_that("stops if no save.drive.folder provided", {
  expect_error(extract_buffered_raster(spatial.ext=c(12,36,-35,-12),dates=c("2010-01-01","2013-01-01"),temporal.res=9,
                                       temporal.direction="prior",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                       GEE.math.fun="max",user.email=user.email))})

test_that("stops if no moving.window.matrix provided", {
  expect_error(extract_buffered_raster(spatial.ext=c(12,36,-35,-12),dates=c("2010-01-01","2013-01-01"),temporal.res=9,
                                       temporal.direction="prior",bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                       GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email))})

test_that("stops if temporal.res but no temporal.direction provided", {
  expect_error(extract_buffered_raster(spatial.ext=c(12,36,-35,-12),dates=c("2010-01-01","2013-01-01"),temporal.res=9,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                       GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email))})



test_that("Success if extent = numeric used", {
  datelist<-c("2010-01-01","2011-01-01")
  results<- extract_buffered_raster(spatial.ext=c(12,36,-35,-12),dates=datelist,temporal.res=9,
                                    temporal.direction="prior",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                    bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                    GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email)
  expect_equal(length(results),length(datelist))})

test_that("Success if extent = Extent object used", {
  datelist<-c("2010-01-01","2011-01-01")
  Extent<-raster::extent(c(12,36,-35,-12))
  results<- extract_buffered_raster(spatial.ext=Extent,dates=datelist,temporal.res=9,
                                    temporal.direction="prior",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                    bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                    GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email)
    expect_equal(length(results),length(datelist))})

test_that("Success if extent = RasterLayer object used", {
  datelist<-c("2010-01-01","2011-01-01")
  numeric<-c(12,36,-35,-12)
  raster<-raster::raster(raster::extent(numeric))
  results<- extract_buffered_raster(spatial.ext=raster,dates=datelist,temporal.res=9,
                                    temporal.direction="prior",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                    bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                    GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email)
    expect_equal(length(results),length(datelist))})

test_that("Success if extent = polygon object used", {
  datelist<-c("2010-01-01","2011-01-01")
  polygon<-sp::Polygon(cbind(c(12,12,36,36),c(-35,-12,-35,-12)))
  results<- extract_buffered_raster(spatial.ext=polygon,dates=datelist,temporal.res=9,
                                    temporal.direction="prior",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                    bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                    GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email)
    expect_equal(length(results),length(datelist))})

test_that("Success if temporal.direction post used", {
  datelist<-c("2010-01-01","2011-01-01")
  results<- extract_buffered_raster(spatial.ext=c(12,36,-35,-12),dates=datelist,temporal.res=9,
                                    temporal.direction="post",moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                    bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                    GEE.math.fun="max",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email)
    expect_equal(length(results),length(datelist))})




test_datasetname<-"MODIS/006/MCD12Q1"
test_bandname<-"LC_Type5"
test.spatial.res.metres<-111320
test.categories<-c(6,7)

test_that("Success if categorical no temporal used", {
  datelist<-c("2010-01-01","2011-01-01")
  results<- extract_buffered_raster(spatial.ext=c(12,36,-35,-12),dates=datelist
                                    ,moving.window.matrix=matrix(1/9,nrow=3,ncol=3),categories = test.categories,
                                    bandname=test_bandname,datasetname=test_datasetname,spatial.res.metres=test.spatial.res.metres,
                                    GEE.math.fun="sum",save.drive.folder="temporary_folder_buffered_extraction",user.email=user.email)
  expect_equal(length(results),length(datelist))})


