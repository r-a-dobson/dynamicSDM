
test_bandname<-"LST_Day_1km"
test_datasetname<-"MODIS/006/MOD11A1"
test.spatial.res.metres<-111320


test_that("stops if no spatial.ext provided", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
 expect_error(extract_dynamic_raster(dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no dates provided", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no temporal.res provided", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no temporal.direction provided", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no datasetname provided", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                        bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no bandname provided", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no spatial.res.metres provided", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  googledrive::drive_auth(user.email)
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,
                         GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no GEE.math.fun provided", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  googledrive::drive_auth(user.email)
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction"))})

test_that("stops if no save.drive.folder provided", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  googledrive::drive_auth(user.email)
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  expect_error(extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=c("2010-01-01","2011-01-01"),temporal.res=14,temporal.direction="prior",
                         datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                         GEE.math.fun="mean",user.email=user.email))})

test_that("Success if extent = numeric used", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  googledrive::drive_auth(user.email)
  googledrive::drive_rm("LST_Day_1km_14_prior_mean_raster_2010-01-01.tif")
  googledrive::drive_rm("LST_Day_1km_14_prior_mean_raster_2011-01-01.tif")
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  datelist<-c("2011-01-01")
  results<-extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=datelist,temporal.res=14,temporal.direction="prior",
                                      datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                                      GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction")
    expect_equal(length(results),length(datelist))})


test_that("Success if extent = Extent object used", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  googledrive::drive_auth(user.email)
  googledrive::drive_rm("LST_Day_1km_14_prior_mean_raster_2010-01-01.tif")
  googledrive::drive_rm("LST_Day_1km_14_prior_mean_raster_2011-01-01.tif")

  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  datelist<-c("2010-01-01")
  Extent<-raster::extent(c(12,36,-35,-12))
  results<-extract_dynamic_raster(spatial.ext=Extent,dates=datelist,temporal.res=14,temporal.direction="prior",
                                  datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                                  GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction")
  expect_equal(length(results),length(datelist))})


test_that("Success if extent = RasterLayer object used", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  googledrive::drive_auth(user.email)
  googledrive::drive_rm("LST_Day_1km_14_prior_mean_raster_2010-01-01.tif")
  googledrive::drive_rm("LST_Day_1km_14_prior_mean_raster_2011-01-01.tif")

  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  datelist<-c("2011-01-01")
  numeric<-c(12,36,-35,-12)
  raster<-raster::raster(raster::extent(numeric))
  results<-extract_dynamic_raster(spatial.ext=raster,dates=datelist,temporal.res=14,temporal.direction="prior",
                                  datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                                  GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction")
  expect_equal(length(results),length(datelist))})

test_that("Success if extent = polygon object used", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  googledrive::drive_auth(user.email)
  googledrive::drive_rm("LST_Day_1km_14_prior_mean_raster_2010-01-01.tif")
  googledrive::drive_rm("LST_Day_1km_14_prior_mean_raster_2011-01-01.tif")

  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  datelist<-c("2011-01-01")
  polygon<-sp::Polygon(cbind(c(12,12,36,36),c(-35,-12,-35,-12)))
  results<-extract_dynamic_raster(spatial.ext=polygon,dates=datelist,temporal.res=14,temporal.direction="prior",
                                  datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                                  GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction")
  expect_equal(length(results),length(datelist))})


test_that("Success if temporal.direction post used", {
   skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
   googledrive::drive_auth(user.email)
   googledrive::drive_rm("LST_Day_1km_14_prior_mean_raster_2010-01-01.tif")
   googledrive::drive_rm("LST_Day_1km_14_prior_mean_raster_2011-01-01.tif")

  datelist<-c("2010-01-01")
  results<-extract_dynamic_raster(spatial.ext=c(0,0,20,20),dates=datelist,temporal.res=14,temporal.direction="post",
                                  datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=10000,
                                  GEE.math.fun="mean",user.email=user.email,save.drive.folder="temporary_folder_buffered_extraction")
  expect_equal(length(results),length(datelist))})



