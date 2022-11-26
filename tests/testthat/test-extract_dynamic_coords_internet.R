
data("sample_occ_abs_data")
sample_occ_abs_data_fortest<-sample_occ_abs_data[sample_occ_abs_data$year==2010,]
sample_occ_abs_data_fortest<-dplyr::sample_n(sample_occ_abs_data_fortest,4)
test_bandname<-"LST_Day_1km"
test_datasetname<-"MODIS/006/MOD11A1"


test_that("stops if no occcurence data provided", {
  expect_error(extract_dynamic_coords(datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                save.method="split",save.directory=tempdir()))})


test_that("stops if no datasetname provided", {
  expect_error(extract_dynamic_coords(occ=sample_occ_abs_data_fortest,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                               save.method="split",save.directory=tempdir()))})

test_that("stops if no bandname provided", {
  expect_error(extract_dynamic_coords(occ=sample_occ_abs_data_fortest,datasetname=test_datasetname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                               save.method="split",save.directory=tempdir()))})


test_that("stops if no spatial.res.metres provided", {
  expect_error(extract_dynamic_coords(occ=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                               save.method="split",save.directory=tempdir()))})


test_that("stops if no temporal.res provided", {
  expect_error(extract_dynamic_coords(occ=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.direction="prior",GEE.math.fun="mean",
                                               save.method="split",save.directory=tempdir()))})


test_that("stops if no temporal.direction provided", {
  expect_error(extract_dynamic_coords(occ=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,GEE.math.fun="mean",
                                               save.method="split",save.directory=tempdir()))})


test_that("stops if no GEE.math.fun provided", {
  expect_error(extract_dynamic_coords(occ=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",
                                               save.method="split",save.directory=tempdir()))})


test_that("stops if no save.method provided", {
  expect_error(extract_dynamic_coords(occ=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                               save.directory=tempdir()))})


test_that("stops if no save.directory provided", {
  expect_error(extract_dynamic_coords(occ=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                               save.method="split"))})


test_that("stops if more than one GEE.math.fun provided", {
  expect_error(extract_dynamic_coords(occ=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun=c("mean","max"),
                                      save.method="split", save.directory=tempdir()))})

test_that("stops if save.directory doesn't exist", {
  expect_error(extract_dynamic_coords(occ=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                      save.method="split", save.directory="incorrect/pathway/that/doesnt/exist"))})




test_that("stops if occcurence data not data.frame", {
  expect_error(extract_dynamic_coords(occ.data=c(20,30,40),datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                      save.method="split",save.directory=tempdir()))})

test_that("stops if datasetname not character", {
  expect_error(extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=5,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                      save.method="split",save.directory=tempdir()))})
test_that("stops if bandname not character", {
  expect_error(extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=88,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                      save.method="split",save.directory=tempdir()))})
test_that("stops if varname not character", {
  expect_error(extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,varname=300,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                      save.method="split",save.directory=tempdir()))})
test_that("stops if spatial.res.metres data not numeric", {
  expect_error(extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres="onethousand",temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                      save.method="split",save.directory=tempdir()))})
test_that("stops if temporal.res not numeric", {
  expect_error(extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res="seven",temporal.direction="prior",GEE.math.fun="mean",
                                      save.method="split",save.directory=tempdir()))})
test_that("stops if temporal.direction not character", {
  expect_error(extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction=5,GEE.math.fun="mean",
                                      save.method="split",save.directory=tempdir()))})


test_that("stops if save.method does not match accepted choices", {
  expect_error(extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                      save.method="notsure",save.directory=tempdir()))})

test_that("stops if temporal.direction does not match accepted choices", {
  expect_error(extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="before",GEE.math.fun="mean",
                                      save.method="split",save.directory=tempdir()))})

test_that("stops if GEE.math.fun does not match viable functions", {
  expect_error(extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="nofunction",
                                      save.method="split",save.directory=tempdir()))})



test_that("If method split, return numeric vector of completed rows, the same length as nrow(occ.data)", {
  results<-extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                      save.method="split",save.directory=tempdir())
  expect_equal(length(results),nrow(sample_occ_abs_data_fortest))})


test_that("If method split, return integer vector of completed rows", {
  results<-extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                  save.method="split",save.directory=tempdir())
  expect_equal(class(results),"integer")})



test_that("If method split save individual csvc files in save.directory, default varname", {
  results<-extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                  save.method="split",save.directory=tempdir())
  n<-sample(1:nrow(sample_occ_abs_data_fortest),1)
  expect_equal(file.exists(paste0(tempdir(),"/",n,"_",test_bandname,"_7_prior_mean_.csv")),TRUE)})



test_that("If method split save individual csv files in save.directory, specified varname", {
  results<-extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,varname="thisisatest",bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                  save.method="split",save.directory=tempdir())
  n<-sample(1:nrow(sample_occ_abs_data_fortest),1)
  expect_equal(file.exists(paste0(tempdir(),"/",n,"_thisisatest_.csv")),TRUE)})




test_that("If method combined save combined data csv file in save.directory, default varname", {
  results<-extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                  save.method="combined",save.directory=tempdir())
  expect_equal(file.exists(paste0(tempdir(),"/all_records_combined_",test_bandname,"_7_prior_mean_.csv")),TRUE)})



test_that("If method combined save combined data csv file in save.directory, default varname", {
  results<-extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,varname="thisisatest",spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                  save.method="combined",save.directory=tempdir())
  expect_equal(file.exists(paste0(tempdir(),"/all_records_combined_thisisatest_.csv")),TRUE)})



test_that("Combined method works with temporal.direction = post", {
  results<-extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="post",GEE.math.fun="mean",
                                  save.method="combined",save.directory=tempdir())
  n<-sample(1:nrow(sample_occ_abs_data_fortest),1)
  expect_equal(file.exists(paste0(tempdir(),"/all_records_combined_",test_bandname,"_7_post_mean_.csv")),TRUE)})


test_that("Split method works with temporal.direction = post", {
  results<-extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="post",GEE.math.fun="mean",
                                  save.method="split",save.directory=tempdir())
  n<-sample(1:nrow(sample_occ_abs_data_fortest),1)
  expect_equal(file.exists(paste0(tempdir(),"/",n,"_",test_bandname,"_7_post_mean_.csv")),TRUE)})


test_that("If method combined returns data.frame", {
  results<-extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                  save.method="combined",save.directory=tempdir())
  expect_equal(class(results),"data.frame")})


test_that("If method combined returns data.frame of correct length", {
  results<-extract_dynamic_coords(occ.data=sample_occ_abs_data_fortest,datasetname=test_datasetname,bandname=test_bandname,spatial.res.metres=1000,temporal.res=7,temporal.direction="prior",GEE.math.fun="mean",
                                  save.method="combined",save.directory=tempdir())
  expect_equal(nrow(results),nrow(sample_occ_abs_data_fortest))})


