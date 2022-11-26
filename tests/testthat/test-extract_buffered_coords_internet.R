
data("sample_occ_abs_data")
sample_occ_abs_data_fortest<-sample_occ_abs_data[sample_occ_abs_data$year==2010,]
sample_occ_abs_data_fortest<-dplyr::sample_n(sample_occ_abs_data_fortest,3)
test_bandname<-"LST_Day_1km"
test_datasetname<-"MODIS/006/MOD11A1"
test.spatial.res.metres<-111320
user.email<-as.character(gargle::gargle_oauth_sitrep()$email)


test_that("stops if no occcurence data provided", {
  expect_error(extract_buffered_coords(
                            moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                            drive.folder="temporary_folder_buffered_extraction",
                            user.email=user.email,
                            datasetname=test_datasetname,
                            bandname=test_bandname,
                            spatial.res.metres=test.spatial.res.metres,
                            temporal.level="year",
                            temporal.res=7,
                            temporal.direction="prior",
                            GEE.math.fun="mean",
                            save.method="split",
                            save.directory=tempdir()))})


test_that("stops if no moving.window.matrix provided", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       datasetname=test_datasetname,
                                       drive.folder="temporary_folder_buffered_extraction",
                                       user.email=user.email,
                                       bandname=test_bandname,
                                       spatial.res.metres=test.spatial.res.metres,
                                       temporal.level="year",
                                       temporal.res=7,
                                       temporal.direction="prior",
                                       GEE.math.fun="mean",
                                       save.method="split",
                                       save.directory=tempdir()))})


test_that("stops if no datasetname provided", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       drive.folder="temporary_folder_buffered_extraction",
                                       user.email=user.email,
                                        bandname=test_bandname,
                                       spatial.res.metres=test.spatial.res.metres,
                                       temporal.level="year",
                                       temporal.res=7,
                                       temporal.direction="prior",
                                       GEE.math.fun="mean",
                                       save.method="split",
                                       save.directory=tempdir()))})



test_that("stops if no drive folder provided", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                      user.email=user.email,
                                      bandname=test_bandname,
                                       datasetname=test_datasetname,
                                       spatial.res.metres=test.spatial.res.metres,
                                       temporal.level="year",
                                       temporal.res=7,
                                       temporal.direction="prior",
                                       GEE.math.fun="mean",
                                       save.method="split",
                                       save.directory=tempdir()))})


test_that("stops if no bandname provided", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       drive.folder="temporary_folder_buffered_extraction",
                                       user.email=user.email,
                                       datasetname=test_datasetname,
                                       spatial.res.metres=test.spatial.res.metres,
                                       temporal.level="year",
                                       temporal.res=7,
                                       temporal.direction="prior",
                                       GEE.math.fun="mean",
                                       save.method="split",
                                       save.directory=tempdir()))})

test_that("stops if no spatial.res provided", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       drive.folder="temporary_folder_buffered_extraction",
                                       user.email=user.email,
                                       datasetname=test_datasetname,
                                       bandname=test_bandname,
                                       temporal.level="year",
                                       temporal.res=7,
                                       temporal.direction="prior",
                                       GEE.math.fun="mean",
                                       save.method="split",
                                       save.directory=tempdir()))})

test_that("stops if no temporal.direction provided", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       drive.folder="temporary_folder_buffered_extraction",
                                       user.email=user.email,
                                       datasetname=test_datasetname,
                                       bandname=test_bandname,
                                       spatial.res.metres=test.spatial.res.metres,
                                       temporal.level="year",
                                       temporal.res=7,
                                       GEE.math.fun="mean",
                                       save.method="split",
                                       save.directory=tempdir()))})

test_that("stops if no math.fun provided", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       drive.folder="temporary_folder_buffered_extraction",
                                       user.email=user.email,
                                       datasetname=test_datasetname,
                                       bandname=test_bandname,
                                       spatial.res.metres=test.spatial.res.metres,
                                       temporal.level="year",
                                       temporal.res=7,
                                       temporal.direction="prior",
                                       save.method="split",
                                       save.directory=tempdir()))})


test_that("stops if no save.method provided", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       drive.folder="temporary_folder_buffered_extraction",
                                       user.email=user.email,
                                       datasetname=test_datasetname,
                                       bandname=test_bandname,
                                       spatial.res.metres=test.spatial.res.metres,
                                       temporal.level="year",
                                       temporal.res=7,
                                       temporal.direction="prior",
                                       GEE.math.fun="mean",
                                       save.directory=tempdir()))})


test_that("stops if no save.method not accepted", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       drive.folder="temporary_folder_buffered_extraction",
                                       user.email=user.email,
                                       datasetname=test_datasetname,
                                       bandname=test_bandname,
                                       spatial.res.metres=test.spatial.res.metres,
                                       temporal.level="year",
                                       temporal.res=7,
                                       save.method="NA",
                                       temporal.direction="prior",
                                       GEE.math.fun="mean",
                                       save.directory=tempdir()))})

test_that("stops if no save.directory provided", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       drive.folder="temporary_folder_buffered_extraction",
                                       user.email=user.email,
                                       datasetname=test_datasetname,
                                       bandname=test_bandname,
                                       spatial.res.metres=test.spatial.res.metres,
                                       temporal.level="year",
                                       temporal.res=7,
                                       temporal.direction="prior",
                                       GEE.math.fun="mean",
                                       save.method="split"))})


test_that("stops if more than one math.fun provided", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       drive.folder="temporary_folder_buffered_extraction",
                                       user.email=user.email,
                                       datasetname=test_datasetname,
                                       bandname=test_bandname,
                                       spatial.res.metres=test.spatial.res.metres,
                                       temporal.level="year",
                                       temporal.res=7,
                                       temporal.direction="prior",
                                       GEE.math.fun=c("mean","sum"),
                                       save.method="split",
                                       save.directory=tempdir()))})


test_that("stops if save.directory doesn't exist", {
  expect_error(extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                       moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                       extraction.drive.folder="temporary_folder_buffered_extraction",
                                       user.email=user.email,
                                       datasetname=test_datasetname,
                                       bandname=test_bandname,
                                       spatial.res.metres=test.spatial.res.metres,
                                       temporal.level="year",
                                       temporal.res=7,
                                       temporal.direction="prior",
                                       GEE.math.fun="mean",
                                       save.method="split",
                                       save.directory="notrealdirectory"))})


test_that("Works if temporal.level = year with prior", {
results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                        moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                        extraction.drive.folder="temporary_folder_buffered_extraction",
                        user.email=user.email,
                        datasetname=test_datasetname,
                        bandname=test_bandname,
                        spatial.res.metres=test.spatial.res.metres,
                        temporal.level="year",
                        temporal.res=7,
                        temporal.direction="prior",
                        GEE.math.fun="mean",
                        save.method="split",
                        save.directory=tempdir())

expect_equal(length(unique(sample_occ_abs_data_fortest$year)),length(results))})



test_that("Works if temporal.level = month with prior", {
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="month",
                                   temporal.res=7,
                                   temporal.direction="prior",
                                   GEE.math.fun="mean",
                                   save.method="split",
                                   save.directory=tempdir())

  expect_equal(nrow(unique(sample_occ_abs_data_fortest[,c("year","month")])),length(results))})





test_that("Works if temporal.level = day with prior", {
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="day",
                                   temporal.res=7,
                                   temporal.direction="prior",
                                   GEE.math.fun="mean",
                                   save.method="split",
                                   save.directory=tempdir())
  expect_equal(nrow(unique(sample_occ_abs_data_fortest[,c("year","month","day")])),length(results))})




test_that("Works if temporal.level = year  with prior", {
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="year",
                                   temporal.res=7,
                                   temporal.direction="prior",
                                   GEE.math.fun="mean",
                                   save.method="split",
                                   save.directory=tempdir())
  year<-unique(as.numeric(sample_occ_abs_data_fortest[1,"year"]))
  n<-year[sample(1:length(year),1)]
  expect_equal(file.exists(paste0(tempdir(),"/",n,"_",test_bandname,"_7_prior_mean_buffered_.csv")),TRUE)})



test_that("Works if temporal.level = month  with prior", {
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="month",
                                   temporal.res=7,
                                   temporal.direction="prior",
                                   GEE.math.fun="mean",
                                   save.method="split",
                                   save.directory=tempdir())
  year<-as.numeric(sample_occ_abs_data_fortest[1,"year"])
  month<-sprintf("%02d", as.numeric(sample_occ_abs_data_fortest[1,"month"]))
  day<-sprintf("%02d", 1) # Use full dates from each individual occurrence record as each will have different data.
  nameofsplitfile<-paste0(year,"-",month)
  expect_equal(file.exists(paste0(tempdir(),"/",nameofsplitfile,"_",test_bandname,"_7_prior_mean_buffered_.csv")),TRUE)})




test_that("Works if temporal.level = day with prior", {
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="day",
                                   temporal.res=7,
                                   temporal.direction="prior",
                                   GEE.math.fun="mean",
                                   save.method="split",
                                   save.directory=tempdir())
  year<-as.numeric(sample_occ_abs_data_fortest[1,"year"])
  month<-sprintf("%02d", as.numeric(sample_occ_abs_data_fortest[1,"month"]))
  day<-sprintf("%02d",  as.numeric(sample_occ_abs_data_fortest[1,"day"])) # Use full dates from each individual occurrence record as each will have different data.
  nameofsplitfile<-paste0(year,"-",month,"-",day)
  expect_equal(file.exists(paste0(tempdir(),"/",nameofsplitfile,"_",test_bandname,"_7_prior_mean_buffered_.csv")),TRUE)})







test_that("Works if temporal.level = year with post", {
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="year",
                                   temporal.res=7,
                                   temporal.direction="post",
                                   GEE.math.fun="mean",
                                   save.method="split",
                                   save.directory=tempdir())
  year<-unique(as.numeric(sample_occ_abs_data_fortest[1,"year"]))
  n<-year[sample(1:length(year),1)]
  expect_equal(file.exists(paste0(tempdir(),"/",n,"_",test_bandname,"_7_post_mean_buffered_.csv")),TRUE)})


test_that("Works if temporal.level = month with post", {
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="month",
                                   temporal.res=7,
                                   temporal.direction="post",
                                   GEE.math.fun="mean",
                                   save.method="split",
                                   save.directory=tempdir())
  year<-as.numeric(sample_occ_abs_data_fortest[1,"year"])
  month<-sprintf("%02d", as.numeric(sample_occ_abs_data_fortest[1,"month"]))
  nameofsplitfile<-paste0(year,"-",month)
    expect_equal(file.exists(paste0(tempdir(),"/",nameofsplitfile,"_",test_bandname,"_7_post_mean_buffered_.csv")),TRUE)})




test_that("Works if temporal.level = day with post", {
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="day",
                                   temporal.res=7,
                                   temporal.direction="post",
                                   GEE.math.fun="mean",
                                   save.method="split",
                                   save.directory=tempdir())

  year<-as.numeric(sample_occ_abs_data_fortest[1,"year"])
  month<-sprintf("%02d", as.numeric(sample_occ_abs_data_fortest[1,"month"]))
  day<-sprintf("%02d", as.numeric(sample_occ_abs_data_fortest[1,"day"])) # Use full dates from each individual occurrence record as each will have different data.
  nameofsplitfile<-paste0(year,"-",month,"-",day)
  expect_equal(file.exists(paste0(tempdir(),"/",nameofsplitfile,"_",test_bandname,"_7_post_mean_buffered_.csv")),TRUE)})


test_that("Works if temporal.level = year with combined", {
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="year",
                                   temporal.res=7,
                                   temporal.direction="post",
                                   GEE.math.fun="mean",
                                   save.method="combined",
                                   save.directory=tempdir())

  expect_equal(file.exists(paste0(tempdir(),"/all_records_combined_",test_bandname,"_7_post_mean_buffered_.csv")),TRUE)})



test_that("Works if temporal.level = month with combined", {
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="month",
                                   temporal.res=7,
                                   temporal.direction="post",
                                   GEE.math.fun="mean",
                                   save.method="combined",
                                   save.directory=tempdir())


  expect_equal(file.exists(paste0(tempdir(),"/all_records_combined_",test_bandname,"_7_post_mean_buffered_.csv")),TRUE)})


test_that("Works if temporal.level = day with combined", {
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="day",
                                   temporal.res=7,
                                   temporal.direction="post",
                                   GEE.math.fun="mean",
                                   save.method="combined",
                                   save.directory=tempdir())

  expect_equal(file.exists(paste0(tempdir(),"/all_records_combined_",test_bandname,"_7_post_mean_buffered_.csv")),TRUE)})



test_datasetname<-"MODIS/006/MCD12Q1"
test_bandname<-"LC_Type5"
test.spatial.res.metres<-500
test.categories<-c(6,7)


test_that("Works if categories ", {
  skip_if_offline()
  results<-extract_buffered_coords(occ.data=sample_occ_abs_data_fortest,
                                   moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
                                   extraction.drive.folder="temporary_folder_buffered_extraction",
                                   user.email=user.email,
                                   datasetname=test_datasetname,
                                   bandname=test_bandname,
                                   spatial.res.metres=test.spatial.res.metres,
                                   temporal.level="year",
                                   GEE.math.fun="sum",
                                   categories = test.categories,
                                   save.method="combined",
                                   save.directory=tempdir())
  expect_equal(file.exists(paste0(tempdir(),"/all_records_combined_",test_bandname,"_sum_buffered_.csv")),TRUE)})
