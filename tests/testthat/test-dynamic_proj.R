
data("sample_model_data")
sample_model_data_test<-dplyr::sample_n(sample_model_data,10)
sample_model_data_train<-dplyr::sample_n(sample_model_data,10)

results1<-readRDS(paste0(testthat::test_path("test-files"),"/testsdm.rds"))

results1_eval<-list(c(0.6,0.7,0.6,0.4,0.5,0.6))

sample_model_data_train_abund<-sample_model_data[sample_model_data$individualCount>0 ,]
sample_model_data_train_abund<-sample_model_data_train_abund[!is.na(sample_model_data_train_abund$individualCount),]
sample_model_data_train_abund$individualCount<-log10(1+sample_model_data_train_abund$individualCount)
sample_model_data_train_abund_test.data<-dplyr::sample_n(sample_model_data_train_abund,415)
sample_model_data_train_abund<-dplyr::sample_n(sample_model_data_train_abund,415)

results3<-readRDS(paste0(testthat::test_path("test-files"),"/testsam.rds"))

results3_eval<-list(c(0.70,0.56,0.60,0.7,0.7,0.90))


test_that("Stops if no models provided", {
  expect_error( dynamic_proj(dates=c("2010-01-01","2010-04-01"),local.directory = testthat::test_path("test-files"),
                                   projection.method=c("binary","proportional","abundance","stacked"),sdm.thresh = 0.5,sdm.weight = as.numeric(unlist(results1_eval)),
                                  sam.weight =as.numeric(unlist(results3_eval)), save.directory=tempdir()))})

test_that("Stops if no dates provided", {
  expect_error( dynamic_proj(local.directory = testthat::test_path("test-files"),
                                   projection.method=c("binary","proportional","abundance","stacked"),sdm.mod =results1,sdm.thresh = 0.5,sdm.weight = as.numeric(unlist(results1_eval)),
                                   sam.mod = results3,sam.weight =as.numeric(unlist(results3_eval)), save.directory=tempdir()))})

test_that("Stops if local.directory doesn't exist", {
  expect_error( dynamic_proj(dates=c("2010-01-01","2010-04-01"),local.directory = "not/real",
                                   projection.method=c("binary","proportional","abundance","stacked"),sdm.mod =results1,sdm.thresh = 0.5,sdm.weight = as.numeric(unlist(results1_eval)),
                                   sam.mod = results3,sam.weight =as.numeric(unlist(results3_eval)), save.directory=tempdir()))})

test_that("Stops if no save.directory or save.folder provided", {
  expect_error( dynamic_proj(dates=c("2010-01-01","2010-04-01"),local.directory = testthat::test_path("test-files"),
                                   projection.method=c("binary"),sdm.mod =results1,sdm.thresh = 0.5,sdm.weight = as.numeric(unlist(results1_eval)),
                                   sam.mod = results3,sam.weight =as.numeric(unlist(results3_eval))))})

test_that("Stops if projection.method doesn't exist", {
  expect_error(dynamic_proj(dates=c("2010-01-01","2010-04-01"),local.directory = testthat::test_path("test-files"),
                                   projection.method=c("fake"),sdm.mod =results1,sdm.thresh = 0.5,sdm.weight = as.numeric(unlist(results1_eval)),
                                   sam.mod = results3,sam.weight =as.numeric(unlist(results3_eval)), save.directory=tempdir()))})

test_that("Stops if threshold not equal to 1 or length of blocks", {
  expect_error( dynamic_proj(dates=c("2010-01-01","2010-04-01"),local.directory = testthat::test_path("test-files"),
                                   projection.method=c("binary","proportional","abundance","stacked"),sdm.mod =results1,sdm.thresh = c(0.5,0.9),sdm.weight = as.numeric(unlist(results1_eval)),
                                   sam.mod = results3,sam.weight =as.numeric(unlist(results3_eval)), save.directory=tempdir()))})

test_that("Stops if SDM weights not equal to 1 or length SDM.MODELS", {
  expect_error( dynamic_proj(dates=c("2010-01-01","2010-04-01"),local.directory = testthat::test_path("test-files"),
                                   projection.method=c("binary","proportional","abundance","stacked"),sdm.mod =results1,sdm.thresh = c(0.5),sdm.weight = c(1,0),
                                   sam.mod = results3,sam.weight =as.numeric(unlist(results3_eval)), save.directory=tempdir()))})

test_that("Stops if SAM weights not equal to 1 or length SAM.MODELS", {
  expect_error( dynamic_proj(dates=c("2010-01-01","2010-04-01"),local.directory = testthat::test_path("test-files"),
                                   projection.method=c("binary","proportional","abundance","stacked"),sdm.mod =results1,sdm.weight = as.numeric(unlist(results1_eval)),
                                   sam.mod = results3,sam.weight =c(1,0), save.directory=tempdir()))})

test_that("Success if projection.method = all", {
  save.directory=tempdir()
  dates=c("2010-01-01","2010-04-01")
  filenames<-paste0(dates,"_stacked.tif")
  dynamic_proj(dates=c("2010-01-01","2010-04-01"),local.directory = testthat::test_path("test-files"),
                     projection.method=c("binary","proportional","abundance","stacked"),sdm.mod =results1,sdm.thresh = 0.5,sdm.weight = as.numeric(unlist(results1_eval)),
                     sam.mod = results3,sam.weight =as.numeric(unlist(results3_eval)), save.directory=tempdir())
  expect_equal(file.exists(paste0(save.directory,"/",filenames[2])),TRUE)})




test_that("Success if projection.method = all + Google Drive used", {
  skip_if_no_GEE_credentials()
  user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
  dates=c("2010-01-01","2010-04-01")
  save.directory=tempdir()
  filenames<-paste0(dates,"_stacked.tif")
  dynamic_proj(dates=c("2010-01-01","2010-04-01"),drive.folder = "testfiles",user.email = user.email,
               projection.method=c("binary","proportional","abundance","stacked"),sdm.mod =results1,sdm.thresh = 0.5,sdm.weight = as.numeric(unlist(results1_eval)),
               sam.mod = results3,sam.weight =as.numeric(unlist(results3_eval)), save.drive.folder = "temporarysavedrivefolder", save.directory=tempdir())
  expect_equal(file.exists(paste0(save.directory,"/",filenames[2])),TRUE)})

