
data("sample_occ_abs_data")
data("sample_surveyeffort")
sample_occ_abs_data_fortest<-dplyr::sample_n(sample_occ_abs_data,10)

test_that("stops if no sampling effort data provided", {
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,spatial.dist = 10000,temporal.dist = 20))})

test_that("identifies missing year col in survey effort df", {expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = subset(sample_surveyeffort, select = -c(year),spatial.dist = 10000,temporal.dist = 20)))})
test_that("identifies missing month col in survey effort df", {expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = subset(sample_surveyeffort, select = -c(month),spatial.dist = 10000,temporal.dist = 20)))})
test_that("identifies missing day col in survey effort df", {expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = subset(sample_surveyeffort, select = -c(day),spatial.dist = 10000,temporal.dist = 20)))})
test_that("identifies missing x col in survey effort df", {expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = subset(sample_surveyeffort, select = -c(x),spatial.dist = 10000,temporal.dist = 20)))})
test_that("identifies missing y col in survey effort df", {expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = subset(sample_surveyeffort, select = -c(y),spatial.dist = 10000,temporal.dist = 20)))})

test_that("identifies wrong year class in survey effort df", {
  wrong.class<-sample_surveyeffort
  wrong.class$year<-as.character(wrong.class$year)
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = wrong.class,spatial.dist = 10000,temporal.dist = 20))})

test_that("identifies wrong month class in survey effort df", {
  wrong.class<-sample_surveyeffort
  wrong.class$month<-as.character(wrong.class$month)
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = wrong.class,spatial.dist = 10000,temporal.dist = 20))})

test_that("identifies wrong day class in survey effort df", {
  wrong.class<-sample_surveyeffort
  wrong.class$day<-as.character(wrong.class$day)
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = wrong.class,spatial.dist = 10000,temporal.dist = 20))})

test_that("identifies wrong x class in survey effort df", {
  wrong.class<-sample_surveyeffort
  wrong.class$x<-as.character(wrong.class$x)
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = wrong.class,spatial.dist = 10000,temporal.dist = 20))})

test_that("identifies wrong y class in survey effort df", {
  wrong.class<-sample_surveyeffort
  wrong.class$x<-as.character(wrong.class$x)
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = wrong.class,spatial.dist = 10000,temporal.dist = 20))})

test_that("stops if no spatial.dist provided", {
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = sample_surveyeffort,temporal.dist = 20))})

test_that("stops if no temporal.dist provided", {
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = sample_surveyeffort, spatial.dist = 10000))})

test_that("stops if spatial.dist not class numeric", {
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = sample_surveyeffort,spatial.dist="300metres",temporal.dist = 20))})

test_that("stops if temporal.dist not class numeric", {
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = sample_surveyeffort, spatial.dist = 10000,temporal.dist="twenty"))})

test_that("stops if spatial.dist not length(1) numeric", {
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = sample_surveyeffort,spatial.dist=c(200,300,400),temporal.dist = 20))})

test_that("stops if temporal.dist not length(1) numeric", {
  expect_error(spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = sample_surveyeffort, spatial.dist = 10000,temporal.dist=c(20,30,40)))})

test_that("Output is class data.frame", {
results<-spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = sample_surveyeffort,spatial.dist = 10000,temporal.dist = 20)
expect_equal(class(results),"data.frame")})

test_that("Output is same nrows as input data.frame", {
  results<-spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = sample_surveyeffort,spatial.dist = 10000,temporal.dist = 20)
  expect_equal(nrow(results),nrow(sample_occ_abs_data_fortest))})

test_that("Output is adds two rows with sampling effort", {
  results<-spatiotemp_weights(occ.data = sample_occ_abs_data_fortest,sampling.events.df = sample_surveyeffort,spatial.dist = 10000,temporal.dist = 20)
  expect_equal(ncol(results),ncol(sample_occ_abs_data_fortest)+2)})







