data("sample_model_data")

sample_model_data2<-dplyr::sample_n(sample_model_data,100)
rm("sample_model_data")

test_that("stops if no occ.data provided", {
  expect_error(spatiotemp_autocorr(varname="Temperaturemean",temporal.level="DOY"))})

test_that("stops if method not accepted", {
  expect_error(spatiotemp_autocorr(occ.data=sample_model_data2,varname="Temperaturemean",temporal.level="decadal"))})

test_that("day method output list object", {
  results<-spatiotemp_autocorr(occ.data=sample_model_data2,varname="Temperaturemean",temporal.level="day")
  expect_equal(class(results),"list")})

test_that("day temporal.level outputs length(2) in each list", {
  results<-spatiotemp_autocorr(occ.data=sample_model_data2,varname="Temperaturemean",temporal.level="day")
  expect_equal(length(results[[1]]),2)})

test_that("year temporal.level works with multiple variables", {
  varnames<-c("Temperaturemean","Precipitationsum")
  results<-spatiotemp_autocorr(occ.data=sample_model_data2,varname=varnames,temporal.level="year")
  expect_equal(length(results),length(varnames))})

test_that("month temporal.level works with multiple variables", {
  varnames<-c("Temperaturemean","Precipitationsum")
  results<-spatiotemp_autocorr(occ.data=sample_model_data2,varname=varnames,temporal.level="month")
    expect_equal(length(results),length(varnames))})

