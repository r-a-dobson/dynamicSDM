
data("sample_occ_data")
sample_occ_data<-rbind(sample_occ_data,sample_occ_data,sample_occ_data) #Needs bigger sample size for test statistics

test_that("stops if no occcurence data provided", {
  expect_error(spatiotemp_bias(temporal.level="month"))})

test_that("stops if no temporal.level provided", {
  expect_error(spatiotemp_bias(occ.data=sample_occ_data))})

test_that("stops if provided temporal temporal.level is incorrect class", {
  expect_error(spatiotemp_bias(occ.data=sample_occ_data, temporal.level="decadal"))})

test_that("Result of length(2) ", {
  result<-spatiotemp_bias(occ.data=sample_occ_data, temporal.level="day")
  expect_equal(length(result),2)})



