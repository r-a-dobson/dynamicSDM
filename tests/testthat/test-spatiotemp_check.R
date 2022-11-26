
data(sample_occ_data)

test_that("stops if no occcurence data provided", {
  expect_error(spatiotemp_check(na.handle="exclude",duplicate.handle="exclude"))})


test_that("stops if no occcurence data not a data frame", {
  expect_error(spatiotemp_check(occ.data="dataframe",na.handle="exclude",duplicate.handle="exclude"))})


test_that("duplicates removed", {
  test.data<-rbind(sample_occ_data,sample_occ_data)
  checked<-spatiotemp_check(occ.data=test.data,duplicate.handle="exclude")
  expect_equal(nrow(checked), 50)})

test_that("NAs removed", {
  test.data<-sample_occ_data
  test.data[1,"x"]<-NA
  checked<-spatiotemp_check(occ.data=test.data,na.handle="exclude")
  expect_equal(nrow(checked), 49)})

test_that("identifies missing year col", {expect_error(spatiotemp_check(occ.data=subset(sample_occ_data, select = -c(year))))})
test_that("identifies missing month col", {expect_error(spatiotemp_check(occ.data=subset(sample_occ_data, select = -c(month))))})
test_that("identifies missing day col", {expect_error(spatiotemp_check(occ.data=subset(sample_occ_data, select = -c(day))))})
test_that("identifies missing x col", {expect_error(spatiotemp_check(occ.data=subset(sample_occ_data, select = -c(x))))})
test_that("identifies missing y col", {expect_error(spatiotemp_check(occ.data=subset(sample_occ_data, select = -c(y))))})

test_that("identifies wrong year class", {
wrong.class<-sample_occ_data
wrong.class$year<-as.character(wrong.class$year)
expect_error(spatiotemp_check(occ.data=wrong.class))})

test_that("identifies wrong month class", {
  wrong.class<-sample_occ_data
  wrong.class$month<-as.character(wrong.class$month)
  expect_error(spatiotemp_check(occ.data=wrong.class))})

test_that("identifies wrong day class", {
  wrong.class<-sample_occ_data
  wrong.class$day<-as.character(wrong.class$day)
  expect_error(spatiotemp_check(occ.data=wrong.class))})

test_that("identifies wrong x class", {
  wrong.class<-sample_occ_data
  wrong.class$x<-as.character(wrong.class$x)
  expect_error(spatiotemp_check(occ.data=wrong.class))})

test_that("identifies wrong y class", {
  wrong.class<-sample_occ_data
  wrong.class$x<-as.character(wrong.class$x)
  expect_error(spatiotemp_check(occ.data=wrong.class))})




test_that("excludes invalid date", {
  wrong.class<-sample_occ_data
  wrong.class[1,"day"]<-90
  expect_equal(nrow(spatiotemp_check(occ.data=wrong.class,date.handle="exclude")),49)})

test_that("ignores invalid date", {
  wrong.class<-sample_occ_data
  wrong.class[1,"day"]<-90
  expect_equal(nrow(spatiotemp_check(occ.data=wrong.class,date.handle="ignore")),50)})



test_that("excludes invalid x", {
  wrong.class<-sample_occ_data
  wrong.class[1,"x"]<-90000
  expect_equal(nrow(spatiotemp_check(occ.data=wrong.class,coord.handle="exclude")),49)})

test_that("ignores invalid x", {
  wrong.class<-sample_occ_data
  wrong.class[1,"x"]<-90000
  expect_equal(nrow(spatiotemp_check(occ.data=wrong.class,coord.handle="ignore")),50)})


test_that("excludes invalid y", {
  wrong.class<-sample_occ_data
  wrong.class[1,"y"]<-90000
  expect_equal(nrow(spatiotemp_check(occ.data=wrong.class,coord.handle="exclude")),49)})

test_that("ignores invalid y", {
  wrong.class<-sample_occ_data
  wrong.class[1,"y"]<-90000
  expect_equal(nrow(spatiotemp_check(occ.data=wrong.class,coord.handle="ignore")),50)})
