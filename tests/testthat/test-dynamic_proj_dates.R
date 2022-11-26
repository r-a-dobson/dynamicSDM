
test_that("Stops if startdate missing", {
  expect_error(dynamic_proj_dates(enddate="2006-05-01",interval.level="month", interval=2))})

test_that("Stops if enddate missing", {
  expect_error(dynamic_proj_dates(startdate="2002-01-01",interval.level="month", interval=2))})

test_that("Stops if interval.level missing", {
  expect_error(dynamic_proj_dates(startdate="2002-01-01",enddate="2006-05-01",interval=2))})

test_that("Stops if interval missing", {
  expect_error(dynamic_proj_dates(startdate="2002-01-01",enddate="2006-05-01",interval.level="month"))})

test_that("Stops if interval.level not accepted", {
  expect_error(dynamic_proj_dates(startdate="2002-01-01",enddate="2006-05-01",interval.level="seconds",interval=2))})


test_that("Stops if start date not class character", {
  expect_error(dynamic_proj_dates(startdate=2002-01-01,enddate="2006-05-01",interval.level="month",interval=2))})


test_that("Stops if start date not class character", {
  expect_error(dynamic_proj_dates(startdate="2002-01-01",enddate=2006-05-01,interval.level="month",interval=2))})

test_that("Stops if invalid startdate", {
  expect_error(dynamic_proj_dates(startdate="2002-19-01",enddate="2006-05-01",interval.level="month",interval=2))})

test_that("Stops if invalid enddate", {
  expect_error(dynamic_proj_dates(startdate="2002-01-01",enddate="2006-13-41",interval.level="month",interval=2))})

test_that("Stops if enddate before startdate", {
  expect_error(dynamic_proj_dates(startdate="2002-01-01",enddate="2000-01-01",interval.level="month",interval=2))})


test_that("Works with interval.level month", {
  results<-dynamic_proj_dates(startdate="2002-01-01",enddate="2002-12-31",interval.level="month",interval=1)
  expect_equal(class(results),"Date")})

test_that("Works with interval.level month", {
  results<-dynamic_proj_dates(startdate="2002-01-01",enddate="2002-12-31",interval.level="month",interval=1)
  expect_equal(length(results),12)})


test_that("Works with interval.level day", {
  results<-dynamic_proj_dates(startdate="2002-01-01",enddate="2002-12-31",interval.level="day",interval=1)
  expect_equal(class(results),"Date")})

test_that("Works with interval.level day", {
  results<-dynamic_proj_dates(startdate="2002-01-01",enddate="2002-12-31",interval.level="day",interval=1)
  expect_equal(length(results),365)})


test_that("Works with interval.level week", {
  results<-dynamic_proj_dates(startdate="2002-01-01",enddate="2002-12-31",interval.level="week",interval=1)
  expect_equal(class(results),"Date")})

test_that("Works with interval.level week", {
  results<-dynamic_proj_dates(startdate="2002-01-01",enddate="2002-12-31",interval.level="week",interval=1)
  expect_equal(length(results),53)})

test_that("Works with interval.level year", {
  results<-dynamic_proj_dates(startdate="2002-01-01",enddate="2003-01-01",interval.level="year",interval=1)
  expect_equal(class(results),"Date")})

test_that("Works with interval.level year", {
  results<-dynamic_proj_dates(startdate="2002-01-01",enddate="2003-01-01",interval.level="year",interval=1)
  expect_equal(length(results),2)})

