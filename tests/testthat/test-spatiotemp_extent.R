
data("sample_explan_data")

test_that("stops if no occcurence data provided", {
  expect_error(spatiotemp_extent())})

test_that("stop if temporal.ext wrong class", {
  expect_error(spatiotemp_extent(occ.data=sample_explan_data,temporal.ext=1234))})

test_that("stop if temporal.ext length not equal 2", {
  expect_error(spatiotemp_extent(occ.data=sample_explan_data,temporal.ext=c("2011-01-02")))})

test_that("stop if temporal.ext contains one invalid date", {
  expect_error(spatiotemp_extent(occ.data=sample_explan_data,temporal.ext=c("2011-01-02","2011-14-02")))})

test_that("stop if temporal.ext contains two invalid date", {
  expect_error(spatiotemp_extent(occ.data=sample_explan_data,temporal.ext=c("2011-91-02","2011-14-02")))})

test_that("stop if temporal.ext not two sequential dates", {
  expect_error(spatiotemp_extent(occ.data=sample_explan_data,temporal.ext=c("2011-01-02","2010-01-02")))})

test_that("Message if occurrence data contains invalid dates", {
  testdata<-sample_explan_data
  testdata[4,"day"]<-56
  expect_warning(spatiotemp_extent(occ.data=testdata,temporal.ext=c("2001-01-02","2020-02-02")))})

test_that("Filters by temporal.ext: all within dates",{
  results<-spatiotemp_extent(occ.data=sample_explan_data,temporal.ext = c("2000-01-01","2020-01-01"))
  expect_equal(nrow(results),nrow(sample_explan_data))})

test_that("Filters by temporal.ext: some not within dates",{
  results<-spatiotemp_extent(occ.data=sample_explan_data,temporal.ext = c("2013-01-01","2020-01-01"))
  expect_equal(nrow(results),148)})

data(sample_extent_data)

test_that("Filters by spatial.ext but all within extent, numeric ",{
  numeric<-c(min(sample_explan_data$x),max(sample_explan_data$x),min(sample_explan_data$y),max(sample_explan_data$y))
  results<-spatiotemp_extent(occ.data=sample_explan_data,spatial.ext = numeric)
  expect_equal(nrow(results),327)})

test_that("Filters by spatial.ext but all within extent, Extent ",{
  Extent<-raster::extent(c(min(sample_explan_data$x),max(sample_explan_data$x),min(sample_explan_data$y),max(sample_explan_data$y)))
  results<-spatiotemp_extent(occ.data=sample_explan_data,spatial.ext = Extent)
  expect_equal(nrow(results),327)})

test_that("Filters by spatial.ext but all within extent, raster ",{
  numeric<-c(min(sample_explan_data$x),max(sample_explan_data$x),min(sample_explan_data$y),max(sample_explan_data$y))
  raster<-raster::raster(raster::extent(numeric))
  results<-spatiotemp_extent(occ.data=sample_explan_data,spatial.ext = raster)
  expect_equal(nrow(results),327)})

test_that("Filters by spatial.ext polygon ",{
  polygon<-sp::Polygon(cbind(c(12,12,36,36),c(-35,-12,-35,-12)))
  results<-spatiotemp_extent(occ.data=sample_explan_data,spatial.ext = polygon)
  expect_equal(nrow(results),327)})

test_that("Filters by spatial.ext but not all within extent, numeric ",{
  numeric<-c(24,36,-35,-12)
  results<-spatiotemp_extent(occ.data=sample_explan_data,spatial.ext = numeric)
  expect_equal(nrow(results),280)})

test_that("Filters by spatial.ext but not all within extent, Extent ",{
  Extent<-raster::extent(c(24,36,-35,-12))
  results<-spatiotemp_extent(occ.data=sample_explan_data,spatial.ext = Extent)
  expect_equal(nrow(results),280)})

test_that("Filters by spatial.ext but not all within extent, raster ",{
  numeric<-c(24,36,-35,-12)
  raster<-raster::raster(raster::extent(numeric))
  raster<-raster::setValues(raster,values=1:raster::ncell(raster))
  results<-spatiotemp_extent(occ.data=sample_explan_data,spatial.ext = raster)
  expect_equal(nrow(results),280)})

test_that("Filters by spatial.ext but all within extent, sf ",{
  results<-spatiotemp_extent(occ.data=sample_explan_data,spatial.ext = sample_extent_data)
  expect_equal(nrow(results),328)})



