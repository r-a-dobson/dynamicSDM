data("sample_occ_data")

test_that("stops if no occcurence data provided and method buffer chosen", {
  expect_error(spatiotemp_pseudoabs(n.pseudoabs=1000,spatial.method="buffer",temporal.method="random",spatial.buffer=c(100,1000),temporal.ext=c('2020-6-16','2022-6-16')))})

test_that("stops if no spatial.method provided", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=1000,temporal.method="random",spatial.ext=c(0, 20, 0, 20),temporal.ext=c('2020-6-16','2022-6-16')))})

test_that("stops if no temporal.method provided", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=1000,spatial.method="random",spatial.ext=c(0, 20, 0, 20),temporal.ext=c('2020-6-16','2022-6-16')))})

test_that("stops if n.pseudoabs is not numeric", {
   expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs="onethousands",temporal.method="random",spatial.method="random",spatial.ext=c(0, 20, 0, 20),temporal.ext=c('2020-6-16','2022-6-16')))})

test_that("stops if temporal.method specified not accepted", {
   expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="static",spatial.method="random",spatial.ext=c(0, 20, 0, 20),temporal.ext=c('2020-6-16','2022-6-16')))})

test_that("stops if spatial.method specified not accepted", {
   expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="random",spatial.method="static",spatial.ext=c(0, 20, 0, 20),temporal.ext=c('2020-6-16','2022-6-16')))})

test_that("stops if temporal.method buffer but no buffer given", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="buffer",spatial.method="random",spatial.ext=c(0, 20, 0, 20)))})

test_that("stops if temporal.method buffer but incorrect class", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="buffer",spatial.method="random",spatial.ext=c(0, 20, 0, 20),temporal.buffer="five days"))})

test_that("stops if temporal.method buffer but buffer incorrect length", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="buffer",spatial.method="random",spatial.ext=c(0, 20, 0, 20),temporal.buffer=c(5)))})

test_that("stops if temporal.method buffer but buffer first further away than second", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="buffer",spatial.method="random",spatial.ext=c(0, 20, 0, 20),temporal.buffer=c(30,5)))})

test_that("stops if temporal.method random but no temporal.ext given", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="random",spatial.method="random",spatial.ext=c(0, 20, 0, 20)))})

test_that("stops if temporal.method random but temporal.ext given wrong class", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="random",spatial.method="random",spatial.ext=c(0, 20, 0, 20),temporal.ext=4))})

test_that("stops if temporal.method random but two dates not given in temporal.ext", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="random",spatial.method="random",spatial.ext=c(0, 20, 0, 20),temporal.ext="2000-04-05"))})

test_that("stops if temporal.method random and temporal.ext contains invalid date", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="random",spatial.method="random",spatial.ext=c(0, 20, 0, 20),temporal.ext=c("2004-15-01","2000-04-05")))})

test_that("stops if temporal.method random and temporal.ext not in date order", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="random",spatial.method="random",spatial.ext=c(0, 20, 0, 20),temporal.ext=c("2004-01-01","2000-04-05")))})

test_that("stops if spatial.method buffer but no buffer given", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="random",spatial.method="buffer",temporal.ext=c("2000-12-01","2004-04-05")))})

test_that("stops if spatial.method buffer but incorrect class", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="random",spatial.method="buffer",spatial.buffer =c("300m") , temporal.ext=c("2000-12-01","2004-04-05")))})

test_that("stops if spatial.method buffer but buffer incorrect length", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="random",spatial.method="buffer",spatial.buffer=300,temporal.ext=c("2000-12-01","2004-04-05")))})

test_that("stops if spatial.method buffer but buffer first further away than second", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="random",spatial.method="buffer",spatial.buffer=c(300,10),temporal.ext=c("2000-12-01","2004-04-05")))})

test_that("stops if spatial.method random but no spatial.ext given", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=100,temporal.method="random",spatial.method="random",temporal.ext=c("2000-12-01","2004-04-05")))})

test_that("stops if temporal.method random but temporal.ext given wrong class", {
  expect_error(spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=10,temporal.method="random",spatial.method="random",spatial.ext="faraway",temporal.ext=c("2000-12-01","2004-04-05")))})

test_that("Success if temporal buffer and spatial buffer method", {
results<-spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=10,temporal.method="buffer",spatial.method="buffer",spatial.buffer=c(100,1000),temporal.buffer=c(14,30))
 expect_equal(class(results),"data.frame")})

test_that("Success if temporal buffer and spatial buffer method", {
  results<-spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=10,temporal.method="buffer",spatial.method="buffer",spatial.buffer=c(100,1000),temporal.buffer=c(14,30))
  expect_equal(nrow(results),10)})

test_that("Success if temporal buffer and spatial buffer method, and n.pseudoabs default", {
  results<-spatiotemp_pseudoabs(occ.data=sample_occ_data,temporal.method="buffer",spatial.method="buffer",spatial.buffer=c(100,1000),temporal.buffer=c(14,30))
  expect_equal(nrow(results),100)})

test_that("Success if spatial extent numeric", {
  numeric<-c(20,36,-35,-12)
  results<-spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=10,temporal.method="buffer",spatial.method="random",spatial.ext=numeric,temporal.buffer=c(14,30))
  expect_equal(class(results),"data.frame")})

test_that("Success if spatial extent extent", {
  Extent<-raster::extent(c(20,36,-35,-12))
  results<-spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=10,temporal.method="buffer",spatial.method="random",spatial.ext=Extent,temporal.buffer=c(14,30))
  expect_equal(class(results),"data.frame")})

test_that("Success if spatial extent raster", {
  raster<-raster::raster(raster::extent(c(20,36,-35,-12)))
  results<-spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=10,temporal.method="buffer",spatial.method="random",spatial.ext=raster,temporal.buffer=c(14,30))
  expect_equal(class(results),"data.frame")})

test_that("Success if spatial extent polygon", {
  polygon<-sp::Polygon(cbind(c(20,12,36,36),c(-35,-12,-35,-12)))
  results<-spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=10,temporal.method="buffer",spatial.method="random",spatial.ext=polygon,temporal.buffer=c(14,30))
  expect_equal(class(results),"data.frame")})

test_that("Success if temporal method random and spatial buffer", {
  results<-spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=10,temporal.method="random",spatial.method="buffer",spatial.buffer = c(100,1000),temporal.ext=c("2005-01-01","2020-01-01"))
  expect_equal(class(results),"data.frame")})

test_that("Success if temporal method random and spatial random, n.pseudoabs default", {
  results<-spatiotemp_pseudoabs(occ.data=sample_occ_data,n.pseudoabs=10,temporal.method="random",spatial.method="buffer",spatial.buffer = c(100,1000),temporal.ext=c("2005-01-01","2020-01-01"))
  expect_equal(class(results),"data.frame")})






