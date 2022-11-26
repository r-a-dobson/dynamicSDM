data("sample_model_data")
sample_model_data<-dplyr::sample_n(sample_model_data,8000)
data("biome_layer")

test_that("stops if no occ.data provided", {
  expect_error(spatiotemp_block(spatial.layer=biome_layer,spatial.split.degrees=3,temporal.block=c("month"),vars.to.block.by=colnames(sample_model_data)[9:12],n.blocks=10))})

test_that("stops if no vars.to.block.by provided", {
  expect_error(spatiotemp_block(occ.data = sample_model_data,spatial.layer=biome_layer,spatial.split.degrees=3,temporal.block=c("month"),n.blocks=10))})

test_that("stops if spatial.layer not class RasterLayer", {
  expect_error(spatiotemp_block(occ.data = sample_model_data,spatial.layer=c(0,20,-20,30),spatial.split.degrees=3,temporal.block=c("month"),vars.to.block.by=colnames(sample_model_data)[9:12],n.blocks=10))})

test_that("stops if spatial.split.degrees not class numeric", {
  expect_error(spatiotemp_block(occ.data = sample_model_data,spatial.layer=biome_layer,spatial.split.degrees="three",temporal.block=c("month"),vars.to.block.by=colnames(sample_model_data)[9:12],n.blocks=10))})

test_that("stops if spatial.layer given but no spatial.split.degrees", {
  expect_error(spatiotemp_block(occ.data = sample_model_data,spatial.layer=biome_layer,temporal.block=c("month"),vars.to.block.by=colnames(sample_model_data)[9:12],n.blocks=10))})

test_that("stops if temporal.block not accepted", {
  expect_error(spatiotemp_block(occ.data = sample_model_data,spatial.layer=biome_layer,spatial.split.degrees=3,temporal.block=c("decadal"),vars.to.block.by=colnames(sample_model_data)[9:12],n.blocks=10))})

test_that("Doesn't error with only temporal blocking by one feature", {
 expect_warning(spatiotemp_block(occ.data = sample_model_data,temporal.block=c("month"),vars.to.block.by=colnames(sample_model_data)[9:12],n.blocks=10))})

test_that("Doesn't error with only temporal blocking by two feature", {
  expect_warning(spatiotemp_block(occ.data = sample_model_data,temporal.block=c("quarter","year"),vars.to.block.by=colnames(sample_model_data)[9:12],n.blocks=10))})

test_that("Doesn't error with only spatial blocking", {
  expect_warning(spatiotemp_block(occ.data = sample_model_data,spatial.layer = biome_layer,spatial.split.degrees=3,vars.to.block.by=colnames(sample_model_data)[9:12],n.blocks=10))})

test_that("Doesn't error with spatial and temporal blocking", {
  expect_warning(spatiotemp_block(occ.data = sample_model_data,spatial.layer = biome_layer,spatial.split.degrees=3,temporal.block=c("month"),vars.to.block.by=colnames(sample_model_data)[9:12],n.blocks=10))})

test_that("Errors if too many blocks expected", {
  expect_error(spatiotemp_block(occ.data = sample_model_data,spatial.layer = biome_layer,spatial.split.degrees=3,temporal.block=c("month"),vars.to.block.by=colnames(sample_model_data)[9:12],n.blocks=100000))})

