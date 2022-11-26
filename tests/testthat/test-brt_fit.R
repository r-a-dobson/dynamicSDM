data("sample_model_data")

sample_model_data_test<-dplyr::sample_n(sample_model_data,100)
sample_model_data_train<-dplyr::sample_n(sample_model_data,1000)

test_that("stops if no occ.data provided", {
  expect_error(brt_fit(response.col = "presence.absence",test.data=sample_model_data_test,distribution="bernoulli", varnames=colnames(sample_model_data_test)[9:12]))})

test_that("stops if no distribution specified", {
  expect_error(brt_fit(occ.data = sample_model_data_train,test.data=sample_model_data_test,response.col = "presence.absence", varnames=colnames(sample_model_data_train)[9:12]))})

test_that("stops if no varnames specified", {
  expect_error(brt_fit(occ.data = sample_model_data_train,test.data=sample_model_data_test,response.col = "presence.absence",distribution="bernoulli"))})

test_that("stops if varnames not in occ.data", {
  expect_error(brt_fit(occ.data = sample_model_data_train,test.data=sample_model_data_test,response.col = "presence.absence",distribution="bernoulli", varnames=c("fakevariable")))})

test_that("stops if block.col not in occ.data", {
  expect_error(brt_fit(occ.data = sample_model_data_train,test.data=sample_model_data_test,response.col = "presence.absence",distribution="bernoulli",block.col="fakecol", varnames=colnames(sample_model_data_train)[9:12]))})

test_that("stops if response.col not in occ.data", {
  expect_error(brt_fit(occ.data = sample_model_data_train,test.data=sample_model_data_test,response.col = "fakecol",distribution="bernoulli", varnames=colnames(sample_model_data_train)[9:12]))})

test_that("If no block col provided, returns list of length 1", {
  results<-brt_fit(occ.data = sample_model_data_train,test.data=sample_model_data_test,response.col = "presence.absence",distribution="bernoulli", varnames=colnames(sample_model_data_train)[9:12])
 expect_equal(length(results),1)})

test_that("Works if no block.col specified, and model parameters given", {
  results<-brt_fit(occ.data = sample_model_data_train,response.col = "presence.absence",distribution="bernoulli", n.trees=1500, shrinkage=0.1,interaction.depth=4,varnames=colnames(sample_model_data_train)[9:12])
  expect_equal(length(results),1)})

test_that("Uses specified number of trees", {
  n<-sample(100:1000,1)
  results<-brt_fit(occ.data = sample_model_data_train,test.data=sample_model_data_test,response.col = "presence.absence",distribution="bernoulli", n.trees=n, shrinkage=0.1,interaction.depth=4,varnames=colnames(sample_model_data_train)[9:12])
  expect_equal(results[[1]]$n.trees,n)})

test_that("Uses specified shrinkage ", {
  n<-sample(0.001:1,1)
  results<-brt_fit(occ.data = sample_model_data_train,test.data=sample_model_data_test,response.col = "presence.absence",distribution="bernoulli", n.trees=1500, shrinkage=n,interaction.depth=4,varnames=colnames(sample_model_data_train)[9:12])
  expect_equal(results[[1]]$shrinkage,n)})

test_that("Uses specified interaction.depth ", {
  n<-sample(1:4,1)
  results<-brt_fit(occ.data = sample_model_data_train,test.data=sample_model_data_test,response.col = "presence.absence",distribution="bernoulli", n.trees=1500, shrinkage=0.01,interaction.depth= n,varnames=colnames(sample_model_data_train)[9:12])
  expect_equal(results[[1]]$interaction.depth,n)})

test_that("Uses specified interaction.depth and weights", {
  n<-sample(1:4,1)
  results<-brt_fit(occ.data = sample_model_data_train,test.data=sample_model_data_test,weights.col="sampling_weights", response.col = "presence.absence",distribution="bernoulli", n.trees=1500, shrinkage=0.01,interaction.depth= n,varnames=colnames(sample_model_data_train)[9:12])
  expect_equal(results[[1]]$interaction.depth,n)})


test_that("Works if block.col specified, and model parameters given", {
  results<-brt_fit(occ.data = sample_model_data_train,response.col = "presence.absence",block.col="blockno",distribution="bernoulli", n.trees=1500, shrinkage=0.1,interaction.depth=4,varnames=colnames(sample_model_data_train)[9:12])
  expect_equal(length(results),length(unique(sample_model_data_train[,"blockno"])))})

test_that("Works if block.col specified, and model parameters not given", {
  results<-brt_fit(occ.data = sample_model_data_train,response.col = "presence.absence",block.col="blockno",distribution="bernoulli",varnames=colnames(sample_model_data_train)[9:12])
  expect_equal(length(results),length(unique(sample_model_data_train[,"blockno"])))})



## Test works with abundance data, not just binary
sample_model_data_train_abund<-sample_model_data[sample_model_data$individualCount>0 ,]
sample_model_data_train_abund<-sample_model_data_train_abund[!is.na(sample_model_data_train_abund$individualCount),]
sample_model_data_train_abund$individualCount<-log10(1+sample_model_data_train_abund$individualCount)

sample_model_data_test_abund<-sample_model_data[sample_model_data$individualCount>0 ,]
sample_model_data_test_abund<-sample_model_data_test_abund[!is.na(sample_model_data_test_abund$individualCount),]
sample_model_data_test_abund$individualCount<-log10(1+sample_model_data_test_abund$individualCount)

test_that("Works if no block.col specified, and model parameters given for different family and data type", {
  results<-brt_fit(occ.data = sample_model_data_train_abund,test.data=sample_model_data_test_abund,response.col = "individualCount",distribution="gaussian",varnames=colnames(sample_model_data_train)[9:12])
  expect_equal(length(results),1)})

test_that("Works if  block.col specified, and no model parameters given for different family and data type", {
  results<-brt_fit(occ.data = sample_model_data_train_abund,response.col = "individualCount",block.col="blockno",distribution="gaussian",varnames=colnames(sample_model_data_train)[9:12])
  expect_equal(length(results),length(unique(sample_model_data_train_abund[,"blockno"])))})




