
data("sample_explan_data")
sample_explan_data$fakeblock<-sample(1:3,nrow(sample_explan_data),replace=T)
sample_explan_data$fakeweights<-sample(1:50,nrow(sample_explan_data),replace=T)
sample_explan_data$fakeabundance<-sample(1:50,nrow(sample_explan_data),replace=T)
sample_explan_data_test<-dplyr::sample_n(sample_explan_data,100)
sample_explan_data_train<-dplyr::sample_n(sample_explan_data,100)


test_that("stops if no occ.data provided", {
  expect_error(brt_fit(response.col = "presence.absence",test.data=sample_explan_data_test,distribution="bernoulli", varnames=colnames(sample_explan_data_test)[14:16]))})

test_that("stops if no distribution specified", {
  expect_error(brt_fit(occ.data = sample_explan_data_train,test.data=sample_explan_data_test,response.col = "presence.absence", varnames=colnames(sample_explan_data_train)[14:16]))})

test_that("stops if no varnames specified", {
  expect_error(brt_fit(occ.data = sample_explan_data_train,test.data=sample_explan_data_test,response.col = "presence.absence",distribution="bernoulli"))})

test_that("stops if varnames not in occ.data", {
  expect_error(brt_fit(occ.data = sample_explan_data_train,test.data=sample_explan_data_test,response.col = "presence.absence",distribution="bernoulli", varnames=c("fakevariable")))})

test_that("stops if block.col not in occ.data", {
  expect_error(brt_fit(occ.data = sample_explan_data_train,test.data=sample_explan_data_test,response.col = "presence.absence",distribution="bernoulli",block.col="fakecol", varnames=colnames(sample_explan_data_train)[14:16]))})

test_that("stops if response.col not in occ.data", {
  expect_error(brt_fit(occ.data = sample_explan_data_train,test.data=sample_explan_data_test,response.col = "fakecol",distribution="bernoulli", varnames=colnames(sample_explan_data_train)[14:16]))})

test_that("If no block col provided, returns list of length 1", {
  results<-brt_fit(occ.data = sample_explan_data_train,test.data=sample_explan_data_test,response.col = "presence.absence",distribution="bernoulli", varnames=colnames(sample_explan_data_train)[14:16])
 expect_equal(length(results),1)})

test_that("Works if no block.col specified, and model parameters given", {
  results<-brt_fit(occ.data = sample_explan_data_train,response.col = "presence.absence",distribution="bernoulli", n.trees=1500, shrinkage=0.1,interaction.depth=4,varnames=colnames(sample_explan_data_train)[14:16])
  expect_equal(length(results),1)})


test_that("Uses specified interaction.depth and weights", {
  n<-sample(1:4,1)
  results<-brt_fit(occ.data = sample_explan_data_train,test.data=sample_explan_data_test,weights.col="fakeweights", response.col = "presence.absence",distribution="bernoulli", n.trees=1500, shrinkage=0.01,interaction.depth= n,varnames=colnames(sample_explan_data_train)[14:16])
  expect_equal(results[[1]]$interaction.depth,n)})

test_that("Works if block.col specified, and model parameters given", {
  results<-brt_fit(occ.data = sample_explan_data_train,response.col = "presence.absence",block.col="fakeblock",distribution="bernoulli", n.trees=1500, shrinkage=0.1,interaction.depth=4,varnames=colnames(sample_explan_data_train)[14:16])
  expect_equal(length(results),length(unique(sample_explan_data_train[,"fakeblock"])))})

test_that("Works if block.col specified, and model parameters not given", {
  results<-brt_fit(occ.data = sample_explan_data_train,response.col = "presence.absence",block.col="fakeblock",distribution="bernoulli",varnames=colnames(sample_explan_data_train)[14:16])
  expect_equal(length(results),length(unique(sample_explan_data_train[,"fakeblock"])))})

## Test works with abundance data, not just binary

test_that("Works if no block.col specified, and model parameters given for different family and data type", {
  results<-brt_fit(occ.data = sample_explan_data_train,test.data=sample_explan_data_test,response.col = "fakeabundance",distribution="gaussian",varnames=colnames(sample_explan_data_train)[14:16])
  expect_equal(length(results),1)})



