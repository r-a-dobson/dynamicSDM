
data("sample_explan_data")
sample_explan_data$fakeblock <-  sample(1:3, nrow(sample_explan_data), replace = T)
sample_explan_data$fakeweights <-  sample(1:50, nrow(sample_explan_data), replace = T)
sample_explan_data$fakeabundance <-  sample(1:50, nrow(sample_explan_data), replace = T)
sample_explan_data_test <- dplyr::sample_n(sample_explan_data, 100)
sample_explan_data_train <- dplyr::sample_n(sample_explan_data, 100)

colnames(sample_explan_data_train)[14]<-"Precipitation8Wsum"

results1 <- brt_fit(occ.data = sample_explan_data_train,
                    test.data = sample_explan_data_test,
                    weights.col = "fakeweights",
                    response.col = "presence.absence",
                    distribution = "bernoulli",
                    n.trees = 1,
                    shrinkage = 0.01,
                    interaction.depth = 1,
                    varnames = "Precipitation8Wsum",
                    block.col = "fakeblock"
)
results1<-results1[1:2]

results1_eval <- list(c(0.6, 0.7))

results3 <- brt_fit(occ.data = sample_explan_data_train,
                        test.data = sample_explan_data_test,
                        response.col = "fakeabundance",
                        distribution = "gaussian",
                        varnames = "Precipitation8Wsum",
                        n.tree = 1,
                        block.col = "fakeblock",
)
results3<-results3[1:2]
results3_eval <- list(c(0.70, 0.56))




test_that("Stops if no models provided", {
  expect_error(
    dynamic_proj(
      dates = c("2010-01-01", "2010-04-01"),
      local.directory = testthat::test_path("test-files"),
      projection.method = c("binary", "proportional", "abundance", "stacked"),
      sdm.thresh = 0.5,
      sdm.weight = as.numeric(unlist(results1_eval)),
      sam.weight = as.numeric(unlist(results3_eval)),
      save.directory = tempdir()
    )
  )
})

test_that("Stops if no dates provided", {
  expect_error(
    dynamic_proj(
      local.directory = testthat::test_path("test-files"),
      projection.method = c("binary", "proportional", "abundance", "stacked"),
      sdm.mod = results1,
      sdm.thresh = 0.5,
      sdm.weight = as.numeric(unlist(results1_eval)),
      sam.mod = results3,
      sam.weight = as.numeric(unlist(results3_eval)),
      save.directory = tempdir()
    )
  )
})

test_that("Stops if local.directory doesn't exist", {
  expect_error(
    dynamic_proj(
      dates = c("2010-01-01", "2010-04-01"),
      local.directory = "not/real",
      projection.method = c("binary", "proportional", "abundance", "stacked"),
      sdm.mod = results1,
      sdm.thresh = 0.5,
      sdm.weight = as.numeric(unlist(results1_eval)),
      sam.mod = results3,
      sam.weight = as.numeric(unlist(results3_eval)),
      save.directory = tempdir()
    )
  )
})

test_that("Stops if no save.directory or save.folder provided", {
  expect_error(
    dynamic_proj(
      dates = c("2010-01-01", "2010-04-01"),
      local.directory = testthat::test_path("test-files"),
      projection.method = c("binary"),
      sdm.mod = results1,
      sdm.thresh = 0.5,
      sdm.weight = as.numeric(unlist(results1_eval)),
      sam.mod = results3,
      sam.weight = as.numeric(unlist(results3_eval))
    )
  )
})

test_that("Stops if projection.method doesn't exist", {
  expect_error(
    dynamic_proj(
      dates = c("2010-01-01", "2010-04-01"),
      local.directory = testthat::test_path("test-files"),
      projection.method = c("fake"),
      sdm.mod = results1,
      sdm.thresh = 0.5,
      sdm.weight = as.numeric(unlist(results1_eval)),
      sam.mod = results3,
      sam.weight = as.numeric(unlist(results3_eval)),
      save.directory = tempdir()
    )
  )
})

test_that("Stops if threshold not equal to 1 or length of blocks", {
  expect_error(
    dynamic_proj(
      dates = c("2010-01-01", "2010-04-01"),
      local.directory = testthat::test_path("test-files"),
      projection.method = c("binary", "proportional", "abundance", "stacked"),
      sdm.mod = results1,
      sdm.thresh = c(0.5, 0.9),
      sdm.weight = as.numeric(unlist(results1_eval)),
      sam.mod = results3,
      sam.weight = as.numeric(unlist(results3_eval)),
      save.directory = tempdir()
    )
  )
})

test_that("Stops if SDM weights not equal to 1 or length SDM.MODELS", {
  expect_error(
    dynamic_proj(
      dates = c("2010-01-01", "2010-04-01"),
      local.directory = testthat::test_path("test-files"),
      projection.method = c("binary", "proportional", "abundance", "stacked"),
      sdm.mod = results1,
      sdm.thresh = c(0.5),
      sdm.weight = c(1, 0),
      sam.mod = results3,
      sam.weight = as.numeric(unlist(results3_eval)),
      save.directory = tempdir()
    )
  )
})

test_that("Stops if SAM weights not equal to 1 or length SAM.MODELS", {
  expect_error(
    dynamic_proj(dates = c("2010-01-01", "2010-04-01"),
      local.directory = testthat::test_path("test-files"),
      projection.method = c("binary", "proportional", "abundance", "stacked"),
      sdm.mod = results1,
      sdm.weight = as.numeric(unlist(results1_eval)),
      sam.mod = results3,
      sam.weight = c(1, 0),
      save.directory = tempdir()
    )
  )
})

test_that("Success if projection.method = all", {
  save.directory = tempdir()
  dates = c("2010-01-01")
  filenames <- paste0(dates, "_stacked.tif")
  dynamic_proj(dates = c("2010-01-01"),
    local.directory = testthat::test_path("test-files"),
    projection.method = c("binary", "proportional", "abundance", "stacked"),
    sdm.mod = results1,
    sdm.thresh = 0.5,
    sdm.weight = as.numeric(unlist(results1_eval)),
    cov.file.type = "csv",
    sam.mod = results3,
    sam.weight = as.numeric(unlist(results3_eval)),
    save.directory = tempdir()
  )
  expect_equal(file.exists(paste0(save.directory, "/", filenames[1])), TRUE)
})



test_that("Success if projection.method = all one mod", {
  save.directory = tempdir()
  dates = c("2010-04-01")
  filenames <- paste0(dates, "_stacked.tif")
  dynamic_proj(dates = c("2010-04-01"),
    local.directory = testthat::test_path("test-files"),
    projection.method = c("binary", "proportional", "abundance", "stacked"),
    sdm.mod = results1[[1]],
    sdm.thresh = 0.5,
    sdm.weight = 1,
    cov.file.type = "csv",
    sam.mod = results3[[1]],
    sam.weight = 1,
    save.directory = tempdir()
  )
  expect_equal(file.exists(paste0(save.directory, "/", filenames[1])), TRUE)
})


test_that("Success if projection.method = tif", {
  save.directory = tempdir()
  dates = c("2011-01-01")
  filenames <- paste0(dates, "_stacked.tif")
  dynamic_proj(dates = c("2011-01-01"),
    local.directory = testthat::test_path("test-files"),
    projection.method = c("binary", "proportional", "abundance", "stacked"),
    sdm.mod = results1,
    sdm.thresh = 0.5,
    sdm.weight = as.numeric(unlist(results1_eval)),
    cov.file.type = "tif",
    sam.mod = results3,
    sam.weight = as.numeric(unlist(results3_eval)),
    save.directory = tempdir()
  )
  expect_equal(file.exists(paste0(save.directory, "/", filenames[1])), TRUE)
})


test_that("Success if projection.method = tif and just sdm", {
  save.directory = tempdir()
  dates = c("2010-01-01")
  filenames <- paste0(dates, "_stacked.tif")
  dynamic_proj(dates = c("2010-01-01"),
    local.directory = testthat::test_path("test-files"),
    projection.method = c("binary"),
    sdm.mod = results1[[1]],
    sdm.thresh = 0.5,
    sdm.weight = 1,
    cov.file.type = "tif"
    ,
    save.directory = tempdir()
  )
  expect_equal(file.exists(paste0(save.directory, "/", filenames[1])), TRUE)
})



data("sample_extent_data")

test_that("Success if spatial mask used", {
  save.directory = tempdir()
  dates = c("2010-04-01")
  filenames <- paste0(dates, "_stacked.tif")
  dynamic_proj(dates = c("2010-04-01"),
    local.directory = testthat::test_path("test-files"),
    projection.method = c("binary", "proportional", "abundance", "stacked"),
    sdm.mod = results1[[1]],
    sdm.thresh = 0.5,
    sdm.weight = 1,
    cov.file.type = "csv",
    spatial.mask = sample_extent_data,
    sam.mod = results3[[1]],
    sam.weight = 1,
    save.directory = tempdir()
  )
  expect_equal(file.exists(paste0(save.directory, "/", filenames[1])), TRUE)
})


test_that("Success if projection.method = tif and spatial mask used", {
  save.directory = tempdir()
  dates = c("2011-01-01")
  filenames <- paste0(dates, "_stacked.tif")
  dynamic_proj(dates = c("2011-01-01"),
    local.directory = testthat::test_path("test-files"),
    projection.method = c("binary", "proportional", "abundance", "stacked"),
    sdm.mod = results1,
    sdm.thresh = 0.5,
    sdm.weight = as.numeric(unlist(results1_eval)),
    cov.file.type = "tif",
    spatial.mask = sample_extent_data,
    sam.mod = results3,
    sam.weight = as.numeric(unlist(results3_eval)),
    save.directory = tempdir()
  )
  expect_equal(file.exists(paste0(save.directory, "/", filenames[1])), TRUE)
})

