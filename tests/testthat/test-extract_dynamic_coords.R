data("sample_explan_data")
sample_occ_abs_data_fortest <- dplyr::sample_n(sample_explan_data, 1)
test_bandname <- "LST_Day_1km"
test_datasetname <- "MODIS/006/MOD11A1"


test_that("stops if no occcurence data provided", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})


test_that("stops if no datasetname provided", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ = sample_occ_abs_data_fortest,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})

test_that("stops if no bandname provided", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})


test_that("stops if no spatial.res.metres provided", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})


test_that("stops if no temporal.res provided", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})


test_that("stops if no temporal.direction provided", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})


test_that("stops if no GEE.math.fun provided", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})


test_that("stops if no save.method provided", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.directory = tempdir()
    )
  )
})


test_that("stops if no save.directory provided", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split"
    )
  )
})


test_that("stops if more than one GEE.math.fun provided", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = c("mean", "max"),
      save.method = "split",
      save.directory = tempdir()
    )
  )
})



test_that("stops if occcurence data not data.frame", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ.data = c(20, 30, 40),
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})

test_that("stops if datasetname not character", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = 5,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})
test_that("stops if bandname not character", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = 88,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})


test_that("stops if spatial.res.metres data not numeric", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = "onethousand",
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})

test_that("stops if temporal.res not numeric", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = "seven",
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})

test_that("stops if temporal.direction not character", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = 5,
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})


test_that("stops if save.method does not match accepted choices", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "notsure",
      save.directory = tempdir()
    )
  )
})

test_that("stops if temporal.direction does not match accepted choices", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "before",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})

test_that("stops if GEE.math.fun does not match viable functions", {
  skip_if_no_GEE_credentials()
  expect_error(
    extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "nofunction",
      save.method = "split",
      save.directory = tempdir()
    )
  )
})



test_that(
  "If method split, return numeric vector of completed rows, the same length as nrow(occ.data)",
  {
    skip_if_no_GEE_credentials()
    results <- extract_dynamic_coords(
        occ.data = sample_occ_abs_data_fortest,
        datasetname = test_datasetname,
        bandname = test_bandname,
        spatial.res.metres = 10000,
        temporal.res = 7,
        temporal.direction = "prior",
        GEE.math.fun = "mean",
        save.method = "split",
        save.directory = tempdir()
      )
    expect_equal(length(results), nrow(sample_occ_abs_data_fortest))
  }
)


test_that("If method split, return integer vector of completed rows", {
  skip_if_no_GEE_credentials()
  results <- extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  expect_equal(class(results), "integer")
})



test_that("If method split save individual csvc files in save.directory, default varname",
          {
            skip_if_no_GEE_credentials()
            results <- extract_dynamic_coords(
                occ.data = sample_occ_abs_data_fortest,
                datasetname = test_datasetname,
                bandname = test_bandname,
                spatial.res.metres = 10000,
                temporal.res = 7,
                temporal.direction = "prior",
                GEE.math.fun = "mean",
                save.method = "split",
                save.directory = tempdir()
              )
            n <- sample(1:nrow(sample_occ_abs_data_fortest), 1)
            expect_equal(file.exists(
              paste0(tempdir(), "/", n, "_", test_bandname, "_7_prior_mean.csv")
            ), TRUE)
          })



test_that("If method split save individual csv files in save.directory, specified varname",
          {
            skip_if_no_GEE_credentials()
            results <- extract_dynamic_coords(
                occ.data = sample_occ_abs_data_fortest,
                datasetname = test_datasetname,
                varname = "thisisatest",
                bandname = test_bandname,
                spatial.res.metres = 10000,
                temporal.res = 7,
                temporal.direction = "prior",
                GEE.math.fun = "mean",
                save.method = "split",
                save.directory = tempdir()
              )
            n <- sample(1:nrow(sample_occ_abs_data_fortest), 1)
            expect_equal(file.exists(paste0(tempdir(), "/", n, "_thisisatest.csv")), TRUE)
          })




test_that("If method combined save combined data csv file in save.directory, default varname",
          {
            skip_if_no_GEE_credentials()
            results <- extract_dynamic_coords(
                occ.data = sample_occ_abs_data_fortest,
                datasetname = test_datasetname,
                bandname = test_bandname,
                spatial.res.metres = 10000,
                temporal.res = 7,
                temporal.direction = "prior",
                GEE.math.fun = "mean",
                save.method = "combined",
                save.directory = tempdir()
              )
            expect_equal(file.exists(
              paste0(
                tempdir(),
                "/all_records_combined_",
                test_bandname,
                "_7_prior_mean.csv"
              )
            ), TRUE)
          })



test_that("If method combined save combined data csv file in save.directory, default varname",
          {
            skip_if_no_GEE_credentials()
            results <- extract_dynamic_coords(
                occ.data = sample_occ_abs_data_fortest,
                datasetname = test_datasetname,
                bandname = test_bandname,
                varname = "thisisatest",
                spatial.res.metres = 10000,
                temporal.res = 7,
                temporal.direction = "prior",
                GEE.math.fun = "mean",
                save.method = "combined",
                save.directory = tempdir()
              )
            expect_equal(file.exists(paste0(
              tempdir(), "/all_records_combined_thisisatest.csv"
            )), TRUE)
          })



test_that("Combined method works with temporal.direction = post", {
  skip_if_no_GEE_credentials()
  results <- extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 100000,
      temporal.res = 7,
      temporal.direction = "post",
      GEE.math.fun = "mean",
      save.method = "combined",
      save.directory = tempdir()
    )
  n <- sample(1:nrow(sample_occ_abs_data_fortest), 1)
  expect_equal(file.exists(
    paste0(
      tempdir(),
      "/all_records_combined_",
      test_bandname,
      "_7_post_mean.csv"
    )
  ), TRUE)
})


test_that("Split method works with temporal.direction = post", {
  skip_if_no_GEE_credentials()
  results <- extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 100000,
      temporal.res = 7,
      temporal.direction = "post",
      GEE.math.fun = "mean",
      save.method = "split",
      save.directory = tempdir()
    )
  n <- sample(1:nrow(sample_occ_abs_data_fortest), 1)
  expect_equal(file.exists(
    paste0(tempdir(), "/", n, "_", test_bandname, "_7_post_mean.csv")
  ), TRUE)
})


test_that("If method combined returns data.frame", {
  skip_if_no_GEE_credentials()
  results <- extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "combined",
      save.directory = tempdir()
    )
  expect_equal(class(results), "data.frame")
})


test_that("If method combined returns data.frame of correct length", {
  skip_if_no_GEE_credentials()
  results <- extract_dynamic_coords(
      occ.data = sample_occ_abs_data_fortest,
      datasetname = test_datasetname,
      bandname = test_bandname,
      spatial.res.metres = 10000,
      temporal.res = 7,
      temporal.direction = "prior",
      GEE.math.fun = "mean",
      save.method = "combined",
      save.directory = tempdir()
    )
  expect_equal(nrow(results), nrow(sample_occ_abs_data_fortest))
})


