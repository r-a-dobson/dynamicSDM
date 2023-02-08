data(sample_occ_data)

test_that("Result adds two columns  ", {
  sample_occ_data_converted <- convert_gbif(sample_occ_data)
  expect_equal(ncol(sample_occ_data_converted), (ncol(sample_occ_data) +
                                                   2))
})

test_that("Result year column numeric not integer  ", {
  sample_occ_data_converted <- convert_gbif(sample_occ_data)
  expect_equal(class(sample_occ_data_converted$year), "numeric")
})

test_that("Result contains x column   ", {
  sample_occ_data_converted <- convert_gbif(sample_occ_data)
  expect_true("x" %in% colnames(sample_occ_data_converted))
})

test_that("Result contains y column   ", {
  sample_occ_data_converted <- convert_gbif(sample_occ_data)
  expect_true("y" %in% colnames(sample_occ_data_converted))
})
