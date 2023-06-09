
data(sample_explan_data)

test_that("stops if no occcurence data provided", {
  expect_error(spatiotemp_bias(temporal.level = "month"))
})

test_that("stops if no temporal.level provided", {
  expect_error(spatiotemp_bias(occ.data = sample_explan_data))
})

test_that("stops if provided temporal temporal.level is incorrect class", {
  expect_error(spatiotemp_bias(occ.data = sample_explan_data, temporal.level =
                                 "decadal"))
})

test_that("Result of length(2) ", {
  result <- spatiotemp_bias(occ.data = sample_explan_data,
                    temporal.level = "day",
                    plot = F)
  expect_equal(length(result), 2)
})

test_that("Result of length(4) method - simple ", {
  result <- spatiotemp_bias(
      occ.data = sample_explan_data,
      temporal.level = c("day", "month", "year"),
      plot = F,
      spatial.method = "simple"
    )
  expect_equal(length(result), 4)
})

test_that("Result of length(4) method - core ", {
  result <- spatiotemp_bias(
      occ.data = sample_explan_data,
      temporal.level = c("day", "month", "year"),
      plot = F,
      spatial.method = "core"
    )
  expect_equal(length(result), 4)
})

test_that("Result of length(4) method - convex ", {
  result <- spatiotemp_bias(
      occ.data = sample_explan_data,
      temporal.level = c("day", "month", "year"),
      plot = F,
      spatial.method = "convex"
    )
  expect_equal(length(result), 4)
})

