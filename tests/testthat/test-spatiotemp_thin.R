

data("sample_explan_data")

test_that("stops if no occcurence data provided", {
  expect_error(spatiotemp_thin(
    method = "day",
    temporal.dist = 20,
    iterations = 10
  ))
})

test_that("stops if temporal.method specified is not accepted", {
  expect_error(
    spatiotemp_thin(
      occ.data = sample_explan_data,
      temporal.method = "week",
      temporal.dist = 20,
      iterations = 10,
      spatial.split.degrees = 4,
      spatial.dist = 10000
    )
  )
})

test_that("stops if no temporal.dist provided", {
  expect_error(
    spatiotemp_thin(
      occ.data = sample_explan_data,
      temporal.method = "day",
      iterations = 10,
      spatial.split.degrees = 4,
      spatial.dist = 10000
    )
  )
})

test_that("stops if temporal.dis wrong class", {
  expect_error(
    spatiotemp_thin(
      occ.data = sample_explan_data,
      temporal.method = "day",
      temporal.dist = "twenty days",
      spatial.split.degrees = 4,
      spatial.dist = 10000,
      iterations = 10
    )
  )
})

test_that("stops if temporal.dis not of length 1", {
  expect_error(
    spatiotemp_thin(
      occ.data = sample_explan_data,
      temporal.method = "day",
      temporal.dist = c(1, 2, 3),
      iterations = 10
    )
  )
})

test_that("stops if iterations wrong class", {
  expect_error(
    spatiotemp_thin(
      occ.data = sample_explan_data,
      temporal.method = "day",
      temporal.dist = 20,
      spatial.split.degrees = 4,
      spatial.dist = 10000,
      iterations = "ten"
    )
  )
})

test_that("stops if iterations not of length 1", {
  expect_error(
    spatiotemp_thin(
      occ.data = sample_explan_data,
      temporal.method = "day",
      temporal.dist = 20,
      spatial.split.degrees = 4,
      spatial.dist = 10000,
      iterations = c(1, 2, 3, 4)
    )
  )
})

test_that("Success when not inputting iterations - default works", {
  result <- spatiotemp_thin(
      occ.data = sample_explan_data,
      temporal.method = "day",
      temporal.dist = 5,
      spatial.split.degrees = 4,
      spatial.dist = 10000,
      iterations = 2
    )
  expect_equal(class(result), "data.frame")
})

test_that("method day produces data.frame", {
  result <- spatiotemp_thin(
      occ.data = sample_explan_data,
      temporal.method = "day",
      temporal.dist = 5,
      spatial.split.degrees = 4,
      spatial.dist = 10000,
      iterations = 2
    )
  expect_equal(class(result), "data.frame")
})

test_that("method day produces smaller data.frame when thinning", {
  result <- spatiotemp_thin(
      occ.data = sample_explan_data,
      temporal.method = "day",
      temporal.dist = 5,
      spatial.split.degrees = 4,
      spatial.dist = 10000,
      iterations = 2
    )
  expect_lt(nrow(result), nrow(sample_explan_data))
})


test_that("method day produces data.frame over 0 even when high temporal.dist value",
          {
            result <- spatiotemp_thin(
                occ.data = sample_explan_data,
                temporal.method = "day",
                temporal.dist = 365,
                spatial.split.degrees = 4,
                spatial.dist = 10000,
                iterations = 2
              )
            expect_gt(nrow(result), 0)
          })

test_that("method DOY produces data.frame", {
  result <- spatiotemp_thin(
      occ.data = sample_explan_data,
      temporal.method = "DOY",
      temporal.dist = 5,
      spatial.split.degrees = 4,
      spatial.dist = 10000,
      iterations = 2
    )
  expect_equal(class(result), "data.frame")
})

test_that("method DOY produces data.frame over 0 even when high temporal.dist value",
          {
            result <- spatiotemp_thin(
                occ.data = sample_explan_data,
                temporal.method = "DOY",
                temporal.dist = 365,
                spatial.split.degrees = 4,
                spatial.dist = 10000,
                iterations = 2
              )
            expect_gt(nrow(result), 0)
          })
