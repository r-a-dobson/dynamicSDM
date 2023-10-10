
data(sample_occ_data)
sample_occ_data <- convert_gbif(sample_occ_data)

test_that("stops if no occcurence data provided", {
  expect_error(spatiotemp_check(na.handle = "exclude", duplicate.handle =
                                  "exclude"))
})

test_that("stops if no occcurence data not a data frame", {
  expect_error(
    spatiotemp_check(
      occ.data = "dataframe",
      na.handle = "exclude",
      duplicate.handle = "exclude"
    )
  )
})

test_that("duplicates removed", {
  checked <- spatiotemp_check(
      occ.data = sample_occ_data,
      duplicate.handle = "exclude",
      date.res = "day"
    )
  expect_equal(nrow(checked), 315)
})

test_that("NAs removed", {
  checked <- spatiotemp_check(occ.data = sample_occ_data,
                     na.handle = "exclude",
                     date.res = "day")
  expect_equal(nrow(checked), 594)
})

test_that("identifies missing year col", {
  expect_error(spatiotemp_check(
    occ.data = subset(sample_occ_data, select = -c(year)),
    date.res = "day"
  ))
})
test_that("identifies missing month col", {
  expect_error(spatiotemp_check(
    occ.data = subset(sample_occ_data, select = -c(month)),
    date.res = "day"
  ))
})
test_that("identifies missing day col", {
  expect_error(spatiotemp_check(
    occ.data = subset(sample_occ_data, select = -c(day)),
    date.res = "day"
  ))
})
test_that("identifies missing x col", {
  expect_error(spatiotemp_check(
    occ.data = subset(sample_occ_data, select = -c(x)),
    date.res = "day"
  ))
})
test_that("identifies missing y col", {
  expect_error(spatiotemp_check(
    occ.data = subset(sample_occ_data, select = -c(y)),
    date.res = "day"
  ))
})

test_that("identifies wrong year class", {
  wrong.class <- sample_occ_data
  wrong.class$year <- as.character(wrong.class$year)
  expect_error(spatiotemp_check(occ.data = wrong.class, date.res = "day"))
})

test_that("identifies wrong month class", {
  wrong.class <- sample_occ_data
  wrong.class$month <- as.character(wrong.class$month)
  expect_error(spatiotemp_check(occ.data = wrong.class, date.res = "day"))
})

test_that("identifies wrong day class", {
  wrong.class <- sample_occ_data
  wrong.class$day <- as.character(wrong.class$day)
  expect_error(spatiotemp_check(occ.data = wrong.class, date.res = "day"))
})

test_that("identifies wrong x class", {
  wrong.class <- sample_occ_data
  wrong.class$x <- as.character(wrong.class$x)
  expect_error(spatiotemp_check(occ.data = wrong.class, date.res = "day"))
})

test_that("identifies wrong y class", {
  wrong.class <- sample_occ_data
  wrong.class$x <- as.character(wrong.class$x)
  expect_error(spatiotemp_check(occ.data = wrong.class, date.res = "day"))
})

test_that("excludes invalid date", {
  expect_equal(nrow(
    spatiotemp_check(
      occ.data = sample_occ_data,
      date.handle = "exclude",
      date.res = "day"
    )
  ), 597)
})

test_that("ignores invalid date", {
  expect_equal(nrow(
    spatiotemp_check(
      occ.data = sample_occ_data,
      date.handle = "ignore",
      date.res = "day"
    )
  ), 600)
})

test_that("excludes invalid x", {
  expect_equal(nrow(
    spatiotemp_check(
      occ.data = sample_occ_data,
      coord.handle = "exclude",
      date.res = "day"
    )
  ), 594)
})

test_that("ignores invalid x", {
  expect_equal(nrow(
    spatiotemp_check(
      occ.data = sample_occ_data,
      coord.handle = "ignore",
      date.res = "day"
    )
  ), 600)
})

test_that("excludes invalid y", {
  expect_equal(nrow(
    spatiotemp_check(
      occ.data = sample_occ_data,
      coord.handle = "exclude",
      date.res = "day"
    )
  ), 594)
})

test_that("ignores invalid y", {
  expect_equal(nrow(
    spatiotemp_check(
      occ.data = sample_occ_data,
      coord.handle = "ignore",
      date.res = "day"
    )
  ), 600)
})

test_that("All work together", {
  expect_equal(nrow(
    spatiotemp_check(
      occ.data = sample_occ_data,
      coord.handle = "exclude",
      date.res = "day",
      date.handle = "exclude",
      duplicate.handle = "exclude",
      na.handle = "exclude"
    )
  ), 311)
})

test_that("Returns all columns", {
  expect_equal(ncol(
    spatiotemp_check(
      occ.data = sample_occ_data,
      coord.handle = "exclude",
      date.res = "day",
      date.handle = "exclude",
      duplicate.handle = "exclude",
      na.handle = "exclude"
    )
  ), 9)
})

testthat::test_that("CoordinateCleaner works (correct ncol)", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  expect_equal(ncol(
    spatiotemp_check(
      occ.data = sample_occ_data,
      coord.handle = "exclude",
      date.res = "day",
      date.handle = "exclude",
      duplicate.handle = "exclude",
      na.handle = "exclude",
      coordclean = T,
      coordclean.species = "quelea",
      coordclean.handle = "exclude"
    )
  ), 9)
})

test_that("CoordinateCleaner works (correct nrow)", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  expect_equal(nrow(
    spatiotemp_check(
      occ.data = sample_occ_data,
      coord.handle = "exclude",
      date.res = "day",
      date.handle = "exclude",
      duplicate.handle = "exclude",
      na.handle = "exclude",
      coordclean = T,
      coordclean.species = "quelea",
      coordclean.handle = "exclude"
    )
  ), 301)
})

test_that("CoordinateCleaner works return report", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  expect_equal(ncol(
    spatiotemp_check(
      occ.data = sample_occ_data,
      coord.handle = "exclude",
      date.handle = "exclude",
      duplicate.handle = "exclude",
      na.handle = "exclude",
      date.res = "day",
      coordclean = T,
      coordclean.species = "quelea",
      coordclean.handle = "report"
    )
  ), ncol(sample_occ_data)+2)
})
