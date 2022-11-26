vector<-c(2,4,0,0,2,45,67,NA,2,5)
vectorNAfirst<-c(NA,4,0,0,2,45,67,2,2,5)
vectorNAlast<-c(2,4,0,0,2,45,67,NA,2,5,NA)

test_that("allNonZero correct", {expect_equal(allNonZero(vector),7)})

test_that("anyNonZero correct", {expect_equal(anyNonZero(vector),1)})

test_that("count correct", {expect_equal(count(vector),9)})

test_that("First correct", {expect_equal(First(vector),2)})

test_that("firstNonNull correct", {expect_equal(firstNonNull(vector),2)})

test_that("First correct", {expect_equal(First(vectorNAfirst),vectorNAfirst[1])})

test_that("firstNonNull correct", {expect_equal(firstNonNull(vectorNAfirst),4)})

test_that("last correct", {expect_equal(last(vector),5)})

test_that("lastNonNull correct", {expect_equal(lastNonNull(vector),5)})

test_that("lastNonNull correct", {expect_equal(lastNonNull(vectorNAlast),5)})

test_that("mode correct", { expect_equal(mode(vector),2)})

test_that("stdDev correct", { expect_equal(round(stdDev(vector)),23)})

test_that("variance correct", { expect_equal(round(variance(vector)),531)})

