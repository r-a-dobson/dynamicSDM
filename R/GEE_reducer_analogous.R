

#' allNonZero returns 1 if all of its inputs are non-zero, 0 otherwise.
#' @param x a numeric vector
#' @noRd
allNonZero <- function(x, na.omit = TRUE) {
  if (na.omit == TRUE) {
    x <- na.omit(x)
  }
  return(sum(x != 0))
}

#' anynonzero returns 1 if any of its inputs are non-zero, 0 otherwise.
#' @param x a numeric vector
#' @noRd
anyNonZero <- function(x, na.omit = TRUE) {
  if (na.omit == TRUE) {
    x <- na.omit(x)
  }
  if (any(x != 0)) {
    return(1)
  }
  if (!any(x != 0)) {
    return(0)
  }
}

#' count computes the number of non-null inputs.
#' @param x a numeric vector
#' @param na.omit a logical
#' @noRd
count <- function(x, na.omit = TRUE) {
  if (na.omit == TRUE) {
    x <- na.omit(x)
  }
  return(sum(!is.na(x)))
}

#'First Returns the first of its inputs.
#' @param x a numeric vector
#' @noRd
First <- function(x) {
  return(x[1])
}

#'firstNonNull Returns the first of its non-null inputs.
#' @param x a numeric vector
#' @noRd
firstNonNull <- function(x) {
  return(na.omit(x)[1])
}

#'last Returns the last of its inputs.
#' @param x a numeric vector
#' @noRd
last <- function(x) {
  return(x[length(x)])
}

#'lastNonNull Returns the last of its non-null inputs.
#' @param x a numeric vector
#' @noRd
lastNonNull <- function(x) {
  x <- na.omit(x)
  return(x[length(x)])
}

#'mode Compute the mode of the inputs..
#' @param x a numeric vector
#' @noRd
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#'stdDev Returns the standard deviation of its inputs.
#' @param x a numeric vector
#' @noRd
stdDev <- function(x, na.omit = TRUE) {
  if (na.omit == TRUE) {
    x <- na.omit(x)
  }
  return(sqrt(mean((x - mean(
    x
  )) ^ 2)))
}

#'variance Returns the variance of its inputs.
#' @param x a numeric vector
#' @noRd
variance <- function(x, na.omit = TRUE) {
  if (na.omit == TRUE) {
    x <- na.omit(x)
  }
  return(var(x) * ((length(x) - 1) / length(x)))
}


#'skip_if_no_GEE_credentials Skips test_that test if no Google Earth Engine credentials set-up
#' @noRd
skip_if_no_GEE_credentials <- function() {

  tryCatch(
    reticulate::import("ee"),
    error = function(e)
      testthat::skip("No Google Earth Engine credentials available")
  ) # => 1
}
