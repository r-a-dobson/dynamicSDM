#' Generate vector of dates for dynamic projections
#'
#' Creates a vector of dates at regular intervals between two given dates.
#' @param startdate a character string, the start date in format YYYY-MM-DD.
#' @param enddate a character string, the end date in format YYYY-MM-DD.
#' @param interval.level a character string, the time-step of intervals. One of '"day"', '"month"' or '"year"': can be abbreviated.
#' @param interval a numeric value, the length of interval in interval.level units to generate between the start and end date.
#' @details Function returns a vector of dates given start.date and end.date at given interval size.
#' @return Vector of dates between start date and end date split at regular intervals.
#' @examples
#' dynamic_proj_dates(
#'  startdate = "2000-01-01",
#'  enddate = "2001-01-01",
#'  interval.level = "month",
#'  interval = 2
#')
#' @export
#'


dynamic_proj_dates <-
  function(startdate,
           enddate,
           interval.level,
           interval) {

    # Match interval.level to available options
    interval.level <- match.arg(arg = interval.level,
                                choices = c("days", "weeks", "months", "years"))

    # Check formatting of input arguments
    if (!is.character(startdate)) {stop("startdate must be character")}
    if (!is.character(enddate)) {stop("enddate must be character")}

    # Check dates given are valid dates
    tryCatch({as.Date(startdate)}, error = function(e) {
      stop("Invalid startdate")})

    tryCatch({as.Date(enddate)}, error = function(e) {
      stop("Invalid enddate")})

    startdate <- as.Date(startdate)
    enddate <- as.Date(enddate)

    # Check start date is earlier than end date
    if (startdate - enddate > 0) {stop("startdate must be before enddate")}

    listofdates <- as.Date(startdate)

    if (interval.level == "days") {
      i <- lubridate::interval(startdate, enddate) %/% lubridate::days(interval)
      if (i == 0) {stop("There are no dates for given interval and end date")}

      # Iterate through number of interval dates, adding the interval each time
      for (int in 1:i) {
        listofdates <- c(listofdates,
                         (startdate + lubridate::days((int * interval))))
      }

    }

    if (interval.level == "weeks") {
      i <- lubridate::interval(startdate, enddate) %/% lubridate::weeks(interval)
      if (i == 0) {stop("There are no dates for given interval and end date")}

      # Iterate through number of interval dates, adding the interval each time
      for (int in 1:i) {
        listofdates <- c(listofdates,
                         (startdate + lubridate::weeks((int * interval))))
      }
    }

    if (interval.level == "months") {
      i <- lubridate::interval(startdate, enddate) %/% months(interval)

      if (i == 0) {stop("There are no dates for given interval and end date")}

      # Iterate through number of interval dates, adding the interval each time
      for (int in 1:i) {
        listofdates <- c(listofdates,
                         (startdate + months((int * interval))))
      }
    }

    if (interval.level == "years") {
      i <- lubridate::interval(startdate, enddate) %/% lubridate::years(interval)
      if (i == 0) {stop("There are no dates for given interval and end date")}

      # Iterate through number of interval dates, adding the interval each time
      for (int in 1:i) {
        listofdates <- c(listofdates,
                         (startdate + lubridate::years((int * interval))))
      }
    }

    return(listofdates)
  }
