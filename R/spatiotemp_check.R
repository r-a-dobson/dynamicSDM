#' Check species occurrence record formatting, completeness and validity.
#'
#' Checks the occurrence record data frame contains the column names and classes required for dynamicSDM functions. Option to exclude records containing missing, duplicate or invalid co-ordinates or dates.
#'
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".
#' @param na.handle a character string, method for handling missing data (NA values) in record co-ordinates and dates. One of '"exclude"' or '"ignore"': can be abbreviated. Default; exclude.
#' @param duplicate.handle a character string, method for handling duplicate record co-ordinates or dates. One of '"exclude"' or '"ignore"': can be abbreviated. Default; exclude.
#' @param coord.handle a character string, method for handling invalid co-ordinates in record data. One of '"exclude"' or '"ignore"': can be abbreviated. Default; exclude.
#' @param date.handle a character string, method for handling invalid dates in record data. One of '"exclude"' or '"ignore"': can be abbreviated. Default; exclude.
#' @details
#' Record dates and co-ordinates are checked for validity using the following rules. Dates must be real dates that could exist. For example, 50th February 2000 is not a valid date. Co-ordinates must have longitude (x) values between -180 and 180, and latitude (y) values between -90 and 90 to be considered valid.
#' @return By default, returns occurrence record data frame, filtered to exclude records containing missing, duplicate or invalid data in record co-ordinates and dates.
#' @examples
#'data("sample_occ_data")
#'spatiotemp_check(sample_occ_data)
#'@export

spatiotemp_check <-
  function(occ.data,
           na.handle = "exclude",
           duplicate.handle = "exclude",
           coord.handle = "exclude",
           date.handle = "exclude") {

    # Check occurrence data present correct class
    if (!class(occ.data) == "data.frame") {
      stop("occ.data must be of class data.frame")
    }

    # Check column names correct for dynamicSDM functions

    if (!"day" %in% colnames(occ.data)) {
      stop("day column not found. Ensure record day col is named 'day'")
    }

    if (!"month" %in% colnames(occ.data)) {
      stop("month column not found. Ensure record month col is named 'month'")
    }

    if (!"year" %in% colnames(occ.data)) {
      stop("year column not found. Ensure record year col is named 'year'")
    }

    if (!"x" %in% colnames(occ.data)) {
      stop("x column not found. Ensure record longitude is named 'x'")
    }

    if (!"y" %in% colnames(occ.data)) {
      stop("y column not found. Ensure record latitude is named 'y'")
    }


    # Check column classes are correct for dynamicSDM functions

    if (!class(occ.data$year) == "numeric") {
      stop("year must be of class numeric")
    }

    if (!class(occ.data$month) == "numeric") {
      stop("month must be of class numeric")
    }

    if (!class(occ.data$day) == "numeric") {
      stop("day must be of class numeric")
    }

    if (!class(occ.data$x) == "numeric") {
      stop("x must be of class numeric")
    }

    if (!class(occ.data$y) == "numeric") {
      stop("y must be of class numeric")
    }


    ## check for NAs in record co-ordinates or dates - then exclude or ignore

    if (!missing(na.handle)) {

      na.handle <- match.arg(arg = na.handle, choices = c("exclude", "ignore"))

      if (na.handle == "exclude") {
        occ.data <- occ.data[!is.na(occ.data[, "y"]), ]
        occ.data <- occ.data[!is.na(occ.data[, "x"]), ]

        message("omitting any species records with coordinates containing NA")

        occ.data <- occ.data[!is.na(occ.data[, "day"]), ]
        occ.data <- occ.data[!is.na(occ.data[, "month"]), ]
        occ.data <- occ.data[!is.na(occ.data[, "year"]), ]

        message("omitting any species records with dates containing NA")
      }
    }


    # Check for duplicate values in record co-ordinates and dates

    if (!missing(duplicate.handle)) {
      duplicate.handle <- match.arg(arg = duplicate.handle,
                                    choices = c("exclude", "ignore"))

      if (duplicate.handle == "exclude") {

        occ.data <- unique(occ.data[, c("year", "month", "day", "x", "y")])

        message("omitting any duplicate records")
      }
    }


    # Check co-ordinate validity

    if (!missing(coord.handle)) {
      coord.handle <- match.arg(arg = coord.handle,
                                choices = c("exclude", "ignore"))

      if (coord.handle == "exclude") {
        # Check longitude within acceptable bounds
        occ.data <- occ.data[which(occ.data[, "x"] >= (-180)), ]
        occ.data <- occ.data[which(occ.data[, "x"] <= (180)), ]
        # Check latitude within acceptable bounds
        occ.data <- occ.data[which(occ.data[, "y"] >= (-90)), ]
        occ.data <- occ.data[which(occ.data[, "y"] <= (90)), ]

        message("any records with invalid co-ordinates excluded")
      }
    }


    # Check date validity

    if (!missing(date.handle)) {
      date.handle <- match.arg(arg = date.handle,
                               choices = c("exclude", "ignore"))

      # Remove any dates that return NA when "Date" objects made from them
      if (date.handle == "exclude") {
        occ.data <- occ.data[-c(which(is.na(as.character(
          as.Date(with(
            occ.data, paste(year, month, day, sep = "-")
          ), "%Y-%m-%d")
        )))), ]

        message("any records with invalid dates excluded")
      }
    }

    return(occ.data)
  }
