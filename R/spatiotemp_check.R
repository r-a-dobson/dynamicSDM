#'Check species occurrence record formatting, completeness and validity.
#'
#'Checks the occurrence record data frame contains the column names and classes required for
#'dynamicSDM functions. Option to exclude records containing missing, duplicate or invalid
#'co-ordinates or dates.
#'
#'@param occ.data a data frame, with columns for occurrence record co-ordinates and dates with
#'  column names as follows; record longitude as "x", latitude as "y", year as "year", month as
#'  "month", and day as "day".
#'@param na.handle a character string, method for handling missing data (NA values) in record
#'  co-ordinates and dates. One of `exclude` or `ignore`: can be abbreviated. Default; `exclude`.
#'@param duplicate.handle a character string, method for handling duplicate record co-ordinates or
#'  dates. One of `exclude` or `ignore`: can be abbreviated. Default; `exclude`.
#'@param coord.handle a character string, method for handling invalid co-ordinates in record data.
#'  One of `exclude` or `ignore`: can be abbreviated. Default; `exclude`.
#'@param date.handle a character string, method for handling invalid dates in record data. One of
#'  `exclude` or `ignore`: can be abbreviated. Default; `exclude`.
#'@param date.res a character string, stating the temporal resolution to exclude dates by. One of
#'  `year`, `month` or `day`.
#' @param coordclean a logical indicating whether to run function
#'  `clean_coordinates` from package `CoordinateCleaner` on `occ.data`. Default = FALSE.
#'@param coordclean.species a character string or vector, specifying the name of the species that
#'  all of `occ.data` records belong to, or a character vector the length of `nrow(occ.data)`
#'  specifying which species each record belongs to. Required if `coordclean` = TRUE.
#'@param coordclean.handle a character string, method for handling records flagged by
#'  `CoordinateCleaner`. One of `exclude` or `report`. Default: exclude.
#'@param ... Other arguments passed onto `CoordinateCleaner`.
#'@details
#'
#'Record dates and co-ordinates are checked for validity using the following rules:
#'
#'* Dates must be real dates that could exist. For example, 50th February 2000 is not a valid date.
#'
#'* Co-ordinates must have longitude (x) values between -180 and 180, and latitude (y) values
#'between -90 and 90 to be considered valid.
#'
#'# `CoordinateCleaner` compatibility
#'
#'`spatiotemp_check()` acts as a helper function for compatibility with the R package
#'`CoordinateCleaner` (Zizka et al., 2019), which offers a diversity of functions for checking the
#'co-ordinates of occurrence records.
#'
#'If `coordclean` = T, then `coordclean.species` must be provided to identify which species each
#'record belonds to. If `coordclean.handle` = `exclude` then all `occ.data` records flagged by
#'`CoordinateCleaner::clean_coordinates()` as potentially erroneous are removed in the returned
#'data. If  `coordclean.handle` = `report`, then the in-built report output by
#'`CoordinateCleaner::clean_coordinates()`is returned. This contains logicals specifying the
#'potentially erroneous records.
#'@references Zizka A, Silvestro D, Andermann T, Azevedo J, Duarte Ritter C, Edler D, Farooq H,
#'Herdean A, Ariza M, Scharn R, Svanteson S, Wengstrom N, Zizka V, Antonelli A (2019).
#'“CoordinateCleaner: standardized cleaning of occurrence records from biological collection
#'databases.” Methods in Ecology and Evolution, -7. \doi{10.1111/2041-210X.13152}, R package version
#'2.0-20, <https://github.com/ropensci/CoordinateCleaner>.
#'@return By default, returns occurrence record data frame, filtered to exclude records containing
#'  missing, duplicate or invalid data in record co-ordinates and dates.
#' @examples
#'data(sample_occ_data)
#'sample_occ_data<-convert_gbif(sample_occ_data)
#'
#'nrow(sample_occ_data)
#'
#' filtered<-spatiotemp_check(
#'  occ.data = sample_occ_data,
#'  coord.handle = "exclude",
#'  date.handle = "exclude",
#'  duplicate.handle = "exclude",
#'  na.handle = "exclude"
#')
#'nrow(filtered)
#'
#'\dontrun{
#'filtered_CC<-spatiotemp_check(
#'  occ.data = sample_occ_data,
#'  coord.handle = "exclude",
#'  date.handle = "exclude",
#'  duplicate.handle = "exclude",
#'  na.handle = "exclude",
#'  coordclean = T,
#'  coordclean.species = "quelea",
#'  coordclean.handle = "exclude"
#')
#'nrow(filtered_CC)
#'}
#'
#'@export

spatiotemp_check <- function(occ.data,
                             na.handle,
                             duplicate.handle,
                             coord.handle,
                             date.handle,
                             date.res = "day",
                             coordclean = F,
                             coordclean.species,
                             coordclean.handle="exclude",
                             ...) {

    # Check occurrence data present correct class
    if (!class(occ.data) == "data.frame") {
      stop("occ.data must be of class data.frame")
    }

    # Check column names correct for dynamicSDM functions
    date.res <- match.arg(date.res, choices = c("day", "month", "year"))

    # Depending on date.res only need to check for certain columns
    n <- match(date.res, c("year", "month", "day"))

    if (!"year" %in% colnames(occ.data)) {
      stop("year column not found. Ensure record year col is named 'year'")
    }

    if (n > 1) {
      if (!"month" %in% colnames(occ.data)) {
        stop("month column not found. Ensure record month col is named 'month'")
      }
    }

    if (n > 2) {
      if (!"day" %in% colnames(occ.data)) {
        stop("day column not found. Ensure record day col is named 'day'")
      }
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


    if (n > 1) {
      if (!class(occ.data$month) == "numeric") {
        stop("month must be of class numeric")
      }
    }

    if (n > 2) {
      if (!class(occ.data$day) == "numeric") {
        stop("day must be of class numeric")
      }
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
        occ.data <- occ.data[!is.na(occ.data[, "y"]),]
        occ.data <- occ.data[!is.na(occ.data[, "x"]),]

        message("omitting any species records with coordinates containing NA")

        occ.data <- occ.data[!is.na(occ.data[, "year"]),]

        if (n > 1) { # n was set earlier depending on date resolution of interest
          occ.data <- occ.data[!is.na(occ.data[, "month"]),]
        }

        if (n > 2) {
          occ.data <- occ.data[!is.na(occ.data[, "day"]),]
        }

        message("omitting any species records with dates containing NA")
      }
    }


    # Check for duplicate values in record co-ordinates and dates

    if (!missing(duplicate.handle)) {
      duplicate.handle <- match.arg(arg = duplicate.handle,
                                    choices = c("exclude", "ignore"))

      if (duplicate.handle == "exclude") {
        if (n == 1) {
          occ.data <- occ.data[!duplicated(occ.data[, c("year", "x", "y")]), ]
        }

        if (n == 2) {
          occ.data <- occ.data[!duplicated(occ.data[, c("year", "month", "x", "y")]), ]
        }

        if (n == 3) {
          occ.data <- occ.data[!duplicated(occ.data[, c("year", "month", "day", "x", "y")]), ]
        }

        message("omitting any duplicate records")
      }
    }


    # Check co-ordinate validity

    if (!missing(coord.handle)) {
      coord.handle <- match.arg(arg = coord.handle,
                                choices = c("exclude", "ignore"))

      if (coord.handle == "exclude") {
        # Check longitude within acceptable bounds
        occ.data <- occ.data[which(occ.data[, "x"] >= (-180)),]
        occ.data <- occ.data[which(occ.data[, "x"] <= (180)),]
        # Check latitude within acceptable bounds
        occ.data <- occ.data[which(occ.data[, "y"] >= (-90)),]
        occ.data <- occ.data[which(occ.data[, "y"] <= (90)),]

        message("any records with invalid co-ordinates excluded")
      }
    }


    # Check date validity

    if (!missing(date.handle)) {
      date.handle <- match.arg(arg = date.handle,
                               choices = c("exclude", "ignore"))

      # Remove any dates that return NA when "Date" objects made from them
      if (date.handle == "exclude") {
        if (n == 1) {

          # Set correct month/day so function only identifies error in year column
          month <- rep(1, nrow(occ.data))
          day <- rep(1, nrow(occ.data))

          occ.data <- occ.data[!is.na(as.character(lubridate::as_date(
            paste(occ.data$year, month, day, sep = "-"), "%Y-%m-%d", tz = NULL))), ]
        }

        if (n == 2) {
          day <- rep(1, nrow(occ.data))
          occ.data <- occ.data[!is.na(as.character(lubridate::as_date(
            paste(occ.data$year, occ.data$month, day, sep = "-"), "%Y-%m-%d", tz = NULL))), ]
        }


        if (n == 3) {
          occ.data <-
            occ.data[!is.na(as.character(lubridate::as_date(
              with(occ.data, paste(year, month, day, sep = "-")), "%Y-%m-%d", tz = NULL))), ]
        }


        message("any records with invalid dates excluded")
      }
    }


    # If user wants to use CoordinateCleaner, function requires "species" column. This creates one!
    if (coordclean) {

      if (length(coordclean.species) == 1) {
        occ.data$coordclean.species_col <-
          rep(coordclean.species, nrow(occ.data))
      }

      if (length(coordclean.species) == nrow(occ.data)) {
        occ.data$coordclean.species_col <- coordclean.species
      }

      if (!length(coordclean.species) == 1 &&
          !length(coordclean.species) == nrow(occ.data)) {
        stop("Argument coordclean.species must be of length (1) or equal to nrow (occ.data)")
      }

      report <- CoordinateCleaner::clean_coordinates(occ.data,
                                                     lon = "x",
                                                     lat = "y",
                                                     species = "coordclean.species_col",
                                                     value = "flagged",
                                                     outliers_mtp = NULL,
                                                     ...)

      if (coordclean.handle == "exclude") {
        occ.data <- occ.data[report,]
        occ.data <- occ.data[, -which(names(occ.data) %in% c("coordclean.species_col"))]
      }

      if (coordclean.handle == "report") {
        return(report)
      }
    }

    return(occ.data)
  }

