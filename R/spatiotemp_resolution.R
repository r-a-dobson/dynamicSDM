#'Filter species occurrence records by given spatial and temporal resolution
#'
#'Filters species occurrence record data frame to exclude records with co-ordinates and dates that
#'do not meet specified spatial and temporal resolution.
#'
#'@param occ.data a data frame, with columns for occurrence record co-ordinates and dates with
#'  column names as follows; record longitude as "x", latitude as "y", year as "year", month as
#'  "month", and day as "day".
#'@param spatial.res optional; a numeric value, the minimum acceptable number of decimal places
#'  given for occurrence record co-ordinates.
#'@param temporal.res optional; a character string, the minimum acceptable temporal resolution of
#'  occurrence record dates. One of `day` , `month` or `year`: can be abbreviated.
#'@details Excludes species occurrence records that do not meet the minimum spatial and temporal
#'  resolution specified.
#'
#'  If `spatial.res` given, the value of 1 represents an acceptable co-ordinate resolution of one
#'  decimal place, roughly equal to 11.1km, and value of 3 represents three decimal places, roughly
#'  equal to 111m.
#'
#'  If `temporal.res` given, `temporal.res = day` would result in exclusion of records without
#'  values for year, month and day, and `temporal.res = year` would only exclude records without
#'  values for year.
#'
#'  `spatial.res` and `temporal.res` can be informed based upon the highest spatial and temporal
#'  resolution of the datasets to be utilised when extracting dynamic variables.
#'
#'  For example, if explanatory variables datasets are annual, then a `temporal.res` of `year` is
#'  adequate, whereas if
#'  datasets are daily, then `temporal.res` of `day` may be more appropriate.
#'@return Returns a data frame of species records filtered by the minimum acceptable spatial
#'  resolution of co-ordinates and temporal resolution of dates.
#' @examples
#'data(sample_occ_data)
#'sample_occ_data <- convert_gbif(sample_occ_data)
#'
#'spatial_res_high <- spatiotemp_resolution(sample_occ_data, spatial.res = 4)
#'
#'spatial_res_low <- spatiotemp_resolution(sample_occ_data, spatial.res = 1)
#'
#'temporal_res <- spatiotemp_resolution(sample_occ_data, temporal.res = "day")
#'@export

spatiotemp_resolution <-  function(occ.data,
                                   spatial.res,
                                   temporal.res) {

  if (!missing(spatial.res)) {

    if (!is.numeric(spatial.res)) {stop("spatial.res should be numeric")}

    # Count number of digits after the decimal places
    res <- nchar(stringr::str_split_fixed(as.character(occ.data$x), "[.]", 2)[, 2])

    # Filter records by given acceptable number of decimal places
    occ.data <- occ.data[res >= spatial.res,]
  }


  if (!missing(temporal.res)) {

    # Match argument given to choices available
    temporal.res <- match.arg(arg = temporal.res, choices = c("day", "month", "year"))

    # If temporal.res is year, exclude records with missing year

    if (temporal.res == "year") {
      occ.data <- occ.data[!is.na(occ.data[, 'year']), ]
    }

    # If temporal.res is month, exclude records with missing year or month

    if (temporal.res == "month") {
      occ.data <- occ.data[!is.na(occ.data[, 'month']),]
      occ.data <- occ.data[!is.na(occ.data[, 'year']),]
    }

    # If temporal.res is day, exclude records with missing year, month and day

    if (temporal.res == "day") {
      occ.data <- occ.data[!is.na(occ.data[, 'month']),]
      occ.data <- occ.data[!is.na(occ.data[, 'year']),]
      occ.data <- occ.data[!is.na(occ.data[, 'day']),]
    }
  }

  return(occ.data)

}
