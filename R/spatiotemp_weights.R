#' Calculate sampling effort across spatial and temporal buffer from species occurrence records
#'
#' Calculates the total number of sampling events across a given spatial and temporal buffer from each occurrence record’s co-ordinate and date.
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".
#' @param sampling.events.df a data.frame, sampling events with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".
#' @param spatial.dist a numeric value, the spatial distance in metres representing the radius from occurrence record co-ordinate to sum sampling events across.
#' @param temporal.dist a numeric value, the temporal distance in days, representing the period before and after the occurrence record date to sum sampling events across.
#' @details For each occurrence record, this function calculates the total number of sampling events within given radius (spatial.dist) from each record co-ordinate and days (temporal.dist) both prior and post record date.
#'
#'In addition to total sampling events, the function also calculates relative sampling effort, scaling from 0 (least sampled) to 1 (most sampled).
#'
#'Output could be used as model weights to correct spatial and temporal biases in occurrence record collections.
#' @return Returns input occurrence record data frame with additional columns for sampling effort "SAMP_EFFORT" and relative sampling effort "REL_SAMP_EFFORT".
#' @examples
#' data("sample_occ_abs_data",package="dynamicSDM")
#' data("sample_surveyeffort",package="dynamicSDM")
#' spatiotemp_weights(occ.data = sample_occ_abs_data,sampling.events.df = sample_surveyeffort,spatial.dist = 200000,temporal.dist = 20)
#'@export
#'
spatiotemp_weights <-  function(occ.data,
                                sampling.events.df,
                                spatial.dist = NULL,
                                temporal.dist = NULL) {

# Check formatting of sampling events data frame provided
  if (!"day" %in% colnames(sampling.events.df)) {
    stop("sampling.events.df day column not found.")
  }

  if (!"month" %in% colnames(sampling.events.df)) {
    stop("sampling.events.df month column not found.")
  }
  if (!"year" %in% colnames(sampling.events.df)) {
    stop("sampling.events.df year column not found.")
  }
  if (!"x" %in% colnames(sampling.events.df)) {
    stop("sampling.events.df x column not found.")
  }
  if (!"y" %in% colnames(sampling.events.df)) {
    stop("sampling.events.df y column not found.")
  }

  # check column classes correct

  if (!is.numeric(sampling.events.df$year)) {
    stop("sampling.events.df year must be of class numeric")
  }
  if (!is.numeric(sampling.events.df$month)) {
    stop("sampling.events.df month must be of class numeric")
  }
  if (!is.numeric(sampling.events.df$day)) {
    stop("sampling.events.df day must be of class numeric")
  }
  if (!is.numeric(sampling.events.df$x)) {
    stop("sampling.events.df x must be of class numeric")
  }
  if (!is.numeric(sampling.events.df$y)) {
    stop("sampling.events.df y must be of class numeric")
  }

  # Check formatting of spatial.dist argument

  if (!is.numeric(spatial.dist)) {
    stop("spatial.dist must be numeric")
  }

  if (!length(spatial.dist) == 1) {
    stop("spatial.dist must be length  1")
  }

  # Check formatting of temporal.dist argument

  if (!is.numeric(temporal.dist)) {
    stop("temporal.dist must be numeric")
  }
  if (!length(temporal.dist) == 1) {
    stop("temporal.dist must be length 1")
  }

  # Create empty vector to bind extracted sampling effort values to
  SAMP_EFFORT = NULL

  # Create polygon of area surrounding co-ordinates to extract sampling effort

  surroundingarea_points <- rangemap::geobuffer_points(occ.data[, c("x", "y")],
                                                       radius = spatial.dist,
                                                       by_point = T)

  for (x in 1:nrow(occ.data)) {
    # Get the earliest date to include in the sampling effort calculation
    date1 <-
      as.Date(with(occ.data, paste(year, month, day, sep = "-")), "%Y-%m-%d")[x]
    date1 <- date1 - temporal.dist

    # Get the latest date to include in the sampling effort calculation
    date2 <-
      as.Date(with(occ.data, paste(year, month, day, sep = "-")), "%Y-%m-%d")[x]
    date2 <- date2 + temporal.dist

    # Convert data frame columns to Date objects
    samplingeffortdates <-
      as.Date(with(sampling.events.df,
                   paste(year, month, day, sep = "-")), "%Y-%m-%d")

    # Extract sampling events between earliest and latest date
    samplingefforttemp <-
      as.data.frame(sampling.events.df[samplingeffortdates >= date1 &
                                         samplingeffortdates <= date2,])

    # Select polygon of radius for this co-ordinate
    surroundingarea_point <- surroundingarea_points[x]

    # Extract co-ordinates for buffer area
    xmin <- sp::bbox(raster::extent(surroundingarea_point))[1, 1]
    xmax <- sp::bbox(raster::extent(surroundingarea_point))[1, 2]
    ymin <- sp::bbox(raster::extent(surroundingarea_point))[2, 1]
    ymax <- sp::bbox(raster::extent(surroundingarea_point))[2, 2]

    # Filter sampling events to include only records within spatial limits
    samplingefforttemp <-
      as.data.frame(samplingefforttemp[samplingefforttemp$x <= xmax &
                                         samplingefforttemp$x >= xmin, ])

    samplingefforttemp <-
      as.data.frame(samplingefforttemp[samplingefforttemp$y <= ymax &
                                         samplingefforttemp$y >= ymin,])

    SAMP_EFFORT <- rbind(SAMP_EFFORT, nrow(samplingefforttemp)) # Bind result
  }

  # Calculate relative sampling effort
  REL_SAMP_EFFORT <- SAMP_EFFORT / (sum(SAMP_EFFORT, na.rm = T))

  return(cbind(occ.data, SAMP_EFFORT, REL_SAMP_EFFORT))
}
