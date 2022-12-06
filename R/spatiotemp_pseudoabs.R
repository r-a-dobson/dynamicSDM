#' Generate pseudo-absence record coordinates and dates
#'
#' Function generates specified number of pseudo-absence record co-ordinates and dates either randomly or buffered in space and time.
#'
#' @param spatial.method a character string, the spatial method for pseudo-absence generation. One of '"buffer"' or '"random"': can be abbreviated.
#' @param temporal.method a character string, the temporal method for pseudo-absence generation. One of '"buffer"' or '"random"': can be abbreviated.
#' @param occ.data optional; a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day". Required if either temporal.method or spatial.method is "'buffer'".
#' @param spatial.buffer optional; a numeric vector, the distances in metres of radii to generate buffered pseudo-absence coordinates within. Only required if spatial.method is "'buffer'".
#' @param temporal.buffer optional; a numeric vector, the two periods in days to generate buffered pseudo-absence dates within. Only required if temporal.method is "'buffer'".
#' @param n.pseudoabs optional; a numeric value, the number of pseudo-absence records to generate. Default; 100.
#' @param temporal.ext optional; a character vector, two dates in format YYYY-MM-DD. First represents start of temporal extent and second represents end of temporal extent to randomly generate pseudo-absences dates within. Only required if temporal.method is "'random'".
#' @param spatial.ext optional; object of class "Extent", "raster" or "polygon" or numeric vector listing xmin, xmax, ymin and ymax in order, the spatial extent to randomly generate pseudo-absences co-ordinates within.
#' @details If spatial.method is "'buffer'", then occurrence record co-ordinates are randomly generated between the given closest radius and furthest away radius specified in spatial.buffer. For example, if “spatial. buffer = c(3000,10000)”, then pseudo-absence co-ordinates are randomly generated at least 3000m radius away from occurrence record co-ordinate but within 10000m radius.
#'
#'If spatial.method is "'random'", then occurrence record co-ordinates are randomly generated across spatial.ext object given.
#'
#'If temporal.method is "'buffer'", then occurrence record dates are randomly generated between the closest and further away number of days specified in temporal.buffer. For example, if “temporal buffer = c(14,30)”, then pseudo-absence dates randomly generated at least 14 days from occurrence record dates but within 30 days.
#'
#'If temporal.method is "'random'", then occurrence record co-ordinates are randomly generated within the two temporal.ext dates given.
#' @return Returns data frame of pseudo-absence coordinates and dates.
#' @examples
#'data("sample_occ_data")
#'spatiotemp_pseudoabs(
#'  sample_occ_data,
#'  spatial.method = "random",
#'  temporal.method = "random",
#'  spatial.ext = c(20, 36, -35, -12),
#'  temporal.ext = c("2011-01-01", "2017-01-01")
#')
#'@export


spatiotemp_pseudoabs <-  function(spatial.method,
                                  temporal.method,
                                  occ.data = NULL,
                                  spatial.ext = NULL,
                                  temporal.ext = NULL,
                                  spatial.buffer = NULL,
                                  temporal.buffer = NULL,
                                  n.pseudoabs = 100) {

  # Check n.pseudoabs argument is correct class
  if (!is.numeric(n.pseudoabs)) {
    stop("n.pseudoabs should be a numeric value")
  }

  # Match argument to available options
  temporal.method <- match.arg(arg = temporal.method,
                               choices = c("buffer", "random"))

  spatial.method <- match.arg(arg = spatial.method,
                              choices = c("buffer", "random"))

  #------------------------------------------------------------------
  # Random generation of pseudo-absence co-ordinates
  #------------------------------------------------------------------
  if (spatial.method == "random") {

    # Check spatial extent of appropriate class, length and order is provided
    if (missing(spatial.ext)) {
      stop("No spatial.ext specified to generate pseudo-absence co-ordinates")}


    if (!any(class(spatial.ext) == c("numeric",
                                     "Extent",
                                     "RasterLayer",
                                     "Polygon"))) {
      stop("spatial.ext must be class numeric, Extent, RasterLayer or Polygon")
    }

    # Numeric vector to polygon

    if (class(spatial.ext) == "numeric" && length(spatial.ext) == 4) {
      spatial.ext <- as(
        raster::extent(spatial.ext[1],
                       spatial.ext[2],
                       spatial.ext[3],
                       spatial.ext[4]),
        'SpatialPolygons'
      )
    }

    #  Extent object to polygon
    if (class(spatial.ext) == "Extent") {
      spatial.ext <- as(spatial.ext, 'SpatialPolygons')
    }

    # RasterLayer object to polygon
    if (class(spatial.ext) == "RasterLayer") {
      spatial.ext <- as(raster::extent(spatial.ext), 'SpatialPolygons')
    }

    # Randomly generate pseudo-absence co-ordinates within extent polygon
    PA_coords <-
      as.data.frame(sp::spsample(
        spatial.ext ,
        type = 'random',
        n = n.pseudoabs,
        iter = 30
      ))
  }


  #------------------------------------------------------------------
  # Random generation of pseudo-absence dates
  #------------------------------------------------------------------

  if (temporal.method == "random") {

    # Check that if temporal random chosen, temporal extent appropriate

    if (missing(temporal.ext)) {
      stop("No temporal.ext specified to randomly generate pseudo-absence dates within.")
    }

    if (!class(temporal.ext) == "character") {
      stop("temporal.ext must be character vector of length 2")
    }

    if (!length(temporal.ext) == 2) {
      stop("two dates must be provides for temporal extent")
    }

    # Check dates are valid
    tryCatch({
      dates <- as.Date(temporal.ext)
    }, error = function(e) {
      stop("Both dates invalid given in temporal.ext. Ensure format YYYY-MM-DD")
    })

    if (any(is.na(dates))) {
      stop("Invalid date given in temporal.ext. Ensure format YYYY-MM-DD")
    }

    firstdate <- as.Date(temporal.ext)[1]

    seconddate <- as.Date(temporal.ext)[2]

    # Check dates are in order

    if (firstdate - seconddate > 0) {
      stop("First date must be before second date")
    }

    # Randomly generate dates within given extent
    PA_dates <-
      firstdate + sample(1:(seconddate - firstdate),
                         size = n.pseudoabs,
                         replace = T)

    # Split dates into year, month and day columns for returned dataframe
    PA_dates <-
      tidyr::separate(as.data.frame(PA_dates),
                      "PA_dates",
                      c("year", "month", "day"),
                      sep = "-")
  }




  #------------------------------------------------------------------
  # Buffered generation of pseudo-absence co-ordinates
  #------------------------------------------------------------------

  if (spatial.method == "buffer") {

    # Check that if spatial buffer chosen, a temporal buffer of appropriate class, length and order is provided
    if (missing(spatial.buffer)) {
      stop("No spatial.buffer to generate pseudo-absence co-ordinates within.")
    }

    if (!is.numeric(spatial.buffer)) {
      stop("spatial.buffer must be numeric")
    }

    if (!length(spatial.buffer) == 2) {
      stop("spatial.buffer must be length 2.")
    }

    if (spatial.buffer[1] - spatial.buffer[2] > 0) {
      stop("Second spatial.buffer must be further away than first")
    }


    # Calculate the number of pseudo-absences to generate in buffer
    # from each occurrence record to meet or slightly exceed the required amount
    value <- ceiling(n.pseudoabs / nrow(occ.data))

    # Create buffer shapefiles for given buffer min and max size from occ coords

    first.buff <- rangemap::geobuffer_points(occ.data[, c("x", "y")],
                                             radius = spatial.buffer[1],
                                             by_point = T)

    second.buff <- rangemap::geobuffer_points(occ.data[, c("x", "y")],
                                              radius = spatial.buffer[2],
                                              by_point = T)

    raster::crs(second.buff) <- NA
    raster::crs(first.buff) <- NA

    PA_coords = NULL # Create vector for pseudoabsence co-ordinates

    for (x in 1:nrow(occ.data)) {
      PA_coords <- rbind(PA_coords, sp::coordinates(
        sp::spsample(
          rgeos::gDifference(second.buff[x], first.buff[x]),
          type = 'random',
          n = value,
          iter = 30
        )
      ))
    }

    colnames(PA_coords) <- c("x", "y")
  }


  #------------------------------------------------------------------
  # Buffered generation of pseudo-absence dates
  #------------------------------------------------------------------

  if (temporal.method == "buffer") {

    # Check temporal buffer of appropriate class, length and order is provided

    if (missing(temporal.buffer)) {
      stop("No temporal.buffer specified specified to generate pseudo-absence dates within.")
    }

    if (!class(temporal.buffer) == "numeric") {
      stop("temporal.buffer must be numeric")
    }

    if (!length(temporal.buffer) == 2) {
      stop("temporal.buffer must be length(2) representing the buffer to generate coords in")
    }

    if (temporal.buffer[1] - temporal.buffer[2] > 0) {
      stop("Second temporal.buffer must be higher than first")
    }

    # Calculate number of pseudo-absences to generate in buffer from each
    # occurrence record to meet or slightly exceed the required amount
    value <- ceiling(n.pseudoabs / nrow(occ.data))

    # Generate list of max temporal buffer distance away from each record date
    date1 <- as.Date(with(occ.data, paste(year, month, day, sep = "-")), "%Y-%m-%d")
    date1 <- date1 + temporal.buffer[2]

    # Create "date" vector for binding pseudoabsence dates too
    PA_dates = date1[1]


    # For each record date, randomly select the number of dates  within buffer
    for (x in 1:length(date1)) {
      PA_dates <- c(PA_dates, as.Date(date1[x] - sample(
        c(0:temporal.buffer[1], 0:(-temporal.buffer[1])), value, replace = T
      )))
    }


    # Remove first one as used to set vector class as "Date"
    PA_dates <- PA_dates[2:length(PA_dates)]
    PA_dates <- as.data.frame(PA_dates)
    PA_dates <- tidyr::separate(PA_dates,
                                       "PA_dates",
                                       c("year", "month", "day"),
                                       sep = "-")
  }


  # If either "buffer" is chosen, there may be slightly more generated than
  # specified by n.pseudoabs, so randomly select this amount from generated

  # Keeps co-ordinates and dates relevant to same occurrence record together before randomly selecting
  if (temporal.method == "buffer" && spatial.method == "buffer") {
    pseudo.df <- dplyr::sample_n(as.data.frame(cbind(PA_coords, PA_dates)),
                      n.pseudoabs)
  }

  if (temporal.method == "buffer" && spatial.method == "random") {
    PA_dates <- dplyr::sample_n(as.data.frame(PA_dates), n.pseudoabs)
    pseudo.df <- as.data.frame(cbind(PA_coords, PA_dates))
  }

  if (temporal.method == "random" && spatial.method == "buffer") {
    PA_coords <- dplyr::sample_n(as.data.frame(PA_coords), n.pseudoabs)
    pseudo.df <- as.data.frame(cbind(PA_coords, PA_dates))
  }

  if (temporal.method == "random" && spatial.method == "random") {
    pseudo.df <- as.data.frame(cbind(PA_coords, PA_dates))
  }

  return(pseudo.df)
}
