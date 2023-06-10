#'Generate pseudo-absence record coordinates and dates
#'
#'Function generates specified number of pseudo-absence record co-ordinates and dates either
#'randomly or buffered in space and time.
#'
#'@param spatial.method a character string, the spatial method for pseudo-absence generation. One of
#'  `buffer` or `random`: can be abbreviated.
#'@param temporal.method a character string, the temporal method for pseudo-absence generation. One
#'  of `buffer` or `random`: can be abbreviated.
#'@param occ.data optional; a data frame, with columns for occurrence record co-ordinates and dates
#'  with column names as follows; record longitude as "x", latitude as "y", year as "year", month as
#'  "month", and day as "day". Required if either `temporal.method` or `spatial.method` is `buffer`.
#'@param spatial.buffer optional; a numeric value or vector, the radius/radii in metres to generate
#'  buffered pseudo-absence coordinates within. Only required if spatial.method is `buffer`. See
#'  details.
#'@param temporal.buffer optional; a numeric value or vector, the period(s) in days to generate
#'  buffered pseudo-absence dates within. Only required if temporal.method is `buffer` . See
#'  details.
#'@param n.pseudoabs optional; a numeric value, the number of pseudo-absence records to generate.
#'  Default; 100.
#'@param temporal.ext optional; a character vector, two dates in format "YYYY-MM-DD". The first
#'  represents the start of the temporal extent and the second represents the end of temporal extent
#'  to randomly generate pseudo-absences dates within. Required if `temporal.method` is `random`,
#'  and optionally used if `buffer`. See details.
#'@param spatial.ext the spatial extent to randomly generate pseudo-absences
#'  within. Object from which extent can be extracted of class `SpatExtent`,
#'  `SpatRaster`, an `sf` polygon or a numeric vector listing xmin, xmax, ymin
#'  and ymax in order. Required if `spatial.method` is `random`, and optionally
#'  used if `buffer`. See details.
#'@param prj a character string, the coordinate reference system of input `occ.data` co-ordinates.
#'  Default is "+proj=longlat +datum=WGS84".
#'@details
#'
#'Below we outline the various approaches to generating pseudo-absences through space and time
#'available in the dynamicSDM package. To select the appropriate pseudo-absence generation
#'approach and buffer size, there are many considerations. We recommend seeking the appropritae
#'literature to inform your decision when species distribution modelling (Barbet-Massin et al.,
#'2012, Phillips et al., 2009, Vanderwal et al., 2009).
#'
#'# Spatial buffer
#'
#'If `spatial.method` is `buffer`, then the pseudo-absence record co-ordinates are randomly
#'generated in a buffered area defined either by
#'
#'* single numeric value for `spatial.buffer` - anywhere between the occurrence record and the
#'circular distance surrounding this point (as specified in metres).
#'
#'* two numeric values for `spatial.buffer` - anywhere between the closest radius from the
#'occurrence record and the furthest away radius (as specified in metres).
#'
#'For example, if `spatial.buffer = c(3000,10000)`, then pseudo-absence co-ordinates are randomly
#'generated at least 3000m radius away from occurrence record co-ordinate but within 10000m radius.
#'Whereas, if `spatial.buffer = 10000`, then pseudo-absence co-ordinates are randomly generated
#'anywhere between 0m and 10000m radius from the occurrence record.
#'
#'If `spatial.ext` is given too, then the generated pseudo-absences are not only constrained to the
#'buffered area but also to this extent. For instance, if occurrence records are coastal, you may
#'want to clip buffers to only terrestrial regions using a country polygon given in `spatial.ext`.
#'
#'# Spatial random
#'
#'If `spatial.method` is `random`, then the pseudo-absence record co-ordinates are randomly
#'generated across `spatial.ext` object given.
#'
#'If `spatial.ext` is a `sf` polygon or `SpatRaster` (mask if possible before
#'input) then these shapes are used, instead of a simple rectangular
#'extent (`SpatExtent`). Therefore, inputting one of these objects will allow for more specific pseudo-absence
#'generation.
#'
#'For example, inputting an `sf` polygon of a specific countries will ensure co-ordinates
#'are terrestrial, whereas an extent (xmin, xmax, ymin, ymax) that encompasses these countries may
#'result in the generation of pseudo-absence records in inappropriate areas, such as oceans or
#'non-study-area countries.
#'
#'# Temporal buffer
#'
#'If `temporal.method` is `buffer`, then pseudo-absence record dates are randomly generated between
#'in a period defined by:
#'
#'* single numeric value for temporal.buffer - any date between the occurrence record date and the
#'total number of days specified prior or post.
#'
#'* two numeric values for temporal.buffer - any date between the closest and furthers away number
#'of days specified.
#'
#'For example, if `temporal.buffer = c(14,30)`, then pseudo-absence dates randomly generated at
#'least 14 days from occurrence record dates but within 30 days. Whereas if `temporal.buffer = 30`,
#'pseudo-absence dates are randomly generated anywhere between 0 and 30 days prior or post the
#'occurrence record date.
#'
#'If `temporal.ext` is given too, then the generated pseudo-absence dates are not only constrained
#'to the buffer period but also to this temporal extent. For instance, an occurrence record recorded
#'at the start of `temporal.ext` with 7 day buffer, may result in generated pseudo-absences outside
#'of the temporal extent of the study.
#'
#'# Temporal random
#'
#'If `temporal.method` is `random`, then pseudo-absence record dates are randomly
#'generated within the two `temporal.ext` dates given.
#'
#'
#'@references
#'Barbet-Massin, M., Jiguet, F., Albert, C. H., Thuiller, W. J. M. I. E. & Evolution 2012. Selecting
#'Pseudo-Absences For Species Distribution Models: How, Where And How Many? 3, 327-338.
#'
#'Phillips, S. J., Dudik, M., Elith, J., Graham, C. H., Lehmann, A., Leathwick, J. & Ferrier, S.
#'2009. Sample Selection Bias And Presence-Only Distribution Models: Implications For Background And
#'Pseudo-Absence Data. 19, 181-197.
#'
#'Vanderwal, J., Shoo, L. P., Graham, C. & Williams, S. E. 2009. Selecting Pseudo-Absence Data For
#'Presence-Only Distribution Modeling: How Far Should You Stray From What You Know? Ecological
#'Modelling, 220, 589-594.

#'@return Returns data frame of pseudo-absence coordinates and dates.
#' @examples
#'data("sample_filt_data")
#'\donttest{
#'
#'spatiotemp_pseudoabs(
#'  sample_filt_data,
#'  spatial.method = "random",
#'  temporal.method = "random",
#'  spatial.ext = c(20, 36, -35, -12),
#'  temporal.ext = c("2011-01-01", "2017-01-01")
#')
#'}
#'@export


spatiotemp_pseudoabs <-  function(spatial.method,
                                  temporal.method,
                                  occ.data,
                                  spatial.ext,
                                  temporal.ext ,
                                  spatial.buffer,
                                  temporal.buffer,
                                  n.pseudoabs = 100,
                                  prj = "+proj=longlat +datum=WGS84") {


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

    spatial.ext<-convert_to_sf(spatial.ext,prj)

    # Randomly generate pseudo-absence co-ordinates within extent
    # Transforms to a CRS st_sample() can use, then transforms back.
    PA_coords <- as.data.frame(sf::st_coordinates(sf::st_transform(
      sf::st_sample(
        sf::st_transform(spatial.ext , 27700),
        type = 'random',
        size = n.pseudoabs,
        iter = 30
      ), prj)))

    colnames(PA_coords)<-c("x","y")

     }

  #------------------------------------------------------------------
  # Random generation of pseudo-absence dates
  #------------------------------------------------------------------

  if (temporal.method == "random") {

    # Check that if temporal random chosen, temporal extent appropriate

    if (missing(temporal.ext)) {
      stop("No temporal.ext specified to randomly generate pseudo-absence dates within.")
    }

    if (!inherits(temporal.ext, "character")) {
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
    PA_dates <- firstdate + sample(1:(seconddate - firstdate),
                                   size = n.pseudoabs,
                                   replace = TRUE)

    # Split dates into year, month and day columns for returned dataframe
    PA_dates <- tidyr::separate(as.data.frame(PA_dates),
                                "PA_dates",
                                c("year", "month", "day"),
                                sep = "-")

    PA_dates[1:3] <- sapply(PA_dates[1:3],as.numeric)
  }




  #------------------------------------------------------------------
  # Buffered generation of pseudo-absence co-ordinates
  #------------------------------------------------------------------

  if (spatial.method == "buffer") {

    # Check that if spatial buffer chosen, a temporal buffer of appropriate class, length and order
    # is provided
    if (missing(spatial.buffer)) {
      stop("No spatial.buffer to generate pseudo-absence co-ordinates within.")
    }

    if (!is.numeric(spatial.buffer)) {
      stop("spatial.buffer must be numeric")
    }

    if (length(spatial.buffer) > 2) {
      stop("spatial.buffer must be of length 1 or 2.")
    }

    if (length(spatial.buffer) == 1) {
      spatial.buffer<-c(0.00000001,spatial.buffer)
    }

    if(length(spatial.buffer)==2){
    if (spatial.buffer[1] - spatial.buffer[2] > 0) {
      stop("Second spatial.buffer must be further away than first")
    }}


    # Calculate the number of pseudo-absences to generate in buffer
    # from each occurrence record to meet or slightly exceed the required amount
    value <- ceiling(n.pseudoabs / nrow(occ.data))

    # Create buffer shapefiles for given buffer min and max size from occ coords

    first.buff <- sf_buffer(occ.data, spatial.buffer[1],prj)

    second.buff <- sf_buffer(occ.data, spatial.buffer[2],prj)


    PA_coords = NULL # Create vector for pseudoabsence co-ordinates

    if (!missing(spatial.ext)) {

      # Convert to sf object
      spatial.ext <- convert_to_sf(spatial.ext, prj)

      # Transform to same CRS as buffers
      spatial.ext <-  sf::st_transform(spatial.ext, 7801)

      # Work around: st_intersection returns object in prj 7801 but sets the crs as NA.
      # For st_difference to work between these outputs, need to also set extent crs as NA
      sf::st_crs(spatial.ext) <- NA }


    for (x in 1:nrow(occ.data)) {

      if(!missing(spatial.ext)){

        # Extract area where buffers overlap
        buffer_difference <- sf::st_difference(second.buff$geometry[[x]],
                                               first.buff$geometry[[x]],
                                               crs = 7801)

        # Intersect - remove any buffered area not intersecting with spatial.ext
        cropped_buffer <- sf::st_intersection(buffer_difference, spatial.ext)

        if(length(cropped_buffer) == 0){
          stop("For at least one occurrence, buffer does not intersect spatial.ext")}

        samp <- sf::st_sample(
          cropped_buffer,
          type = 'random',
          size = value

        )}


      if(missing(spatial.ext)){
      # Sample co-ordinates within area that buffers intersect
      samp <- sf::st_sample(
        sf::st_difference(second.buff$geometry[[x]], first.buff$geometry[[x]]),
        type = 'random',
        size = value
      )}

      sf::st_crs(samp) <- 7801

      # Transform co-ordinates back to original data CRS
      PA_coords <- rbind(PA_coords, sf::st_coordinates(sf::st_transform(samp, crs = prj)))

    }

    PA_coords <- data.frame(PA_coords, row.names = NULL)
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

    if (!inherits(temporal.buffer, "numeric")) {
      stop("temporal.buffer must be numeric")
    }

    if (length(temporal.buffer) > 2) {
      stop("temporal.buffer must be length 1 or 2 representing the buffer(s) to generate dates in")
    }

    if (length(temporal.buffer) == 1) {
      temporal.buffer<-c(0,temporal.buffer)
    }

    if (temporal.buffer[1] - temporal.buffer[2] > 0) {
      stop("Second temporal.buffer must be higher than first")
    }

    # Calculate number of pseudo-absences to generate in buffer from each
    # occurrence record to meet or slightly exceed the required amount
    value <- ceiling(n.pseudoabs / nrow(occ.data))


    date1 <- as.Date(with(occ.data, paste(year, month, day, sep = "-")), "%Y-%m-%d")

    # Generate dates of max temporal buffer distance away for each record date

    date2 <- date1 + temporal.buffer[2]
    date3 <- date1 - temporal.buffer[2]

    # If temporal.ext given, ensure maximum dates are within these extents.
    if (!missing(temporal.ext)) {
      firstdate <- as.Date(temporal.ext)[1]

      if (any(as.numeric(date3 - firstdate) < 0)) {
        date3[(as.numeric(date3 - firstdate) < 0)] <- firstdate
      }

      seconddate <- as.Date(temporal.ext)[2]

      if (any(as.numeric(date2 - seconddate) > 0)) {
        date2[(as.numeric(date2 - seconddate) > 0)] <- seconddate
      }

    }

    date4 <- date1 + temporal.buffer[1]
    date5 <- date1 - temporal.buffer[1]


    # If temporal.ext given, ensure maximum dates are within these extents.
    if (!missing(temporal.ext)) {
      firstdate <- as.Date(temporal.ext)[1]

      if (any(as.numeric(date5 - firstdate) < 0)) {
        date5[(as.numeric(date5 - firstdate) < 0)] <- firstdate
      }

      seconddate <- as.Date(temporal.ext)[2]

      if (any(as.numeric(date4 - seconddate) > 0)) {
        date4[(as.numeric(date4 - seconddate) > 0)] <- seconddate
      }

    }


    # Create "date" vector for binding pseudoabsence dates too
    PA_dates = date1[1]

    # For each record date, randomly select the number of dates  within buffer
    for (x in 1:length(date1)) {
      random_date <- sample(c(dynamic_proj_dates(as.character(date3[x]),
                                                 as.character(date5[x]),
                                                 interval = 1,
                                                 interval.level = "day"),
                              dynamic_proj_dates(as.character(date4[x]),
                                                 as.character(date2[x]),
                                                 interval = 1,
                                                 interval.level = "day")), value, replace = TRUE)

      PA_dates <- c(PA_dates, as.Date(random_date))

    }



    # Remove first one as used to set vector class as "Date"
    PA_dates <- PA_dates[2:length(PA_dates)]
    PA_dates <- as.data.frame(PA_dates)
    PA_dates <- tidyr::separate(PA_dates, "PA_dates", c("year", "month", "day"), sep = "-")
    PA_dates[1:3] <- sapply(PA_dates[1:3],as.numeric)

  }


  # If either "buffer" is chosen, there may be slightly more generated than
  # specified by n.pseudoabs, so randomly select this amount from generated

  # Keeps co-ordinates and dates relevant to same occurrence record together before randomly
  # selecting
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
