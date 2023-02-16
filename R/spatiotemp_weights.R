#'Calculate sampling effort across spatial and temporal buffer from species occurrence records
#'
#'Calculates the total number of sampling events across a given spatial and temporal buffer from
#'each occurrence record’s co-ordinate and date.
#'@param occ.data a data frame, with columns for occurrence record co-ordinates and dates with
#'  column names as follows; record longitude as "x", latitude as "y", year as "year", month as
#'  "month", and day as "day".
#'@param samp.events a data.frame, sampling events with column names as follows; record longitude as
#'  "x", latitude as "y", year as "year", month as "month", and day as "day".
#'@param spatial.dist a numeric value, the spatial distance in metres representing the radius from
#'  occurrence record co-ordinate to sum sampling events across.
#'@param temporal.dist a numeric value, the temporal distance in days, representing the period
#'  before and after the occurrence record date to sum sampling events across.
#'@param prj a character string, the coordinate reference system of input `occ.data` co-ordinates
#'  Default is "+proj=longlat +datum=WGS84".
#'@details For each occurrence record, this function calculates the total number of sampling events
#'  within given radius (`spatial.dist`) from each record co-ordinate and days (`temporal.dist`)
#'  both prior and post record date.
#'
#'  In addition to total sampling events, the function also calculates relative sampling effort,
#'  scaling from 0 (least sampled) to 1 (most sampled).
#'
#'  Output could be used to calculate model weights to correct spatial and temporal biases in
#'  occurrence record collections (Stolar and Nielsen, 2015).
#'@references Stolar, J. & Nielsen, S. E. 2015. Accounting For Spatially Biased Sampling Effort In
#'    Presence-Only Species Distribution Modelling. Diversity And Distributions, 21, 595-608.
#'@return Returns input occurrence record data frame with additional columns for sampling effort
#'  "SAMP_EFFORT" and relative sampling effort "REL_SAMP_EFFORT".
#' @examples
#' data("sample_explan_data")
#' data("sample_events_data")
#' \dontshow{
#' sample_explan_data<-sample_explan_data[1:2,]
#' }
#'
#' spatiotemp_weights(
#'  occ.data = sample_explan_data,
#'  samp.events = sample_events_data,
#'  spatial.dist = 200000,
#'  temporal.dist = 20
#')
#'
#'@export
#'

spatiotemp_weights <-  function(occ.data,
                                samp.events,
                                spatial.dist = 0,
                                temporal.dist = 0,
                                prj="+proj=longlat +datum=WGS84") {

  # Check formatting of sampling events data frame provided
  if (!"day" %in% colnames(samp.events)) {
    stop("samp.events day column not found.")
  }

  if (!"month" %in% colnames(samp.events)) {
    stop("samp.events month column not found.")
  }
  if (!"year" %in% colnames(samp.events)) {
    stop("samp.events year column not found.")
  }
  if (!"x" %in% colnames(samp.events)) {
    stop("samp.events x column not found.")
  }
  if (!"y" %in% colnames(samp.events)) {
    stop("samp.events y column not found.")
  }

  # check column classes correct

  if (!is.numeric(samp.events$year)) {
    stop("samp.events year must be of class numeric")
  }
  if (!is.numeric(samp.events$month)) {
    stop("samp.events month must be of class numeric")
  }
  if (!is.numeric(samp.events$day)) {
    stop("samp.events day must be of class numeric")
  }
  if (!is.numeric(samp.events$x)) {
    stop("samp.events x must be of class numeric")
  }
  if (!is.numeric(samp.events$y)) {
    stop("samp.events y must be of class numeric")
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
  year = NULL
  day = NULL
  month = NULL
  # Create polygon of area surrounding co-ordinates to extract sampling effort

  occ.data.dates <- as.Date(with(occ.data, paste(year, month, day, sep = "-")), "%Y-%m-%d")

  occ.data<-occ.data[order(occ.data.dates),]

  occ.data.save<-occ.data
  # To improve function speed complete by unique date chunks

  unique_dates<-unique(occ.data[,c("year","month","day")])

  # Transform points into crs that st_buffer is able to use and in metres (lon-lat unreliable)
  surround <- sf_buffer(occ.data,spatial.dist,prj)

  occ.data$row_numb<-rep(1:nrow(occ.data))

  for (date in 1:nrow(unique_dates)){

    occ.data.split<-occ.data %>% dplyr::filter(year == unique_dates[date, "year"],
                                               month == unique_dates[date, "month"],
                                               day == unique_dates[date, "day"])


    # Get the earliest date to include in the sampling effort calculation
    date1 <- as.Date(with(occ.data.split, paste(year, month, day, sep = "-")), "%Y-%m-%d")[1]
    date1 <- date1 - temporal.dist

    # Get the latest date to include in the sampling effort calculation
    date2 <- as.Date(with(occ.data.split, paste(year, month, day, sep = "-")), "%Y-%m-%d")[1]
    date2 <- date2 + temporal.dist


    # Extract sampling events between earliest and latest date

    sampeff <- as.data.frame(samp.events[samp.events$year >= lubridate::year(date1) &
                                         samp.events$year <= lubridate::year(date2),])

    sampeff <- as.data.frame(sampeff[sampeff$month >= lubridate::month(date1) &
                                         sampeff$month <= lubridate::month(date2),])

    sampeff <- as.data.frame(sampeff[sampeff$day >= lubridate::day(date1) &
                                         sampeff$day <= lubridate::day(date2),])



    for (x in 1:nrow(occ.data.split)) {

      if (occ.data.split[x, "row_numb"] %% 100 == 0) {

        message(paste0("Records completed: ", occ.data.split[x, "row_numb"], "\n"))
      }

      # Select polygon of radius for this co-ordinate
      surround_point <- sf::st_transform(surround$geometry[occ.data.split[x, "row_numb"]],
                                         crs = prj)

      # Extract co-ordinates for buffer area
      xmin <- sf::st_bbox(surround_point)[1]
      xmax <- sf::st_bbox(surround_point)[3]
      ymin <- sf::st_bbox(surround_point)[2]
      ymax <- sf::st_bbox(surround_point)[4]

      # Filter sampling events to include only records within spatial limits
      sampeff2 <- as.data.frame(sampeff[sampeff$x <= xmax & sampeff$x >= xmin,])

      sampeff2 <- as.data.frame(sampeff2[sampeff2$y <= ymax & sampeff2$y >= ymin, ])

      SAMP_EFFORT <- rbind(SAMP_EFFORT, nrow(sampeff2)) # Bind result
    }
  }

  # Calculate relative sampling effort. Add one so each record contributes at least a bit
  REL_SAMP_EFFORT <- (SAMP_EFFORT+1) / (sum(SAMP_EFFORT, na.rm = TRUE))

  return(cbind(occ.data.save, SAMP_EFFORT, REL_SAMP_EFFORT))
}
