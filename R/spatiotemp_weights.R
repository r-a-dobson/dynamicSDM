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
#' @example
#' Read in occurrence record data frame
#' data("sample_occ_abs_data",package="dynamicSDM")
#'
#' Read in sampling event data frame
#' data("sample_surveyeffort",package="dynamicSDM")
#'
#' Take sample of occurrence record data frame for example
#' sample_occ_abs_data<-dplyr::sample_n(sample_occ_abs_data,4)
#'
#' spatiotemp_weights(occ.data = sample_occ_abs_data,sampling.events.df = sample_surveyeffort,spatial.dist = 200000,temporal.dist = 20)
#'
#'          x         y year month day presence.absence individualCount SAMP_EFFORT REL_SAMP_EFFORT
#' 1 28.37458 -25.62458 2015     2   8                1              NA          18       0.6923077
#' 2 27.53590 -29.51060 2016     7  22                0               0           0       0.0000000
#' 3 30.43040 -26.99470 2014     7   2                0               0           8       0.3076923
#' 4 26.20792 -29.04125 2012     2  23                1              NA           0       0.0000000

spatiotemp_weights<-function(occ.data,sampling.events.df, spatial.dist=NA,temporal.dist=NA){

  # Check formatting of sampling events data frame provided
  if(!"day" %in% colnames(sampling.events.df)){stop("sampling.events.df day column not found. Ensure sampling.event.df day column is named day")}
  if(!"month" %in% colnames(sampling.events.df)){stop("sampling.events.df month column not found. Ensure sampling.event.df month column is named month")}
  if(!"year" %in% colnames(sampling.events.df)){stop("sampling.events.df year column not found. Ensure sampling.event.df year column is named year")}
  if(!"x" %in% colnames(sampling.events.df)){stop("sampling.events.df x column not found. Ensure sampling.event.df longitude column is named x")}
  if(!"y" %in% colnames(sampling.events.df)){stop("sampling.events.df y column not found. Ensure sampling.event.df latitude column is named y")}

  ### check column classes correct
  if (!class(sampling.events.df$year)=="numeric"){stop("sampling.events.df year must be of class numeric")}
  if (!class(sampling.events.df$month)=="numeric"){stop("sampling.events.df month must be of class numeric")}
  if (!class(sampling.events.df$day)=="numeric"){stop("sampling.events.df day must be of class numeric")}
  if (!class(sampling.events.df$x)=="numeric"){stop("sampling.events.df x must be of class numeric")}
  if (!class(sampling.events.df$y)=="numeric"){stop("sampling.events.df y must be of class numeric")}

  # Check formatting of spatial.dist argument
  if(!class(spatial.dist)=="numeric"){stop("spatial.dist must be numeric")}
  if(!length(spatial.dist)==1){stop("spatial.dist must be length  1 representing the distance from record to sum sampling effort across")}

  # Check formatting of temporal.dist argument
  if(!class(temporal.dist)=="numeric"){stop("temporal.dist must be numeric")}
  if(!length(temporal.dist)==1){stop("temporal.dist must be length 1 representing the temporal distance from record date to sum sampling effort across")}


  SAMP_EFFORT=NULL  ## Create empty vector to bind extracted sampling effort values to

  for(x in 1:nrow(occ.data)){

  date1<-as.Date(with(occ.data, paste(year, month, day,sep="-")), "%Y-%m-%d")[x] - temporal.dist ## Get the earliest date to include in the sampling effort calculation
  date2<-as.Date(with(occ.data, paste(year, month, day,sep="-")), "%Y-%m-%d")[x] + temporal.dist  ## Get the latest date to include in the sampling effort calculation

  samplingeffortdates<-as.Date(with(sampling.events.df, paste(year, month, day,sep="-")), "%Y-%m-%d")  ## Convert data frame columns to Date objects

  samplingefforttemp <-as.data.frame(sampling.events.df[samplingeffortdates >= date1 &    # Extract sampling events between earliest and latest date
               samplingeffortdates <= date2, ])

  surroundingarea<-geobuffer::geobuffer_pts(xy = data.frame(lon = c(occ.data[x,"x"]),lat = c(occ.data[x,"y"])),dist_m = spatial.dist,step_dg = 60,output = "sp") # Create polygon of spatial area surrounding occurrence co-ordinate to extract sampling effort across

  xmin<-sp::bbox(raster::extent(surroundingarea))[1,1] # Extract co-ordinates for buffer area
  xmax<-sp::bbox(raster::extent(surroundingarea))[1,2]
  ymin<-sp::bbox(raster::extent(surroundingarea))[2,1]
  ymax<-sp::bbox(raster::extent(surroundingarea))[2,2]

  samplingefforttemp <-as.data.frame(samplingefforttemp[samplingefforttemp$x <= xmax &    # Filter temporally filtered sampling events to include only records within spatial limits in longitude
                                                          samplingefforttemp$x >= xmin, ])

  samplingefforttemp <-as.data.frame(samplingefforttemp[samplingefforttemp$y <= ymax &    # Filter temporally filtered sampling events to include only records within spatial limits in latitude
                                                      samplingefforttemp$y >= ymin, ])

  SAMP_EFFORT<-rbind(SAMP_EFFORT,nrow(samplingefforttemp))} ## Continue iterating through each record

  REL_SAMP_EFFORT<-SAMP_EFFORT/(sum(SAMP_EFFORT,na.rm=T)) ## Calculate relative sampling effort
  return(cbind(occ.data,SAMP_EFFORT,REL_SAMP_EFFORT)) }



