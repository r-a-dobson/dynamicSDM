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
#'x<-c(27.79125, 28.54125, 25.54125, 30.04125, 29.95792)
#'y<-c(-26.79125, -26.37458, -26.70792, -29.37458, -28.45792)
#'year<-c(2014, 2016, 2011, 2011, 2015)
#'month<-c(1, 2, 3, 2, 4)
#'day<-c(27, 25, 16, 25, 26)
#'occ.data<-data.frame(cbind(x,y,year,month,day))
#'spatiotemp_pseudoabs(occ.data,
#'                     spatial.method="random",
#'                     temporal.method="random",
#'                     spatial.ext =c(20,36,-35,-12),
#'                     temporal.ext = c("2011-01-01","2017-01-01"))
#'
#'Returns:
#'message "n.pseudoabs missing - using default of equal to number of occurrence records"
#'         x         y year month day
#'1 24.52388 -13.89866 2015    02  12
#'2 27.63831 -20.60380 2013    02  28
#'3 22.68096 -27.77846 2011    07  24
#'4 33.46343 -26.71086 2011    08  08
#'5 23.82245 -29.50937 2016    05  16
#'
#'
#'x<-c(27.79125, 28.54125, 25.54125, 30.04125, 29.95792)
#'y<-c(-26.79125, -26.37458, -26.70792, -29.37458, -28.45792)
#'year<-c(2014, 2016, 2011, 2011, 2015)
#'month<-c(1, 2, 3, 2, 4)
#'day<-c(27, 25, 16, 25, 26)
#'occ.data<-data.frame(cbind(x,y,year,month,day))
#'spatiotemp_pseudoabs(occ.data,
#'                    spatial.method="random",
#'                    temporal.method="random",
#'                    spatial.ext =c(20,36,-35,-12),
#'                    temporal.ext = c("2011-01-01","2017-01-01"))
#'
#'Returns:
#'spatiotemp_pseudoabs(occ.data,spatial.method="buffer",temporal.method="buffer",spatial.buffer = c(50000,250000),temporal.buffer = c(1,7),n.pseudoabs = 10)
#'x         y year month day
#'1  27.72052 -27.85885 2014    02  02
#'2  28.12310 -25.43493 2014    02  03
#'3  27.91989 -26.24847 2016    03  04
#'4  27.82286 -28.04036 2016    03  03
#'5  26.87930 -26.90877 2011    03  23
#'6  26.95451 -27.84577 2011    03  23
#'7  32.00781 -29.35225 2011    03  05
#'8  29.79891 -30.63704 2011    03  04
#'9  29.98996 -27.40757 2015    05  03
#'10 29.33290 -29.60677 2015    05

spatiotemp_pseudoabs<-function(spatial.method,temporal.method,occ.data=NULL,spatial.ext=NULL,temporal.ext=NULL,spatial.buffer=NULL,temporal.buffer=NULL,n.pseudoabs=100){

if(!class(n.pseudoabs)=="numeric"){stop("n.pseudoabs should be a numeric value")}

# Match argument to available options
temporal.method<-match.arg(arg = temporal.method, choices = c( "buffer","random"))
spatial.method<-match.arg(arg = spatial.method, choices = c( "buffer","random"))


## Random generation of pseudo-absence co-ordinates

  if(spatial.method=="random"){

  ### Check that if spatial random chosen, a spatial extent of appropriate class, length and order is provided
  if (missing(spatial.ext)){stop("No spatial.ext specified to randomly generate pseudo-absence co-ordinates within.")}
    if(!any(class(spatial.ext)==c("numeric","Extent","RasterLayer","Polygon"))){
      stop("spatial.ext must be of class numeric, Extent, RasterLayer or Polygon")}

   ### Numeric vector to polygon
    if(class(spatial.ext)=="numeric" && length(spatial.ext)==4){spatial.ext<-as(raster::extent(spatial.ext[1],spatial.ext[2],spatial.ext[3],spatial.ext[4]), 'SpatialPolygons')}

    ### Extent object to polygon
    if(class(spatial.ext)=="Extent"){spatial.ext<-as(spatial.ext, 'SpatialPolygons')}

    ### RasterLayer object to polygon
    if(class(spatial.ext)=="RasterLayer"){spatial.ext<-as(raster::extent(spatial.ext), 'SpatialPolygons')}

    pseudoabs_coords<-as.data.frame(sp::spsample(spatial.ext ,type='random', n=n.pseudoabs,iter=30))} ## Randomly generate specified number of pseudo-absence co-ordinates within extent polygon



## Random generation of pseudo-absence dates

if(temporal.method=="random"){

  ### Check that if temporal random chosen, a temporal extent of appropriate class, length and order is provided
  if (missing(temporal.ext)){stop("No temporal.ext specified to randomly generate pseudo-absence dates within.")}
  if(!class(temporal.ext)=="character"){stop("temporal.ext must be character vector of length 2")}
  if(!length(temporal.ext)==2){stop("two dates must be provides for temporal extent")}

  tryCatch({dates<-as.Date(temporal.ext)},error=function(e){stop("Both dates invalid given in temporal.ext. Ensure format YYYY-MM-DD")})  ## Check dates are valid
  if(any(is.na(dates))){stop("Invalid date given in temporal.ext. Ensure format YYYY-MM-DD")}
  firstdate<-as.Date(temporal.ext)[1]
  seconddate<-as.Date(temporal.ext)[2]
  if(firstdate-seconddate>0){stop("First date must be before second date")} # Check dates are in order


  pseudoabs_dates<- firstdate + sample(1:(seconddate-firstdate), size=n.pseudoabs,replace=T) ## Randomly generate dates within given extent
  pseudoabs_dates<-tidyr::separate(as.data.frame(pseudoabs_dates), "pseudoabs_dates", c("year", "month", "day"), sep = "-")} ## Split dates into year, month and day columns for returned dataframe






## Buffered generation of pseudo-absence co-ordinates

if(spatial.method=="buffer"){

  ### Check that if spatial buffer chosen, a temporal buffer of appropriate class, length and order is provided
  if(missing(spatial.buffer)){stop("No spatial.buffer specified to generate pseudo-absence co-ordinates within.")}
  if(!class(spatial.buffer)=="numeric"){stop("spatial.buffer must be numeric")}
  if(!length(spatial.buffer)==2){stop("spatial.buffer must be length  2 representing the buffer in metres to generate coords in. e.g. c(500,3000) buffer of 500m to 3000m")}
  if(spatial.buffer[1]-spatial.buffer[2]>0){stop("Second spatial.buffer must be further away than first")}


  ## Calculate the number of pseudo-absences to generate in buffer from each occurrence record to meet or slightly exceed the required amount
  value<-ceiling(n.pseudoabs/nrow(occ.data))

  # Create buffer shapefiles for given buffer min and max size from each occurrence record co-ordinates as specified by the user

  first.buff<-rangemap::geobuffer_points(occ.data[, c("x","y")],radius=spatial.buffer[1],by_point = T)

  second.buff<-rangemap::geobuffer_points(occ.data[, c("x","y")],radius=spatial.buffer[2],by_point = T)

  raster::crs(second.buff)<-NA
  raster::crs(first.buff)<-NA

  pseudoabs_coords=NULL ## Create vector for binding pseudoabsence co-ordinates too

  for (x in 1:nrow(occ.data)){pseudoabs_coords<-rbind(pseudoabs_coords,sp::coordinates(sp::spsample(rgeos::gDifference(second.buff[x], first.buff[x]),type='random', n=value,iter=30)))}

  colnames(pseudoabs_coords)<-c("x","y")}# Set pseudoabsence co-ordinate column names as "x" for co-ordinate longitude and "y" for co-ordinate latitude



## Buffered generation of pseudo-absence dates

if(temporal.method=="buffer"){

  ### Check that if temporal buffer chosen, a temporal buffer of appropriate class, length and order is provided
  if (missing(temporal.buffer)){stop("No temporal.buffer specified specified to generate pseudo-absence dates within.")}
    if(!class(temporal.buffer)=="numeric"){stop("temporal.buffer must be numeric")}
      if(!length(temporal.buffer)==2){stop("temporal.buffer must be length(2) representing the buffer to generate coords in")}
          if(temporal.buffer[1]-temporal.buffer[2]>0){stop("Second temporal.buffer must be higher than first")}

  ## Calculate the number of pseudo-absences to generate in buffer from each occurrence record to meet or slightly exceed the required amount
  value<-ceiling(n.pseudoabs/nrow(occ.data))

  ## Generate list of max temporal buffer distance (specified by user) away from each occurrence record date
  date1<-as.Date(with(occ.data, paste(year, month, day,sep="-")), "%Y-%m-%d")+ temporal.buffer[2]

  pseudoabs_dates=date1[1] ## Create "Date" vector for binding pseudoabsence dates too

for (x in 1:length(date1)){pseudoabs_dates<-c(pseudoabs_dates,as.Date(date1[x]- sample(c(0:temporal.buffer[1],0:(-temporal.buffer[1])), value,replace=T)))} # For each occ.data record date, randomly select the calculated number of dates (object "value") within the buffer period

    pseudoabs_dates<-pseudoabs_dates[2:length(pseudoabs_dates)] ## Remove first one as used to set vector class as "Date"
    pseudoabs_dates<-as.data.frame(pseudoabs_dates)
    pseudoabs_dates<-tidyr::separate(pseudoabs_dates, "pseudoabs_dates", c("year", "month", "day"), sep = "-")  }



# If either method "buffer" is chosen, there may be slightly more generated than specified by n.pseudoabs, so randomly select this amount from generated lists

if(temporal.method=="buffer" && spatial.method=="buffer"){
  pseudo.df<-dplyr::sample_n(as.data.frame(cbind(pseudoabs_coords,pseudoabs_dates)),n.pseudoabs)} ## Keeps co-ordinates and dates relevant to same occurrence record together before randomly selecting

if(temporal.method=="buffer" && spatial.method=="random"){
pseudoabs_dates<-dplyr::sample_n(as.data.frame(pseudoabs_dates),n.pseudoabs) ## As ceiling used to calculate maximum number of points in buffer to generate to meet specified number of pseudo.abs, this may be slightly too many. This randomyl selects the exact number specified by the user
pseudo.df<-as.data.frame(cbind(pseudoabs_coords,pseudoabs_dates))}

if(temporal.method=="random" && spatial.method=="buffer"){
pseudoabs_coords<-dplyr::sample_n(as.data.frame(pseudoabs_coords),n.pseudoabs)
pseudo.df<-as.data.frame(cbind(pseudoabs_coords,pseudoabs_dates))}

if(temporal.method=="random" && spatial.method=="random"){
pseudo.df<-as.data.frame(cbind(pseudoabs_coords,pseudoabs_dates))}

return(pseudo.df)}
