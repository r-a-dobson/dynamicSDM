#' Filter species occurrence records by a given spatial and temporal extent.
#'
#' Function excludes species occurrence records with co-ordinates outside given spatial extent and dates outside given temporal extent.
#'
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".
#' @param temporal.ext optional; a character vector, two dates in format YYYY-MM-DD. First date represents start of temporal extent and second date represents end of temporal extent for inclusion.
#' @param spatial.ext optional; object of class "Extent", "raster" or "polygon" or numeric vector listing xmin, xmax, ymin and ymax in order, the spatial extent to filter by.
#' @details If spatial.ext is provided, the function checks whether species occurrence record co-ordinates are within the given spatial extent of the study (spatial.ext) and excludes any outside of this extent.
#'
#' If temporal.ext is provided, the function checks whether species occurrence record dates are within the given temporal extent of the study (temporal.ext) and excludes any outside of this extent.
#' @return Returns data frame of occurrence records filtered to the spatial and temporal extent given.
#' @examples
#' x<-c(27.79125, 28.54125, 25.54125, 30.04125, 29.95792)
#' y<-c(-26.79125, -26.37458, -26.70792, -29.37458, -28.45792)
#' month<-c(1, 2, 3, 2, 4)
#' day<-c(27, 25, 16, 25, 26)
#' year<-c(2014, 2016, 2011, 2011, 2015)
#' occ.data<-data.frame(cbind(x,y,year,month,day))
#'spatiotemp_extent(occ.data,
#'                  temporal.ext=c("2012-01-01","2017-01-01"),
#'                 spatial.ext =c(28,31,-30,-26))

spatiotemp_extent<-function(occ.data, temporal.ext=NA,spatial.ext=NA){

  if(!missing(temporal.ext)){

    if(!class(temporal.ext)=="character"){stop("temporal.ext must be character vector of length 2")}

      if(!length(temporal.ext)==2){stop("two dates must be provided for temporal extent")}

        tryCatch({dates<-as.Date(temporal.ext)},error=function(e){stop("Invalid dates provided in temporal.ext. Ensure format YYYY-MM-DD")})

        if(any(is.na(dates))){stop("Invalid date given in temporal.ext. Ensure format YYYY-MM-DD")}

        firstdate<-as.Date(temporal.ext)[1]
        seconddate<-as.Date(temporal.ext)[2]

          if(firstdate-seconddate>0){stop("First date given in temporal.ext must be earlier than second date given")}


    ## Create date object from occurrence data frame year, month and day columns

    occdates<-as.Date(with(occ.data, paste(year, month, day,sep="-")), "%Y-%m-%d")

    if(any(is.na(as.character(as.Date(occdates))))){warning("occ.data contains invalid date. NAs may be present in returned data.frame.")}

        occ.data <- occ.data[occdates >= firstdate &    # subset occurrence record dataframe to records within specified temporal extent
                 occdates <= seconddate, ]}


  if(!missing(spatial.ext)){

  if(!any(class(spatial.ext)==c("numeric","Extent","RasterLayer","Polygon"))){stop("spatial.ext must be of class numeric, Extent, RasterLayer or Polygon")}


 ### Numeric extent to co-ords

  if(class(spatial.ext)=="numeric" && !length(spatial.ext)==4){stop("spatial.ext numeric vector should be of length four c(xmin, xmax, ymin and ymax)")}

  ### Subset dataframe by spatial extent given
    occ.data<-occ.data[which(occ.data[,"x"]>=(extract_xy_min_max(spatial.ext)[1])),]
    occ.data<-occ.data[which(occ.data[,"x"]<=(extract_xy_min_max(spatial.ext)[2])),]
    occ.data<-occ.data[which(occ.data[,"y"]>=(extract_xy_min_max(spatial.ext)[3])),]
    occ.data<-occ.data[which(occ.data[,"y"]<=(extract_xy_min_max(spatial.ext)[4])),]}

  return(occ.data)}






