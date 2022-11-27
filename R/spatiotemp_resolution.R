#' Filter species occurrence records by given spatial and temporal resolution
#'
#' Filters species occurrence record data frame to exclude records with co-ordinates and dates that do not meet specified spatial and temporal resolution.
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".
#' @param spatial.res optional; a numeric value, the minimum acceptable number of decimal places given for occurrence record co-ordinates.
#' @param temporal.res optional; a character string, the minimum acceptable temporal resolution of occurrence record dates. One of '"day"', '"month"' or '"year"': can be abbreviated.
#' @details Excludes species occurrence records that do not meet the minimum spatial and temporal resolution specified.
#'
#' If spatial.res given, the value of 1 represents acceptable co-ordinate resolution of one decimal place, roughly equal to 11.1km, and value of 3 represents three decimal places, roughly equal to 111m.
#'
#' If temporal.res given, temporal.res = "day" would result in exclusion of records without values for year, month and day, and temporal.res = "year" would only exclude records without values for year.
#'
#' spatial.res and temporal.res can be informed based upon the highest spatial and temporal resolution of the datasets to be utilised when extracting dynamic variables. For instance, if explanatory variables datasets are annual, then temporal.res of "year" is adequate, whereas if datasets are daily, then temporal.res of "day" is more appropriate.
#' @return Returns a data frame of species records filtered by the minimum acceptable spatial resolution of co-ordinates and temporal resolution of dates.
#' @examples
#'x<-c(27.1, 28.54125, 25.54125, 30.04125, 29.95792)
#'y<-c(-26.79125, -26.37458, -26.70792, -29.37458, -28.45792)
#'year<-c(2014, 2016, 2011, 2011, 2015)
#'month<-c(1, 2, 3, 2, 4)
#'day<-c(27, 25, 16, 25, 26)
#'occ.data<-data.frame(cbind(x,y,year,month,day))
#'spatiotemp_resolution(occ.data,spatial.res=5,temporal.res="day")

spatiotemp_resolution<-function(occ.data, spatial.res=NULL, temporal.res=NULL){

  ## Filter by spatial resolution if specified

  if(!missing(spatial.res)){

    if(!class(spatial.res)=="numeric"){stop("spatial.res should be numeric")}

    res<-nchar(stringr::str_split_fixed(as.character(occ.data$x),"[.]",2)[,2]) ## Counts number of digits after the decimal places

    occ.data<-occ.data[res>=spatial.res,]} ##Filter records by given acceptable resolution specified in number of decimal places


  if(!missing(temporal.res)){

   temporal.res<-match.arg(arg = temporal.res, choices = c("day", "month","year")) # Check argument given for temporal.res

  ### If temporal.res is year, function exclude records with missing year values

  if(temporal.res=="year"){occ.data<-occ.data[!is.na(occ.data[,'year']),]}

  ### If temporal.res is month, function exclude records with missing year or month values

  if(temporal.res=="month"){
    occ.data<-occ.data[!is.na(occ.data[,'month']),]
    occ.data<-occ.data[!is.na(occ.data[,'year']),]}

  ### If temporal.res is day, function exclude records with missing year, month and day values

  if(temporal.res=="day"){
    occ.data<-occ.data[!is.na(occ.data[,'month']),]
    occ.data<-occ.data[!is.na(occ.data[,'year']),]
    occ.data<-occ.data[!is.na(occ.data[,'day']),]}}

  return(occ.data)

}

