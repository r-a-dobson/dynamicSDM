#' Generate vector of dates for dynamic projections
#'
#' Creates a vector of dates at regular intervals between two given dates.
#' @param startdate a character string, the start date in format YYYY-MM-DD.
#' @param enddate a character string, the end date in format YYYY-MM-DD.
#' @param interval.level a character string, the time-step of intervals. One of '"day"', '"month"' or '"year"': can be abbreviated.
#' @param interval a numeric value, the length of interval in interval.level units to generate between the start and end date.
#' @details Function returns a vector of dates given start.date and end.date at given interval size.
#' @return Vector of dates between start date and end date split at regular intervals.
#' @example
#’ Generate all dates at 2 month intervals that exist between dates given.
#'dynamic_proj_dates(startdate="2000-01-01",enddate="2001-01-01",interval.level = "month",interval=2)
#'Returns:
#'"2000-01-01" "2000-03-01" "2000-05-01" "2000-07-01" "2000-09-01" "2000-11-01" "2001-01-01"

dynamic_proj_dates<-function(startdate,enddate,interval.level, interval){

  ## Match user interval.level to available options

  interval.level<- match.arg(arg = interval.level, choices = c("days", "weeks","months","years"))

  # Check formatting of input arguments
  if(!class(startdate)==c("character")){stop("startdate must be class character in format YYYY-MM-DD")}
  if(!class(enddate)==c("character")){stop("enddate must be class character in format YYYY-MM-DD")}

  # Check dates given are valid dates
  tryCatch({as.Date(startdate)},error=function(e){stop("Invalid date given in startdate")})
  tryCatch({as.Date(enddate)},error=function(e){stop("Invalid date given in enddate")})

  startdate<-as.Date(startdate)
  enddate<-as.Date(enddate)

  # Check start date is earlier than end date
  if(startdate-enddate>0){stop("startdate must be before enddate")}

  listofdates<-as.Date(startdate)

  if(interval.level=="days"){
      i<-lubridate::interval(as.Date(startdate), as.Date(enddate)) %/% lubridate::days(interval) # Calculate number of dates between start and end date at user-specified number of days apart
      if(i==0){stop("There are no dates for given interval and end date")}
      for(int in 1:i){listofdates<-c(listofdates,(as.Date(startdate) + lubridate::days((int*interval))))}}# Iterate through number of dates, adding the interval and recording the date each time

  if(interval.level=="weeks"){
      i<-lubridate::interval(as.Date(startdate), as.Date(enddate)) %/% lubridate::weeks(interval) ## Calculate number of dates between start and end date at user-specified number of weeks apart
      if(i==0){stop("There are no dates for given interval and end date")}
      for(int in 1:i){listofdates<-c(listofdates,(as.Date(startdate) + lubridate::weeks((int*interval))))}}# Iterate through number of dates, adding the interval and recording the date each time

  if(interval.level=="months"){
      i<- lubridate::interval(as.Date(startdate), as.Date(enddate)) %/% months(interval) # Calculate number of dates between start and end date at user-specified number of months apart
      if(i==0){stop("There are no dates for given interval and end date")}
      for(int in 1:i){listofdates<-c(listofdates,(as.Date(startdate) + months((int*interval))))}} # Iterate through number of dates, adding the interval and recording the date each time

  if(interval.level=="years"){
       i<-lubridate::interval(as.Date(startdate), as.Date(enddate)) %/% lubridate::years(interval)# Calculate number of dates between start and end date at user-specified number of years apart
       if(i==0){stop("There are no dates for given interval and end date")}
       for(int in 1:i){listofdates<-c(listofdates,(as.Date(startdate) + lubridate::years((int*interval))))}}# Iterate through number of dates, adding the interval and recording the date each time

  return(listofdates)}
