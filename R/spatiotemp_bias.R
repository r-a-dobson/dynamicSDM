#' Test for spatial and temporal bias in species occurrence records
#'
#' Generates plots for visual assessment of spatial and temporal biases in occurrence records. Tests whether the spatiotemporal distribution of records is significantly different from the distribution from random sampling.
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".
#' @param temporal.level a character string, the time step to test for temporal bias at. One of '"day"' or '"month"', '"year"'. Can be abbreviated.
#' @details To assess temporal sampling bias, the function returns a histogram plot of the frequency distribution of records across the given time step specified by temporal.level. The observed frequency of sampling across the categorical time steps are compared to the distribution expected from random sampling, using a chi-squared test.
#'
#'To assess spatial sampling bias, the function returns a scatterplot of the spatial distribution of occurrence records to illustrate any spatial clustering. The average nearest neighbour distance of record co-ordinates is then compared to that of records randomly generated at same density using a t-test.
#'
#' As the spatial bias calculation involves calculation of a distance matrix. To reduce computation time, it is recommended that a sample of large occurrence datasets are input.
#' @return Returns list containing chi-squared and t-test results.
#' @example
#'data("sample_occ_data",package="dynamicSDM")
#'spatiotemp_bias(occ.data = sample_occ_data,temporal.level = "month")
#'@export
#'
spatiotemp_bias<-function(occ.data,temporal.level=NA){

  # Match abbreviate temporal.level to available options, fail if no match to options
  temporal.level<-match.arg(arg = temporal.level, choices = c("day", "month","year"))

  ### Temporal bias

  if(temporal.level=="day"){
    occ.data.frequency<-table(occ.data[,"day"]) ## Generate frequency table, number of records for each unique day
    expected.random.frequency<-rep(1/length(occ.data.frequency),length(occ.data.frequency)) ## Calculate expected probability for each unique day if sampling were  randomly distributed across time
    hist(as.numeric(as.vector((occ.data[,"day"]))),main="Frequency distribution of occurrence record day of day",xlab="Day")} ## Plot histogram of frequency of records for each day to visualise bias

  if(temporal.level=="month"){
    occ.data.frequency<-table(occ.data[,"month"]) ## Generate frequency table, number of records for each unique month
    expected.random.frequency<-rep(1/length(occ.data.frequency),length(occ.data.frequency)) ## Calculate expected probability for each unique month if sampling were  randomly distributed across time
    hist(as.numeric(as.vector((occ.data[,"month"]))),main="Frequency distribution of occurrence record month",xlab="Month")}## Plot histogram of frequency of records for each month to visualise bias

  if(temporal.level=="year"){
    occ.data.frequency<-table(occ.data[,"year"]) ## Generate frequency table, number of records for each unique year
    expected.random.frequency<-rep(1/length(occ.data.frequency),length(occ.data.frequency))## Calculate expected probability for each unique year if sampling were randomly distributed across time
    hist(as.numeric(as.vector((occ.data[,"year"]))),main="Frequency distribution of occurrence record year",xlab="Year")}## Plot histogram of frequency of records for each year to visualise bias

 #### Spatial Bias

  plot(occ.data$x,occ.data$y,main="Spatial cluster of occurrence records",xlab="Longitude",ylab="Latitude") ### Plot spatial distribution of records to visualise bias

  dist <- geosphere::distm(data.frame(occ.data$x,occ.data$y))  # Calculate distance between each occurrence record
  min.d <- apply(dist, 1, function(x) order(x, decreasing=F)[2]) # Calculate which column contain the minimum distance from another record
  min.d<-cbind(min.d,rep(1:ncol(dist),1)) ## Add column numbers for each occurrence record
  min.nndist.actual<-dist[min.d] ## Extract minimum nearest neighbour distance for each occurrence record


  ## Random simulated same density so extract min and max dimensions for sample area
  xmin<-min(occ.data$x)
  xmax<-max(occ.data$x)
  ymin<-min(occ.data$y)
  ymax<-max(occ.data$y)

  df<-data.frame(sp::coordinates(sp::spsample( sp::Polygon(cbind(c(xmin,xmin,xmax,xmax),c(ymin,ymax,ymax,ymin))), n=nrow(occ.data), type="random"))) # Randomly generate same number of occurrence records as occ.data in this area

  dist <- geosphere::distm(data.frame(df$x,df$y)) # Calculate distance between each simulated set of co-ordinates
  min.d <- apply(dist, 1, function(x) order(x, decreasing=F)[2]) # Calculate which column contain the minimum distance from another simulated set of co-ordinates
  min.d<-cbind(min.d,rep(1:ncol(dist),1)) ## Add column numbers for each simulated set of co-ordinates
  min.nndist.random<-dist[min.d] ## Extract minimum nearest neighbour distance for each simulated set of co-ordinates

  list<- list(chisq.test(x=occ.data.frequency, p=expected.random.frequency),  # Perform chi-squared for comparing categorical temporal distribution of occ.data to randomly simulated.
              t.test(min.nndist.actual,min.nndist.random))  ## Perform t-test for comparing average nearest neighbour distance of occ.data co-ordinates to the average nearest neighbour distance of co-ordinates randomly simulated at same density

  names(list)<-c("Temporal bias","Spatial bias")

  return(list)}


