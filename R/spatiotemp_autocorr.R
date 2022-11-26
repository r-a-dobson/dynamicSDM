#' Test for spatial and temporal autocorrelation in species distribution model explanatory data or model residuals.
#'
#' Function performs statistical tests to assess spatial and temporal autocorrelation in given explanatory variable data or model residuals.
#'
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day" and associated explanatory data.
#' @param temporal.level a character string, the time step to test for temporal autocorrelation at. One of '"day"' or '"month"', '"year"'. Can be abbreviated.
#' @param varname a character string or vector, the name(s) of the columns within occ.data containing data to test for autocorrelation.
#' @details
#' To test for temporal autocorrelation, the function first calculates the average value across records for each time step (temporal.level). The correlation between the average value at one time point (t) and the value at the previous time point (t-1) is calculated and plotted. A significant relationship between values at consecutive data points indicates temporal autocorrelation is present.
#'
#' To test for spatial autocorrelation, the function calculates a distance matrix between all record co-ordinates. Moran’s I statistical test is calculated to test whether points closer in space have more similar values than those more distant from each other.
#'
#' As the spatial autocorrelation calculation involves computation of a distance matrix between all occurrence records. To reduce computation time, it is recommended that a sample of large occurrence datasets are input.
#' @return Returns a list of temporal and spatial autocorrelation test results for each variable.
#' @example
#'Load in occurrence and explanatory variable data frame
#'data("sample_model_data")
#'Take sample of 2000 for reduced computing time of example
#'sample_model_data<-dplyr::sample_n(sample_model_data,2000)
#'
#'spatiotemp_autocorr(sample_model_data,varname="Temperaturemean",temporal.level="year")
#'Returns:
#'
#'$Temperaturemean
#'$Temperaturemean$"Temporal autocorrelation"
#'
#'Pearson's product-moment correlation
#'
#'data:  first_obs and second_obs
#'t = 1.0892, df = 13, p-value = 0.2958
#'alternative hypothesis: true correlation is not equal to 0
#'95 percent confidence interval:
#' -0.2618759  0.6980383
#'sample estimates:
#'      cor
#'0.2891802
#'
#'
#'$Temperaturemean$"Spatial autocorrelation"
#'     observed      expected           sd p.value
#'1 -0.09301179 -0.0005002501 0.0002770151       0


spatiotemp_autocorr<-function(occ.data,varname,temporal.level){

  temporal_ordered<-occ.data[order(occ.data[,temporal.level]),] ### Re-order the data into time order (based upon temporal.level specified by user)

  distance_matrix <- geodist::geodist(data.frame(occ.data$x,occ.data$y), measure = 'geodesic' ) ## Calculate spatial distance matrix between each occurrence record co-ordinate for spatial autocorrelation
  diag(distance_matrix) <-FALSE ### Set diagonal combinations as FALSE

  list.of.results <- vector("list", length(varname))  # Create list to contain all variables results
  list.of.results.split<-vector("list", 2) # Create list to contain each variables spatial and temporal autocorrelation results
  temporal.level="year"

  for(v in 1:length(varname)){

  #### Temporal autocorrelation
  temporal_agg<-aggregate(as.formula(paste0(varname[v],"~",temporal.level)),FUN=mean,data=temporal_ordered,na.rm=T) ## Aggregate variable data by taking the mean for each distinct temporal.level specified by user.
  data <-temporal_agg[,varname[v]] # Extract aggregate data for the variable at each time step
  first_obs <- data[-length(data)] ## Remove final timestep (as this will not have a value for the following timestep for temporal autocorrelation calculation)
  second_obs<-data[-1]  ## Obtain value at following timestep
  plot(first_obs, second_obs, xlab=paste0(varname[v],'(t)'), ylab=paste0(varname[v],'(t-1)'),pch = 19) ## Plot relationship between variable value at one timestep compared to the following timestep
  abline(lm(first_obs ~ second_obs))
  list.of.results.split[[1]]<-cor.test(first_obs,second_obs) ## Calculate correlation between variable value at one timestep compared to the following timestep

    #### Spatial autocorrelation

  list.of.results.split[[2]]<-as.data.frame(ape::Moran.I(occ.data[,varname[v]], distance_matrix)) ## Calculate Moran's I statistic for specified variable using distance matrix
  names(list.of.results.split)<-c("Temporal autocorrelation","Spatial autocorrelation") ### Names the test results
  list.of.results[[v]]<-list.of.results.split}

  names(list.of.results)<-c(varname) ## Names the list of results for each variable as the variables name
  return(list.of.results)}





