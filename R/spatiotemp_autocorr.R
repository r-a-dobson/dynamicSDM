#' Test for spatial and temporal autocorrelation in species distribution model explanatory data.
#'
#' Function performs statistical tests to assess spatial and temporal autocorrelation in given explanatory variable data.
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
#' @examples
#'data("sample_model_data")
#'spatiotemp_autocorr(sample_model_data,varname="Temperaturemean",temporal.level="year")
#'@export

spatiotemp_autocorr <- function(occ.data, varname, temporal.level) {

  # Re-order the data into chronological order (based on temporal.level)

  temporal_ordered <- occ.data[order(occ.data[, temporal.level]), ]


  # Create list to contain all variables results
  list.of.results <- vector("list", length(varname))

  # Create list for results
  list.of.results.split <- vector("list", 2)

 for (v in 1:length(varname)) {

   # ----------------------------------------------------------------------
   # Temporal autocorrelation
   # ----------------------------------------------------------------------

   # Aggregate variable data by taking the mean for each temporal.level
   temporal_agg <-
     aggregate(as.formula(paste0(varname[v], "~", temporal.level)),
               FUN = mean,
               data = temporal_ordered,
               na.rm = T)

    # Extract aggregated data for variable at each time step
    data <- temporal_agg[, varname[v]]

    # Remove final timestep (as will not have value for the following timestep)
    first_obs <- data[-length(data)]
    second_obs <- data[-1]  # Obtain values at following timestep

    # Plot relationship between value at one timestep compared to following
    plot(
      first_obs,
      second_obs,
      xlab = paste0(varname[v], '(t)'),
      ylab = paste0(varname[v], '(t-1)'),
      pch = 19
    )
    abline(lm(first_obs ~ second_obs))

    # Calculate correlation statistic
    list.of.results.split[[1]] <- cor.test(first_obs, second_obs)

    # ----------------------------------------------------------------------
    # Spatial autocorrelation
    # ----------------------------------------------------------------------

    # Calculate spatial distance matrix between each record co-ordinate
    distance_matrix <- geodist::geodist(data.frame(occ.data$x, occ.data$y),
                                        measure = 'geodesic')

    # Set diagonal combinations as FALSE
    diag(distance_matrix) <- FALSE

    ## Calculate Moran's I statistic for specified variable using distance matrix
    list.of.results.split[[2]] <-
      as.data.frame(ape::Moran.I(occ.data[, varname[v]], distance_matrix))

    # Names the results
    names(list.of.results.split) <- c("Temporal autocorrelation",
                                      "Spatial autocorrelation")

    list.of.results[[v]] <- list.of.results.split # Bind results to list
  }

  # Names list of results for each variable as variables name
  names(list.of.results) <- c(varname)

  return(list.of.results)
}
