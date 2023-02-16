#'Test for spatial and temporal autocorrelation in species distribution model explanatory data.
#'
#'Function performs statistical tests to assess spatial and temporal autocorrelation in given
#'explanatory variable data.
#'
#'@param occ.data a data frame, with columns for occurrence record co-ordinates and dates with
#'  column names as follows; record longitude as "x", latitude as "y", year as "year", month as
#'  "month", and day as "day" and associated explanatory data.
#'@param temporal.level a character string or vector, the time step(s) to test for temporal
#'  autocorrelation at. One or multiple of `day` or `month`, `year`. Can be abbreviated.
#'@param varname a character string or vector, the name(s) of the columns within `occ.data`
#'  containing data to test for autocorrelation.
#'@param plot a logical indicating whether to generate plot of temporal autocorrelation. See details
#'  for plot description. Default = FALSE.
#'@details To test for temporal autocorrelation, the function first calculates the average value
#'  across records for each time step (`temporal.level`). The correlation between the average value
#'  at one time point (t) and the value at the previous time point (t-1) is calculated and plotted
#'  (if `plot` = TRUE) A significant relationship between values at consecutive data points
#'  indicates temporal autocorrelation is present.
#'
#'  To test for spatial autocorrelation, the function calculates a distance matrix between all
#'  record co-ordinates. Moran’s I statistical test is calculated to test whether points closer in
#'  space have more similar values than those more distant from each other(Legendre, 1993). Please
#'  note that NA values are removed before Moran's I calculation.
#'
#'  As the spatial autocorrelation calculation involves computation of a distance matrix between all
#'  occurrence records. To reduce computation time, it is recommended that a sample of large
#'  occurrence datasets are input.
#'@references
#'Legendre, P. J. E. 1993. Spatial Autocorrelation: Trouble Or New Paradigm? 74, 1659-1673.
#'@return Returns a list of temporal and spatial autocorrelation test results for each variable.
#' @examples
#'data("sample_explan_data")
#'spatiotemp_autocorr(sample_explan_data,
#'                    varname = c("year_sum_prec","eight_sum_prec"),
#'                    temporal.level = c("year","month","day"))
#'@export

spatiotemp_autocorr <- function(occ.data,
                                varname,
                                temporal.level,
                                plot = FALSE) {



  # Create list to contain all variables results
  list.of.results <- vector("list", length(varname))

  # Create list for results
  list.of.results.split <- vector("list", 2)

  # Create list for plots
  plot.list <- vector("list", length(varname))


  # Match temporal.level to available options
  temporal.level <- match.arg(arg = temporal.level,
                              choices = c("day", "month","year"),
                              several.ok = TRUE)

  for (v in 1:length(varname)) {

    # ----------------------------------------------------------------------
    # Temporal autocorrelation
    # ----------------------------------------------------------------------
    # Create list to contain all variables results
    list.of.temp <- vector("list", length(temporal.level))

    # Create list to contain all variable plots
    list.of.var.plot <- vector("list", length(temporal.level))

    for (t in 1:length(temporal.level)) {

      temp <- temporal.level[t]

      # Re-order the data into chronological order (based on temporal.level)
      temporal_ordered <- occ.data[order(occ.data[, temp]), ]


      # Aggregate variable data by taking the mean for each temporal.level
      temporal_agg <- aggregate(as.formula(paste0(varname[v], "~", temp)),
                                FUN = mean,
                                data = temporal_ordered,
                                na.rm = TRUE)

      # Extract aggregated data for variable at each time step
      data <- temporal_agg[, varname[v]]

      # Remove final timestep (as will not have value for the following timestep)
      first_obs <- data[-length(data)]
      second_obs <- data[-1]  # Obtain values at following timestep

      if(plot) {

        plot.df <- as.data.frame(cbind(first_obs, second_obs))

        p<-ggplot2::ggplot(plot.df, ggplot2::aes_string(first_obs, second_obs)) +
                                 ggplot2::geom_point() +
                                 ggplot2::labs(x = paste0(varname[v], '(t)'),
                                               y = paste0(varname[v], '(t-1)'),
                                               title = paste(varname[v],
                                                             "- temporal autocorrelation:",
                                                             temp))+
                                ggplot2::geom_smooth(method = "lm", se = FALSE)

        list.of.var.plot[[t]] <- p
      }

      # Calculate correlation statistic
      list.of.temp[[t]] <- cor.test(first_obs, second_obs)

    }

    if(plot){names(list.of.var.plot)<-temporal.level}

    names(list.of.temp) <- temporal.level
    # ----------------------------------------------------------------------
    # Spatial autocorrelation
    # ----------------------------------------------------------------------

    # Calculate spatial distance matrix between each record co-ordinate
    distance_matrix <- geodist::geodist(data.frame(occ.data$x, occ.data$y),
                                        measure = 'geodesic')

    # Set diagonal combinations as FALSE
    diag(distance_matrix) <- FALSE

    # Calculate Moran's I statistic for specified variable using distance matrix

    SA<-as.data.frame(ape::Moran.I(occ.data[, varname[v]], distance_matrix,na.rm=TRUE))

    # Names the results

    list.of.results.split <- list(Temporal_autocorrelation = list.of.temp,
                                  Spatial_autocorrelation = SA)

    list.of.results[[v]] <- list.of.results.split # Bind results to list

    if(plot){plot.list [[v]] <- list.of.var.plot}
  }
  # Names list of results for each variable as variables name
  names(list.of.results) <- c(varname)


  if(plot){

    names(plot.list) <- c(varname)

    oldpar<- par(no.readonly=TRUE)
    on.exit(par(oldpar))

    #Function to allow users to click through each plot individually
    graphics::par(ask=TRUE)

    for (i in 1:length(plot.list)) {
      for (t in 1:length(temporal.level)) {
        plot(plot.list[[i]][[t]])
      }
    }


    res.list.plots <- list(Statistical_tests = list.of.results, Plots = plot.list)

    return(res.list.plots)
  }



  return(list.of.results)
}
