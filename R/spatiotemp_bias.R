#'Test for spatial and temporal bias in species occurrence records
#'
#'Generates plots for visual assessment of spatial and temporal biases in occurrence records. Tests
#'whether the spatiotemporal distribution of records is significantly different from the
#'distribution from random sampling.
#'
#'@param occ.data a data frame, with columns for occurrence record co-ordinates and dates with
#'  column names as follows; record longitude as "x", latitude as "y", year as "year", month as
#'  "month", and day as "day".
#'@param temporal.level a character string or vector, the time step(s) to test for temporal bias at.
#'  One or multiple of `day` or `month`, `year.` Can be abbreviated.
#'@param plot a logical indicating whether to generate plots of spatial and temporal bias. See
#'  details for plot descriptions. Default = `FALSE`.
#'@param spatial.method a character string, the method to calculate the spatial bias statistic. One
#'  of; `simple`, `convex_hull` or `core`. See details.
#'@param prj a character string, the coordinate reference system of occ.data co-ordinates. Default
#'  is "+proj=longlat +datum=WGS84".
#'@param centroid a numeric vector of length two, specifying the centroid co-ordinates in the order
#'  of longitude then latitude. Only required if `spatial.method` = `core.` Default is mean of all
#'  occurrence record co-ordinates.
#'@param radius a numeric value, the radial distance in metres from the given centroid co-ordinate
#'  to measure spatial bias within. Only required if `spatial.method` = `core.` See details for more
#'  information. Default is mean distance of all co-ordinates from `centroid`.
#'@details
#'
#'# Temporal bias
#'
#'To assess temporal sampling bias, the function returns a histogram plot
#'of the frequency distribution of records across the given time step specified by `temporal.level`
#'(if `plot = TRUE`). The observed frequency of sampling across the categorical time steps are
#'compared to the distribution expected from random sampling, using a chi-squared test (Greenwood
#'and Nikulin, 1996) .
#'
#'# Spatial bias
#'
#' To assess spatial sampling bias, the function returns a scatterplot of the spatial
#'distribution of occurrence records to illustrate any spatial clustering (if `plot = TRUE`). The
#'average nearest neighbour distance of record co-ordinates is then compared to that of records
#'randomly generated at same density using a t-test, following the nearest neighbour index
#'established by Clark and Evans (1954).
#'
#'# Bias: methods
#'
#'Below we outline the methods for which these tests for biases can be applied. `dynamicSDM` offers
#'the additional functionality of the `core` approach. This enables users to explore sampling biases
#'in set areas of a species range. This may be valuable if periphery-core relationships could lead
#'to inaccurate inferences of sampling bias. For instance, if species are expanding or shifting
#'their ranges through space and time.
#'#'
#'* `simple` - generates the random points within a rectangle created using
#'the minimum and maximum longitude and latitude of occurrence co-ordinates.
#'
#'* `convex_hull` - generates the random points within the convex hull of occurrence record
#'co-ordinates (i.e. the smallest convex set that contains all records).
#'
#'* `core` - generates the random points within specified circular area generated from a centroid
#'point and radius. If these arguments ( `centroid` and `radius`) are not provided then `centroid`
#'is calculated by averaging co-ordinates of all occurrence records, and `radius` is the mean
#'distance away of all records from the centroid.
#'
#'For each method, only occurrence records within the specified area are tested for spatial and
#'temporal sampling biases.
#'
#'# Computation time
#'
#'As the spatial bias test involves the calculation of a distance matrix. To reduce computation
#'time, it is recommended that only a representative sample of large occurrence datasets are input.
#'
#'@references
#'Clark, P. J. & Evans, F. C. J. E. 1954. Distance To Nearest Neighbor As A Measure Of Spatial
#'Relationships In Populations. 35, 445-453.
#'
#'Greenwood, P. E. & Nikulin, M. S. 1996. A Guide To Chi-Squared Testing, John Wiley & Sons.
#'
#'@return Returns list containing chi-squared and t-test results, and plots if specified.
#' @examples
#' \donttest{
#'data(sample_explan_data)
#'
#'bias_simple <- spatiotemp_bias(
#'occ.data = sample_explan_data,
#'temporal.level = c("year"),
#'spatial.method = "simple",
#'plot = FALSE
#')
#'}
#'@export


spatiotemp_bias <-  function(occ.data,
                             temporal.level,
                             plot = FALSE,
                             spatial.method = "simple",
                             centroid,
                             radius,
                             prj = "+proj=longlat +datum=WGS84") {

#--------------------------------------------------------------------------
# Temporal bias
#--------------------------------------------------------------------------

  # Match temporal.level to available options
  temporal.level <- match.arg(arg = temporal.level,
                              choices = c("day", "month","year"),
                              several.ok = TRUE)

  # Create list to contain all variables results
  res.list <- vector("list", length(temporal.level) + 1)

  plot.list <- vector("list", length(temporal.level) + 1)

  for (t in 1:length(temporal.level)) {

    temp <- temporal.level[t]

    # Generate frequency table, number of records for each unique day
    occ.data.frequency <- table(occ.data[, temp])

    # Calculate expected probability if sampling randomly distributed in time
    expected.random.frequency <- rep(1 / length(occ.data.frequency),
                                     length(occ.data.frequency))

    # Compare occ.data temporal distribution compared to randomly simulated.
    res.list[[t]] <- chisq.test(x = occ.data.frequency, p = expected.random.frequency)

    if(plot) {


    # Plot histogram of frequency of records for each day to visualise bias
    p<- ggplot2::ggplot(occ.data,
                     ggplot2::aes_string(x = as.numeric(as.vector((occ.data[, temp])))))+
                     ggplot2::geom_histogram(binwidth = 1,
                                             color = "black",
                                             fill = "white")+
                     ggplot2::labs(title =  paste0("Frequency distribution of records: ", temp),
                                   x = paste0(temp))
    plot.list[[t]] = p

    }

    }


  #--------------------------------------------------------------------------
  # Spatial Bias
  #--------------------------------------------------------------------------

  spatial.method <- match.arg(spatial.method, choices = c("simple", "core", "convex_hull"))

  if(spatial.method=="simple") {

    # Random simulated same density so extract dimensions for sample area
    xmin <- min(occ.data$x)
    xmax <- max(occ.data$x)
    ymin <- min(occ.data$y)
    ymax <- max(occ.data$y)

    area <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(cbind(
            c(xmin, xmin, xmax, xmax, xmin),
            c(ymin, ymax, ymax, ymin, ymin)))),
            ID = "A")))

    # Crop occ.data to this area
    clipped.occ <- dynamicSDM::spatiotemp_extent(occ.data, spatial.ext = area, prj = prj)

  }



    if(spatial.method=="core") {

      # If no centroid specified take average of all co-ordinates
      if (missing(centroid)) {
        centroid <- data.frame(x = mean(occ.data$x), y = mean(occ.data$y))
      }

    # Average distance in metres of all points from centroid
    if (missing(radius)) {
      radius <- mean(raster::pointDistance(occ.data[, c("x", "y")], centroid, lonlat = FALSE))
    }

    # Create spatial buffer of radius from centroid
    area <- sf::st_buffer(sf::st_as_sf(centroid, coords = c("x", "y")), dist = radius)

    area <- sf::as_Spatial(area)


    # Crop occ.data to this area
    clipped.occ <- dynamicSDM::spatiotemp_extent(occ.data, spatial.ext = area, prj = prj)
  }

  if (spatial.method == "convex_hull") {
    # Create convex hull of occurrence records
    area <- sf::st_simplify(sf::st_convex_hull(sf::st_union(sf::st_geometry(
                                                            sf::st_as_sf(occ.data,
                                                                          coords = c("x", "y"))))))
    area <- sf::as_Spatial(area)

    # Crop occ.data to this area
    clipped.occ <- dynamicSDM::spatiotemp_extent(occ.data, spatial.ext = area, prj = prj)

  }

  # Calculate distance between each occurrence record
  dist <- geosphere::distm(data.frame(clipped.occ$x, clipped.occ$y))

  # Calculate which column contain the minimum distance from another record
  min.d <- apply(dist, 1, function(x)   order(x, decreasing = FALSE)[2])

  # Add column numbers for each occurrence record
  min.d <- cbind(min.d, rep(1:ncol(dist), 1))

  # Extract minimum nearest neighbour distance for each occurrence record
  min.nndist.actual <- dist[min.d]


  # Randomly generate same number of occurrence records as occ.data in this area
  df <- data.frame(sp::coordinates(sp::spsample(area,
                                                n = nrow(clipped.occ),
                                                type = "random")))

  # Calculate distance between each simulated set of co-ordinates
  dist <- geosphere::distm(data.frame(df$x, df$y))

  # Calculate minimum distance between simulated set of co-ordinates
  min.d <- apply(dist, 1, function(x)
    order(x, decreasing = FALSE)[2])

  # Add column numbers for each simulated set of co-ordinates
  min.d <- cbind(min.d, rep(1:ncol(dist), 1))

  # Extract minimum nearest neighbour distance for simulated set of co-ordinates
  min.nndist.random <- dist[min.d]

  # Compare occ.data average NND to randomly simulated at same density NND
  res.list[[length(temporal.level)+1]]<- t.test(min.nndist.actual, min.nndist.random)

  names(res.list) <- c(paste0("Temporal_bias_",temporal.level),"Spatial_bias")

  if(plot){

    x <- df$x
    y <- df$y
    # Plot spatial distribution of records to visualise clustering or bias
    plot.list[[length(temporal.level)+1]]<- ggplot2::ggplot(clipped.occ, ggplot2::aes(x, y)) +
      ggplot2::geom_point(ggplot2::aes(colour="Occurrence"))+
      ggplot2::coord_fixed(ratio = 1)+
      ggplot2::labs( title = "Spatial cluster of occurrence records",
                     x = "Longitude",
                     y = "Latitude",color="Type")+
      ggplot2::geom_point(df,mapping =ggplot2::aes(x=x,y=y,colour="Randomly simulated"),alpha=0.2)+
      ggplot2::theme(legend.position="right")+
      ggplot2::scale_color_manual(values = c("Occurrence" = "black", "Randomly simulated" = "red"))

  #Function to allow users to click through each plot individually
  oldpar<- par(no.readonly=TRUE)
  suppressWarnings(on.exit(graphics::par(oldpar),add=T))

  graphics::par(ask=TRUE)

  for (i in 1:length(plot.list)){
    plot(plot.list[[i]])
  }


  names(plot.list) <- c(paste0("Temporal_bias_plot_",temporal.level),"Spatial_bias_plot")

  res.list.plots <- list(Statistical_tests = res.list, Plots = plot.list)

  return(res.list.plots)}

  return(res.list)
}
