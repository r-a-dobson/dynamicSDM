#'Generate a “moving window” matrix of optimal size
#'
#'Calculates an optimal “moving window” matrix size for use when extracting spatially buffered
#'explanatory variables, by using the radius of interest and spatial resolution of environmental
#'data.
#'@param radial.distance a numeric value, the radius of interest in metres.
#'@param spatial.res.degrees a numeric value, the spatial resolution in degrees of explanatory
#'  variable data.
#'@param spatial.res.metres a numeric value, the spatial resolution in metres of explanatory
#'  variable data.
#'@param spatial.ext the spatial extent of the study. Object from which extent can be extracted of
#'  class `Extent`, `RasterLayer`, `sf` or `polygon` or numeric vector listing xmin, xmax, ymin and
#'  ymax in order.
#'@details
#'
#'# Importance for other functions in `dynamicSDM` To extract spatially buffered explanatory
#'variable data using dynamicSDM functions `extract_buffered_coords()` or
#'`extract_buffered_raster()`, a “moving window” matrix specifying the neighbourhood of cells to
#'include in the calculation is required.
#'
#'For example, by using a three by three “moving window” matrix of equal weights, the explanatory
#'variable would be calculated across the nine grid cells neighbouring the cell of interest and the
#'cell of interest.
#'
#'# Why use a moving window matrix instead of circular buffer? The benefit of using a “moving
#'window” over calculating explanatory variable values across a set radius from each record
#'co-ordinate, is that when generating projection rasters at high spatial and temporal resolution,
#'these can be generated much faster as the “moving windows” standardise the calculation.
#'
#'# Function calculation
#'
#'* 1) To calculate the “moving window” matrix size, the `get_moving_window()` function first
#'calculates the circular area of interest, using the user-specified radius of interest and the
#'equation for area of a circle.
#'
#'This radius should be chosen to represent the radial distance from species occurrence record
#'co-ordinates that the explanatory variable data might be relevant and impact species presence.
#'
#'* 2) Then, the average grid cell area of the explanatory variable data (derived from user-provided
#'spatial resolution and extent) is calculated. If `spatial.res.degrees` is given then `spatial.ext`
#'is required to calculate average cell area size. If `spatial.res.metres` is given then average
#'cell area is calculated by squaring this value to get cell area in square metres.
#'
#'* 3) Finally, the function then calculates the optimal “moving window” matrix that best matches
#'circular area of interest with the “moving window” matrix area. The matrix of weights will have an
#'odd number of sides.
#'
#'@return Returns "moving window" matrix with an odd number of sides and equal weights.
#' @examples
#'get_moving_window(radial.distance = 100000, spatial.res.metres = 111320)
#'
#'@export

get_moving_window <-  function(radial.distance,
                               spatial.res.degrees,
                               spatial.res.metres,
                               spatial.ext) {

  # Check that all arguments neccessary are given and of correct class

  if (missing(spatial.res.degrees) && missing(spatial.res.metres)) {
    stop("spatial.res.degrees and spatial.res.metres missing")
  }

  if (!missing(spatial.res.degrees)) {

    if (!any(class(spatial.res.degrees) == "numeric")) {
        stop("spatial.res.degrees must be of class numeric")
    }

      if (missing(spatial.ext)) {
        stop("spatial.ext is missing. Required when spatial.res given in degrees.")
      }


    # Check spatial.ext is valid
      if (!any(class(spatial.ext) %in% c("numeric", "Extent","sf", "RasterLayer", "Polygon"))) {
        stop("spatial.ext must be of class numeric, Extent, sf, RasterLayer or Polygon")
      }

      if (any(class(spatial.ext) == "numeric") && !length(spatial.ext) == 4) {
        stop("spatial.ext numeric vector should be of length four c(xmin, xmax, ymin and ymax)")
      }

      # Create raster of same resolution and extent to extract average cell area size for region
      rast_area <- raster::rasterToPoints(raster::area(
        raster::raster(ext = raster::extent(
              extract_xy_min_max(spatial.ext)[1],
              extract_xy_min_max(spatial.ext)[2],
              extract_xy_min_max(spatial.ext)[3],
              extract_xy_min_max(spatial.ext)[4]),
              resolution = spatial.res.degrees)))

      # Convert mean km2 to m2
      meancellarea <- (mean(rast_area[, 3], na.rm = TRUE)) * 1000000

    }


    if (!missing(spatial.res.metres)) {

      if (!any(class(spatial.res.metres) == "numeric")) {
        stop("spatial.res.metres must be of class numeric")
      }

    # Convert metres to square metres
      meancellarea <- (spatial.res.metres ^ 2)
    }

    # Calculate area of circle in m2 using radial.distance

    accessiblearea <- pi * radial.distance ^ 2

    # Number of cells that sum area of which best match circlar area of interest
    cells <- round(sqrt(accessiblearea / meancellarea))

    # If matrix is  even number, add one as matrix size must be odd for focal function in raster
    if ((round(cells / 2) == cells / 2) == TRUE) {
      cells <- cells + 1
    }

    return(matrix(1, nrow = cells, ncol = cells))
  }
