#'Extract explanatory variables from static rasters
#'
#'Explanatory variable data are extracted from static environmental rasters at
#'record co-ordinate or across moving window matrix
#'
#'@param occ.data a data frame, with columns for occurrence record co-ordinates
#'  and dates with column names as follows; record longitude as "x", latitude as
#'  "y", and associated explanatory variable data.
#'@param static.rasters a `SpatRaster` containing one or more SpatRaster layers
#'  to extract data from.
#'@param extraction.method a character string or vector, the methods to extract
#'  data from SpatRaster using `terra` package `extract` function. One of `simple`
#'  or `bilinear`. If `simple` values for the cell a point falls in are
#'  returned. If `bilinear` the returned values are interpolated from the values
#'  of the four nearest raster cells.
#'@param varnames a character string or vector, the unique names for each
#'  explanatory variable in order of layers in the SpatRaster.
#'@param GEE.math.fun optional; a character string, the mathematical function to
#'  compute across the specified spatial matrix for each record.
#'@param moving.window.matrix optional; a matrix of weights with an odd number
#'  of sides, representing the spatial neighbourhood of cells (“moving
#'  window”) to calculate `GEE.math.fun` across from record co-ordinate. See
#'  details for more information.
#'
#'@details Function to extract data from static rasters either at occurrence
#'  record co-ordinates or spatially buffered using a moving window matrix.
#'
#'
#'Note:
#' * `varnames` must be in the order of raster layers within the SpatRaster.
#' * `extraction.method` must be of length one to apply to all layers, or
#' length equal to the number of layers in `static.rasters`.
#'
#' # Spatial buffering (optional)
#'
#'  Using the `focal` function from `terra` R package (Hijmans et al., 2022),
#'  `GEE.math.fun` is calculated across the spatial buffer area from the record
#'  co-ordinate. The spatial buffer area used is specified by the argument
#'  `moving.window.matrix`, which dictates the neighbourhood of cells
#'  surrounding the cell containing the occurrence record to include in this
#'  calculation.
#'
#'  See function `get_moving_window()` to generate appropriate
#'  `moving.window.matrix`.
#'
#'  # Mathematical function
#'
#'  `GEE.math.fun` specifies the mathematical function to be calculated over the
#'  spatial buffered area and temporal period. Options are limited to Google
#'  Earth Engine ImageCollection Reducer functions
#'  (<https://developers.google.com/earth-engine/apidocs/>) for which an
#'  analogous R function is available. This includes: "allNonZero","anyNonZero",
#'  "count", "first","firstNonNull", "last", "lastNonNull", "max","mean",
#'  "median","min", "mode","product", "sampleStdDev", "sampleVariance",
#'  "stdDev", "sum" and "variance".
#'
#'@returns Returns the occurrence data frame with added columns for extracted
#'    data.
#'
#'@export
#'@references
#'Hijmans, R.J., Bivand, R., Forner, K., Ooms, J., Pebesma, E. and Sumner, M.D.,
#'2022. Package ‘terra’. Maintainer: Vienna, Austria.
#'@examples
#'\donttest{
#'data("sample_explan_data")
#'random_cat_layer <- terra::rast(sample_extent_data)
#'random_cat_layer <- terra::setValues(random_cat_layer,
#'                                     sample(0:10, terra::ncell(random_cat_layer),
#'                                            replace = TRUE))
#'
#'extract_static_coords(occ.data = sample_explan_data,
#'                      varnames = "random_cat_layer",
#'                      static.rasters = random_cat_layer)
#'
#'}

extract_static_coords <- function(occ.data,
                                  varnames,
                                  extraction.method = "simple",
                                  static.rasters,
                                  moving.window.matrix,
                                  GEE.math.fun){

  if(inherits(static.rasters, "RasterLayer")){
    static.rasters <- terra::rast(static.rasters)
  }

  if(inherits(static.rasters, "RasterStack")){
    static.rasters <- terra::rast(static.rasters)
  }


  if (!inherits(static.rasters, "SpatRaster")) {
    stop("Please provide static rasters as SpatRaster object")
  }

  if (!terra::nlyr(static.rasters) == length(varnames)) {
    stop("Provide unique name for each layer in raster stack")
  }

  if(length(extraction.method)==1) {
    extraction.method <- rep(extraction.method, terra::nlyr(static.rasters))
  }

  if (!terra::nlyr(static.rasters) == length(extraction.method)) {
    stop("Provide single or unique extraction.method for each layer in stack")
  }

  if(!missing(moving.window.matrix)){

    if (missing(GEE.math.fun)) {
      stop("Please provide math function for spatial buffering")
    }

    # Match GEE.math.fun argument to analogous R function
    R.FUNC <- list(allNonZero,
                   anyNonZero,
                   count,
                   First,
                   firstNonNull,
                   last,
                   lastNonNull,
                   max,
                   mean,
                   stats::median,
                   min,
                   mode,
                   prod,
                   sd,
                   var,
                   stdDev,
                   sum,
                   variance
    )

    namelist <- c("allNonZero",
                  "anyNonZero",
                  "count",
                  "first",
                  "firstNonNull",
                  "last",
                  "lastNonNull",
                  "max",
                  "mean",
                  "median",
                  "min",
                  "mode",
                  "product",
                  "sampleStdDev",
                  "sampleVariance",
                  "stdDev",
                  "sum",
                  "variance"
    )

    # Match named function to actual function for use. Error if no match.

    GEE.math.fun <- match.arg(arg = GEE.math.fun, choices = namelist)

    # Match GEE.math.fun argument to analogous R function
    math.fun <- R.FUNC[[match(GEE.math.fun, namelist)]]

    extracted_data = NULL

    for (x in 1:terra::nlyr(static.rasters)) {

      focal_raster<- terra::focal(static.rasters[[x]],
                                   moving.window.matrix,
                                   fun = math.fun,
                                   na.rm = TRUE)

      extracted_data <- cbind(extracted_data,
                              terra::extract(focal_raster,
                              y = as.matrix(occ.data[, c("x", "y")]),
                              method = extraction.method )[,1])

    }



  }



  if (missing(moving.window.matrix)) {
    extracted_data = NULL

    for (x in 1:terra::nlyr(static.rasters)) {
      extracted_data <- cbind(extracted_data,
                              terra::extract(static.rasters[[x]],
                                             y = as.matrix(occ.data[, c("x", "y")]),
                                             method = extraction.method)[,1])
    }

  }

  extracted_data <- as.data.frame(extracted_data)

  colnames(extracted_data) <- c(varnames)

  final_data <- cbind(occ.data, extracted_data)

  return(final_data)

}






