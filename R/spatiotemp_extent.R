#'Filter species occurrence records by a given spatial and temporal extent.
#'
#'Function excludes species occurrence records with co-ordinates outside a given spatial extent and
#'record dates outside a given temporal extent.
#'
#'@param occ.data a data frame, with columns for occurrence record co-ordinates and dates with
#'  column names as follows; record longitude as "x", latitude as "y", year as "year", month as
#'  "month", and day as "day".
#'@param temporal.ext optional; a character vector, two dates in format "YYYY-MM-DD". First date
#'  represents start of temporal extent and second date represents end of temporal extent for
#'  inclusion.
#'@param spatial.ext the spatial extent to filter by. Object from which extent can be extracted of
#'  class `Extent`, `RasterLayer`, `SpatialPolygonsDataFrame`,`sf` or `polygon` or numeric vector
#'  listing xmin, xmax, ymin and ymax in order.
#'@param prj a character string, the coordinate reference system of input `occ.data` co-ordinates.
#'  Default is "+proj=longlat +datum=WGS84".
#'@details
#'
#' # Spatial extent
#'
#'If `spatial.ext` is provided, `spatiotemp_extent()` checks whether species occurrence record
#'co-ordinates are within the given spatial extent of the study (`spatial.ext`) and excludes any
#'outside of this extent.
#'
#'
#'If `spatial.ext` object can be used as a mask by `raster::mask()` then the mask is used to filter
#'records in a more targetted way. If not, then the rectangular extent of the `spatial.ext` object
#'is used. If an `sf` polygon object is provided, this is first transformed into a `Spatial` object
#'for use by `raster::mask()`.
#'
#'# Temporal extent
#'
#'If `temporal.ext` is provided, `spatiotemp_extent()` checks whether species
#'occurrence record dates are within the given temporal extent of the study and excludes any outside
#'of this extent.
#'@return Returns data frame of occurrence records filtered to the spatial and temporal extent
#'  given.
#' @examples
#'data(sample_filt_data)
#'data(sample_extent_data)
#'\donttest{
#'results <- spatiotemp_extent(occ.data = sample_filt_data,
#'                             spatial.ext = sample_extent_data,
#'                             temporal.ext = c("2012-01-01", "2017-01-01"))
#'}
#'@export

spatiotemp_extent <- function(occ.data,
                              temporal.ext,
                              spatial.ext,
                              prj="+proj=longlat +datum=WGS84") {


  # Check formatting of temporal.ext

  if (!missing(temporal.ext)) {
    if (!is.character(temporal.ext)) {
      stop("temporal.ext must be character vector of length 2")
    }

    if (!length(temporal.ext) == 2) {
      stop("two dates must be provided for temporal extent")
    }

    # Check validity of temporal.ext dates
    tryCatch({
      dates <- as.Date(temporal.ext)}, error = function(e) {
        stop("Invalid dates provided in temporal.ext. Ensure format YYYY-MM-DD")
      })

    if (any(is.na(dates))) {
      stop("Invalid date given in temporal.ext. Ensure format YYYY-MM-DD")
    }

    firstdate <- as.Date(temporal.ext)[1]
    seconddate <- as.Date(temporal.ext)[2]

    if (firstdate - seconddate > 0) {
      stop("First date in temporal.ext must be earlier than second date given")
    }

    # To speed up crop by years first

    occ.data <- occ.data[occ.data$year >= lubridate::year(firstdate) &
                           occ.data$year <= lubridate::year(seconddate), ]

    # Now finer temporal extent cropping
    # Create date object from occurrence data frame year, month and day columns
    occdates <- as.Date(with(occ.data, paste(year, month, day, sep = "-")), "%Y-%m-%d")

    if (any(is.na(as.character(lubridate::as_date(occdates))))) {
      warning("occ.data contains invalid date. NAs may be present in output.")
    }

    # Subset occurrence record dataframe to records within specified temporal extent
    occ.data <- occ.data[occdates >= firstdate & occdates <= seconddate, ]
  }


  if (!missing(spatial.ext)) {

    if (any(class(spatial.ext) == "numeric") && length(spatial.ext) == 4) {
      spatial.ext <- raster::extent(spatial.ext)
    }

    # Create spatial points dataframe from occurrence records
    points <- sp::SpatialPointsDataFrame(coords = occ.data[,c("x","y")],
                                       data = occ.data,
                                       proj4string = sp::CRS(prj))
    r <- spatial.ext

    # Convert sf object to Spatial object that can be tranformed into raster
    if("sf" %in% class(spatial.ext)){
      spatial.ext <- sf::as_Spatial(spatial.ext)}


    # Convert polygon object to Extent object that can be transformed into raster
    if ("Polygon" %in% class(spatial.ext)) {
      xmin <- sp::bbox(spatial.ext)[1, 1]
      xmax <- sp::bbox(spatial.ext)[1, 2]
      ymin <- sp::bbox(spatial.ext)[2, 1]
      ymax <- sp::bbox(spatial.ext)[2, 2]
      r<-raster::extent(xmin,xmax,ymin,ymax)}


    if("RasterLayer" %in% class(spatial.ext)){
      spatial.ext <- raster::rasterToPolygons(spatial.ext)}


    # Convert spatial.ext to raster in same projection
    r <- raster::raster(r)
    raster::crs(r) <- prj
    raster::res(r) <- 0.01 # High resolution for precise clipping
    r <- raster::setValues(r, values = 1:raster::ncell(r)) # Set fake raster values - not important


    # Mask to original spatial.ext, if fails keep original. Depending on spatial.ext type.
    tryCatch({
      r <- raster::mask(r, spatial.ext)
    }, error = function(error_message) {
      r <- r
      message("spatial.mask could not be used. Check valid input type.")})


    # Remove points that return NA as these do not overlap the spatial extent
    occ.data<-occ.data[!is.na(raster::extract(r,points)),]

  }

  return(occ.data)
}
