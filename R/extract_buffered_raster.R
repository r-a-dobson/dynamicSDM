#'Extract spatially buffered and temporally dynamic rasters of explanatory variable data.
#'
#'Extract rasters for spatially buffered and temporally dynamic explanatory variables at each
#'projection date using Google Earth Engine.
#'@param dates a character string, vector of dates in format "YYYY-MM-DD".
#'@param spatial.ext the spatial extent for the extracted raster. Object from which extent can be
#'  extracted of class `Extent`, `RasterLayer`,`SpatialPolygonsDataFrame`, `sf` or `polygon` or
#'  numeric vector listing xmin, xmax, ymin and ymax in order.
#'@param datasetname a character string, the Google Earth Engine dataset to extract data from.
#'@param bandname a character string, the Google Earth Engine dataset bandname to extract data for.
#'@param temporal.level a character string indicating the temporal resolution of the remote-sensing
#'  dataset (`datasetname`). One of `day`, `month` or `year`: can be abbreviated. Default; `day`.
#'@param spatial.res.metres a numeric value, specifying the spatial resolution in metres of the
#'  raster to be extracted.
#'@param GEE.math.fun a character string, the mathematical function to compute across the specified
#'  period and spatial buffer from each projection date and cell.
#'@param moving.window.matrix a matrix of weights with an odd number of sides to specify spatial
#'  neighbourhood of cells ("moving window") to calculate `GEE.math.fun` across for each cell in
#'  `spatial.ext`. See details for more information.
#'@param user.email a character string, user email for initialising Google Drive.
#'@param varname optional; a character string, the unique name for the explanatory variable. Default
#'  varname is “bandname_temporal.res_temporal.direction_ GEE.math.fun_buffered_raster".
#'@param temporal.res optional; a numeric value, the temporal resolution in days prior or post each
#'  projection date to calculate `GEE.math.fun` across.
#'@param temporal.direction optional; a character string, the temporal direction for extracting
#'  dynamic variable data across relative to each projection date given. One of `prior` or `post`:
#'  can be abbreviated.
#'@param categories optional; a character string, the categories to use in the calculation if data
#'  are categorical. See details for more information.
#'@param save.directory optional; a character string, path to local directory to save extracted
#'  rasters to.
#'@param save.drive.folder optional; a character string, Google Drive folder to save extracted
#'  rasters to. Folder must be uniquely named within Google Drive. Do not provide path.
#'@param agg.factor optional;a postive integer, the aggregation factor expressed as number of cells
#'  in each direction. See details.
#'@param resume a logical indicating whether to search `save.directory` or `save.drive.folder` and
#'  return to previous progress through projection dates.Default = TRUE.
#'@details For each projection date, this function downloads rasters at a given spatial extent and
#'  resolution for spatially buffered and temporally dynamic explanatory variables. Rasters can be
#'  saved directly to Google Drive or a local directory. These rasters can be
#'  combined to create projection covariate data frames for projecting dynamic species distribution
#'  and abundance at high spatiotemporal resolution.
#'
#'  # Temporal dimension
#'
#'  If `temporal.res` and `temporal.direction` are not given, explanatory variable data for all
#'  cells within `spatial.ext` are extracted. If `temporal.res` and `temporal.direction` are given,
#'  explanatory variable data for all cells within `spatial.ext` are extracted, for which
#'  `GEE.math.fun` has been first calculated over the specified period in relation to the projection
#'  date (prior or post).
#'
#'  # Categorical data and temporally dynamic variables
#'
#'  Please be aware, if specific categories are given (argument `categories`) when extracting
#'  categorical data, then temporal buffering cannot be completed. The most recent categorical data
#'  to the occurrence record date will be used and spatial buffering will take place.
#'
#'  If, specific categories are not given when extracting from categorical datasets, be careful to
#'  choose appropriate mathematical functions for such data. For instance, "first" or "last" may be
#'  more relevant that "sum" of land cover classification numbers.
#'
#'
#'  # Spatial dimension
#'
#'  Using the `focal` function in `raster` R package (Hijmans et al., 2015), `GEE.math.fun` is
#'  calculated across the spatial buffer area from each cell in `spatial.ext`. The spatial buffer
#'  area used is defined by `moving.window matrix`, which dictates the neighbourhood of cells
#'  surrounding each cell in `spatial.ext` to include in the calculation.
#'  See \link{get_moving_window}.
#'
#'  # Mathematical function
#'
#'  `GEE.math.fun` specifies the mathematical function to be calculated over the spatial buffered
#'  area and temporal period. Options are limited to Google Earth Engine ImageCollection Reducer
#'  functions (<https://developers.google.com/earth-engine/apidocs/>) for which an analogous R
#'  function is available. This includes: "allNonZero","anyNonZero", "count",
#'  "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product",
#'  "sampleStdDev", "sampleVariance", "stdDev", "sum" and "variance".
#'
#'  # Categorical data
#'
#'  If explanatory variable data are categorical (e.g. land cover classes), `categories` can be used
#'  to specify the categories of importance to the calculation. The category or categories given
#'  will be converted in a binary representation, with “1” for those listed, and “0” for all others
#'  in the dataset. Ensure that the `GEE.math.fun` given is appropriate for such data.
#'
#'  For example, this function could return the sum of suitable land cover classified cells in the
#'  “moving window” from each cell across spatial extent given.
#'
#'
#'  # Aggregation factor
#'
#'  `agg.factor` given represents the factor to aggregate `RasterLayer` data with function
#'  `aggregate` in `raster` R package (Hijmans et al., 2015). Aggregation uses the `GEE.math.fun` as
#'  the function. Following aggregation spatial buffering using the moving window matrix occurs.
#'  This is included to minimise computing time if data are of high spatial resolution and a large
#'  spatial buffer is needed. Ensure to calculate `get_moving_window()` with the spatial resolution
#'  of the data post-aggregation by this factor.
#'
#'
#'  # Google Earth Engine
#'
#'  `extract_buffered_raster()` requires users to have installed R package `rgee` (Aybar et al.,
#'  2020) and initialised Google Earth Engine with valid log-in credentials. Please follow
#'  instructions on the following website <https://cran.r-project.org/package=rgee>
#'
#'  * `datasetname` must be in the accepted Google Earth Engine catalogue layout (e.g.
#'  “MODIS/006/MCD12Q1” or “UCSB-CHG/CHIRPS/DAILY”)
#'
#'  * `bandname` must be as specified under the dataset in the Google Earth Engine catalogue (e.g.
#'  “LC_Type5”, “precipitation”). For datasets and band names, see
#'  <https://developers.google.com/earth-engine/datasets>.
#'
#'  # Google Drive
#'
#'  `extract_buffered_raster()` also requires users to have installed the R package
#'  `googledrive`(D'Agostino McGowan and Bryan, 2022) and initialised Google Drive with valid log-in
#'  credentials, which must be stated using argument `user.email`. Please follow instructions on
#'  <https://googledrive.tidyverse.org/> for initialising the `googledrive` package.
#'
#'  The `save.drive.folder` must be uniquely named within your Google Drive and do not provide the
#'  path.
#'
#'
#'  # Occasional rgee errors
#'
#' As this function uses the `rgee` package to extract rasters from Google Earth Engine, below we
#' outline occasional `rgee` errors that may occur when extracting rasters:
#'
#' * "To avoid memory excess problems, ee_as_raster will not build Raster objects for large images"
#'
#' This can be a sporadic error. It may be related to GEE server usage or internet
#' connection at the time you tested the function. Try restarting your R session or try again at
#' another time. Also, try clearing the files from the "dynamicSDM_download_bucket" in your Google
#' Drive.
#'
#' This error could also be due to an issue with your input `spatial.res.metres`. This
#' function will extract rasters at all typical spatial resolutions of remote-sensing data and at
#' global extents. If this error persists, please ensure you have not accidentally given an
#' unrealistically high spatial resolution (e.g. `spatial.res.metres = 0.01` when you may be
#' confusing the spatial resolution in degrees with metres).
#'
#' * "Pixel type not supported: Type Long. Convert the image to a floating point type or a smaller
#' integer type, for example, using ee.Image.toDouble()"
#'
#' This error appears when `rgee` has been given an input that cannot be extracted. Some common
#' causes include:
#'
#' + Inappropriate `GEE.math.fun` for extracting categorical data.
#'
#' + Dates or spatial extents that are not covered by the chosen GEE dataset. Remember to check
#' whether the first projection date minus `temporal.res` is still within the temporal extent of the
#' dataset.
#'
#'@references Aybar, C., Wu, Q., Bautista, L., Yali, R. and Barja, A., 2020. rgee: An R package for
#'  interacting with Google Earth Engine. Journal of Open Source Software, 5(51), p.2272.
#'
#'  D'Agostino McGowan L., and Bryan J., 2022. googledrive: An Interface to Google Drive.
#'  <https://googledrive.tidyverse.org>, <https://github.com/tidyverse/googledrive>.
#'
#'  Hijmans, R. J., Van Etten, J., Cheng, J., Mattiuzzi, M., Sumner, M., Greenberg, J. A.,
#'  Lamigueiro, O. P., Bevan, A., Racine, E. B. & Shortridge, A. 2015. Package ‘raster’. R package,
#'  734.
#'@return Returns details of successful explanatory variable raster extractions for each projection
#'  date.
#'@export
#'@examplesIf googledrive::drive_has_token()
#'
#' dates <- dynamic_proj_dates("2018-01-01", "2018-12-01", interval = 3,interval.level = "month")
#' \dontshow{
#'dates <- dates[1]
#'}
#'
#'data("sample_extent_data")
#'user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
#'
#' matrix<-get_moving_window(radial.distance = 10000,
#'                             spatial.res.degrees = 0.05,
#'                             spatial.ext = sample_extent_data)
#'
#'extract_buffered_raster(dates = dates,
#'                        datasetname = "MODIS/006/MCD12Q1",
#'                        bandname="LC_Type5",
#'                        spatial.res.metres = 500,
#'                        GEE.math.fun = "sum",
#'                        moving.window.matrix = matrix,
#'                        user.email = user.email,
#'                        categories=c(6,7),
#'                        agg.factor = 12,
#'                        spatial.ext = sample_extent_data,
#'                        varname = "total_grass_crop_lc",
#'                        save.directory = tempdir())
#'


extract_buffered_raster <- function(dates,
                                    spatial.ext,
                                    datasetname,
                                    bandname,
                                    temporal.level,
                                    spatial.res.metres,
                                    GEE.math.fun,
                                    moving.window.matrix,
                                    user.email,
                                    varname,
                                    temporal.res,
                                    temporal.direction,
                                    categories,
                                    save.directory,
                                    agg.factor,
                                    save.drive.folder,
                                    resume = TRUE) {

    # Check all arguments that are required have been given and are valid .

    if (missing(spatial.res.metres)) {stop("spatial.res.metres is missing")}


     if (missing(temporal.level)) {stop("temporal.level is missing.")}


  if (!any(class(spatial.ext) %in% c("numeric",
                                     "SpatialPolygonsDataFrame",
                                     "Extent",
                                     "RasterLayer",
                                     "Polygon",
                                     "sf"))) {

    stop("Please check the class of spatial.ext")

  }


  if (missing(save.directory) && missing(save.drive.folder)) {
    stop("Please give save.directory or save.drive.folder to export raster to.")
  }



    # Check user email provided
    if (missing(user.email)) {stop("Provide user.email for Google Drive")}

    # Set default variable names

    # Temporally dynamic variable name
    if (!missing(temporal.res) && missing(varname)) {
      varname <- paste0(bandname,
                        "_",
                        temporal.res,
                        "_",
                        temporal.direction,
                        "_",
                        GEE.math.fun,
                        "_buffered_raster")
      message(paste0("Default varname set as: ", varname))
    }

    # Temporally static variable name
    if (missing(temporal.res) && missing(varname)) {
      varname <- paste0(bandname, "_", GEE.math.fun, "_buffered_raster")
      message(paste0("Default varname set as: ", varname))
    }

    # Import python module
    ee <- reticulate::import("ee")

    # Initiate Google Drive
    googledrive::drive_auth(email = user.email)
    googledrive::drive_user()

    # Initiate Google Earth Engine
    rgee::ee_check("rgee")
    rgee::ee_Initialize(drive = TRUE)

    # If resume, create list of all files currently in save directory / folder
    if (resume) {

      if (!missing(save.drive.folder)) {

        # Initiate Google Drive
        googledrive::drive_auth(email = user.email)
        googledrive::drive_user()

        save.folderpath <- googledrive::drive_find(pattern = save.drive.folder, type = 'folder')
        file_list <- googledrive::drive_ls(path = googledrive::as_id(save.folderpath$id))$name
      }

      if (!missing(save.directory)) {
        file_list <- list.files(save.directory)
      }
    }



    completed.list <- NULL

      dates_df<-tidyr::separate(as.data.frame(dates), "dates", c("year", "month", "day"), sep = "-")

      if(temporal.level=="day"){uniquedates<-unique(dates_df[,c("day","month","year"),drop=FALSE])}

    if(temporal.level=="month"){uniquedates<-unique(dates_df[,c("month","year"),drop=FALSE])}

    if(temporal.level=="year"){uniquedates<-unique(dates_df[,c("year"),drop=FALSE])}

    for (x in 1:nrow(uniquedates)){

      if(temporal.level=="year"){
        # Get unique date to name
        year<-as.numeric(uniquedates[x,"year"])
        month<-dates_df[dates_df$year==year,"month"][1]
        day<-dates_df[dates_df$year==year,"day"][1]

        # Get date to extract data for
        year2 <- year
        month2 <- 1
        day2 <- 1

        }

      if(temporal.level=="month"){
        # Get unique date to name
        year<-as.numeric(uniquedates[x,"year"])
        month<-as.numeric(uniquedates[x,"month"])
        day<-dates_df[dates_df$year==year & as.numeric(dates_df$month)==month,"day"][1]

        # Get date to extract data for
        year2 <- year
        month2 <- month
        day2 <- 1
        }

      if(temporal.level=="day"){
        # Get unique date to name
        year<-as.numeric(uniquedates[x,"year"])
        month<-as.numeric(uniquedates[x,"month"])
        day<-as.numeric(uniquedates[x,"day"])

        # Get date to extract data for
        year2 <- year
        month2 <- month
        day2 <- day
        }

      date1<-as.Date(paste(year, month, day,sep="-"), "%Y-%m-%d")


      extraction_date<-as.Date(paste(year2, month2, day2,sep="-"), "%Y-%m-%d")

      ### Numeric extent to co-ords

      if (inherits(spatial.ext, "numeric") && !length(spatial.ext) == 4) {
        stop("spatial.ext should be length four: xmin, xmax, ymin and ymax")
      }

      xmin <- extract_xy_min_max(spatial.ext)[1]
      xmax <- extract_xy_min_max(spatial.ext)[2]
      ymin <- extract_xy_min_max(spatial.ext)[3]
      ymax <- extract_xy_min_max(spatial.ext)[4]


      # Create Google Earth Engine Geometry Polygon for extracting set spatial extent of raster
      geometry <- ee$Geometry$Polygon(list(c(xmin , ymin),
                                           c(xmin , ymax),
                                           c(xmax,  ymax),
                                           c(xmax,  ymin)))

      if (missing(temporal.res) ||  !missing(categories)) {

        extraction_date <- as.character(extraction_date)

        # Create Google Earth Engine ImageCollection for dataset, band and date
        image_collection <- ee$ImageCollection(paste0(datasetname))$
          filterDate(paste0(extraction_date))$select(paste0(bandname))

        # As static, only one Image in ImageCollection, so select the first.
        image_collection_reduced <- image_collection$reduce(ee$Reducer$first())
      }

      if (!missing(temporal.res)) {

        firstdate <- extraction_date

        # List of all GEE ImageCollection Reducer functions available
        GEE.FUNC <- list(ee$Reducer$allNonZero(),
                         ee$Reducer$anyNonZero(),
                         ee$Reducer$count(),
                         ee$Reducer$first(),
                         ee$Reducer$firstNonNull(),
                         ee$Reducer$last(),
                         ee$Reducer$lastNonNull(),
                         ee$Reducer$max(),
                         ee$Reducer$mean(),
                         ee$Reducer$median(),
                         ee$Reducer$min(),
                         ee$Reducer$mode(),
                         ee$Reducer$product(),
                         ee$Reducer$sampleStdDev(),
                         ee$Reducer$sampleVariance(),
                         ee$Reducer$stdDev(),
                         ee$Reducer$sum(),
                         ee$Reducer$variance()
          )

        # Names to match the GEE Reducer functions
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

        # Match named function to actual function for use.
        GEE.math.fun <- match.arg(arg = GEE.math.fun, choices = namelist)

        # Prior selected so minus temporal.res days from the record date
        if (temporal.direction == "prior") {
          seconddate <- as.character(firstdate - temporal.res)
          firstdate <- as.character(firstdate)

          # ImageCollection of all Images of dataset and band between dates
          image_collection <- ee$ImageCollection(paste0(datasetname))$
            filterDate(seconddate, firstdate)$
            select(paste0(bandname))
        }

        # Post selected so minus temporal.res days from the record date
        if (temporal.direction == "post") {
          seconddate <- as.character(firstdate + temporal.res)
          firstdate <- as.character(firstdate)

          # ImageCollection of all Images of dataset and band between dates
          image_collection <- ee$ImageCollection(paste0(datasetname))$
            filterDate(firstdate, seconddate)$
            select(paste0(bandname))
        }


    # Reduce ImageCollection using function specified in GEE.math.fun
    image_collection_reduced <- image_collection$reduce(GEE.FUNC[[match(GEE.math.fun, namelist)]])

      }

      #If resume=TRUE check for the file in the save folder/directory. If present move to next date
      if (resume) {

       check_file <- paste0(varname, "_", date1, ".tif")

        file_list_filt <- file_list[grep(check_file, file_list)]

        if (!length(file_list_filt) == 0) {
          next()
        }
        }

      # Download raster of Reduced ImageCollection to users Google Drive folder
      tryCatch({
        rgee::ee_as_raster(
          image = image_collection_reduced,
          container = "dynamicSDM_download_bucket",
          scale = spatial.res.metres,
          dsn = paste0(tempdir(),"/", varname, "_", date1, "_unprocessed"),
          region = geometry,
          timePrefix = FALSE,
          via = "drive"
        )
      }, error = function(e) {
        message("ERROR :", conditionMessage(e), "\n")
      })

      # Authenticate Google Drive
      googledrive::drive_auth(email = user.email)
      googledrive::drive_user()

      pathforthisfile <- paste0(tempfile(), ".tif") # Create temp file name

      # Download unprocessed raster to temporary directory
      googledrive::drive_download(
        paste0(varname, "_", date1, "_unprocessed.tif"),
        path = pathforthisfile,
        overwrite = TRUE
      )
      raster <- raster::raster(pathforthisfile) # Import raster from temp

      # Delete unprocessed raster from Google Drive.
      googledrive::drive_rm(paste0(varname, "_", date1, "_unprocessed.tif"))


      # Match GEE.math.fun argument to analogous R function
      R.FUNC.LIST <- list(allNonZero,
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

      # Match named function to actual function for use. Error if no match found
      GEE.math.fun <- match.arg(arg = GEE.math.fun, choices = namelist)

      # Match GEE.math.fun argument to analogous R function
      math.fun <- R.FUNC.LIST[[match(GEE.math.fun, namelist)]]


      # Process a categorical data raster
      # If more than one category, iterate through each category and add rasters

      if (!missing(categories)) {
        rast <- raster == categories[1]

        if (length(categories) > 1) {
          for (cat in 2:length(categories)) {
            rast <- rast + (raster == categories[cat])
          }
        }

        if(!missing(agg.factor)) {
          rast <- raster::aggregate(rast, agg.factor, fun = math.fun, na.rm = TRUE)
        }
        # If data are categorical then moving.window.matrix with weights = 1
        moving.window.matrix[1:nrow(moving.window.matrix),
                             1:ncol(moving.window.matrix)] <- 1

        # Calculate math.fun function across moving.window.matrix for the raster
        focalraster <- raster::focal(rast,
                                     moving.window.matrix,
                                     fun = math.fun,
                                     na.rm = TRUE)
      }

      # Process a continuous data raster
      if (missing(categories)) {

        if (!missing(agg.factor)) {
          raster <- raster::aggregate(raster, agg.factor, fun = math.fun, na.rm = TRUE)
        }

        # Calculate math.fun function across moving.window.matrix for the raster
        focalraster <- raster::focal(raster, moving.window.matrix, fun = math.fun)
      }

      if (!missing(save.directory)) {
        pathforthisfile <- paste0(save.directory, "/", varname, "_", date1, ".tif")
      }

      # Write spatially buffered raster to temp dir or save dir
      raster::writeRaster(focalraster, pathforthisfile, overwrite = TRUE)


      if(!missing(save.drive.folder)){
      # Check folder exists in  Google Drive
      save.folderpath <- googledrive::drive_find(pattern = save.drive.folder, type = 'folder')

      # If more than one folder partially matches, use grep to get exact match
      if(nrow(save.folderpath)>1) {

        save.folderpath <- save.folderpath[grep(paste0("^", save.drive.folder, "$"),
                                                save.folderpath$name), ]
      }
      # If exact match to more than one folder then not uniquely named. Cannot write file.
      if (nrow(save.folderpath) > 1) {
        stop("save.drive.folder is not uniquely named in your Google Drive ")
      }

      if (nrow(save.folderpath) == 0) {
        stop("save.drive.folder doesn't exist")
      }

      googledrive::drive_upload(
        media = pathforthisfile,
        path = googledrive::as_id(save.folderpath$id),
        name = paste0(varname, "_", date1, ".tif"),
        overwrite = TRUE
      )
   }
      if (temporal.level == "month") {
        dates.in.period <- merge(uniquedates[x, , drop = FALSE],
                                 dates_df,
                                 by = c("year", "month"),
                                 all.x = TRUE)
        dates.in.period <-as.Date(with(dates.in.period,paste(year, month, day,sep="-")), "%Y-%m-%d")

      }

      if (temporal.level == "year") {
        dates.in.period <- merge(uniquedates[x, , drop = FALSE],
                                 dates_df,
                                 by = c("year"),
                                 all.x = TRUE)
        dates.in.period <-as.Date(with(dates.in.period,paste(year, month, day,sep="-")), "%Y-%m-%d")
      }

      if (temporal.level == "day") {
        dates.in.period <- date1
      }

      if (length(dates.in.period) > 1) {


        if (!missing(save.drive.folder)) {

        lapply(dates.in.period[2:length(dates.in.period)], FUN = function(savefile)
            googledrive::drive_upload(
              media = pathforthisfile,
              path = googledrive::as_id(save.folderpath$id),
              name = paste0(varname, "_", savefile, ".tif"),
              overwrite = TRUE
            )
        )}

      if (!missing(save.directory)) {

        lapply(dates.in.period[2:length(dates.in.period)], FUN = function(savefile)
          raster::writeRaster(focalraster,
                              paste0(save.directory, "/", varname, "_", savefile, ".tif"),
                              overwrite = TRUE
            )
        )}}




      # Record successful download and iterate onto next date
      completed.list <- c(completed.list, paste0(varname, "_", dates.in.period))
      message(paste0("Completed: ",varname, "_", dates.in.period))
    }



    return(completed.list)
  }
