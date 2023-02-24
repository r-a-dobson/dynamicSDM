#'Extract temporally dynamic rasters of explanatory variables.
#'
#'Extract rasters for temporally dynamic explanatory variables at each projection date using Google
#'Earth Engine.
#'@param dates a character string, vector of dates in format "YYYY-MM-DD".
#'@param spatial.ext the spatial extent for the extracted raster. Object from which extent can be
#'  extracted of class `Extent`, `RasterLayer`, `SpatialPolygonsDataFrame`, `sf` or `polygon` or
#'  numeric vector listing xmin, xmax, ymin and ymax in order.
#'@param datasetname a character string, the Google Earth Engine dataset to extract data from.
#'@param bandname a character string, the Google Earth Engine dataset bandname to extract data for.
#'@param spatial.res.metres a numeric value, specifying the spatial resolution in metres of the
#'  raster to be extracted.
#'@param GEE.math.fun a character string, the mathematical function to compute across the specified
#'  time frame from each projection date and for each cell.
#'@param user.email a character string, user email for initialising Google Drive.
#'@param varname optional; a character string, the unique name for the explanatory variable. Default
#'  varname is “bandname_temporal.res_temporal.direction_ GEE.math.fun_raster".
#'@param temporal.res a numeric value, the temporal resolution in days to extract data across.
#'@param temporal.direction a character string, the temporal direction for extracting dynamic
#'  variable data across relative to each projection date given. One of `prior` or `post`: can
#'  be abbreviated.
#'@param save.drive.folder optional; a character string, Google Drive folder name to save extracted
#'  rasters to. Folder must be uniquely named within your Google Drive. Do not provide path.
#'@param save.directory optional; a character string, path to local directory to save extracted
#'  rasters to.
#'@param resume a logical indicating whether to search `save.directory` or `save.drive.folder` and
#'  return to previous progress through projection dates.Default = TRUE.
#'@details For each projection date, this function downloads rasters at a given spatial extent and
#'  resolution for temporally dynamic explanatory variables. For each cell within the spatial
#'  extent, the `GEE.math.fun` is calculated on the data extracted from across the specified number
#'  of days prior or post the projection date. Rasters can be saved to Google Drive or a local
#'  directory too. These rasters can be combined to create projection covariate data frames for
#'  projecting dynamic species distribution and abundance at high spatiotemporal resolution.
#'
#'  # Google Earth Engine
#'
#'  `extract_dynamic_raster()` requires users to have installed the R package `rgee` (Aybar et al.,
#'  2020) and initialised Google Earth Engine with valid log-in credentials. Please follow
#'  instructions on the following website <https://cran.r-project.org/package=rgee>.
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
#'  `extract_dynamic_raster()` also requires users to have installed the R package
#'  `googledrive`(D'Agostino McGowan and Bryan, 2022) and initialised Google Drive with valid log-in
#'  credentials, which must be stated using argument `user.email`. Please follow instructions on
#'  <https://googledrive.tidyverse.org/> for initialising the `googledrive` package.
#'
#'  The `save.drive.folder` must be uniquely named within your Google Drive and do not provide the
#'  path.
#'
#'Note: When running this function a folder labelled "dynamicSDM_download_bucket" will be created in
#'your Google Drive. This will be emptied once the function has finished running and output rasters
#'will be found in the `save.drive.folder` or `save.directory`.
#'
#'# Mathematical function
#'
#'  `GEE.math.fun` specifies the mathematical function to be calculated over the temporal period
#'  from each projection date. Options are limited to Google Earth Engine ImageCollection Reducer
#'  functions (<https://developers.google.com/earth-engine/apidocs/>) for which an analogous R
#'  function is available. This includes: "allNonZero","anyNonZero", "count",
#'  "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product",
#'  "sampleStdDev", "sampleVariance", "stdDev", "sum" and "variance".
#'
#' # Categorical data
#'
#'  Please be aware, at current this function does not support the extraction of temporally dynamic
#'  variables for specific categories within categorical datasets.
#'
#'  When extracting from categorical datasets, be careful to choose appropriate mathematical
#'  functions for such data. For instance, "first" or "last" may be more relevant that "sum" of land
#'  cover classification numbers.
#'
#'  # Occasional rgee errors
#'
#' As this function uses the `rgee` package to extract rasters from Google Earth Engine, below we
#' outline occasional `rgee` errors that may occur when extracting rasters:
#'
#' * _To avoid memory excess problems, ee_as_raster will not build Raster objects for large images_
#'
#' This can be a sporadic error. It may be related to GEE server usage or internet
#' connection at the time you tested the function. Try restarting your R session or try again at
#' another time.
#'
#' Note: This error could also be due to an issue with your input `spatial.res.metres`. This
#' function will extract rasters at all typical spatial resolutions of remote-sensing data and at
#' global extents. If this error persists, please ensure you have not accidentally given an
#' unrealistically high spatial resolution (e.g. `spatial.res.metres = 0.01` when you may be
#' confusing the spatial resolution in degrees with metres).
#'
#' * _Pixel type not supported: Type<Long>. Convert the image to a floating point type or a smaller
#' integer type, for example, using ee.Image.toDouble()_
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
#'
#'@references Aybar, C., Wu, Q., Bautista, L., Yali, R. and Barja, A., 2020. rgee: An R package for
#'interacting with Google Earth Engine. Journal of Open Source Software, 5(51), p.2272.
#'
#'D'Agostino McGowan L., and Bryan J., 2022. googledrive: An Interface to Google Drive.
#'https://googledrive.tidyverse.org, https://github.com/tidyverse/googledrive.
#'@return Returns details of successful explanatory variable extractions for each projection date.
#'@export
#'@examplesIf googledrive::drive_has_token()
#'dates <- dynamic_proj_dates("2018-01-01", "2018-12-01", interval = 3,interval.level = "month")
#'\dontshow{
#'dates <- dates[1]
#'}
#'
#'data("sample_extent_data")
#'user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
#'
#'extract_dynamic_raster(dates = dates,
#'                       datasetname = "UCSB-CHG/CHIRPS/DAILY",
#'                       bandname="precipitation",
#'                       user.email = user.email,
#'                       spatial.res.metres = 5566,
#'                       GEE.math.fun = "sum",
#'                       temporal.direction = "prior",
#'                       temporal.res = 56,
#'                       spatial.ext = sample_extent_data,
#'                       varname = "total_annual_precipitation_prior",
#'                       save.directory = tempdir())
#'
#'
#'
#'

extract_dynamic_raster <- function(dates,
                                   spatial.ext,
                                   datasetname,
                                   bandname,
                                   spatial.res.metres,
                                   GEE.math.fun,
                                   user.email,
                                   varname,
                                   temporal.res,
                                   temporal.direction,
                                   save.directory,
                                   save.drive.folder,
                                   resume = TRUE) {


    # Set default varname for saving raster

    if (missing(varname)) {
      varname <- paste0(bandname,
                        "_",
                        temporal.res,
                        "_",
                        temporal.direction,
                        "_",
                        GEE.math.fun,
                        "_raster")
      message(paste0("Default varname set as: ", varname))

    }

    # Check user email provided
    if (missing(user.email)) {
      stop("Provide email linked to Google Drive.")
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
    # Check arguments match available options/inputs
    temporal.direction <- match.arg(arg = temporal.direction,
                                    choices = c("prior", "post"))

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

    if (!missing(save.directory) && !dir.exists(save.directory)) {
      stop("save.directory not found.")
    }

    if (missing(spatial.res.metres)) {
      stop("Provide the spatial resolution in metres to extract raster at.")
    }


    if (inherits(spatial.ext, "numeric") && !length(spatial.ext) == 4) {
      stop("spatial.ext vector should be length 4: xmin, xmax, ymin and ymax")
    }

    xmin <- extract_xy_min_max(spatial.ext)[1]
    xmax <- extract_xy_min_max(spatial.ext)[2]
    ymin <- extract_xy_min_max(spatial.ext)[3]
    ymax <- extract_xy_min_max(spatial.ext)[4]

    # Create Google Earth Engine geometry object from co-ordinates
    geometry <- ee$Geometry$Polygon(list(c(xmin , ymin),
                                         c(xmin , ymax),
                                         c(xmax,  ymax),
                                         c(xmax,  ymin)))

    # List all GEE ImageCollection Reducer functions available
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
                          ee$Reducer$variance())

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

    # Match GEE.math.fun to Google Earth Engine ImageCollection Reducer function

    GEE.math.fun <- match.arg(arg = GEE.math.fun, choices = namelist)

    # Create empty vector to record dates completed
    completed.list <- NULL

    # Iterate this process through each unique date in projection dates

    for (x in 1:length(dates)) {

      firstdate <- as.character(as.Date(dates[x]))

      # Take temporal.res days from projection date
      if (temporal.direction == "prior") {
        seconddate <- as.character(as.Date(dates[x]) - temporal.res)

        # Create ImageCollection object for the variable  between the two dates
        image_collection <- ee$ImageCollection(paste0(datasetname))$
          filterDate(seconddate, firstdate)$
          select(paste0(bandname))
      }

      # Add temporal.res days to projection date
      if (temporal.direction == "post") {
        seconddate <- as.character(as.Date(dates[x]) + temporal.res)

        # Create ImageCollection object for the variable  between the two dates
        image_collection <- ee$ImageCollection(paste0(datasetname))$
          filterDate(firstdate, seconddate)$
          select(paste0(bandname))
      }

      # Reduce the ImageCollection using GEE Reducer function chosen by user
      image_collection_reduced <- image_collection$reduce(GEE.FUNC[[match(GEE.math.fun, namelist)]])


      #If resume=TRUE check for the file in the save folder/directory. If present move to next date
      if (resume) {

        check_file <- paste0(varname, "_", firstdate, ".tif")

        file_list <- file_list[grep(check_file, file_list)]

        if (!length(file_list) == 0) {
          next()
        }
        }


      tryCatch({
           rgee::ee_as_raster(
          image = image_collection_reduced,
          container = "dynamicSDM_download_bucket",# Drive folder to save raster to
          scale = spatial.res.metres, # Specifies spatial resolution of raster
          dsn = paste0(tempdir(),"/",varname, "_", firstdate,"_unprocessed"),# Names raster file
          region = geometry,# Crops to spatial.extent
          timePrefix = FALSE,
          via = "drive"
        )
      },
      error = function(e) {
        message("ERROR :", conditionMessage(e), "\n")
      })

      # Initiate Google Drive
      googledrive::drive_auth(email = user.email)
      googledrive::drive_user()

      pathforthisfile <- paste0(tempfile(), ".tif") # Create temp file name

      if (!missing(save.directory)) {pathforthisfile <- paste0(save.directory, "/", varname, "_", firstdate, ".tif")}

      googledrive::drive_download(
        paste0(varname, "_", firstdate,"_unprocessed.tif"),
        path = pathforthisfile,
        overwrite = TRUE
      )

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
        name =   paste0(varname, "_", firstdate, ".tif"),
        overwrite = TRUE
      )

      }

      # Record that this date has been processed
      message(paste0("Completed: ",varname, "_", firstdate))
      completed.list <-rbind(completed.list, paste0(varname, "_", firstdate))
    }





    message("Clearing Google Drive download bucket - dynamicSDM_download_bucket")
    googledrive::drive_auth(email = user.email)
    rgee::ee_clean_container(name="dynamicSDM_download_bucket",type="drive")

    return(completed.list)
  }
