#'Combine explanatory variable rasters into covariates for each projection date.
#'
#'Explanatory variable rasters are imported, resampled to a given spatial resolution and extent,
#'stacked and then exported as a covariate data frame or raster stack for each projection date.
#'
#'@param dates a character string, vector of dates in format "YYYY-MM-DD".
#'@param spatial.ext optional; the spatial extent to crop explanatory variable
#'  rasters to. Object of class `SpatExtent`, `SpatRaster`, an `sf` polygon or
#'  numeric vector listing xmin, xmax, ymin and ymax in order.
#'@param varnames a character string, the unique names for each explanatory variable.
#'@param spatial.res.degrees optional; a numeric value, the spatial resolution in degrees for
#'  projection rasters to be resampled to. Required if `spatial.ext` given.
#'@param resample.method a character string or vector length of varnames, specifying resampling
#'  method to use. One of `ngb` and `bilinear`. See details for more information.
#'@param drive.folder optional; a character string or vector, Google Drive folder or folders to read
#'  projection covariate rasters from. Folder must be uniquely named within Google Drive. Do not
#'  provide path.
#'@param user.email optional; a character string, user email for initialising Google Drive. Required
#'  if `drive.folder` or `save.drive.folder` used.
#'@param local.directory optional; a character string or vector, path to local directory or
#'  directories to read projection covariate rasters from.
#'@param save.directory optional; a character string, path to local directory to save projection
#'  covariates to.
#'@param save.drive.folder optional; a character string, Google Drive folder to save projection
#'  covariates to. Folder must be uniquely named within Google Drive. Do not provide path.
#'@param cov.file.type a character string, the type of file to export projection covariates as. One
#'  of: `tif` (SpatRaster with multiple layers) or `csv`(data frame).
#'@param prj a character string, the coordinate reference system desired for projection covariates.
#'  Default is "+proj=longlat +datum=WGS84".
#'@param cov.prj a character string, the coordinate reference system desired for output projection
#'  covariates. Default is assumed to be the same as `prj`.
#'@param spatial.mask an object of class `Raster`, `sf` or `Spatial`, representing a mask in which
#'  NA cells in the mask layer are removed from the projection covariates.
#'@param static.rasters a RasterStack of one or more rasters to be added to covariates for each date.
#'@param static.varnames a character string or vector, the unique names for each
#'  explanatory variable in order of rasters in `static.raster` stack.
#'@param static.resample.method a character string or vector length of `static.varnames`, specifying resampling
#'  method to use on static rasters provided. One of `ngb` and `bilinear`. See details for more information..
#'@param static.moving.window.matrix optional; a matrix of weights with an odd number
#'  of sides, representing the spatial neighbourhood of cells (“moving
#'  window”) to calculate `GEE.math.fun` across from record co-ordinate. See
#'  details for more information.
#'@param static.GEE.math.fun optional; a character string, the mathematical function to
#'  compute across the specified spatial matrix for each cell in `spatial.ext`
#'@details
#'# Input variable rasters
#'
#'For each projection date, the rasters for each explanatory variable are imported from a local
#'directory or Google Drive folder.
#'
#'Such rasters should be uniquely named "tif" files within the directory or drive folder and
#'contain the variable name (as stated in `varnames`) and projection date in format "YYYY-MM-DD".
#'If more than one “tif” file in the Google Drive folder or local directory matches the projection
#'date and explanatory variable name, then the function will error.
#'
#'# Processing rasters
#'If required, rasters are cropped and resampled to the same spatial extent and
#'resolution. If `spatial.mask` is given, then cells with NA in this mask layer
#'are removed from the returned projection covariates. See `terra::mask()` in R
#'package `terra` for details (Hijmans et al., 2022).
#'
#'Rasters are then stacked and reprojected if `cov.prj` is different to `prj`.
#'
#'Note: if explanatory variable rasters are not of the same spatial resolution and extent, then the
#'function will error. Resample methods (`resample.method`) include:
#'
#'* `ngb`:  Each cell acquires the value of its nearest neighbour cell in the original raster. This
#'is typically used for categorical variables.
#'
#'* `bilinear`: the distance-weighted average of the four nearest cells are used to estimate a new
#'cell value. This is typically used for continuous variables.
#'
#'If only one `resample.method` is given, but these are more than one explanatory variables, the
#'same `resample.method` is used for all.
#'
#'# Output covariates
#'
#'The raster stacks are then converted into data frames or remain as raster stacks depending on
#'`cov.file.type`. Column names or raster layer names will be the unique explanatory variable names
#'(`varnames`). These are exported to the local directory or Google Drive folder with file names
#'containing the relevant projection date in "YYYY-MM-DD" format.
#'
#'# Google Drive compatibility
#'
#'If `drive.folder` or `save.drive.folder` given, please ensure the folder name is unique within
#'your Google Drive. Do not provide the path if the folder is nested within others.
#'
#'If one of `drive.folder` or `save.drive.folder` are used then user.email is required to access the
#'appropriate Google Drive user account. This requires users to have installed R package
#'`googledrive` and initialised Google Drive with valid log-in credentials. Please follow
#'instructions on <https://googledrive.tidyverse.org/>.
#'
#'
#'# Static rasters
#'
#'If static datasets are to be added into the dynamic projection covariates,
#'then five arguments are available to specify their inclusion.
#'
#'Please provide the static rasters in RasterStack format, specifying any
#'spatial buffering needed (using `static.moving.window.matrix` and
#'`static.GEE.math.fun` as described below).
#'
#'The resample method or methods for rasters within the static raster stack need
#'to be specified too using `static.resample.method`. Please also provide
#'`static.varnames` to name the static variables in the covariate data exported.
#'
#'#' # Spatial buffering of static rasters (optional)
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
#'@returns Exports combined covariates in "csv" or "tif" file for each projection date to the local
#'directory or Google Drive folder.
#'@export
#'@references
#'Hijmans, R.J., Bivand, R., Forner, K., Ooms, J., Pebesma, E. and Sumner, M.D.,
#'2022. Package 'terra'. Maintainer: Vienna, Austria.
#'@examplesIf googledrive::drive_has_token()
#'
#'data("sample_extent_data")
#'
#'# Set extraction variables
#'projectiondates <- dynamic_proj_dates("2018-01-01", "2018-12-01", interval = 3,interval.level =
#'"month")
#'variablenames <- c("eight_sum_prec", "year_sum_prec")
#'spatial.res.metres <- 500
#'cov_resolution <- 0.05
#'
#'# Get Google Drive email
#'user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
#'\dontshow{
#'projectiondates <- projectiondates[1]
#'spatial.res.metres <-20000
#'cov_resolution <- 5
#'}
#'extract_dynamic_raster(dates=projectiondates,
#'                       datasetname = "UCSB-CHG/CHIRPS/DAILY",
#'                       bandname="precipitation",
#'                       user.email = user.email,
#'                       spatial.res.metres = spatial.res.metres,
#'                       GEE.math.fun = "sum",
#'                       temporal.direction = "prior",
#'                       temporal.res = 56,
#'                       spatial.ext = sample_extent_data,
#'                       varname = variablenames[1],
#'                       save.directory = tempdir())
#'
#'
#'extract_dynamic_raster(dates=projectiondates,
#'                      datasetname = "UCSB-CHG/CHIRPS/DAILY",
#'                      bandname="precipitation",
#'                      user.email = user.email,
#'                      spatial.res.metres = spatial.res.metres,
#'                      GEE.math.fun = "sum",
#'                      temporal.direction = "prior",
#'                      temporal.res = 364,
#'                      spatial.ext = sample_extent_data,
#'                      varname = variablenames[2],
#'                      save.directory = tempdir())
#'
#'dynamic_proj_covariates(dates = projectiondates,
#'                        varnames = variablenames,
#'                        local.directory = tempdir(),
#'                        spatial.ext = sample_extent_data,
#'                        spatial.mask = sample_extent_data,
#'                        spatial.res.degrees = cov_resolution,
#'                        resample.method = c("bilinear","bilinear"),
#'                        cov.file.type = "csv",
#'                        prj="+proj=longlat +datum=WGS84",
#'                        save.directory = tempdir())
#'


dynamic_proj_covariates <- function(dates,
                                    varnames,
                                    drive.folder,
                                    user.email,
                                    local.directory,
                                    spatial.ext,
                                    spatial.mask,
                                    spatial.res.degrees,
                                    resample.method,
                                    cov.file.type,
                                    prj="+proj=longlat +datum=WGS84",
                                    cov.prj,
                                    save.directory,
                                    save.drive.folder,
                                    static.rasters,
                                    static.varnames,
                                    static.resample.method,
                                    static.moving.window.matrix,
                                    static.GEE.math.fun
                                    ) {

  # Check arguments to prevent error downstream

  if (missing(spatial.ext)) {
    message("spatial.ext missing. Rasters must have same extent.")
  }

  if (missing(cov.prj)) {
    cov.prj <- prj
  }

  if (missing(spatial.res.degrees)) {
    message("spatial.res.degrees missing. Rasters must have same resolution.")
  }


  if (!missing(spatial.res.degrees)) {
    if (missing(resample.method)) {stop("Provide a resample method(s).")}

    if (length(resample.method) != 1 &&
        length(resample.method) != length(varnames)) {
      stop("resample.method must be of length(1) or length(varnames).")
    }
  }

  if (missing(local.directory) && missing(drive.folder)) {
    stop("Provide local.directory or drive.folder to download rasters from.")
  }

  if (missing(save.directory) && missing(save.drive.folder)) {
    stop("Provide save.directory or save.drive.folder to export data.frame.")
  }


  # Process spatial extent given for cropping rasters before stacking into covariate data frame

  # Check spatial.ext appropriate object class.

  if (!missing(spatial.ext)) {


    # Numeric extent to co-ords
    if (is.numeric(spatial.ext) && length(spatial.ext) != 4) {
      stop("spatial.ext vector should be length 4: xmin, xmax, ymin, ymax.")
    }

    xmin <- extract_xy_min_max(spatial.ext)[1]
    xmax <- extract_xy_min_max(spatial.ext)[2]
    ymin <- extract_xy_min_max(spatial.ext)[3]
    ymax <- extract_xy_min_max(spatial.ext)[4]

  }

  # If drive.folder given intitate Google Drive and list files in the folder(s)

  if (!missing(drive.folder)) {
    # Check user email provided
    if (missing(user.email)) {
      stop("Provide user email linked to Google Drive account.")
    }

    # Initiate Google Drive
    googledrive::drive_auth(email = user.email)
    googledrive::drive_user()


    # List all files in Google Drive folders
    drivefiles = NULL

    for (folder in 1:length(drive.folder)) {

      # Check folder exists in user's Google Drive
      folderpath <- googledrive::drive_find(pattern = drive.folder[folder], type = 'folder')

      if(nrow(folderpath)>1) {
        folderpath <- folderpath[grep(paste0("^", drive.folder[folder], "$"),
                                      folderpath$name), ]
      }

      if (nrow(folderpath) == 0) {stop("drive.folder does not exist.")}

      drivefiles <- rbind(drivefiles,
                          googledrive::drive_ls(path = googledrive::as_id(folderpath$id))[,1:2])


    }
  }

  # If local.directory list files in the folder(s)

  if (!missing(local.directory)) {
    directoryfiles <- list.files(local.directory, full.names = TRUE)
  }






  #-----------------------------------------------------------------
  # Process static
  #-----------------------------------------------------------------
  if (!missing(static.rasters)) {

    if(inherits(static.rasters, "RasterLayer")){
      static.rasters <- terra::rast(static.rasters)
    }

    if(inherits(static.rasters, "RasterStack")){
      static.rasters <- terra::rast(static.rasters)
    }

    if (!inherits(static.rasters, "SpatRaster")) {
      stop("Please provide static rasters as SpatRaster object")
    }

  if (!terra::nlyr(static.rasters) == length(static.varnames)) {
    stop("Provide unique name for each layer in raster stack")
  }


  if(!missing(static.moving.window.matrix)){

    if (missing(static.GEE.math.fun)) {
      stop("Please provide math function for spatial buffering")
    }}

  static.raster.stack<- vector("list",(length(static.varnames) + length(varnames)))

  for (sr in 1:length(static.varnames)) {

    name <- static.varnames[sr]

    raster <- static.rasters[[sr]] # Read raster from local dir

    terra::crs(raster) <- prj # Check that projection is set

    if(!missing(static.moving.window.matrix)){

      if (missing(static.GEE.math.fun)) {
        stop("Please provide math function for spatial buffering of static")
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

      static.GEE.math.fun <- match.arg(arg = static.GEE.math.fun, choices = namelist)

      # Match GEE.math.fun argument to analogous R function
      math.fun <- R.FUNC[[match(static.GEE.math.fun, namelist)]]


      raster<- terra::focal(raster,
                             static.moving.window.matrix,
                             fun = math.fun,
                             na.rm = TRUE)


    }


    # Crop to spatial.ext provided so that all rasters are same extent for stacking
    if (!missing(spatial.ext)) {

      # Create raster of desired extent and resolution
      r <- terra::rast(terra::ext(xmin, xmax, ymin, ymax))

      terra::res(r) <- spatial.res.degrees
      r <- terra::setValues(r, 1:terra::ncell(r))

      if(!missing(spatial.mask)) {

        if(inherits(spatial.mask, "sfc_POLYGON")){
          spatial.mask <- terra::vect(spatial.mask)
        }

        if(inherits(spatial.mask, "sfc_MULTIPOLYGON")){
          spatial.mask <- terra::vect(spatial.mask)
        }

        if("sf" %in% class(spatial.mask)){
          spatial.mask <- terra::vect(spatial.mask)
        }

        # Mask to original spatial.ext, if fails keep original. Depending on spatial.ext type.
        tryCatch({
          r <- terra::mask(r, spatial.mask)
        }, error = function(error_message) {
          r <- r
          message("spatial.mask could not be used. Check valid input type.")
        })}

      # Resample raster using single method given
      if (length(static.resample.method) == 1) {
        raster <- terra::resample(raster, r, method = static.resample.method)
      }

      # Resample raster using variable specific method given
      if (!length(static.resample.method) == 1) {
        raster <- terra::resample(raster, r, method = static.resample.method[sr])
      }

      if(!missing(spatial.mask)) {
        tryCatch({
          raster <- terra::mask(raster, spatial.mask)
        }, error = function(error_message) {
          raster <- raster
          message("spatial.mask could not be used. Check valid input type.")
        })}


    }

    terra::crs(raster) <- prj

    static.raster.stack[[sr]] <- raster # Add raster to stack

  }



  }




  # Iterate through each projection date and variable.

  listofdone <- as.Date("2000-01-01") # Date vector to record completed (removed before return)

  for (x in 1:length(dates)) {
    date <- dates[x]

    stack <- vector("list",length(varnames)) # Empty stack to bind rasters for this date to
    start <- 1
    end <- length(varnames)

    if (!missing(static.rasters)) {
      stack <- static.raster.stack
      start <- length(static.varnames)+1
      end <-  length(static.varnames) + length(varnames)
      varnames <- c(static.varnames,varnames)
    }

    for (v in start:end) {

      name <- varnames[v]

      # Read in raster for this variable and date from Google Drive

      if (!missing(drive.folder)) {

        # Extract the ID for the file (prevents error if duplicate named files within Drive
        fileid <- drivefiles[grep(name, drivefiles$name),]
        fileid <- fileid[grep(date, fileid$name),"id"]

        pathforthisfile <- paste0(tempfile(), ".tif") # Create temp file name

        googledrive::drive_download(file = googledrive::as_id(fileid$id)[1],
                                    path = pathforthisfile,
                                    overwrite = TRUE) # Download from Drive

        raster <- terra::rast(pathforthisfile)# Read raster from temp dir

        terra::crs(raster) <- prj # Check that projection is set
      }

      # Alternatively, read in raster for this variable and date from local directory
      if (!missing(local.directory)) {
        fileimport <- directoryfiles[grep(name, directoryfiles)]
        fileimport <- fileimport[grep(date, fileimport)] # Select files
        raster <- terra::rast(fileimport) # Read raster from local dir
        terra::crs(raster) <- prj # Check that projection is set
      }

      # Crop to spatial.ext provided so that all rasters are same extent for stacking
      if (!missing(spatial.ext)) {

        # Create raster of desired extent and resolution
        r <- terra::rast(terra::ext(xmin, xmax, ymin, ymax))
        terra::res(r) <- spatial.res.degrees
        r <- terra::setValues(r, 1:terra::ncell(r))


        if(!missing(spatial.mask)) {

          if(inherits(spatial.mask, "sfc_POLYGON")){
            spatial.mask <- terra::vect(spatial.mask)
          }

          if(inherits(spatial.mask, "sfc_MULTIPOLYGON")){
            spatial.mask <- terra::vect(spatial.mask)
          }

          # Mask to original spatial.ext, if fails keep original. Depending on spatial.ext type.
          tryCatch({
            r <- terra::mask(r, spatial.mask)
          }, error = function(error_message) {
            r <- r
            message("spatial.mask could not be used. Check valid input type.")
          })}

        # Resample raster using single method given
        if (length(resample.method) == 1) {
          raster <- terra::resample(raster, r, method = resample.method)
        }

        # Resample raster using variable specific method given
        if (!length(resample.method) == 1) {
          raster <- terra::resample(raster, r, method = resample.method[v])
        }

        if(!missing(spatial.mask)) {
        # Mask to original spatial.ext, if fails keep original. Depending on spatial.ext type.
        tryCatch({
          raster <- terra::mask(raster, spatial.mask)
        }, error = function(error_message) {
          raster <- raster
          message("spatial.mask could not be used. Check valid input type.")
        })}


      }

      terra::crs(raster) <- prj

      stack[[v]] <- raster # Add raster to stack

    }

    stack <- terra::rast(stack)

    names(stack) <- varnames # Label each layer in stack as variable

    if (!prj == cov.prj) {
      stack <- terra::project(stack, cov.prj)
    }

    cov.file.type <- match.arg(cov.file.type, choices = c("tif", "csv"))

    # Save covariate data frame to Google Drive folder
    if (!missing(save.drive.folder)) {
      # Check user email provided
      if (missing(user.email)) {
        stop("Please provide user email linked to Google Drive account")
      }

      # Initiate Google Drive
      googledrive::drive_auth(email = user.email)
      googledrive::drive_user()

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

      if (cov.file.type == "csv") {

        stack_df <- as.data.frame(cbind(terra::xyFromCell(stack,
                                                          1:terra::ncell(stack)),
                                        terra::values(stack)))

        csvfile <- paste0(tempfile(), ".csv")

        if (!missing(save.directory)) {
          csvfile <- paste0(save.directory, "/", date, "_projection_dataframe.csv")
        }

        utils::write.csv(stack_df, file = csvfile, row.names = FALSE) # Save to temporary location

        googledrive::drive_upload( # Upload to Google Drive
          media = csvfile,
          path = googledrive::as_id(save.folderpath$id),
          name = paste0(date, "_projection_dataframe.csv"),
          overwrite = TRUE)}

      if (cov.file.type == "tif") {

        # Save to temporary location before Google Drive upload
        rasterfile <- paste0(tempfile(), ".tif")

        stack_rast <- stars::st_as_stars(stack)

        # By writing with the stars package we keep band layer names for projections
        stars::write_stars(stack_rast, rasterfile)

        # Upload to Google Drive
        googledrive::drive_upload(media = rasterfile,
                                  path = googledrive::as_id(save.folderpath$id),
                                  name = paste0(date, "_projection_rasterstack.tif"),
                                  overwrite = TRUE)}
    }

    # Alternatively save covariate data.frame to local directory

    if (!missing(save.directory)) {
      if (!dir.exists(save.directory)) {
        stop("save.directory does not exist")
      }

      if (cov.file.type == "csv") {

        stack_df <- as.data.frame(cbind(terra::xyFromCell(stack,
                                                          1:terra::ncell(stack)),
                                        terra::values(stack)))

        csvfile <- paste0(tempfile(), ".csv")

        if (!missing(save.directory)) {
          csvfile <- paste0(save.directory, "/", date, "_projection_dataframe.csv")
        }

        utils::write.csv(stack_df, file = csvfile, row.names = FALSE) # Save to temporary location
      }


      if (cov.file.type == "tif") {

        stack_rast <- stars::st_as_stars(stack)

        stars::write_stars(stack_rast, paste0(save.directory,
                                              "/",
                                              date,
                                              "_projection_rasterstack.tif"))


      }}

    listofdone <- c(listofdone, dates[x]) # Record that this date has been completed
  }


  return(listofdone[2:length(listofdone)])
}
