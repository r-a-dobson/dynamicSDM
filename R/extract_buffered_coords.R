#'Extract spatially buffered and temporally dynamic explanatory variable data for occurrence
#'records.
#'
#'For each species occurrence record co-ordinate and date, spatially buffered and temporally dynamic
#'explanatory data are extracted using Google Earth Engine.
#'@param occ.data a data frame, with columns for occurrence record co-ordinates and dates with
#'  column names as follows; record longitude as "x", latitude as "y", year as "year", month as
#'  "month", and day as "day".
#'@param datasetname a character string, the Google Earth Engine dataset to extract data from.
#'@param bandname a character string, the Google Earth Engine dataset bandname to extract data for.
#'@param spatial.res.metres a numeric value, the spatial resolution in metres for data extraction.
#'@param GEE.math.fun a character string, the mathematical function to compute across the specified
#'  spatial matrix and period for each record.
#'@param moving.window.matrix a matrix of weights with an odd number of sides, representing the
#'  spatial neighbourhood of cells (“moving window”) to calculate `GEE.math.fun` across from record
#'  co-ordinate. See details for more information.
#'@param user.email a character string, user email for initialising Google Drive.
#'@param save.method a character string, the method used to save extracted variable data. One of
#'  `split` or `combined`: can be abbreviated. See details.
#'@param temporal.level a character string, the temporal resolution of the explanatory variable
#'  data. One of `day`, `month` or `year`: can be abbreviated. Default; `day.`
#'@param save.directory a character string, path to a local directory to save extracted variable
#'  data to.
#'@param varname optional; a character string, a unique name for the explanatory variable. Default
#'  varname is “bandname_temporal.res_temporal.direction_GEE.math.fun_buffered".
#'@param temporal.res optional; a numeric value, the temporal resolution in days to extract data and
#'  calculate `GEE.math.fun` across from occurrence record date.
#'@param temporal.direction optional; a character string, the temporal direction for extracting data
#'  across relative to the record date. One of `prior` or `post`: can be abbreviated.
#'@param categories optional; a character string, the categories to use in calculation if data are
#'  categorical. See details for more information.
#'@param agg.factor optional;a postive integer, the aggregation factor expressed as number of cells
#'  in each direction. See details.
#'@param prj a character string, the coordinate reference system of `occ.data` coordinates.Default
#'  is "+proj=longlat +datum=WGS84".
#'@param resume a logical indicating whether to search `save.directory` and return to previous
#'  progress. Only possible if save.method = `split` has previously and currently been employed.
#'  Default = TRUE.
#'@details For each individual species occurrence record co-ordinate and date, this function
#'  extracts data for a given band within a Google Earth Engine dataset across a user-specified
#'  spatial buffer and temporal period and calculates a mathematical function on such data.
#'
#'  # Temporal dimension
#'
#'  If `temporal.res` and `temporal.direction` are not given, the function
#'  extracts explanatory variable data for all of the cells surrounding and including the cell
#'  containing the occurrence record co-ordinates.
#'
#'  If `temporal.res` and `temporal.direction` is given, the function extracts explanatory variable
#'  data for which `GEE.math.fun` has been first calculated over this period in relation to the
#'  occurrence record date.
#'
#'  # Spatial dimension
#'
#'  Using the `focal` function from `terra` R package (Hijmans et al., 2022), `GEE.math.fun` is
#'  calculated across the spatial buffer area from the record co-ordinate. The spatial buffer area
#'  used is specified by the argument `moving.window.matrix`, which dictates the neighbourhood of
#'  cells surrounding the cell containing the occurrence record to include in this calculation.
#'
#'  See function `get_moving_window()` to generate appropriate `moving.window.matrix`.
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
#'  When explanatory variable data are categorical (e.g. land cover classes), argument `categories`
#'  can be used to specify the categories of importance to the calculation. The category or
#'  categories given will be converted in a binary representation, with “1” for those listed, and
#'  “0” for all others in the dataset. Ensure that the `GEE.math.fun` given is appropriate for such
#'  data. For example, the sum of suitable land cover classified cells across the “moving window”
#'  from the species occurrence record co-ordinates.
#'
#'  # Categorical data and temporally dynamic variables
#'
#'  Please be aware, if specific categories are given (argument `categories`) when extracting
#'  categorical data, then temporal buffering cannot be completed. The most recent categorical data
#'  to the occurrence record date will be used for spatial buffering.
#'
#'  If specific categories are not given when extracting from categorical datasets, be careful to
#'  choose appropriate mathematical functions for such data. For instance, "first" or "last" may be
#'  more relevant that "sum" of land cover classification numbers.
#'
#'
#'  # Temporal level to extract data at:
#'
#'  `temporal.level` states the temporal resolution of the explanatory variable data and improves
#'  the speed of `extract_buffered_coords()` extraction. For example, if the explanatory data
#'  represents an annual variable, then all record co-ordinates from the same year can be extracted
#'  from the same buffered raster, saving computation time. However, if the explanatory data
#'  represents a daily variable, then only records from the exact same day can be extracted from the
#'  same raster. For the former, `temporal.level` argument should be `year` and for the latter,
#'  `temporal.level` should be `day`.
#'
#'  # Aggregation factor
#'
#'  `agg.factor` given represents the factor to aggregate `RasterLayer` data with function
#'  `aggregate` in `terra` R package (Hijmans et al., 2022). Aggregation uses the `GEE.math.fun` as
#'  the function. Following aggregation spatial buffering using the moving window matrix occurs.
#'  This is included to minimise computing time if data are of high spatial resolution and a large
#'  spatial buffer is needed. Ensure to calculate `get_moving_window()` with the spatial resolution
#'  of the data post-aggregation by this factor.
#'
#'
#'  # Google Earth Engine
#'
#'  `extract_buffered_coords()` requires users to have installed R package `rgee` (Aybar et al.,
#'  2020) and initialised Google Earth Engine with valid log-in credentials. Please follow
#'  instructions on the following website <https://cran.r-project.org/package=rgee>
#'
#'  * `datasetname` must be in the accepted Google Earth Engine catalogue layout (e.g.
#'  “MODIS/006/MCD12Q1” or “UCSB-CHG/CHIRPS/DAILY”)
#'
#'  * `bandname` must be as specified under the
#'  dataset in the Google Earth Engine catalogue (e.g. “LC_Type5”, “precipitation”). For datasets
#'  and band names, see <https://developers.google.com/earth-engine/datasets>.
#'
#'  # Google Drive
#'
#'  `extract_buffered_coords()` also requires users to have installed the R package
#'  `googledrive`(D'Agostino McGowan and Bryan, 2022) and initialised Google Drive with valid log-in
#'  credentials, which must be stated using argument `user.email`. Please follow instructions on
#'  <https://googledrive.tidyverse.org/> for initialising the `googledrive` package.
#'
#' Note: When running this function a folder labelled "dynamicSDM_download_bucket" will be created
#' in your Google Drive. This will be emptied once the function has finished running and output
#' rasters will be found in the save.drive.folder or save.directory specified.
#'
#'  # Exporting extracted data
#'
#'  For `save.method` = `combined`, the function with save “csv” files containing all occurrence
#'  records and associated values for the explanatory variable.
#'
#'  For `save.method` = `split`, the function will save individual “csv” files for each record with
#'  each unique period of the given temporal.level (e.g. each year, each year and month combination
#'  or each unique date).
#'
#'  `split` protects users if internet connection is lost when extracting data for large occurrence
#'  datasets. The argument `resume` can be used to resume to previous progress if connection is
#'  lost.
#'
#'@references Aybar, C., Wu, Q., Bautista, L., Yali, R. and Barja, A., 2020. rgee: An R package for
#'  interacting with Google Earth Engine. Journal of Open Source Software, 5(51), p.2272.
#'
#'  D'Agostino McGowan L., and Bryan J., 2022. googledrive: An Interface to Google Drive.
#'  <https://googledrive.tidyverse.org>, <https://github.com/tidyverse/googledrive>.
#'
#'Hijmans, R.J., Bivand, R., Forner, K., Ooms, J., Pebesma, E. and Sumner, M.D.,
#'2022. Package 'terra'. Maintainer: Vienna, Austria.
#'@return Returns details of successful explanatory variable extractions.
#'@export
#'@examplesIf googledrive::drive_has_token()
#'
#'data(sample_filt_data)
#'
#'\dontshow{
#'sample_filt_data<-sample_filt_data[1,]
#'}
#'
#'user.email<-as.character(gargle::gargle_oauth_sitrep()$email)
#'
#'matrix<-get_moving_window(radial.distance = 10000,
#'                             spatial.res.degrees = 0.05,
#'                             spatial.ext = sample_extent_data)
#'
#' extract_buffered_coords(occ.data = sample_filt_data,
#'                       datasetname = "MODIS/006/MCD12Q1",
#'                       bandname = "LC_Type5",
#'                       spatial.res.metres = 500,
#'                       GEE.math.fun = "sum",
#'                       moving.window.matrix=matrix,
#'                       user.email = user.email,
#'                       save.method ="split",
#'                       temporal.level = "year",
#'                       categories = c(6,7),
#'                       agg.factor = 12,
#'                       varname = "total_grass_crop_lc",
#'                       save.directory = tempdir()
#' )

extract_buffered_coords <-  function(occ.data,
                                     datasetname,
                                     bandname,
                                     spatial.res.metres,
                                     GEE.math.fun,
                                     moving.window.matrix,
                                     user.email,
                                     save.method,
                                     varname,
                                     temporal.res,
                                     temporal.level,
                                     temporal.direction,
                                     categories,
                                     save.directory,
                                     agg.factor,
                                     prj = "+proj=longlat +datum=WGS84",
                                     resume = TRUE) {



    # Check user email provided
    if (missing(user.email)) {stop("Provide email linked to Google Drive")}

    # Check formatting of arguments to avoid common errors
    if (!length(GEE.math.fun) == 1) {stop("Only provide one GEE.math.fun")}

    if (!dir.exists(save.directory)) {stop("save.directory not found")}

    # Check arguments match available options
    save.method <- match.arg(arg = save.method, choices = c("split", "combined"))

    temporal.level <- match.arg(arg = temporal.level, choices = c("day", "month", "year"))

    if (!missing(temporal.res)) {

      if (missing(temporal.direction)) {
        stop("temporal.direction missing.")
      }

      temporal.direction <-
        match.arg(arg = temporal.direction, choices = c("prior", "post"))
    }

    if (missing(temporal.res)) {
      message("temporal.res missing, assuming static")
    }

    #Set default varname
    if (missing(varname)) {
      if (!missing(temporal.res)) {
        varname <- paste0(bandname,
                          "_",
                          temporal.res,
                          "_",
                          temporal.direction,
                          "_",
                          GEE.math.fun,
                          "_buffered")

        message(paste0("Default varname: ",varname))
      }

      if (missing(temporal.res)) {
        varname <- paste0(bandname, "_", GEE.math.fun, "_buffered")
        message(paste0("Default varname: ", varname))
      }
    }

    # Load Google Drive
    ee <- reticulate::import("ee")
    googledrive::drive_auth(email = user.email)
    googledrive::drive_user()


    # Load Google Earth Engine
    rgee::ee_check("rgee")
    rgee::ee_Initialize(drive = TRUE)


    # Assign occurrence record a unique ID for saving files.
    occ.data$unique.ID.DYN <- rep(1:nrow(occ.data))


    # Split occurrence by temporal.level
    # e.g. if annual can use raster data from each year at once, saving time

    if (temporal.level == "day") {
      uniqueocc <- unique(occ.data[, c("day", "month", "year")])
    }

    if (temporal.level == "month") {
      uniqueocc <- unique(occ.data[, c("month", "year")])
    }

    if (temporal.level == "year") {
      uniqueocc <- data.frame(unique(occ.data[, c("year")]))
      colnames(uniqueocc) <- "year"
    }

    # For each uniqueocc step, download appropriate raster from GEE
    # cropped to minimum extent needed to cover all co-ordinates in this step

    combined_data_set = NULL
    rowscomplete = NULL

    for (x in 1:nrow(uniqueocc)) {

      if (temporal.level == "year") {
        year <- as.numeric(uniqueocc[x, "year"])
        month <-  1 # Set arbitrary values as need complete date for GEE and
        day <- 1 # only the year actually matters here.
        occforperiod <- occ.data[occ.data$year == year, ]
        nameofsplitfile <- year
      }

      if (temporal.level == "month") {
        year <- as.numeric(uniqueocc[x, "year"])
        month <- as.numeric(uniqueocc[x, "month"]) # Add leading zero for date
        day <- 1 # Set arbitrary values as need complete date for GEE and
                # only year and month matters here.
        occforperiod <- occ.data[occ.data$year == year, ]
        occforperiod <- occforperiod[occforperiod$month == month, ]
        nameofsplitfile <- paste0(year, "-", sprintf("%02d", month))
      }

      if (temporal.level == "day") {
        year <- as.numeric(uniqueocc[x, "year"])
        month <- as.numeric(uniqueocc[x, "month"])
        day <- as.numeric(uniqueocc[x, "day"]) # Use dates from each recored
        occforperiod <- occ.data[occ.data$year == year, ]
        occforperiod <- occforperiod[occforperiod$month == month, ]
        occforperiod <- occforperiod[occforperiod$day == day, ]
        nameofsplitfile <- paste0(year,
                                  "-",
                                  sprintf("%02d", month),
                                  "-",
                                  sprintf("%02d", day))
      }


      date1 <- as.Date(paste(year, month, day, sep = "-"), "%Y-%m-%d")

      # If the extraction lost connection, this resumes the loop to where it had previously reached
      if (save.method == "split") {
        if (resume) {
          # Checks if output file for this loop has already been written
          if (file.exists(paste0(save.directory,
                                 "/",
                                 nameofsplitfile,
                                 "_",
                                 varname,
                                 ".csv"))) {
            next()
          }}
      }



      # Work out minimum area to download for extraction of moving window matrix
      # around each occurrence record (to minimise computing time)

      # Using data resolution and matrix cell length, calc radius from occ
      # occurrence co-ordinate to edge of matrix. (circumference/2)
      spatial.buffer <- nrow(moving.window.matrix) * spatial.res.metres / 2


      # If aggregation factor is given, the moving window matrix will give how many cells of the
      # aggregated size to calculate spatial buffer function over. Therefore, we need to work out
      # number of spatial.res.metre size cells this will involve pre-aggregation
      if(!missing(agg.factor)){
       spatial.buffer <- (nrow(moving.window.matrix) * agg.factor * spatial.res.metres) /2
      }

      # The matrix is square not circular, so then need to work out the radius
      # to furthest corner of matrix away from co-ordinate.
      # Rearrange pythagorus theorem (a2 + b2 = c2),
      # so c = square root of a2 + b2 (a and b= radius from previous calc)
      spatial.buffer <- sqrt((spatial.buffer ^ 2) + (spatial.buffer ^ 2))

      ## To ensure that all cells are definitely included round to nearest 5
      spatial.buffer <- ceiling(spatial.buffer / 5) * 5

      # From each occurrence record point in this step, add the radius

      spatial.buffer<- sf_buffer(occforperiod,spatial.buffer,prj)

      # Buffering is completed in a different CRS, reproject output to user set prj
      spatial.buffer <-sf::st_transform(spatial.buffer,crs=prj)


      # Extract min and max longtiude and latitude co-ordinates
      # This is the minimum possible area to extract from environmental dataset
      xmin <- sf::st_bbox(spatial.buffer)[1]
      xmax <- sf::st_bbox(spatial.buffer)[3]
      ymin <- sf::st_bbox(spatial.buffer)[2]
      ymax <- sf::st_bbox(spatial.buffer)[4]

      # Create GEE Geometry Polygon using min and max co-ordinates
      geometry <- ee$Geometry$Polygon(list(c(xmin ,  ymin),
                                           c(xmin , ymax),
                                           c(xmax, ymax),
                                           c(xmax,  ymin)))
      # Static variable
      if (missing(temporal.res) ||  !missing(categories)) {

        date1 <- as.character(date1)

        # Create GEE ImageCollection for specified dataset, band and date
        image_collection <-
          ee$ImageCollection(paste0(datasetname))$
          filterDate(date1)$select(paste0(bandname))

        # As static, only one Image in ImageCollection, so select the first Image
        image_collection_reduced <- image_collection$reduce(ee$Reducer$first())
      }

 if(missing(categories)){
      # Temporally dynamic variable
      if (!missing(temporal.res)) {
        firstdate <- date1

        # Prior selected so minus temporal.res days from the record date
        if (temporal.direction == "prior") {
          seconddate <- as.character(date1 - temporal.res)
          firstdate <- as.character(firstdate)

          # ImageCollection of specified dataset and band between two dates
          image_collection <- ee$ImageCollection(paste0(datasetname))$
            filterDate(seconddate, firstdate)$
            select(paste0(bandname))
        }

        # Post selected so minus temporal.res days from the record date
        if (temporal.direction == "post") {
          seconddate <- as.character(date1 + temporal.res)
          firstdate <- as.character(firstdate)

          # ImageCollection of specified dataset and band between two dates
          image_collection <- ee$ImageCollection(paste0(datasetname))$
            filterDate(firstdate, seconddate)$
            select(paste0(bandname))
        }

        # This is a list of all GEE ImageCollection Reducer functions available
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

        # Character names matching the GEE Reducer functions
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

    # Reduce ImageCollection using function specified in GEE.math.fun
    image_collection_reduced <- image_collection$reduce(GEE.FUNC[[match(GEE.math.fun, namelist)]])
      }}

      # Download raster of Reduced ImageCollection to Google Drive
      tryCatch({
        rgee::ee_as_raster(
          image = image_collection_reduced,
          container = "dynamicSDM_download_bucket",
          scale = spatial.res.metres,
          dsn = paste0(tempdir(),"/",varname, "_", date1),
          region = geometry,
          timePrefix = FALSE,
          via = "drive"
        )
      }, error = function(e) {
        message("ERROR :", conditionMessage(e), "\n")
      })

      # Get temp file name for downloading raster
      pathforthisfile <- paste0(tempfile(), ".tif")

      # Authenticate Google Drive
      googledrive::drive_auth(email = user.email)
      googledrive::drive_user()



      # List all files in Google Drive folders
      folderpath <- googledrive::drive_find(pattern = "dynamicSDM_download_bucket", type = 'folder')
      drivefiles <- googledrive::drive_ls(path = googledrive::as_id(folderpath$id))

      # Get ID of most recent file in bucket with this name (prevents errors if duplicate)
      drivefiles <- drivefiles[grep(paste0(varname, "_", date1, ".tif"),drivefiles$name),"id"][1,]

      # Download raster from Google Drive to temp directory for processing
      googledrive::drive_download(googledrive::as_id(drivefiles$id),
                                  path = pathforthisfile,
                                  overwrite = TRUE)
      raster <- terra::rast(pathforthisfile) # Read in raster from temp dir


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


      # Process categorical raster

      if (!missing(categories)) {

        # Convert the categorical raster into binary 1 (specified) & 0 (other)
        rast <- raster == categories[1]

        # If more than one category, iterate through each category and add binary rasters together
        if (length(categories) > 1) {
          for (cat in 2:length(categories)) {
            rast <- rast + (raster == categories[cat])
          }
        }

        if(!missing(agg.factor)) {
          rast <- terra::aggregate(rast, agg.factor, fun = math.fun, na.rm = TRUE)
        }

        # If data are categorical then matrix weights must = 1
        moving.window.matrix[1:nrow(moving.window.matrix),
                             1:ncol(moving.window.matrix)] <- 1

        ## Calculate math.fun across moving.window.matrix for the raster
        focalraster <- terra::focal(rast,
                                     moving.window.matrix,
                                     fun = math.fun,
                                     na.rm = TRUE)
      }


      # Process continuous data raster
      # Calculate math.fun across moving.window.matrix for the raster
      if (missing(categories)) {

        if(!missing(agg.factor)) {
          raster <- terra::aggregate(raster, agg.factor, fun = math.fun, na.rm=TRUE)
        }

        focalraster <- terra::focal(raster,
                                     moving.window.matrix,
                                     fun = math.fun, na.rm=TRUE)
      }

      # Extract value at co-ordinates of each occurrence record
      extracted_data <- as.data.frame(terra::extract(focalraster,
                                                      as.matrix(cbind(
                                                        occforperiod[, "x"],
                                                        occforperiod[, "y"]
                                                      )))[,1])

      colnames(extracted_data) <- varname

      extracted_data <- as.data.frame(cbind(occforperiod, extracted_data))

      # If split chosen, save individual .csv file with record and value
      if (save.method == "split") {
        write.csv(extracted_data,
                  file = paste0(save.directory,
                                "/",
                                nameofsplitfile,
                                "_",
                                varname,
                                ".csv"),
                  row.names = FALSE
        )


      }

      if (save.method == "combined") {
        combined_data_set <- rbind(combined_data_set, extracted_data)
      }

      # Record uniqueID of records explanatory data extracted for in this loop
      rowscomplete <- c(rowscomplete, nameofsplitfile)
      message(paste0("Completed: ",nameofsplitfile))

    }

    # Print names of occurrence records data successfully extracted for
    if (save.method == "split") {

      message("Clearing Google Drive download bucket - dynamicSDM_download_bucket")

      rgee::ee_clean_container(name="dynamicSDM_download_bucket",type="drive")


      return(sort(rowscomplete))
    }


    # Save combined data.frame to given directory
    if (save.method == "combined") {
      write.csv(combined_data_set,
                row.names = FALSE,
                file = paste0(save.directory,
                              "/all_records_combined_",
                              varname,
                              ".csv")
      )


      message("Clearing Google Drive download bucket - dynamicSDM_download_bucket")
      rgee::ee_clean_container(name="dynamicSDM_download_bucket",type="drive")

      return(combined_data_set)
    }
  }

