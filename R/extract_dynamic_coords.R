#' Extract temporally dynamic explanatory variable data for occurrence records.
#'
#' For each species occurrence record co-ordinate and date, temporally dynamic explanatory data are
#' extracted using Google Earth engine
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with
#'   column names as follows; record longitude as "x", latitude as "y", year as "year", month as
#'   "month", and day as "day".
#' @param datasetname a character string, the Google Earth Engine dataset to extract data from.
#' @param bandname a character string, the Google Earth Engine dataset bandname to extract data for.
#' @param spatial.res.metres a numeric value, the spatial resolution in metres for data extraction.
#' @param GEE.math.fun a character string, the mathematical function to compute across the
#'   `temporal.res` period for each record.
#' @param save.method a character string, the method used to save extracted variable data. One of
#'   `split` or `combined`: can be abbreviated. See details.
#' @param temporal.res a numeric value, the temporal resolution in days to extract data and
#'   calculate `GEE.math.fun` across from each record's date.
#' @param temporal.direction a character string, the temporal direction for extracting data across
#'   relative to the record date. One of `prior` or `post`: can be abbreviated.
#' @param varname optional; a character string, the unique name for the explanatory variable.
#'   Default varname is "bandname_temporal.res_temporal.direction_ GEE.math.fun".
#' @param save.directory a character string, the path to a local directory to save extracted
#'   variable data to.
#' @param resume a logical indicating whether to search `save.directory` and start from previous
#'   progress by function. Only possible if `save.method` = `split` has been used.
#' @details
#' For each individual species occurrence record co-ordinate and date, this function extracts data
#' for a given band within a Google Earth Engine dataset across a user-specified period and
#' calculates a mathematical function on such data.
#'
#'  # Google Earth Engine
#'
#'  `extract_dynamic_coords()` requires users to have installed R package `rgee` (Aybar et al.,
#'  2020) and initialised Google Earth Engine with valid log-in credentials. Please follow
#'  instructions on the following website <https://cran.r-project.org/package=rgee>.
#'
#'  * `datasetname` must be in the accepted Google Earth Engine catalogue layout (e.g.
#'  `"MODIS/006/MCD12Q1"` "or `"UCSB-CHG/CHIRPS/DAILY"`)
#'
#'  * `bandname` must be as specified under the
#'  dataset in the Google Earth Engine catalogue (e.g. `"LC_Type5"`, `"precipitation"`). For
#'  datasets
#'  and band names, see <https://developers.google.com/earth-engine/datasets>.
#'
#'
#'  # Mathematical function
#'
#'  `GEE.math.fun` specifies the mathematical function to be calculated over the temporal period
#'  from each record's date. Options are limited to Google Earth Engine ImageCollection Reducer
#'  functions (<https://developers.google.com/earth-engine/apidocs/>) for which an analogous R
#'  function is available. This includes: "allNonZero","anyNonZero", "count",
#'  "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product",
#'  "sampleStdDev", "sampleVariance", "stdDev", "sum" and "variance".
#'
#'  # Categorical data
#'
#'  Please be aware, at current this function does not support the extraction of temporally dynamic
#'  variables for specific categories within categorical datasets.
#'
#'  When extracting from categorical datasets, be careful to choose appropriate mathematical
#'  functions for such data. For instance, "first" or "last" may be more relevant that "sum" of land
#'  cover classification numbers.
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
#'@references Aybar, C., Wu, Q., Bautista, L., Yali, R. and Barja, A., 2020. rgee: An R package for
#'interacting with Google Earth Engine. Journal of Open Source Software, 5(51), p.2272.
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
#'extract_dynamic_coords(occ.data=sample_filt_data,
#'  datasetname = "UCSB-CHG/CHIRPS/DAILY",
#'  bandname="precipitation",
#'  spatial.res.metres = 5566 ,
#'  GEE.math.fun = "sum",
#'  temporal.direction = "prior",
#'  temporal.res = 364,
#'  save.method = "split",
#'  resume = TRUE,
#'  varname = "total_annual_precipitation_prior",
#'  save.directory= tempdir())
#'
#'


extract_dynamic_coords <- function(occ.data,
                                   datasetname,
                                   bandname,
                                   spatial.res.metres,
                                   GEE.math.fun,
                                   save.method,
                                   temporal.res,
                                   temporal.direction,
                                   varname,
                                   resume=FALSE,
                                   save.directory
                                   ) {

    #Set default varname
    if (missing(varname)) {
      varname <- paste0(bandname,
                        "_",
                        temporal.res,
                        "_",
                        temporal.direction,
                        "_",
                        GEE.math.fun)
      message(paste0("Default varname: ", varname))
    }

    # Initial argument formatting checks
    if (!length(GEE.math.fun) == 1) {
      stop("Only provide one GEE.math.fun")
    }
    if (!dir.exists(save.directory)) {
      stop("save.directory does not exist")
    }

    if (!is.character(datasetname)) {
      stop("datasetname must be of class character")
    }
    if (!is.character(bandname)) {
      stop("bandname must be of class character")
    }

    if (!is.numeric(spatial.res.metres)) {
      stop("spatial.res.metres must be of class numeric")
    }

    if (!is.numeric(temporal.res)) {
      stop("temporal.res must be of class numeric")
    }


    # Match choices, ensure argument options are valid

    save.method <- match.arg(arg = save.method,
                             choices = c("split", "combined"))

    temporal.direction <- match.arg(arg = temporal.direction,
                                    choices = c("prior", "post"))

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
    # Match GEE.math.fun to available options
    GEE.math.fun <- match.arg(arg = GEE.math.fun, choices = namelist)

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


    # Match GEE.math.fun argument to analogous R function
    GEE.math.fun <- R.FUNC[[match(GEE.math.fun, namelist)]]

    # Import python module
    ee <- reticulate::import("ee")

    # Check Google Earth Engine and initialise
    rgee::ee_check("rgee")
    rgee::ee_Initialize()


    combined_data_set = NULL # Empty vector for binding extracted data to

    rowscomplete = NULL # Keep note of rows completed

    # Assign each record a unique number for naming individual .csv files
    occ.data$unique.ID.DYN <- rep(1:nrow(occ.data))

    for (x in 1:nrow(occ.data)) {

      # If the extraction lost connection, this resumes the loop to where it had previously reached
      if (save.method == "split") {
        if (resume) {
          # Checks if output file for this loop has already been written
          if (file.exists(paste0(save.directory,
                                 "/",
                                 occ.data[x, "unique.ID.DYN"],
                                 "_",
                                 varname,
                                 ".csv"))) {
            next()
          }}
      }

      # Every 20 extractions, refresh connection with GEE prevents sticking
      if (x%%20 == 0) {
        invisible(rgee::ee_Initialize(quiet = TRUE))
      }

      date1 <- as.Date(with(occ.data[x, ], paste(year, month, day, sep = "-")),
                       "%Y-%m-%d")

      # Calculate the date the set number of days prior to record date
      if (temporal.direction == "prior") {
        date2 <- as.character(date1 - temporal.res)
        date1 <- as.character(date1)

        # ImageCollection object for dataset/band between prior & record date
        image_collection <- ee$ImageCollection(paste0(datasetname)) %>%
          ee$ImageCollection$filterDate(date2, date1) %>%
          ee$ImageCollection$map(function(x) {
            date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
            name <- ee$String$cat(paste0(bandname), date)
            x$select(paste0(bandname))$rename(name)
          })
      }

      # Calculate the date the set number of days post the record date
      if (temporal.direction == "post") {
        date2 <- as.character(date1 + temporal.res)
        date1 <- as.character(date1)

        # ImageCollection object for dataset/band between record & post date
        image_collection <- ee$ImageCollection(paste0(datasetname)) %>%
          ee$ImageCollection$filterDate(date1, date2) %>%
          ee$ImageCollection$map(function(x) {
            date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
            name <- ee$String$cat(paste0(bandname), date)
            x$select(paste0(bandname))$rename(name)
          })
      }

      # Set co-ordinate of the occurrence record as a GEE Geometry object
      coords <-rgee::ee$Geometry$Point(c(as.numeric(occ.data[x, "x"]),
                                         as.numeric(occ.data[x, "y"])))

      extracted_data <- NULL

      # Function extracts every value from ImageCollection at co-ordinates
      # trycatch to prevent GEE failure when NA returned
      tryCatch({
        extracted_data <- rgee::ee_extract(x = image_collection,
                                           y = coords,
                                           scale = spatial.res.metres)
      },
      error = function(e) {
        message("NA returned - rgee 'error': ", conditionMessage(e), "\n")
      })

      # NAs removed before the GEE.math.fun is calculated.
      if (!is.null(extracted_data)) {
        extracted_data <- na.omit(t(extracted_data))

        # Calculate GEE.math.fun on extracted data.
        if (nrow(extracted_data) > 0) {

          value <- as.data.frame(apply(extracted_data, 2, GEE.math.fun))
          colnames(value) <- varname
          extracted_data <- as.data.frame(cbind(occ.data[x,], value))}
        }


      # If no data were extracted return NA for this record.
      if (is.null(extracted_data) == TRUE) {
        value <- as.data.frame(NA)
        colnames(value) <- varname
        extracted_data <- as.data.frame(cbind(occ.data[x, ], value))
      }


      if (save.method == "split") {
        write.csv(extracted_data,row.names = FALSE,file = paste0(save.directory,
                                                                 "/",
                                                                 occ.data[x, "unique.ID.DYN"],
                                                                 "_",
                                                                 varname,
                                                                 ".csv"))


      }

      if (save.method == "combined") {
        combined_data_set <- as.data.frame(rbind(combined_data_set,
                                                 extracted_data))
      }

      message(paste0("Completed record: ",x))
      rowscomplete <- c(rowscomplete, x) # Record row completion
    }


    if (save.method == "split") {

      return(rowscomplete)
    }

    # Save combined data frame to save.directory
    if (save.method == "combined") {
      write.csv(combined_data_set,
                row.names = FALSE,
                file = paste0(save.directory,
                              "/all_records_combined_",
                              varname,
                              ".csv")
      )



      return(combined_data_set)
    }
  }






