#' Extract temporally dynamic explanatory variable data for occurrence records.
#'
#' For each species occurrence record co-ordinate and date, temporally dynamic explanatory data are extracted using Google Earth engine
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".
#' @param datasetname a character string, the Google Earth Engine dataset to extract data from.
#' @param bandname a character string, the Google Earth Engine dataset bandname to extract data for.
#' @param spatial.res.metres a numeric value, the spatial resolution in metres for data extraction.
#' @param GEE.math.fun a character string, the mathematical function to compute across the temporal.res period for each record.
#' @param save.method a character string, the method used to save extracted variable data. One of '"split"' or '"combined"': can be abbreviated. See details.
#' @param temporal.res a numeric value, the temporal resolution in days to extract data and calculate GEE.math.fun across from record date.
#' @param temporal.direction a character string, the temporal direction for extracting data across relative to the record date. One of '"prior"' or '"post"': can be abbreviated.
#' @param varname optional; a character string, the unique name for the explanatory variable. Default varname is “bandname_temporal.res_temporal.direction_ GEE.math.fun".
#' @param save.directory a character string, the path to a local directory to save extracted variable data to.
#' @details
#''For each individual species occurrence record co-ordinate and date, this function extracts data for a given band within a Google Earth Engine dataset across a user-specified period and calculates a mathematical function on such data.
#’
#’ extract_dynamic_coords requires users to have installed R package "rgee" (Aybar et al., 2020) and initialised Google Earth Engine with valid log-in credentials. Please follow instructions on the following website https://cran.r-project.org/web/packages/rgee/vignettes/rgee01.html. datasetname must be in the accepted Google Earth Engine Data catalogue layout (e.g. “MODIS/006/MCD12Q1” or “UCSB-CHG/CHIRPS/DAILY”) and bandname as specified in the dataset (e.g. “LC_Type5”, “precipitation”). For datasets and band names, see https://developers.google.com/earth-engine/datasets.
#'
#' GEE.math.fun specifies the mathematical function to be calculated over the temporal period. Options are limited to Google Earth Engine ImageCollection Reducer functions (https://developers.google.com/earth-engine/apidocs/) for which an analogous R function is available. This includes: "allNonZero","anyNonZero", "count", "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product", "sampleStdDev", "sampleVariance", "stdDev", "sum" and "variance".
#'
#' For save.method '"combined"', the function with save “.csv” files containing all occurrence records and associated values for the explanatory variable. If save.method '"split"' is chosen, the function will save individual “.csv” files for each record with assigned unique ID value in file name. '"split"' method is provided to protect users if internet connection is lost when extracting data for large occurrence datasets.
#' @references
#'Aybar, C., Wu, Q., Bautista, L., Yali, R. and Barja, A., 2020. rgee: An R package for interacting with Google Earth Engine. Journal of Open Source Software, 5(51), p.2272.
#'@return Returns details of successful explanatory variable extractions.
#'@export


extract_dynamic_coords <-
  function(occ.data,
           datasetname,
           bandname,
           spatial.res.metres,
           GEE.math.fun,
           save.method,
           temporal.res,
           temporal.direction,
           varname,
           save.directory) {

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

    # Names to match  GEE Reducer functions to
    namelist <-
      c(
        "allNonZero",
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

    R.FUNC.LIST <-
      list(
        allNonZero,
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
    GEE.math.fun <- R.FUNC.LIST[[match(GEE.math.fun, namelist)]]

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

      # Every 20 extractions, refresh connection with GEE prevents sticking
      if (x%%20 == 0) {invisible(rgee::ee_Initialize(quiet = T))}

      date1 <- as.Date(with(occ.data[x, ], paste(year, month, day, sep = "-")),
                       "%Y-%m-%d")

      # Calculate the date the set number of days prior to record date
      if (temporal.direction == "prior") {
        date2 <- as.character(date1 - temporal.res)
        date1 <- as.character(date1)

        # ImageCollection object for dataset/band between prior & record date
        image_collection <-
          ee$ImageCollection(paste0(datasetname)) %>%
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
        image_collection <-
          ee$ImageCollection(paste0(datasetname)) %>%
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
        cat("NA returned - rgee 'error': ", conditionMessage(e), "\n")
      })

      # NAs removed before the GEE.math.fun is calculated.
      if (!is.null(extracted_data)) {
        extracted_data <- na.omit(t(extracted_data))

        # Calculate GEE.math.fun on extracted data.
        if (nrow(extracted_data) > 0) {
          value <- as.data.frame(apply(extracted_data, 2, GEE.math.fun))
          colnames(value) <- varname
          extracted_data <-
            as.data.frame(cbind(occ.data[x,], value))
        }
      }

      # If no data were extracted return NA for this record.
      if (is.null(extracted_data) == T) {
        value <- as.data.frame(NA)
        colnames(value) <- varname
        extracted_data <- as.data.frame(cbind(occ.data[x, ], value))
      }


      if (save.method == "split") {
        write.csv(extracted_data,file = paste0(save.directory,
                                               "/",
                                               occ.data[x, "unique.ID.DYN"],
                                               "_",
                                               varname,
                                               "_.csv"))

        print(paste0("Record number: ",
                     x,
                     " saved to ",
                     save.directory,
                     "/",
                     occ.data[x, "unique.ID.DYN"],
                     "_",
                     varname,
                     "_.csv"
          )
        )
      }

      if (save.method == "combined") {
        combined_data_set <- as.data.frame(rbind(combined_data_set,
                                                 extracted_data))
      }


      rowscomplete <- c(rowscomplete, x) # Record row completion
    }


    if (save.method == "split") {
      print("Data successfully extracted for:")
      return(rowscomplete)
    }

    # Save combined data frame to save.directory
    if (save.method == "combined") {
      write.csv(
        combined_data_set,
        file = paste0(save.directory,
                      "/all_records_combined_",
                      varname,
                      "_.csv")
      )
      print(
        paste0(
          "Data successfully extracted for:",
          save.directory,
          "/all_records_combined",
          varname,
          "_.csv"
        )
      )

      return(combined_data_set)
    }
  }






