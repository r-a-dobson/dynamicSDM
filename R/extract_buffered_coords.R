#'Extract spatially buffered and temporally dynamic explanatory variable data for occurrence records.
#'
#' For each species occurrence record co-ordinate and date, spatially buffered and temporally dynamic explanatory data are extracted using Google Earth Engine.
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".
#' @param datasetname a character string, the Google Earth Engine dataset to extract data from.
#' @param bandname a character string, the Google Earth Engine dataset bandname to extract data for.
#' @param spatial.res.metres a numeric value, the spatial resolution in metres for data extraction.
#' @param GEE.math.fun a character string, the mathematical function to compute across the specified spatial matrix and period for each record.
#' @param moving.window.matrix a matrix of weights with an odd number of sides, representing the spatial neighbourhood of cells (“moving window”) to calculate GEE.math.fun across from record co-ordinate. See details for more information.
#' @param extraction.drive.folder a character string, Google Drive folder to temporarily save rasters to using Google Earth Engine before spatial buffering in R.
#' @param user.email a character string, user email for initialising Google Drive.
#' @param save.method a character string, the method used to save extracted variable data. One of '"split"' or '"combined"': can be abbreviated. See details.
#' @param temporal.level a character string, the temporal resolution of the explanatory variable data. One of '"day"', '"month"' or '"year"': can be abbreviated. Default; day.
#' @param save.directory a character string, path to a local directory to save extracted variable data to.
#' @param varname optional; a character string, a unique name for the explanatory variable. Default varname is “bandname_temporal.res_temporal.direction_ GEE.math.fun_buffered".
#' @param temporal.res optional; a numeric value, the temporal resolution in days to extract data and calculate GEE.math.fun across from occurrence record date.
#' @param temporal.direction optional; a character string, the temporal direction for extracting data across relative to the record date. One of '"prior"' or '"post"': can be abbreviated.
#' @param categories optional; a character string, the categories to use in calculation if data are categorical. See details for more information.
#' @param agg.factor optional; a numeric value, the factor to aggregate data by before spatial buffering.
#' @details
#'For each individual species occurrence record co-ordinate and date, this function extracts data for a given band within a Google Earth Engine dataset across a user-specified spatial buffer and temporal period and calculates a mathematical function on such data.
#'
#' If temporal.res and temporal.direction are not given, the function extracts explanatory variable data (in RasterLayer format) for all of the cells surrounding and including the cell containing the occurrence record co-ordinates. If temporal.res and temporal.direction is given, the function extracts explanatory variable data (in RasterLayer format) for which GEE.math.fun has been first calculated over this period in relation to the occurrence record date.
#'
#' Using the focal function in raster R package (Hijmans et al., 2015), GEE.math.fun is calculated across the spatial buffer area from the record co-ordinate. The spatial buffer area used is specified by argument moving.window matrix, which dictates the neighbourhood of cells surrounding the cell containing the occurrence record to include in the calculation.
#'
#' When explanatory variable data are categorical (e.g. land cover classes), argument categories can be used to specify the categories of importance to the calculation. The category or categories given will be converted in a binary representation, with “1” for those listed, and “0” for all others in the dataset. Ensure that the GEE.math.fun given is appropriate for such data. For example, this function could return the sum of suitable land cover classified cells in the “moving window” from species occurrence record co-ordinates.
#'
#' extract_buffered_coords requires users to have installed R package "rgee" (Aybar et al., 2020) and initialised Google Earth Engine with valid log-in credentials. Please follow instructions on the following website https://cran.r-project.org/web/packages/rgee/vignettes/rgee01.html. datasetname must be in the accepted Google Earth Engine Data catalogue layout (e.g. “MODIS/006/MCD12Q1” or “UCSB-CHG/CHIRPS/DAILY”) and bandname as specified in the dataset (e.g. “LC_Type5”, “precipitation”). For datasets and band names, see https://developers.google.com/earth-engine/datasets.
#'
#' extract_buffered_coords also requires users to have installed the R package "googledrive" (D'Agostino McGowan and Bryan, 2022) and initialised Google Drive with valid log-in credentials, which must be provided under argument user.email. Please follow instructions on https://googledrive.tidyverse.org/ for initialising the googledrive package.
#'
#' GEE.math.fun specifies the mathematical function to be calculated over the spatial buffered area and temporal period. Options are limited to Google Earth Engine ImageCollection Reducer functions (https://developers.google.com/earth-engine/apidocs/) for which an analogous R function is available. This includes: "allNonZero","anyNonZero", "count", "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product", "sampleStdDev", "sampleVariance", "stdDev", "sum" and "variance".
#'
#' temporal.level states the temporal resolution of the explanatory variable data and improves the speed of extract_buffered_coords extraction. For example, if the explanatory data represents an annual variable, then all record co-ordinates from the same year can be extracted from the same raster. However, if the explanatory data represents a daily variable, then only records from the exact same day can be extracted from the same raster. For the former, temporal.level argument should be ‘year’ and for the latter, temporal.level should be ‘day’.
#'
#' For save.method '"combined"', the function with save “.csv” files containing all occurrence records and associated values for the explanatory variable. If save.method '"split"' is chosen, the function will save individual “.csv” files for all of the records with each unique period of the given temporal.level (e.g. each year, each year and month combination or each unique date). '"split"' method is provided to protect users if internet connection is lost when extracting data for large occurrence datasets.
#' @references
#'Aybar, C., Wu, Q., Bautista, L., Yali, R. and Barja, A., 2020. rgee: An R package for interacting with Google Earth Engine. Journal of Open Source Software, 5(51), p.2272.
#'
#'D'Agostino McGowan L., and Bryan J., 2022. googledrive: An Interface to Google Drive. https://googledrive.tidyverse.org, https://github.com/tidyverse/googledrive.
#'
#'Hijmans, R. J., Van Etten, J., Cheng, J., Mattiuzzi, M., Sumner, M., Greenberg, J. A., Lamigueiro, O. P., Bevan, A., Racine, E. B. & Shortridge, A. 2015. Package ‘raster’. R package, 734.
#' @return Returns details of successful explanatory variable extractions.
#'@export

extract_buffered_coords <-
  function(occ.data,
           datasetname,
           bandname,
           spatial.res.metres,
           GEE.math.fun,
           moving.window.matrix,
           extraction.drive.folder,
           user.email,
           save.method,
           varname = NULL,
           temporal.res = NULL,
           temporal.level = NULL,
           temporal.direction = NULL,
           categories = NULL,
           save.directory = NULL,
           agg.factor=NULL
           ) {


    if (missing(extraction.drive.folder)) {
      stop("Missing extraction.drive.folder")
    }

    # Check user email provided
    if (missing(user.email)) {stop("Provide email linked to Google Drive")}

    # Check formatting of arguments to avoid common errors
    if (!length(GEE.math.fun) == 1) {stop("Only provide one GEE.math.fun")}

    if (!dir.exists(save.directory)) {stop("save.directory not found")}

    # Check arguments match available options
    save.method <- match.arg(arg = save.method,
                             choices = c("split", "combined"))

    temporal.level <- match.arg(arg = temporal.level,
                                choices = c("day", "month", "year"))

    if (!missing(temporal.res)) {
      if (missing(temporal.direction)) {stop("temporal.direction missing.")}

      temporal.direction <- match.arg(arg = temporal.direction,
                                      choices = c("prior", "post"))
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
    rgee::ee_Initialize(drive = T)

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

      # Work out minimum area to download for extraction of moving window matrix
      # around each occurrence record (to minimise computing time)

      # Using data resolution and matrix cell length, calc radius from occ
      # occurrence co-ordinate to edge of matrix. (circumference/2)
      spatial.buffer <- nrow(moving.window.matrix) * spatial.res.metres / 2

      # The matrix is square not circular, so then need to work out the radius
      # to furthest corner of matrix away from co-ordinate.
      # Rearrange pythagorus theorem (a2 + b2 = c2),
      # so c = square root of a2 + b2 (a and b= radius from previous calc)
      spatial.buffer <- sqrt((spatial.buffer ^ 2) + (spatial.buffer ^ 2))

      ## To ensure that all cells are definitely included round to nearest 5
      spatial.buffer <- ceiling(spatial.buffer / 5) * 5

      # From each occurrence record point in this step, add the radius using the rangemap package.
      #Requires >1 co-ord. If only 1, just bind same co-ord twice and take first.
      if (nrow(occforperiod) > 1) {
        spatial.buffer <-
          rangemap::geobuffer_points(occforperiod[, c("x", "y")],
                                     radius = spatial.buffer,
                                     by_point = T)
      }

      if (nrow(occforperiod) == 1) {
        spatial.buffer <-
          rangemap::geobuffer_points(rbind(occforperiod[, c("x", "y")],
                                           occforperiod[, c("x", "y")]),
                                     radius = spatial.buffer,
                                     by_point = T)[1]
      }

      # Extract min and max longtiude and latitude co-ordinates
      # This is the minimum possible area to extract from environmental dataset
      xmin <- sp::bbox(raster::extent(spatial.buffer))[1, 1]
      xmax <- sp::bbox(raster::extent(spatial.buffer))[1, 2]
      ymin <- sp::bbox(raster::extent(spatial.buffer))[2, 1]
      ymax <- sp::bbox(raster::extent(spatial.buffer))[2, 2]

      # Create GEE Geometry Polygon using min and max co-ordinates
      geometry <- ee$Geometry$Polygon(list(c(xmin ,  ymin),
                                           c(xmin , ymax),
                                           c(xmax, ymax),
                                           c(xmax,  ymin)))
      # Static variable
      if (missing(temporal.res)) {
        date1 <- as.character(date1)

        # Create GEE ImageCollection for specified dataset, band and date
        image_collection <-
          ee$ImageCollection(paste0(datasetname))$
          filterDate(date1)$select(paste0(bandname))

        # As static, only one Image in ImageCollection, so select the first Image
        image_collection_reduced <- image_collection$reduce(ee$Reducer$first())
      }


      # Temporally dynamic variable
      if (!missing(temporal.res)) {
        firstdate <- date1

        # Prior selected so minus temporal.res days from the record date
        if (temporal.direction == "prior") {
          seconddate <- as.character(date1 - temporal.res)
          firstdate <- as.character(firstdate)

          # ImageCollection of specified dataset and band between two dates
          image_collection <-
            ee$ImageCollection(paste0(datasetname))$
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
        GEE.FUNC <-
          list(
            ee$Reducer$allNonZero(),
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

        # Match named function to actual function for use.
        GEE.math.fun <- match.arg(arg = GEE.math.fun, choices = namelist)

        # Reduce ImageCollection using function specified in GEE.math.fun
        image_collection_reduced <-
          image_collection$reduce(GEE.FUNC[[match(GEE.math.fun, namelist)]])
      }

      # Download raster of Reduced ImageCollection to Google Drive
      tryCatch({
        raster <- rgee::ee_as_raster(
          image = image_collection_reduced,
          container = extraction.drive.folder,
          scale = spatial.res.metres,
          dsn = paste0(varname, "_", date1),
          region = geometry,
          timePrefix = FALSE,
          via = "drive"
        )
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })

      # Get temp file name for downloading raster
      pathforthisfile <- paste0(tempfile(), ".tif")

      # Authenticate Google Drive
      googledrive::drive_auth(email = user.email)
      googledrive::drive_user()

      # Download raster from Google Drive to temp directory for processing
      googledrive::drive_download(paste0(varname, "_", date1, ".tif"),
                                  path = pathforthisfile,
                                  overwrite = T)
      raster <- raster::raster(pathforthisfile) # Read in raster from temp dir



      # Match GEE.math.fun argument to analogous R function
      R.FUNC <-
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
            rast <- rast + raster == categories[cat]
          }
        }

        if(!missing(agg.factor)) {
          rast <- raster::aggregate(rast, agg.factor, fun = math.fun, na.rm=TRUE)
        }

        # If data are categorical then matrix weights must = 1
        moving.window.matrix[1:nrow(moving.window.matrix),
                             1:ncol(moving.window.matrix)] <- 1

        ## Calculate math.fun across moving.window.matrix for the raster
        focalraster <- raster::focal(rast,
                                     moving.window.matrix,
                                     fun = math.fun,
                                     na.rm = T)
      }


      # Process continuous data raster
      # Calculate math.fun across moving.window.matrix for the raster
      if (missing(categories)) {

        if(!missing(agg.factor)) {
          raster <- raster::aggregate(raster, agg.factor, fun = math.fun, na.rm=TRUE)
        }

        focalraster <- raster::focal(raster,
                                     moving.window.matrix,
                                     fun = math.fun, na.rm=TRUE)
      }

      # Extract value at co-ordinates of each occurrence record
      extracted_data <- as.data.frame(raster::extract(focalraster,
                                                      sp::SpatialPoints(cbind(
                                                        occforperiod[, "x"],
                                                        occforperiod[, "y"]
                                                      ))))

      colnames(extracted_data) <- varname

      extracted_data <- as.data.frame(cbind(occforperiod, varname))

      # If split chosen, save individual .csv file with record and value
      if (save.method == "split") {
        write.csv(
          extracted_data,
          file = paste0(save.directory,
                        "/",
                        nameofsplitfile,
                        "_",
                        varname,
                        "_.csv"),
          row.names = FALSE
        )

        print(
          paste0(
            "Records for temporal.level: ",
            nameofsplitfile,
            " saved to ",
            save.directory,
            "/",
            nameofsplitfile,
            "_",
            varname,
            "_.csv"
          )
        )
      }

      if (save.method == "combined") {
        combined_data_set <- rbind(combined_data_set, extracted_data)
      }

      # Record uniqueID of records explanatory data extracted for in this loop
      rowscomplete <- c(rowscomplete, nameofsplitfile)

      # Remove the raster used in this loop from Google Drive
      googledrive::drive_rm(paste0(varname, "_", date1, ".tif"))
    }

    # Print names of occurrence records data successfully extracted for
    if (save.method == "split") {
      print("Data successfully extracted for:")
      return(sort(rowscomplete))
    }


    # Save combined data.frame to given directory
    if (save.method == "combined") {
      write.csv(
        combined_data_set,
        row.names = FALSE,
        file = paste0(save.directory,
                      "/all_records_combined_",
                      varname,
                      "_.csv")
      )
      print(
        paste0(
          "Data successfully extracted to: ",
          save.directory,
          "/all_records_combined_",
          varname,
          "_.csv"
        )
      )
      return(combined_data_set)
    }
  }

