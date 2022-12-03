#' Extract temporally dynamic rasters of explanatory variable data.
#'
#' Extract rasters for temporally dynamic explanatory variables at each projection date using Google Earth Engine.
#' @param dates a character string, vector of dates in format YYYY-MM-DD.
#' @param spatial.ext the spatial extent for the extracted raster. Object of class "Extent", "RasterLayer" or "polygon" or numeric vector listing xmin, xmax, ymin and ymax in order.
#' @param datasetname a character string, the Google Earth Engine dataset to extract data from.
#' @param bandname a character string, the Google Earth Engine dataset bandname to extract data for.
#' @param spatial.res.metres a numeric value, specifying the spatial resolution in metres of the raster to be extracted.
#' @param GEE.math.fun a character string, the mathematical function to compute across the specified time frame from each projection date and for each cell.
#' @param user.email a character string, user email for initialising Google Drive.
#' @param varname optional; a character string, the unique name for the explanatory variable. Default varname is “bandname_temporal.res_temporal.direction_ GEE.math.fun_raster".
#' @param temporal.res a numeric value, the temporal resolution in days to extract data across.
#' @param temporal.direction a character string, the temporal direction for extracting dynamic variable data across relative to each projection date given. One of '"prior"' or '"post"': can be abbreviated.
#' @param save.drive.folder a character string, Google Drive folder to save extracted rasters to.
#' @param save.directory optional; a character string, path to local directory to save extracted rasters to.
#' @details
#' For each projection date, this function downloads rasters at given spatial extent and resolution for temporally dynamic explanatory variables. For each cell in spatial extent, the GEE.math.fun is calculated for data extracted across specified number of days prior or post the projection date. Rasters of such data are saved directly to Google Drive, with option to export to local directory too. These can be combined to create projection covariate data frames for projection dynamic species distribution and abundance at high spatiotemporal resolution
#'
#' extract_dynamic_raster requires users to have installed R package "rgee" (Aybar et al., 2020) and initialised Google Earth Engine with valid log-in credentials. Please follow instructions on the following website . datasetname must be in the accepted Google Earth Engine Data catalogue layout (e.g. “MODIS/006/MCD12Q1” or “UCSB-CHG/CHIRPS/DAILY”) and bandname as specified in the dataset (e.g. “LC_Type5”, “precipitation”). For datasets and band names, see .
#'
#' extract_dynamic_raster also requires users to have installed the R package "googledrive" (D'Agostino McGowan and Bryan, 2022) and initialised Google Drive with valid log-in credentials, which must be provided under argument user.email. Please follow instructions on https://googledrive.tidyverse.org/ for initialising the googledrive package.
#'
#' GEE.math.fun specifies the mathematical function to be calculated over the temporal period from each projection date. Options are limited to Google Earth Engine ImageCollection Reducer functions (https://developers.google.com/earth-engine/apidocs/) for which an analogous R function is available. This includes: "allNonZero","anyNonZero", "count", "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product", "sampleStdDev", "sampleVariance", "stdDev", "sum" and "variance".
#' @references
#'Aybar, C., Wu, Q., Bautista, L., Yali, R. and Barja, A., 2020. rgee: An R package for interacting with Google Earth Engine. Journal of Open Source Software, 5(51), p.2272.
#'
#'D'Agostino McGowan L., and Bryan J., 2022. googledrive: An Interface to Google Drive. https://googledrive.tidyverse.org, https://github.com/tidyverse/googledrive.
#' @return Returns details of successful explanatory variable extractions for each projection date.
#'@export


extract_dynamic_raster <-
  function(dates,
           spatial.ext,
           datasetname,
           bandname,
           spatial.res.metres,
           GEE.math.fun,
           user.email,
           varname = NULL,
           temporal.res,
           temporal.direction,
           save.directory = NULL,
           save.drive.folder) {


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
    rgee::ee_Initialize(drive = T)

    # Check arguments match available options/inputs
    temporal.direction <- match.arg(arg = temporal.direction,
                                    choices = c("prior", "post"))

    if (!any(class(spatial.ext) == c("numeric",
                                     "Extent",
                                     "RasterLayer",
                                     "Polygon"))) {
      stop("spatial.ext must be class numeric, Extent, RasterLayer or Polygon")
    }


    if (!missing(save.directory) && !dir.exists(save.directory)) {
      stop("save.directory not found.")
    }

    if (missing(spatial.res.metres)) {
      stop("Provide the spatial resolution in metres to extract raster at.")
    }

    if (missing(save.drive.folder)) {
      stop("Provide Google Drive folder name to save extracted rasters")
    }

    if (class(spatial.ext) == "numeric" && !length(spatial.ext) == 4) {
      stop("spatial.ext vector should be length 4: xmin, xmax, ymin and ymax")
    }

    xmin <- extract_xy_min_max(spatial.ext)[1]
    xmax <- extract_xy_min_max(spatial.ext)[2]
    ymin <- extract_xy_min_max(spatial.ext)[3]
    ymax <- extract_xy_min_max(spatial.ext)[4]

    # Create Google Earth Engine geometry object from co-ordinates
    geometry <- ee$Geometry$Polygon(list(c(xmin ,  ymin),
                                         c(xmin , ymax),
                                         c(xmax, ymax),
                                         c(xmax,  ymin)))

    # List all GEE ImageCollection Reducer functions available
    GEE.FUNC.LIST <-
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

    # Names to match the GEE Reducer functions
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
        image_collection <-
          ee$ImageCollection(paste0(datasetname))$
          filterDate(seconddate, firstdate)$
          select(paste0(bandname))
      }

      # Add temporal.res days to projection date
      if (temporal.direction == "post") {
        seconddate <- as.character(as.Date(dates[x]) + temporal.res)

        # Create ImageCollection object for the variable  between the two dates
        image_collection <-
          ee$ImageCollection(paste0(datasetname))$
          filterDate(firstdate, seconddate)$
          select(paste0(bandname))
      }

      # Reduce the ImageCollection using GEE Reducer function chosen by user
      image_collection_reduced <-
        image_collection$reduce(GEE.FUNC.LIST[[match(GEE.math.fun, namelist)]])

      tryCatch({
        rgee::ee_as_raster(
          image = image_collection_reduced,
          container = save.drive.folder,# Drive folder to save raster to
          scale = spatial.res.metres, # Specifies spatial resolution of raster
          dsn = paste0(varname, "_", firstdate),# Names raster file
          region = geometry,# Crops to spatial.extent
          timePrefix = FALSE,
          via = "drive"
        )
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })

      # Initiate Google Drive
      googledrive::drive_auth(email = user.email)
      googledrive::drive_user()

      # Save file to local directory if required
      if (!missing(save.directory)) {
        googledrive::drive_download(
          paste0(varname, "_", firstdate, ".tif"),
          path = paste0(save.directory, "/", varname, "_", firstdate, ".tif"),
          overwrite = T
        )
      }

      # Record that this date has been processed
      completed.list <-rbind(completed.list, paste0(varname, "_", firstdate))
    }

    if (missing(save.directory)) {
      print(paste0("Data extracted to Google Drive folder:", save.drive.folder))
    }

    if (!missing(save.directory)) {
      print(
        paste0(
          "Data extracted to Google Drive folder:",
          save.drive.folder,
          "and local directory:",
          save.directory
        )
      )
    }

    return(completed.list)
  }
