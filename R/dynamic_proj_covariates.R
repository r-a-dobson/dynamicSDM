#' Combine explanatory variable rasters into a covariate data frame for each projection date.
#'
#' Explanatory variable rasters are imported for each projection date, resampled to given spatial resolution and extent and stacked, and then written to a covariate data frame for each projection date.
#'
#' @param dates a character string, vector of dates in format YYYY-MM-DD.
#' @param spatial.ext optional; the spatial extent to crop explanatory variable rasters to. Object of class "Extent", "RasterLayer" or "polygon" or numeric vector listing xmin, xmax, ymin and ymax in order.
#' @param varnames a character string, the unique names for each explanatory variable.
#' @param spatial.res.degrees optional; a numeric value, the spatial resolution in degrees for projection rasters to be resampled to. Required if spatial.ext given.
#' @param resample.method a character string or vector length of varnames, specifying resampling method to use. One of "ngb" and "bilinear". See details for more information.
#' @param drive.folder optional; a character string or vector, Google Drive folder or folders to read projection covariate rasters from.
#' @param user.email optional; a character string, user email for initialising Google Drive. Required if drive.folder or save.drive.folder used.
#' @param local.directory optional; a character string or vector, path to local directory or directories to read projection covariate rasters from.
#' @param save.directory optional; a character string, path to local directory to save projection covariate data frames to.
#' @param save.drive.folder optional; a character string, Google Drive folder to save projection covariate data frames to.
#' @details
#' For each projection date, appropriate rasters for each explanatory variable are imported from a local directory or Google Drive folder. If required, rasters are cropped and resampled to the same spatial extent and resolution. Rasters are then stacked and transformed into a covariate data frame with column names matching the unique explanatory variable names. A data frame of these projection covariates are exported to a local directory or Google Drive folder as “.csv” files named as the relevant date in “YYYY-MM-DD” format.
#' Note: if explanatory variable rasters are not of the same spatial resolution and extent, then the function will error.
#'Resample methods (resample.method) include: "ngb", in which each cell acquires the value of its nearest neighbour cell in the original raster and is often used for categorical variables; and "bilinear", in which the distance-weighted average of the four nearest cells are used to estimate a new cell value and is often used for continuous variables. If only one resample.method is given, but these are more than one explanatory variables, the same resample.method is used for all.
#'
#’ dynamic_proj_covariates requires explanatory variable rasters in local.directory or drive.folder to be uniquely named “.tif” files containing the variable name and relevant projection date in format YYYY-MM-DD. If more than one “.tif” file in the Google Drive folder or local directory matches the projection date and explanatory variable name, then the function will error.
#'
#'If drive.folder or save.drive.folder arguments are used to download rasters for use or upload function output, then users must have installed R package "googledrive" and initialised Google Drive with valid log-in credentials. The credentials must be given under function argument user.email to initiate the correct Google Drive account. Please follow instructions on https://googledrive.tidyverse.org/.
#'@export


dynamic_proj_covariates <-
  function(dates,
           varnames,
           drive.folder = NULL,
           user.email = NULL,
           local.directory = NULL,
           spatial.ext = NULL,
           spatial.res.degrees = NULL,
           resample.method = NULL,
           save.directory = NULL,
           save.drive.folder = NULL) {

    # Check arguments to prevent error downstream

    if (missing(spatial.ext)) {
      message("spatial.ext missing. Rasters must have same extent.")
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
      if (!any(class(spatial.ext) == c("numeric",
                                       "Extent",
                                       "RasterLayer",
                                       "Polygon"))) {
        stop("spatial.ext must be numeric, Extent, RasterLayer or Polygon")
      }

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

      # Check folder exists in user's Google Drive
      folderpath <- googledrive::drive_find(pattern = drive.folder,
                                            type = 'folder')
      if (nrow(folderpath) == 0) {stop("drive.folder does not exist.")
      }

      # List all files in Google Drive folders
      drivefiles = NULL
      for (folder in 1:length(drive.folder)) {
        drivefiles <-
          c(drivefiles,
            googledrive::drive_ls(path = paste0(drive.folder[folder]))$name)
      }
    }

    # If local.directory list files in the folder(s)

    if (!missing(local.directory)) {
      directoryfiles <- list.files(local.directory, full.names = T)
    }

    # Iterate through each projection date and variable.

    listofdone <- NULL # Empty vector to record dates completed

    for (x in 1:length(dates)) {
      date <- dates[x]

      stack <- raster::stack() # Empty stack to bind rasters for this date to

      for (v in 1:length(varnames)) {
        name <- varnames[v]

        # Read in raster for this variable and date from Google Drive

        if (!missing(drive.folder)) {
          filedownload <- drivefiles[grep(name, drivefiles)]
          filedownload <- filedownload[grep(date, filedownload)] # Select files
          pathforthisfile <- paste0(tempfile(), ".tif") # Create temp file name
          googledrive::drive_download(file = filedownload,
                                      path = pathforthisfile,
                                      overwrite = T) # Download from Drive
          raster <- raster::raster(pathforthisfile)# Read raster from temp dir
        }

        # Alternatively, read in raster for this variable and date from local directory
        if (!missing(local.directory)) {
          fileimport <- directoryfiles[grep(name, directoryfiles)]
          fileimport <- fileimport[grep(date, fileimport)] # Select files
          raster <- raster::raster(fileimport) # Read raster from local dir
        }

        # Crop to spatial.ext provided so that all rasters are same extent for stacking
        if (!missing(spatial.ext)) {

          # Create raster of desired extent and resolution
          r <- raster::raster(raster::extent(xmin, xmax, ymin, ymax))
          raster::res(r) <- spatial.res.degrees
          r <- raster::setValues(r, 1:raster::ncell(r))

          # Resample raster using single method given
          if (length(resample.method) == 1) {
            raster <- raster::resample(raster, r, method = resample.method)
          }

          # Resample raster using variable specific method given
          if (!length(resample.method) == 1) {
            raster <- raster::resample(raster, r, method = resample.method[v])
          }
        }

        stack <- raster::stack(stack , raster) # Add raster to stack
      }

      names(stack) <- varnames # Label each layer in stack as variable

      stack <- as.data.frame(raster::rasterToPoints(stack)) # Create data frame

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
        save.folderpath <- googledrive::drive_find(pattern = save.drive.folder,
                                  type = 'folder')

        if (nrow(save.folderpath) == 0) {stop("save.drive.folder doesn't exist")}

        csvfile <- paste0(tempfile(), ".csv")
        write.csv(stack, file = csvfile) # Save to temporary location
        googledrive::drive_upload( # Upload to Google Drive
          media = csvfile,
          path = googledrive::as_id(save.folderpath$id),
          name = paste0(date, "_projection_dataframe.csv"),
          overwrite = T
        )
      }

      # Alternatively save covariate data.frame to local directory

      if (!missing(save.directory)) {
        if (!dir.exists(save.directory)) {
          stop("save.directory does not exist")
        }
        write.csv(stack,
                  file = paste0(save.directory,
                                "/",
                                date,
                                "_projection_dataframe.csv"))
        }

      listofdone <- rbind(listofdone, date) # Record that this date has been completed
    }

    if (!missing(save.directory)) {
      print(paste0("Data extracted and saved to :",save.directory))
    }

    if (!missing(save.drive.folder)) {
      print(paste0("Data extracted and saved to:",save.drive.folder))
    }

    return(listofdone)
  }
