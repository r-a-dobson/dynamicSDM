#'Project species distribution and abundance models onto dynamic environmental covariates.
#'
#'Projects fitted species distribution and abundance models onto projection covariates for each date
#'given.
#'
#'@param dates a character string, vector of dates in format "YYYY-MM-DD".
#'@param cov.file.type a character string, the type of file that contains projection covariates. One
#'  of: "`tif`" (raster stack) or `csv`(data frame).
#'@param projection.method a character string or vector, the method or methods to project
#'  distribution and abundance onto projection covariates. Options include `proportional`, `binary`,
#'  `abundance` and `stacked`. See details for more information.
#'@param drive.folder optional; a character string, the Google Drive folder to read projection
#'  covariate data frames from. Folder must be uniquely named within Google Drive. Do not provide
#'  path.
#'@param local.directory optional; a character string, the path to a local directory to read
#'  projection covariate data frames from.
#'@param user.email optional; a character string, user email for initialising Google Drive. Required
#'  if `drive.folder` or `save.drive.folder` used.
#'@param sdm.mod optional; a model object or list of model objects fitted to species distribution
#'  data.
#'@param sam.mod optional; a model object or list of model objects fitted to species abundance data.
#'@param sdm.weight optional; a numeric string, weights given to each `sdm.mod` model projection,
#'  given in the same order as the `sdm.mod` list. Default is equal weighting to all models.
#'@param sdm.thresh optional; a numeric value, the threshold to convert projected distribution
#'  suitability into binary presence-absence. Default 0.5. Required if projection.method is
#'  "`binary`" or  "`stacked`".
#'@param sam.weight optional; a numeric string, weights given to each `sdm.mod` model projection,
#'  given in the same order as the `sam.mod` list. Default is equal weighting to all models.
#'@param save.directory optional; a character string, path to local directory to save projection
#'  rasters to.
#'@param save.drive.folder optional; a character string, Google Drive folder to save projection
#'  rasters to. Folder must be uniquely named within Google Drive. Do not provide path.
#'@param prj a character string, the coordinate reference system of input projection covariates.
#'  Default is "+proj=longlat +datum=WGS84".
#'@param proj.prj a character string, the coordinate reference system desired for output projection
#'  rasters. Default is assumed to be the same as prj.
#'@param spatial.mask an object of class `Raster`, `sf` or `Spatial`, representing a mask in which
#'  NA cells in the mask layer are removed from the projection covariates.
#'@details Function projects a model object or list of model objects onto projection covariate data
#'  frames for each projection date given.
#'
#'  # Projection covariate input
#'
#'  * Data frames: if `cov.file.type = csv`, then projection covariates must be saved "csv" files
#'  in the `drive.folder` or `local.directory` given. Here, they must be unique in containing the
#'  relevant projection date in “YYYY-MM-DD” format. For instance, two or more “csv” files saved
#'  within the Google Drive folder or local directory that contain the projection date will result
#'  in function error. Additionally, column names of projection covariate data frames must match the
#'  explanatory variable names that fitted models are trained on.
#'
#'  * Raster stacks: if `cov.file.type = tif`, then projection covariates must be saved "tif"
#'  files, similarly named and formatted as above. Raster layer names must match the explanatory
#'  variable names that fitted models are trained on.
#'
#'  Note: It is important to state the coordinate reference system projection of covariates using
#'  argument `prj`.
#'
#'  # Model input
#'
#'  When multiple models are provided in `sdm.mod` or `sam.mod`, the function projects each model
#'  onto the projection covariates and takes the average value across all model projections. If
#'  `sdm.weight` or `sam.weight` is specified, then the weighted average of model projections is
#'  returned. For example, this could be used to down weigh projections by poorly performing models
#'  in an ensemble.
#'
#'  # Projection output
#'
#'  * proportional: Projects `sdm.mod` model objects onto projection covariates for each date,
#'  exporting rasters for projected distribution suitability, a continuous measure between 0 (least
#'  suitable) and 1 (most suitable).
#'
#'  * binary: Projects `sdm.mod` onto projection covariates for each date, exporting rasters for
#'  projected binary presence (1) or absence (0), derived from distribution suitability using
#'  user-specified threshold (`sdm.thresh`) or default threshold of 0.5.
#'
#'  * abundance: Projects `sam.mod` onto projections covariates for each date, exporting rasters for
#'  projected abundance in the units that `sam.mod` were fitted onto.
#'
#'  * stacked: Follows the binary projection method and then projects abundance onto only binary
#'  presence (1) cells using the abundance projection method.
#'
#'  # Projection output
#'
#'  Projections are output as rasters. These can be reprojected to a different coordinate reference
#'  system using argument `proj.prj`.
#'
#'  One or both of `save.drive.folder` and `save.directory` are required to specify where projection
#'  rasters are to be saved.
#'
#'  # Google Drive compatibility
#'
#'  If `drive.folder` or `save.drive.folder` given, please ensure the folder name is unique within
#'  your Google Drive. Do not provide the path if the folder is nested within others.
#'
#'  If one of `drive.folder` or `save.drive.folder` are used then `user.email` is required to access
#'  the appropriate Google Drive user account. This requires users to have installed R package
#'  `googledrive` and initialised Google Drive with valid log-in credentials. Please follow
#'  instructions on <https://googledrive.tidyverse.org/>.
#'
#'@return Exports projection rasters for each projection date to user-specified Google Drive
#'  folder or local directory.
#'
#'@export
#'@examples
#'\donttest{
#'# Read in data
#'data("sample_explan_data")
#'
#'# Set variable names
#'variablenames<-c("eight_sum_prec","year_sum_prec","grass_crop_percentage")
#'
#' model <- brt_fit(sample_explan_data,
#'                  response.col = "presence.absence",
#'                  varnames = variablenames,
#'                  interaction.depth = 1,
#'                  distribution = "bernoulli",
#'                  n.trees = 1500)
#'
#'data(sample_cov_data)
#'utils::write.csv(sample_cov_data,file=paste0(tempdir(),"/2018-04-01_covariates.csv"))
#'
#'dynamic_proj(dates = "2018-04-01",
#'             projection.method = c("proportional"),
#'             local.directory = tempdir(),
#'             cov.file.type = "csv",
#'             sdm.mod = model,
#'             save.directory = tempdir())
#'}


dynamic_proj <-  function(dates,
                          projection.method,
                          local.directory,
                          drive.folder,
                          user.email,
                          sdm.mod,
                          sdm.thresh = 0.5,
                          sdm.weight = 1,
                          sam.mod,
                          sam.weight = 1,
                          save.directory,
                          save.drive.folder,
                          cov.file.type,
                          prj =  "+proj=longlat +datum=WGS84",
                          proj.prj,
                          spatial.mask) {

    # Set thresholds
    if (!missing(sdm.mod) && missing(sdm.thresh)) {
      message("No sdm.thresh. Default 0.5")
    }

   if (missing(proj.prj)) {
     proj.prj <- prj
   }

    if (!missing(sdm.mod) && !missing(sdm.thresh)) {
      if (!length(sdm.thresh) == 1) {
        stop("sdm.thresh should be of length(1) ")
      }
    }
      if (!missing(sdm.mod) && missing(sdm.weight)) {
        message("No sdm.weight specified. Default equal weighting.")
      }

      if (!length(sdm.weight) == length(sdm.mod) && !length(sdm.weight) == 1) {
        stop(
          "sdm.weight should be of length(1) or equal to number of sdm models."
        )
      }

    if (!missing(sam.mod)) {
      if (missing(sam.weight)) {
        message("No sam.weight specified. Default equal weighting")
      }
      if (!length(sam.weight) == length(sam.mod) && !length(sam.weight) == 1) {
        stop(
          "sam.weight should be of length(1) or equal to number of sam models."
        )
      }
    }

  # Convert mask to SPDF for use with raster::mask

  if (!missing(spatial.mask)) {
    spatial.mask<-convert_to_sf(spatial.mask,prj)
    spatial.mask<-sf::as_Spatial(spatial.mask)
  }



    #Check directory exists if provided
    if (!missing(local.directory)) {
      if (!dir.exists(local.directory)) {
        stop("local.directory does not exist")
      }
    }

    if (missing(save.directory) && missing(save.drive.folder)) {
      stop("No folder or directory to save projections too.")
    }

  if (!missing(save.drive.folder)) {
    # Check user email provided
    if (missing(user.email)) {
      stop("Please provide user email (user.email) linked to Google Drive")
    }
  }

    # Check Google Drive folder exists if provided
    if (!missing(drive.folder)) {
      # Check user email provided
      if (missing(user.email)) {
        stop("Please provide user email (user.email) linked to Google Drive")
      }
      googledrive::drive_auth(email = user.email)  #Initiate Google Drive
      googledrive::drive_user()


      # Check folder exists in user's Google Drive
      folderpath <- googledrive::drive_find(pattern = drive.folder, type = 'folder')


      if (nrow(folderpath) == 0) {stop("drive.folder does not exist.")}


      # If more than one partial match, use grep to extract exact match
      if(nrow(folderpath)>1) {
        folderpath <- folderpath[grep(paste0("^", drive.folder, "$"),
                                      folderpath$name), ]
      }

      # If exact match to more than one folder then not uniquely named. Cannot write file.
      if (nrow(folderpath) > 1) {
        stop("save.drive.folder is not uniquely named in your Google Drive ")
      }

      drivefiles <- googledrive::drive_ls(path = googledrive::as_id(folderpath$id))[,1:2]

    }

    # Match projection.method argument to available options
    projection.method <- match.arg(projection.method, choices = c("proportional",
                                                                  "binary",
                                                                  "abundance",
                                                                  "stacked"), several.ok = T)

    cov.file.type <- match.arg(cov.file.type, choices = c("tif", "csv"))



    for (x in 1:length(dates)) {

      date <- dates[x]

      # Read in projection data from local directory
      if (!missing(local.directory)) {

        filename <- list.files(local.directory, full.names = T) # List all files
        filename <- filename[grep(date, filename)] # Get file name matching date

        if(cov.file.type == "csv") {

          filename <- filename[grep("*.csv", filename)] # Get only .csv file name
          projection_df <- read.csv(filename)
        } # Read in file

        if (cov.file.type == "tif") {
          filename <- filename[grep("*rasterstack.tif", filename)] # Get only .csv file name
          projection_df <- raster::brick(filename)

          if (!missing(spatial.mask)) {
          projection_df <- raster::mask(projection_df, spatial.mask)
          }

        }

        }


      # Read in projection data from Google Drive folder
      if (!missing(drive.folder)) {


        fileid <- drivefiles[grep(date, drivefiles$name),]



        if (cov.file.type == "csv") {

        fileid <- fileid[grep("*.csv", fileid$name),"id"]# Get file name matching date

        pathforthisfile <- paste0(tempfile(), ".csv") # Temporary file name
        googledrive::drive_download(file = googledrive::as_id(fileid$id),
                                    path = pathforthisfile,
                                    overwrite = T) # Download file to temp dir
        projection_df <- read.csv(pathforthisfile) }# Read in file

        if (cov.file.type == "tif") {

          fileid <- fileid[grep("*rasterstack.tif", fileid$name),"id"]# Get file name matching date

          pathforthisfile <- paste0(tempfile(), ".tif") # Temporary file name
          googledrive::drive_download(file = googledrive::as_id(fileid$id),
                                      path = pathforthisfile,
                                      overwrite = T) # Download file to temp dir

          projection_df <- raster::brick(pathforthisfile)

          if (!missing(spatial.mask)) {
            projection_df <- raster::mask(projection_df, spatial.mask)
          }

        }}

      ### If one model object given for either

      if (!missing(sdm.mod)) {
        if (!inherits(sdm.mod, "list")) {

          if (cov.file.type == "tif") {

            SDMpred <- raster::predict(model = sdm.mod,
                                       object = projection_df)

            SDMbinary <- SDMpred > sdm.thresh} # Probability to binary

          if(cov.file.type=="csv") {

            SDMpred <- stats::predict(sdm.mod,
                                      newdata = projection_df,
                                      type = "response",
                                      na.action = stats::na.pass)

            SDMbinary <- as.numeric(SDMpred > sdm.thresh)} # Probability to binary

        }


        if (inherits(sdm.mod, "list")) {

          proj_blocks = NULL
          proj_stack <- raster::stack()

          # Make projection with each model
          for (model in 1:length(sdm.mod)) {

            if(cov.file.type == "tif") {

              proj_stack <- raster::stack(proj_stack,
                                                 raster::predict(model=sdm.mod[[model]],
                                                                 object = projection_df))}

            if(cov.file.type=="csv"){

              proj_blocks <- cbind(proj_blocks,
                                          stats::predict(sdm.mod[[model]],
                                                         newdata = projection_df,
                                                         type = "response",
                                                         na.action = stats::na.pass))}
          }

          if (length(sdm.weight) == 1) {
            if(cov.file.type=="csv") {
              SDMpred <- matrixStats::rowWeightedMeans(proj_blocks,
                                                       w = rep(sdm.weight, length(sdm.mod)),
                                                       na.rm = T)
            }

            if(cov.file.type=="tif") {
              SDMpred <- raster::weighted.mean(proj_stack,
                                               w = rep(sdm.weight, length(sdm.mod)),
                                               na.rm = T)
            }

          }


          if (length(sdm.weight) > 1) {

            if (cov.file.type == "csv") {
              SDMpred <- matrixStats::rowWeightedMeans(proj_blocks, w = sdm.weight, na.rm = T)
            }

            if (cov.file.type == "tif") {
              SDMpred <- raster::weighted.mean(proj_stack, w = sdm.weight, na.rm = T)
            }

          }


          if (cov.file.type == "tif") {
            SDMbinary <- SDMpred > sdm.thresh # Probability to binary
          }

          if (cov.file.type == "csv") {
            SDMbinary <- as.numeric(SDMpred > sdm.thresh) # Probability to binary
          }


      }}


      if (!missing(sam.mod)) {

        if (inherits(sam.mod, "gbm")) {

          if (cov.file.type == "tif") {
            SAMpred <- raster::predict(model = sam.mod,
                                       object = projection_df)
          }

          if (cov.file.type == "csv") {
            SAMpred <- stats::predict(sam.mod,
                                      newdata = projection_df,
                                      type = "response",
                                      na.action = stats::na.pass)
          }
        }

        if (inherits(sam.mod, "list")) {

          proj_blocks = NULL
          proj_stack <- raster::stack()

          # Make projection with each model
          for (model in 1:length(sam.mod)) {

            if (cov.file.type == "tif") {
              proj_stack <- raster::stack(proj_stack, raster::predict(model = sam.mod[[model]],
                                                                      object = projection_df))
            }


            if(cov.file.type=="csv") {
              proj_blocks <- cbind(proj_blocks,stats::predict(sam.mod[[model]],
                                                              newdata = projection_df,
                                                              type = "response",
                                                              na.action = stats::na.pass))
            }}

          if (length(sam.weight) == 1) {

            if (cov.file.type == "csv") {

              SAMpred <- matrixStats::rowWeightedMeans(proj_blocks,
                                                       w = rep(sam.weight, length(sam.mod)),
                                                       na.rm = T)
            }
            if(cov.file.type=="tif") {
              SAMpred <- raster::weighted.mean(proj_stack,
                                               w = rep(sam.weight, length(sam.mod)),
                                               na.rm = T)
            }
              }

          if (length(sam.weight) > 1) {
            if (cov.file.type == "csv") {
              SAMpred <- matrixStats::rowWeightedMeans(proj_blocks, w = sam.weight, na.rm = T)
            }
            if (cov.file.type == "tif") {
              SAMpred <- raster::weighted.mean(proj_stack, w = sam.weight, na.rm = T)
            }

          }
          }
        }

      ## Stacked
      if (!missing(sdm.mod) && !missing(sam.mod)) {
        stacked <- SDMbinary * SAMpred
      }

      # Create rasters for each projection method requested by user
      if ("binary" %in% projection.method) {

        if (cov.file.type == "tif") {
          binaryrast <- SDMbinary
        }

        if(cov.file.type == "csv") {
          binaryrast <- raster::rasterFromXYZ(cbind(projection_df[, "x"],
                                                    projection_df[, "y"],
                                                    SDMbinary), crs = prj)
        }


        if (!missing(spatial.mask)) {

          binaryrast <- raster::mask(binaryrast, spatial.mask)
        }

        # If projection of covariates are not the same as desired, reproject the projection
        if (!prj == proj.prj) {
          binaryrast <- raster::projectRaster(binaryrast, crs = proj.prj)
        }



      }

      if ("abundance" %in% projection.method) {

        if (cov.file.type == "tif") {
          abundancerast <- SAMpred
        }

        if (cov.file.type == "csv") {
          abundancerast <- raster::rasterFromXYZ(cbind(projection_df[, "x"],
                                                       projection_df[, "y"],
                                                       SAMpred), crs = prj)
        }

        if (!missing(spatial.mask)) {
          abundancerast <- raster::mask(abundancerast, spatial.mask)
        }


        # If projection of covariates are not the same as desired, reproject the projection
        if (!prj == proj.prj) {
          abundancerast <- raster::projectRaster(abundancerast, crs = proj.prj)
        }

      }

      if ("proportional" %in% projection.method) {

        if (cov.file.type == "tif") {
          proportionalrast <- SDMpred
        }

        if (cov.file.type == "csv") {
          proportionalrast <- raster::rasterFromXYZ(cbind(projection_df[, "x"],
                                                          projection_df[, "y"],
                                                          SDMpred), crs = prj)
        }

        if (!missing(spatial.mask)) {
          proportionalrast <- raster::mask(proportionalrast, spatial.mask)
        }

        # If projection of covariates are not the same as desired, reproject the projection
        if (!prj == proj.prj) {
          proportionalrast <- raster::projectRaster(proportionalrast, crs = proj.prj)
        }




      }

      if ("stacked" %in% projection.method) {

        if (cov.file.type == "tif") {
          stackedrast <- stacked
        }

        if (cov.file.type == "csv") {
          stackedrast <- raster::rasterFromXYZ(cbind(projection_df[, "x"],
                                                     projection_df[, "y"],
                                                     stacked), crs = prj)
        }

        if (!missing(spatial.mask)) {
          stackedrast <- raster::mask(stackedrast, spatial.mask)
        }

        # If projection of covariates are not the same as desired, reproject the projection
        if (!prj == proj.prj) {
          stackedrast <- raster::projectRaster(stackedrast, crs = proj.prj)
        }



      }

      ## Save projection rasters to local directory
      if (!missing(save.directory)) {

        if (exists("binaryrast")) {

          raster::writeRaster(binaryrast,
                              file = paste0(save.directory, "/", date, "_binary.tif"),
                              overwrite = T,
                              crs = proj.prj)
        }

        if (exists("abundancerast")) {

          raster::writeRaster(abundancerast,
                              file = paste0(save.directory, "/", date, "_abundance.tif"),
                              overwrite = T,
                              crs = proj.prj)
        }
        if (exists("proportionalrast")) {

          raster::writeRaster(proportionalrast,
                              file = paste0(save.directory, "/", date, "_proportional.tif"),
                              overwrite = T,
                              crs = proj.prj
          )
        }

        if (exists("stackedrast")) {

          raster::writeRaster(stackedrast,
                              file = paste0(save.directory, "/", date, "_stacked.tif"),
                              overwrite = T,
                              crs = proj.prj)
        }
      }

      # Save projection rasters to Google Drive folder
      if (!missing(save.drive.folder)) {
        # Check user email provided
        if (missing(user.email)) {
          stop(" Please provide user email linked to Google Drive account")
        }

        # Initiate Google Drive
        googledrive::drive_auth(email = user.email)
        googledrive::drive_user()
        save.folderpath <- googledrive::drive_find(pattern = save.drive.folder,
                                                   type = 'folder')

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

        filename <- paste0(tempfile(), ".tif") # Temporary file name

        if (exists("binaryrast")) {

          raster::writeRaster(binaryrast, filename, overwrite = T)
          googledrive::drive_upload(
            media = filename,
            path = googledrive::as_id(save.folderpath$id),
            name = paste0(date, "_binary.tif"),
            overwrite = T
          )
        }

        if (exists("abundancerast")) {
          raster::writeRaster(abundancerast, filename, overwrite = T, crs = prj)
          googledrive::drive_upload(
            media = filename,
            path = googledrive::as_id(save.folderpath$id),
            name = paste0(date, "_abundance.tif"),
            overwrite = T
          )
        }

        if (exists("proportionalrast")) {
          raster::writeRaster(proportionalrast, filename, overwrite = T, crs = prj)
          googledrive::drive_upload(
            media = filename,
            path = googledrive::as_id(save.folderpath$id),
            name = paste0(date, "_proportional.tif"),
            overwrite = T
          )
        }

        if (exists("stackedrast")) {
          raster::writeRaster(stackedrast, filename, overwrite = T, crs = prj)
          googledrive::drive_upload(
            media = filename,
            path = googledrive::as_id(save.folderpath$id),
            name = paste0(date, "_stacked.tif"),
            overwrite = T
          )
        }
      }
    }
    }


