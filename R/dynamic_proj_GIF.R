#' Create GIF of dynamic species distribution and abundance projections
#'
#' Plots dynamic species distribution and abundance projections through time and combines images into a GIF.
#' @param dates a character vector , projection dates in format YYYY-MM-DD.
#' @param drive.folder optional; a character string, the Google Drive folder to read projection rasters from.
#' @param projection.type a character string, the type of distribution or abundance projection to plot. One of "'proportional'", "'binary'", "'abundance'" and "'stacked'".
#' @param local.directory optional; a character string, the path to local directory to read projection rasters from.
#' @param user.email optional; a character string, user email for initialising Google Drive. Required if drive.folder or save.drive.folder used.
#' @param save.directory optional; a character string, path to local directory to save GIF to.
#' @param save.drive.folder optional; a character string, Google Drive folder to save GIF to.
#' @param width optional; a numeric value, the GIF width in pixels. Default = 480.
#' @param height optional; a numeric value, the GIF height in pixels. Default = 480.
#' @param legend.max optional; a numeric value, the maximum limit of legend values to standardise across projections.
#' @param legend.min optional; a numeric value, the minimum limit of legend values to standardise across projections.
#' @param legend.name optional; a character string, the name for the legend title. Default = projection.type.
#' @param file.name optional, a character string, the name for the saved GIF file. Default = projection.type.
#' @details
#' Function reads in projection rasters for each date and projection.type. These are plotted using ggplot2 and combined into Graphics Interchange Format (GIF).
#'
#' For dynamic_proj_GIF to find the projection rasters for each date, then “.tif” files must be uniquely named with the date in format YYYY-MM-DD and projection.type. If more than one file name matches the date and projection.type, the function will error.
#'
#' If one of drive.folder or save.drive.folder is used then user.email for the Google Drive account must be provided. This requires users to have installed R package "googledrive" and initialised Google Drive with valid log-in credentials. Please follow instructions on https://googledrive.tidyverse.org/.
#' @references Wickham, H., and Chang, W, 2016. Package ‘ggplot2’. Create elegant data visualisations using the grammar of graphics. Version, 2(1), pp.1-189.
#' @return Exports GIF to Google Drive folder or local directory.
#'@export


dynamic_proj_GIF <-
  function(dates,
           projection.type,
           drive.folder = NULL,
           user.email = NULL,
           local.directory = NULL,
           save.drive.folder = NULL,
           save.directory = NULL,
           width = 480,
           height = 480,
           legend.max = NULL,
           legend.min = NULL,
           legend.name = NULL,
           file.name = NULL) {

    # Check neccessary arguments have been provided
    if (missing(drive.folder) && missing(local.directory)) {
      stop("Provide local.directory or drive.folder to import covariates.")
    }

    if (missing(save.directory) && missing(save.drive.folder)) {
      stop("Provide save.directory or save.drive.folder to export data frame.")
    }

    # Match projection type to available options
    projection.type <- match.arg(projection.type,
                                 choices = c("proportional",
                                             "binary",
                                             "abundance" ,
                                             "stacked"))

    tempfilelist <- NULL # Empty vector to bind  written .png file names to

    # Iterate through each projection date.
    for (x in 1:length(dates)) {

      date <- dates[x]

      # Read in projection rasters from local directory
      if (!missing(local.directory)) {
        if (!dir.exists(local.directory)) {
          stop("local.directory not found")
        }

        # Get raster file name
        filename <- list.files(local.directory, full.names = T) # List files
        filename <- filename[grep(date, filename)] # Files for this date and
        filename <- filename[grep(projection.type, filename)]# projection type.

        # Read raster file into R
        projraster <- raster::raster(filename)
      }


      ### Read in projection rasters from Google Drive folder
      if (!missing(drive.folder)) {

        # Check user email provided
        if (missing(user.email)) {stop("Provide user email for Google Drive.")}

        #Initialise Google Drive
        googledrive::drive_auth(email = user.email)
        googledrive::drive_user()

        # Get raster file name
        filename <- googledrive::drive_ls(path = paste0(drive.folder))$name
        filename <- filename[grep(date, filename)] # Select files for date and
        filename <- filename[grep(projection.type, filename)] # projection.type.

        #Read raster file into R
        pathforthisfile <- paste0(tempfile(), ".tif") # Create temp file name

        googledrive::drive_download(file = filename,
                                    path = pathforthisfile,
                                    overwrite = T) # Download raster to temp dir
        projraster <- raster::raster(pathforthisfile) # Import raster
      }

      # Convert raster to data frame for plotting data with ggplot2
      projraster <- as.data.frame(raster::rasterToPoints(projraster))
      colnames(projraster) <- c("x", "y", "value") # Rename columns

      #Set default plot parameters
      if (missing(legend.max)) {
        legend.max <- max(projraster[, "value"], na.rm = T)
      }
      if (missing(legend.min)) {
        legend.min <- min(projraster[, "value"], na.rm = T)
      }
      if (missing(legend.name)) {
        legend.name <- projection.type
      }

      # Plot projection with ggplot2
      plot <- ggplot2::ggplot(data = projraster) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value)) +
        ggplot2::ggtitle(dates[x]) +
        ggplot2::scale_fill_gradientn(
          colours = terrain.colors(7),
          trans = 'reverse',
          name = legend.name,
          limits = c(legend.max, legend.min)
        ) +
        ggplot2::theme(
          panel.background =  ggplot2::element_rect(fill = "white", color = "white"),
          plot.title =  ggplot2::element_text(
            size = 25,
            face = "bold",
            hjust = 0.5
          ),
          axis.line = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          legend.text =  ggplot2::element_text(size = 24),
          legend.title =  ggplot2::element_text(size = 30, face = "bold")
        )

      # Save temporary  png file of plot
      tempfilename <- paste0(tempfile(), ".png")
      tempfilelist <- c(tempfilelist, tempfilename)
      png(file = paste0(tempfilename),
          width = width,
          height = height)
      print(plot)
      dev.off()
    }

    # Read in all png images and create a GIF file
    GIF <- tempfilelist %>%
      magick::image_read() %>% # reads each path file
      magick::image_join() %>% # joins image
      magick::image_animate(fps = 1)

    if (missing(file.name)) {file.name <- projection.type}

    # Save GIF file to local directory
    if (!missing(save.directory)) {
      magick::image_write(GIF, paste0(save.directory, "/", file.name, ".gif"))
      print(paste0("GIF saved to ", save.directory,"/", file.name, ".gif"))
    }

    # Save GIF file to Google Drive folder
    if (!missing(save.drive.folder)) {
      # Check user email provided
      if (missing(user.email)) {
        stop("Provide user email linked to Google Drive account")
      }

      #Initialise Google Drive
      googledrive::drive_auth(email = user.email)
      googledrive::drive_user()

      filename <- paste0(tempfile(), ".gif")
      magick::image_write(GIF, filename) # Write image to temporary location
      save.folderpath <-
        googledrive::drive_find(pattern = paste0(save.drive.folder),
                                type = 'folder')

      googledrive::drive_upload(
        media = filename,
        path = googledrive::as_id(save.folderpath$id),
        name = paste0(file.name, ".gif"),
        overwrite = T
      )
      print(paste0("GIF saved to : ",save.drive.folder,"/",file.name,".gif"))
    }
  }
