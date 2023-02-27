#'Create GIF of dynamic species distribution and abundance projections
#'
#'Plots dynamic species distribution and abundance projections through time and combines images into
#'a GIF.
#'@param dates a character vector , projection dates in format "YYYY-MM-DD".
#'@param drive.folder optional; a character string, the Google Drive folder to read projection
#'  rasters from. Folder must be uniquely named within Google Drive. Do not provide path.
#'@param projection.type a character string, the type of distribution or abundance projection to
#'  plot. One of `proportional`, `binary`, `abundance` and `stacked`.
#'@param local.directory optional; a character string, the path to local directory to read
#'  projection rasters from.
#'@param user.email optional; a character string, user email for initialising Google Drive. Required
#'  if `drive.folder` or `save.drive.folder` used.
#'@param save.directory optional; a character string, path to local directory to save GIF to.
#'@param save.drive.folder optional; a character string, Google Drive folder to save GIF to. Folder
#'  must be uniquely named within Google Drive. Do not provide path.
#'@param width optional; a numeric value, the GIF width in inches Default = 480.
#'@param height optional; a numeric value, the GIF height in inches Default = 480.
#'@param legend.max optional; a numeric value, the maximum limit of legend values to standardise
#'  across projections.
#'@param legend.min optional; a numeric value, the minimum limit of legend values to standardise
#'  across projections.
#'@param legend.name optional; a character string, the name for the legend title. Default =
#'  projection.type.
#'@param colour.palette.custom optional; a character string or vector, the colours to use as plot
#'  colour palette.
#'@param colour.palette optional; a character string, the colormap option to use from `viridis`. See
#'  details for colour palette options.
#'@param file.name optional, a character string, the name for the output GIF file. Default =
#'  `projection.type`.
#'@param borders a logical indicating whether to add country borders to map. Default = `FALSE`.
#'@param border.regions optional; a character string or vector, the region or regionss for which to
#'  add map borders. Required if `borders = TRUE`.
#'@param border.colour optional; a character vector, the colour for plotted map borders. Default =
#'  `black.`
#'@details Function reads in projection rasters for each date. These are plotted using `ggplot2` and
#'  combined into a Graphics Interchange Format (GIF).
#'
#'  # Import projection rasters
#'
#'  Projection rasters for each date must be “tif” files that are uniquely named with the date
#'  in format "YYYY-MM-DD" and `projection.type.` If more than one file name matches the date and
#'  `projection.type`, the function will error.
#'
#'  # Google Drive compatibility
#'
#'  If `drive.folder` or `save.drive.folder` is given, please ensure the folder name is unique
#'  within your Google Drive. Do not provide the path if the folder is nested within others.
#'
#'  If one of `drive.folder` or `save.drive.folder` are used then `user.email` is required to access
#'  the appropriate Google Drive user account. This requires users to have installed R package
#'  `googledrive` and initialised Google Drive with valid log-in credentials. Please follow
#'  instructions on <https://googledrive.tidyverse.org/>.
#'
#'  Options for colour palettes using `viridis` are illustrated at:
#'  <https://ggplot2.tidyverse.org/reference/scale_viridis.html>. Available options include: "magma"
#'  (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option),
#'  "cividis" (or "E"), "rocket" (or "F"), "mako"(or "G") and "turbo" (or "H").
#'
#' @references Wickham, H., and Chang, W, 2016. Package ‘ggplot2’. Create elegant data
#'   visualisations using the grammar of graphics. Version, 2(1), pp.1-189.
#' @return Exports GIF to Google Drive folder or local directory.
#' @examples
#'projectiondates <- dynamic_proj_dates(startdate = "2018-01-01",
#'                                      enddate = "2018-12-01",
#'                                      interval = 3,
#'                                      interval.level = "month")
#'data(sample_proj_rast)
#'\donttest{
#'# Save sample projection rasters to replicate output from `dynamic_proj()`
#'
#'raster::writeRaster(
#'  sample_proj_rast,
#'  filename = paste0(tempdir(), "/", paste0(projectiondates, "_proportional.tif")),
#'  bylayer = TRUE,
#'  format = "GTiff",
#'  overwrite = TRUE
#')
#'dynamic_proj_GIF(
#'dates = projectiondates,
#'projection.type = "proportional",
#'local.directory = tempdir(),
#'save.directory = tempdir()
#')
#'}
#'@export

dynamic_proj_GIF <- function(dates,
                             projection.type,
                             drive.folder,
                             user.email,
                             local.directory,
                             save.drive.folder,
                             save.directory,
                             width = 10,
                             height = 10,
                             legend.max,
                             legend.min,
                             legend.name,
                             file.name,
                             borders = FALSE,
                             border.regions,
                             border.colour = "black",
                             colour.palette.custom,
                             colour.palette) {

    # Check neccessary arguments have been provided
    if (missing(drive.folder) && missing(local.directory)) {
      stop("Provide local.directory or drive.folder to import covariates.")
    }

   if (missing(colour.palette.custom)) {
     colour.palette <- "inferno"
   }

    if (missing(save.directory) && missing(save.drive.folder)) {
      stop("Provide save.directory or save.drive.folder to export data frame.")
    }

    # Match projection type to available options
    projection.type <- match.arg(projection.type, choices = c("proportional",
                                                              "binary",
                                                              "abundance" ,
                                                              "stacked"))

    tempfilelist <- NULL # Empty vector to bind  written .png file names to

    # Iterate through each projection date.
    for (d in 1:length(dates)) {

    date <- dates[d]

      # Read in projection rasters from local directory
      if (!missing(local.directory)) {
        if (!dir.exists(local.directory)) {
          stop("local.directory not found")
        }

        # Get raster file name
        filename <- list.files(local.directory, full.names = TRUE) # List files
        filename <- filename[grep(date, filename)] # Files for this date and
        filename <- filename[grep(projection.type, filename)]# projection type.
        filename <- filename[grep(".tif$", filename)]# projection type.

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

        drive.folderpath <-  googledrive::drive_find(pattern = paste0(drive.folder),
                                                    type = 'folder')

        if(nrow(drive.folderpath)>1) {
          drive.folderpath <- drive.folderpath[grep(paste0("^", drive.folder, "$"),
                                                    drive.folderpath$name), ]
        }

        drivefiles <- googledrive::drive_ls(path = googledrive::as_id(drive.folderpath$id))[,1:2]
        fileid <- drivefiles[grep(date, drivefiles$name),]
        fileid <- fileid[grep(projection.type, fileid$name),"id"]

        #Read raster file into R
        pathforthisfile <- paste0(tempfile(), ".tif") # Create temp file name

        # Download raster to temp dir
        googledrive::drive_download(file = googledrive::as_id(fileid$id),
                                    path = pathforthisfile, overwrite = TRUE)
        projraster <- raster::raster(pathforthisfile) # Import raster
      }

      # Convert raster to data frame for plotting data with ggplot2
      projraster <- as.data.frame(raster::rasterToPoints(projraster))
      colnames(projraster) <- c("x", "y", "value") # Rename columns

      #Set default plot parameters
      if (missing(legend.max)) {
        legend.max <- max(projraster[, "value"], na.rm = TRUE)
      }
      if (missing(legend.min)) {
        legend.min <- min(projraster[, "value"], na.rm = TRUE)
      }
      if (missing(legend.name)) {
        legend.name <- projection.type
      }


      x <- projraster$x
      y <- projraster$y

      value <- projraster$value
      # Plot projection with ggplot2

if(!missing(colour.palette)){

  if(!borders){
  ggplot2::ggplot(data = projraster) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value)) +
        ggplot2::ggtitle(as.character(date)) +
        viridis::scale_fill_viridis(
          option=colour.palette,
          name = legend.name,
          limits = c(legend.min, legend.max)) +
        ggplot2::theme(panel.background =  ggplot2::element_rect(fill = "white", color = "white"),
                       plot.title =  ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
          axis.line = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          legend.text =  ggplot2::element_text(size = 24),
          legend.title =  ggplot2::element_text(size = 30, face = "bold"))}



  if(borders){

    ggplot2::ggplot(data = projraster) +
      ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value)) +
      ggplot2::ggtitle(as.character(date)) +
      viridis::scale_fill_viridis(
        option=colour.palette,
        name = legend.name,
        limits = c(legend.min, legend.max)) +
      ggplot2::theme(panel.background =  ggplot2::element_rect(fill = "white", color = "white"),
                     plot.title =  ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                     axis.line = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     legend.text =  ggplot2::element_text(size = 24),
                     legend.title =  ggplot2::element_text(size = 30, face = "bold"))+
      ggplot2::borders(database = "world",
              regions = border.regions ,
              fill = NA,
              colour = border.colour,
              xlim = c(min(projraster$x,na.rm=T), max(projraster$x,na.rm=T)),
              ylim = c(min(projraster$y,na.rm=T), max(projraster$y,na.rm=T)))}



  }


  if(!missing(colour.palette.custom)){

    if(!borders){
      ggplot2::ggplot(data = projraster) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value)) +
        ggplot2::ggtitle(as.character(date)) +
      ggplot2::scale_fill_gradientn( limits = c(legend.min, legend.max),
                              colours = colour.palette.custom,
                              name = legend.name)+
        ggplot2::theme(panel.background =  ggplot2::element_rect(fill = "white", color = "white"),
                       plot.title =  ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                       axis.line = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       axis.text = ggplot2::element_blank(),
                       legend.text =  ggplot2::element_text(size = 24),
                       legend.title =  ggplot2::element_text(size = 30, face = "bold"))}



    if(borders){

      ggplot2::ggplot(data = projraster) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value)) +
        ggplot2::ggtitle(as.character(date)) +
        ggplot2::scale_fill_gradientn( limits = c(legend.min, legend.max),
                                       colours = colour.palette.custom,
                                       name = legend.name)+
        ggplot2::theme(panel.background =  ggplot2::element_rect(fill = "white", color = "white"),
                       plot.title =  ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                       axis.line = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       axis.text = ggplot2::element_blank(),
                       legend.text =  ggplot2::element_text(size = 24),
                       legend.title =  ggplot2::element_text(size = 30, face = "bold"))+
        ggplot2::borders(database = "world",
                regions = border.regions ,
                fill = NA,
                colour = border.colour,
                xlim = c(min(projraster$x,na.rm=T), max(projraster$x,na.rm=T)),
                ylim = c(min(projraster$y,na.rm=T), max(projraster$y,na.rm=T)))
    }

    }


      # Save temporary  png file of plot
      tempname <- paste0(tempfile(), ".png")
      tempfilelist <- c(tempfilelist, tempname)

      ggplot2::ggsave(paste0(tempname),
                      width = width,
                      height = height,
                      units = "in")

    }

    # Read in all png images and create a GIF file
    GIF <- tempfilelist %>%
      magick::image_read() %>% # reads each path file
      magick::image_join() %>% # joins image
      magick::image_animate(fps = 1)

    if (missing(file.name)) {file.name <- projection.type}

    # Save GIF file to local directory
    if (!missing(save.directory)) {

        if (!dir.exists(save.directory)) {
          stop("save.directory not found")
        }

      magick::image_write(GIF, paste0(save.directory, "/", file.name, ".gif"))

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
      save.folderpath <-  googledrive::drive_find(pattern = paste0(save.drive.folder),
                                                  type = 'folder')

      # If more than one folder partially matches, use grep to get exact match
      if(nrow(save.folderpath)>1) {
        save.folderpath <- save.folderpath[grep(paste0("^", save.drive.folder, "$"),
                               save.folderpath$name),]
      }

      # If exact match to more than one folder then not uniquely named. Cannot write file.
      if (nrow(save.folderpath) > 1) {
        stop("save.drive.folder is not uniquely named in your Google Drive ")
      }

      googledrive::drive_upload(
        media = filename,
        path = googledrive::as_id(save.folderpath$id),
        name = paste0(file.name, ".gif"),
        overwrite = TRUE
      )

    }
  }
