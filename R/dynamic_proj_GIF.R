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
#' @details
#' Function reads in projection rasters for each date and projection.type. These are plotted using ggplot2 and combined into Graphics Interchange Format (GIF).
#'
#' For dynamic_proj_GIF to find the projection rasters for each date, then “.tif” files must be uniquely named with the date in format YYYY-MM-DD and projection.type. If more than one file name matches the date and projection.type, the function will error.
#'
#' If one of drive.folder or save.drive.folder is used then user.email for the Google Drive account must be provided. This requires users to have installed R package "googledrive" and initialised Google Drive with valid log-in credentials. Please follow instructions on https://googledrive.tidyverse.org/.
#' @references Wickham, H., and Chang, W, 2016. Package ‘ggplot2’. Create elegant data visualisations using the grammar of graphics. Version, 2(1), pp.1-189.
#' @return Exports GIF to Google Drive folder or local directory.

dynamic_proj_GIF<-function(dates,projection.type,drive.folder=NULL,user.email=NULL,local.directory=NULL,save.drive.folder=NULL,save.directory=NULL){

  # Check neccessary arguments have been provided
  if(missing(drive.folder) && missing(local.directory)){stop("Please provide one of local.directory or drive.folder to import projection covariates from.")}
  if(missing(save.directory) && missing(save.drive.folder)){stop("Please provide one of save.directory or save.drive.folder to export projection data.frame to.")}

  # Match projection type to available options
  projection.type<-match.arg(projection.type,choices=c("proportional","binary","abundance" , "stacked"))

  tempfilelist<-NULL # Empty vector to bind list of written .png file names to

  for(x in 1:length(dates)){ # Iterate through each projection date.

  date<-dates[x]

  ### Read in projection rasters from local directory
  if(!missing(local.directory)){
    if(!dir.exists(local.directory)){stop("local.directory does not exist")}

    # Get raster file name
    filename<-list.files(local.directory,full.names = T) # List all files in the directory
    filename<-filename[grep(date,filename)] # Select only file names for this date
    filename<-filename[grep(projection.type,filename)]# Select only file names for this date and projection type. There should only be one matching this description in the directory. See function details for more information.

    #Read raster file into R
    projectionraster<-raster::raster(filename)}


  ### Read in projection rasters from Google Drive folder
  if(!missing(drive.folder)){
    # Check user email provided
    if (missing(user.email)){stop("user.email is missing. Please provide user email linked to Google Drive account")}

    #Initialise Google Drive
    googledrive::drive_auth(email=user.email)
    googledrive::drive_user()

    # Get raster file name
    filename<-googledrive::drive_ls(path = paste0(drive.folder))$name  # List all files in the Google Drive folder
    filename<-filename[grep(date,filename)] # Select only file names for this date
    filename<-filename[grep(projection.type,filename)] # Select only file names for this date and projection type. There should only be one matching this description in the folder. See function details for more information.

    #Read raster file into R
    pathforthisfile<-paste0(tempfile(),".tif") # Create temp file name
    googledrive::drive_download(file=filename,path=pathforthisfile,overwrite=T) # Download projection raster to temporary folder
    projectionraster<-raster::raster(pathforthisfile)}

  # Convert raster into data frame for plotting data with ggplot2
  projectionraster<-as.data.frame(raster::rasterToPoints(projectionraster))
  colnames(projectionraster)<-c("x","y","value") # Rename columns

  # Plot projection with ggplot2
  plot<- ggplot2::ggplot(data = projectionraster) +
   ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value))+
   ggplot2::ggtitle(dates[x])+
   ggplot2::scale_fill_gradientn(colours = terrain.colors(7),trans = 'reverse',name = projection.type)+
   ggplot2::theme(panel.background =  ggplot2::element_rect(fill = "white", color = "white"),plot.title =  ggplot2::element_text(size=25,face = "bold",hjust = 0.5),
          axis.line= ggplot2::element_blank(),axis.ticks= ggplot2::element_blank(),axis.text= ggplot2::element_blank(), legend.text =  ggplot2::element_text(size=24),
          legend.title =  ggplot2::element_text(size=30,face="bold"))#+

  # Save temporary  png file of plot
  tempfilename<-paste0(tempfile(),".png")
  tempfilelist<-c(tempfilelist,tempfilename)
  png(file=paste0(tempfilename))
  print(plot)
  dev.off()}

  # Read in all png images and create a GIF file
  GIF<-tempfilelist %>%
   magick::image_read() %>% # reads each path file
   magick::image_join() %>% # joins image
   magick::image_animate(fps=1)

  #Save GIF file to local directory
  if(!missing(save.directory)){
    if(!dir.exists(save.directory)){stop("save.directory does not exist")}
    magick::image_write(GIF,paste0(save.directory,"/",projection.type,".gif"))
    print(paste0("GIF successfully saved to ",save.directory,"/",projection.type,".gif"))}

  #Save GIF file to Google Drive folder
  if(!missing(save.drive.folder)){
    # Check user email provided
    if (missing(user.email)){stop("user.email is missing. Please provide user email linked to Google Drive account")}

    #Initialise Google Drive
    googledrive::drive_auth(email=user.email)
    googledrive::drive_user()

    filename<-paste0(tempfile(),".gif")
    magick::image_write(GIF,filename) # Write image to temporary location for upload to Google Drive
    save.folderpath<-googledrive::drive_find(pattern=paste0(save.drive.folder), type='folder')
    googledrive::drive_upload(media=filename,path=googledrive::as_id(save.folderpath$id),name=paste0(projection.type,".gif"),overwrite=T)
    print(paste0("GIF successfully saved to Google Drive location: ",save.drive.folder,"/",projection.type,".gif"))}} # Upload GIF to Google Drive folder
