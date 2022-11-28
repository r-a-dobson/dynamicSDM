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

dynamic_proj_covariates<-function(dates,varnames,drive.folder=NULL,user.email=NULL, local.directory=NULL,spatial.ext=NULL,spatial.res.degrees=NULL,resample.method=NULL,save.directory=NULL,save.drive.folder = NULL){

  # Check arguments to prevent error downstream

  if (missing(spatial.ext)){message("spatial.ext is missing. All rasters must have the same spatial extent or error will occur. ")}
  if (missing(spatial.res.degrees)){message("spatial.res.degrees is missing. All rasters must have the same resolution or error will occur.")}

  if(!missing(spatial.res.degrees)){
    if(missing(resample.method)){stop("Please provide a resample method or vector of methods for each variable. ")}
    if(!length(resample.method)==1 &&!length(resample.method)==length(varnames)){stop("resample.method must be of length(1) to apply method to all variables or length(varnames) detailing methods to apply to each variable in order")}}

  if(missing(local.directory) && missing(drive.folder)){stop("Please provide one of local.directory or drive.folder to download variable rasters from")}

  if(missing(save.directory) && missing(save.drive.folder)){stop("Please provide one of save.directory or save.drive.folder to export projection data.frame to.")}


# Process spatial extent given for cropping rasters before stacking into covariate data frame

  if(!missing(spatial.ext)){
    if(!any(class(spatial.ext)==c("numeric","Extent","RasterLayer","Polygon"))){ # Check spatial.ext appropriate object class.
    stop("spatial.ext must be of class numeric, Extent, RasterLayer or Polygon")}

    ### Numeric extent to co-ords
    if(class(spatial.ext)=="numeric" && !length(spatial.ext)==4){stop("spatial.ext numeric vector should be of length four c(xmin, xmax, ymin and ymax)")}

    xmin<-extract_xy_min_max(spatial.ext)[1]
    xmax<-extract_xy_min_max(spatial.ext)[2]
    ymin<-extract_xy_min_max(spatial.ext)[3]
    ymax<-extract_xy_min_max(spatial.ext)[4]}

## If drive.folder given intitate Google Drive and list files in the folder(s)

  if(!missing(drive.folder)){
    # Check user email provided
    if (missing(user.email)){stop("user.email is missing. Please provide user email linked to Google Drive account")}

    # Initiate Google Drive
    googledrive::drive_auth(email=user.email)
    googledrive::drive_user()

    #Check folder exists in user's Google Drive
    folderpath<-googledrive::drive_find(pattern=paste0(drive.folder), type='folder')
    if(nrow(folderpath)==0){stop("drive.folder provided does not exist.")}

    # List all files in Google Drive folders
    list.of.drive.files=NULL
    for(folder in 1:length(drive.folder)){list.of.drive.files<-c(list.of.drive.files,googledrive::drive_ls(path = paste0(drive.folder[folder]))$name)}}

## If local.directory list files in the folder(s)

  if(!missing(local.directory)){
    if(!dir.exists(local.directory)){stop("local.directory does not exist")}

    list.of.directory.files<-list.files(local.directory,full.names = T)}


## Iterate through each projection date and variable, import raster, crop and resample if required and stack into projection covariate data frame.

  listofdone<-NULL # Empty vector to record dates completed

  for(x in 1:length(dates)){

    date<-dates[x]

    stack<- raster::stack() # Create empty stack to bind rasters for this date too

  for (v in 1:length(varnames)){

    name<-varnames[v]

    ## Read in raster for this variable and date from Google Drive
    if(!missing(drive.folder)){
    file.to.download<-list.of.drive.files[grep(name,list.of.drive.files)] # Select files that include this variable name
    file.to.download<-file.to.download[grep(date,file.to.download)]  # Select files that include this variable name and this projection date
    pathforthisfile<-paste0(tempfile(),".tif") # Create temp file name
    googledrive::drive_download(file=file.to.download,path=pathforthisfile,overwrite=T) # Download raster from Google Drive to temporary location
    raster<- raster::raster(pathforthisfile)} # Read raster into R from temp file

    ## Alternatively, read in raster for this variable and date from local directory
    if(!missing(local.directory)){
    file.to.import<-list.of.directory.files[grep(name,list.of.directory.files)] # Select files that include this variable name
    file.to.import<-file.to.import[grep(date,file.to.import)] # Select files that include this variable name and this projection date
    raster<- raster::raster(file.to.import)} # Read raster into R from local directory

    #Crop to spatial.ext provided so that all rasters are same extent for stacking
    if(!missing(spatial.ext)){raster<-raster::crop(raster,raster::extent(xmin,xmax,ymin,ymax)) # Crop raster to spatial.ext co-ordinates if provided

    # Resample to same resolution if neccessary following resample.method specified for the variable
    if(!missing(spatial.res.degrees)){
      r <- raster::raster(raster::extent(xmin,xmax,ymin,ymax)) # Create empty raster of same extent
      raster::res(r) <- spatial.res.degrees # Give empty raster of same resolution as aim to resample by
      r<- raster::setValues(r,1:raster::ncell(r))} # Set fake raster values

      if(length(resample.method)==1){raster<-raster::resample(raster,r,method=resample.method)} # Resample raster to empty raster with desired resolution using single method given
      if(!length(resample.method)==1){raster<-raster::resample(raster,r,method=resample.method[v])}}# Resample raster to empty raster with desired resolution using method given for this variable

    stack <- raster::stack(stack ,raster)} ## Add raster to stack and iterate through all variables

  names(stack)<-varnames # Label each layer in stack as appropriate variable

  stack<-as.data.frame(raster::rasterToPoints(stack)) # Create data.frame from raster stack.

# Save covariate data.frame to Google Drive folder
  if(!missing(save.drive.folder)){
    # Check user email provided
    if (missing(user.email)){stop("user.email is missing. Please provide user email linked to Google Drive account")}

    # Initiate Google Drive
    googledrive::drive_auth(email=user.email)
    googledrive::drive_user()

    #Check folder exists in user's Google Drive
    save.folderpath<-googledrive::drive_find(pattern=paste0(save.drive.folder), type='folder')
    if(nrow(save.folderpath)==0){stop("save.drive.folder provided does not exist.")}

    csvfile<-paste0(tempfile(),".csv")
    write.csv(stack,file=csvfile) # Save projection covariate .csv file to temporary location
    googledrive::drive_upload(media=csvfile,path=googledrive::as_id(save.folderpath$id),name=paste0(date,"_projection_dataframe.csv"),overwrite=T)} #Upload covariates to Google Drive. File name includes the date that these covariate represent

  # Alternatively save covariate data.frame to local directory
  if(!missing(save.directory)){
    if(!dir.exists(save.directory)){stop("save.directory does not exist")}
   write.csv(stack,file=paste0(save.directory,"/",date,"_projection_dataframe.csv"))} #File name includes the date that these covariate represent

  listofdone<-rbind(listofdone,date)} # Record that this date has been completed

  if(!missing(save.directory)){print(paste0("Data successfully extracted and saved to folders:", save.directory))}
  if(!missing(save.drive.folder)){print(paste0("Data successfully extracted and saved to folders:", save.drive.folder))}

  return(listofdone)}


