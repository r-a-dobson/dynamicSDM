#' Project species distribution and abundance models onto dynamic environmental covariates.
#'
#' Projects fitted species distribution and abundance models onto projection covariates for each date given.
#'
#' @param dates a character string, vector of dates in format YYYY-MM-DD.
#' @param projection.method a character string or vector, the method or methods to project distribution and abundance onto projection covariates. Options include "'proportional'", "'binary'", "'abundance'" and "'stacked'". See details for more information.
#' @param drive.folder optional; a character string, the Google Drive folder to read projection covariate data frames from.
#' @param local.directory optional; a character string, the path to a local directory to read projection covariate data frames from.
#' @param user.email optional; a character string, user email for initialising Google Drive. Required if drive.folder or save.drive.folder used.
#' @param sdm.mod optional; a model object or list of model objects fitted to species distribution data.
#' @param sam.mod optional; a model object or list of model objects fitted to species abundance data.
#' @param sdm.weight optional; a numeric string, weights given to each sdm.mod model projection, given in the same order as the sdm.mod list. Default is equal weighting to all models.
#' @param sdm.thresh optional; a numeric value, the threshold to convert projected distribution suitability into binary presence-absence. Default 0.5. Required if projection.method is "'binary'" or "stacked".
#' @param sam.weight optional; a numeric string, weights given to each sdm.mod model projection, given in the same order as the sam.mod list. Default is equal weighting to all models.
#' @param save.directory optional; a character string, path to local directory to save projection rasters to.
#' @param save.drive.folder optional; a character string, Google Drive folder to save projection rasters to.
#' @details Function projects a model object or list of model objects onto projection covariate data frames for each projection date given.
#'
#'Projection covariate data frames must be saved “.csv” files in the drive.folder or save.drive.folder given, and must be unique in containing the relevant projection date in “YYYY-MM-DD” format. For instance, two or more “.csv” files saved with the given Google Drive folder or local directory that contain the projection date will result in function error.  Additionally, column names of projection covariate data frames must match the explanatory variable names that fitted models are trained on.
#'
#'When multiple models are provided, the function projects each model onto the projection covariates and takes the average value across all model projections. However, if users have specified sdm.weight or sam.weight then the weighted average of model projections is returned. For example, this could be used to down weight projections of poorly performing models in an ensemble.
#'
#'The "'proportional'" projection.method projects sdm.mod model objects onto projection covariates for each date, exporting rasters for projected distribution suitability, a continuous measure between 0 (least suitable) and 1 (most suitable).
#'
#'The "'binary'" projection.method projects sdm.mod onto projection covariates for each date, exporting rasters for projected binary presence (1) or absence (0), derived from "'proportional'" projected distribution suitability using user-specified threshold (sdm.thresh) or default threshold of 0.5.
#'
#'The "'abundance'" projection.method projects sam.mod onto projections covariates for each date, exporting rasters for projected abundance in the abundance units that sam.mod were fitted onto.
#'
#'The "stacked" projection.method follows the "'binary'" projection.method and then projects abundance onto only binary presence (1) cells using "abundance" projection.method.
#'
#'One or both of save.drive.folder and save.directory are required to specify where projection rasters are to be saved.
#'
#'If one of drive.folder or save.drive.folder are used then user.email is required to access the appropriate Google Drive user account. This requires users to have installed R package "googledrive" and initialised Google Drive with valid log-in credentials. Please follow instructions on https://googledrive.tidyverse.org/.
#' @return Exports model projection rasters for each projection date to user-specified Google Drive folder or local directory.
#'@export

dynamic_proj<-function(dates,projection.method,local.directory=NULL,drive.folder=NULL,user.email=NULL,sdm.mod=NULL,sdm.thresh=0.5,sdm.weight=1,sam.mod=NULL,sam.weight=1, save.directory=NULL,save.drive.folder=NULL){

  ### Set thresholds
  if(!missing(sdm.mod)){
    if(missing(sdm.thresh)){message("No sdm.thresh specified. Default 0.5")}
    if(!missing(sdm.thresh)){if(!length(sdm.thresh)==1){stop("sdm.thresh should be of length(1) ")}}
    if(missing(sdm.weight)){message("No sdm.weight specified. Default equal weighting.")}
    if(!length(sdm.weight)==length(sdm.mod) && !length(sdm.weight)==1){stop("If sdm.weight specified, sdm.weight should be of length(1) or equal to number of models given")}}

  if(!missing(sam.mod)){
    if(missing(sam.weight)){message("No sam.weight specified. Default equal weighting")}
    if(!length(sam.weight)==length(sam.mod) && !length(sam.weight)==1){stop("If sam.weight specified, sam.weight should be of length(1) or equal to number of models given")}}

  #Check directory exists if provided
  if(!missing(local.directory)){if(!dir.exists(local.directory)){stop("local.directory does not exist")}}
  if(missing(save.directory) && missing(save.drive.folder)){stop("No folder or directory to save projections too. Please provide save.directory or save.drive.folder.")}

  #Check Google Drive folder exists if provided
  if(!missing(drive.folder)){
    # Check user email provided
    if (missing(user.email)){stop("user.email is missing. Please provide user email linked to Google Drive account")}
    googledrive::drive_auth(email=user.email)  #Initiate Google Drive
    googledrive::drive_user()
    folderpath<-googledrive::drive_find(pattern=paste0(drive.folder), type='folder')
    if(nrow(folderpath)==0){stop("drive.folder provided does not exist.")}}

# Match projection.method argument to available options
  projection.method<-match.arg(projection.method,choices=c("proportional","binary","abundance" , "stacked"),several.ok = T)


  for(x in 1:length(dates)){

    date<-dates[x]

    ### Read in projection data from local directory
    if(!missing(local.directory)){
    filename<-list.files(local.directory,full.names = T) # List all files in local directory
    filename<-filename[grep(date,filename)] # Select file name that matches projection date
    filename<-filename[grep("*.csv",filename)] # Select file name that leads to a .csv file
    projection_df<-read.csv(filename)} # Read in projection covariate data frame

    ### Read in projection data from Google Drive folder
    if(!missing(drive.folder)){
    filename<-googledrive::drive_ls(path = paste0(drive.folder))$name  ## List all files in Google Drive folder
    filename<-filename[grep(date,filename)] #Select file name that matches projection date
    filename<-filename[grep("*.csv",filename)]
    pathforthisfile<-paste0(tempfile(),".csv") # Create temporary file name
    googledrive::drive_download(file=filename,path=pathforthisfile,overwrite=T) # Download projection covariates to temp directory
    projection_df<-read.csv(pathforthisfile)} # Read in projection covariate data frame

    ### If one model object given for either

  if(!missing(sdm.mod)){

    if(!class(sdm.mod)=="list"){
      SDMpred<- predict(sdm.mod, newdata = projection_df,type = "response",na.action = na.pass)
      SDMbinary<-as.numeric(SDMpred>sdm.thresh)}


    if(class(sdm.mod)=="list"){

      projections_blocks=NULL
      # Make projection from each model
      for(model in 1:length(sdm.mod)){projections_blocks<-cbind(projections_blocks,predict(sdm.mod[[model]], newdata = projection_df,type = "response",na.action = na.pass))}


      if(length(sdm.weight)==1){SDMpred<-matrixStats::rowWeightedMeans(projections_blocks, w = rep(sdm.weight,length(sdm.mod)),na.rm=T)}
      if(length(sdm.weight)>1){SDMpred<-matrixStats::rowWeightedMeans(projections_blocks, w = sdm.weight,na.rm=T)}

      SDMbinary<-as.numeric(SDMpred>sdm.thresh)}}



  if(!missing(sam.mod)){

    if(class(sam.mod)=="gbm"){SAMpred<-predict(sam.mod, newdata = projection_df,type = "response",na.action = na.pass)}

    if(class(sam.mod)=="list"){

    projections_blocks=NULL
    # Make projection from each model
    for(model in 1:length(sam.mod)){projections_blocks<-cbind(projections_blocks,predict(sam.mod[[model]], newdata = projection_df,type = "response",na.action = na.pass))}

    if(length(sam.weight)==1){SAMpred<-matrixStats::rowWeightedMeans(projections_blocks, w = rep(sam.weight,length(sam.mod)),na.rm=T)}
    if(length(sam.weight)>1){SAMpred<-matrixStats::rowWeightedMeans(projections_blocks, w = sam.weight,na.rm=T)}}}


  ## Stacked
  if(!missing(sdm.mod) && !missing(sam.mod)){stacked<-SDMbinary*SAMpred}

   # Create rasters for each projection method requested by user
  if("binary" %in% projection.method){binaryrast<-raster::rasterFromXYZ(cbind(projection_df[,"x"],projection_df[,"y"],SDMbinary),crs= "+proj=longlat")}
    if("abundance" %in% projection.method){abundancerast<-raster::rasterFromXYZ(cbind(projection_df[,"x"],projection_df[,"y"],SAMpred),crs= "+proj=longlat")}
      if("proportional" %in% projection.method){proportionalrast<-raster::rasterFromXYZ(cbind(projection_df[,"x"],projection_df[,"y"],SDMpred),crs= "+proj=longlat")}
        if("stacked" %in% projection.method){stackedrast<-raster::rasterFromXYZ(cbind(projection_df[,"x"],projection_df[,"y"],stacked),crs= "+proj=longlat")}

    ## Save projection rasters to local directory
    if(!missing(save.directory)){if(!dir.exists(save.directory)){stop("save.directory does not exist")}#Check save.directory exists if provided
      if(exists("binaryrast")){raster::writeRaster(binaryrast,file=paste0(save.directory,"/",date,"_binary.tif"),overwrite=T,crs= "+proj=longlat")}
      if(exists("abundancerast")){raster::writeRaster(abundancerast,file=paste0(save.directory,"/",date,"_abundance.tif"),overwrite=T)}
      if(exists("proportionalrast")){raster::writeRaster(proportionalrast,file=paste0(save.directory,"/",date,"_proportional.tif"),overwrite=T)}
      if(exists("stackedrast")){raster::writeRaster(stackedrast,file=paste0(save.directory,"/",date,"_stacked.tif"),overwrite=T)}}

   # Save projection rasters to Google Drive folder
    if(!missing(save.drive.folder)){
      # Check user email provided
      if (missing(user.email)){stop("user.email is missing. Please provide user email linked to Google Drive account")}
      #Initiate Google Drive
      googledrive::drive_auth(email=user.email)
      googledrive::drive_user()
      save.folderpath<-googledrive::drive_find(pattern=paste0(save.drive.folder), type='folder')
      if(nrow(save.folderpath)==0){stop("save.drive.folder provided does not exist.")} #Check save Google Drive folder exists if provided

      filename<-paste0(tempfile(),".tif")# Temporary file name to save each raster to then upload to Google Drive

      if(exists("binaryrast")){raster::writeRaster(binaryrast,filename,overwrite=T)
          googledrive::drive_upload(media=filename,path=googledrive::as_id(save.folderpath$id),name=paste0(date,"_binary.tif"),overwrite=T)}

      if(exists("abundancerast")){raster::writeRaster(abundancerast,filename,overwrite=T)
          googledrive::drive_upload(media=filename,path=googledrive::as_id(save.folderpath$id),name=paste0(date,"_abundance.tif"),overwrite=T)}

      if(exists("proportionalrast")){raster::writeRaster(proportionalrast,filename,overwrite=T)
          googledrive::drive_upload(media=filename,path=googledrive::as_id(save.folderpath$id),name=paste0(date,"_proportional.tif"),overwrite=T)}

      if(exists("stackedrast")){raster::writeRaster(stackedrast,filename,overwrite=T)
          googledrive::drive_upload(media=filename,path=googledrive::as_id(save.folderpath$id),name=paste0(date,"_stacked.tif"),overwrite=T)}}}}




