#' Extract spatially buffered and temporally dynamic rasters of explanatory variable data.
#'
#' Extract rasters for spatially buffered and temporally dynamic explanatory variables at each projection date using Google Earth Engine.
#' @param dates a character string, vector of dates in format YYYY-MM-DD.
#' @param spatial.ext the spatial extent for the extracted raster. Object of class "Extent", "RasterLayer" or "polygon" or numeric vector listing xmin, xmax, ymin and ymax in order.
#' @param datasetname a character string, the Google Earth Engine dataset to extract data from.
#' @param bandname a character string, the Google Earth Engine dataset bandname to extract data for.
#' @param spatial.res.metres a numeric value, specifying the spatial resolution in metres of the raster to be extracted.
#' @param GEE.math.fun a character string, the mathematical function to compute across the specified period and spatial buffer from each projection date and cell.
#' @param moving.window.matrix “moving window” matrix of weights with an odd number of sides to specify spatial neighbourhood of cells to calculate GEE.math.fun across for each cell in spatial.ext. See details for more information.
#' @param user.email a character string, user email for initialising Google Drive.
#' @param varname optional; a character string, the unique name for the explanatory variable. Default varname is “bandname_temporal.res_temporal.direction_ GEE.math.fun_buffered_raster".
#' @param temporal.res optional; a numeric value, the temporal resolution in days prior or post each projection date to calculate GEE.math.fun across.
#' @param temporal.direction optional; a character string, the temporal direction for extracting dynamic variable data across relative to each projection date given. One of '"prior"' or '"post"': can be abbreviated.
#' @param categories optional; a character string, the categories to use in the calculation if data are categorical. See details for more information.
#' @param save.directory optional; a character string, path to local directory to save extracted rasters to.
#' @param save.drive.folder a character string, Google Drive folder to save extracted rasters to.
#' @details
#' For each projection date, this function downloads rasters at given spatial extent and resolution for spatially buffered and temporally dynamic explanatory variables. Rasters are saved directly to Google Drive, with option to export to local directory too. These can be combined to create projection covariate data frames for projection dynamic species distribution and abundance at high spatiotemporal resolution
#'
#' If temporal.res and temporal.direction are not given, the function extracts explanatory variable data (in RasterLayer format) for all of the cells in spatial extent (spatial.ext) given. If temporal.res and temporal.direction is given, the function extracts explanatory variable data (in RasterLayer format) for which GEE.math.fun has been first calculated over this period in relation to the projection date.
#'
#' Using the focal function in raster R package (Hijmans et al., 2015), GEE.math.fun is calculated across the spatial buffer area from each cell across spatial extent given. The spatial buffer area used is specified by argument moving.window matrix, which dictates the neighbourhood of cells surrounding each cell in spatial.ext to include in the calculation.
#'
#' When explanatory variable data are categorical (e.g. land cover classes), argument categories can be used to specify the categories of importance to the calculation. The category or categories given will be converted in a binary representation, with “1” for those listed, and “0” for all others in the dataset. Ensure that the GEE.math.fun given is appropriate for such data. For example, this function could return the sum of suitable land cover classified cells in the “moving window” from each cell across spatial extent given.
#'
#' extract_buffered_raster requires users to have installed R package "rgee" (Aybar et al., 2020) and initialised Google Earth Engine with valid log-in credentials. Please follow instructions on the following website . datasetname must be in the accepted Google Earth Engine Data catalogue layout (e.g. “MODIS/006/MCD12Q1” or “UCSB-CHG/CHIRPS/DAILY”) and bandname as specified in the dataset (e.g. “LC_Type5”, “precipitation”). For datasets and band names, see .
#'
#' extract_buffered_raster also requires users to have installed the R package "googledrive" (D'Agostino McGowan and Bryan, 2022) and initialised Google Drive with valid log-in credentials, which must be provided under argument user.email. Please follow instructions on https://googledrive.tidyverse.org/ for initialising the googledrive package.
#'
#' GEE.math.fun specifies the mathematical function to be calculated over the spatial buffered area and temporal period. Options are limited to Google Earth Engine ImageCollection Reducer functions (https://developers.google.com/earth-engine/apidocs/) for which an analogous R function is available. This includes: "allNonZero","anyNonZero", "count", "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product", "sampleStdDev", "sampleVariance", "stdDev", "sum" and "variance".
#' @references
#'Aybar, C., Wu, Q., Bautista, L., Yali, R. and Barja, A., 2020. rgee: An R package for interacting with Google Earth Engine. Journal of Open Source Software, 5(51), p.2272.
#'
#'D'Agostino McGowan L., and Bryan J., 2022. googledrive: An Interface to Google Drive. https://googledrive.tidyverse.org, https://github.com/tidyverse/googledrive.
#'
#'Hijmans, R. J., Van Etten, J., Cheng, J., Mattiuzzi, M., Sumner, M., Greenberg, J. A., Lamigueiro, O. P., Bevan, A., Racine, E. B. & Shortridge, A. 2015. Package ‘raster’. R package, 734.
#' @return Returns details of successful explanatory variable extractions for each projection date.

extract_buffered_raster<-function(dates,spatial.ext,datasetname,bandname,spatial.res.metres,GEE.math.fun,moving.window.matrix,user.email,varname=NULL,temporal.res=NULL,temporal.direction=NULL,categories=NULL,save.directory=NULL,save.drive.folder){

  # Check all arguments that are required have been given and are valid .
  if (missing(spatial.res.metres)){stop("spatial.res.metres is missing")}
  if (missing(save.drive.folder)){stop("save.drive.folder is missing")}
  if(!any(class(spatial.ext)==c("numeric","Extent","RasterLayer","Polygon"))){stop("spatial.ext must be of class numeric, Extent, RasterLayer or Polygon")}

  # Check user email provided
  if (missing(user.email)){stop("user.email is missing. Please provide user email linked to Google Drive account")}

  # Set default variable names
  # Temporally dynamic variable name
  if(!missing(temporal.res)){
    if (missing(varname)){message(paste0("varname is missing. Default varname set as: ",bandname,"_",temporal.res,"_",temporal.direction,"_",GEE.math.fun,"_buffered_raster"))
      varname<-paste0(bandname,"_",temporal.res,"_",temporal.direction,"_",GEE.math.fun,"_buffered_raster")}}
  # Temporally static variable name
  if (missing(temporal.res)){message("temporal.res missing. Explanatory variable assumed to be static.")
    message(paste0("varname is missing. Default varname set as: ",bandname,"_",GEE.math.fun,"_buffered_raster"))
    varname<-paste0(bandname,"_",GEE.math.fun,"_buffered_raster")}

  #Import python module
  ee <- reticulate::import("ee")

  #Initiate Google Drive
  googledrive::drive_auth(email=user.email)
  googledrive::drive_user()

  #Initiate Google Earth Engine
  rgee::ee_check("rgee")
  rgee::ee_Initialize(drive=T)

  completed.list<-NULL

  for (x in 1:length(dates)){

    date1<-as.character(as.Date(dates[x]))

    ### Numeric extent to co-ords
    if(class(spatial.ext)=="numeric" && !length(spatial.ext)==4){stop("spatial.ext numeric vector should be of length four c(xmin, xmax, ymin and ymax)")}

    xmin<-extract_xy_min_max(spatial.ext)[1]
    xmax<-extract_xy_min_max(spatial.ext)[2]
    ymin<-extract_xy_min_max(spatial.ext)[3]
    ymax<-extract_xy_min_max(spatial.ext)[4]


    # Create Google Earth Engine Geometry Polygon for extracting set spatial extent of raster
    geometry <- ee$Geometry$Polygon(list(c(xmin ,  ymin ),c(xmin , ymax),c(xmax, ymax),c(xmax,  ymin )))

    if(missing(temporal.res)){

      date1<-as.character(date1)

      image_collection <- ee$ImageCollection(paste0(datasetname))$ # Google Earth Engine function to create ImageCollection for specified dataset, band and date for this loop.
        filterDate(paste0(date1))$
        select(paste0(bandname))

      image_collection_reduced <- image_collection$reduce(ee$Reducer$first())} # As this is static, there should only be one Image in the ImageCollection, so we reduce it to select the first Image.


    if(!missing(temporal.res)){

      firstdate<-as.Date(dates[x])

      GEE.FUNC.LIST<-list(ee$Reducer$allNonZero(), ee$Reducer$anyNonZero(), ee$Reducer$count(), ee$Reducer$first(),ee$Reducer$firstNonNull(),
                          ee$Reducer$last(), ee$Reducer$lastNonNull(), ee$Reducer$max(),ee$Reducer$mean(), ee$Reducer$median(),
                          ee$Reducer$min(), ee$Reducer$mode(), ee$Reducer$product(), ee$Reducer$sampleStdDev(), ee$Reducer$sampleVariance(),
                          ee$Reducer$stdDev(), ee$Reducer$sum(), ee$Reducer$variance()) ## This is a list of all GEE ImageCollection Reducer functions available

      namelist<-c("allNonZero","anyNonZero", "count", "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product", "sampleStdDev", "sampleVariance",
                  "stdDev", "sum", "variance") # These are the names to match the GEE Reducer functions, which the user will specify in argument GEE.math.fun

      GEE.math.fun<-match.arg(arg = GEE.math.fun, choices = namelist)  # Match named function to actual function for use. Error if no match found.

      if(temporal.direction=="prior"){
        seconddate<-as.character(firstdate-temporal.res)# Prior selected so minus temporal.res days from the record date
        firstdate<-as.character(firstdate)

        image_collection <- ee$ImageCollection(paste0(datasetname))$
          filterDate(seconddate,firstdate)$ # Create ImageCollection of all Images of specified dataset and band between these two dates
          select(paste0(bandname))}


      if(temporal.direction=="post"){
        seconddate<-as.character(firstdate+temporal.res)# Post selected so minus temporal.res days from the record date
        firstdate<-as.character(firstdate)

        image_collection <- ee$ImageCollection(paste0(datasetname))$
          filterDate(firstdate,seconddate)$ # Create ImageCollection of all Images of specified dataset and band between these two dates
          select(paste0(bandname))}

      image_collection_reduced <- image_collection$reduce(GEE.FUNC.LIST[[match(GEE.math.fun,namelist)]])} # Reduce ImageCollection using function specified in GEE.math.fun

    # Download raster of Reduced ImageCollection to users Google Drive folder
    tryCatch({raster<-rgee::ee_as_raster(
      image = image_collection_reduced,
      container=save.drive.folder,
      scale=spatial.res.metres,
      dsn=paste0(varname,"_",date1,"_unprocessed"), # Name unprocessed to identify which to delete later. This raster has not been spatial buffered so cannot be used in projections yet.
      region = geometry,
      timePrefix=FALSE,
      via = "drive")},error=function(e){cat("ERROR :",conditionMessage(e),"\n")})

    googledrive::drive_auth(email=user.email) # Authenticate Google Drive
    googledrive::drive_user()  # Check Google Drive user information

    pathforthisfile<-paste0(tempfile(),".tif") # Create temp file name for unprocessed (not spatially buffered) raster
    googledrive::drive_download(paste0(varname,"_",date1,"_unprocessed.tif"),path=pathforthisfile,overwrite=T)  # Download unprocessed (not spatially buffered) raster to temporary directory
    raster<-raster::raster(pathforthisfile) # Read unprocessed (not spatially buffered) raster in for processing

    googledrive::drive_rm(paste0(varname,"_",date1,"_unprocessed.tif")) # Delete unprocessed (not spatially buffered) raster from Google Drive.


    # Match GEE.math.fun argument to analogous R function
    R.FUNC.LIST<-list(allNonZero,anyNonZero,count,First,firstNonNull,last,lastNonNull,max,mean,stats::median,
                      min,mode,prod,sd,var,stdDev,sum,variance)

    namelist<-c("allNonZero","anyNonZero", "count", "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product", "sampleStdDev", "sampleVariance",
                "stdDev", "sum", "variance") # These are the names to match the GEE Reducer functions, which the user will specify in argument GEE.math.fun

    GEE.math.fun<-match.arg(arg = GEE.math.fun, choices = namelist)  # Match named function to actual function for use. Error if no match found.

    math.fun<-R.FUNC.LIST[[match(GEE.math.fun,namelist)]]# Match GEE.math.fun argument to analogous R function


    ## Process a categorical data raster
    if(!missing(categories)){
      rast<-raster==categories[1]
      if(length(categories)>1){for(cat in 2:length(categories)){rast<-rast+raster==categories[cat]}} # If more than one category, iterate through each category and add binary rasters together
      moving.window.matrix[1:nrow(moving.window.matrix),1:ncol(moving.window.matrix)]<-1 ## If data are categorical then focal should be completed using moving.window.matrix with weights = 1 for each cell.
      focalraster<-raster::focal(rast,moving.window.matrix,FUN=math.fun,na.rm=T)} ## Calculate the math.fun function across moving.window.matrix for the raster with focal function in R.

    ## Process a continuous data raster
    if(missing(categories)){focalraster<-raster::focal(raster,moving.window.matrix,FUN=math.fun)} ## Calculate the math.fun function across moving.window.matrix for the raster with focal function in R.

    if(!missing(save.directory)){pathforthisfile<-save.directory}

    raster::writeRaster(focalraster,pathforthisfile,overwrite=T) # Write spatially buffered raster to temporary directory or save directory if specified

    # Find folder path of save.drive.folder and upload the processed raster to this folder
    folderpath<-  googledrive::drive_find(pattern=paste0(save.drive.folder), type='folder')
    googledrive::drive_upload(media=pathforthisfile,path=googledrive::as_id(folderpath$id),name=paste0(varname,"_",date1,".tif"),overwrite=T)

    completed.list<-rbind(completed.list,paste0(varname,"_",date1))} # Record that this has been successfully downloaded and iterate onto next date

  if(missing(save.directory)){print(paste0("Data successfully extracted and saved to Google Drive folder:",save.drive.folder))}
  if(!missing(save.directory)){print(paste0("Data successfully extracted and saved to Google Drive folder:",save.drive.folder," and local directory:",save.directory))}

  return(completed.list)}

