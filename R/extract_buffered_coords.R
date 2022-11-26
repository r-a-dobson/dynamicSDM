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
#' @examples
#' Sum total MODIS Annual Land grass (6) and cereal cropland (7) cells across the nine surrounding cells and the one including the record's co-ordinate for the year specific to the record’s date.
#'extract_buffered_coords(occ.data=occ.data,
#'                        datasetname="MODIS/006/MCD12Q1",
#'                        bandname="LC_Type5",
#'                        spatial.res.metres=500,
#'                        GEE.math.fun="sum",
#'                        moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
#'                        extraction.drive.folder= "drivefoldername",
#'                        user.email="myusernameatgmail.com",
#'                        save.method="combined",
#'                        temporal.level="year",
#'                        categories=c(6,7),
#'                        save.directory="C:/path/to/folder")
#'
#' Extract mean MODIS Terra Land Surface Temperature across the seven days prior to the occurrence record’s date and then average this across the nine surrounding cells and the one including the record's co-ordinate.
#'extract_buffered_coords(occ.data=occ.data,
#'                        datasetname="MODIS/006/MOD11A1",
#'                        bandname="LST_Day_1km",
#'                        spatial.res.metres=111320,
#'                        GEE.math.fun="mean",
#'                        moving.window.matrix=matrix(1/9,nrow=3,ncol=3),
#'                        extraction.drive.folder="drivefoldername",
#'                        user.email="myusernameatgmail.com",
#'                        temporal.level="day",
#'                        save.method="split",
#'                        temporal.res=7,
#'                        temporal.direction="prior",
#'                        GEE.math.fun="mean",
#'                        save.directory=tempdir())

extract_buffered_coords<-function(occ.data,datasetname,bandname,spatial.res.metres, GEE.math.fun,moving.window.matrix,extraction.drive.folder,user.email,save.method,varname=NULL,temporal.res=NULL,temporal.level=NULL,temporal.direction=NULL,categories=NULL,save.directory=NULL){

  if(missing(extraction.drive.folder)){stop("No extraction.drive.folder specified. Please create and provide the name of folder within your Google Drive")}

  # Check user email provided
  if (missing(user.email)){stop("user.email is missing. Please provide user email linked to Google Drive account")}

  ## Check formatting of arguments to avoid common errors
  if (!length(GEE.math.fun)==1){stop("Only provide one GEE.math.fun")}
  if(!dir.exists(save.directory)){stop("save.directory does not exist")}
  save.method<-match.arg(arg = save.method, choices = c("split", "combined"))
  temporal.level<-match.arg(arg = temporal.level, choices = c("day", "month","year")) # Check arguments match available options

  if (!missing(temporal.res)){if (missing(temporal.direction)){ stop("temporal.res specfied but temporal.direction is missing. Please specifiy if the temporal.res period is to be prior or post the occurrence record date")}
      temporal.direction<-match.arg(arg = temporal.direction, choices = c("prior", "post"))}

  if (missing(temporal.res)){message("temporal.res missing, assuming temporally static variable.")}

  #Set default varname - depending on whether spatial and temporal or spatial only.
  if (missing(varname)){
    if(!missing(temporal.res)){
      message(paste0("varname is missing. Default varname set as: ",bandname,"_",temporal.res,"_",temporal.direction,"_",GEE.math.fun,"_buffered"))
      varname<-paste0(bandname,"_",temporal.res,"_",temporal.direction,"_",GEE.math.fun,"_buffered")}

    if(missing(temporal.res)){
      message(paste0("varname is missing. Default varname set as: ",bandname,"_",GEE.math.fun,"_buffered"))
      varname<-paste0(bandname,"_",GEE.math.fun,"_buffered")}}

  ## Load Google Drive to download rasters from GEE to
  ee <- reticulate::import("ee")
  googledrive::drive_auth(email=user.email)
  googledrive::drive_user()

  ## Load Google Earth Engine
  rgee::ee_check("rgee")
  rgee::ee_Initialize(drive=T)

  # Warning for categorical datasets where categories have not been specifed using argument categories
  if(missing(categories)){message("No categories specified. If data are categorical all categories will be included in calculation.")}

  occ.data$unique.ID.DYN<-rep(1:nrow(occ.data)) # Assign each occurrence record a unique ID for saving files.


  ### Split occurrence by temporal.level - if annual resolution only matters then can use raster data from that year all at once, instead of redownloading it for each individual record in that year

  if(temporal.level=="day"){
    uniqueocc<-unique(occ.data[,c("day","month","year")])}

  if(temporal.level=="month"){
    uniqueocc<-unique(occ.data[,c("month","year")])}

  if(temporal.level=="year"){
    uniqueocc<-data.frame(unique(occ.data[,c("year")]))
    colnames(uniqueocc)<-"year"}

  #### for each uniqueocc level, need to download the appropriate raster from GEE, cropped to minimum extent needed to cover all occurrence co-ordinates in this level.

  combined_data_set=NULL
  rowscomplete=NULL

  for (x in 1:nrow(uniqueocc)){

  if(temporal.level=="year"){
      year<-as.numeric(uniqueocc[x,"year"])
      month<-1# Set arbitrary values for month and day as need complete date for GEE but only the year actually matters here.
      day<-1
      occforperiod<-occ.data[occ.data$year==year,]
      nameofsplitfile<-year}

  if(temporal.level=="month"){
      year<-as.numeric(uniqueocc[x,"year"])
      month<-as.numeric(uniqueocc[x,"month"]) # Add leading zero for date format
      day<- 1 # Set arbitrary value for day as need complete date for GEE but only the year and month actually matters here.
      occforperiod<-occ.data[occ.data$year==year,]
      occforperiod<-occforperiod[occforperiod$month==month,]
      nameofsplitfile<-paste0(year,"-", sprintf("%02d",month))}

  if(temporal.level=="day"){
      year<-as.numeric(uniqueocc[x,"year"])
      month<-as.numeric(uniqueocc[x,"month"])
      day<-as.numeric(uniqueocc[x,"day"]) # Use full dates from each individual occurrence record as each will have different data.
      occforperiod<-occ.data[occ.data$year==year,]
      occforperiod<-occforperiod[occforperiod$month==month,]
      occforperiod<-occforperiod[occforperiod$day==day,]
      nameofsplitfile<-paste0(year,"-",sprintf("%02d",month),"-",sprintf("%02d",day))}

    date1<-as.Date(paste(year, month,day,sep="-"), "%Y-%m-%d")


  ## Next step is to work out minimum area of environmental data to download for extraction of moving window matrix around each occurrence record (to minimise computing time)

  # Using environmental data resolution (spatial.res.metres) and cell length of matrix (nrow(moving.window.matrix)) calculate radius (divide circumference by 2) from occurrence co-ordinate to edge of matrix.
  spatial.buffer<-nrow(moving.window.matrix)*spatial.res.metres/2

  ## The matrix is square not circular, so then need to work out the radius to furthers corner of matrix away from co-ordinate, using pythagorus theorem (a2 + b2 = c2), so c = square root of a2 + b2 (the radius from previous calculation)

  spatial.buffer<- sqrt((spatial.buffer^2)+(spatial.buffer^2)) # Matrix is square not circular so work out radius to furthest corner away from co-ordinate using pythagorus theorem

  ## To ensure that all cells are definitely included round to nearest 5
  spatial.buffer<-ceiling(spatial.buffer/5)*5

  # From each occurrence record point in this loop, add the calculated radius in metres using the geobuffer package
  spatial.buffer<-geobuffer::geobuffer_pts(xy = data.frame(lon = c(occforperiod[,"x"]),lat = c(occforperiod[,"y"])),dist_m = spatial.buffer,step_dg = 60,output = "sp") # Add this radius to all occurrence record co-ordinates that are being extracted in this loop

  # Extract min and max longtiude and latitude co-ordinates, this is the minimum possible area to extract from environmental dataset
  xmin<-sp::bbox(raster::extent(spatial.buffer))[1,1]
  xmax<-sp::bbox(raster::extent(spatial.buffer))[1,2]
  ymin<-sp::bbox(raster::extent(spatial.buffer))[2,1]
  ymax<-sp::bbox(raster::extent(spatial.buffer))[2,2]

  geometry <- ee$Geometry$Polygon(list(c(xmin ,  ymin ),c(xmin , ymax),c(xmax, ymax),c(xmax,  ymin))) # Create GEE Geometry Polygon using min and max co-ordinates required for this loop

  ## Static variable
  if(missing(temporal.res)){

    date1<-as.character(date1)

    image_collection <- ee$ImageCollection(paste0(datasetname))$  ## Google Earth Engine function to create ImageCollection for specified dataset, band and date for this loop.
      filterDate(date1)$
      select(paste0(bandname))

    image_collection_reduced <- image_collection$reduce(ee$Reducer$first())} # As this is static, there should only be one Image in the ImageCollection, so we reduce it to select the first Image.


  ## Temporally dynamic variable
  if(!missing(temporal.res)){

      firstdate<-date1

      if(temporal.direction=="prior"){
        seconddate<-as.character(date1-temporal.res) # Prior selected so minus temporal.res days from the record date
        firstdate<-as.character(firstdate)

        image_collection <- ee$ImageCollection(paste0(datasetname))$
          filterDate(seconddate,firstdate)$    # Create ImageCollection of all Images of specified dataset and band between these two dates
          select(paste0(bandname))}

      if(temporal.direction=="post"){
        seconddate<-as.character(date1+temporal.res) # Post selected so minus temporal.res days from the record date
        firstdate<-as.character(firstdate)

        image_collection <- ee$ImageCollection(paste0(datasetname))$
          filterDate(firstdate,seconddate)$ # Create ImageCollection of all Images of specified dataset and band between these two dates
          select(paste0(bandname))}


      GEE.FUNC.LIST<-list(ee$Reducer$allNonZero(), ee$Reducer$anyNonZero(), ee$Reducer$count(), ee$Reducer$first(),ee$Reducer$firstNonNull(),
                          ee$Reducer$last(), ee$Reducer$lastNonNull(), ee$Reducer$max(),ee$Reducer$mean(), ee$Reducer$median(),
                          ee$Reducer$min(), ee$Reducer$mode(), ee$Reducer$product(), ee$Reducer$sampleStdDev(), ee$Reducer$sampleVariance(),
                          ee$Reducer$stdDev(), ee$Reducer$sum(), ee$Reducer$variance()) ## This is a list of all GEE ImageCollection Reducer functions available

      namelist<-c("allNonZero","anyNonZero", "count", "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product", "sampleStdDev", "sampleVariance",
                  "stdDev", "sum", "variance") # These are the names to match the GEE Reducer functions, which the user will specify in argument GEE.math.fun

      GEE.math.fun<-match.arg(arg = GEE.math.fun, choices = namelist) # Match named function to actual function for use. Error if no match found.


      image_collection_reduced <- image_collection$reduce( GEE.FUNC.LIST[[match(GEE.math.fun,namelist)]])} # Reduce ImageCollection using function specified in GEE.math.fun

  # Download raster of Reduced ImageCollection to users Google Drive folder
    tryCatch({raster<-rgee::ee_as_raster(
      image = image_collection_reduced,
      container=extraction.drive.folder,
      scale=spatial.res.metres,
      dsn=paste0(varname,"_",date1),
      region = geometry,
      timePrefix=FALSE,
      via = "drive")},error=function(e){cat("ERROR :",conditionMessage(e),"\n")})

    pathforthisfile<-paste0(tempfile(),".tif")

    googledrive::drive_auth(email=user.email) # Authenticate Google Drive
    googledrive::drive_user() # Check Google Drive user information

    googledrive::drive_download(paste0(varname,"_",date1,".tif"),path=pathforthisfile,overwrite=T) # Download raster from Google Drive to temp directory for processing
    raster<-raster::raster(pathforthisfile) # Read in the raster from the temp directory

    # Match GEE.math.fun argument to analogous R function
    R.FUNC.LIST<-list(allNonZero,anyNonZero,count,First,firstNonNull,last,lastNonNull,max,mean,stats::median,
                      min,mode,prod,sd,var,stdDev,sum,variance)

    namelist<-c("allNonZero","anyNonZero", "count", "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product", "sampleStdDev", "sampleVariance",
                "stdDev", "sum", "variance") # These are the names to match the GEE Reducer functions, which the user will specify in argument GEE.math.fun

    GEE.math.fun<-match.arg(arg = GEE.math.fun, choices = namelist) # Match named function to actual function for use. Error if no match found.

    math.fun<-R.FUNC.LIST[[match(GEE.math.fun,namelist)]]# Match GEE.math.fun argument to analogous R function


  ## Process categorical data raster
  if(!missing(categories)){
  #### Convert the categorical raster into binary 1 (categories specified by user) and 0 (any other category)
  rast<-raster==categories[1]
  if(length(categories)>1){for(cat in 2:length(categories)){rast<-rast+raster==categories[cat]}} # If more than one category, iterate through each category and add binary rasters together
  moving.window.matrix[1:nrow(moving.window.matrix),1:ncol(moving.window.matrix)]<-1 ## If data are categorical then focal should be completed using moving.window.matrix with weights = 1 for each cell.
  focalraster<-raster::focal(rast,moving.window.matrix,FUN=math.fun,na.rm=T)} ## Calculate the math.fun function across moving.window.matrix for the raster with focal function in R.

  ## Process continuous data raster
  if(missing(categories)){focalraster<-raster::focal(raster,moving.window.matrix,FUN=math.fun)} ## Calculate the math.fun function across moving.window.matrix for the raster with focal function in R.

  # Extract value at co-ordinates of each occurrence record from focal raster
  extracted_data<-as.data.frame(raster::extract(focalraster,sp::SpatialPoints(cbind(occforperiod[,"x"],occforperiod[,"y"]))))
  colnames(extracted_data)<-varname

  if(save.method=="split"){write.csv(extracted_data,file=paste0(save.directory,"/",nameofsplitfile,"_",varname,"_.csv"),row.names=FALSE) # If split chosen, save individual .csv file with record and value
      print(paste0("Records for temporal.level: ",nameofsplitfile," saved to ",save.directory,"/",nameofsplitfile,"_",varname,"_.csv"))}

  if(save.method=="combined"){combined_data_set<-rbind(combined_data_set,extracted_data)} # If combined chosen, bind the value to the results data.frame

  #Record uniqueID of records explanatory data have now been extracted for in this loop
  rowscomplete<-c(rowscomplete, nameofsplitfile)

  ## Remove the raster used in this loop from Google Drive
  googledrive::drive_rm(paste0(varname,"_",date1,".tif"))}

  if(save.method=="split"){ ## Print names of occurrence records data successfully extracted for
  print("Data successfully extracted for:")
  return(sort(rowscomplete))}

  if(save.method=="combined"){ ## Save combined data.frame to given directory
  write.csv(combined_data_set,file=paste0(save.directory,"/all_records_combined_",varname,"_.csv"),row.names=FALSE)
  print(paste0("Data successfully extracted and saved to ",save.directory,"/all_records_combined_",varname,"_.csv"))
  return(combined_data_set)}}



