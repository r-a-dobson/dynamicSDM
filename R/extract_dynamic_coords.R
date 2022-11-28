#' Extract temporally dynamic explanatory variable data for occurrence records.
#'
#' For each species occurrence record co-ordinate and date, temporally dynamic explanatory data are extracted using Google Earth engine
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".
#' @param datasetname a character string, the Google Earth Engine dataset to extract data from.
#' @param bandname a character string, the Google Earth Engine dataset bandname to extract data for.
#' @param spatial.res.metres a numeric value, the spatial resolution in metres for data extraction.
#' @param GEE.math.fun a character string, the mathematical function to compute across the temporal.res period for each record.
#' @param save.method a character string, the method used to save extracted variable data. One of '"split"' or '"combined"': can be abbreviated. See details.
#' @param temporal.res a numeric value, the temporal resolution in days to extract data and calculate GEE.math.fun across from record date.
#' @param temporal.direction a character string, the temporal direction for extracting data across relative to the record date. One of '"prior"' or '"post"': can be abbreviated.
#' @param varname optional; a character string, the unique name for the explanatory variable. Default varname is “bandname_temporal.res_temporal.direction_ GEE.math.fun".
#' @param save.directory a character string, the path to a local directory to save extracted variable data to.
#' @details
#''For each individual species occurrence record co-ordinate and date, this function extracts data for a given band within a Google Earth Engine dataset across a user-specified period and calculates a mathematical function on such data.
#’
#’ extract_dynamic_coords requires users to have installed R package "rgee" (Aybar et al., 2020) and initialised Google Earth Engine with valid log-in credentials. Please follow instructions on the following website https://cran.r-project.org/web/packages/rgee/vignettes/rgee01.html. datasetname must be in the accepted Google Earth Engine Data catalogue layout (e.g. “MODIS/006/MCD12Q1” or “UCSB-CHG/CHIRPS/DAILY”) and bandname as specified in the dataset (e.g. “LC_Type5”, “precipitation”). For datasets and band names, see https://developers.google.com/earth-engine/datasets.
#'
#' GEE.math.fun specifies the mathematical function to be calculated over the temporal period. Options are limited to Google Earth Engine ImageCollection Reducer functions (https://developers.google.com/earth-engine/apidocs/) for which an analogous R function is available. This includes: "allNonZero","anyNonZero", "count", "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product", "sampleStdDev", "sampleVariance", "stdDev", "sum" and "variance".
#'
#' For save.method '"combined"', the function with save “.csv” files containing all occurrence records and associated values for the explanatory variable. If save.method '"split"' is chosen, the function will save individual “.csv” files for each record with assigned unique ID value in file name. '"split"' method is provided to protect users if internet connection is lost when extracting data for large occurrence datasets.
#' @references
#'Aybar, C., Wu, Q., Bautista, L., Yali, R. and Barja, A., 2020. rgee: An R package for interacting with Google Earth Engine. Journal of Open Source Software, 5(51), p.2272.
#'@return Returns details of successful explanatory variable extractions.
#'@export

extract_dynamic_coords<-function(occ.data,datasetname,bandname,spatial.res.metres,GEE.math.fun,save.method,temporal.res,temporal.direction,varname,save.directory){

  #Set default varname
  if (missing(varname)){message(paste0("varname is missing. Default varname set as: ",bandname,"_",temporal.res,"_",temporal.direction,"_",GEE.math.fun))
    varname<-paste0(bandname,"_",temporal.res,"_",temporal.direction,"_",GEE.math.fun)}

  # Initial argument formatting checks
  if (!length(GEE.math.fun)==1){stop("Only provide one GEE.math.fun")}
  if (!dir.exists(save.directory)){stop("save.directory does not exist")}
  if (!class(occ.data)=="data.frame"){stop("occurrence must be of class data.frame")}
  if (!class(datasetname)=="character"){stop("datasetname must be of class character")}
  if (!class(bandname)=="character"){stop("bandname must be of class character")}
  if (!class(varname)=="character"){stop("varname must be of class character")}
  if (!class(spatial.res.metres)=="numeric"){stop("spatial.res.metres must be of class numeric")}
  if (!class(temporal.res)=="numeric"){stop("temporal.res must be of class numeric")}
  if (!class(temporal.direction)=="character"){stop("temporal.direction must be of class numeric")}


  #Match choices, ensure argument options are valid
  save.method<- match.arg(arg = save.method, choices = c("split", "combined"))
  temporal.direction<-  match.arg(arg = temporal.direction, choices = c("prior", "post"))


  namelist<-c("allNonZero","anyNonZero", "count", "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product", "sampleStdDev", "sampleVariance",
              "stdDev", "sum", "variance") # These are the names to match the GEE Reducer functions, which the user will specify in argument GEE.math.fun

  GEE.math.fun<-match.arg(arg = GEE.math.fun, choices = namelist) # Match GEE.math.fun to Google Earth Engine ImageCollection Reducer functions

  R.FUNC.LIST<-list(allNonZero,anyNonZero,count,First,firstNonNull,last,lastNonNull,max,mean,stats::median,
                    min,mode,prod,sd,var,stdDev,sum,variance)

  GEE.math.fun<-R.FUNC.LIST[[match(GEE.math.fun,namelist)]]# Match GEE.math.fun argument to analogous R function

  ##Import python module
  ee <- reticulate::import("ee")

  ### Check Google Earth Engine and initialise
  rgee::ee_check("rgee")
  rgee::ee_Initialize()




  if(save.method=="split"){  ## This method saves extracted explanatory variable data for each record in individual .csv files. Protection against internet connection loss.

  rowscomplete=NULL # Keep note of rows completed

  occ.data$unique.ID.DYN<-rep(1:nrow(occ.data)) # Assign each record a unique number for naming individual record .csv files when saving

  for (x in 1:nrow(occ.data)){

    date1<-as.Date(with(occ.data[x,], paste(year, month, day,sep="-")), "%Y-%m-%d")

    if(temporal.direction=="prior"){

      date2<-as.character(date1-temporal.res) # Calculate the date the set number of days (temporal.res set by user) prior to the occurrence record date
      date1<-as.character(date1)

      ## This function accesses Google Earth Engine and compiles an ImageCollection object for the specified dataset and band between the prior date and record date
      image_collection <- ee$ImageCollection(paste0(datasetname)) %>%
        ee$ImageCollection$filterDate(date2,date1) %>%
        ee$ImageCollection$map(
          function(x) {
            date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
            name <- ee$String$cat(paste0(bandname), date)
            x$select(paste0(bandname))$rename(name)})}

    if(temporal.direction=="post"){

      date2<-as.character(date1+temporal.res) # Calculate the date the set number of days (temporal.res set by user) post the occurrence record date
      date1<-as.character(date1)

      ## This function accesses Google Earth Engine and compiles an ImageCollection object for the specified dataset and band between the record date and post date.
      image_collection <- ee$ImageCollection(paste0(datasetname)) %>%
        ee$ImageCollection$filterDate(date1,date2) %>%
        ee$ImageCollection$map(
          function(x) {
            date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
            name <-ee$String$cat(paste0(bandname), date)
            x$select(paste0(bandname))$rename(name)})}


    coords <- rgee::ee$Geometry$Point(c(as.numeric(occ.data[x,"x"]),as.numeric(occ.data[x,"y"]))) ## Set co-ordinate of the occurrence record as a GEE Geometry object

    extracted_data<-NULL
    tryCatch({extracted_data <- rgee::ee_extract( ## This function extracts every value from the ImageCollection at the record co-ordinate.
      x = image_collection,
      y = coords,
      scale = spatial.res.metres)},
    error=function(e){cat("NA values returned - rgee 'error': ",conditionMessage(e),"\n")}) # This blocks the GEE function causing the function to error when an NA value is returned for one occurrence record. This information may be valuable to the user and does not mean an error functionally has occurred. For instance, remote sensing data may simply not cover this record's co-ordinate or date.


    if(!is.null(extracted_data)){

      extracted_data<-na.omit(t(extracted_data)) # NA's are removed before the GEE.math.fun is calculated.

          if(nrow(extracted_data)>0){
            value<-as.data.frame(apply(extracted_data,2,GEE.math.fun)) #Calculate user specified GEE.math.fun on extracted data.
            colnames(value)<-varname
            extracted_data<-as.data.frame(cbind(occ.data[x,],value))}}


    if(is.null(extracted_data)==T){ # If no data were able to be extracted, an NA value is returned for this record.
      value<-as.data.frame(NA)
      colnames(value)<-varname
      extracted_data<-as.data.frame(cbind(occ.data[x,],value))}


  write.csv(extracted_data,file=paste0(save.directory,"/",occ.data[x,"unique.ID.DYN"],"_",varname,"_.csv")) # Write individual .csv for occurrence record data with explanatory variable data column
  print(paste0("Record number: ",x," saved to ",save.directory,"/",occ.data[x,"unique.ID.DYN"],"_",varname,"_.csv"))
  rowscomplete<-c(rowscomplete,x)} # Note that this row has been completed

  print("Data successfully extracted for:")
  return(rowscomplete)} # Inform users which rows data were successfully extracted and saved for.




if(save.method=="combined"){ ## This method creates a data frame of all occurrence records and extracted explanatory variable data, and saves this as .csv file once all have been iterated through.

    occ.data$unique.ID.DYN<-rep(1:nrow(occ.data))# Assign each record a unique number for naming individual record .csv files when saving

    combined_data_set=NULL

    for (x in 1:nrow(occ.data)){

      date1<-as.Date(with(occ.data[x,], paste(year, month, day,sep="-")), "%Y-%m-%d")

      if(temporal.direction=="prior"){
        date2<-as.character(date1-temporal.res)  # Calculate the date the set number of days (temporal.res set by user) prior to the occurrence record date
        date1<-as.character(date1)

      ## This function accesses Google Earth Engine and compiles an ImageCollection object for the specified dataset and band between the prior date and record date
        image_collection <- ee$ImageCollection(paste0(datasetname)) %>%
          ee$ImageCollection$filterDate(date2,date1) %>%
          ee$ImageCollection$map(
            function(x) {
              date <- rgee::ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
              name <- rgee::ee$String$cat(paste0(bandname), date)
              x$select(paste0(bandname))$rename(name)})}


      if(temporal.direction=="post"){
        date2<-as.character(date1+temporal.res)# Calculate the date the set number of days (temporal.res set by user) post the occurrence record date
        date1<-as.character(date1)

        ## This function accesses Google Earth Engine and compiles an ImageCollection object for the specified dataset and band between the record date and post date
        image_collection <- ee$ImageCollection(paste0(datasetname)) %>%
          ee$ImageCollection$filterDate(date1,date2) %>%
          ee$ImageCollection$map(
            function(x) {
              date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
              name <- ee$String$cat(paste0(bandname), date)
              x$select(paste0(bandname))$rename(name)})}


      coords <- ee$Geometry$Point(c(as.numeric(occ.data[x,"x"]),as.numeric(occ.data[x,"y"])))  ## Set co-ordinate of the occurrence record as a GEE Geometry object

      extracted_data<-NULL
      tryCatch({extracted_data <- rgee::ee_extract(  ## This function extracts every value from the ImageCollection at the record co-ordinate.
        x = image_collection,
        y = coords,
        scale = spatial.res.metres)},
      error=function(e){cat("NA values returned - rgee error message: ",conditionMessage(e),"\n")}) # This blocks the GEE function causing the function to error when an NA value is returned for one occurrence record. This information may be valuable to the user and does not mean an error functionally has occurred. For instance, remote sensing data may simply not cover this record's co-ordinate or date.


      if(!is.null(extracted_data)){

        extracted_data<-na.omit(t(extracted_data))# NA's are removed before the GEE.math.fun is calculated.

        if(nrow(extracted_data)>0){
          value<-as.data.frame(apply(extracted_data,2,GEE.math.fun))#Calculate user specified GEE.math.fun on extracted data.
          colnames(value)<-varname
          extracted_data<-as.data.frame(cbind(occ.data[x,],value))}}

        if(is.null(extracted_data) || ncol(extracted_data)==0){ # If no data were able to be extracted, an NA value is returned for this record.
          value<-as.data.frame(NA)
          colnames(value)<-varname
          extracted_data<-as.data.frame(cbind(occ.data[x,],value))}

      combined_data_set<-as.data.frame(rbind(combined_data_set,extracted_data))} #Bind extracted data to occurrence record data frame

      write.csv(combined_data_set,file=paste0(save.directory,"/all_records_combined_",varname,"_.csv")) # Save combined data frame to save.directory
      print(paste0("Data successfully extracted and saved to ",save.directory,"/all_records_combined",varname,"_.csv"))
      return(combined_data_set)}}
