#' Combine extracted explanatory variable data for occurrence records into single data frame.
#'
#' Combines the split output files from functions extract_dynamic_coords and extract_buffered_coords into single data frame containing all occurrence records and explanatory variables.
#' @param varnames a character string, the unique names for each explanatory variable.
#' @param local.directory a character string or vector, the path to local directory or directories to read extracted explanatory data frames from.
#' @details
#'When extract_dynamic_coords and extract_buffered_coords have been used to extract dynamic explanatory variables for occurrence records, output for individual records and each variable will be split into separate “.csv” files.
#'
#'This function reads in these files and combined data into a single data frame containing each occurrence records and associated explanatory data from each variable.
#'
#'To prevent error, the “.csv” files must be uniquely named within the folder and include an exact character match for the varnames provided. All “.csv” files matching the varnames should have the same number and names of columns, which the default output of extract_dynamic_coords and extract_buffered_coords will if given the same occ.data data frame.
#' @return Returns a data frame containing all occurrence records with associated explanatory variable data.
#' @example
#'extract_coords_combine(local.directory=c("path/to/folder1","path/to/folder2"),
#'                       varnames=c("EVI_7_post_mean","LST_Day_1km_7_post_mean","Fpar_20_prior_max"))

extract_coords_combine<-function(varnames,local.directory){

  if(any(!dir.exists(local.directory))){stop("One of the directories given in local.directory argument does not exist")}

  df_list = vector(mode = "list", length = length(varnames))  ### Create empty list to add dataframes to before merging

# Read in and combine if neccessary data for each variable and store in list

  for (v in 1:length(varnames)){

  list<-list.files(local.directory,full.names = T, pattern="*.csv") ### List all csv. files in local.directory

  if(length(list)==0){stop("no files found in local.directory" )}

  list<-list[grep(varnames[v],list)] ## Get file names matching each variable name given

  if(length(list)==0){stop(paste0("no files found in local.directory for varnames: ",varnames[v]))}

    df_list[[v]]<-list  %>% # list files to read in - only important if "split" method used to extract explanatory variable data.
      lapply(read.csv) %>% # read in each one as csv
        dplyr::bind_rows(, .id = "column_label")} ## bind csv. by column labels

  merged.data.frame = Reduce(function(...) merge(..., all=T), df_list) ### Merge all data frames within the list into single, merged data frame

  return(merged.data.frame)}


