#'Combine extracted explanatory variable data for occurrence records into single data frame.
#'
#'Combines the split output files from functions `extract_dynamic_coords()` and
#'`extract_buffered_coords()` into single data frame containing all occurrence records and
#'explanatory variables.
#'@param varnames a character string, the unique names for each explanatory variable.
#'@param local.directory a character string or vector, the path to local directory or directories to
#'  read extracted explanatory data frames from.
#'@details When functions `extract_dynamic_coords()` and `extract_buffered_coords()` have been used
#'  to extract dynamic explanatory variables for occurrence records, the output for individual
#'  records and each variable will be split into separate “csv” files.
#'
#'  This function reads in these files and combines data into a single data frame containing each
#'  occurrence records and associated explanatory data from each variable.
#'
#'  To prevent error, the “csv” files must be uniquely named within the folder(s) and include an
#'  exact character match for the varnames provided. All “csv” files matching the varnames should
#'  have the same number and names of columns. This is the default output
#'  of`extract_dynamic_coords()` and `extract_buffered_coords()`.
#'
#'@return Returns a data frame containing all occurrence records with associated explanatory
#'  variable data.
#'
#'@examplesIf googledrive::drive_has_token()
#'data(sample_filt_data)
#'\dontshow{
#'sample_filt_data<-sample_filt_data[1:2,]
#'}
#'
#'dynamicSDM::extract_dynamic_coords(
#'  occ.data = sample_filt_data,
#'  datasetname = "UCSB-CHG/CHIRPS/DAILY",
#'  bandname = "precipitation",
#'  spatial.res.metres = 10000,
#'  GEE.math.fun = "sum",
#'  temporal.direction = "prior",
#'  temporal.res = 56,
#'  save.method = "split",
#'  varname = "eightweekprec",
#'  save.directory = tempdir()
#')
#'dynamicSDM::extract_dynamic_coords(
#'  occ.data = sample_filt_data,
#'  datasetname = "UCSB-CHG/CHIRPS/DAILY",
#'  bandname = "precipitation",
#'  spatial.res.metres = 10000,
#'  GEE.math.fun = "sum",
#'  temporal.direction = "prior",
#'  temporal.res = 364,
#'  save.method = "combined",
#'  varname = "annualweekprec",
#'  save.directory = tempdir()
#' )
#'
#'extract_coords_combine(varnames = c("eightweekprec", "annualweekprec"), local.directory =
#'tempdir())
#'@export

extract_coords_combine <- function(varnames,
                                   local.directory) {

  if (any(!dir.exists(local.directory))) {
    stop("One or more local directory(s) not found")
  }

  # Create empty list to add dataframes to before merging
  df_list = vector(mode = "list", length = length(varnames))

  # Read in and combine data for each variable

  for (v in 1:length(varnames)) {

    list <- list.files(local.directory,full.names = T,pattern = "*.csv")

    if (length(list) == 0) {stop("no files found in local.directory")}

    # Get file names matching variable name
    list <- list[grep(varnames[v], list)]

    if (length(list) == 0) {stop(paste0("no files for varname:", varnames[v]))}

    df_list[[v]] <- list  %>%
      lapply(read.csv) %>% # read in each one as csv
      dplyr::bind_rows() # bind csv. by column labels
  }

  # Only want to merge by the columns that are common to all three
  mergeby<-colnames(df_list[[1]])[1:(length(colnames(df_list[[1]]))-1)]

  # Merge all data frames within the list into single data frame
  merged.data.frame = Reduce(function(...)
    merge(..., by = mergeby, all = T), df_list)

  return(merged.data.frame)
}


