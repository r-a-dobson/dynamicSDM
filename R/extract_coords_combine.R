#'Combine extracted explanatory variable data for occurrence records into single data frame.
#'
#'Combines the split output files from functions `extract_dynamic_coords()` and
#'`extract_buffered_coords()` into single data frame containing all occurrence records and
#'explanatory variables.
#'@param varnames a character string, the unique names for each explanatory variable.
#'@param local.directory a character string or vector, the path to local directory or directories to
#'  read extracted explanatory data frames from.
#'@param set_class a logical indicating whether to set the classes of each column in the data
#'   before merging to avoid error. See details for more information.
#'@param col_classes optional; a named vector specifying the classes for columns within occ.data.
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
#'  # Column classes
#'
#'  When co-ordinate data have been extracted using the `split` method in `extract_dynamic_coords()`
#'  and `extract_buffered_coords()`, sometimes the classes of columns in the separate exported data
#'  frames can vary. For instance, one split row may contain an NA value in one column, whilst
#'  another split contains a numerical value. When `extract_coords_combine()` attempts to read these
#'  in and bind them together, an error can occur due to the class mismatch (logical compared to
#'  numeric).
#'
#'  There are two arguments that can help to resolve this error:
#'
#'  + If `set_class = TRUE` and `col_classes` is given, then the column classes for each split data
#'  frame will be set as the classes in `col_classes`.
#'
#'  + If `set_class = TRUE` and `col_classes` is not given, then the column classes for each split
#'  data frame will be set by the classes of columns in the first read in csv. Be aware that this
#'  approach may be inaccurate and lead to parsing warnings, as it depends on the first split
#'  containing the correct classes (e.g. numeric not a logical in the example above).
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
#'extract_coords_combine(varnames = c("eightweekprec", "annualweekprec"),
#'                       local.directory = tempdir(),
#'                       set_class = TRUE,
#'                       col_classes = c(sapply(sample_filt_data,class),
#'                                     eightweekprec = "numeric",
#'                                     annualweekprec="numeric"))
#'@export


extract_coords_combine <- function(varnames,
                                   local.directory,
                                   set_class = FALSE,
                                   col_classes) {

  if (any(!dir.exists(local.directory))) {
    stop("One or more local directory(s) not found")
  }

  # Create empty list to add dataframes to before merging
  df_list = vector(mode = "list", length = length(varnames))

  # Read in and combine data for each variable

  for (v in 1:length(varnames)) {

    list <- list.files(local.directory,full.names = TRUE,pattern = "*.csv")

    if (length(list) == 0) {stop("no files found in local.directory")}

    # Get file names matching variable name
    list <- list[grep(varnames[v], list)]

    if (length(list) == 0) {stop(paste0("no files for varname:", varnames[v]))}


    if(set_class){

      if (missing(col_classes)) {
        data_classes <- sapply(readr::read_csv(list[1]), class)
        data_classes <- lapply(data_classes, `[[`, 1)
        data_classes <- unlist(data_classes)
        data_classes <- gsub("logical", "character", data_classes)

      }

      if(!missing(col_classes)){

        data_classes <- c(col_classes[1:(length(col_classes)-length(varnames))],
                          unique.ID.DYN = "numeric",
                          col_classes[(length(col_classes)-length(varnames))+v])
      }

    df_list[[v]] <- list  %>%
      lapply(function(x) {readr::read_csv(x, col_types = data_classes)}) %>%
      dplyr::bind_rows()

    }

    if(!set_class){

      df_list[[v]] <- list  %>%
        lapply(utils::read.csv) %>% # read in each one as csv
        dplyr::bind_rows()
      }

  }

  # Only want to merge by the columns that are common to all three
  mergeby<-colnames(df_list[[1]])[1:(length(colnames(df_list[[1]]))-1)]

  # Merge all data frames within the list into single data frame
  merged.data.frame = Reduce(function(...)
    merge(..., by = mergeby, all = TRUE), df_list)

  return(merged.data.frame)
}


