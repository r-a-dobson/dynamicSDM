#' Reformats GBIF data into `dynamicSDM` data frame
#'
#' Function converts GBIF occurrence records into the format required for `dynamicSDM`
#' functions.
#'
#' @param gbif.df a data frame, the direct output from GBIF occurrence record download.
#' @details For most `dynamicSDM` functions, an occurrence data frame with record co-ordinate
#'   columns labelled "x" and "y" with numeric columns for record "day", "month" and "year" are
#'   required. This function takes the input data frame and returns a reformatted data frame
#'   suitable for direct input into `dynamicSDM` functions.
#' @return Returns data frame correctly formatted for input into `dynamicSDM` functions.
#' @examples
#'
#' data(sample_occ_data)
#' converted <- convert_gbif(sample_occ_data)
#'
#'@export

convert_gbif <- function(gbif.df) {

  # Create new df with columns of correct name and class for dynamicSDM functions
  dynsdm <- data.frame(
    x = gbif.df$decimalLongitude,
    y = gbif.df$decimalLatitude,
    year = as.numeric(gbif.df$year),
    month = as.numeric(gbif.df$month),
    day = as.numeric(gbif.df$day)
  )

  # Remove existing year month and day columns from occurrence df
  gbif.df <- gbif.df[, !(names(gbif.df) %in% c("year", "month", "day"))]

  # Bind newly formatted columns to occurrence df
  gbif.df <- as.data.frame(cbind(dynsdm, gbif.df))

  return(gbif.df)

}

