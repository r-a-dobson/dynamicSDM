#' Thin species occurrence records by spatial and temporal proximity.
#'
#' Thins species occurrence records that are within minimum spatial and temporal distance apart.
#'
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".
#' @param temporal.method a character string, the method to calculate temporal distance between records. One of "'DOY'" or "'day'". See details for more information.
#' @param spatial.dist a numeric value, the spatial buffer distances in metres to thin records by.
#' @param temporal.dist a numeric value, the temporal buffer in days to thin records by.
#' @param spatial.split.degrees a numeric value, the grid cell resolution in degrees to split occurrence records by before temporal thinning.
#' @param iterations a numeric value, the number of iterations to randomly thin occurrence records by. Default; 100.
#' @details
#’ spatiotemp_thin calculates the temporal distance between occurrence records in given area and excludes records below minimum temporal distance apart. Then calculates spatial distance between all occurrence records and filters records below minimum spatial distance apart. Thus, spatiotemp_thin offers spatial and temporal thinning, improving functionality for dynamic species distribution modelling compared to existing spThin package functions (Aiello-Lammens et al., 2015).
#' For temporal thinning, the function first splits occurrence records into grid cells of given size in degrees (set by spatial.split.degrees). This is to prevent spatially distant but temporally close records from being excluded. For each grid cell, all records within the cell are temporally thinned. This process works by removing records that are within given temporal distance (temporal.dist) from each other by randomly selecting one of the two. This iterates through until no records are within the given temporal distance of each other in each grid cell, following a similar algorithm to spThin (Aiello-Lammens et al., 2015).
#'
#' Two methods exist for measuring the temporal distance between occurrence records. temporal.method = “DOY” calculates the minimum days apart within the annual cycle and “day” uses the absolute number of days. For instance, two dates “2010-01-05” and “2012-12-05” can be calculated as either 1065 absolute days apart, or within the annual cycle these dates represent day 5 and day 339 of the year, and are 31 days apart. Therefore, thinning by 40 days using “DOY” method would remove one of these records, but using the “day” method would not. Chosen temporal.method will depend upon whether bias towards a point within the annual cycle is more likely to bias models than bias towards a given point in linear time.
#'
#' Following temporal thinning, spatial thinning occurs across entire dataset. The spatial distance between each record is calculated, and records within the given spatial distance (spatial.dist) from each other are excluded by randomly selecting one of these. This iterates through until no records are with the given spatial distances of each other across entire dataset, again following a similar algorithm to spThin (Aiello-Lammens et al., 2015).
#'
#' As random selection could alter the total number of occurrence records remaining in the occurrence record dataset, this process is iterated through a specified number of times and the thinned data frame with the highest number of records remaining is returned.
#' @references
#' Aiello-Lammens, M. E., Boria, R. A., Radosavljevic, A., Vilela, B. & Anderson, R. P. 2015. spThin: an R package for spatial thinning of species occurrence records for use in ecological niche models. Ecography, 38, 541-545.

#' @return Returns data frame of occurrence records thinned by specified temporal and spatial distance.
#' @examples
#' x<-c(27.79125, 28.54125, 25.54125, 30.04125, 29.95792)
#' y<-c(-26.79125, -26.37458, -26.70792, -29.37458, -28.45792)
#' year<-c(2014, 2016, 2011, 2011, 2015)
#' month<-c(1, 2, 3, 2, 4)
#' day<-c(27, 25, 16, 25, 26)
#' occ.data<-data.frame(cbind(x,y,year,month,day))
#'
#' spatiotemp_thin(occ.data=occ.data, temporal.method="day", temporal.dist=14,spatial.split.degrees=3,spatial.dist=100000, iterations=5)
#'@export

spatiotemp_thin <-  function(occ.data,
                             temporal.method,
                             temporal.dist,
                             spatial.split.degrees,
                             spatial.dist,
                             iterations = 100) {


  # Check correct formatting of iterations argument

  if (!is.numeric(iterations)) {
    stop("iterations must be of class numeric")
  }

  if (!length(iterations) == 1) {
    stop("iterations must be of length(1)")
  }

  if (!is.numeric(temporal.dist)) {
    stop("temporal.dist must be of class numeric")
  }

  if (!length(temporal.dist) == 1) {
    stop("temporal.dist must be of length(1)")
  }

  # Round min coords down to nearest 10 to ensure all points included in a cell
  xmin <- floor(min(occ.data$x) / 10) * 10
  ymin <- floor(min(occ.data$y) / 10) * 10
  # Round max coords up to nearest 10 to ensure all points included in a cell
  xmax <- ceiling(max(occ.data$x) / 10) * 10
  ymax <- ceiling(max(occ.data$y) / 10) * 10

  # Create a grid using the rounded minimum and maximum longitude and latitude
  split_grid <- raster::raster(raster::extent(c(xmin, xmax, ymin, ymax)))

  # Set grid as resolution specified by the user
  raster::res(split_grid) <- spatial.split.degrees

  # Fill grid squares with numerical value to create label
  split_grid <- raster::setValues(split_grid,  1:raster::ncell(split_grid))

  # Convert occ.data co-ordinates into spatial points
  occ.data.points <- sp::SpatialPointsDataFrame(data = occ.data,
                                                coords = cbind(occ.data$x,
                                                               occ.data$y))
  # Extract grid cell number each record belongs to, to group before thinning
   split2 <- raster::extract(split_grid, occ.data.points)

   occ.data$split2 <- split2

  #-------------------------------------------------------
  # Temporal thinning
  #-------------------------------------------------------

  # Create empty list to fill with thinned dataset after each iteration
  list.of.thinned.dfs <- vector("list", iterations)

  for (It in 1:iterations) {
    # Create null vector to bind results to in each split
    results <- NULL

    # Filter occurrence data to each individual grid cell
    for (split in 1:length(unique(split2))) {

      occ.data.split <- dplyr::filter(occ.data, split2 == unique(split2)[split])

      # Create distance matrix between points

      temporal.method <- match.arg(temporal.method, choices = c("DOY", "day"))


      if (temporal.method == "DOY") {

        # Turn dates into "day of year"
        dayofyear <-
          c(lubridate::yday(as.Date(with(
            occ.data.split, paste(year, month, day, sep = "-")
          ), "%Y-%m-%d")))

        # Create matrix of distance in days between records based on day of year
        matrix <- matrix(NA, nrow = length(dayofyear), length(dayofyear))

        for (x in 1:length(dayofyear)) {
          value <- dayofyear[x]
          a <- value - dayofyear
          b <- 365 - (dayofyear) + (value - 1)
          c <- 365 - (value) + (dayofyear - 1)
          matrix[x,] <- apply(data.frame(a, b, c), 1, FUN = min) # Extract min
        }

        matrix <- abs(matrix) # Make distances positive

        # Compare to set minimum distance to thin by.
        matrix <- matrix < temporal.dist
        diag(matrix) <- FALSE # Set diagonal combinations as FALSE as always 0
      }


      if (temporal.method == "day") {

        # Standardise all dates compared to earliest data in data.frame
        dayssince <-
          as.Date(with(occ.data.split, paste(year, month, day, sep = "-")),
                  "%Y-%m-%d") -
          min(as.Date(with(
            occ.data.split, paste(year, month, day, sep = "-")
          ), "%Y-%m-%d"))

        # Create matrix of distance apart in days of each date
        matrix <- as.matrix(dist(dayssince))
        matrix <- matrix < temporal.dist
        diag(matrix) <- FALSE
      }

      matrix.original <- matrix

      # Work out how many records each record is under the temporal thinning
      # distance from (when one is removed, others may not longer need removing)
      level.of.overlap <- rowSums(matrix.original, na.rm = T)

      # Create vector of TRUE to record which row gets removed in the iteration
      record.of.removal <- rep(TRUE, length(level.of.overlap))

      # For each iteration reset values
      matrix <- matrix.original
      overlap <- level.of.overlap
      remove <- record.of.removal

      # While there are still records too close together,
      # keep iterating through and randomly removing them
      # and look at impact on others to select next record to remove

      while (any(matrix)) {

        # Identify rows with highest overlap as we want to exclude these first
        rows.to.remove <- as.numeric(which(overlap == max(overlap, na.rm = T)))

        # Randomly select one of the rows with highest overlap to exclude first
        rows.to.remove <- sample(rows.to.remove, 1)

        # Remove therow from the distance matrix & minus its from overlap vector
        overlap <- overlap - matrix[rows.to.remove, ]

        # Removed row no longer overlaps any others, set to 0
        overlap[rows.to.remove] <- 0

        # Change value of removed row from TRUE to FALSE as removed from matrix
        matrix[rows.to.remove, ] <- FALSE
        matrix[, rows.to.remove] <- FALSE

        # Keep record of which rows to remove from final occurrence data.frame
        remove[rows.to.remove] <- FALSE
      }

      # Make the new, thinned, data set
      thinned.data.frame <- occ.data.split[remove, , drop = FALSE]
      results <- rbind(results, thinned.data.frame)
    }


    #-------------------------------------------------------
    # Spatial thinning
    #-------------------------------------------------------

    # Which distances between records are below the distance set by user
    distance_matrix <- geodist::geodist(data.frame(results$x, results$y),
                                        measure = 'geodesic') < spatial.dist

    # Set diagonal as FALSE as these will always be below the spatial.dist
    diag(distance_matrix) <- FALSE

    # Create empty list to fill with thinned dataset after each iteration

    matrix.original <- distance_matrix

    # Work out how many records each record is under the thinning distance from
    level.of.overlap <- rowSums(matrix.original, na.rm = T)

    # Make a vector of TRUE to record which row gets removed in the iteration
    record.of.removal <- rep(TRUE, length(level.of.overlap))

    # For each iteration reset these
    matrix <- matrix.original
    overlap <- level.of.overlap
    remove <- record.of.removal

    # While there are still records too close together,
    # keep iterating through and randomly removing them
    # and look at impact on others to select next record to remove

    while (any(matrix)) {

      # Identify rows with highest overlap as we want to exclude these first
      rows.to.remove <- as.numeric(which(overlap == max(overlap, na.rm = T)))

      # Randomly select one of the rows with highest overlap to exclude first
      rows.to.remove <- sample(rows.to.remove, 1)

      # Remove the row from  distance matrix, minus its impact from overlap
      overlap <- overlap - matrix[rows.to.remove, ]

      # Removed row no longer overlaps any others
      overlap[rows.to.remove] <- 0

      # Change value of removed row from TRUE to FALSE as removed from matrix
      matrix[rows.to.remove, ] <- FALSE
      matrix[, rows.to.remove] <- FALSE

      # Keep record of which rows to remove from final occurrence data frame
      remove[rows.to.remove] <- FALSE
    }

    # Make the new, thinned, data set
    thinned.data.frame <- results[remove, , drop = FALSE]

    list.of.thinned.dfs[[It]] <- thinned.data.frame
  }

  # Calculate which thinned dataframe has the most records remaining

  n.records <- unlist(lapply(list.of.thinned.dfs, nrow))

  thinned.df <- as.data.frame(list.of.thinned.dfs[which.max(n.records)])

  return(thinned.df)
}
