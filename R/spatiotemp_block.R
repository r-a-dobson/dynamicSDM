#'Split occurrence records into spatial and temporal blocks for model fitting.
#'
#'Splits occurrence records into spatial and temporal sampling units and groups sampling units into
#'multiple blocks that have similar mean and range of environmental explanatory variables and sample
#'size.
#'
#'@param occ.data a data frame, with columns for occurrence record co-ordinates and dates with
#'  column names as follows; record longitude as "x", latitude as "y", year as "year", month as
#'  "month", and day as "day", and associated explanatory variable data.
#'@param vars.to.block.by a character string or vector, the explanatory variable column names to
#'  group sampling units based upon.
#'@param spatial.layer optional; a `RasterLayer` object, a categorical spatial layer for sample
#'  unit splitting.
#'@param spatial.split.degrees a numeric value, the grid cell resolution in degrees to split
#'  `spatial.layer` by. Required if `spatial.layer` given.
#'@param temporal.block optional; a character string or vector, the time step for sampling unit
#'  splitting. Any combination of `day`, `month`, `year` or `quarter.` See details.
#'@param n.blocks optional; a numeric value of two or more, the number of blocks to group
#'  occurrence records into. Default; 10.
#'@param iterations optional; a numeric value, the number of random block groupings to trial before
#'  selecting the optimal grouping. Default; 5000.
#'@details
#'
#'# Blocking for autocorrelation
#'
#'Blocking is an established method to account for spatial autocorrelation in SDMs. Following Bagchi
#'et al., (2013), the blocking method involves splitting occurrence data into sampling units based
#'upon non-contiguous ecoregions, which are then grouped into spatially disaggregated blocks of
#'approximately equal sample size, within which the mean and range of explanatory variable data are
#'similar. When species distribution model fitting, blocks are left out in-turn in a jack-knife
#'approach for model training and testing.
#'
#'We adapt this approach to account for temporal autocorrelation by enabling users to split records
#'into sampling units based upon spatial and temporal characteristic before blocking occurs.
#'
#'# Spatial splitting
#'
#'If the `spatial.layer` has categories that take up large contiguous areas,
#'`spatiotemp_block()` will split categories into smaller units using grid cells at specified
#'resolution (`spatial.split.degrees`).
#'
#'# Temporal splitting
#'
#'If `temporal.block` is given, then occurrence records with unique values for the given level are
#'considered unique sampling unit. For instance, if `temporal.block` = `year`, then records from the
#'same year are considered a sampling unit to be grouped into blocks.
#'
#'Note: If spatial splitting is also used, then spatial characteristics may split these further into
#'separate sampling units.
#'
#'The `temporal.block` option `quarter` splits occurrence records into sampling units based on which
#'quarter of the year the record month belongs to: (1) January-March, (2) April-June, (3)
#'July-September and (4) October-December. This could be employed if seasonal biases in occurrence
#'record collection are driving autocorrelation.
#'
#'# Block generation
#'
#'Once split into sampling units based upon temporal and spatial characteristics, these units are
#'then assigned into given number of blocks (`n.blocks`), so that the mean and range of explanatory
#'variables (`vars.to.block.by`) and total sample size are similar across each. The number of
#'`iterations` specifies how many random shuffles are used to optimise block equalisation.
#'
#'
#'@references Bagchi, R., Crosby, M., Huntley, B., Hole, D. G., Butchart, S. H. M., Collingham, Y.,
#'  Kalra, M., Rajkumar, J., Rahmani, A. & Pandey, M. 2013. Evaluating the effectiveness of
#'  conservation site networks under climate change: accounting for uncertainty. Global Change
#'  Biology, 19, 1236-1248.
#'
#'@return Returns occurrence data frame with column "BLOCK.CATS", assigning each record to a
#'  spatiotemporal block.
#' @examples
#' \donttest{
#' data("sample_explan_data")
#' data("sample_biome_data")
#' spatiotemp_block(
#'  occ.data = sample_explan_data,
#'  spatial.layer = sample_biome_data,
#'  spatial.split.degrees = 3,
#'  temporal.block = c("month"),
#'  vars.to.block.by = colnames(sample_explan_data)[14:16],
#'  n.blocks = 3,
#'  iterations = 30
#')
#'}
#'@export

spatiotemp_block <- function(occ.data,
                             vars.to.block.by,
                             spatial.layer,
                             spatial.split.degrees,
                             temporal.block,
                             n.blocks = 10,
                             iterations = 5000) {

    # Save occ.data to return with added block column at end
    occ.data.save <- occ.data

    if (n.blocks < 2) {
      stop("n.blocks must be over one for blocking")
    }

    if (missing(spatial.layer)) {
      message("spatial.layer is missing. No blocking by spatial features.")
    }

    if (missing(temporal.block)) {
      message("temporal.block is missing. No blocking by temporal features.")
      temporal.block.2 <-NULL
    }

    if (!missing(temporal.block)) {
      temporal.block.2 <- match.arg(temporal.block,
                                    choices = c("day", "month", "year", "quarter"),
                                    several.ok = TRUE)

      # Split data into quarters fo the year by month
      if (any(temporal.block.2 == "quarter")) {
        occ.data$quarter <- cut(occ.data$month,
                                breaks = c(0, 3, 6, 9, 12),
                                labels = c("first", "second", "third", "fourth"))
      }
    }

    if (!missing(spatial.layer)) {

      if (!inherits(spatial.layer, "RasterLayer")) {
        stop("spatial.layer must be of class RasterLayer")
      }
      if (missing(spatial.split.degrees)) {
        stop("spatial.layer given but not spatial.split.degrees")
      }
      if (!inherits(spatial.split.degrees, "numeric")) {
        stop("spatial.split.degrees must be of class numeric")
      }

      occ.data.points <- sp::SpatialPointsDataFrame(data = occ.data,
                                                    coords = cbind(occ.data$x, occ.data$y),
                                                    proj4string = raster::crs(spatial.layer))

      # Assign occ points value from the categorical RasterLayer
      split1 <- raster::extract(spatial.layer, occ.data.points)
      occ.data$split1 <- split1


      # Assign occurrence points value from RasterLayer at
      # given res to split large categories of categorical RasterLayer

      # Create another grid with same spatial extent and CRS as spatial.layer
      split_grid <- raster::raster(raster::extent(spatial.layer),
                                   crs = raster::crs(spatial.layer))

      # Set the grid's resolution as specified by the user
      raster::res(split_grid) <- spatial.split.degrees
      # Fill grid squares with numerical value to create label
      split_grid <- raster::setValues(split_grid,  1:raster::ncell(split_grid))
      split_grid <- raster::crop(split_grid, raster::extent(spatial.layer))
      # Extract grid cell number that each occurrence record belongs too.
      split2 <- raster::extract(split_grid, occ.data.points)
      occ.data$split2 <- split2
    }

    # Set column names depending on methods employed

    cols <-c(if("split1" %in% colnames(occ.data)) "split1",
             if("split2" %in% colnames(occ.data)) "split2",
             if(all(temporal.block.2 %in% colnames(occ.data))) temporal.block.2)

    ## Create unique ID based on the spatial.layer, split cell and temporal

    if (length(cols) > 1) {
      occ.data$ID_BL <- as.numeric(as.factor(apply(occ.data[, cols], 1, paste, collapse = "-")))
    }

    if (length(cols) == 1) {
      occ.data$ID_BL <- as.numeric(as.factor(occ.data[, cols]))
    }

    # List all unique IDs of occurrence records
    blockdata <- as.data.frame(unique(occ.data$ID_BL))
    occ.data$count <- rep(1, nrow(occ.data))  # Add column to sum records

    # Aggregate to count records per unique ID.
    blockdata <- cbind(blockdata, aggregate(count ~ ID_BL, data = occ.data, FUN = 'sum')[, 2])

    colnames(blockdata)<-c("ID_BL","count")
    vars.to.block.by<-c(vars.to.block.by)

    # For each variable specified, take the mean of each ID group

    for (n in 1:length(vars.to.block.by)) {

    formula <- as.formula(paste(paste(c(vars.to.block.by[n], "ID_BL"), collapse = "~"), sep = ""))
    vardata<-aggregate(formula, data = occ.data, FUN = 'mean')
    blockdata <-merge(blockdata,vardata,by="ID_BL",all.x=T)

    }

    colnames(blockdata) <- c("ID_BL","count", vars.to.block.by)

    # Check minimum unique IDs met for number of blocks (n.blocks) desired.

    if (nrow(blockdata) < n.blocks) {
      stop("Not enough sampling units for number of blocks.")
    }

    # Create empty list object to store random groupings of sampling units
    groupings <- list()

    # Create empty vector to store variance of mean and range of each variable
    results <- NULL

    for (x in 1:iterations) {

      # Randomly shuffle sampling units
      shuff <- blockdata[sample(1:nrow(blockdata)),]

      # Split shuffled datasets into roughly equal block groupings
      groups <- split(shuff, cut(1:nrow(shuff), n.blocks, FALSE))

      # Bind random block grouping to list.
      groupings[[x]] <- groups

      # Empty vector to bind variance of means between groups for each variable
      variances = NULL

      # Calculate total sample size per block
      samplesize <- lapply(groups, function(df_inlist) {base::sum((df_inlist[, "count"]))})

      # Calculate mean and variance of block sample size
      variance.sum <- var(unlist(samplesize))


      variances <- rbind(variances, variance.sum)


      for (y in 3:ncol(blockdata)) {
        # First col is ID, second col count

        # Calculates the mean for each block

        mean.var <- lapply(groups, function(df_inlist) {base::mean((df_inlist[, y]),na.rm=T)})

        # Variance in means across blocks (want to minimise this when blocking)
        variance.mean <- var(unlist(mean.var))

        range <- lapply(groups, function(df_inlist) {
                          (max(df_inlist[, y],na.rm=T) - min(df_inlist[, y],na.rm=T))})

        variance.range <- var(unlist(range))

        variances <- rbind(variances, variance.mean, variance.range)
      }

      # Record variances in mean and range for each variable for this random blocking
      results <- rbind(results, t(variances))}

    results <- as.data.frame(results)

    # Scale variance of mean and range for each variable
    # So no variable has more weighting than another when minimising mean/var

    results <- as.data.frame(sapply(results, function(x) {(x - min(x)) / (max(x) - min(x))}))


    # Select grouping with minimum variance in mean and range across variables.
    optimal.blocking <- groupings[[which.min(rowSums(results,na.rm=TRUE))]]

    # Extract sample units within each block of optimal grouping
    blocks <- lapply(optimal.blocking, function(x) {x[, "ID_BL"]})

    # Add block number to each occurrence record in data.frame
    for (b in 1:n.blocks) {
      occ.data[occ.data$ID_BL %in% c(as.numeric(blocks[[b]])), "block"] <- b
    }

    # Add block numbers to original provided data frame and return to user
    occ.data.save$BLOCK.CATS <- occ.data$block

    return(occ.data.save)
  }

