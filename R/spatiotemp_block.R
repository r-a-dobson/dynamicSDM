#' Split occurrence records into spatial and temporal blocks for model fitting.
#'
#' Splits occurrence records into spatial and temporal sampling units and groups sampling units into multiple blocks that have similar mean and range of environmental explanatory variables.
#'
#' @param occ.data a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day", and associated explanatory variable data.
#' @param vars.to.block.by a character string or vector, the explanatory variable column names to group sampling units based upon.
#' @param spatial.layer optional; a RasterLayer object, a categorical spatial layer for sampling unit splitting.
#' @param spatial.split.degrees optional; a numeric value, the grid cell resolution in degrees to split spatial.layer by.
#' @param temporal.block optional; a character string or vector, the time step for sampling unit splitting. Any combination of '"day"', '"month"' or '"year"' or "quarter".
#' @param n.blocks optional; a numeric value, the number of blocks to group occurrence records into. Default; 10.
#' @param iterations optional; a numeric value, the number of random block groupings to trial before selecting the optimal grouping. Default; 5000.
#' @details
#' Blocking is an established method to account for spatial autocorrelation in SDMs. Following Bagchi et al., (2013), the blocking method involves splitting occurrence data into sampling units based upon non-contiguous ecoregions, which are then grouped into spatially disaggregated blocks of approximately equal sample size, within which the mean and range of explanatory variable data are similar. When species distribution model fitting, blocks are left out in-turn in a jack-knife approach for model training and testing.
#'
#' We adapt this approach to account for temporal autocorrelation by enabling users to split records into sampling units based upon spatial and temporal characteristic before blocking occurs.
#'
#' Spatial splitting: If the spatial.layer raster has categories that take up large contiguous areas, spatial.split.degrees will split categories into smaller units using grid cells at given resolution.
#' Temporal splitting: If temporal.block is given, then occurrence records with unique values for given level are considered unique sampling unit. For instance, if temporal.block = “year”, then records from the same year are considered a sampling unit to be grouped into blocks. However, if spatial splitting is also used, then spatial characteristics may split these further into separate sampling units.
#' Once split into sampling units based upon temporal and spatial characteristics, these units are then assigned into given number of blocks (n.blocks), so that the mean and range of explanatory variables (vars.to.block.by) are similar across each.
#' @references
#' Bagchi, R., Crosby, M., Huntley, B., Hole, D. G., Butchart, S. H. M., Collingham, Y., Kalra, M., Rajkumar, J., Rahmani, A. & Pandey, M. 2013. Evaluating the effectiveness of conservation site networks under climate change: accounting for uncertainty. Global Change Biology, 19, 1236-1248.
#'
#' @return Returns occurrence data frame with column "BLOCK.CATS", assigning each record to a spatiotemporal block.
#' @example
#' data("sample_model_data", package ="dynamicSDM")
#' data("biome_layer", package ="dynamicSDM")
#' spatiotemp_block(occ.data = sample_model_data,spatial.layer = biome_layer,spatial.split.degrees=3,temporal.block=c("month"),vars.to.block.by=colnames(sample_model_data)[9:12],n.blocks=10)
#'@export
spatiotemp_block<-function(occ.data,vars.to.block.by,spatial.layer=NULL,spatial.split.degrees=NULL,temporal.block=NULL,n.blocks=10,iterations=5000){

  occ.data.save<-occ.data # Save original occ.data to return to used with added block column at end

  if (missing(spatial.layer)){message("spatial.layer is missing. No blocking by spatial features.")}
  if (missing(temporal.block)){message("temporal.block is missing. No blocking by temporal features.")}


  if(!missing(temporal.block)){
    temporal.block.2<-match.arg(temporal.block,choices=c("day","month","year","quarter"),several.ok = T)
    if(any(temporal.block.2=="quarter")){occ.data$quarter <- cut(occ.data$month, breaks = c(0, 3, 6,9, 12),labels = c("first", "second", "third","fourth"))}} # Split data into quarters fo the year by month

  if (!missing(spatial.layer)){
    if(!class(spatial.layer)=="RasterLayer"){stop("spatial.layer must be of class RasterLayer")}
    if(!class(spatial.split.degrees)=="numeric"){stop("spatial.split.degrees must be of class numeric")}

    occ.data.points<- sp::SpatialPointsDataFrame(data = occ.data, coords = cbind(occ.data$x, occ.data$y),
                                                 proj4string =raster::crs(spatial.layer))

    ### Assign occurrence points value from the categorical RasterLayer provided by extracting at record co-ordinates
    split1<-raster::extract(spatial.layer, occ.data.points) #For each record co-ordinate extract spatial.layer value from given RasterLayer
    occ.data$split1<-split1


    ### Assign occurrence points value from a RasterLayer at given spatial.split.degrees to spatially split large categories of previous categorical RasterLayer
    split_grid<-raster::raster(raster::extent(spatial.layer),crs=raster::crs(spatial.layer)) # Create another grid with same spatial extent and CRS as spatial.layer
    raster::res(split_grid)<-spatial.split.degrees  # Set the grid's resolution as specified by the user
    split_grid <- raster::setValues(split_grid,  1:raster::ncell(split_grid)) #fill grid squares with numerical value to create label
    split_grid <-raster::crop(split_grid,raster::extent(spatial.layer))
    split2<-raster::extract(split_grid, occ.data.points)  # Extract grid cell number that each occurrence record belongs too.
    occ.data$split2<-split2}


  cols <-c(if(exists("split1")) "split1",if(exists("split2"))"split2",if(exists("temporal.block.2"))temporal.block.2) # Set column names depending on methods employed

  if(length(cols)>1){occ.data$ID_FOR_BLOCKING<-as.numeric(as.factor(apply(occ.data[ , cols ] , 1 , paste , collapse = "-" )))} ## Create unique ID based on the spatial.layer category,spatial.split.degrees cell number and temporal.block
  if(length(cols)==1){occ.data$ID_FOR_BLOCKING<-as.numeric(as.factor(occ.data[ , cols ]))} ## Create unique ID based on one of the spatial.layer category,spatial.split.degrees cell number or temporal.block

  blockdata<-as.data.frame(unique(occ.data$ID_FOR_BLOCKING)) # List all unique IDs of occurrence records derived from the spatial and temporal categories assigned

  # For each explanatory variable specified, aggregate the data to take the mean of each ID for each variable. These average will be used to assign records with each unique ID to a block

  for (n in 1:length(vars.to.block.by)){blockdata<-cbind(blockdata, aggregate(as.formula(paste(paste(c(vars.to.block.by[n],"ID_FOR_BLOCKING"), collapse="~"),sep = "")),data=occ.data,FUN='mean')[,2])}
  colnames(blockdata)<-c("ID_FOR_BLOCKING",vars.to.block.by)

  #Check minimum number of unique IDs met for the number of blocks (n.blocks) desired.
  if(nrow(blockdata)<n.blocks){stop("Not enough individual sampling units for given number of blocks. Please try adding spatial.split.degrees for large classifications or more temporal split categories if appropriate.")}

  groupings<-list() # Create empty list object to store random groupings of sampling units

  results<-NULL # Create empty vector to store variance of mean and range of each variable for random groupins

  for (x in 1:iterations){

    shuff<- blockdata[sample(1:nrow(blockdata)), ] # Randomly shuffle sampling units

    groups<-split(shuff, cut(1:nrow(shuff), n.blocks, FALSE)) # Split shuffled datasets into roughly equal block groupings

    groupings[[x]]<-groups # Bind random block grouping to list.

    variances<-NULL # Empty vector to bind the variance of means between groups for each variable to

    for (y in 2:ncol(blockdata)){ # Iterates through each variable

      mean <- lapply(groups,function(df_inlist) {base::mean((df_inlist[,y]))}) # Calculates the mean for each block

      variance.mean<-var(unlist(mean)) # Calculate the variance in means across blocks (we want to minimise this value when blocking)

      range <- lapply(groups,function(df_inlist) {(max(df_inlist[,y])-min(df_inlist[,y]))})

      variance.range<-var(unlist(range))


      variances<-rbind(variances,variance.mean,variance.range)}

    results<-rbind(results,t(variances))} # Record variances in mean and range for each variable for this random blocking

  results<-as.data.frame(results)

  results<-as.data.frame(sapply(results, function(x){(x-min(x))/(max(x)-min(x))}))   # Scale variance of mean and range for each variable so that no one variable has more weighting than another when minimising mean and variance between blocks

  optimal.blocking<-groupings[[which.min(rowSums(results))]] # Select block group with minimum variance in mean and range across all variables.

  blocks<-lapply(optimal.blocking,function(x){x[,"ID_FOR_BLOCKING"]}) # Extract sample units within each block

  for (b in 1:n.blocks){occ.data[occ.data$ID_FOR_BLOCKING %in% c(as.numeric(as.character(blocks[[b]]))), "block"]<-b} # Add block number to each occurrence record in data.frame

  occ.data.save$BLOCK.CATS<-occ.data$block # Add block numbers to original provided data frame and return to user

  return(occ.data.save)}
