#' @name sample_explan_data
#' @title Sample species occurrence records with associated dynamic explanatory variables
#' @description A dataset containing a sample of the bird species, the red-billed quelea (Quelea
#'   quelea), distribution records from between 2002-2019 (GBIF 2021, GBIF 2022); generated
#'   pseudo-absence records, and associated extracted dynamic explanatory variables. The variables
#'   are as follows:
#'
#' @format A data frame with 330 rows and 17 variables:
#' \describe{
#'   \item{x}{species occurrence record longitude.}
#'   \item{y}{species occurrence record latitude.}
#'   \item{year}{species occurrence record year.}
#'   \item{month}{species occurrence record month.}
#'   \item{day}{species occurrence record day.}
#'   \item{decimalLatitude}{species occurrence record latitude.}
#'   \item{decimalLongitude}{species occurrence record longitude.}
#'   \item{occurrenceStatus}{species presence or absence character.}
#'   \item{source}{source of occurrence or pseudo-absence data point.}
#'   \item{species}{name of species occurrence records belong to name}
#'   \item{SAMP_EFFORT}{total number of avian e-Bird sampling events within spatiotemporal buffer of
#'   occurrence record location and dates.}
#'   \item{REL_SAMP_EFFORT}{proportion of total number of avian e-Bird sampling events within
#'   spatiotemporal buffer of occurrence record location and dates relative to other records}
#'   \item{unique.ID.DYN}{unique id value assigned when extracting dynamic explanatory variable
#'   data}
#'   \item{eight_sum_prec}{sum Climate Hazards Group InfraRed Precipitation With Station Data (Funk
#'   et al., 2016) total daily precipitation at record co-ordinate across 52-weeks prior to record
#'   date (mm).}
#'   \item{grass_crop_percentage}{total number of MODIS Land Cover Type Yearly 500m (Friedl &
#'   Sulla-Menashe, 2019) "cereal cropland" and "grassland" cells in surrounding area of record
#'   co-ordinate in record year.}
#'   \item{year_sum_prec}{sum Climate Hazards Group InfraRed Precipitation With Station Data (CHIRPS
#'   Daily) total daily precipitation at record co-ordinate across 52-weeks prior to record date
#'   (mm).}
#'   \item{presence.absence}{binary species presence or absence at record location and date.}
#' }
#' @docType data
#' @references
#' Friedl, M., Sulla-Menashe, D. (2019). MCD12Q1 MODIS/Terra+Aqua Land Cover Type Yearly L3 Global
#' 500m SIN Grid V006. NASA EOSDIS Land Processes DAAC.
#'
#'
#' Funk, Chris, Pete Peterson, Martin Landsfeld, Diego Pedreros, James Verdin, Shraddhanand Shukla,
#' Gregory Husak, James Rowland, Laura Harrison, Andrew Hoell & Joel Michaelsen. "The climate
#' hazards infrared precipitation with stations-a new environmental record for monitoring extremes".
#' Scientific Data 2, 150066. doi:10.1038/sdata.2015.66 2015.
#'
#' GBIF.org (12 July 2021) GBIF Occurrence Download \doi{10.15468/dl.ppcu6q}
#'
#' GBIF.org (25 July 2022) GBIF Occurrence Download \doi{10.15468/dl.k2kftv}
#'
"sample_explan_data"


#' @name sample_extent_data
#' @title MULTIPOLYGON object for the extent of southern Africa
#' @description
#' A MULTIPOLYGON (package "sf") object containing polygons for each country within southern Africa.
#' The variables are as follows:
#'
#' @format A simple feature collection with 10 features and 1 field.
#' \describe{
#'   \item{geometry}{MULTIPOLYGON object co-ordinates for country boundaries.}
#'   \item{name}{name of country the polygon represents.}
#' }
#' @docType data
"sample_extent_data"

#' @name sample_occ_data
#'
#' @title Sample species occurrence records
#'@description
#' A dataset containing a sample of the bird species, the red-billed quelea (Quelea quelea),
#' distribution records between 1976-2021 (GBIF 2021 & GBIF 2022).
#' The variables are as follows:
#'
#' @format A data frame with 600 rows and 7 variables:
#' \describe{
#'   \item{year}{species occurrence record year.}
#'   \item{month}{species occurrence record month.}
#'   \item{day}{species occurrence record day.}
#'   \item{decimalLatitude}{species occurrence record latitude.}
#'   \item{decimalLongitude}{species occurrence record longitude.}
#'   \item{occurrenceStatus}{species presence or absence character.}
#'   \item{source}{source of occurrence or pseudo-absence data point.}
#' }
#' @docType data
#' @references
#' GBIF.org (12 July 2021) GBIF Occurrence Download \doi{10.15468/dl.ppcu6q}
#'
#' GBIF.org (25 July 2022) GBIF Occurrence Download \doi{10.15468/dl.k2kftv}
"sample_occ_data"



#' @name sample_filt_data
#'
#' @title Sample of filtered species occurrence records
#'@description
#' A dataset containing a sample of the bird species, the red-billed quelea (Quelea quelea),
#' distribution records (GBIF 2021 & GBIF 2022) that have been filtered to special extent of
#' southern Africa and quality checked using `dynamicSDM` functions.
#' The variables are as follows:
#'
#' @format A data frame with 330 rows and 12 variables:
#' \describe{
#'   \item{x}{species occurrence record x}
#'   \item{y}{species occurrence record y}
#'   \item{year}{species occurrence record year.}
#'   \item{month}{species occurrence record month.}
#'   \item{day}{species occurrence record day.}
#'   \item{decimalLatitude}{species occurrence record latitude.}
#'   \item{decimalLongitude}{species occurrence record longitude.}
#'   \item{occurrenceStatus}{species presence or absence character.}
#'   \item{source}{source of occurrence or pseudo-absence data point.}
#'   \item{species}{name of species occurrence records belong to name}
#'   \item{SAMP_EFFORT}{total number of avian e-Bird sampling events within spatiotemporal buffer of
#'   occurrence record location and dates.}
#'   \item{REL_SAMP_EFFORT}{proportion of total number of avian e-Bird sampling events within
#'   spatiotemporal buffer of occurrence record location and dates relative to other records}
#' }
#' @docType data
#' @references
#' GBIF.org (12 July 2021) GBIF Occurrence Download \doi{10.15468/dl.ppcu6q}
#'
#' GBIF.org (25 July 2022) GBIF Occurrence Download \doi{10.15468/dl.k2kftv}
"sample_filt_data"


#' @name sample_biome_data
#' @title Biome classification raster for southern Africa.
#' @description
#' Categorical RasterLayer cropped to southern Africa at 0.08 degree resolution from OpenLandMap
#' Potential Distribution of Biomes (Hengl et al., 2018).
#' @format A RasterLayer
#' \describe{
#'   \item{class}{RasterLayer}
#'   \item{dimensions}{4839, 3243, 15692877  (nrow, ncol, ncell)}
#'   \item{resolution}{0.008983153, 0.008983153  (x, y)}
#'   \item{extent}{11.71845, 40.85081, -47.89832, -4.428839  (xmin, xmax, ymin, ymax)}
#'   \item{crs}{"+proj=longlat +datum=WGS84 +no_defs"}
#'   \item{source}{biome_type_2001-01-01.tif }
#'   \item{names}{biome_type_2001-01-01.tif }
#'   \item{values}{Numerical categorical value ranging from 0, 255  (min, max).}
#' }
#' @docType data
#' @references
#' Hengl T, Walsh MG, Sanderman J, Wheeler I, Harrison SP, Prentice IC. (2018) Global Mapping of
#' Potential Natural Vegetation: An Assessment of Machine Learning Algorithms for Estimating Land
#' Potential. PeerJ Preprints.
"sample_biome_data"

#' @name sample_events_data
#' @title  Sample e-Bird sampling event records
#'@description
#' A dataset containing a sample of e-Bird sampling events for all bird species across southern
#' Africa between 2000-2020 (Fink et al., 2021, GBIF, 2021).
#' The variables are as follows:
#'
#' @format A data frame with 1000 rows and 5 variables:
#' \describe{
#'   \item{day}{avian e-Bird sampling event day.}
#'   \item{month}{avian e-Bird sampling event month.}
#'   \item{year}{avian e-Bird sampling event year.}
#'   \item{y}{avian e-Bird sampling event latitude.}
#'   \item{x}{avian e-Bird sampling event longitude.}
#' }
#'
#' @docType data
#' @references
#' Fink, D., T. Auer, A. Johnston, M. Strimas-Mackey, O. Robinson, S. Ligocki, W. Hochachka, L.
#' Jaromczyk, C. Wood, I. Davies, M. Iliff, L. Seitz. 2021. eBird Status and Trends, Data Version:
#' 2020; Released: 2021. Cornell Lab of Ornithology, Ithaca, New York.
#'  \doi{10.2173/ebirdst.2020}
#'
#' GBIF.org (12 July 2021) GBIF Occurrence Download \doi{10.15468/dl.ppcu6q}

"sample_events_data"



#' @name sample_cov_data
#' @title Sample projection covariates three variables across for southern Africa.
#' @description
#' Data frame of co-ordinates and associated dynamic explanatory variable values for "2018-04-01"
#' cropped to southern Africa at 2 degree resolution.
#' @format A data frame with 225 rows and 6 variables
#' \describe{
#'   \item{X}{row name}
#'   \item{x}{grid cell longitude}
#'   \item{y}{grid cell latitude}
#'   \item{eight_sum_prec}{sum Climate Hazards Group InfraRed Precipitation With Station Data
#'   (Funk et al., 2015) total daily precipitation at record co-ordinate across 52-weeks prior to
#'   "2018-04-01" (mm).}
#'   \item{grass_crop_percentage}{total number of MODIS Land Cover Type Yearly 500m (Friedl et al.,
#'   2019) "cereal cropland" and "grassland" cells in surrounding area of record co-ordinate in
#'   2018.}
#'   \item{year_sum_prec}{sum Climate Hazards Group InfraRed Precipitation With Station Data (Funk
#'   et al., 2015) total daily precipitation at record co-ordinate across 52-weeks prior to
#'   "2018-04-01" (mm).}
#' }
#' @docType data
#' @references
#' Friedl, M., Sulla-Menashe, D. (2019). MCD12Q1 MODIS/Terra+Aqua Land Cover Type Yearly L3 Global
#' 500m SIN Grid V006. NASA EOSDIS Land Processes DAAC. Accessed 2022-11-24 from
#'
#'
#' Funk, Chris, Pete Peterson, Martin Landsfeld, Diego Pedreros, James Verdin, Shraddhanand Shukla,
#' Gregory Husak, James Rowland, Laura Harrison, Andrew Hoell & Joel Michaelsen. "The climate
#' hazards infrared precipitation with stations-a new environmental record for monitoring extremes".
#' Scientific Data 2, 150066.
"sample_cov_data"

#' @name sample_proj_rast
#' @title Distribution suitability projections for red-billed quelea in southern Africa
#' @description
#' RasterBrick of distribution suitability projections for red-billed quelea across southern at one
#' degree resolution for the dates: "2018-01-01", "2018-04-01", "2018-07-01" and "2018-10-01". A
#' value of 1 represents high suitability and a value of 0 low suitability.
#' @format A RasterBrick
#' \describe{
#'   \item{class}{RasterBrick}
#'   \item{dimensions}{31, 30, 930, 4  (nrow, ncol, ncell, nlayers)}
#'   \item{resolution}{1, 1  (x, y)}
#'   \item{extent}{11.6901, 41.6901, -35.43802, -4.438023  (xmin, xmax, ymin, ymax)}
#'   \item{crs}{"+proj=longlat +datum=WGS84 +no_defs"}
#'   \item{source}{memory} \item{names}{ X2018.01.01_proportional, X2018.04.01_proportional,
#'   X2018.07.01_proportional, X2018.10.01_proportional }
#'   \item{min values}{Numerical minimum distribution suitability projected in each layer.}
#'   \item{max values}{Numerical maximum distribution suitability projected in each layer.}
#' }
#' @docType data
"sample_proj_rast"

