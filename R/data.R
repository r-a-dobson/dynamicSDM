#' @name sample_model_data
#' @title Sample species occurrence records with associated dynamic explanatory variables
#' @description
#' A dataset containing a sample of the bird species, the red-billed quelea (Quelea quelea), distribution and abundance records from between 2002-2017 including generated pseudo-absence records and associated extracted dynamic explanatory variables.
#' The variables are as follows:
#'
#' @format A data frame with 10000 rows and 14 variables:
#' \describe{
#'   \item{x}{species occurrence record longitude.}
#'   \item{y}{species occurrence record latitude.}
#'   \item{year}{species occurrence record year.}
#'   \item{month}{species occurrence record month.}
#'   \item{day}{species occurrence record day.}
#'   \item{presence.absence}{binary species presence or absence at record location and date.}
#'   \item{individualCount}{number of individuals of the species present at record location and date.}
#'   \item{sampling_weights}{total number of avian e-Bird sampling events within spatiotemporal buffer of occurrence record location and dates.}
#'   \item{Temperaturemean}{mean MODIS Terra Land Surface Temperature at record co-ordinate across 52-weeks prior to record date (kelvin).}
#'   \item{TemperatureEightmean}{mean MODIS Terra Land Surface Temperature at record co-ordinate across 8-weeks prior to record date (kelvin).}
#'   \item{Precipitationsum}{sum Climate Hazards Group InfraRed Precipitation With Station Data (CHIRPS Daily) total daily precipitation at record co-ordinate across 52-weeks prior to record date (mm).}
#'   \item{Precipitation8Wsum}{sum Climate Hazards Group InfraRed Precipitation With Station Data (CHIRPS Daily) total daily precipitation at record co-ordinate across 52-weeks prior to record date (mm).}
#'   \item{watercount}{total number of MODIS Land Cover Type Yearly 500m "	Water Bodies: at least 60% of area is covered by permanent water bodies" cells in surrounding area of record co-ordinate in record year.}
#'   \item{blockno}{Block group assinged for model fitting by jacknife approach to account for spatial and temporal autocorrelation}
#' }
#' @docType data
#' @references
#' Friedl, M., Sulla-Menashe, D. (2019). MCD12Q1 MODIS/Terra+Aqua Land Cover Type Yearly L3 Global 500m SIN Grid V006 [Data set]. NASA EOSDIS Land Processes DAAC. Accessed 2022-11-24 from https://doi.org/10.5067/MODIS/MCD12Q1.006
#'
#' Funk, Chris, Pete Peterson, Martin Landsfeld, Diego Pedreros, James Verdin, Shraddhanand Shukla, Gregory Husak, James Rowland, Laura Harrison, Andrew Hoell & Joel Michaelsen. "The climate hazards infrared precipitation with stations-a new environmental record for monitoring extremes". Scientific Data 2, 150066. doi:10.1038/sdata.2015.66 2015.
#'
#' GBIF.org (12 July 2021) GBIF Occurrence Download https://doi.org/10.15468/dl.ppcu6q
#'
#' GBIF.org (25 July 2022) GBIF Occurrence Download https://doi.org/10.15468/dl.k2kftv
#'
#' Wan, Z., Hook, S., Hulley, G. (2015). MOD11A1 MODIS/Terra Land Surface Temperature/Emissivity Daily L3 Global 1km SIN Grid V006 [Data set]. NASA EOSDIS Land Processes DAAC. Accessed 2022-11-24 from https://doi.org/10.5067/MODIS/MOD11A1.006
"sample_model_data"


#' @name sample_occ_abs_data
#' @title Sample species occurrence records with pseudo-absence records
#' @description
#' A dataset containing a sample of the bird species, the red-billed quelea (Quelea quelea), distribution and abundance records between 2002-2017 with generated spatiotemporal pseudo-absence records.
#' The variables are as follows:
#'
#' @format A data frame with 50 rows and 7 variables:
#' \describe{
#'   \item{x}{species occurrence record longitude.}
#'   \item{y}{species occurrence record latitude.}
#'   \item{year}{species occurrence record year.}
#'   \item{month}{species occurrence record month.}
#'   \item{day}{species occurrence record day.}
#'   \item{presence.absence}{binary species presence or absence at record location and date.}
#'   \item{individualCount}{number of individuals of the species present at record location and date.}
#' }
#' @docType data
#' @references
#' GBIF.org (12 July 2021) GBIF Occurrence Download https://doi.org/10.15468/dl.ppcu6q
#'
#' GBIF.org (25 July 2022) GBIF Occurrence Download https://doi.org/10.15468/dl.k2kftv
"sample_occ_abs_data"

#' @name sample_occ_data
#'
#' @title Sample species occurrence records
#'@description
#' A dataset containing a sample of the bird species, the red-billed quelea (Quelea quelea), distribution and abundance records between 2002-2017.
#' The variables are as follows:
#'
#' @format A data frame with 10000 rows and 14 variables:
#' \describe{
#'   \item{x}{species occurrence record longitude.}
#'   \item{y}{species occurrence record latitude.}
#'   \item{year}{species occurrence record year.}
#'   \item{month}{species occurrence record month.}
#'   \item{day}{species occurrence record day.}
#'   \item{presence.absence}{binary species presence or absence at record location and date.}
#'   \item{individualCount}{number of individuals of the species present at record location and date.}
#' }
#' @docType data
#' @references
#' GBIF.org (12 July 2021) GBIF Occurrence Download https://doi.org/10.15468/dl.ppcu6q
#'
#' GBIF.org (25 July 2022) GBIF Occurrence Download https://doi.org/10.15468/dl.k2kftv
"sample_occ_data"

#' @name biome_layer
#' @title Categorical raster.
#' @description
#' Categorical RasterLayer cropped to southern Africa at 10 degree resolution.
#' Represents a biome classification RasterLayer that could be used when spatially blocking species occurrence data.
#'
#' @format A RasterLayer
#' \describe{
#'   \item{class}{RasterLayer}
#'   \item{dimensions}{4, 3, 12   (nrow, ncol, ncell)}
#'   \item{resolution}{10, 10  (x, y)}
#'   \item{extent}{11.71845, 40.85081, -47.89832, -4.428839  (xmin, xmax, ymin, ymax)}
#'   \item{crs}{"+proj=longlat +datum=WGS84 +no_defs"}
#'   \item{source}{memory}
#'   \item{names}{layer}
#'   \item{values}{Numerical categorical value.}
#' }
#' @docType data
"biome_layer"


#' @name sample_surveyeffort
#' @title  Sample e-Bird sampling event records
#'@description
#' A dataset containing a sample of e-Bird sampling events for all bird species across southern Africa between 2000-2020.
#' The variables are as follows:
#'
#' @format A data frame with 10000 rows and 5 variables:
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
#' Fink, D., T. Auer, A. Johnston, M. Strimas-Mackey, O. Robinson, S. Ligocki, W. Hochachka, L. Jaromczyk, C. Wood, I. Davies, M. Iliff, L. Seitz. 2021. eBird Status and Trends, Data Version: 2020; Released: 2021. Cornell Lab of Ornithology, Ithaca, New York. https://doi.org/10.2173/ebirdst.2020
#'
#'GBIF.org (12 July 2021) GBIF Occurrence Download  https://doi.org/10.15468/dl.ppcu6q
"sample_surveyeffort"



