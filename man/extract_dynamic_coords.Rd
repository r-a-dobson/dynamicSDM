% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_dynamic_coords.R
\name{extract_dynamic_coords}
\alias{extract_dynamic_coords}
\title{Extract temporally dynamic explanatory variable data for occurrence records.}
\usage{
extract_dynamic_coords(
  occ.data,
  datasetname,
  bandname,
  spatial.res.metres,
  GEE.math.fun,
  save.method,
  temporal.res,
  temporal.direction,
  varname,
  save.directory
)
}
\arguments{
\item{occ.data}{a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".}

\item{datasetname}{a character string, the Google Earth Engine dataset to extract data from.}

\item{bandname}{a character string, the Google Earth Engine dataset bandname to extract data for.}

\item{spatial.res.metres}{a numeric value, the spatial resolution in metres for data extraction.}

\item{GEE.math.fun}{a character string, the mathematical function to compute across the temporal.res period for each record.}

\item{save.method}{a character string, the method used to save extracted variable data. One of '"split"' or '"combined"': can be abbreviated. See details.}

\item{temporal.res}{a numeric value, the temporal resolution in days to extract data and calculate GEE.math.fun across from record date.}

\item{temporal.direction}{a character string, the temporal direction for extracting data across relative to the record date. One of '"prior"' or '"post"': can be abbreviated.}

\item{varname}{optional; a character string, the unique name for the explanatory variable. Default varname is “bandname_temporal.res_temporal.direction_ GEE.math.fun".}

\item{save.directory}{a character string, the path to a local directory to save extracted variable data to.}
}
\value{
Returns details of successful explanatory variable extractions.
}
\description{
For each species occurrence record co-ordinate and date, temporally dynamic explanatory data are extracted using Google Earth engine
}
\details{
'For each individual species occurrence record co-ordinate and date, this function extracts data for a given band within a Google Earth Engine dataset across a user-specified period and calculates a mathematical function on such data.

GEE.math.fun specifies the mathematical function to be calculated over the temporal period. Options are limited to Google Earth Engine ImageCollection Reducer functions (https://developers.google.com/earth-engine/apidocs/) for which an analogous R function is available. This includes: "allNonZero","anyNonZero", "count", "first","firstNonNull", "last", "lastNonNull", "max","mean", "median","min", "mode","product", "sampleStdDev", "sampleVariance", "stdDev", "sum" and "variance".

For save.method '"combined"', the function with save “.csv” files containing all occurrence records and associated values for the explanatory variable. If save.method '"split"' is chosen, the function will save individual “.csv” files for each record with assigned unique ID value in file name. '"split"' method is provided to protect users if internet connection is lost when extracting data for large occurrence datasets.
}
\references{
Aybar, C., Wu, Q., Bautista, L., Yali, R. and Barja, A., 2020. rgee: An R package for interacting with Google Earth Engine. Journal of Open Source Software, 5(51), p.2272.
}
