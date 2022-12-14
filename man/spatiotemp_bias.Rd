% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatiotemp_bias.R
\name{spatiotemp_bias}
\alias{spatiotemp_bias}
\title{Test for spatial and temporal bias in species occurrence records}
\usage{
spatiotemp_bias(occ.data, temporal.level)
}
\arguments{
\item{occ.data}{a data frame, with columns for occurrence record co-ordinates and dates with column names as follows; record longitude as "x", latitude as "y", year as "year", month as "month", and day as "day".}

\item{temporal.level}{a character string, the time step to test for temporal bias at. One of '"day"' or '"month"', '"year"'. Can be abbreviated.}
}
\value{
Returns list containing chi-squared and t-test results.
}
\description{
Generates plots for visual assessment of spatial and temporal biases in occurrence records. Tests whether the spatiotemporal distribution of records is significantly different from the distribution from random sampling.
}
\details{
To assess temporal sampling bias, the function returns a histogram plot of the frequency distribution of records across the given time step specified by temporal.level. The observed frequency of sampling across the categorical time steps are compared to the distribution expected from random sampling, using a chi-squared test.

To assess spatial sampling bias, the function returns a scatterplot of the spatial distribution of occurrence records to illustrate any spatial clustering. The average nearest neighbour distance of record co-ordinates is then compared to that of records randomly generated at same density using a t-test.

As the spatial bias calculation involves calculation of a distance matrix. To reduce computation time, it is recommended that a sample of large occurrence datasets are input.
}
\examples{
data("sample_occ_data",package="dynamicSDM")
spatiotemp_bias(occ.data = sample_occ_data,temporal.level = "month")
}
