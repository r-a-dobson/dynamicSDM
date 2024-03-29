% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatiotemp_extent.R
\name{spatiotemp_extent}
\alias{spatiotemp_extent}
\title{Filter species occurrence records by a given spatial and temporal extent.}
\usage{
spatiotemp_extent(
  occ.data,
  temporal.ext,
  spatial.ext,
  prj = "+proj=longlat +datum=WGS84"
)
}
\arguments{
\item{occ.data}{a data frame, with columns for occurrence record co-ordinates and dates with
column names as follows; record longitude as "x", latitude as "y", year as "year", month as
"month", and day as "day".}

\item{temporal.ext}{optional; a character vector, two dates in format "YYYY-MM-DD". First date
represents start of temporal extent and second date represents end of temporal extent for
inclusion.}

\item{spatial.ext}{the spatial extent to filter by. Object from which extent
can be extracted of class \code{SpatExtent}, \code{SpatRaster}, \code{sf} polygon or
numeric vector listing xmin, xmax, ymin and ymax in order.}

\item{prj}{a character string, the coordinate reference system of input \code{occ.data} co-ordinates.
Default is "+proj=longlat +datum=WGS84".}
}
\value{
Returns data frame of occurrence records filtered to the spatial and temporal extent
given.
}
\description{
Function excludes species occurrence records with co-ordinates outside a given spatial extent and
record dates outside a given temporal extent.
}
\section{Spatial extent}{
If \code{spatial.ext} is provided, \code{spatiotemp_extent()} checks whether species occurrence record
co-ordinates are within the given spatial extent of the study (\code{spatial.ext}) and excludes any
outside of this extent.

If \code{spatial.ext} object can be used as a mask by \code{terra::mask()} then the mask is used to filter
records in a more targeted way. If not, then the rectangular extent of the \code{spatial.ext} object
is used.
}

\section{Temporal extent}{
If \code{temporal.ext} is provided, \code{spatiotemp_extent()} checks whether species
occurrence record dates are within the given temporal extent of the study and excludes any outside
of this extent.
}

\examples{
data(sample_filt_data)
data(sample_extent_data)
\donttest{
results <- spatiotemp_extent(occ.data = sample_filt_data,
                            spatial.ext = sample_extent_data,
                            temporal.ext = c("2012-01-01", "2017-01-01"))
}
}
