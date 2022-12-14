% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dynamic_proj_covariates.R
\name{dynamic_proj_covariates}
\alias{dynamic_proj_covariates}
\title{Combine explanatory variable rasters into a covariate data frame for each projection date.}
\usage{
dynamic_proj_covariates(
  dates,
  varnames,
  drive.folder = NULL,
  user.email = NULL,
  local.directory = NULL,
  spatial.ext = NULL,
  spatial.res.degrees = NULL,
  resample.method = NULL,
  save.directory = NULL,
  save.drive.folder = NULL
)
}
\arguments{
\item{dates}{a character string, vector of dates in format YYYY-MM-DD.}

\item{varnames}{a character string, the unique names for each explanatory variable.}

\item{drive.folder}{optional; a character string or vector, Google Drive folder or folders to read projection covariate rasters from.}

\item{user.email}{optional; a character string, user email for initialising Google Drive. Required if drive.folder or save.drive.folder used.}

\item{local.directory}{optional; a character string or vector, path to local directory or directories to read projection covariate rasters from.}

\item{spatial.ext}{optional; the spatial extent to crop explanatory variable rasters to. Object of class "Extent", "RasterLayer" or "polygon" or numeric vector listing xmin, xmax, ymin and ymax in order.}

\item{spatial.res.degrees}{optional; a numeric value, the spatial resolution in degrees for projection rasters to be resampled to. Required if spatial.ext given.}

\item{resample.method}{a character string or vector length of varnames, specifying resampling method to use. One of "ngb" and "bilinear". See details for more information.}

\item{save.directory}{optional; a character string, path to local directory to save projection covariate data frames to.}

\item{save.drive.folder}{optional; a character string, Google Drive folder to save projection covariate data frames to.}
}
\description{
Explanatory variable rasters are imported for each projection date, resampled to given spatial resolution and extent and stacked, and then written to a covariate data frame for each projection date.
}
\details{
For each projection date, appropriate rasters for each explanatory variable are imported from a local directory or Google Drive folder. If required, rasters are cropped and resampled to the same spatial extent and resolution. Rasters are then stacked and transformed into a covariate data frame with column names matching the unique explanatory variable names. A data frame of these projection covariates are exported to a local directory or Google Drive folder as ???.csv??? files named as the relevant date in ???YYYY-MM-DD??? format.
Note: if explanatory variable rasters are not of the same spatial resolution and extent, then the function will error.
Resample methods (resample.method) include: "ngb", in which each cell acquires the value of its nearest neighbour cell in the original raster and is often used for categorical variables; and "bilinear", in which the distance-weighted average of the four nearest cells are used to estimate a new cell value and is often used for continuous variables. If only one resample.method is given, but these are more than one explanatory variables, the same resample.method is used for all.

If drive.folder or save.drive.folder arguments are used to download rasters for use or upload function output, then users must have installed R package "googledrive" and initialised Google Drive with valid log-in credentials. The credentials must be given under function argument user.email to initiate the correct Google Drive account. Please follow instructions on https://googledrive.tidyverse.org/.
}
