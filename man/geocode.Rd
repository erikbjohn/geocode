% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocode.R
\docType{package}
\name{geocode}
\alias{geocode}
\alias{geocode-package}
\alias{geocode}
\title{\code{geocode} package}
\usage{
geocode(l.pkg.path, GEO, api.key, source, l.study.extent)
}
\arguments{
\item{l.pkg.path}{list of local data paths (example: pkg.path$points.address points to points.address.rdata)}

\item{GEO}{data.table with one row (missing)}

\item{api.key}{google api.key character}

\item{source}{data source example: mmed.20101215}

\item{l.study.extent}{list of character vectors (zips, cities, states)}
}
\description{
geocode

Creates and loads points.address file
}
\details{
See the Vignette on
}
\keyword{clean}
\keyword{geocode}
\keyword{points}