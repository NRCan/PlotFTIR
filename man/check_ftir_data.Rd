% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_ftir_data}
\alias{check_ftir_data}
\title{Check FTIR Data}
\usage{
check_ftir_data(ftir)
}
\arguments{
\item{ftir}{A data.frame of FTIR spectral data including column to be
converted. Can't contain both `absorbance` and `transmittance` column.}
}
\value{
invisible ftir data if ok
}
\description{
Check provided FTIR dataframe is appropriate for manipulation or plotting
Not typically called directly, but as a function in data integrity check process before
further calculation or plotting happens. Sets dataframe attribute "intensity" to
"transmittance" or "absorbance" if not previously set.
}
\keyword{internal}
