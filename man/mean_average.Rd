% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalization.R
\name{mean_average}
\alias{mean_average}
\title{Transform channels}
\usage{
mean_average(microarray)
}
\arguments{
\item{microarray}{A tibble containing at least CH1, CH2 and name columns.}
}
\value{
Same table with two new columns: M(ean) and A(verage).
}
\description{
Add (M)ean and (A)verage to a tibble containing CH1 and CH2 columns.
}
