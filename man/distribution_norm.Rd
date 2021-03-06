% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalization.R
\name{distribution_norm}
\alias{distribution_norm}
\title{Distribution normalization}
\usage{
distribution_norm(microarrays)
}
\arguments{
\item{microarrays}{A tibble containing M and name columns.}
}
\value{
A new tibble with "number" and M columns, M column
is normalized.
}
\description{
Sort all values in ascending order, build a mean value for each
position in the order, so distributions of all experiments become
the same.
}
