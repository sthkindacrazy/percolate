% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{extract_boards}
\alias{extract_boards}
\title{extract_boards}
\usage{
extract_boards(lines)
}
\arguments{
\item{lines}{lines extracted from the file}
}
\value{
board class objects, if lines do not meet these specifications, return NA
}
\description{
extract_boards
}
\examples{
lines <- readLines(file)
lines <- lines \%>\% trimws() \%>\% .[. != ""]  \%>\% paste(collapse = ",")
extract_boards(lines)
}
