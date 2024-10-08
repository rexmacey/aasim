% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age_functions.r
\name{days_since_last_birthday}
\alias{days_since_last_birthday}
\title{Calculate days since last birthday}
\usage{
days_since_last_birthday(birthdate, asOfDate = Sys.Date())
}
\arguments{
\item{birthdate}{Birthdate in character (YYYY-MM-DD) or as class date.}

\item{asOfDate}{The date to calculate the age. Default is the system date.}
}
\value{
days as numeric
}
\description{
Calculate days since last birthday
}
\examples{
\dontrun{days_since_last_birthday("1950-01-01")}
}
