% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/histReturn_functions.R
\name{calcRandHistReturns}
\alias{calcRandHistReturns}
\title{Calculate Random Returns Using Historical Data}
\usage{
calcRandHistReturns(
  n,
  stockWt,
  nConsecMonths = 12,
  retAdj = 0,
  sbiSub,
  seed = NA
)
}
\arguments{
\item{n}{n Number of 1 year returns and inflation rates to generate}

\item{stockWt}{stockWt Weighting to stocks. Bonds will be 1 - StockWt.  Enter 60\% as 0.60.}

\item{nConsecMonths}{Number of consecutive months.  Default is 12. Must be 1, 2, 3, 4, 6, or 12.}

\item{retAdj}{An adjustment to the return (not inflation).  To reduce all returns by 1\%, enter 0.01.}

\item{sbiSub}{Subset of sbi table restricted by the min and max dates of the simulation}

\item{seed}{Random seed}
}
\value{
n observations of two columns; return and inflation. These have 1 added to them so 4\% will return as 1.04.
}
\description{
Returns a blend of stock and bond returns. The stocks are weighted by
stockWt and bonds by (1 - stockWt). In addition, the inflation rate is
returned.  Both of these have 1 a
added to them so 4\% will return as 1.04.  The resulting returns and inflation
rates will represent one year periods.  There will be n of these
}
\details{
The sbi should include Month, Stocks, Bonds, and Inflation.  Data is in
decimal (enter 8\% as 0.08)

The nConsecutiveMonths must be 1, 2, 3, 4, 6, or 12. For example, if it is 4,
then 3 random draws of 4 consecutive months will be used to produce the one
year result.  The same months are used for the return and inflation.
}
\examples{
\dontrun{calcRandHistReturns(30, 0.6)}
}
