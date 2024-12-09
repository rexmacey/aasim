% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphSim.r
\name{chartSuccessBarComparison}
\alias{chartSuccessBarComparison}
\title{Bar Chart Comparing Success Rates}
\usage{
chartSuccessBarComparison(sim, vs = "T")
}
\arguments{
\item{sim}{Simulation object}

\item{vs}{'T' (default) to use success rates against target value; otherwise
success rates vs zero values will be used.}
}
\value{
Plotly bar chart object
}
\description{
This will examine the sim for elements of class 'simResult'.  The
success rates of those elements will be displayed in a bar chart.
}
\examples{
\dontrun{chartSuccessBarComparison(sim, "T")}
}
