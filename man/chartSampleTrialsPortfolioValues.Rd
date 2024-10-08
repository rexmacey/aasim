% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphSim.r
\name{chartSampleTrialsPortfolioValues}
\alias{chartSampleTrialsPortfolioValues}
\title{Chart a Sample of the Trials (Wealth Over Time) Given a Vector of Indices}
\usage{
chartSampleTrialsPortfolioValues(
  sim,
  sampleIndex,
  logScale = TRUE,
  simResult = "simulation"
)
}
\arguments{
\item{sim}{Simulation object}

\item{sampleIndex}{Vector of indices of the trials to plot.}

\item{logScale}{True to display the y-axis in log format}

\item{simResult}{Name of item in sim with results (class = 'simResult')}
}
\value{
Chart object (plotly)
}
\description{
This will produce a chart with a length(sampleIndex) number of lines representing
individual trials. The sampleIndex is a vector with the numbers of the trials
to display.  For example, to plot the first hundred trials, sampleIndex would
equal 1:100.
}
\examples{
\dontrun{chartSampleTrialsPortfolioValues(sim, sampleIndex, logScale)}
}
