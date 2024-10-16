#' Get maximum length of all trials
#'
#' @param sim Simulation object after running simulate function
#' @param simResult Name of item in sim with results (class = 'simResult')
#'
#' @return integer
#' @export
#'
#' @examples \dontrun{getMaxLength(sim)}
getMaxLength <- function(sim, simResult = "simulation") {
    return(max(sim[[simResult]]$lengths))
}

#' Get minimum length of all trials
#'
#' @param sim Simulation object after running simulate function
#' @param simResult Name of item in sim with results (class = 'simResult')
#'
#' @return integer
#' @export
#'
#' @examples \dontrun{getMinLength(sim)}
getMinLength <- function(sim, simResult = "simulation") {
    return(min(sim[[simResult]]$lengths))
}
#' Get Target Values
#'
#' Returns a vector of length = longest trial + 1. It contain the
#' year by year target values. This is useful for comparing ending values of
#' trials to determine success or failure.
#'
#' @param sim Simulation object after running simulate function
#' @param simResult Name of item in sim with results (class = 'simResult')
#'
#' @return vector of values
#' @export
#'
#' @examples \dontrun{getTargetValues(sim)}
getTargetValues <- function(sim, simResult = "simulation") {
  maxLength <- getMaxLength(sim, simResult)
  out <- rep(sim$targetValue, maxLength + 1)
  if (sim$targetValueIsReal) {
      out <- out * (1 + sim$defaultInflation) ^ (0:maxLength)
  }
  return(out)
}

#' Get Terminal Values of Each Trial
#'
#' Returns a vector with a length equal to the number of trials containing the
#' year by year terminal values. This is useful for determining
#' success or failure.
#'
#' @param sim Simulation object after running simulate function
#' @param simResult Name of item in sim with results (class = 'simResult')
#' @param inflationAdjusted TRUE to adjust portfolio values by inflation, otherwise FALSE.
#'
#' @return vector of values
#' @export
#'
#' @examples \dontrun{getTerminalValues(sim)}
getTerminalValues <- function(sim, simResult = "simulation", inflationAdjusted = FALSE) {
    pv <- sim[[simResult]]$portfolioValues
    if (inflationAdjusted) {
        pv <- inflationAdjustPortfolioValues(sim, simResult)
    } else {
        pv <- sim[[simResult]]$portfolioValues
    }
    return(sapply(1:sim[[simResult]]$nTrials, function(x) pv[[x]][sim[[simResult]]$lengths[[x]] + 1]))
}
#' Get Success Statistics
#'
#' Success vs target is defined as a trial with a terminal value >= the
#' terminal value.   Success vs zero (0) is defined as a trial with a terminal
#' value > 0. The list returned by this function includes:
#' vector of success by trial vs target,
#' vector of success by trial vs 0,
#' integer count of success vs target,
#' integet count of success vs 0,
#' numeric percentage success vs target
#' numeric percentage success vs 0
#'
#' @param sim Simulation object after running simulate function
#' @param simResult Name of item in sim with results (class = 'simResult')
#'
#' @return list
#' @export
#'
#' @examples \dontrun{getSuccessStats(sim)}
getSuccessStats <- function(sim, simResult = "simulation") {
    targets <- getTargetValues(sim, simResult)[sim[[simResult]]$lengths + 1]
    terminalValues <- getTerminalValues(sim, simResult)
    out <- list()
    out$successVsTargetByTrial <- terminalValues >= targets
    out$successVs0ByTrial <- terminalValues > 0
    out$vsTargetCount <- sum(out$successVsTarget)
    out$vs0Count <- sum(out$successVs0)
    out$vsTargetPct <- 100 * (out$vsTargetCount / sim[[simResult]]$nTrials)
    out$vs0Pct <- 100 * (out$vs0Count / sim[[simResult]]$nTrials)
    return(out)
}

#' Get Frequency of Lengths
#'
#' Returns a table in a data frame columns for Lengths and the frequency
#' (number of trials) of that length.
#'
#' @param sim Simulation object after running simulate function
#' @param simResult Name of item in sim with results (class = 'simResult')
#'
#' @return data frame
#' @export
#'
#' @examples \dontrun{getFrequencyByLength(sim)}
getFrequencyByLength <- function(sim, simResult = "simulation") {
    out <- as.data.frame(table(sim[[simResult]]$lengths))
    names(out) <- c("Length", "Frequency")
    out$Length <- as.numeric(as.character(out$Length))
    return(out)
}

#' Get Success Statistics by Length of Trial
#'
#' Returns a data frame with one row for each length of a trial. It includes
#' the frequency (number of trials with that length); how many (count) were
#' succcessful vs the target and vs 0; the percentage success rates, the
#' cumulative counts and percentages.
#'
#' @param sim Simulation object after running simulate function
#' @param simResult Name of item in sim with results (class = 'simResult')
#'
#' @return data frame
#' @export
#'
#' @examples \dontrun{getSuccessByLength(sim)}
getSuccessByLength <- function(sim, simResult = "simulation") {
    successStats <- getSuccessStats(sim, simResult)
    out <- getFrequencyByLength(sim, simResult)
    out$SuccessVsTargetCount <- NA
    out$SuccessVs0Count <- NA
    for (i in 1:nrow(out)) {
        l <- out$Length[i]
        idx <- which(sim[[simResult]]$lengths == l)
        out$SuccessVsTargetCount[i] <- sum(successStats$successVsTargetByTrial[idx])
        out$SuccessVs0Count[i] <- sum(successStats$successVs0ByTrial[idx])
    }
    out$SuccessVsTargetPct <- 100 * (out$SuccessVsTargetCount / out$Frequency)
    out$SuccessVs0Pct <- 100 * (out$SuccessVs0Count / out$Frequency)
    out$SuccessVsTargetCumCount <- cumsum(out$SuccessVsTargetCount)
    out$SuccessVs0CumCount <- cumsum(out$SuccessVs0Count)
    out$CumFrequency <- cumsum(out$Frequency)
    out$SuccessVsTargetCumPct <- 100 * out$SuccessVsTargetCumCount / out$CumFrequency
    out$SuccessVs0CumPct <- 100 * out$SuccessVs0CumCount / out$CumFrequency
    return(out)
}

#' Get the Distribution of Value by Year
#'
#' For each year (1 to maximum length of a trail), it returns the number of
#' trials of that length (nTrials) and the quantiles associated with the probs.
#'
#' @param sim Simulation object after running simulate function
#' @param probs numeric vector of probabilities with values [0-1]. The
#' default is c(0.0, 0.05, 0.25, 0.50, 0.75, 0.95, 1.0).
#' @param simResult Name of item in sim with results (class = 'simResult')
#' @param inflationAdjusted TRUE to adjust portfolio values by inflation, otherwise FALSE.
#'
#' @return data frame
#' @export
#'
#' @examples \dontrun{getDistOfValuesByYear(sim)}
getDistOfValuesByYear <- function(sim, probs = c(0.0, 0.05, 0.25, 0.50, 0.75, 0.95, 1.0),
                                  simResult = "simulation", inflationAdjusted = TRUE) {
    maxLength <- getMaxLength(sim, simResult)
    out <- matrix(NA, nrow = 1 + maxLength, ncol = length(probs) + 2)
    out[, 1] <- 0:maxLength
    if (inflationAdjusted) {
        pv <- inflationAdjustPortfolioValues(sim, simResult)
    } else {
        pv <- sim[[simResult]]$portfolioValues
    }
    for (i in 0:maxLength) {
        values <- sapply(1:sim[[simResult]]$nTrials, function(x) pv[[x]][i + 1])
        out[i + 1, 2] <- sim[[simResult]]$nTrials - sum(is.na(values))
        out[i + 1, 3:(length(probs) + 2)] <- stats::quantile(values, probs, na.rm = TRUE)
    }
    out <- as.data.frame(out)
    colnames(out) <- c("Length", "nTrials", paste0("p", 100 * probs))
    return(out)
}


#' Get the Distribution of a Series
#'
#' Intended to produce summary statistics for series in the simulation such as
#' rateOfReturns, agesDeath1 and such.  The geometric average makes sense to
#' include for rateOfReturn and inflation, but not for ages.  If isReturnSeries
#' == TRUE, the returns and standard deviation will be showin in percent, not
#' decimal.
#'
#' @param x Series for which to get the distribution
#' @param probs numeric vector of probabilities with values in [0, 1]
#' @param isReturnSeries boolean if a return series. Will include geometric average
#' and subtract 1 from the probs and summary items.
#'
#' @return list with nObs - the number of observations;
#' probs - the quanitles associated with the probs;
#' summary - results of the summary function;
#' sd - the (standard deviation); and,
#' geomAvg the geometric average if isReturnSeries is TRUE
#' @export
#'
#' @examples \dontrun{getDistribution(unlist(sim$simulation$rateOfReturns) - 1,
#' includeGeomAvg = TRUE)}
getDistribution <- function(x,
                            probs = c(0.0, 0.05, 0.25, 0.50, 0.75, 0.95, 1.0),
                            isReturnSeries = FALSE) {
    out <- list()
    out$nObs <- length(x)
    out$probs <- stats::quantile(x, probs, na.rm = TRUE)
    out$summary <- summary(x)
    out$sd <- stats::sd(x, na.rm = TRUE)
    if (isReturnSeries) {
        out$geomAvg <- 100 * (exp(mean(log(x))) - 1)
        out$probs <- out$probs * 100 - 100
        out$summary <- out$summary * 100 - 100
        out$sd <- out$sd * 100


    }
    out$dfProbs <- data.frame(Percentile = names(out$probs),
                              Return = out$probs, row.names = NULL)
    out$dfSummary <- data.frame(Statistic = names(out$summary),
                                Return = as.numeric(out$summary), row.names = NULL)
    return(out)
}

#' Identify the Items in a List with a Class Name
#'
#' This will identify all the items in a list that are of a class named
#' nameClass.  The result is a list with two items.  Indices contains the
#' indices (locations) of the items, and names contains the name.
#'
#' @param sim Simulation object
#' @param nameClass name of class
#'
#' @return list with two items. See description.
#' @export
#'
#' @examples \dontrun{whichItemsClassName(sim, "simResult")}
whichItemsClassName <- function(sim, nameClass = "simResult") {
    out <- list()
    out$indices <- which(sapply(sim, function(x) nameClass %in% class(x)))
    out$names <- names(sim[out$indices])
    return(out)
}

#' Inflation Adjust Portfolio Values
#'
#' @param sim simulation object
#' @param simResult Name of item in sim with results (class = 'simResult')
#'
#' @return List same size as portfolioValues
#' @export
#'
#' @examples \dontrun{inflationAdjustPortfolioValues(sim, simResult)}
inflationAdjustPortfolioValues <- function(sim, simResult) {
    pv <- sim[[simResult]]$portfolioValues
    infl <- sim[[simResult]]$inflationHist
    if (length(infl) == 0) {
        maxLen <- getMaxLength(sim, simResult)
        disc <- (1 + sim$defaultInflation) ^ (0:maxLen)
        out <- lapply(pv, function(x) x / disc[1:length(x)])
    } else {
        if (length(pv) != length(infl)) stop("Length of portfolioValues != length of inflationHist")
        disc <- lapply(infl, function(x) cumprod(c(1, x)))
        out <- lapply(1:length(pv), function(x) pv[[x]]/disc[[x]])
    }
    return(out)
}
