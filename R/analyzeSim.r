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

#' Compare Rates of Return
#'
#' Compares the rates of return from different simulations
#'
#' @param sim Simulation object
#'
#' @return data frame with the Method, GeomReturn, StdDev, and nObs
#' @export
#'
#' @examples \dontrun{compareRateOfReturns(sim)}
compareRateOfReturns <- function(sim) {
    simResults <- sim[sapply(sim, function(x) "simResult" %in% class(x))]

    out <- data.frame(Method = sapply(simResults, function(x) x$returnGeneratingMethod),
                      'GeomReturn' = NA,
                      'StdDev' = NA,
                      nObs = NA,
                      row.names = NULL)
    for (i in 1:length(simResults)) {
        simulationReturnStats <- getDistribution(unlist(simResults[[i]]$rateOfReturns), ,isReturnSeries = TRUE)
        out[i, 'GeomReturn'] <- round(simulationReturnStats$geomAvg, 1)
        out[i, 'StdDev'] <- round(simulationReturnStats$sd, 1)
        out[i, "nObs"] <- prettyNum(simulationReturnStats$nObs, big.mark = ",")
    }
    return(out)
}

#' Make a Table Representing a Single Trial
#'
#' This function exists to produce a table reflecting the details of a single
#' trial.   If the trial number is not supplied, a trial is selected with a
#' length within the 50\% confidence interval and that is closest to the
#' median terminal value.
#'
#' @param sim A simulation object
#' @param simResult Name of item in sim with results (class = 'simResult')
#' @param trialNum Optional - the trial number to create a table for
#'
#' @return a data frame with the Year, PortfolioValue, CashFlow, Return, and Inflation
#' @export
#'
#' @examples \dontrun{makeTrialTable(sim, simResult, trialNum)}
makeTrialTable <- function(sim, simResult, trialNum) {
    if (missing(trialNum)) {  # select a representative trial
        idx <- sim[[simResult]]$lengths >= quantile(sim[[simResult]]$lengths, 0.25) &
            sim[[simResult]]$lengths <= quantile(sim[[simResult]]$lengths, 0.75) # length in 50% CI
        tv <- getTerminalValues(sim, simResult)
        trialNum <- which(idx)[which.min(abs(tv[idx] - median(tv)))]  # terminal value closest to median
    }
    trialLength <- sim[[simResult]]$lengths[trialNum]
    nLives <- nPersons.sim(sim)

    out <- data.frame(Year = 0:trialLength)
    if (nLives >= 1) {
        anb <- calculate_age_nearest_birthday(sim$persons[[1]]$birthDate, asOfDate = sim$asOfDate)
        if (sim$persons[[1]]$initials == "") {
            cname <- "AgeP1"
        } else {
            cname <- paste0("Age", sim$persons[[1]]$initials)
        }
        out[[cname]] <- anb:(anb + trialLength)
    }
    if (nLives >= 2) {
        anb <- calculate_age_nearest_birthday(sim$persons[[2]]$birthDate, asOfDate = sim$asOfDate)
        if (sim$persons[[2]]$initials == "") {
            cname <- "AgeP2"
        } else {
            cname <- paste0("Age", sim$persons[[2]]$initials)
        }
        out[[cname]] <- anb:(anb + trialLength)
    }
    out[["PortfolioValue"]] <- prettyNum(sim[[simResult]]$portfolioValues[[trialNum]], big.mark = ",")
    out[["CashFlow"]] <- prettyNum(sim[[simResult]]$cashFlows[[trialNum]], big.mark = ",", drop0trailing = FALSE)
    out[["Return"]] <- c(NA, round(100 * sim[[simResult]]$rateOfReturns[[trialNum]] - 100, 2))
    if (length(sim[[simResult]]$inflationHist) == 0) {
        out[["Inflation"]] <- c(NA, rep(100 * sim$defaultInflation, trialLength))
    } else {
        out[["Inflation"]] <- c(NA, round(100 * sim[[simResult]]$inflationHist[[trialNum]] - 100, 2))
    }
    return(out)
}

#' Find Trial by Quantile
#'
#' This function returns the trial number (index) with
#' a terminal value that matches the quantile (qTile).  So, qTile == 0 will
#' identify which trial has the lowest value, 1 the highest, and 0.5 the median.
#' If more than one trial has the same terminal value, the function returns the
#' trial that has that terminal value earliest.  So, if the quantile of the
#' terminal values is 0, and there are multiple trials that terminate with 0,
#' the function returns the trial which ends with 0 earliest.
#'
#' @param sim Simulation object.
#' @param simResult Name of item in sim with results (class = 'simResult')
#' @param qTile Quantile (0 - 1) of terminal value
#'
#' @return Integer representing which trial terminates with the qTile value.
#' @export
#'
#' @examples \dontrun{findTrialByQuantile(sim, simResult, qTile)}
findTrialByQuantile <- function(sim, simResult, qTile) {
    tv <- getTerminalValues(sim, simResult)
    qTileTV <- quantile(tv, qTile)
    idxVec <- which(tv == qTileTV)
    idx <- which.min(sapply(idxVec, function(x) which(sim[[simResult]]$portfolioValues[[x]] == qTileTV)[1]))
    return(idxVec[1])
}

#' Get Start and End Dates of Chronological Trials By Index
#'
#' Given a vector of indices, this will return the starting and ending months of those trials.
#' So getDatesOfChronologicalTrial(sim, c(3,5)) would return the starting and ending months
#' for the 3rd and 5th trials.
#'
#' @param sim Simulation object
#' @param idxVec Vector of indices to a Chronological simulation
#'
#' @return list with startDates and endDates vectors
#' @export
#'
#' @examples \dontrun{getDatesOfChronologicalTrial(sim, idxVec)}
getDatesOfChronologicalTrial <- function(sim, idxVec) {
    sbiSub <- sbi[sbi$Month >= sim$minDate & sbi$Month <= sim$maxDate,]
    out <- list()
    out$startDates <- sbiSub$Month[idxVec]
    out$endDates <- sbiSub$Month[idxVec + sim$length * 12 - 1]
    return(out)
}

#' Growth of $1
#'
#' Calculates the growth of $1 from the rates of returns in the
#' trials specified by idxVec.
#'
#' @param sim Simulation object.
#' @param simResult Name of item in sim with results (class = 'simResult')
#' @param idxVec Vector of one or more indices representing a trial.
#'
#' @return Data frame. The number of row will be one more than the longest length of the trials. The
#' first column is the year and each subsequent column is the growth of $1 for each element in idxVec.
#' @export
#'
#' @examples \dontrun{getGrowth1Dollar(sim, simResult, c(1, 2, 3))}
getGrowth1Dollar <- function(sim, simResult, idxVec) {
    returnLst <- sim[[simResult]]$rateOfReturns[idxVec]
    returnLst <- lapply(returnLst, function(x) c(1, cumprod(x)))
    max_length <- max(sapply(returnLst, length))
    df <- t(do.call(rbind, lapply(returnLst, function(x) {
        length(x) <- max_length
        x})))
    df <- data.frame(cbind(0:(max_length - 1), df))
    # View the resulting data frame
    return(df)
}
