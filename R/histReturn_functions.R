#' Subroutine (helper) to calculate random returns using historical data
#'
#' @param sbiSub tibble or data frame with historical data
#' @param stockWt Weighting to stocks. Bonds will be 1 - StockWt
#' @param nConsecMonths  Number of consecutive months (default is 12). Must be 1,
#'  2, 3, 4, 6, or 12.
#'
#' @return Two element vector with return and inflation rates + 1
#'
#' @examples \dontrun{randHistSub(sbiSub, 0.6)}
randHistSub <- function(sbiSub, stockWt, nConsecMonths = 12) {
    idx <- sample(nrow(sbiSub) - nConsecMonths + 1, 12 / nConsecMonths, replace = TRUE)
    idx12 <- as.vector(sapply(idx, seq, length.out = nConsecMonths)) # there should be 12 index
    out <- c(
        return = prod(sbiSub$Stocks[idx12] * stockWt + sbiSub$Bonds[idx12] * (1 - stockWt) + 1),
        inflation = prod(sbiSub$Inflation[idx12] + 1)
    )
    return(out)
}

#' Calculate Random Returns Using Historical Data
#'
#' Returns a blend of stock and bond returns. The stocks are weighted by
#' stockWt and bonds by (1 - stockWt). In addition, the inflation rate is
#' returned.  Both of these have 1 a
#' added to them so 4\% will return as 1.04.  The resulting returns and inflation
#' rates will represent one year periods.  There will be n of these
#'
#' The sbi should include Month, Stocks, Bonds, and Inflation.  Data is in
#' decimal (enter 8\% as 0.08)
#'
#' The nConsecutiveMonths must be 1, 2, 3, 4, 6, or 12. For example, if it is 4,
#' then 3 random draws of 4 consecutive months will be used to produce the one
#' year result.  The same months are used for the return and inflation.
#'
#' @param n n Number of 1 year returns and inflation rates to generate
#' @param stockWt stockWt Weighting to stocks. Bonds will be 1 - StockWt.  Enter 60\% as 0.60.
#' @param nConsecMonths Number of consecutive months.  Default is 12. Must be 1, 2, 3, 4, 6, or 12.
#' @param retAdj An adjustment to the return (not inflation).  To reduce all returns by 1\%, enter 0.01.
#' @param sbiSub Subset of sbi table restricted by the min and max dates of the simulation
#' @param seed Random seed
#'
#' @return n observations of two columns; return and inflation. These have 1 added to them so 4\% will return as 1.04.
#' @export
#'
#' @examples \dontrun{calcRandHistReturns(30, 0.6)}
calcRandHistReturns <- function(n,
                                stockWt,
                                nConsecMonths = 12,
                                retAdj = 0,
                                sbiSub,
                                seed = NA) {

    if (!is.na(seed)) {
        set.seed(seed)
    }
    # if (n < 1) return(c(return = NA, inflation = NA))
    if (n < 1) n <- 1
    allowed_values <- c(1, 2, 3, 4, 5, 6, 12) # required so nConsecMonths can produce 12 months
    nConsecMonths <- as.numeric(match.arg(as.character(nConsecMonths), choices = as.character(allowed_values)))

    if ((nrow(sbiSub) - nConsecMonths + 1) < 1)
        stop(
            paste0("(nrow(sbiSub) - nConsecMonths + 1) < 1 is TRUE.
                   nrow(sbiSub)=", nrow(sbiSub),
                   "nConsecMonths=", nConsecMonths)
        )
    out <- as.data.frame(t(replicate(
        n, randHistSub(sbiSub, stockWt, nConsecMonths)
    ))
    )

    out$return <- out$return + retAdj
    return(out)
}

#' Get Historical Returns and Inflation Statistics
#'
#' @param stockWt Numeric vector containing weightings to stocks.  Weight to
#' bonds will be 1 - stockWt.  Enter as decimal so 60\% is 0.60. seq(0, 1, 0.1)
#' would produce the statistics for portfolios with weights to stocks of 0\% to
#' 100\% in 10\% increments.
#' @param minDate Earliest historical date to use. Default is earliest in set.
#' @param maxDate Latest historical date to use. Default is most recent in set.
#'
#' @return A list
#' @return A list with four items.  portfolio is a data frame with a row for each
#' stockWt and columns: StockWt, GeomReturn, ArithReturn, and StdDev.  The
#' second item is the inflation rate.  These in decimal, not percent.  The
#' minDate and maxDate are returned.
#' @export
#'
#' @examples \dontrun{getHistoricalReturnStats(seq(0, 1, 0.1))}
getHistoricalReturnStats <- function(stockWt,
                                     minDate = min(sbi$Month),
                                     maxDate = max(sbi$Month)) {
    sbiSub <- sbi[sbi$Month >= minDate & sbi$Month <= maxDate, ]
    nYrs <- max(1, nrow(sbiSub) / 12)
    out <- list()
    nPortfolios <- length(stockWt)
    out$portfolio <- data.frame(
        StockWt = stockWt,
        GeomReturn = numeric(nPortfolios),
        ArithReturn = numeric(nPortfolios),
        StdDev = numeric(nPortfolios)
    )
    for (i in 1:length(stockWt)) {
        wt <- stockWt[i]
        sbiSub$Port <- sbiSub$Stocks * wt + sbiSub$Bonds * (1 - wt)
        out$portfolio[i, "GeomReturn"] <- prod(1 + sbiSub$Port) ^ (1 / nYrs) - 1
        out$portfolio[i, "StdDev"] <- stats::sd(sbiSub$Port) * sqrt(12)
        out$portfolio[i, "ArithReturn"] <- out$portfolio[i, "GeomReturn"] +
            out$portfolio[i, "StdDev"] ^ 2 / 2
    }
    out$inflation = prod(1 + sbiSub$Inflation) ^ (1 / nYrs) - 1
    out$minDate <- minDate
    out$maxDate <- maxDate
    return(out)
}
#' Calculate Chronological History
#'
#' Generates sequential returns and inflation rates using the sbi table.
#' It will start with the startIdx element of sbiSub and generate n annual
#' returns using consecutive months.
#'
#' @param startIdx Starting index
#' @param n Number of years to generate returns and inflation.
#' @param sbiSub A subset of the sbi table limited by sim$minDate and sim$maxDate.
#' @param stockWt Weighting to stocks. Bonds will be 1 - StockWt.  Enter 60\% as 0.60.
#'
#' @return data frame with n rows and two columns; return and inflation. These have 1 added to them so 4\% will return as 1.04.
#' @export
#'
#' @examples \dontrun{calcChronologicalHist(1, 10, sbiSub, 0.6)}
calcChronologicalHist <- function(startIdx, n, sbiSub, stockWt) {
    endIdx <- startIdx + n * 12 - 1
    out <- data.frame(
        return = sapply(seq(startIdx, endIdx, 12), function(x) (prod(1 + sbiSub$Stocks[x:(x + 11)]) - 1) * stockWt +
                                                               (prod(1 + sbiSub$Bonds[x:(x + 11)]) - 1) * (1 - stockWt)),
        inflation = sapply(seq(startIdx, endIdx, 12), function(x) prod(1 + sbiSub$Inflation[x:(x + 11)])))
    return(out)
}

#' Get the Min and Max SBI Dates
#'
#' Returns a list with the first and last dates of the months in the
#' Stock, Bonds, Inflation (SBI) table.
#'
#' @return list with minDate and maxDate
#' @export
#'
#' @examples \dontrun{getSBIDateRange()}
getSBIDateRange <- function() {
    out <- list()
    out$minDate = min(sbi$Month)
    out$maxDate = max(sbi$Month)
    return(out)
}
