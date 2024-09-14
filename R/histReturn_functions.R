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
#' @param minDate Earliest historical date to use. Default is earliest in set.
#' @param maxDate Latest historical date to use Default is most recent in set.
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
                                minDate = min(sbi$Month),
                                maxDate = max(sbi$Month),
                                seed = NA) {
    # utils::data("sbi") # stocks, bonds and inflation data by month
    if (!is.na(seed)) {
        set.seed(seed)
    }
    allowed_values <- c(1, 2, 3, 4, 5, 6, 12) # required so nConsecMonths can produce 12 months
    nConsecMonths <- as.numeric(match.arg(as.character(nConsecMonths), choices = as.character(allowed_values)))
    sbiSub <- sbi[sbi$Month >= minDate & sbi$Month <= maxDate,]
    if ((nrow(sbiSub) - nConsecMonths + 1) < 1)
        stop(
            paste(
                "MinDate (",
                minDate,
                ") and MaxDate (",
                maxDate,
                ") must be such that there at least nConsecMonths (",
                nConsecMonths,
                ")."
            )
        )
    out <- as.data.frame(t(replicate(
        n, randHistSub(sbiSub, stockWt, nConsecMonths)
    ))
    )

    out$return <- out$return + retAdj
    return(out)
}

#' Get Historical Return Stats
#'
#' @param stockWt stockWt Weighting to stocks. Bonds will be 1 - StockWt.  Enter 60\% as 0.60.
#' @param minDate Earliest historical date to use. Default is earliest in set.
#' @param maxDate Latest historical date to use Default is most recent in set.
#'
#' @return Names numeric vector with StocksGeomRet, BondsGeomRet, PortGeomRet,
#' StocksSD, BondsSD, PortSD, StocksArithRet, BondsArithRet, PortArithRet in
#' decimal.
#' @export
#'
#' @examples \dontrun{getHistoricalReturnStats(0.6)}
getHistoricalReturnStats <- function(stockWt, minDate = min(sbi$Month),
                                     maxDate = max(sbi$Month)) {
    # utils::data("sbi") # stocks, bonds and inflation data by month
    sbiSub <- sbi[sbi$Month >= minDate & sbi$Month <= maxDate,]
    sbiSub$Port <- sbiSub$Stocks * stockWt + sbiSub$Bonds * (1 - stockWt)
    nYrs <- nrow(sbiSub) / 12
    out <- c(StocksGeomRet = prod(1 + sbiSub$Stocks)^(1 / nYrs) - 1,
             BondsGeomRet = prod(1 + sbiSub$Bonds)^(1 / nYrs) - 1,
             PortGeomRet = prod(1 + sbiSub$Port)^(1 / nYrs) - 1,
             StocksSD = stats::sd(sbiSub$Stocks) * sqrt(12),
             BondsSD = stats::sd(sbiSub$Bonds) * sqrt(12),
             PortSD = stats::sd(sbiSub$Port) * sqrt(12),
             Inflation = prod(1 + sbiSub$Inflation)^(1 / nYrs) - 1)
    out1 <- out[1:3] + out[4:6] ^ 2 / 2
    names(out1) <- c("StocksArithRet", "BondsArithRet", "PortArithRet")
    return(c(out, out1))
}

