test_that("calcRandHistReturns", {
    idx <- sbi$Month >= as.Date("2023-01-01") & sbi$Month <= as.Date("2023-12-31")
    sbiSub <- sbi[idx,]
    expPortRet <- prod(sbiSub$Stocks * 0.6 + sbiSub$Bonds * 0.4 + 1)
    expInfl <- prod(sbiSub$Inflation + 1)
    rndReturns <- calcRandHistReturns(1, 0.6, 12, 0,"2023-01-01", "2023-12-31")
    expect_equal(length(rndReturns[1]), 1)
    expect_equal(rndReturns[[1]], expPortRet)
    expect_equal(rndReturns[[2]], expInfl)
    rndReturns <- calcRandHistReturns(1, 0.6, 12, 0.01, "2023-01-01", "2023-12-31")
    expect_equal(rndReturns[[1]], expPortRet + 0.01)
    expect_equal(rndReturns[[2]], expInfl)
    expect_error(calcRandHistReturns(1, 0.6, 12, 0, "2023-01-01", "2023-02-28"))
    rndReturns <- calcRandHistReturns(10, 0.6, 12, 0,"2023-01-01", "2023-12-31")
    expect_equal(length(rndReturns$return), 10)
})

test_that("simulateUsingHistory", {
    # The His1 simulation has no cash flows.  Expected value is ending value
    # times produce of returns.
    simEndingValue <- simTest$Hist1$simulation$portfolioValues[[500]][11]
    simExpValue <- prod(simTest$Hist1$simulation$rateOfReturns[[500]])*simTest$Hist1$startValue
    expect_equal(simEndingValue, simExpValue)
})
