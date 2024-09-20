test_that("simulations", {
  simCH <- simTest$ChronHist1
  expect_equal(round(simCH$simulation$portfolioValues[[1]][31]),4742333)
  expect_equal(round(simCH$simulation$portfolioValues[[500]][31]),246153)
  expect_equal(round(simCH$simulation$portfolioValues[[817]][31]),4278721)
})

test_that("calcRandReturns", {
    rndReturns <- calcRandReturns(10, 0.08, 0, 1, 101)
    expect_equal(length(rndReturns), 10)
    expect_equal(rndReturns[1], 0.08)
    expect_equal(sd(rndReturns), 0)
})

test_that("calcCFSub", {
    # this tests the various startyr and endyr types, default inflation, no inflation
    #p1curage 64, p1ret 65, p1agedeath 86,
    #p2curage 61, p2ret 65, p2agedeath 88
    sim3 <- simTest$sim3
    sim3 <- calcAgesForSimulation(sim3)
    cfMat <- t(sapply(1:10, function(x) calcCFSub(sim3$cf[x,], sim3, sim3$simulation$lengths[1], sim3$simulation$agesDeath1[1], sim3$simulation$agesDeath2[1])))
    expMat <- matrix(0, nrow = 10, ncol = 28)
    expMat[1, 3:5] <- 100 # yr 2 to 4
    expMat[2, 3:5] <- 100 * (1 + sim3$defaultInflation)^(2:4)  # default inflation adjustment
    expMat[3, 6:7] <- -200 # p1curage = 64, cash flows at 69 and 70
    # 4 all zero.  p2curage - 61, cash flows at 58 so in the past
    expMat[5, 2:23] <- -400 # p1 ages 65-86 = index 2:23
    expMat[6, 5:28] <- -500 # p2 ages 65-88 = index 5:28
    expMat[7, 1] <- -700 # p1ret-1:p1ret-1 is ages p1 64:64 index = 1
    expMat[8, 4] <- -800 # p2ret-1:p2ret-1 is ages p2 64:64 index = 4
    expMat[9, 23] <- 900 # 1stDeath p1 in 86-64 = 22 years = index 23 (p2 dies later see next line)
    expMat[10, 28] <- 1000 # 2ndDeath p2 in 88-61 - 27 years = index 28
    expect_equal(cfMat, expMat)
    # need to test the combination of defaultInflationAdj == FALSE, inflation > 0. Similar to row 2 above
    cf <- sim3$cf[2, ]
    cf$defaultInflationAdj <- FALSE
    cfVec <- calcCFSub(cf, sim3, sim3$simulation$lengths[1], sim3$simulation$agesDeath1[1], sim3$simulation$agesDeath2[1])
    expVec <- rep(0, sim3$simulation$lengths[1] + 1)
    expVec[3:5] <- 100 * (1 + cf$inflation)^(2:4)  # inflation adjustment
    expect_equal(cfVec, expVec)
})

test_that("validateCashFlows", {
    for (sim in simTest) {
        expect_equal(validateCashFlows(sim), NULL)
    }
    sim <- simTest[[3]]
    sim$lengthType <- "F"
    expect_error(validateCashFlows(sim))
    sim <- simTest[[3]]
    sim$cf$type[1] <- "x"
    expect_error(validateCashFlows(sim))
})



