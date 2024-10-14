#' Runs a simulation on a simulation object
#'
#' @param sim sim(ulation) object
#'
#' @return A list with 6 items.
#'   lengths is a vector containing the number of years of each trial.
#'   agesDeath1 is a vector of the ages of the deaths of person 1.
#'   agesDeath2 is a vector of the ages of the deaths of person 2.
#'   cashFlows is a list, each item is a vector of the cashFlows of each trial.
#'   portfolioValues is a list each item is a vector of portfolio values for each trial.
#'   ratesOfReturns is a list each item is a vector of 1 + the annual rate of return
#' @export
#'
#' @examples \dontrun{simulateWealth(sim)}
simulateWealth <- function(sim) {
    result <- initializeSimulation(sim)
    sim <- result$sim
    sbiSub <- result$sbiSub
    out <- result$out

    if (sim$returnGeneratorMethod == "C") {
        if (nPersons.sim(sim) >= 1) {
            idx <- which(sim$cf$startType %in% c("p1death", "p2death", "1stdeath", "2nddeath"))
            sim$cf[idx, "startType"] <- "yr"
            sim$cf[idx, "start"] <- sim$length
            idx <- which(sim$cf$endType %in% c("p1death", "p2death", "1stdeath", "2nddeath"))
            sim$cf[idx, "endType"] <- "yr"
            sim$cf[idx, "end"] <- sim$length
        }
    }

    for (iTrial in 1:out$nTrials) {
        out$portfolioValues[[iTrial]] <- numeric(out$lengths[iTrial] + 1)
        if (sim$returnGeneratorMethod == "S") {
            out$rateOfReturns[[iTrial]] <- 1 + calcRandReturns(out$lengths[iTrial], sim$ror, sim$stdDev, 1)
            histInflation <- 0
        } else if (sim$returnGeneratorMethod == "H") {
            temp <- calcRandHistReturns(out$lengths[iTrial], sim$stockWt, sim$nConsecMonths, sim$retAdj, sbiSub)
            out$rateOfReturns[[iTrial]] <- temp$return
            out$inflationHist[[iTrial]] <- temp$inflation
            histInflation <- out$inflationHist[[iTrial]]
        } else if (sim$returnGeneratorMethod == "C") {
            temp <- calcChronologicalHist(iTrial, out$lengths[iTrial], sbiSub, sim$stockWt)
            out$rateOfReturns[[iTrial]] <- temp$return + 1
            out$inflationHist[[iTrial]] <- temp$inflation
            histInflation <- out$inflationHist[[iTrial]]
        } else {
            stop(paste("Invalid randReturnTYpe:", sim$returnGeneratorMethod, "Should be S, H or C."))
        }

        out$cashFlows[[iTrial]] <- calcCF(sim,
                            out$lengths[iTrial],
                            out$agesDeath1[iTrial],
                            out$agesDeath2[iTrial],
                            histInflation)
        out$cashFlows[[iTrial]][1] <- sim$startValue + out$cashFlows[[iTrial]][1]
        out$irr[iTrial] <- calcIRR(out$cashFlows[[iTrial]])
        if (sim$returnGeneratorMethod == "S" & !is.na(out$irr[iTrial])) {
            out$probSuccess[iTrial] <- AA_Prob(out$irr[iTrial], sim$ror, sim$stdDev, out$lengths[iTrial])
        }
        out$portfolioValues[[iTrial]][1] <- out$cashFlows[[iTrial]][1]
        if (out$lengths[iTrial] < 1) next
        for (i in 2:(out$lengths[iTrial] + 1)) {
            out$portfolioValues[[iTrial]][i] <- out$portfolioValues[[iTrial]][i - 1] * out$rateOfReturns[[iTrial]][i - 1] + out$cashFlows[[iTrial]][i]
            if (out$portfolioValues[[iTrial]][i] <= 0) {
                out$portfolioValues[[iTrial]][i] <- 0
                break
            }
        }
    }
    class(out) <- c("simResult", class(out))
    return(out)
}

#' Helper function for Simulation methods
#'
#' @param sim Simulation object
#'
#' @return list with modified sim, sbiSub, and out list
#'
#' @examples \dontrun{initializeSimulation(sim)}
initializeSimulation <- function(sim) {
    out <- list()
    sbiSub <- sbi[sbi$Month >= sim$minDate & sbi$Month <= sim$maxDate,]
    if (sim$returnGeneratorMethod == "C") {
        out$nTrials <- nrow(sbiSub) - sim$length * 12 + 1
    } else {
        out$nTrials <- sim$nTrials
    }
    set.seed(sim$seed)
    sim <- calcAgesForSimulation(sim) # adds curAge (nearest birthday) to person(s)
    bknTrials <- sim$nTrials
    sim$nTrials <- out$nTrials
    out <- c(out, getHorizons.sim(sim))
    sim$nTrials <- bknTrials
    out$portfolioValues <- list()
    out$rateOfReturns <- list()
    out$inflationHist <- list()
    out$cashFlows <- list()

    out$irr <- numeric(out$nTrials)
    out$probSuccess <- as.numeric(rep(NA, out$nTrials)) # only with RGM = "S"
    out$runDate <- Sys.Date()
    return(list(sim = sim, sbiSub = sbiSub, out = out))
}

#' Runs simulations on a simulation object
#'
#' Uses the Statistical, Historical and Chronological methods to generate returns.
#'
#' @param sim sim(ulation) object
#'
#' @return 3 lists (simS, simH, and simC) each with 6 items.
#'   lengths is a vector containing the number of years of each trial.
#'   agesDeath1 is a vector of the ages of the deaths of person 1.
#'   agesDeath2 is a vector of the ages of the deaths of person 2.
#'   cashFlows is a list, each item is a vector of the cashFlows of each trial.
#'   portfolioValues is a list each item is a vector of portfolio values for each trial.
#'   ratesOfReturns is a list each item is a vector of 1 + the annual rate of return
#' @export
#'
#' @examples \dontrun{simulateSHC(sim)}
simulateSHC <- function(sim) {
    out <- sim
    out$simS <- simulateMethodS(sim)
    out$simH <- simulateMethodH(sim)
    out$simC <- simulateMethodC(sim)
    return(out)
}

#' Simulate using the Statistical Return Generating Method
#'
#' @param sim sim(ulation) object
#'
#' @return A list with 6 items.
#'   lengths is a vector containing the number of years of each trial.
#'   agesDeath1 is a vector of the ages of the deaths of person 1.
#'   agesDeath2 is a vector of the ages of the deaths of person 2.
#'   cashFlows is a list, each item is a vector of the cashFlows of each trial.
#'   portfolioValues is a list each item is a vector of portfolio values for each trial.
#'   ratesOfReturns is a list each item is a vector of 1 + the annual rate of return
#' @export
#'
#' @examples \dontrun{simulateMethodS(sim)}
simulateMethodS <- function(sim) {
    sim$returnGeneratorMethod <- "S"
    result <- initializeSimulation(sim)
    sim <- result$sim
    sbiSub <- result$sbiSub
    out <- result$out
    for (iTrial in 1:sim$nTrials) {
        out$portfolioValues[[iTrial]] <- numeric(out$lengths[iTrial] + 1)
        out$rateOfReturns[[iTrial]] <- 1 + calcRandReturns(out$lengths[iTrial], sim$ror, sim$stdDev, 1)
        histInflation <- 0
        out$cashFlows[[iTrial]] <- calcCF(sim,
                                          out$lengths[iTrial],
                                          out$agesDeath1[iTrial],
                                          out$agesDeath2[iTrial],
                                          histInflation)
        out$cashFlows[[iTrial]][1] <- sim$startValue + out$cashFlows[[iTrial]][1]
        out$irr[iTrial] <- calcIRR(out$cashFlows[[iTrial]])
        out$probSuccess[iTrial] <- AA_Prob(out$irr[iTrial], sim$ror, sim$stdDev, out$lengths[iTrial])
        out$portfolioValues[[iTrial]][1] <- out$cashFlows[[iTrial]][1]
        if (out$lengths[iTrial] < 1) next
        for (i in 2:(out$lengths[iTrial] + 1)) {
            out$portfolioValues[[iTrial]][i] <- out$portfolioValues[[iTrial]][i - 1] * out$rateOfReturns[[iTrial]][i - 1] + out$cashFlows[[iTrial]][i]
            if (out$portfolioValues[[iTrial]][i] <= 0) {
                out$portfolioValues[[iTrial]][i] <- 0
                break
            }
        }
    }
    class(out) <- c("simResult", "methodS", class(out))
    return(out)
}

#' Simulate using the Historical Random Return Generating Method
#'
#' @param sim sim(ulation) object
#'
#' @return A list with 6 items.
#'   lengths is a vector containing the number of years of each trial.
#'   agesDeath1 is a vector of the ages of the deaths of person 1.
#'   agesDeath2 is a vector of the ages of the deaths of person 2.
#'   cashFlows is a list, each item is a vector of the cashFlows of each trial.
#'   portfolioValues is a list each item is a vector of portfolio values for each trial.
#'   ratesOfReturns is a list each item is a vector of 1 + the annual rate of return
#' @export
#'
#' @examples \dontrun{simulateMethodH(sim)}
simulateMethodH <- function(sim) {
    sim$returnGeneratorMethod <- "H"
    result <- initializeSimulation(sim)
    sim <- result$sim
    sbiSub <- result$sbiSub
    out <- result$out

    for (iTrial in 1:sim$nTrials) {
        out$portfolioValues[[iTrial]] <- numeric(out$lengths[iTrial] + 1)
        temp <- calcRandHistReturns(out$lengths[iTrial], sim$stockWt, sim$nConsecMonths, sim$retAdj, sbiSub)
        out$rateOfReturns[[iTrial]] <- temp$return
        out$inflationHist[[iTrial]] <- temp$inflation
        histInflation <- out$inflationHist[[iTrial]]
        out$cashFlows[[iTrial]] <- calcCF(sim,
                                          out$lengths[iTrial],
                                          out$agesDeath1[iTrial],
                                          out$agesDeath2[iTrial],
                                          histInflation)
        out$cashFlows[[iTrial]][1] <- sim$startValue + out$cashFlows[[iTrial]][1]
        out$irr[iTrial] <- calcIRR(out$cashFlows[[iTrial]])
        out$portfolioValues[[iTrial]][1] <- out$cashFlows[[iTrial]][1]
        if (out$lengths[iTrial] < 1) next
        for (i in 2:(out$lengths[iTrial] + 1)) {
            out$portfolioValues[[iTrial]][i] <- out$portfolioValues[[iTrial]][i - 1] * out$rateOfReturns[[iTrial]][i - 1] + out$cashFlows[[iTrial]][i]
            if (out$portfolioValues[[iTrial]][i] <= 0) {
                out$portfolioValues[[iTrial]][i] <- 0
                break
            }
        }
    }
    class(out) <- c("simResult", "methodH", class(out))
    return(out)
}

#' Simulate using the Chronological Return Generating Method
#'
#' @param sim sim(ulation) object
#'
#' @return A list with 6 items.
#'   lengths is a vector containing the number of years of each trial.
#'   agesDeath1 is a vector of the ages of the deaths of person 1.
#'   agesDeath2 is a vector of the ages of the deaths of person 2.
#'   cashFlows is a list, each item is a vector of the cashFlows of each trial.
#'   portfolioValues is a list each item is a vector of portfolio values for each trial.
#'   ratesOfReturns is a list each item is a vector of 1 + the annual rate of return
#' @export
#'
#' @examples \dontrun{simulateMethodC(sim)}
simulateMethodC <- function(sim) {
    sim$returnGeneratorMethod <- "C"

    bkLengthType = sim$lengthType
    sim$lengthType <- "F"
    result <- initializeSimulation(sim)
    sim <- result$sim
    sbiSub <- result$sbiSub
    out <- result$out
    if (nPersons.sim(sim) >= 1) {
        idx <- which(sim$cf$startType %in% c("p1death", "p2death", "1stdeath", "2nddeath"))
        sim$cf[idx, "startType"] <- "yr"
        sim$cf[idx, "start"] <- sim$length
        idx <- which(sim$cf$endType %in% c("p1death", "p2death", "1stdeath", "2nddeath"))
        sim$cf[idx, "endType"] <- "yr"
        sim$cf[idx, "end"] <- sim$length
    }
    for (iTrial in 1:out$nTrials) {
        out$portfolioValues[[iTrial]] <- numeric(out$lengths[iTrial] + 1)
        temp <- calcChronologicalHist(iTrial, out$lengths[iTrial], sbiSub, sim$stockWt)
        out$rateOfReturns[[iTrial]] <- temp$return + 1
        out$inflationHist[[iTrial]] <- temp$inflation
        histInflation <- out$inflationHist[[iTrial]]

        out$cashFlows[[iTrial]] <- calcCF(sim,
                                          out$lengths[iTrial],
                                          out$agesDeath1[iTrial],
                                          out$agesDeath2[iTrial],
                                          histInflation)
        out$cashFlows[[iTrial]][1] <- sim$startValue + out$cashFlows[[iTrial]][1]
        out$irr[iTrial] <- calcIRR(out$cashFlows[[iTrial]])
        out$portfolioValues[[iTrial]][1] <- out$cashFlows[[iTrial]][1]
        if (out$lengths[iTrial] < 1) next
        for (i in 2:(out$lengths[iTrial] + 1)) {
            out$portfolioValues[[iTrial]][i] <- out$portfolioValues[[iTrial]][i - 1] * out$rateOfReturns[[iTrial]][i - 1] + out$cashFlows[[iTrial]][i]
            if (out$portfolioValues[[iTrial]][i] <= 0) {
                out$portfolioValues[[iTrial]][i] <- 0
                break
            }
        }
    }
    sim$lengthType <- bkLengthType
    out$nTrials <- nrow(sbiSub) - sim$length * 12 + 1
    class(out) <- c("simResult", "methodC", class(out))
    return(out)
}
#' Get Time Horizons
#'
#' This will return the lengths of the cash flow and portfolio values vectors.
#' Each of the vectors is 1 longer than the number of years in a trial because
#' the first element represents the start (year 0).
#'
#' @param sim Sim(ulation) object
#'
#' @return List with three vectors. lengths is number of years + 1 in each trial; agesDeath1 are the ages at which the first person passes,
#' agesDeath2 are the ages at which the second person passes.
#'
#' @examples \dontrun{getHorizons(sim1)}
getHorizons.sim <- function(sim) {
    out <- list()
    out$lengths <- NA
    out$agesDeath1 <- NA
    out$agesDeath2 <- NA
    if (toupper(substr(sim$lengthType, 1, 1) == "F")) {
        # used fixed length
        out$lengths <- rep(sim$length, sim$nTrials)
        if (nPersons.sim(sim) >= 1) {
            out$agesDeath1 <- rep(sim$persons[[1]]$curAge + sim$length, sim$nTrials)
        }
        if (nPersons.sim(sim) >= 2) {
            out$agesDeath2 <- rep(sim$persons[[2]]$curAge + sim$length, sim$nTrials)
        }
        return(out)
    }
    if (nPersons.sim(sim) == 0) {
        # also use fixed length since there is no mortality info
        out$lengths <- rep(sim$length, sim$nTrials)
        return(out)
    }
    # Generate random ages of death using mortality info
    out$agesDeath1 <- generateAgesAtDeath(
        sim$persons[[1]]$curAge,
        sim$persons[[1]]$gender,
        sim$nTrials,
        sim$persons[[1]]$mort.factor,
        sim$persons[[1]]$mort.adj.years,
        TRUE
    )
    if (nPersons.sim(sim) == 2) {
        out$agesDeath2 <- generateAgesAtDeath(
            sim$persons[[2]]$curAge,
            sim$persons[[2]]$gender,
            sim$nTrials,
            sim$persons[[2]]$mort.factor,
            sim$persons[[2]]$mort.adj.years,
            TRUE
        )
        out$lengths <- apply(
            cbind(out$agesDeath1 - sim$persons[[1]]$curAge,
                  out$agesDeath2 - sim$persons[[2]]$curAge),
            1, max)
    } else {
        out$lengths <- out$agesDeath1 - sim$persons[[1]]$curAge
    }
    return(out)
}

#' Convert a Cash Flow Type to a Year
#'
#' This returns the index value for the cash flow vector.  The first element
#' of the vector represents year 0 - the inception (now).  And index value of
#' 2 represents year 1 and so on.
#'
#' cfType must be in "yr,start,end,p1age,p1ret,p1death,p2age,p2ret,p2death,1stdeath,2nddeath"
#'
#' @param cfType Cash flow type string
#' @param sim sim(ulation) object
#' @param length length of trial in years
#' @param ageDeath1 age of death of person 1 if applicable
#' @param ageDeath2 age of death of person 2 if applicable
#' @param value start or end value from cash flow record
#'
#' @return integer representing year in which cash flow starts or ends
#'
#' @examples  \dontrun{cvtCF2Yr(cf$startType,sim,length,ageDeath1,ageDeath2,cf$start)}
cvtCF2Yr <- function(cfType,
                     sim,
                     length,
                     ageDeath1,
                     ageDeath2,
                     value) {
    if (nPersons.sim(sim) == 0) {
        yr <- switch(
            tolower(cfType),
            yr = value + 1,
            start = 1, # 1 is year 0, 2 is year 1
            end = length + 1
        )
    }
    if (nPersons.sim(sim) == 1) {
        yrsDeath1 <- ageDeath1 - sim$persons[[1]]$curAge + 1
        yr <- switch(
            tolower(cfType),
            yr = value + 1,
            start = 1,
            end = length + 1,
            p1age = value - sim$persons[[1]]$curAge + 1,
            p1ret = sim$persons[[1]]$retireAge - sim$persons[[1]]$curAge +
                1,
            "p1ret-1" = sim$persons[[1]]$retireAge - sim$persons[[1]]$curAge,
            p1death = ageDeath1 - sim$persons[[1]]$curAge + 1,
            '1stdeath' = yrsDeath1
        )
    }
    if (nPersons.sim(sim) >= 2) {
        yrsDeath1 <- ageDeath1 - sim$persons[[1]]$curAge + 1
        yrsDeath2 <- ageDeath2 - sim$persons[[2]]$curAge + 1
        yr <- switch(
            tolower(cfType),
            yr = value + 1,
            start = 1,
            end = length + 1,
            p1age = value - sim$persons[[1]]$curAge + 1,
            p1ret = sim$persons[[1]]$retireAge - sim$persons[[1]]$curAge +
                1,
            "p1ret-1" = sim$persons[[1]]$retireAge - sim$persons[[1]]$curAge,
            p1death = ageDeath1 - sim$persons[[1]]$curAge + 1,
            p2age = value - sim$persons[[2]]$curAge + 1,
            p2ret = sim$persons[[2]]$retireAge - sim$persons[[2]]$curAge +
                1,
            "p2ret-1" = sim$persons[[2]]$retireAge - sim$persons[[2]]$curAge,
            p2death = ageDeath2 - sim$persons[[2]]$curAge + 1,
            '1stdeath' = min(yrsDeath1, yrsDeath2),
            '2nddeath' = max(yrsDeath1, yrsDeath2)
        )
    }
    return(yr)
}

#' Calculate Cash Flow Vector for a trial
#'
#' This takes all the cash flows and combines them into a vector of values for a single trial.
#' Each value in the vector represents a contribution (+) or withdrawal (-) for the  year.
#'
#' @param sim sim(ulation) object
#' @param length The number of years in that trial.
#' @param ageDeath1 age of death of person 1 if applicable
#' @param ageDeath2 age of death of person 2 if applicable
#' @param histInflation If historical inflation is to be used, this should be
#' a vector of 1 + inflation rates of length length
#'
#' @return vector of cash flows for a single trial
#'
#' @examples \dontrun{calcCF(sim,10,82,91)}
calcCF <- function(sim, length, ageDeath1, ageDeath2, histInflation = 0) {
    out <- numeric(length + 1)
    # if (nPersons.sim(sim)>=1) yrsDeath1<-ageDeath1-sim$persons[[1]]$curAge+1
    # if (nPersons.sim(sim)>=2) yrsDeath2<-ageDeath2-sim$persons[[2]]$curAge+1
    if (nCF.sim(sim) >= 1) {
        for (i in 1:nCF.sim(sim)) {
            cf <- sim$cf[i, ]
            out <- out + calcCFSub(cf, sim, length, ageDeath1, ageDeath2, histInflation)
        }
    }
    return(out)
}

#' Calculate A Cash Flow Vector
#'
#' This takes a single cash flow description and creates a numeric vector.
#' These vectors can be combined to create a net flow for each trial.
#'
#' @param cf description of a cash flow.
#' @param sim sim(ulation) object
#' @param length The number of years in that trial.
#' @param ageDeath1 age of death of person 1 if applicable
#' @param ageDeath2 age of death of person 2 if applicable
#' @param histInflation If historical inflation is to be used, this should be
#' a vector of 1 + inflation rates with length length
#'
#' @return vector of cash flows
#'
#' @examples \dontrun{calcCF(sim,10,82,91)}
calcCFSub <- function(cf, sim, length, ageDeath1, ageDeath2, histInflation = 0) {
    out <- numeric(length + 1)
    startyr <- cvtCF2Yr(cf$startType,
                        sim,
                        length,
                        ageDeath1,
                        ageDeath2,
                        cf$start)
    startyr <- pmax(startyr, 1)
    endyr <- cvtCF2Yr(cf$endType, sim, length, ageDeath1, ageDeath2, cf$end)
    endyr <- pmin(endyr, length + 1)
    if (endyr <= 0) return(out)
    if (startyr > endyr) return(out)
    if (startyr > (length + 1)) return(out)
    # Override inflation inputs if Chronological history or (Random history and
    #the override inflation input) is true
    # if (sim$description == "Chrono") browser()
    if (sim$returnGeneratorMethod == "C" | (sim$returnGeneratorMethod == "H" & sim$overrideInflation)) {
        # inflationRates <- c(1, cumprod(histInflation))[(startyr - 1):(endyr - 1)]
        inflationRates <- c(1, cumprod(histInflation))[startyr:endyr]
    } else {
        if (cf$defaultInflationAdj) {
            inflationRates <- (1 + sim$defaultInflation) ^ ((startyr - 1):(endyr - 1))
        } else {
            inflationRates <- (1 + cf$inflation) ^ ((startyr - 1):(endyr - 1))
        }
    }

    out[startyr:endyr] <- ifelse(tolower(cf$type) == "c", 1, -1) * cf$amount * inflationRates
    return(out)
}

#' Random return(s) from a lognormally distribution in decimal format
#'
#' Returns n lognormally distributed random variables with a mean of r and a
#' standard deviation of sd.  Periods over 1 year are annualized.  Specify
#' inputs in decimal formats (e.g. 8%=0.08 not 8).
#'
#' @param n Number of returns to generate
#' @param r Mean return
#' @param sd Standard deviation of returns
#' @param t Time in years
#' @param seed Random seed
#'
#' @return Random returns in decimal format
#' @export
#'
#' @examples \dontrun{calcRandReturns(10,.08,.12,1)}
#'
calcRandReturns <- function(n, r, sd, t, seed=NA){
    if (!is.na(seed)) {
        set.seed(seed)
    }
    vMean <- 1 + r
    vSD <- sd
    vLNSD <- sqrt(log(1 + (vSD / vMean) ^ 2)) # var
    vLNER <- log(vMean) - vLNSD ^ 2 / 2
    dblRnd <- stats::runif(n)
    return(exp(stats::qnorm(dblRnd, vLNER * t, vLNSD * sqrt(t)) / t) - 1)
}

#' Calculates ages of persons in Sim
#'
#' @param sim sim(ulation) object
#'
#' @return sim If there are persons, a curAge is added to each person
#' @export
#'
#' @examples \dontrun{calcAgesForSimulation(sim)}
calcAgesForSimulation <- function(sim) {
    if (nPersons.sim(sim) == 0) return(sim)
    for (i in 1:nPersons.sim(sim)) {
        sim$persons[[i]]$curAge <- calculate_age_nearest_birthday(sim$person[[i]]$birthDate, sim$asOfDate)
    }
    return(sim)
}

#' Validate Cash Flows
#'
#' @param sim Simulation object
#'
#' @return NULL is all cash flows are valid. Otherwise an error will be generated
#' @export
#'
#' @examples \dontrun{validateCashFlows(sim)}
validateCashFlows <- function(sim) {
    temp <- unique(c(which(grepl("p1", sim$cf$startType)), which(grepl("p1", sim$cf$endType))) )
    if (length(temp) > 0 & nPersons.sim(sim) < 1) {
        stop("No persons defined in the simulation but there are cash flows associated with p1. See cash flows ", temp)
    }
    temp <- unique(c(which(grepl("p2", sim$cf$startType)), which(grepl("p2", sim$cf$endType))) )
    if (length(temp) > 0 & nPersons.sim(sim) < 2) {
        stop("No persons defined in the simulation but there are cash flows associated with p2. See cash flows ", temp)
    }
    temp <- unique(c(which(grepl("death", sim$cf$startType)), which(grepl("death", sim$cf$endType))))
    if (length(temp) > 0 & (sim$lengthType == "F" | sim$returnGeneratorMethod == "C")) {
        stop("A fixed length simulation was setup but there are cash flows associated with death. See cash flows ", temp)
    }
    temp <- c(which(grepl("C", toupper(sim$cf$type))), which(grepl("W", toupper(sim$cf$type))))
    if (length(temp) != nrow(sim$cf)) {
        stop("The type of cash flow should be 'c' or 'w'.")
    }
    return(NULL)
}

#' Calculates the IRR of a stream of cash flow
#'
#' Returns the IRR of a stream of cash flows.  NA will be returned
#' if there isn't at least one positive and one negative value.  If
#' the IRR is outside the range -99.99\% to 10000\%, NA is likely to be produced.
#'
#' @param cashflows Vector of cash flows. These should include at least one
#' positive and one negative value.
#'
#' @return Decimal IRR or NA.
#' @export
#'
#' @examples \dontrun{calcIRR(cashflows)}
calcIRR <- function(cashflows) {
    if (is.na(sum(cashflows))) return(NA)
    if (length(cashflows) == 0) return(NA)
    # all cash flows are positive, return -100%.
    if (min(cashflows) >= 0) return(-1)
    # all cash flows are negative, return NA
    if (max(cashflows) <= 0) return(NA)
    npv <- function(rate, cashflows) {
        sum(cashflows / (1 + rate) ^ (0:(length(cashflows) - 1)))
    }
    out <- tryCatch({
        # Your code that might produce an error
        uniroot(npv, c(-0.9999, 100), tol = 0.00005, cashflows = cashflows)$root
    }, error = function(e) {
        # Code to execute if an error occurs
        NA
    })
    return(out)
}

#' Probability of achieving a specified return
#'
#' Returns the probability of doing at least as well as over t
#'
#' @param target Target (specified) return
#' @param r Mean Return
#' @param sd Standard deviation of returns
#' @param t Time in years
#'
#' @return value representing a probability.
#' @export
#'
#' @examples AA_Prob(7.339443,8,12,10)
AA_Prob <- function(target, r, sd, t) {
    vmean <- 1 + r/100
    vsd <- sd/100
    vlnsd <- sqrt(log(1 + (vsd/vmean)^2))
    vlner <- log(vmean) - vlnsd^2/2
    return(1 - pnorm(log((target/100 + 1)^t), vlner * t, vlnsd *
                         sqrt(t)))
}
