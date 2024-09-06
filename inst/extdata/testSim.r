library(readxl)
library(aasim)

simulatefromexcel <- function(xlsxfilename){
    xl <- read_excel(xlsxfilename)
    xlcf <- read_excel(xlsxfilename, sheet = 2)
    out <- lapply(xl$description,
                  simulateexcelsub,
                  xl = xl,
                  xlcf = xlcf)
    return(out)
}
simulateexcelsub <- function(simDescription, xl, xlcf){
    simInput <- xl[xl$description == simDescription, ]
    sim1 <- initializeSim(
        description = simInput$description,
        nTrials = simInput$nTrials,
        startValue = simInput$startValue,
        lengthType = simInput$lengthType,
        length = simInput$length,
        seed = simInput$seed,
        defaultInflation = simInput$defaultInflation,
        ror = simInput$ror,
        stdDev = simInput$stdDev,
        targetValue = .Machine$double.eps,
        targetValueIsReal = simInput$targetValueIsReal
    )
    if (!is.na(simInput$p1name)) {
        sim1 <- addPerson.sim(
            sim1,
            simInput$p1name,
            simInput$p1init,
            simInput$p1age,
            simInput$p1gender,
            simInput$p1retage,
            simInput$p1mortfactor,
            simInput$p1mortadjyears
        )
    }

    if (!is.na(simInput$p2name)) {

        sim1 <- addPerson.sim(
            sim1,
            simInput$p2name,
            simInput$p2init,
            simInput$p2age,
            simInput$p2gender,
            simInput$p2retage,
            simInput$p2mortfactor,
            simInput$p2mortadjyears)
    }

    cfInput <- xlcf[xlcf$simulation == simDescription, ]

    for (i in 1:nrow(cfInput)) {
        sim1$cf <- addCF(
            simCF = sim1$cf,
            description = cfInput$description[i],
            startType = cfInput$starttype[i],
            start = cfInput$start[i],
            endType = cfInput$endtype[i],
            end = cfInput$end[i],
            type = cfInput$type[i],
            amount = cfInput$amount[i],
            defaultInflationAdj = cfInput$defaultInflationAdj[i],
            inflation = cfInput$inflation[i]
        )
    }
    sim1$simulation <- simulate(sim1)
    return(sim1)
}

results <- simulatefromexcel("siminput.xlsx")

tmp <- sapply(results$simulation$portfolioValues, function(x) x[length(x)])
