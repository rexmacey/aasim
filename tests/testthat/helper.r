library(readxl)
# library(aasim)

readExcelSimData <- function(xlsxfilename) {
    out <- list()
    out$xl <- read_excel(xlsxfilename)
    out$xlcf <- read_excel(xlsxfilename, sheet = 2)
    return(out)
}

simulatefromexcel <- function(xlsxfilename, asOfDate){
    xlInputs <- readExcelSimData(xlsxfilename = xlsxfilename)
    out <- lapply(xlInputs$xl$description,
                  simulateexcelsub,
                  xl = xlInputs$xl,
                  xlcf = xlInputs$xlcf,
                  asOfDate)
    names(out) <- xlInputs$xl$description
    return(out)
}

# set up for the simulation. do not run
# ready to step through simulate after this.
setupSimulation <- function(simDescription, xl, xlcf){
    simInput <- xl[xl$description == simDescription, ]
    out <- initializeSim(
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
        out <- addPerson.sim(
            out,
            simInput$p1name,
            simInput$p1init,
            simInput$p1birthdate,
            simInput$p1gender,
            simInput$p1retage,
            simInput$p1mortfactor,
            simInput$p1mortadjyears
        )
    }

    if (!is.na(simInput$p2name)) {

        out <- addPerson.sim(
            out,
            simInput$p2name,
            simInput$p2init,
            simInput$p2birthdate,
            simInput$p2gender,
            simInput$p2retage,
            simInput$p2mortfactor,
            simInput$p2mortadjyears)
    }

    cfInput <- xlcf[xlcf$simulation == simDescription, ]

    for (i in 1:nrow(cfInput)) {
        out$cf <- addCF(
            simCF = out$cf,
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
    return(out)
}

simulateexcelsub <- function(simDescription, xl, xlcf, asOfDate){
    sim <- setupSimulation(simDescription, xl, xlcf)
    sim$simulation <- aasim::simulate(sim, asOfDate)
    return(sim)
}

simTest <- simulatefromexcel(paste0(testthat::test_path(),"/data/siminput.xlsx"), as.Date("2024-09-12"))
