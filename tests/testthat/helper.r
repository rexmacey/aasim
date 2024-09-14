readExcelSimData <- function(xlsxfilename) {
    out <- list()
    out$xl <- readxl::read_excel(xlsxfilename)
    out$xlcf <- readxl::read_excel(xlsxfilename, sheet = 2)
    return(out)
}

simulatefromexcel <- function(xlsxfilename){
    xlInputs <- readExcelSimData(xlsxfilename = xlsxfilename)
    xlInputs$xl$minDate <- as.Date(xlInputs$xl$minDate)
    xlInputs$xl$minDate[is.na(xlInputs$xl$minDate)] <- as.Date("1900-01-01")
    xlInputs$xl$maxDate <- as.Date(xlInputs$xl$maxDate)
    xlInputs$xl$maxDate[is.na(xlInputs$xl$maxDate)] <- Sys.Date() + 1
    out <- lapply(xlInputs$xl$description,
                  simulateexcelsub,
                  xl = xlInputs$xl,
                  xlcf = xlInputs$xlcf)
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
        targetValueIsReal = simInput$targetValueIsReal,
        stockWt = simInput$stockWt,
        nConsecMonths = simInput$nConsecMonths,
        retAdj = simInput$retAdj,
        minDate = simInput$minDate,
        maxDate = simInput$maxDate,
        overrideInflation = simInput$overrideInflation,
        asOfDate = simInput$asOfDate,
        randReturnType = simInput$randReturnType
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
    if (nrow(cfInput) >= 1) {
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
    }
    return(out)
}

simulateexcelsub <- function(simDescription, xl, xlcf){
    sim <- setupSimulation(simDescription, xl, xlcf)
    sim$simulation <- simulateRandom(sim)
    return(sim)
}

simTest <- simulatefromexcel(paste0(testthat::test_path(),"/data/siminput.xlsx"))
