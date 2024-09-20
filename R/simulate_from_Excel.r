#' Read Simulation Inputs from Excel
#'
#' This functions reads data from the first two tabs of an Excel workbook.
#' The first is expected to contain general information on the simulation and
#' the second is expected to contain information on the cash flows.
#'
#' @param xlsxfilename File name including path
#'
#' @return List with an xl data frame and a xlcf data frame
#' @export
#'
#' @examples \dontrun{readExcelSimData(paste0(testthat::test_path(),"/data/siminput.xlsx"))}
readExcelSimData <- function(xlsxfilename) {
    out <- list()
    out$xl <- readxl::read_excel(xlsxfilename)
    out$xlcf <- readxl::read_excel(xlsxfilename, sheet = 2)
    return(out)
}

#' Simulate using data from Excel
#'
#' This functions reads data from the first two tabs of an Excel workbook.
#' The first is expected to contain general information on the simulation and
#' the second is expected to contain information on the cash flows.
#'
#' Columns in the first tab might include: description,	nTrials, startValue,
#' lengthType, length,	seed,	defaultInflation,	ror,	stdDev,	targetValue,
#' targetValueIsReal,	stockWt,	nConsecMonths,	retAdj,	minDate,	maxDate,
#' overrideInflation,	asOfDate,	returnGeneratorMethod,	p1name,	p1init,	p1birthdate,
#' p1gender,	p1retage,	p1mortfactor,	p1mortadjyears,	p2name,	p2init,
#' p2birthdate,	p2gender,	p2retage,	p2mortfactor,	p2mortadjyears.
#'
#' Columns in the second tab should include: simulation,	description,
#' starttype,	start,	endtype,	end,	type,	amount,	defaultInflationAdj,
#' inflation.
#'
#'
#' @param xlsxfilename File name including path
#'
#' @return List containing lists for each simulation in the file.
#' @export
#'
#' @examples \dontrun{simulatefromexcel(paste0(testthat::test_path(),"/data/siminput.xlsx"))}
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


#' Set up (prepare) object for simulation
#'
#' This can be useful for debugging.
#' After this function is run, you are ready to step through simulate after this.
#'
#' @param simDescription String giving the description of the simulation to prepare.
#' @param xl Data frame from readExcelSimData with general simulation inputs.
#' @param xlcf Data frame from readExcelSimData with cash flow.
#'
#' @return List with simulation inputs
#' @export
#' @examples \dontrun{setupSimulation, xl, xlcf}
setupSimulation <- function(simDescription, xl, xlcf){
    simInput <- xl[xl$description == simDescription, ]
    out <- simClass(
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
        returnGeneratorMethod = simInput$returnGeneratorMethod
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

#' Sub function for simulating using data from Excel.
#'
#' @param simDescription String giving the description of the simulation to prepare.
#' @param xl Data frame from readExcelSimData with general simulation inputs.
#' @param xlcf Data frame from readExcelSimData with cash flow.
#'
#' @return List with results for the simulation
#' @export
#'
#' @examples \dontrun{simulateexcelsub("sim1", xl, xlcf)}
simulateexcelsub <- function(simDescription, xl, xlcf){
    sim <- setupSimulation(simDescription, xl, xlcf)
    sim$simulation <- simulateWealth(sim)
    return(sim)
}
