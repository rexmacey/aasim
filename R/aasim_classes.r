#' Intialize a Simulation
#'
#' @param description Description of simulation
#' @param nTrials number of trials
#' @param startValue Starting dollar value
#' @param lengthType 'R' for random, or 'F' for fixed. If number of persons is 0, then F is assumed.
#' @param length Length for fixed type of simulations in years
#' @param seed Seed for random number generator
#' @param defaultInflation Inflation rate in decimal
#' @param ror Rate of Return in decimal
#' @param stdDev Standard Deviation in decimal
#' @param targetValue Dollar value used to determine if a trial is a success
#' @param targetValueIsReal Logical to indicate if target value should be
#'   adjusted for inflation. Default is TRUE.
#' @param stockWt The allocation to stocks, in decimal, used to generate historical random returns.
#' @param nConsecMonths Number of consecutive months, 1, 2, 3, 4, 6, or 12 used to generate historical random returns.
#' @param retAdj A value to add to each randomly generated annual historic return in decimal.
#' @param minDate MinDate Earliest date to use when generating historical random returns.
#' @param maxDate MaxDate Latest date to use when generating historical random returns.
#' @param asOfDate Date to run the simulation as of. Used when calculating ages.
#'
#' @return List with values describing simulation
#' @seealso [calcRandHistReturns()]
#' @export
#'
#' @examples \dontrun{initializeSim("Sim1 Test", nTrials=500, 1000000, lengthType="R",
#' length=0, seed=-101, defaultInflation=0, ror=0.10, stdDev=.08,
#' targetValue=.Machine$double.eps, targetValueIsReal=FALSE)}
initializeSim <-
    function(description,
             nTrials,
             startValue,
             lengthType,
             length = 10,
             seed,
             defaultInflation = 0,
             ror,
             stdDev,
             targetValue = .Machine$double.eps,
             targetValueIsReal = TRUE,
             stockWt = 0.6,
             nConsecMonths = 12,
             retAdj = 0,
             minDate = min(sbi$Month),
             maxDate = max(sbi$Month),
             asOfDate = Sys.Date()) {
        sim <- list()
        sim[["description"]] <- description
        sim[["nTrials"]] <- nTrials
        sim[["startValue"]] <- startValue
        sim[["lengthType"]] <- lengthType
        sim[["length"]] <- length
        sim[["seed"]] <- seed
        sim[["defaultInflation"]] <- defaultInflation
        sim[["ror"]] <- ror
        sim[["stdDev"]] <- stdDev
        sim[["targetValue"]] <- targetValue
        sim[["targetValueIsReal"]] <- targetValueIsReal
        sim[["stockWt"]] <- stockWt
        sim[["nConsecMonths"]] <- nConsecMonths
        sim[["retAdj"]] <- retAdj
        sim[["minDate"]] <- minDate
        sim[["maxDate"]] <- maxDate
        sim[["asOfDate"]] <- asOfDate
        sim[["cf"]] <- initializeCF()
        sim[["persons"]] <- list()
        class(sim) <- "sim"
        return(sim)
    }

#' Add Person to persons list in a simulation
#'
#' @param sim Object of type sim (simulation)
#' @param name Name of person
#' @param initials Initials or short name, useful for display
#' @param birthDate Birth date to determine current age, format YYYY-MM-DD.
#' @param gender 'M' or 'Male' or 'F' or 'Female'
#' @param retireAge Retirement age.
#' @param mort.factor Mortality factor, default = 1.  This is multiplied by each mortality rate. Values >1 decrease life expectancy.
#' @param mort.adj.years Mortality adjustment, default = 0.  This value is added to each generated age at death.
#'
#' @return sim object with person added to simulation
#' @export
#'
#' @examples \dontrun{sim1 <- addPerson.sim(sim, name, initials, birthDate, gender,
#' retireAge, mort.factor, mort.adj.years)}
#' \dontrun{sim1<-addPerson.sim(sim1,"Rex Macey","RM",56,"M",65,1.0, 0.0)}
addPerson.sim <- function(sim,
                          name,
                          initials,
                          birthDate,
                          gender,
                          retireAge,
                          mort.factor = 1.0,
                          mort.adj.years = 0.0) {
    # npersons <- length(sim$persons)
    p <- list()
    p[["name"]] <- name
    p[["initials"]] <- initials
    p[["birthDate"]] <- birthDate
    p[["gender"]] <- gender
    p[["retireAge"]] <- retireAge
    p[["mort.factor"]] <- mort.factor
    p[["mort.adj.years"]] <- mort.adj.years
    sim[["persons"]][[length(sim[["persons"]]) + 1]] <- p
    return(sim)
}

#' Number of persons in a sim(ulation) object
#'
#' @param sim Simulation object
#'
#' @return Number of persons in object
#' @export
#'
#' @examples \dontrun{nPersons.sim(sim1)}
nPersons.sim <- function(sim) {
    return(length(sim$persons))
}

#' Number of cash flows in a sim(ulation) object
#'
#' @param sim Simulation object
#'
#' @return Number of cash flows in object
#' @export
#'
#' @examples \dontrun{nCF.sim(sim1)}
nCF.sim <- function(sim){
    return(nrow(sim$cf))
}

#' Initialize Cash Flow data frame
#'
#' @return Data frame with no rows but columns for cash flows
#'
#' @examples \dontrun{initializeCF()}
initializeCF <- function() {
    cf.df <- data.frame(
        description = character(),
        # description of cash flow
        startType = character(),
        start = integer(),
        endType = character(),
        end = integer(),
        type = character(),
        amount = double(),
        defaultInflationAdj = logical(),
        inflation = double()
    )
    return(cf.df)
}

#' Validate a Cash Flow Type
#'
#' @param strCF String to be tested
#'
#' @return Logical value. TRUE is a valid cash flow type, FALSE if not.
#' @export
#'
#' @examples \dontrun{validateCFType("p1age")}
validateCFType <- function(strCF) {
    validStrings <- unlist(
        strsplit(
            "yr,start,end,p1age,p1ret,p1ret-1,p1death,p2age,p2ret,p2ret-1,p2death,1stdeath,2nddeath",
            ","
        )
    )
    strCF.lower <- tolower(strCF)
    return(strCF.lower %in% validStrings)
}

#' Add a Cash Flow record to the Cash Flow data frame
#'
#' Here are the types that describe when a cash flow starts or ends: yr
#' indicates a numeric value will be supplied such as 1 to start year one and 10
#' to end year 10. p1age, p2age is the age of person1 or person 2; p1ret, p2ret
#' is the retirement age of person1 or person 2; p1death, p2death is the age at
#' death  of person1 or person 2; 1stdeath, 2nddeath is the death of the 1st to
#' die, or the 2nd to die; start is first year (equivalent to startType=='yr' and
#' specifying 1 for the start); end is last year
#'
#' @param simCF The data frame to which to add a cash flow
#' @param description Description of cash flow
#' @param startType Type of starting period.
#' @param start numeric value representing startType='yr' or 'p1age
#' @param endType Type of ending period.
#' @param end numeric value representing endType='yr' or 'p1age
#' @param type either 'c' for contribution or 'w' for withdrawal
#' @param amount dollar amount of cash flow
#' @param defaultInflationAdj logical to indicate whether cash flow is to be adjusted using the default inflation.
#' @param inflation numeric value representing inflation rate to use if the default rate is not used.
#'
#' @return A data frame with the cash flows of the simulation and the added cash flow
#' @export
#'
#' @examples \dontrun{simCF <- addCF(simCF, description, startType, start,
#'                    endType, end, type, amount, defaultInflationAdj)}
#' \dontrun{simCF <- addCF(simCF, "Retirement Expense", "p1ret", 0,
#'                    "p1death", 0, "w", 40000, TRUE)}
addCF <-
    function(simCF,
             description,
             startType,
             start,
             endType,
             end,
             type,
             amount,
             defaultInflationAdj,
             inflation) {
        new.df <- data.frame(description = description,
            startType = startType,
            start = start,
            endType = endType,
            end = end,
            type = type,
            amount = amount,
            defaultInflationAdj = defaultInflationAdj,
            inflation = inflation)
        simCF <- rbind(simCF, new.df)
        return(simCF)
    }
