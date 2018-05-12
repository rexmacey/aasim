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
#' @examples simulate(sim)
simulate<-function(sim){
    trials<-getHorizons.sim(sim)
    portfolioValues<-list()
    rateOfReturns<-list()
    trials$cashFlows<-list()
    for (iTrial in 1:sim$nTrials){
        portfolioValues[[iTrial]]<-numeric(trials$lengths[iTrial]+1)
        portfolioValues[[iTrial]][1]<-sim$startValue
        cashflows<-calcCF(sim,trials$lengths[iTrial],trials$agesDeath1[iTrial],trials$agesDeath2[iTrial])
        trials$cashFlows[[iTrial]]<-cashflows
        randomReturns<-1+calcRandReturns(trials$lengths[iTrial],sim$ror,sim$stdDev,1)
        rateOfReturns[[iTrial]]<-randomReturns
        for (y in 1:trials$lengths[iTrial]){
            portfolioValues[[iTrial]][y+1]<-portfolioValues[[iTrial]][y] * randomReturns[y] + cashflows[y]
            if (portfolioValues[[iTrial]][y+1]<=0){
                portfolioValues[[iTrial]][y+1]<-0
                break
            }
        }
    }
    trials$portfolioValues<-portfolioValues
    trials$rateOfReturns<-rateOfReturns
    return(trials)
}

#' Get Time Horizons
#'
#' This will return the lengths of each trial
#'
#' @param sim Sim(ulation) object
#'
#' @return List with three vectors. lengths is number of years in each trial; agesDeath1 are the ages at which the first person passes,
#' agesDeath2 are the ages at which the second person passes.
#'
#' @examples getHorizons(sim1)
getHorizons.sim<-function(sim){
    #source("generateAgesAtDeath.r")
    out<-list()
    out$lengths<-NA
    out$agesDeath1<-NA
    out$agesDeath2<-NA
    if (toupper(substr(sim$lengthType,1,1)=="F")){  # used fixed length
        out$lengths<-rep(sim$length,sim$nTrials)
        return(out)
    }
    if (nPersons.sim(sim)==0) {  # also use fixed length since there is no mortality info
        out$lengths<-rep(sim$length,sim$nTrials)
        return(out)
    }
    # Generate random ages of death using mortality info
    out$agesDeath1<-generateAgesAtDeath(sim$persons[[1]]$curAge,
                                    sim$persons[[1]]$gender,
                                    sim$nTrials,
                                    sim$persons[[1]]$mort.factor)
    if (nPersons.sim(sim)==2){
        out$agesDeath2<-generateAgesAtDeath(sim$persons[[2]]$curAge,
                                        sim$persons[[2]]$gender,
                                        sim$nTrials,
                                        sim$persons[[2]]$mort.factor)
        if (sim$nTrials>1){
            out$agesDeath2<-sample(out$agesDeath2,sim$nTrials)    # randomly order 2nd deaths
        }
        out$lengths<- apply(cbind(out$agesDeath1-sim$persons[[1]]$curAge+1,out$agesDeath2-sim$persons[[2]]$curAge+1),1,max)
    } else {
        out$lengths<- out$agesDeath1-sim$persons[[1]]$curAge+1
    }
    return(out)
}

#' Convert a Cash Flow Type to a Year
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
#' @examples cvtCF2Yr(cf$startType,sim,length,ageDeath1,ageDeath2,cf$start)
cvtCF2Yr<-function(cfType,sim,length,ageDeath1,ageDeath2,value){
    if (nPersons.sim(sim)==0){
        yr<-switch(tolower(cfType),
                   yr = value,
                   start = 1,
                   end = length)
    }
    if (nPersons.sim(sim)==1){
        yrsDeath1<-ageDeath1-sim$persons[[1]]$curAge+1
        yr<-switch(tolower(cfType),
                   yr = value,
                   start = 1,
                   end = length,
                   p1age = value-sim$persons[[1]]$curAge+1,
                   p1ret = sim$persons[[1]]$retireAge-sim$persons[[1]]$curAge+1,
                   p1death = ageDeath1-sim$persons[[1]]$curAge+1,
                   '1stdeath' = yrsDeath1)
    }
    if (nPersons.sim(sim)>=2){
        yrsDeath1<-ageDeath1-sim$persons[[1]]$curAge+1
        yrsDeath2<-ageDeath2-sim$persons[[2]]$curAge+1
        yr<-switch(tolower(cfType),
                   yr = value,
                   start = 1,
                   end = length,
                   p1age = value-sim$persons[[1]]$curAge+1,
                   p1ret = sim$persons[[1]]$retireAge-sim$persons[[1]]$curAge+1,
                   p1death = ageDeath1-sim$persons[[1]]$curAge+1,
                   p2age = value-sim$persons[[2]]$curAge+1,
                   p2ret = sim$persons[[2]]$retireAge-sim$persons[[2]]$curAge+1,
                   p2death = ageDeath2-sim$persons[[2]]$curAge+1,
                   '1stdeath' = min(yrsDeath1,yrsDeath2),
                   '2nddeath' = max(yrsDeath1,yrsDeath2))
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
#'
#' @return
#'
#' @examples calcCF(sim,10,82,91)
calcCF<-function(sim,length,ageDeath1,ageDeath2){
    out<-numeric(length)

    # if (nPersons.sim(sim)>=1) yrsDeath1<-ageDeath1-sim$persons[[1]]$curAge+1
    # if (nPersons.sim(sim)>=2) yrsDeath2<-ageDeath2-sim$persons[[2]]$curAge+1
    for (i in 1:nCF.sim(sim)){
        cf<-sim$cf[i,]
        startyr<-cvtCF2Yr(cf$startType,sim,length,ageDeath1,ageDeath2,cf$start)
        startyr<-max(1,startyr)
        endyr<-cvtCF2Yr(cf$endType,sim,length,ageDeath1,ageDeath2,cf$end)
        endyr<-min(length,endyr)
        if (startyr>endyr) break
        if (cf$inflationAdj){
            out[startyr:endyr] <- out[startyr:endyr] +
                ifelse(tolower(cf$type)=="c",1,-1) * cf$amount * (1+sim$inflation)^(startyr:endyr)
        } else {
            out[startyr:endyr] <- out[startyr:endyr] +
                ifelse(tolower(cf$type)=="c",1,-1) * cf$amount
        }
    }
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
#' @examples calcRandReturns(10,.08,.12,1)
#'
calcRandReturns<-function(n,r,sd,t,seed=NA){
    if (!is.na(seed)) {
        set.seed(seed)
    }
    vMean <- 1 + r
    vSD <- sd
    vLNSD <- sqrt(log(1 + (vSD / vMean) ^ 2)) # var
    vLNER <- log(vMean) - vLNSD ^ 2 / 2
    dblRnd<-stats::runif(n)
    return(exp(stats::qnorm(dblRnd, vLNER * t, vLNSD * sqrt(t)) / t) - 1)
}

