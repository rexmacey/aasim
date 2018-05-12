#' Generate Ages at Death
#'
#' This function generates ages at death depending on a mortality table. If n=1, the function
#' will return the life expectancy.
#'
#' @param curAge Current age of individual.  Assumes just turned this age.
#' @param gender 'Male' or 'Female'
#' @param n Number of ages to generate
#' @param mort.factor A value multiplied by each mortality value. Higher than 1 shortens lifespan
#'
#' @return Vector of ages at death
#'
generateAgesAtDeath <- function(curAge, gender, n = 1, mort.factor=1.0) {
    #load(file="./data/mortality.rdata")
    mortality <- readRDS("./data/mortality.rds")
    maxAge <- mortality[nrow(mortality), "age"]
    out <- integer(n)
    if (curAge < 0)
        curAge <- 0
    if (curAge > maxAge) {
        out[1:n] <- maxAge
        return(out)
    }
    mortalityColumn <- ifelse(substr(toupper(gender), 1, 1) == "M", 2, 3)
    numLivesRemaining <- numeric(maxAge - curAge + 2)
    intLivesRemaining <- integer(maxAge - curAge + 2)
    numLivesRemaining[1] <- n
    intLivesRemaining[1] <- n
    j = 2
    k = 1
    for (i in curAge:maxAge) {
        numLivesRemaining[j] <-
            numLivesRemaining[j - 1] * (1 - mort.factor * mortality[i + 1, mortalityColumn])
        intLivesRemaining[j] <- round(numLivesRemaining[j])
        l <- intLivesRemaining[j - 1] - intLivesRemaining[j]
        if (l > 0){
            out[k:(k+l-1)] <- i
            k <- k + l
        }
        j <- j + 1
    }
    return(out)
}

