#' Calculate current age
#'
#' @param birthdate Birthdate in character (YYYY-MM-DD) or as class date.
#' @param asOfDate The date to calculate the age. Default is the system date.
#'
#' @return Age in years as numeric
#' @export
#'
#' @examples \dontrun{calculate_age("1950-01-01")}
calculate_age <- function(birthdate, asOfDate = Sys.Date()) {
    # Convert the input birthdate to a Date object
    if (typeof(birthdate) == "character") birthdate <- as.Date(birthdate)
    if (typeof(asOfDate) == "character") asOfDate <- as.Date(asOfDate)
    # Get the current date
    current_date <- asOfDate

    # Calculate the difference in years
    age <- as.numeric(format(current_date, "%Y")) - as.numeric(format(birthdate, "%Y"))

    # Adjust the age if the birthday hasn't occurred yet this year
    if (format(current_date, "%m-%d") < format(birthdate, "%m-%d")) {
        age <- age - 1
    }

    return(age)
}

#' Calculate days until next birthday
#'
#' @param birthdate Birthdate in character (YYYY-MM-DD) or as class date.
#' @param asOfDate The date to calculate the age. Default is the system date.
#'
#' @return days as numeric
#' @export
#'
#' @examples \dontrun{days_until_next_birthday("1950-01-01")}
days_until_next_birthday <- function(birthdate, asOfDate = Sys.Date()) {
    # Convert the input birthdate to a Date object
    if (typeof(birthdate) == "character") birthdate <- as.Date(birthdate)
    if (typeof(asOfDate) == "character") asOfDate <- as.Date(asOfDate)
    # Get the current date
    current_date <- asOfDate

    # Extract the month and day from the birthdate
    birth_month_day <- format(birthdate, "%m-%d")

    # Create the next birthday date for the current year
    next_birthday <- as.Date(paste(format(current_date, "%Y"), birth_month_day, sep = "-"))

    # If the next birthday has already passed this year, set it to next year
    if (next_birthday < current_date) {
        next_birthday <- as.Date(paste(as.numeric(format(current_date, "%Y")) + 1, birth_month_day, sep = "-"))
    }

    # Calculate the number of days until the next birthday
    days_until <- as.numeric(difftime(next_birthday, current_date, units = "days"))

    return(days_until)
}

#' Calculate days since last birthday
#'
#' @param birthdate Birthdate in character (YYYY-MM-DD) or as class date.
#' @param asOfDate The date to calculate the age. Default is the system date.
#'
#' @return days as numeric
#' @export
#'
#' @examples \dontrun{days_since_last_birthday("1950-01-01")}
days_since_last_birthday <- function(birthdate, asOfDate = Sys.Date()) {
    # Convert the input birthdate to a Date object
    if (typeof(birthdate) == "character") birthdate <- as.Date(birthdate)
    if (typeof(asOfDate) == "character") asOfDate <- as.Date(asOfDate)
    # Get the current date
    current_date <- asOfDate

    # Extract the month and day from the birthdate
    birth_month_day <- format(birthdate, "%m-%d")

    # Create the last birthday date for the current year
    last_birthday <- as.Date(paste(format(current_date, "%Y"), birth_month_day, sep = "-"))

    # If the last birthday hasn't occurred yet this year, set it to last year
    if (last_birthday > current_date) {
        last_birthday <- as.Date(paste(as.numeric(format(current_date, "%Y")) - 1, birth_month_day, sep = "-"))
    }

    # Calculate the number of days since the last birthday
    days_since <- as.numeric(difftime(current_date, last_birthday, units = "days"))

    return(days_since)
}

#' Calculate age at nearest birthday
#'
#' Returns the age at the previous or next birthday, whichever is nearest.
#'
#' @param birthdate Birthdate in character (YYYY-MM-DD) or as class date.
#' @param asOfDate The date to calculate the age. Default is the system date.
#'
#' @return Age in years as numeric
#' @export
#'
#' @examples \dontrun{calculate_age_nearest_birthday("1950-01-01")}
calculate_age_nearest_birthday <- function(birthdate, asOfDate = Sys.Date()) {
    if (days_since_last_birthday(birthdate, asOfDate) <= days_until_next_birthday(birthdate, asOfDate)) {
        return(calculate_age(birthdate, asOfDate))
    } else {
        return(calculate_age(birthdate, asOfDate) + 1)
    }
}

