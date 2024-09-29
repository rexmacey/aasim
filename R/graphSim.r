#' Success Donut Chart
#'
#' Produces a simple donut chart showing the number and percent of successes
#' and failures.
#'
#' @param sim Simulation object after running simulate function
#' @param vs Choices are 'T' for target or "Z' for zero
#'
#' @return a ggplot object
#' @export
#'
#' @examples \dontrun{chartSucessDonut(sim)}
chartSuccessDonut <- function(sim, vs = "T") {
  successStats <- getSuccessStats(sim)

  # Create a data frame with Success and Failure counts
  vs <- toupper(substr(vs, 1, 1))
  if (vs == "T") {
      data <- data.frame(
          Legend = c("Success", "Failure"),
          count = c(successStats$vsTargetCount, sim$nTrials - successStats$vsTargetCount))
      gtitle <- "Success Rate vs Target"
  } else {
      data <- data.frame(
          Legend = c("Success", "Failure"),
          count = c(successStats$vs0Count, sim$nTrials - successStats$vs0Count))
      gtitle <- "Success Rate vs $0"
  }


  # Calculate the percentage
  data <- data %>%
      dplyr::mutate(percentage = count / sum(count) * 100)

  # Create the donut chart
  out <- ggplot(data, aes(x = 2, y = count, fill = Legend)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")),
                position = position_stack(vjust = 0.5)) +
      theme_void() +
      theme(legend.position = "right") +
      labs(title = gtitle)
  return(out)
}

#' Values across Time vs Target Chart
#'
#' Produces a ribbon chart.  There's a line for the median value of all the
#' trials at each year. Additionally there are 50% and 90% confidence intervals.
#' A black line shows the value of the target value.
#'
#' @param sim Simulation object after running simulate function
#'
#' @return ggplot2 object
#' @export
#'
#' @examples \dontrun{chartValuesOverTime(sim)}
chartValuesOverTime <- function(sim) {
    targetValues <- getTargetValues(sim)
    data <- cbind(getDistOfValuesByYear(sim, c(0.05, 0.25, 0.50, 0.75, 0.95)), Target = getTargetValues(sim))
    cols <- c("Median" = "red", "Target" = "black")
    fills <- c("90%" = "lightblue", "50%" = "blue")
    out <- ggplot(data, aes(x = Length)) +
        geom_ribbon(aes(ymin= p5, ymax = p95, fill = "90%"), alpha = 0.9, show.legend = TRUE) +
        geom_ribbon(aes(ymin= p25, ymax = p75, fill = "50%"), alpha = 0.2, show.legend = TRUE) +
        geom_line(aes(y = p50, color = "Median"), linewidth = 1.5, show.legend = TRUE) +
        geom_line(aes(y = Target, color = "Target"), linewidth = 1.25, show.legend = TRUE) +
        scale_y_continuous(labels = dollar) +
        scale_fill_manual(values = fills) +
        scale_color_manual(values = cols) +
        guides(color = guide_legend(override.aes = list(fill = NA))) +
    labs(title = "Median Values across Time vs. Target",
         x = "Year",
         y = "Value ($)",
         fill = "Confidence Intervals",
         color = "Lines")
    return(out)
}

#' Success across Time Chart
#'
#' Produces two charts, one over the other.  The top shows stacked bars with
#' the number of successes and failures by year.  The bottom is a line representing
#' the cumulative success rate (% of successful trials through that length).  If
#' the value of vs is T, then the target is used, otherwise zero.
#'
#' @param sim Simulation object after running simulate function
#' @param vs Choices are 'T' for target or "Z' for zero
#'
#' @return ggplot2 object
#' @export
#'
#' @examples \dontrun{(sim)}
chartSuccessOverTime <- function(sim, vs = "T") {
    if (toupper(substr(vs, 1, 1)) == "T") {
        data1 <- getSuccessByLength(sim) %>% rename(Success = SuccessVsTargetCount) %>%
            mutate(PctSuccess = SuccessVsTargetPct , Failure = Frequency - Success) %>%
            select(Length, Failure, Success, PctSuccess)
        data2 <- getSuccessByLength(sim) %>%
            mutate(Cumulative = SuccessVsTargetCumPct) %>%
            select(Length, Cumulative)
        gtitle1 <- "Successes and Failures vs Target by Length of Trial"
        gtitle2 <- "Cumulative Success Rate vs Target by Length of Trial"
    } else {
        data1 <- getSuccessByLength(sim) %>% rename(Success = SuccessVs0Count) %>%
            mutate(PctSuccess = SuccessVs0Pct , Failure = Frequency - Success) %>%
            select(Length, Failure, Success, PctSuccess)
        data2 <- getSuccessByLength(sim) %>%
            mutate(Cumulative = SuccessVs0CumPct) %>%
            select(Length, Cumulative)
        gtitle1 <- "Successes and Failures vs $0 by Length of Trial"
        gtitle2 <- "Cumulative Success Rate vs $0 by Length of Trial"
    }
    dataLong <- reshape2::melt(data1, id.vars = c("Length", "PctSuccess"),
                               variable.name = "variable", value.name = "value")
    # Create the plot
    p1 <- ggplot(dataLong, aes(x = Length, y = value, fill = variable)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(
            name = "# of Trials") +
        scale_fill_manual(name = "Outcome", values = c("Failure" = "red", "Success" = "green")) +
        labs(title = gtitle1,
             x = "Length (Years)") +
        theme(legend.position = "top")

    p2 <- ggplot(data2, aes(x = Length, y = Cumulative)) +
        geom_line(color = "darkblue", size = 1.5) +
        labs(title = gtitle2,
             x = "Length (Years)",
             y = "Success (%)") +
        theme()
    out <- ggarrange(p1, p2,
                     ncol = 1, nrow = 2)
    return(out)
}

#' Helper function for the Distribution of Ages/Lengths Chart
#'
#' @param rawData A vector with the lengths of trials or the ages at death
#' @param gtitle Main title for the chart
#' @param xtitle Title of x-axis
#'
#' @return Chart object. One panel.
#'
#' @examples \dontrun{chartDistOfTimeSub(rawdata, gtitle, xtitle)}
chartDistOfTimeSub <- function(rawData, gtitle, xtitle) {
    data <- as.data.frame(table(rawData), stringsAsFactors = FALSE) %>%
        rename(EndAge = rawData) %>% mutate(Quartile = 1, EndAge = as.numeric(EndAge))
    qtiles <- round(c(quantile(rawData, 0.25), quantile(rawData, 0.5), quantile(rawData, 0.75)))
    qtilesY <- data[sapply(qtiles, function(x) which(data$EndAge == x)), "Freq"]
    idx <- data$EndAge > qtiles[1] & data$EndAge <= qtiles[2]
    data$Quartile[idx] <- 2
    idx <- data$EndAge > qtiles[2] & data$EndAge <= qtiles[3]
    data$Quartile[idx] <- 3
    idx <- data$EndAge > qtiles[3]
    data$Quartile[idx] <- 4
    data <- data %>% mutate(Quartile = as.factor(Quartile))

    out <- ggplot(data, aes(x = EndAge, y = Freq, color = Quartile, fill = Quartile)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(
            name = "# of Trials") +
        labs(title = gtitle,
             x = xtitle) +
        theme(legend.position = "top")  +
        annotate("text", x = qtiles, y = 0, label = qtiles, color = "black")
    return(out)
}

#' Distribution of Ages/Lengths Chart
#'
#' This chart shows how long trials last.
#'
#' @param sim Simulation object after running simulate function
#'
#' @return Chart object.  If nPersons is 0 or 1 there will be one panel representing
#' the lengths of the trials or the ages of Person 1.  If nPersons is 2, there will
#' be 3 panels to represent Person 1, Person 2 and the lengths.
#'
#' @export
#'
#' @examples \dontrun{chartDistOfTime(sim)}
chartDistOfTime <- function(sim) {
    if (nPersons.sim(sim) == 0) {
        return(chartDistOfTimeSub(sim$simulation$lengths, "Distribution of Lengths of Trials",
                                  "Length (Years)"))
    }
    makeName <- function(name, initials, num) {
        prefix <- "Distribution of Ages for"
        if (name != "") return(paste(prefix, name))
        if (initials != "") return(paste(prefix, initials))
        return(paste(prefix, "Person", num))
    }
    if (nPersons.sim(sim) == 1) {
        gtitle <- makeName(sim$persons[[1]]$name, sim$persons[[1]]$initials, 1)
        return(chartDistOfTimeSub(sim$simulation$agesDeath1, gtitle,
                                  "Age (Years)"))
    }
    if (nPersons.sim(sim) == 2) {
        gtitle <- makeName(sim$persons[[1]]$name, sim$persons[[1]]$initials, 1)
        p1 <- chartDistOfTimeSub(sim$simulation$agesDeath1, gtitle,
                                 "Age (Years)")
        gtitle <- makeName(sim$persons[[2]]$name, sim$persons[[2]]$initials, 2)
        p2 <- chartDistOfTimeSub(sim$simulation$agesDeath2, gtitle,
                                 "Age (Years)")
        p3 <- chartDistOfTimeSub(sim$simulation$lengths, "Distribution of Lengths of Trials",
                                 "Length (Years)")
        return(ggarrange(p1, p2, p3, ncol = 1, nrow = 3))
    }
}


