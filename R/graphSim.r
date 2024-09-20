#' Success Donut Chart
#'
#' Produces a simple donut chart showing the number and percent of successes
#' and failures.
#'
#' @param sim Simulation object after running simulate function
#'
#' @return a ggplot object
#' @export
#'
#' @examples \dontrun{chartSucessDonut(sim)}
chartSuccessDonut <- function(sim) {
  successStats <- getSuccessStats(sim)

  # Create a data frame with Success and Failure counts
  data <- data.frame(
      Legend = c("Success", "Failure"),
      count = c(successStats$vsTargetCount, sim$nTrials - successStats$vsTargetCount)
  )

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
      labs(title = paste0(sim$description, ": Success vs Failure"))
  return(out)
}

#' Values across Time vs Target
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
    out <- ggplot(data, aes(x = Length)) +
        geom_ribbon(aes(ymin= p5, ymax = p95, fill = "90% CI"), alpha = 0.9, show.legend = TRUE) +
        geom_ribbon(aes(ymin= p25, ymax = p75, fill = "50% CI"), alpha = 0.2, show.legend = TRUE) +
        geom_line(aes(y = p50), color = "red", show.legend = TRUE) +
        geom_line(aes(y = Target), color = "black", show.legend = TRUE) +
        scale_y_continuous(labels = dollar) +
        scale_fill_manual(values = c("90% CI" = "lightblue", "50% CI" = "blue")) +
    labs(title = paste0(sim$description, ": Median Values across Time vs. Target"),
         x = "Year",
         y = "Value ($)",
         fill = "Confidence Interval")
    return(out)
}

