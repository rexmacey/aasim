#' Success Donut Chart
#'
#' Produces a simple donut chart showing the number and percent of successes
#' and failures.
#'
#' @param sim Simulation object after running simulate function
#' @param vs Choices are 'T' for target or "Z' for zero
#' @param simResult Name of item in sim with results (class = 'simResult')
#'
#' @return a ggplot object
#' @export
#'
#' @examples \dontrun{chartSucessDonut(sim)}
chartSuccessDonut <- function(sim, vs = "T", simResult = "simulation") {
  successStats <- getSuccessStats(sim, simResult)

  # Create a data frame with Success and Failure counts
  vs <- toupper(substr(vs, 1, 1))
  if (vs == "T") {
      data <- data.frame(
          Legend = c("Success", "Failure"),
          count = c(successStats$vsTargetCount, sim[[simResult]]$nTrials - successStats$vsTargetCount))
      gtitle <- "Success Rate vs Target"
  } else {
      data <- data.frame(
          Legend = c("Success", "Failure"),
          count = c(successStats$vs0Count, sim[[simResult]]$nTrials - successStats$vs0Count))
      gtitle <- "Success Rate vs $0"
  }


  # Calculate the percentage
  data <- data %>%
      dplyr::mutate(percentage = count / sum(count) * 100)

  # Create the donut chart
  # out <- ggplot(data, aes(x = 2, y = count, fill = Legend)) +
  #     geom_bar(stat = "identity", width = 1) +
  #     coord_polar(theta = "y") +
  #     xlim(0.5, 2.5) +
  #     geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")),
  #               position = position_stack(vjust = 0.5)) +
  #     theme_void() +
  #     theme(legend.position = "right") +
  #     labs(title = gtitle)
  out <- plot_ly(data, labels = ~Legend, values = ~count, type = 'pie', hole = 0.6,
                 textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      plotly::layout(title = gtitle,
             margin = list(t = 100),
             showlegend = TRUE,
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.1))
  return(out)
}

#' Values across Time vs Target Chart
#'
#' Produces a ribbon chart.  There's a line for the median value of all the
#' trials at each year. Additionally there are 50% and 90% confidence intervals.
#' A black line shows the value of the target value.
#'
#' @param sim Simulation object after running simulate function
#' @param logScale TRUE to display a log scale on the y-axis, FALSE to display a linear scale.
#' @param simResult Name of item in sim with results (class = 'simResult')
#' @param inflationAdjusted TRUE to adjust portfolio values by inflation, otherwise FALSE.
#'
#' @return ggplot2 object
#' @export
#'
#' @examples \dontrun{chartValuesOverTime(sim)}
chartValuesOverTime <- function(sim, logScale = FALSE, simResult = "simulation", inflationAdjusted = FALSE) {
    targetValues <- getTargetValues(sim, simResult)
    data <- cbind(getDistOfValuesByYear(sim, c(0.05, 0.25, 0.50, 0.75, 0.95), simResult, inflationAdjusted), Target = getTargetValues(sim, simResult))
    # cols <- c("Median" = "red", "Target" = "black")
    # fills <- c("90%" = "lightblue", "50%" = "blue")
    # out <- ggplot(data, aes(x = Length)) +
    #     geom_ribbon(aes(ymin= p5, ymax = p95, fill = "90%"), alpha = 0.9, show.legend = TRUE) +
    #     geom_ribbon(aes(ymin= p25, ymax = p75, fill = "50%"), alpha = 0.2, show.legend = TRUE) +
    #     geom_line(aes(y = p50, color = "Median"), linewidth = 1.5, show.legend = TRUE) +
    #     geom_line(aes(y = Target, color = "Target"), linewidth = 1.25, show.legend = TRUE) +
    #     scale_y_continuous(labels = dollar) +
    #     scale_fill_manual(values = fills) +
    #     scale_color_manual(values = cols) +
    #     guides(color = guide_legend(override.aes = list(fill = NA))) +
    # labs(title = "Median Values across Time vs. Target",
    #      x = "Year",
    #      y = "Value ($)",
    #      fill = "Confidence Intervals",
    #      color = "Lines")
    if (logScale) {
        data <- data %>% mutate(across(c(p5, p25, p50, p75, p95, Target), ~ ifelse(. == 0, 0.01, .)))
        yaxisRange <- c(log10(1), log10(max(data$p95)))
    } else {
        yaxisRange <- c(NA, NA)
    }
    out <- plot_ly(data, x = ~Length) %>%
        add_ribbons(ymin = ~p5, ymax = ~p95, name = "90%", fillcolor = "lightblue", opacity = 0.9) %>%
        add_ribbons(ymin = ~p25, ymax = ~p75, name = "50%", fillcolor = "blue", opacity = 0.2) %>%
        add_lines(y = ~p50, name = "Median", line = list(color = "red", width = 1.5)) %>%
        add_lines(y = ~Target, name = "Target", line = list(color = "black", width = 1.25)) %>%
        plotly::layout(title = "Portfolio Values across Time vs. Target",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Value ($)", tickformat = "$,.0f",
                            range = yaxisRange,
                            type = ifelse(logScale, "log", "linear")),
               legend = list(title = list(text = "Legend")),
               showlegend = TRUE)
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
#' @param simResult Name of item in sim with results (class = 'simResult')
#'
#' @return ggplot2 object
#' @export
#'
#' @examples \dontrun{(sim)}
chartSuccessOverTime <- function(sim, vs = "T", simResult = "simulation") {
    if (toupper(substr(vs, 1, 1)) == "T") {
        data1 <- getSuccessByLength(sim, simResult) %>% rename(Success = SuccessVsTargetCount) %>%
            mutate(PctSuccess = SuccessVsTargetPct , Failure = Frequency - Success) %>%
            select(Length, Failure, Success, PctSuccess)
        data2 <- getSuccessByLength(sim, simResult) %>%
            mutate(Cumulative = SuccessVsTargetCumPct) %>%
            select(Length, Cumulative)
        gtitle1 <- "Successes and Failures vs Target by Length of Trial"
        gtitle2 <- "Cumulative Success Rate vs Target by Length of Trial"
    } else {
        data1 <- getSuccessByLength(sim, simResult) %>% rename(Success = SuccessVs0Count) %>%
            mutate(PctSuccess = SuccessVs0Pct , Failure = Frequency - Success) %>%
            select(Length, Failure, Success, PctSuccess)
        data2 <- getSuccessByLength(sim, simResult) %>%
            mutate(Cumulative = SuccessVs0CumPct) %>%
            select(Length, Cumulative)
        gtitle1 <- "Successes and Failures vs $0 by Length of Trial"
        gtitle2 <- "Cumulative Success Rate vs $0 by Length of Trial"
    }
    dataLong <- reshape2::melt(data1, id.vars = c("Length", "PctSuccess"),
                               variable.name = "variable", value.name = "value")
    # Create the plot
    p1 <- plot_ly(dataLong, x = ~Length, y = ~value, type = 'bar', color = ~variable,
                  colors = c("Failure" = "red", "Success" = "green")) %>%
        plotly::layout(xaxis = list(title = "Length (Years)"),
               yaxis = list(title = "# of Trials"),
               barmode = 'stack',
               legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = 1.1)) %>%
        add_annotations(
            x = .2,
            y = 1,
            xref = "paper",
            yref = "paper",
            text = gtitle1,
            showarrow = FALSE
        )

    p2 <- plot_ly(data2, x = ~Length, y = ~Cumulative, type = 'scatter', mode = 'lines',
                  line = list(color = 'darkblue', width = 1.5), showlegend = F) %>%
        plotly::layout(xaxis = list(title = "Length (Years)"),
               yaxis = list(title = "Cumulative Success (%)",
                            range = c(0, 100))) %>%
        add_annotations(
            x = median(data2$Length),
            y = (min(data2$Cumulative) + max(data2$Cumulative)) / 2,
            xref = "x",
            yref = "y",
            text = gtitle2,
            showarrow = FALSE
        )

    # Combine the plots
    out <- subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(title = "")
    # annotations <- list(
    #     list(x = 0.2,
    #          y = 1,
    #          text = gtitle1,
    #          xref = "paper",
    #          yref = "paper",
    #          xanchor = "center",
    #          yanchor = "bottom",
    #          showarrow = FALSE),
    #     list(x = 0.2,
    #          y = .43,
    #          yshift = 0,
    #          text = gtitle2,
    #          xref = "paper",
    #          yref = "paper",
    #          xanchor = "center",
    #          yanchor = "middle",
    #          showarrow = TRUE)
    #
    # )
    return(out)
}

#' Helper function for the Distribution of Ages/Lengths Chart
#'
#' @param rawData A vector with the lengths of trials or the ages at death
#' @param gtitle Main title for the chart
#' @param xtitle Title of x-axis
#' @param showLegend TRUE to show the legend, FALSE to hide it.
#'
#' @return Chart object. One panel.
#'
#' @examples \dontrun{chartDistOfTimeSub(rawdata, gtitle, xtitle)}
chartDistOfTimeSub <- function(rawData, gtitle, xtitle, showLegend = TRUE) {
    data <- as.data.frame(table(rawData), stringsAsFactors = FALSE) %>%
        rename(EndAge = rawData) %>% mutate(Quartile = "Q1", EndAge = as.numeric(EndAge))
    qtiles <- round(c(quantile(rawData, 0.25), quantile(rawData, 0.5), quantile(rawData, 0.75)))
    qtilesY <- data[sapply(qtiles, function(x) which(data$EndAge == x)), "Freq"]
    idx <- data$EndAge > qtiles[1] & data$EndAge <= qtiles[2]
    data$Quartile[idx] <- "Q2"
    idx <- data$EndAge > qtiles[2] & data$EndAge <= qtiles[3]
    data$Quartile[idx] <- "Q3"
    idx <- data$EndAge > qtiles[3]
    data$Quartile[idx] <- "Q4"
    data <- data %>% mutate(Quartile = as.factor(Quartile))

    # out <- ggplot(data, aes(x = EndAge, y = Freq, color = Quartile, fill = Quartile)) +
    #     geom_bar(stat = "identity") +
    #     scale_y_continuous(
    #         name = "# of Trials") +
    #     labs(title = gtitle,
    #          x = xtitle) +
    #     theme(legend.position = "top")  +
    #     annotate("text", x = qtiles, y = 0, label = qtiles, color = "black")
    out <- plot_ly(data, x = ~EndAge, y = ~Freq,
        type = 'bar',
        color = ~Quartile,
        colors = "Set1",
        showlegend = showLegend,
        legendgroup = ~EndAge) %>%
        plotly::layout(title = "", #gtitle
                       margin = list(t = 100),
               xaxis = list(title = xtitle),
               yaxis = list(title = "# of Trials"),
               # legend = list(orientation = 'h', x = 0.5, xanchor = 'left', y = 1.1, title = "Quartile"),
               annotations = list(
                   list(x = qtiles[1], y = 0, text = as.character(qtiles[1]), showarrow = FALSE, font = list(color = 'black')),
                   list(x = qtiles[2], y = 0, text = as.character(qtiles[2]), showarrow = FALSE, font = list(color = 'black')),
                   list(x = qtiles[3], y = 0, text = as.character(qtiles[3]), showarrow = FALSE, font = list(color = 'black')),
                   list(x = min(data$EndAge), y = max(data$Freq), text = gtitle, showarrow = FALSE, font = list(color = 'black'))
               ))
    return(out)
}

#' Distribution of Ages/Lengths Chart
#'
#' This chart shows how long trials last.
#'
#' @param sim Simulation object after running simulate function
#' @param simResult Name of item in sim with results (class = 'simResult')
#'
#' @return Chart object.  If nPersons is 0 or 1 there will be one panel representing
#' the lengths of the trials or the ages of Person 1.  If nPersons is 2, there will
#' be 3 panels to represent Person 1, Person 2 and the lengths.
#'
#' @export
#'
#' @examples \dontrun{chartDistOfTime(sim)}
chartDistOfTime <- function(sim, simResult = "simulation") {
    if (nPersons.sim(sim) == 0 | sim$returnGeneratorMethod == "C") {
        return(chartDistOfTimeSub(sim[[simResult]]$lengths, "Distribution of Lengths of Trials",
                                  "Length (Years)", TRUE))
    }
    makeName <- function(name, initials, num) {
        prefix <- "Distribution of Ages for"
        if (name != "") return(paste(prefix, name))
        if (initials != "") return(paste(prefix, initials))
        return(paste(prefix, "Person", num))
    }
    if (nPersons.sim(sim) == 1) {
        gtitle <- makeName(sim$persons[[1]]$name, sim$persons[[1]]$initials, 1)
        return(chartDistOfTimeSub(sim[[simResult]]$agesDeath1, gtitle,
                                  "Age (Years)", TRUE))
    }
    if (nPersons.sim(sim) == 2) {
        gtitle <- makeName(sim$persons[[1]]$name, sim$persons[[1]]$initials, 1)
        p1 <- chartDistOfTimeSub(sim[[simResult]]$agesDeath1, gtitle,
                                 "Age (Years)", FALSE)
        gtitle <- makeName(sim$persons[[2]]$name, sim$persons[[2]]$initials, 2)
        p2 <- chartDistOfTimeSub(sim[[simResult]]$agesDeath2, gtitle,
                                 "Age (Years)", FALSE)
        p3 <- chartDistOfTimeSub(sim[[simResult]]$lengths, "Distribution of Lengths of Trials",
                                 "Length (Years)", TRUE)
        # return(ggarrange(p1, p2, p3, ncol = 1, nrow = 3))
        out <- subplot(p1, p2, p3, nrows = 3, shareX = FALSE, titleX = TRUE, titleY = TRUE)
        return(out)
    }
}

#' Chart a Sample of the Trials (Wealth Over Time)
#'
#' This will produce a chart with a sampleSize number of lines representing
#' individual trials.
#'
#' @param sim Simulation object
#' @param sampleSize Size of random sample
#' @param logScale True to display the y-axis in log format
#' @param simResult Name of item in sim with results (class = 'simResult')
#' @param inflationAdjusted TRUE to adjust portfolio values by inflation, otherwise FALSE.
#'
#' @return Chart object (plotly)
#' @export
#'
#' @examples \dontrun{chartRandomSmapleTrialsPortfolioValues(sim, sampleSize, logScale))}
chartRandomSampleTrialsPortfolioValues <- function(sim, sampleSize, logScale = TRUE,
                                                   simResult = "simulation",
                                                   inflationAdjusted = FALSE) {
    lenPV <- length(sim[[simResult]]$portfolioValues)
    if (sampleSize > lenPV) sampleSize <- lenPV
    idxSmpl <- sample(1:lenPV, sampleSize)
    return(chartSampleTrialsPortfolioValues(sim, idxSmpl, logScale, simResult, inflationAdjusted))
}

#' Chart a Sample of the Trials (Wealth Over Time) Given a Vector of Indices
#'
#' This will produce a chart with a length(sampleIndex) number of lines representing
#' individual trials. The sampleIndex is a vector with the numbers of the trials
#' to display.  For example, to plot the first hundred trials, sampleIndex would
#' equal 1:100.
#'
#' @param sim Simulation object
#' @param sampleIndex Vector of indices of the trials to plot.
#' @param logScale True to display the y-axis in log format
#' @param simResult Name of item in sim with results (class = 'simResult')
#' @param inflationAdjusted TRUE to adjust portfolio values by inflation, otherwise FALSE.
#'
#' @return Chart object (plotly)
#' @export
#'
#' @examples \dontrun{chartSampleTrialsPortfolioValues(sim, sampleIndex, logScale)}
chartSampleTrialsPortfolioValues <- function(sim, sampleIndex, logScale = FALSE,
                                             simResult = "simulation",
                                             inflationAdjusted = FALSE) {
    # convert_to_hex <- function(color) {
    #     rgb_vals <- col2rgb(color)
    #     rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
    # }
    if (inflationAdjusted) {
        pv <- inflationAdjustPortfolioValues(sim, simResult)
    } else {
        pv <- sim[[simResult]]$portfolioValues
    }
    smpl <- pv[sampleIndex]
    smplLengths <- sapply(smpl, length)
    max_length <- max(smplLengths)
    smplSuccessVsTarget <- getSuccessStats(sim, simResult)$successVsTargetByTrial[sampleIndex] # T/F by trial
    padded_smpl <- lapply(smpl, function(x) {
        length(x) <- max_length
        return(x)
    })
    #df <- data.frame(t(data.frame(padded_smpl))) %>% mutate(ID = row_number(), Success = smplSuccessVsTarget)
    df <- data.frame(t(data.frame(padded_smpl))) %>% mutate(ID = row_number(), Color = ifelse(smplSuccessVsTarget, "green", "red"))
    names(df) <- c(paste0("Y",0:(max_length - 1)), "ID", "Color")
    rownames(df) <- NULL

    # Reshape the data frame to long format
    df_long <- df %>%
        # tidyr::pivot_longer(cols = -ID, names_to = "Time", values_to = "Wealth") %>%
        reshape2::melt(id.vars = c("ID", "Color"), variable.name = "Time", values_to = "Wealth", factorsAsStrings = TRUE) %>%
        dplyr::mutate(Time = as.character(Time)) %>% rename(Wealth = value) %>%
        dplyr:: mutate(Time = as.numeric(substring(Time, 2, nchar(Time)))) %>%
        dplyr::filter(!is.na(Wealth)) %>% group_by(ID)

    df_long <- rbind(df_long,
                     data.frame(ID = rep(0, max_length),
                                Color = rep("blue", max_length),
                                Time = 0:(max_length - 1),
                                Wealth = getTargetValues(sim, simResult)[1:max_length]))

    if (logScale) {
        df_long[df_long$Wealth < 1, "Wealth"] <- log10(10)
        yaxisRange <- c(log10(1), log10(max(df_long$Wealth)))
    } else {
        yaxisRange <- c(NA, NA)
    }

    # Plot
    color_definitions <- c("red" = "#FF0000", "green" = "#00FF00", "blue" = "#0000FF")

    # out <- plot_ly(df_long %>% dplyr::filter(ID != 0), x = ~Time, y = ~Wealth, type = 'scatter',
    #                mode = 'lines',
    #                color = ~Color, colors = color_definitions,
    #                line = list(width = 0.25)) %>%
    #     plotly::layout(title = paste0("Wealth Over Time (Sample of ", length(sampleIndex), " Trials)"),
    #                    xaxis = list(title = "Time (Years)"),
    #                    yaxis = list(title = "Wealth ($)",
    #                                 type = ifelse(logScale, "log", "linear"),
    #                                 range = yaxisRange),
    #                    showlegend = FALSE)
    out <- plot_ly(df_long, x = ~Time, y = ~Wealth, type = 'scatter',
                   mode = 'lines',
                   color = ~Color, colors = color_definitions,
                   line = list(width = 0.25)) %>%
        plotly::layout(title = paste0("Wealth Over Time (Sample of ", length(sampleIndex), " Trials)"),
                       xaxis = list(title = "Time (Years)"),
                       yaxis = list(title = "Wealth ($)",
                                    type = ifelse(logScale, "log", "linear"),
                                    range = yaxisRange),
                       showlegend = FALSE)

    # out <- out %>%
    #     add_trace(x = df_long %>% dplyr::ungroup() %>% dplyr::filter(ID == 0) %>% dplyr::select(Time),
    #               y = df_long %>% dplyr::ungroup() %>% dplyr::filter(ID == 0) %>% dplyr::select(Wealth),
    #               type = 'scatter',
    #               mode = 'lines',
    #               color = "blue", colors = color_definitions,
    #              line = list(width = 1))

    return(out)
}

