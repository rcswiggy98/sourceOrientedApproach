#' Makes propensity score standardized mean difference ggplot2::ggplot plots
#'
#' @param plot Any element (a data.table) of the "dataPlots" named sublist returned by createStratSMD \code{plot}
#' @param a.melt Any element (a melted data.table) "dataPlotMelts" named sublist returned by createStratSMD \code{a.melt}
#' @return NA
plotStratSMD <- function(plot, a.melt){
  require(ggplot2)
  cbbPalette <- c("#E69F00", "#0072B2", "#D55E00", "#CC79A7", "#56B4E9", "#009E73","#F0E442")
  x.lab <- "Standardized Mean Difference \n (High/Low)"

  dataPlot <- plot
  dataPlotMelt <- a.melt
  browser()
  varNames <- as.character(dataPlot$variable)[order(dataPlot$Before)]
  dataPlotMelt$variable <- factor(dataPlotMelt$variable, levels = varNames)

  ggplot(data = dataPlotMelt, mapping = aes(x = variable,
                                            y = SMD,
                                            group = Dataset,
                                            color = Dataset)) +
    scale_colour_manual(values=cbbPalette) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = c(0), color = "black", size = 0.1) +
    coord_flip(ylim = c(-2,2)) +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman"),
          legend.key = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=10),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=12),
          legend.text = element_text(size=12),
          legend.title = element_blank(),
          legend.position = "right",
          plot.title = element_text(size = 12, hjust = 0.5)) +
    labs(y = x.lab, title = "")
}
