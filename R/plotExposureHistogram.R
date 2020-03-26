#' @param model Any sublist of the returned list from analyzeMatches that contains the
#' named "raw" element (the raw and matched datasets, respectively)
#' @param title Title of the plot.
#' @return NA
plotExposureHistogram <- function(model, title = "Exposure histogram"){
  require(ggplot2)

  # extract the exposure variable, region, High
  dt <- model$raw[, c(2,18,24)]
  dt$High <- factor(dt$High, levels = c(0, 1), labels = c("Control", "Exposed"))

  p <- ggplot(dt, aes_string(x=names(dt)[1], fill="High")) +
      geom_histogram() +
      ggtitle(title) +
      theme_bw() +
      theme(axis.line = element_line(size=1, colour = "black"),
            panel.grid.major = element_line(colour = "#d3d3d3"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(), panel.background = element_blank(),
            plot.title = element_text(size = 14, face = "bold"),
            text=element_text(),
            axis.text.x=element_text(colour="black", size = 9),
            axis.text.y=element_text(colour="black", size = 9)) +
      facet_grid(. ~ region, scales = "free")

  p
}
