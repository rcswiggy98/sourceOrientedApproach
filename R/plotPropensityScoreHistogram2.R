##' Plot a histogram of the propensity score by exposure group AND raw or matched using
##' ggplot2::geom_histogram()
#'
#' This method plots a histogram of the propensity scores for the high exposed
#' and control locations.
#'
#' @param model Any sublist of the returned list from analyzeMatches that contains the
#' named "raw" and "matched" elements (the raw and matched datasets, respectively)
#' @param main Title for the plot.
#'
#' @return NA
plotPropensityScoreHistogram2 <- function(model, main = "Propensity score histogram"){
  require(ggplot2)
  require(gridExtra)
  require(data.table)
  # require(dplyr)

  matched <- model$matched[, .(prop.scores, matched = "matched")]
  unmatched <- model$raw[, .(prop.scores, matched = "unmatched")]
  # unmatched <- data.table(anti_join(model$raw, model$matched, by="zip"))[, .(prop.scores, matched = "unmatched")]
  dt <- rbind(matched, unmatched)

  p<- ggplot(dt, aes(x=prop.scores, fill=matched, group=matched)) + geom_histogram() + ggtitle(main)

  p
}
