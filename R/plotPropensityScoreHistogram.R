#' Plot a histogram of the propensity score by exposure group
#'
#' This function plots a histogram of the propensity scores for the high exposed
#' and control locations.
#'
#' @param dataset A data table.  Two of the column names must be High and prop.scores. \code{dataset}
#' @param main Title for the plot. \code{main}
#'
#' @return NA
#'
#' @keywords keywords
#'
#' @export
#'
#'
#' @examples
#' # Include these regions
#' regions <- c("IndustrialMidwest", "Northeast", "Southeast")
#'
#' # Covariates to adjust for using propensity score matching
#' covariate.vars <- c("logPop", "PctUrban","MedianHHInc", "PctPoor", "smokerate2000")
#'
#' dataset <- getMatchedDataset(exposure = inmap2005, covariates, covariate.vars, regions)
#'
#' plotPropensityScoreHistogram(dataset$raw)



plotPropensityScoreHistogram <- function(dataset, main = ""){
  #function used for color markers in map
  makeTransparent = function(..., alpha=0.5) {
    if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
    alpha = floor(255*alpha)
    newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
    .makeTransparent = function(col, alpha) {
      rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
    }
    newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
    return(newColor)
  }

  prop_hist <- hist(dataset$prop.scores, plot = FALSE, breaks = 50)
  hist(subset(dataset, High ==1)$prop.scores, xlim = c(0,1),
       col=makeTransparent("red", alpha=0.7), breaks = 50,
       main = main, xlab = "Propensity Score",
       ylim = c(0,max(prop_hist$counts)))
  hist(subset(dataset, High ==0)$prop.scores, breaks = 50,
       col=makeTransparent("blue", alpha=0.7),add = T)
  legend(0.5,max(prop_hist$counts), c("High Exposed","Low Exposed"), fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),
           cex = 1, bty = "n", xjust = 0.5)
}
