#' Create a plot of the standardized mean differences in covariates
#'
#' This function takes a matchitobject and returns a plot of the standardized
#' mean differences (SMD) between the high exposed and control locations for
#' each covariate in the propensity score model.  The SMD is a common way to
#' evalulate whether covariates were balanced between the two groups during matching.
#' Code for this plot was adopted from a vignette to the R tableone package (Yoshida and Bohn).
#'
#' @param matched.model A matchitobject returned from the matchit function. \code{matched.model}

#'
#' @return NA
#'
#' @keywords keywords
#'
#' @export
#'
#' @references
#' \insertRef{austin2011introduction}{sourceOrientedApproach}
#'
#' \insertRef{yoshidapackage}{sourceOrientedApproach}
#'
#' \insertRef{ho2011matchit}{sourceOrientedApproach}
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
#' createSMDplot(dataset$matched.model)

createSMDplot <- function(matched.model){
  require(ggplot2)
  require(MatchIt)

  sum.all <-  summary(matched.model, standardize = TRUE)[[3]]
  SMD.vars <- rownames(sum.all)[-1]
  SMD.all <- sum.all[-1,4]
  SMD.matched <- summary(matched.model, standardize = TRUE)$sum.matched[-1,4]
  SMD.vars <- SMD.vars[!SMD.all  %in% c(-Inf, Inf)]
  SMD.matched <- SMD.matched[!SMD.all  %in% c(-Inf, Inf)]
  SMD.all <- SMD.all[!SMD.all %in% c(-Inf, Inf)]

  dataPlot <- data.frame(variable = SMD.vars,
                         Before = SMD.all,
                         After = SMD.matched)
  dataPlotMelt <- melt(data = dataPlot,
                       id.vars = c("variable"),
                       variable.name = "Dataset",
                       value.name = "SMD")
  varNames <- as.character(dataPlot$variable)[order(dataPlot$Before)]
  dataPlotMelt$variable <- factor(dataPlotMelt$variable, levels = varNames)

  cbbPalette <- c("#E69F00", "#0072B2", "#D55E00", "#CC79A7", "#56B4E9", "#009E73","#F0E442")

  x.lab <- "Standardized Mean Difference \n (High/Low)"

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
