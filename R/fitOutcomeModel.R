#' Fit a Poisson regression model to the health outcome data
#'
#' This function takes a dataset from the getMatchedDataset function and fits
#' a Poisson regression model.  There are two types of analyses that can be performed.
#' First, using the matched dataset, a matched analysis can be performed.  Second,
#' using the raw dataset, a stratified analysis can be performed. log(offset.var) is
#' used as an offset in the model.
#'
#' @param dataset A data table.  If a matched analysis is performed, then this should be
#' the matched dataset.  If a stratified analysis is performed, this should be the raw
#' dataset. \code{dataset}
#' @param outcome A data table containing the outcome data.  The first column should be
#' the ZIP code.\code{outcome}
#' @param covariate.vars A character vector containing the names of covariates in the dataset
#' to adjust for in the outcome model. Must be a subset of the colnames of dataset. \code{covariate.vars}
#' @param outcome.var The name of the outcome variable in the outcome data table. \code{outcome.var}
#' @param offset.var The name of the variable in the outcome data table to use as an offset in the
#' Poisson regression model.  Typically, this is the number of person years observed. \code{offset.var}
#' @param analysis.type The type of analysis to perform.  Either "matched" or "stratified".  A
#' "matched" analysis should use the matched dataset.  A "stratified" analysis should use the raw
#' dataset. \code{analysis.type}
#'
#' @return An object of the glm class.
#'
#' @keywords keywords
#'
#' @export
#'
#' @examples
#' data('ihd2005')
#' data('inmap2005')
#' data('covariates')
#'
#' # Include these regions
#' regions <- c("IndustrialMidwest", "Northeast", "Southeast")
#'
#' # Covariates to adjust for using propensity score matching
#' covariate.vars <- c("logPop", "PctUrban","MedianHHInc", "PctPoor", "smokerate2000")
#'
#' dataset <- getMatchedDataset(exposure = inmap2005, covariates, covariate.vars, regions)
#'
#' fitOutcomeModel(dataset$matched, ihd2005, covariate.vars)


# analysis.type = c("matched","stratified")
fitOutcomeModel <- function(dataset, outcome, covariate.vars, outcome.var = "IHD",
                            offset.var = "person_years", analysis.type = "matched"){

  require(data.table)
  setkey(outcome, zip)
  dataset <- merge(dataset,outcome, by = "zip")
  dataset <- dataset[complete.cases(dataset), ]

  if(analysis.type == "stratified"){
    #Discard observations outside mutual support of high and low propensity scores

    ps_low <- max(min(subset(dataset, High == 1)$prop.scores), min(subset(dataset, High == 0)$prop.scores))
    #ps_low <- max(min(dataset[High == 1,]$prop.scores),min(dataset[High == 0,]$prop.scores))

    ps_high <- min(max(subset(dataset, High == 1)$prop.scores),max(subset(dataset, High == 0)$prop.scores))
    dataset <- subset(dataset, prop.scores >= ps_low & prop.scores <= ps_high)

    #Find PS quantiles and stratify into 5 groups
    PS_quantiles <- quantile(dataset$prop.scores, c(0.20,0.40,0.60,0.80))
    dataset$PS_group <- as.factor(findInterval(dataset$prop.scores, PS_quantiles))

    outcome.formula <- as.formula(paste(outcome.var, "~ High +",paste(c(covariate.vars,"PS_group"),collapse = " + ")))
  }

  if(analysis.type != "stratified"){
    outcome.formula <- as.formula(paste(outcome.var, "~ High +",paste(covariate.vars,collapse = " + ")))
  }

  outcome.model <- glm(outcome.formula,
                       family = poisson(),
                       offset = log(dataset[[offset.var]]),
                       data = dataset)
  return(outcome.model)
}
