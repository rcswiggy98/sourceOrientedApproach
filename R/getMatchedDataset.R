#' Obtain a propensity score matched dataset
#'
#' This function is designed for use with the analysis performed in
#' A Source Oriented Approach to Coal Emissions Health Effects.  Specifically,
#' this function dichotomizes the exposure variable (typically, coal emissions)
#' based upon a specified percentile cutoff resulting in ZIP codes being classified
#' as either highly exposed or control locations. Using the MatchIt package, high exposed
#' ZIP codes are matched to control locations with similar propensity scores.
#' A caliper can be specified, which is the maximum allowed difference in
#' propensity scores when
#' selecting matches.  The default caliper is 20 percent of the pooled standard
#' deviation of the logit of the propensity scores of the high exposed and
#' control locations, as suggested in Austin 2011.  Categorical variables
#' can be matched on exactly.
#'
#' @param exposure A data table.  The first column must be the unit identifier (for example, 5-digit U.S. zip code)
#' and the second column is the exposure of interest \code{exposure}
#' @param covariates A data table. The first column must be the unit identifier (for example, 5-digit U.S. zip code).
#' Subsequent columns are covariates to include in the propensity score model. \code{covariates}
#' @param covariate.vars A character vector containing a subset of
#'the covariate column names to include in the propensity score model. \code{covariate.vars}
#' @param regions A character vector containing the geographic regions
#' ("Northeast", "Southeast", etc.) to include in the analysis. Defaults to
#' all regions. \code{regions}
#' @param exact.vars A character vector of colnames in covariates to match exactly
#' on.  This variable cannot be a continuous variable.\code{exact.vars}
#' @param exposure.cutoff.percentile A numeric value between 0 and 1 specifying the
#' cutoff, in terms of percentile of the exposure distribution, between high exposed
#' and controls locations.
#' @param caliper A numeric value specifying the maximum allowable difference
#' in propensity scores when performing matching.
#'
#' @return output A list containing the raw dataset, the matched dataset,
#' a matchitobject, the caliper, and the high/control cutoff.
#'
#' @keywords keywords
#'
#' @export
#'
#' @references
#' \insertRef{austin2011introduction}{sourceOrientedApproach}
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


getMatchedDataset <- function(exposure, covariates, covariate.vars, regions = "all",
                              exact.vars = NULL, exposure.cutoff.percentile = 0.80, caliper = "default"){

  require(MatchIt)
  require(data.table)
  exposure <- data.table(exposure)
  covariates <- data.table(covariates)

  setkey(exposure,zip)
  setkey(covariates, zip)
  exposure.var <- colnames(exposure)[2]

  if(!is.null(exact.vars)){
    if(is.numeric(dataset[ , get(exact.vars)])){
    print("Exact matching variables should be categorical. No exact matching performed.")
    exact.vars = NULL
    }
  }

  if(length(covariate.vars) == 1 & "all" %in% covariate.vars){
    covariate.vars <- colnames(covariates)[-1]
  }
  if(!all(covariate.vars %in% colnames(covariates))){
    print("Invalid covariate.var...must be a colname of dataset")
    print("Continue with valid names.")
    covariate.vars <- covariate.vars[which(covariate.vars %in% colnames(covariates))]
  }


  #join two data tables
  #dataset <- exposure[covariates, nomatch = 0]
  #dataset <- exposure[covariates, ]
  dataset <- merge(exposure, covariates, by = "zip")

  #Only use zip codes with all covariates
  dataset <- dataset[complete.cases(dataset), ]

  #dichotomize exposure ~ note this is done before dropping other regions
  cutoff <- quantile(dataset[[exposure.var]], exposure.cutoff.percentile)
  dataset$High <- ifelse(dataset[[exposure.var]] > cutoff,1,0 )

  #subset by regions ~ note this is done after dichotomizing the exposure
  if(!"all" %in% regions){
    if(all(regions %in% unique(dataset$region))){
      dataset <- subset(dataset, region %in% regions)
    } else{
      print("Invalid region selected...proceeding with all.")
    }
  }

  #NEED TO THINK ABOUT WHAT TO DO WHEN MATCHING IS EXACT AND THERE ARE NOT HIGH/LOW IN EACH REGION
  #stop here if there is not High/Low exposures
  # region.counts <- dataset[ , list(zipcodes = length(zip)), by = c("region","High")]
  # region.counts <- region.counts[ , list(levels = length(zipcodes)), by = "region"]
  # if(!all(region.counts$levels == 2)){
  #   print("The following regions do not have both exposure levels:\n")
  #   print(region.counts$region[which(region.counts$levels != 2)])
  #   print("Returning raw dataset...")
  #   dataset.matched = NA
  #   matched.model = NA
  #   matched <- list(dataset,dataset.matched, matched.model)
  #   names(matched) = c("dataset.raw", "dataset.matched", "model")
  #   return(matched)
  # }

  #get propensity scores from logistic regression
  formula.propensity.score <- as.formula(paste("High ~ ", paste(covariate.vars,collapse = " + ")))
  model.propensity.score  <- glm(formula.propensity.score, data = dataset, family = binomial())
  dataset$prop.scores <- predict(model.propensity.score, type = "response")

  #save this dataset for later analysis
  dataset.raw <- dataset

  #default caliper is 20% of the pooled standard deviation of the logit of the propensity score
  if(caliper == "default"){
    #logit function
    logit <- function(p){
      y <- log(p/(1-p))
      return(y)
    }
    VarHigh <- var(logit(subset(dataset,High == 1)$prop.scores))
    VarLow <- var(logit(subset(dataset,High == 0)$prop.scores))
    pooled_Var <- 0.5*(VarHigh + VarLow)
    caliper <- 0.2*(sqrt(pooled_Var))
  }

  matched.model <- matchit(formula.propensity.score, data = dataset, method = "nearest",
                           distance = "logit", discard = "both", ratio = 1, exact = exact.vars,
                           caliper = caliper)
  dataset.matched <- data.table(match.data(matched.model))

  matched <- list(dataset.raw,dataset.matched, matched.model, caliper, cutoff)
  names(matched) = c("raw", "matched", "match.model", "caliper", "cutoff")

  return(matched)
}
