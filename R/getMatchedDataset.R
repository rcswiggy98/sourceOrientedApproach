#' A one sentence description of what your function does
#'
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces.
#'
#' @param inputParameter1 A description of the input parameter \code{inputParameter1}
#' @param inputParameter2 A description of the input parameter \code{inputParameter2}
#'
#' @return output A description of the object the function outputs
#'
#' @keywords keywords
#'
#' @export
#'
#' @examples
#' R code here showing how your function works

getMatchedDataset <- function(dataset, regions, covariates, exact.vars = NA,
                              exposure.cutoff = 4, caliper = "default"){

  require(data.table)
  require(MatchIt)

  #logit function
  logit <- function(p){
    y <- log(p/(1-p))
    return(y)
  }



  if(exposure != "HYSPLIT"){
    dataset$Exposure <- dataset$Exposure_InMAP
    dataset <- dataset[ , c("Exposure_HYSPLIT", "Exposure_InMAP") := NULL]
    #print("InMAP exposures used in analysis...")

  }
  if(exposure == "HYSPLIT"){
    dataset$Exposure <- dataset$Exposure_HYSPLIT
    dataset <- dataset[ , c("Exposure_HYSPLIT", "Exposure_InMAP") := NULL]
    #print("HYSPLIT exposures used in analysis...")
  }

  dataset$logPop <- log(dataset$TotPop)

  #only include complete records for all covariates
  dataset <- dataset[complete.cases(dataset),]


  dataset <- dataset[ , High := ifelse(Exposure > exposure.cutoff, 1, 0)]

  #only include certain regions
  if(! "all" %in% regions){
    dataset <- subset(dataset, region %in% regions)
  }

  #stop here if there is not High/Low exposures
  region.counts <- dataset[ , list(zipcodes = length(zip)), by = c("region","High")]
  region.counts <- region.counts[ , list(levels = length(zipcodes)), by = "region"]
  if(!all(region.counts$levels == 2)){
    print("The following regions do not have both exposure levels:\n")
    print(region.counts$region[which(region.counts$levels != 2)])
    print("Returning raw dataset...")
    dataset.matched = NA
    matched.model = NA
    matched <- list(dataset,dataset.matched, matched.model)
    names(matched) = c("dataset.raw", "dataset.matched", "model")
    return(matched)
  }

  formula.propensity.score <- as.formula(paste("High ~ ", paste(covariates,collapse = " + ")))
  model.propensity.score  <- glm(formula.propensity.score, data = dataset, family = binomial())
  dataset$prop.scores <- predict(model.propensity.score, type = "response")

  #save this dataset for later analysis
  dataset.raw <- dataset

  if(caliper == "default"){
    VarHigh <- var(logit(subset(dataset,High == 1)$prop.scores))
    VarLow <- var(logit(subset(dataset,High == 0)$prop.scores))
    pooled_Var <- 0.5*(VarHigh + VarLow)
    caliper <- 0.2*(sqrt(pooled_Var))
  }

  matched.model <- matchit(formula.propensity.score, data = dataset, method = "nearest",
                           distance = "logit", discard = "both", ratio = 1, exact = exact.vars,
                           caliper = caliper)
  dataset.matched <- data.table(match.data(matched.model))

  matched <- list(dataset.raw,dataset.matched, matched.model, caliper)
  names(matched) = c("dataset.raw", "dataset.matched", "model", "caliper")

  return(matched)
}
