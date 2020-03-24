#' @param exposure A data table.  The first column must be the unit identifier
#' (for example, 5-digit U.S. zip code)
#' and the second column is the exposure of interest \code{exposure}
#' @param covariates A data table. The first column must be the unit identifier
#' (for example, 5-digit U.S. zip code).
#' Subsequent columns are covariates to include in the propensity score model.
#' \code{covariates}
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
#' in propensity scores when performing matching
#' @param  ... additional matching parameters to be passed to the matchit method
analyzeMatches <- function(exposure, covariates, covariate.vars, regions = "all",
                          exact.vars = NULL, exposure.cutoff.percentile = 0.80,
                          match.method = "nn", caliper.type = "default",
                          caliper.threshold = 0.2, quantiles = NULL, do.pairs=T, ...){
  require(MatchIt)
  require(data.table)

  exposure <- data.table(exposure)
  covariates <- data.table(covariates)
  # for indexing
  #exposure[, idx := seq_len(nrow(exposure))]
  #covariates[, idx := seq_len(nrow(exposure)) ]

  setkey(exposure, zip)
  setkey(covariates, zip)
  #setindex(exposure, idx)
  #setindex(covariates, idx)
  exposure.var <- colnames(exposure)[2]

  if(!is.null(exact.vars)){
    if(is.numeric(covariates[[exact.vars]])){
      print("Exact matching variables should be categorical. No exact matching performed.")
      exact.vars = NULL
    }
  }

  #used when covariate.vars = "all"
  if(length(covariate.vars) == 1 & "all" %in% covariate.vars){
    covariate.vars <- colnames(covariates)[!colnames(covariates) %in% c("zip","City","State","Latitude","Longitude")]
    #covariate.vars <- colnames(covariates)[-4]
  }
  if(!all(covariate.vars %in% colnames(covariates))){
    print("Invalid covariate.var...must be a colname of dataset")
    print("Continue with valid names.")
    covariate.vars <- covariate.vars[which(covariate.vars %in% colnames(covariates))]
  }

  # zip | exposure | covariate columns
  dataset <- merge(exposure, covariates, by = "zip")

  # Only use zip codes with all covariates
  dataset <- dataset[complete.cases(dataset), ]

  # dichotomize exposure ~ note this is done before dropping other regions
  cutoff <- quantile(dataset[[exposure.var]], exposure.cutoff.percentile)
  dataset$High <- ifelse(dataset[[exposure.var]] > cutoff,1,0 )

  # subset by regions ~ note this is done after dichotomizing the exposure
  if(!"all" %in% regions){
    if(all(regions %in% unique(dataset$region))){
      dataset <- subset(dataset, region %in% regions)
    } else{
      print("Invalid region selected...proceeding with all.")
    }
  }
  # remove regional covariate if there is only one region
  if(length(regions)<=2){
    covariate.vars <- covariate.vars[!covariate.vars %in% c("region")]
  }

  # get propensity scores from logistic regression
  formula.propensity.score <- as.formula(paste("High ~ ", paste(covariate.vars,collapse = " + ")))
  model.propensity.score  <- glm(formula.propensity.score, data = dataset, family = binomial())
  dataset$prop.scores <- predict(model.propensity.score, type = "response")

  # dataset w/prop.scores before matching
  dataset.raw <- dataset
  # add an index temporarily for joining matched zips
  #dataset[, idx := seq_len(nrow(dataset))]

  # -------- DO THE MATCHING -------------------
  # MatchIt's docs https://r.iq.harvard.edu/docs/matchit/2.4-20/_TT_matchit_TT__Implem.html
  if(match.method == "nn") {
    # default caliper is 20% of the pooled standard deviation of logit of the propensity score
    if(caliper.type == "default"){
      logit <- function(p){return(log(p/(1-p)))}
      VarHigh <- var(logit(subset(dataset,High == 1)$prop.scores))
      VarLow <- var(logit(subset(dataset,High == 0)$prop.scores))
      pooled_Var <- 0.5*(VarHigh + VarLow)
      caliper <- caliper.threshold*(sqrt(pooled_Var))
    } else {
      caliper <- 0
    }

    if (...length() == 0){
      matched.model <- matchit(formula.propensity.score, data = dataset, method = "nearest",
                               distance = "logit", discard = "both", ratio = 1,
                               exact = exact.vars, caliper = caliper)
    } else {
      matched.model <- matchit(formula.propensity.score, data = dataset, method = "nearest",
                               caliper=caliper,...)
    }

    pairs <- NULL
    if(do.pairs){
      # get a dt where each row has each zip code of a matched pair, and their long/lat
      dt.merged <- data.table(matched.model$match.matrix, keep.rownames = T)
      colnames(dt.merged) <- c("zipt", "zipc")
      # removed unmatched data
      dt.merged <- dt.merged[complete.cases(dt.merged), ]
      dt.merged <- dt.merged[!zipc=="-1", ]
      zip1 <- as.integer(dt.merged[["zipt"]])
      zip2 <- as.integer(dt.merged[["zipc"]])
      # match individually to covariates, 'joining' on row #
      dt.zips1 <- dataset[zip1,.(zip1=zip,Latitude1=Latitude,Longitude1=Longitude,region1=region,State1=State)]
      dt.zips2 <- dataset[zip2,.(zip2=zip,Latitude2=Latitude,Longitude2=Longitude,region2=region,State2=State)]
      pairs <- cbind(dt.zips1, dt.zips2)
    }

    dataset.matched <- data.table(match.data(matched.model))
    summary <- data.table("total zips"=nrow(dataset.raw),
                          "zips discarded"=nrow(dataset.raw)-nrow(dataset.matched),
                          "controls"=nrow(dataset.matched[High==0]),
                          "exposed"=nrow(dataset.matched[High==1])
                          )
  } else if(match.method == "stratified") {
    .q <- quantiles
    if(is.null(.q)) .q <- seq(0,1,0.2)

    # discard rows not in (min_exposed_prop,max_exposed_prop) AND (min_control_prop,max_control_prop)
    ps_low <- max(min(subset(dataset, High == 1)$prop.scores), min(subset(dataset, High == 0)$prop.scores))
    ps_high <- min(max(subset(dataset, High == 1)$prop.scores),max(subset(dataset, High == 0)$prop.scores))
    dataset <- subset(dataset, prop.scores >= ps_low & prop.scores <= ps_high)

    PS_quantiles <- quantile(dataset$prop.scores, probs=.q)
    # label the prop scores with their quantile
    dataset$PS_group <- findInterval(dataset$prop.scores, PS_quantiles, all.inside = T)
    dataset.matched <- dataset
    summary <- NULL
    matched.model <- NULL
    caliper <- NULL
    pairs <- NULL
  } else if(match.method == "dapsm" || match.method == "exact") {
    print("Not implemented")
    summary <- NULL
    dataset.matched <- NULL
    matched.model <- NULL
    caliper <- NULL
    pairs <- NULL
  } else {
    print("No valid matching method specified")
    summary <- NULL
    dataset.matched <- NULL
    matched.model <- NULL
    caliper <- NULL
    pairs <- NULL
  }

  matched <- list(dataset.raw, dataset.matched, matched.model, pairs, caliper, cutoff, summary)
  names(matched) = c("raw", "matched", "match.model", "pairs", "caliper", "cutoff", "summary")

  return(matched)
}
