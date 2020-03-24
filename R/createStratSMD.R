#' @param models List of outputs (themselves lists) from analyzeMatches, or any sublist
#' of the output containing both the named "matched" and "raw" elements \code{models}
#' @param regional Boolean corresponding to whether or not to return SMDs for all regions \code{regional}
#' @return List of 2 element lists
createStratSMD <- function(models=list(), regional = F){
  require(data.table)

  sdev <- function(x){return(sqrt(var(x)))}
  model.names <- names(models)

  smds.raw <- list()
  smds.matched <- list()

  for(i in 1:length(models)){
    name <- model.names[i]
    covariates.raw <- models[[i]]$raw[,c(7:24)][, region := as.factor(region)]
    covariates.match <- models[[i]]$matched[,c(7:24,26)][, region := as.factor(region)]
    noregion <- names(covariates.raw[,!"region"])

    if(regional){
      return(NULL)
    } else {
      mean.t.r <- covariates.raw[High==1, lapply(.SD, mean), .SDcols = noregion][,!"High"]
      mean.c.r <- covariates.raw[High==0, lapply(.SD, mean), .SDcols = noregion][,!"High"]
      sd.r <- covariates.raw[, lapply(.SD, sdev), .SDcols = noregion][,!"High"]
      smd.r <- (mean.t.r - mean.c.r) / sd.r

      mean.t.m <- covariates.match[High==1, lapply(.SD, mean), .SDcols=noregion, by=PS_group][,!c("High","PS_group")]
      mean.c.m <- covariates.match[High==0, lapply(.SD, mean), .SDcols=noregion, by=PS_group][,!c("High","PS_group")]
      sd.m <- covariates.match[, lapply(.SD, sdev), .SDcols = noregion, by=PS_group][,!c("High","PS_group")]
      smd.m <- (mean.t.m - mean.c.m) / sd.m

      smds.raw[[i]] <- smd.r
      smds.matched[[i]] <- smd.m
    }
  }

  # make data.tables ready to be plotted
  #SMD.vars <- list(variable = names(models[[1]]$raw[,7:23][,!"region"]))
  SMD.vars <- names(models[[1]]$raw[,7:23][,!"region"])
  dataPlots <- mapply(function(r, m){
    #SMD.all <- list(Before = transpose(r)[["V1"]])
    SMD.all <- transpose(r)[["V1"]]
    SMD.PSMatches <- lapply(1:ncol(transpose(m)), function(i){
      # get ith column from the matched smds, corresponding to smds of ith PS quantile
      return(transpose(m[i,])[["V1"]])
    })
    names(SMD.PSMatches) <- paste("Quantile", 1:length(SMD.PSMatches), sep = " ")
    dt <- do.call(data.table, c(list(variable=SMD.vars, Before=SMD.all),SMD.PSMatches))
    return(dt)
  }, smds.raw, smds.matched, SIMPLIFY = F)
  names(dataPlots) <- model.names
  dataPlotMelts <- lapply(dataPlots, melt, id.vars=c("variable"),
                          variable.name="Dataset",value.name="SMD")
  names(dataPlotMelts) <- names(dataPlots)

  l.out <- list(dataPlots = dataPlots, dataPlotMelts=dataPlotMelts)

  return(l.out)
}
