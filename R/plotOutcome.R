#' Generates a plot of either the outcome (incidence rate) or effect of treatment (IRR)
#' based on the fitted model returned from fitOutcomeModel. For stratified matching, the
#' model will use \code{model$raw}. For nn matching, \code{model$matched}.
#'
#' @param model The named list returned from analyzeMatches called on the exposure and covariates
#' of interest.
#' @param outcome data.table containing the outcome variable of interest (i.e. IHD)
#' @param regions Character vector indicating regions to fit individual Poisson glms on. If "all",
#' defaults to all regions in the appropriate dataset in model.
#' @param adj Numeric vector indicating an adjustment of PM2.5 not attributable to coal EGUs.
#' It should have the same length as the number of rows in model$matched or model$raw, with each
#' element corresponding to the relevant zip code in model$matched or model$raw.
#' @param do.effect Boolean. If TRUE, the plot returned shows the estimated effects for
#' each region along with a 95% confidence interval. If FALSE, the returned plot is
#' a boxplot of regional IHD outcomes.
#' @param do.sens.analy Do sensitivity analysis? If do.effect = T, and not stratified,
#' then this method calls analyzeMatches on the fly, computes the exposure for different cutoffs,
#' and plots the relevant confidence intervals for each cutoff.
#' (TODO, Not implemented yet)
#' @param outcome.var Outcome variable. Should be a column in \code{outcome}.
#' @param offset.var Offset for a Poisson regression. Should be a column in \code{outcome}.
#' @param name Name of the plot.
#' @return NA
plotOutcome <- function(model, outcome, regions="all", adj = NULL, do.effect = T,
                        do.sens.analy = F, outcome.var = "IHD", offset.var = "person_years",
                        name = "plot"){
  require(ggplot2)
  require(data.table)
  # require(plotly)
  source('./R/fitOutcomeModel.R')

  # is this a stratified model?
  is.stratified <- "PS_group" %in% names(model$matched)
  analysis.type <- ifelse(is.stratified, "stratified", "matched")

  # extract correct dataset (replace fitOutcomeModel so that it accepts matched dataset)
  # dataset <- switch(analysis.type, stratified = model$matched, matched = model$matched)
  dataset <- model$matched

  if(do.effect){
    # fit regional outcome models (what if user says 'all') and extract IRRs
    # more than 5 quantiles in stratified analysis?
    if("all" %in% regions){
      # get all unique regions
      regions <- unique(dataset$region)
    }

    # check edge case: if a region contains only treated or control zips, remove
    ok <- sapply(regions, function(r){
            return( length(unique(dataset[region==r][["High"]])) > 1 )
    }, simplify = T)
    regions <- regions[ok]

    datasets <- sapply(regions, function(r) dataset[region==r], simplify = F)

    # need to fit num_regions * num_propensity_strata glms.
    if(is.stratified){
      effects <- sapply(1:length(datasets), function(i){
        data <- datasets[[i]]
        out.mods <- sapply(levels(data$PS_group), function(l){
          r <- names(datasets)[i]
          if(nrow(data[PS_group == l]) == 0) return(c(NA, NA, NA, r, l))
          m <- fitOutcomeModel(dataset=data[PS_group == l], outcome = outcome,
                               covariate.vars = names(dataset)[c(7:17,19:23)],
                               analysis.type = "matched", outcome.var = outcome.var,
                               offset.var = offset.var)
          # get the irr
          irr <- exp(coef(m)["High"])

          # remove blowups
          if(is.na(irr)) return(c(NA, NA, NA, r, l))
          if(irr > 5) return(c(NA, NA, NA, r, l))

          # get the confidence interval (Wald)
          ci <- exp(confint.default(m, "High", level=0.95))

          # return irr, conf.int, region, and the PS quantile
          return(c(irr, ci, r, l))
        }, simplify = F)

        return(out.mods)
      }, simplify = T)

      # make a data.table for plotting, cols are IRR | Left | Right | Region | PS_group
      dt <- transpose(do.call(data.table, effects))
      names(dt) <- c("IRR", "L", "R", "region", "PS_group")
      dt <- dt[, .(IRR=as.numeric(IRR), L=as.numeric(L), R=as.numeric(R), region, PS_group)]
      pd <- position_dodge(0.4)
      p <- ggplot(dt, aes(x=PS_group, y=IRR, color=region, group=region)) + geom_point(position=pd) +
        geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                      position=pd) + ylim(0, 2) + ggtitle(name)
    } else {
      out.mods <- sapply(datasets, function(d){
          m <- fitOutcomeModel(dataset=d, outcome = outcome,
                               covariate.vars = names(dataset)[c(7:17,19:23)],
                               analysis.type = analysis.type, outcome.var = outcome.var,
                               offset.var = offset.var)
          return(m)
      }, simplify = F)
      names(out.mods) <- regions

      effects <- sapply(1:length(out.mods), function(i){
        m <- out.mods[[i]]
        r <- names(out.mods)[i]

        if(nrow(m$model) == 0) return(c(NA, NA, NA, r))

        irr <- exp(coef(m)["High"])
        # remove blowups
        if(is.na(irr)) return(c(NA, NA, NA, r))
        if(irr > 5) return(c(NA, NA, NA, r))

        ci <- exp(confint.default(m, "High", level=0.95))
        return(c(irr, ci, r))
      }, simplify = F)

      # make a data.table for plotting, cols are IRR | Left | Right | Region
      dt <- transpose(do.call(data.table, effects))
      names(dt) <- c("IRR", "L", "R", "region")
      dt <- dt[, .(IRR=as.numeric(IRR), L=as.numeric(L), R=as.numeric(R), region)]

      p <- ggplot(dt, aes(x=region, y=IRR)) + geom_point() +
        geom_errorbar(aes(ymin=L, ymax=R), width=.2,
                      position=position_dodge(0.05)) + ggtitle(name)
    }

  } else {
    setkey(outcome, zip)
    dt <- merge(dataset, outcome, by = "zip")
    dt <- dt[complete.cases(dt), ][, c(offset.var, outcome.var, "region"), with = F]
    y.lab <- paste(outcome.var, "/", offset.var, sep="")
    dt$out <- dt[[outcome.var]] / dt[[offset.var]]

    p <- ggplot(dt, aes(x=region, y=out)) + geom_boxplot() + ylab(y.lab) + ggtitle(name)
  }
  p
}
