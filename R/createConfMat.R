#' @param models Named list of outputs from analyzeMatches.
#' @param ground.truth Character corresponding to a list element in models, will be
#' the 2nd column of each confusion matrix (what to compare against).
#' @return A named list of tables, names corresponding to the compared exposure.
createConfMat <- function(models=list(), ground.truth="CMAQ") {
  require(caret)
  require(data.table)

  # inner join all raw datasets to the ground.truth
  gt<- models[[ground.truth]]
  n <- nrow(gt$raw)

  joined.models <- sapply(models, function(m){
    # how many zips not in ground.truth data? Positive means
    # records from this model not considered. Negative means records from
    # the ground.truth model are not being considered.

    r <- m$raw[gt$raw, on="zip", nomatch=0]
    drops <- n - nrow(r)
    match <- m$matched[r, on="zip", nomatch=0]
    gtmatch <- gt$matched[r, on="zip", nomatch=0]
    return(list(raw=r, matched=match, gtmatched = gtmatch, dropped=drops))
  }, simplify = F)

  preds <- sapply(joined.models, function(m){
                  raw <- m$raw[, .(zip, High)]
                  matched <- m$matched[, .(zip, High)]

                  # get this model's classifications
                  p <- matched[raw, on="zip"][, .(zip, High=as.integer(High))]
                  p[is.na(p)] <- -1L
                  treatment <- factor(p[["High"]], levels=c("-1", "0", "1"),
                                      labels = c("Dropped", "Control", "Treated"))

                  # get base case (ground.truth) classifications
                  gtmatched <- m$gtmatched[, .(zip, High)]
                  ref <- gtmatched[raw, on="zip"][, .(zip, High=as.integer(High))]
                  ref[is.na(ref)] <- -1L
                  treatment.ref <- factor(ref[["High"]], levels=c("-1", "0", "1"),
                                          labels = c("Dropped", "Control", "Treated"))

                  return(list(treatment=treatment, ref=treatment.ref))
                  }, simplify = F, USE.NAMES = T
  )
  drops <- sapply(joined.models, function(m) m$dropped)

  confMats <- list()
  for(i in 1:length(preds)){
    # hacky
    name <- names(preds[i])[1]
    confMats[[name]] <- list(confusionMatrix(preds[[name]]$treatment,
                                             reference = preds[[name]]$ref,
                                             dnn=c(name, ground.truth)),
                             drops[[i]])
  }
  return(confMats[names(confMats) != ground.truth])
}
