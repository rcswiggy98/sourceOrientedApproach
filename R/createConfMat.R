#' @param models Outputs from analyzeMatches, representing matches done by the exposure metrics \code{models}
#' @param ground.truth Character corresponding to a list element in models, will be
#' the 2nd column of each confusion matrix (what to compare against)
#' @return A named list of tables, names corresponding to the compared exposure
createConfMat <- function(models=list(), ground.truth="CMAQ") {
  require(caret)
  require(data.table)

  preds <- sapply(models, function(m){
                            raw <- m$raw[, .(zip, High)]
                            matched <- m$matched[, .(zip, High)]
                            p <- matched[raw, on="zip"][, .(zip, High=as.integer(High))]
                            p[is.na(p)] <- -1L
                            treatment <- factor(p[["High"]], levels=c("-1", "0", "1"),
                                                labels = c("Dropped", "Control", "Treated"))
                            return(treatment)},
                  simplify = F, USE.NAMES = T
  )
  # get the base case (2nd column of the conf matrix)
  ref <- preds[[ground.truth]]
  confMats <- list()
  for(i in 1:length(preds)){
    # hacky
    name <- names(preds[i])[1]
    confMats[[name]] <- confusionMatrix(preds[[name]], reference = ref, dnn=c(name, ground.truth))
  }
  return(confMats[names(confMats) != ground.truth])
}
