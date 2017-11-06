#' Plot a U.S. map depicting the exposure variable
#'
#' This function plots a map depicting the exposure variable on a U.S. map.
#' The exposure variable can be either continous or binary.
#'
#' @param dataset  A data table containing the exposure variable.  If continuous, the exposure
#' variable must be the second column of the data table.  If binary, the exposure should be
#' either one or zero and in a column named High. In addition, the data table must contain
#' each ZIP code's coordinates in columns named Longtitude and Latitude \code{dataset}
#' @param exposure.binary TRUE/FALSE. Whether to plot the binary or continuous exposure.
#' \code{exposure.binary}
#'
#' @return NA
#'
#' @keywords keywords
#'
#' @export
#'
#' @references
#' \insertRef{kahle2013ggmap}{sourceOrientedApproach}
#'
#' @examples
#' data('covariates')
#' data('inmap2005')
#' covariate.vars <- c("logPop", "PctUrban", "PctBlack","MedianHHInc","smokerate2000")
#' dataset <- getMatchedDataset(exposure, covariates, covariate.vars)
#'
#' #plot the raw dataset
#' plotExposureUSmap(dataset$raw, exposure.binary = TRUE)
#' plotExposureUSmap(dataset$raw, exposure.binary = FALSE)

plotExposureUSmap <- function(dataset, exposure.binary = TRUE, main = ""){
  require(ggmap)
  require(maps)
  require(maptools)

  #function that returns the states in each geographic region
  getStatesFromRegion <- function(regions){
    states = character()
    if("Northeast" %in% regions) {states = c(states,"maine", "new hampshire", "vermont", "new york",
                                             "pennsylvania", "delaware", "new jersey", "maryland",
                                             "district of columbia", "virginia", "massachusetts",
                                             "connecticut", "rhode island")}
    if("IndustrialMidwest" %in% regions) {states = c(states,"west virginia", "ohio", "kentucky", "indiana",
                                                     "illinois", "wisconsin", "michigan")}
    if("Southeast" %in% regions) {states = c(states,"florida", "georgia", "south carolina", "north carolina",
                                             "tennessee", "alabama", "mississippi", "arkansas","louisiana")}
    if("UpperMidwest" %in% regions) {states = c(states, "minnesota", "iowa", "missouri", "kansas", "nebraska",
                                                "south dakota", "north dakota")}
    if("Southwest" %in% regions) {states = c(states, "texas", "oklahoma", "new mexico", "arizona")}
    if("SouthernCalifornia" %in% regions) {states = c(states, "california")}
    if("Northwest" %in% regions) {states = c(states,"nevada", "utah", "colorado", "wyoming", "montana", "idaho", "oregon", "washington")}
    return(states)
  }

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


  all_states <- map_data("state", maptype="satelite")
  regions <- unique(dataset$region)
  states <- getStatesFromRegion(regions)
  remaining_states <- subset(all_states, region %in% states)

  p1 <- qmap('nebraska', zoom = 3, maptype ='satellite')
  p1 <- p1 + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),
                           colour="white",fill=NA, size=0.2)
  p1 <- p1 + geom_polygon( data=remaining_states, aes(x=long, y=lat, group = group),
                           colour="white",fill=rgb(255, 250, 240, maxColorValue=255, alpha=200), size=0.2)

  if(exposure.binary == TRUE){
    # untreated zip codes
    p1 <- p1 + geom_point( data=subset(dataset, High==0), aes(x=Longitude, y=Latitude, color=Trt),
                           size=0.01, col=makeTransparent("blue", alpha=0.6))
    # treated zip codes
    p1 <- p1 + geom_point( data=subset(dataset, High==1), aes(x=Longitude, y=Latitude, color=Trt),
                           size=0.01, col=makeTransparent("red", alpha=0.6))
  }

  if(exposure.binary == FALSE){
    cont.var <- colnames(dataset)[2]
    p1 <- p1 + geom_point(data = dataset, aes_string(x="Longitude", y="Latitude", colour=cont.var),
                          alpha = 0.2, size=0.01)
    p1 <- p1 + scale_colour_gradient(low = "white", high = "black")
  }

  p1 <- p1 + ggtitle(main) + theme(legend.position = "none")
  p1
}
