#' Obtain a propensity score matched dataset
#'
#' This function dichotomizes the exposure variable based upon the specified
#' percentile cutoff.  This dichotomization is done prior to any subsetting
#' on geographic region.   Using the MatchIt package, high exposed
#' ZIP codes are matched to control locations with similar propensity scores.
#' The caliper is the maximum allowed difference in propensity scores when
#' selecting matches.  The default caliper is 20% of the pooled standard
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
#' data('covariates')
#' data('inmap2005')
#' covariate.vars <- c("logPop", "PctUrban", "PctBlack","MedianHHInc","smokerate2000")
#' dataset <- getMatchedDataset(exposure, covariates, covariate.vars)

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
    p1 <- p1 + geom_point(data = dataset, aes(x=Longitude, y=Latitude, colour=PM25inMAP),
                          alpha = 0.2, size=0.01)
    p1 <- p1 + scale_colour_gradient(low = "white", high = "black")
  }

  p1 <- p1 + ggtitle(main) + theme(legend.position = "none")
  p1
}
