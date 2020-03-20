#' @param  data A data.table containing columns zip, Latitude and Longitude for the zip codes and a column
#' High indicating if this was a treated or control zip \code{zips}
#' @param pairs  A data.table containing columns Latitude1, Longitude1, Latitude2, Longitude2
#' for the pairs of matched zips. \code{matches}
#' @param add.exposure TRUE/FALSE. Whether to plot continuous exposure as well. \code{add.exposure}
#' @param filters A list of parameters to pass to analyzeMatchesUI to do the filtering \code{filters}
plotMatches <- function(data, pairs, filters = list(), add.exposure = F, stratified = F){
  ## TODO:
  ##       summary tables of counts of matched/control
  ##       confusion matrix?
  ##       long/lat filters
  #require(ggmap)
  require(maps)
  require(maptools)
  #require('./R/latLongToState.R')
  require(dplyr)
  require(plotly)

  normalize <- function(x){
    return(x-min(x)/(max(x)-min(x)))
  }

  data <- copy(data)
  pairs <- copy(pairs)
  if (stratified) {
    data[, norm.lat:=normalize(Latitude)][, norm.long:=normalize(Longitude)]
    # apply filters
    # filter by region
    if(filters$region != "All"){
      data <- data[region%in%filters$region]
      pairs <- pairs[region1%in%filters$region][region2%in%filters$region]
    }
    # filter by state
    if(filters$state != "All") {
      data <- data[State%in%filters$State]
      pairs <- pairs[State1%in%filters$State][State2%in%filters$State]
    }
    # filter by lat/long
    #data <- data[norm.lat > filters$norm.lat[1] && norm.lat < filters$norm.lat[2]]
    #data <- data[norm.long > filters$norm.long[1] && norm.long < filters$norm.long[2]]
    #pairs <- pairs[norm.long1 > filters$norm.long[1] && norm.long1 < filters$norm.long[2]][norm.long2 > filters$norm.long[1] && norm.long2 < filters$norm.long[2]]
    #pairs <- pairs[norm.lat1 > filters$norm.lat[1] && norm.lat1 < filters$norm.lat[2]][norm.lat2 > filters$norm.lat[1] && norm.lat2 < filters$norm.lat[2]]
    # read output
    zips.treated <- data[High==1, .(zip, Latitude, Longitude, region, High, PS_group = as.factor(PS_group))]
    zips.control <- data[High==0, .(zip, Latitude, Longitude, region, High, PS_group = as.factor(PS_group))]
  } else {
    data[, norm.lat:=normalize(Latitude)][, norm.long:=normalize(Longitude)]
    pairs[, norm.lat1:=normalize(Latitude1)][, norm.long1:=normalize(Longitude1)]
    pairs[, norm.lat2:=normalize(Latitude2)][, norm.long2:=normalize(Longitude2)]

    # apply filters
    # filter by region
    if(filters$region != "All"){
      data <- data[region%in%filters$region]
      pairs <- pairs[region1%in%filters$region][region2%in%filters$region]
    }
    # filter by state
    if(filters$state != "All") {
      data <- data[State%in%filters$State]
      pairs <- pairs[State1%in%filters$State][State2%in%filters$State]
    }
    # filter by lat/long
    #data <- data[norm.lat > filters$norm.lat[1] && norm.lat < filters$norm.lat[2]]
    #data <- data[norm.long > filters$norm.long[1] && norm.long < filters$norm.long[2]]
    #pairs <- pairs[norm.long1 > filters$norm.long[1] && norm.long1 < filters$norm.long[2]][norm.long2 > filters$norm.long[1] && norm.long2 < filters$norm.long[2]]
    #pairs <- pairs[norm.lat1 > filters$norm.lat[1] && norm.lat1 < filters$norm.lat[2]][norm.lat2 > filters$norm.lat[1] && norm.lat2 < filters$norm.lat[2]]

    # read output from analyzeMatches
    zips.treated <- data[High==1, .(zip, Latitude, Longitude, region, High)]
    zips.control <- data[High==0, .(zip, Latitude, Longitude, region, High)]
    #adding col in place, FIX THIS!
    pairs[, id := seq_len(nrow(pairs))]
  }
  # map projection
  geo <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("gray95"),
    countrycolor = toRGB("gray80")
  )
  fig <- plot_geo(locationmode = 'USA-states', color=I("red"))

  if (stratified){
    # plot the zip code
    if(filters$subset == "Treated") {
      fig <- fig %>% add_markers(
        data = zips.treated, x = ~Longitude, y = ~Latitude, #text = ~zip,
        size = ~0.01, hoverinfo = "text", alpha = 0.5, symbol=I("circle"), color=~PS_group
      )
    } else if(filters$subset == "Control"){
      fig <- fig %>% add_markers(
        data = zips.control, x = ~Longitude, y = ~Latitude, #text = ~zip,
        size = ~0.01, hoverinfo = "text", alpha = 0.5, symbol=I("square"), color=~PS_group
      )
    } else {
      fig <- fig %>% add_markers(
        data = zips.treated, x = ~Longitude, y = ~Latitude, #text = ~zip,
        size = ~0.01, hoverinfo = "text", alpha = 0.5, symbol=I("circle"), color=~PS_group
      )
      fig <- fig %>% add_markers(
        data = zips.control, x = ~Longitude, y = ~Latitude, ##text = ~zip,
        size = ~0.01, hoverinfo = "text", alpha = 0.5, symbol=I("square"), color=~PS_group
      )
    }
  } else {
    # plot the zip code
    if(filters$subset == "Treated") {
      fig <- fig %>% add_markers(
        data = zips.treated, x = ~Longitude, y = ~Latitude, #text = ~zip,
        size = ~0.01, hoverinfo = "text", alpha = 0.5, marker=list(color="red")
      )
    } else if(filters$subset == "Control"){
      fig <- fig %>% add_markers(
        data = zips.control, x = ~Longitude, y = ~Latitude, #text = ~zip,
        size = ~0.01, hoverinfo = "text", alpha = 0.5, marker=list(color="blue")
      )
    } else {
      fig <- fig %>% add_markers(
        data = zips.treated, x = ~Longitude, y = ~Latitude, #text = ~zip,
        size = ~0.01, hoverinfo = "text", alpha = 0.5, marker=list(color="red")
      )
      fig <- fig %>% add_markers(
        data = zips.control, x = ~Longitude, y = ~Latitude, ##text = ~zip,
        size = ~0.01, hoverinfo = "text", alpha = 0.5, marker=list(color="blue")
      )
    }
  }
  if(!stratified){
    # just color the strata
    fig <- fig %>% add_segments(
      data = group_by(pairs, id),
      x = ~Longitude1, xend = ~Longitude2,
      y = ~Latitude1, yend = ~Latitude2,
      alpha = 0.5, size = I(0.1), hoverinfo = "none"
    )
  }
  fig <- fig %>% layout(
    title = 'Matched Data Points',
    geo = geo, showlegend = T, height=800
  )
  fig
}



  # #function that returns the states in each geographic region
  # getStatesFromRegion <- function(regions){
  #   states = character()
  #   if("Northeast" %in% regions) {states = c(states,"maine", "new hampshire", "vermont", "new york",
  #                                            "pennsylvania", "delaware", "new jersey", "maryland",
  #                                            "district of columbia", "virginia", "massachusetts",
  #                                            "connecticut", "rhode island")}
  #   if("IndustrialMidwest" %in% regions) {states = c(states,"west virginia", "ohio", "kentucky", "indiana",
  #                                                    "illinois", "wisconsin", "michigan")}
  #   if("Southeast" %in% regions) {states = c(states,"florida", "georgia", "south carolina", "north carolina",
  #                                            "tennessee", "alabama", "mississippi", "arkansas","louisiana")}
  #   if("UpperMidwest" %in% regions) {states = c(states, "minnesota", "iowa", "missouri", "kansas", "nebraska",
  #                                               "south dakota", "north dakota")}
  #   if("Southwest" %in% regions) {states = c(states, "texas", "oklahoma", "new mexico", "arizona")}
  #   if("SouthernCalifornia" %in% regions) {states = c(states, "california")}
  #   if("Northwest" %in% regions) {states = c(states,"nevada", "utah", "colorado", "wyoming", "montana", "idaho", "oregon", "washington")}
  #   return(states)
  # }
  #
  # #function used for color markers in map
  # makeTransparent = function(..., alpha=0.5) {
  #   if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  #   alpha = floor(255*alpha)
  #   newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  #   .makeTransparent = function(col, alpha) {
  #     rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  #   }
  #   newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  #   return(newColor)
  # }
  #
  #
  # all_states <- map_data("state", maptype="satelite")
  # regions <- unique(dataset$region)
  # states <- getStatesFromRegion(regions)
  # remaining_states <- subset(all_states, region %in% states)
  #
  # # can we use another mapping library besides ggmap?
  # p1 <- qmap('iowa', zoom = 4, maptype ='satellite')
  # p1 <- p1 + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),
  #                          colour="white",fill=NA, size=0.2)
  # p1 <- p1 + geom_polygon( data=remaining_states, aes(x=long, y=lat, group = group),
  #                          colour="white",fill=rgb(255, 250, 240, maxColorValue=255, alpha=200), size=0.2)
  #
  # if(add.exposure == F){
  #   # untreated zip codes
  #   p1 <- p1 + geom_point( data=subset(dataset, High==0), aes(x=Longitude, y=Latitude, color=Trt),
  #                          size=0.01, col=makeTransparent("blue", alpha=0.6))
  #   # treated zip codes
  #   p1 <- p1 + geom_point( data=subset(dataset, High==1), aes(x=Longitude, y=Latitude, color=Trt),
  #                          size=0.01, col=makeTransparent("red", alpha=0.6))
  #   plotly::plot_geo()
  #
  #   # figure out a way to draw lines!
  # } else {
  #   cont.var <- colnames(dataset)[2]
  #   p1 <- p1 + geom_point(data = dataset, aes_string(x="Longitude", y="Latitude", colour=cont.var),
  #                         alpha = 0.2, size=0.01)
  #   p1 <- p1 + scale_colour_gradient(low = "white", high = "black")
  # }
  #
  # p1 <- p1 + ggtitle(main) + theme(legend.position = "none")
  # p1

