#' @param  data A data.table containing columns zip, Latitude and Longitude for the zip codes and a column
#' High indicating if this was a treated or control zip \code{zips}
#' @param pairs  A data.table containing columns Latitude1, Longitude1, Latitude2, Longitude2
#' for the pairs of matched zips. \code{matches}
#' @param stratified TRUE/FALSE. If this is stratified matching. \code{add.exposure}
#' @param filters A list of parameters to pass to analyzeMatchesUI to do map filtering \code{filters}
#' @param plotname Name of the plot to be displayed as title \code{plotname}
#' @export
plotMatches <- function(data, pairs, filters = list(),
                        stratified = F, plotname="plot"){
  #require(ggmap)
  #require('./R/latLongToState.R')
  require(maps)
  require(maptools)
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
    countrycolor = toRGB("gray80"),
    lonaxis = list(range = c(-120, -75)),
    lataxis = list(range = c(25, 47))
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
    title = plotname,
    geo = geo, showlegend = T,
    xaxis = list(fixedrange=T),
    yaxis = list(fixedrange=T)
  )
  fig
}
