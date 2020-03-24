source('./R/analyzeMatches.R')
source('./R/plotMatches.R')
source('./R/createSMDplot.R')
source('./R/createConfMat.R')
library(data.table)
library(shiny)

# read in data before looping server
states <- readLines("./data/states.txt")
states[51] <- "All"
states <- as.list(states)
load('data/cmaqddm2005.RData')
cmaqddm2005 <- as.data.table(cmaqddm2005)
cmaqddm2005 <- cmaqddm2005[, .(zip=ZIP, Exposure_DDM)]
load('data/inmap2005.RData')
inmap2005 <- as.data.table(inmap2005)
hyads2005 <- cmaqddm2005
load('data/covariates.RData')

ui <- fluidPage(
  titlePanel("Exposure/matching vis"),

  sidebarLayout(
    sidebarPanel(
      actionButton("init", "Render matches/Reload"),

      # pass to analyzeMatches after user clicks init button
      selectInput("match.method",
                  label = "Matching method",
                  choices = list("nn", "stratified",
                              "dapsm", "exact"),
                  selected = "nn"),

      # pass to analyzeMatches after user clicks init button
      sliderInput("exposure.cutoff.percentile",
                  label = "Exposure cutoff",
                  min = 0, max = 1, value = 0.8),

      # pass to analyzeMatches after user clicks init button
      sliderInput("caliper.threshold",
                  label = "Caliper (for nn matching only)",
                  min = 0, max = 2, value = 0.2, step=0.01),

      # change quantiles in analyzeMatches to this after user clicks init button
      numericInput("n.prop.quantiles",
                  label = "# of strata (for stratified matching only)",
                  min = 2, max = 25, value = 5, step=1),

      # pass to analyzeMatches after user clicks init button; all data needs to be loaded!
      selectInput("exposure.var", label = "Exposure to plot matches for",
                  choices = list("HyADS", "CMAQ", "INMAP"),
                  selected = "CMAQ", width = NULL),

      # exposures to compare against (for confusion matrix plot)
      checkboxGroupInput("exposure.vars.compare", label = "Exposures to compare against",
                  choices = list("HyADS", "CMAQ", "INMAP"),
                  selected = NULL, width = NULL),

      # pass to analyzeMatches, this will remove all units not in the given regions from
      # the matching process entirely
      checkboxGroupInput("region.match", label = "Regions to match units",
                  choices = list("Northeast", "IndustrialMidwest", "Southeast",
                                 "UpperMidwest", "Southwest", "SouthernCalifornia", "Northwest"),
                  selected = list("Northeast", "IndustrialMidwest", "Southeast",
                                  "UpperMidwest", "Southwest", "SouthernCalifornia", "Northwest"),
                  width = NULL),

      helpText("Filter the plotted matched data using the options below, or use the legend/tools
               in the plot itself:"),

      # these should automatically refresh as the user changes them
      # show only those data in a given region
      selectInput("region", label = "Region",
                         choices = list("All", "Northeast", "IndustrialMidwest", "Southeast",
                                        "UpperMidwest", "Southwest", "SouthernCalifornia", "Northwest"),
                         selected = "All", width = NULL),

      # show only those data in a given state
      selectInput("state", label = "State",
                         choices = states,
                         selected = "All", width = NULL),

      # show only control or treated data
      selectInput("subset", label = "Treatment",
                         choices = list("Treated", "Control", "All"), selected = "Treated"),

      # last filter; of the remaining units, select only a subset of them, based on lat/long
      sliderInput("norm.long", "Normalized Longitude", min = 0, max = 1, value = c(0,1)),
      sliderInput("norm.lat", "Normalized Latitude", min = 0, max = 1, value = c(0,1))
    ),

    # prompt the user for main exposure: static plot only.
    # plot the secondary exposure as well.
    # plot histograms of exposure by region.
    # fit Outcome model and plot effect and boxplot per region
    # propensity score distributions and confusion matrices at the end
    mainPanel(#plotly::plotlyOutput("map", height="50%"),
              uiOutput("secondary.maps"),
              uiOutput("SMD"),
              uiOutput("conf.mats"))
  )
)


server <- function(input, output, session) {
  observeEvent(input$init, {
    # get recompute parameters
    match.method <- input$match.method
    is.stratified <- ifelse(match.method == "stratified", T, F)
    cutoff <- input$exposure.cutoff.percentile
    caliper.threshold <- input$caliper.threshold
    quantiles <- seq(0,1,1/input$n.prop.quantiles)
    exposure <- switch (input$exposure.var,
      "INMAP" = inmap2005,
      "CMAQ" = cmaqddm2005,
      "HyADS" = cmaqddm2005
    )
    match.regions <- input$region.match
    filter.params <- list(region=input$region,
                          state=input$state,
                          subset=input$subset,
                          norm.long=input$norm.long,
                          norm.lat=input$norm.lat)
    # recompute
    mm.out <- analyzeMatches(exposure=exposure, covariates=covariates, regions=match.regions,
                             covariate.vars="all", exposure.cutoff.percentile=cutoff,
                             match.method=match.method, caliper.type="default",
                             caliper.threshold=caliper.threshold, quantiles=quantiles)
    # compute different exposures to compare
    output$conf.mats <- NULL
    if(length(input$exposure.vars.compare) > 0){
      # don't want to duplicate main exposure
      exposure.vars.compare <- input$exposure.vars.compare[!input$exposure.vars.compare %in% c(input$exposure.var)]
      exposure.vars.compare <- c(exposure.vars.compare, input$exposure.var)
      exposures <- lapply(exposure.vars.compare,switch,CMAQ=cmaqddm2005,HyADS=cmaqddm2005,INMAP=inmap2005)
      names(exposures) <- exposure.vars.compare
      more.models <- sapply(exposures,
                            function(x){return(analyzeMatches(exposure=x,covariates=covariates,
                                                             regions=match.regions,covariate.vars="all",
                                                             exposure.cutoff.percentile=cutoff,
                                                             match.method=match.method, caliper.type="default",
                                                             caliper.threshold=caliper.threshold,
                                                             quantiles=quantiles)[c(1:4)])},
                            simplify=F, USE.NAMES=T)
      # plot additional exposure maps
      output$secondary.maps <- renderUI({
        maps.list <- lapply(1:length(more.models), function(i){
          name <- paste("map",i,sep="")
          plotlyOutput(name)#, width = paste(100/length(more.models), "%", sep=""))
        })
        do.call(tagList, maps.list)
      })
      for(i in 1:length(more.models)){
        local({
          idx <- i
          plotname <- paste("map", idx, sep = "")
          output[[plotname]] <- plotly::renderPlotly({
            plotMatches(data=more.models[[idx]]$matched,
                        pairs=more.models[[idx]]$pairs,
                        filters=filter.params,
                        stratified=is.stratified,
                        plotname = names(more.models)[idx])
          })
        })
      }

      # plot the confusion matrices
      mats.to.plot <- createConfMat(more.models, ground.truth = input$exposure.var)
      output$conf.mats <- renderUI({
        plot.output.list <- lapply(1:length(more.models), function(i){
          name <- paste("plot", i, sep="")
          tableOutput(name)#, width = paste(95/length(mats.to.plot),"%", sep=""))
        })
        do.call(tagList, plot.output.list)
      })
      for(i in 1:length(mats.to.plot)){
        local({
          idx <- i
          plotname <- paste("plot", idx, sep = "")
          output[[plotname]] <- renderTable(mats.to.plot[[idx]]$table, rownames = T, colnames = T)
        })
      }
    }

    # make the SMD balance plots
    # output$SMD <- NULL
    output$SMD <- renderUI({
      smd.list <- lapply(1:length(more.models), function(i){
        name <- paste("smd", i, sep="")
        plotOutput(name)#, width = paste(95/length(mats.to.plot),"%", sep=""))
      })
      do.call(tagList, smd.list)
    })
    for(i in 1:length(more.models)) {
      local({
        idx <- i
        plotname <- paste("smd",i,sep="")
        if(!is.stratified) output[[plotname]] <- renderPlot({createSMDplot(more.models[[idx]]$match.model)})
        else output[[plotname]] <- renderPlot({createStratSMDplot(more.models[[idx]]$matched)})
      })
    }

    # make the outcome model plots and the estimated IRRs for the given cutoff

    # make the propensity score histograms

    # just compute the exposures for all 3 and let the user shift between them?
    #output$map <- plotly::renderPlotly({
    #  plotMatches(data=mm.out$matched,pairs=mm.out$pairs,filters=filter.params,stratified=is.stratified)
    #})
  })
}

shinyApp(ui, server)

# difference in casual effect?
