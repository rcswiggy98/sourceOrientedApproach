source('./R/analyzeMatches.R')
source('./R/plotMatches.R')
source('./R/createSMDplot.R')
source('./R/createConfMat.R')
source('./R/plotOutcome.R')
library(data.table)
library(shiny)

# read in data before looping server
states <- readLines("./data/states.txt")
states[51] <- "All"
states <- as.list(states)
load('./data/cmaqddm2005.RData')
cmaqddm2005 <- as.data.table(cmaqddm2005)[, .(zip=ZIP, Exposure_DDM)]
load('./data/inmap2005.RData')
inmap2005 <- as.data.table(inmap2005)
load('./data/hyads_raw_2005.RData')
hyads2005 <- hyads_raw[, .(zip=ZIP, hyads)]
rm(hyads_raw)
load('./data/hyads_pm25_2005.RData')
hyadspm2005 <- hyads_pm25[, .(zip=ZIP, hyads_pm25)]
rm(hyads_pm25)
load('data/covariates.RData')
# SPECIFY outcome here
load('data/medicare_sim2005.RData')
outcome <- medicare_sim2005
rm(medicare_sim2005)

ui <- fluidPage(
  titlePanel("Exposure/matching vis"),

  sidebarLayout(
    sidebarPanel(
      actionButton("init", "Render matches/Reload"),

      # pass to analyzeMatches after user clicks init button
      selectInput("match.method",
                  label = "Matching method",
                  choices = list("nn", "stratified", "dapsm", "exact"),
                  selected = "nn"),

      # pass to analyzeMatches after user clicks init button
      numericInput("exposure.cutoff.percentile",
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
      #selectInput("exposure.var", label = "Exposure to plot matches for",
      #            choices = list("HyADS", "CMAQ", "INMAP"),
      #            selected = "CMAQ", width = NULL),

      checkboxGroupInput("stuff.to.plot", label = "Plots to include",
                         choiceNames = list("Treatment map", "Confusion matrix",
                                            "Exposure histogram", "SMD balance",
                                            "Propensity histogram", "Outcome boxplot",
                                            "Effect CIs"),
                         choiceValues = list("treatment", "conf.mat",
                                             "exp.hist", "smd", "prop.hist",
                                             "outcome", "effect")
                         ),

      # exposures to compare
      checkboxGroupInput("exposure.vars.compare", label = "Exposures to compare",
                          choices = list("HyADS", "HyADSpm25", "CMAQ", "INMAP"),
                          selected = "CMAQ", width = NULL),

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

      # ------------- these should automatically refresh as the user changes them -------------
      # show only those data in a given region
      selectInput("region", label = "Region",
                  choices = list("All", "Northeast", "IndustrialMidwest", "Southeast",
                                "UpperMidwest", "Southwest", "SouthernCalifornia", "Northwest"),
                  selected = "All", width = NULL),

      # show only those data in a given state
      selectInput("state", label = "State", choices = states,
                  selected = "All", width = NULL),

      # show only control or treated data
      selectInput("subset", label = "Treatment",
                  choices = list("Treated", "Control", "All"), selected = "Treated"),

      # last filter; of the remaining units, select only a subset of them, based on lat/long
      sliderInput("norm.long", "Normalized Longitude", min = 0, max = 1, value = c(0,1)),
      sliderInput("norm.lat", "Normalized Latitude", min = 0, max = 1, value = c(0,1))
    ),


    mainPanel(#plotly::plotlyOutput("map", height="50%"),
              uiOutput("treat.maps"),
              uiOutput("SMD"),
              uiOutput("conf.mats"),
              uiOutput("outcome"),
              uiOutput("effect"))
  )
)


server <- function(input, output, session) {
  observeEvent(input$init, {
    #exposure <- switch (input$exposure.var,
    #  "INMAP" = inmap2005,
    #  "CMAQ" = cmaqddm2005,
    #  "HyADS" = hyads2005,
    #  "HyADSpm25" = hyadspm2005
    #)

    match.method <- input$match.method
    is.stratified <- ifelse(match.method == "stratified", T, F)
    cutoff <- input$exposure.cutoff.percentile
    caliper.threshold <- input$caliper.threshold
    quantiles <- seq(0,1,1/input$n.prop.quantiles)
    match.regions <- input$region.match
    filter.params <- list(region=input$region,
                          state=input$state,
                          subset=input$subset,
                          norm.long=input$norm.long,
                          norm.lat=input$norm.lat)
    # recompute
    #mm.out <- analyzeMatches(exposure=exposure, covariates=covariates, regions=match.regions,
    #                         covariate.vars="all", exposure.cutoff.percentile=cutoff,
    #                         match.method=match.method, caliper.type="default",
    #                         caliper.threshold=caliper.threshold, quantiles=quantiles)

    # do the compute to match the data given the selected exposures
    exposure.vars.compare <- input$exposure.vars.compare
    # add more exposures here
    exposures <- lapply(exposure.vars.compare, switch,
                        CMAQ=cmaqddm2005,HyADS=hyads2005,HyADSpm25=hyadspm2005,INMAP=inmap2005)
    names(exposures) <- exposure.vars.compare
    more.models <- sapply(exposures,
                          function(x){
                            return(analyzeMatches(exposure=x,covariates=covariates,
                                                  regions=match.regions,covariate.vars="all",
                                                  exposure.cutoff.percentile=cutoff,
                                                  match.method=match.method,caliper.type="default",
                                                  caliper.threshold=caliper.threshold,
                                                  quantiles=quantiles)[c(1:4)])},
                          simplify=F, USE.NAMES=T)

    # make the treated/control maps
    if("treatment" %in% input$stuff.to.plot){
      output$treat.maps <- renderUI({
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
    } else {
      output$treat.maps <- NULL
    }


    # plot the confusion matrices
    # TODO: specify ground truth?? Cannot compare all combinations
    if("conf.mat" %in% input$stuff.to.plot){
      mats.to.plot <- createConfMat(more.models, ground.truth = input$exposure.vars.compare[1])
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
    } else {
      output$conf.mats <- NULL
    }

    # make the SMD balance plots
    if("smd" %in% input$stuff.to.plot){
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
          plotname <- paste("smd",idx,sep="")
          if(!is.stratified){
            output[[plotname]] <- renderPlot({
              createSMDplot(more.models[[idx]]$match.model, name=names(more.models)[idx])
            })
          }
          else {
            # get matched and raw datasets only from analyzeMatches
            smds <- createStratSMD(models=more.models[idx])
            output[[plotname]] <- renderPlot({
              plotStratSMD(smds[[1]][[1]], smds[[2]][[1]], name=names(more.models)[idx])
            })
          }
        })
      }
    } else {
      output$SMD <- NULL
    }

    # make the outcome boxplots
    if("outcome" %in% input$stuff.to.plot){
      output$outcome <- renderUI({
        outcome.list <- lapply(1:length(more.models), function(i){
          name <- paste("outcome", i, sep="")
          plotOutput(name, width = "50%")#, width = paste(95/length(mats.to.plot),"%", sep=""))
        })
        do.call(tagList, outcome.list)
      })
      for(i in 1:length(more.models)) {
        local({
          idx <- i
          plotname <- paste("outcome",idx,sep="")
          output[[plotname]] <- renderPlot(
            plotOutcome(more.models[[idx]], outcome, do.effect = F, name=names(more.models)[idx])
          )
        })
      }
    } else {
      output$outcome <- NULL
    }

    if("effect" %in% input$stuff.to.plot) {
      # make the effect CIs
      output$effect <- renderUI({
        effect.list <- lapply(1:length(more.models), function(i){
          name <- paste("effect", i, sep="")
          plotOutput(name, width = "50%")#, width = paste(95/length(mats.to.plot),"%", sep=""))
        })
        do.call(tagList, effect.list)
      })
      for(i in 1:length(more.models)) {
        local({
          idx <- i
          plotname <- paste("effect",idx,sep="")
          output[[plotname]] <- renderPlot(
            plotOutcome(more.models[[idx]], outcome, regions=match.regions,
                        do.effect = T, name=names(more.models)[idx])
          )
        })
      }
    } else {
      output$effect <- NULL
    }


    # make the propensity score histograms
  })
}

shinyApp(ui, server)
