source('./R/analyzeMatches.R')
source('./R/plotMatches.R')
source('./R/createSMDplot.R')
source('./R/createStratSMD.R')
source('./R/createConfMat.R')
source('./R/plotOutcome.R')
source('./R/plotStratSMD.R')
source('./R/plotPropensityScoreHistogram2.R')
source('./R/plotExposureHistogram.R')
library(data.table)
library(shiny)
library(plotly)

# --------------- read in data before looping server -----------------
states <- readLines("./data/states.txt")
states[51] <- "All"
states <- as.list(states)

# Load all exposures here, the names need to match the names defined in the list "exposures",
# the zip code column should be named "zip"
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

# Load covariates, rename to 'covariates'
load('data/covariates.RData')

# Load outcome here, rename to 'outcome'
load('data/medicare_sim2005.RData')
outcome <- medicare_sim2005
rm(medicare_sim2005)

# ------------------------------------------------------------------------------------

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
                  label = "Exposure cutoff (percentile)",
                  min = 0, max = 1, value = 0.8, step=0.025),

      # pass to analyzeMatches after user clicks init button
      sliderInput("caliper.threshold",
                  label = "Caliper (for nn matching only)",
                  min = 0, max = 2, value = 0.2, step=0.01),

      # change quantiles in analyzeMatches to this after user clicks init button
      numericInput("n.prop.quantiles",
                  label = "# of strata (for stratified matching only)",
                  min = 2, max = 25, value = 5, step=1),

      selectInput("exposure.var", label = "Reference exposure (for confusion matrix only)",
                  choices = list("HyADS", "HyADSpm25", "CMAQ", "INMAP"),
                  selected = "CMAQ", width = NULL),

      checkboxGroupInput("stuff.to.plot", label = "Plots to include",
                         choiceNames = list("Treatment map", "Confusion matrix",
                                            "Exposure histogram (raw data)", "SMD balance",
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

      helpText("Filter the maps of matched data using the options below, or use the legend/tools
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


    mainPanel(uiOutput("exp.hists"),
              uiOutput("treat.maps"),
              uiOutput("prop.hists"),
              uiOutput("SMD"),
              uiOutput("conf.mats"),
              uiOutput("outcome"),
              uiOutput("effect"))
  )
)


server <- function(input, output, session) {
  observeEvent(input$init, {
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

    exposure.vars.compare <- input$exposure.vars.compare
    # ground truth for confusion matrices
    exposure.var.confmat <- input$exposure.var

    # ---- **** add more exposures here **** -----
    exposures <- lapply(exposure.vars.compare, switch,
                        CMAQ=cmaqddm2005,HyADS=hyads2005,HyADSpm25=hyadspm2005,INMAP=inmap2005)
    names(exposures) <- exposure.vars.compare

    # make this reactive to the matching parameters ONLY, not the plots, to avoid recomputing
    more.models <- sapply(exposures,
                          function(x){
                            return(analyzeMatches(exposure=x,covariates=covariates,
                                                  regions=match.regions,covariate.vars="all",
                                                  exposure.cutoff.percentile=cutoff,
                                                  match.method=match.method,caliper.type="default",
                                                  caliper.threshold=caliper.threshold,
                                                  quantiles=quantiles))},
                          simplify=F, USE.NAMES=T)

    # make regional exposure histograms
    if("exp.hist" %in% input$stuff.to.plot){
      output$exp.hists <- renderUI({
        maps.list <- lapply(1:length(more.models), function(i){
          name <- paste("exphist",i,sep="")
          plotOutput(name)
        })
        do.call(tagList, maps.list)
      })
      for(i in 1:length(more.models)){
        local({
          idx <- i
          plotname <- paste("exphist", idx, sep = "")
          m <- more.models[[idx]]
          n <- names(more.models)[idx]
          cutoff <- formatC(signif(m$cutoff[[1]], digits = 3), format="e")
          title <- paste("Exposure histogram for ", n, " (cutoff : ", cutoff, ")", sep = "")
          output[[plotname]] <- renderPlot({
            plotExposureHistogram(m, title)
          })
        })
      }
    } else {
      output$exp.hists <- NULL
    }

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

    # make the confusion matrices
    if("conf.mat" %in% input$stuff.to.plot){
      # if matches for ground truth exposure not computed, get them now
      m <- list()
      if(!exposure.var.confmat %in% exposure.vars.compare){
        # ---- **** add more exposures here **** -----
        exposure.confmat <- switch(exposure.var.confmat,
                                   CMAQ=cmaqddm2005,HyADS=hyads2005,HyADSpm25=hyadspm2005,INMAP=inmap2005)
        m.out <- analyzeMatches(exposure=exposure.confmat,covariates=covariates,regions=match.regions,
                                covariate.vars="all",exposure.cutoff.percentile=cutoff,match.method=match.method,
                                caliper.type="default",
                                caliper.threshold=caliper.threshold,quantiles=quantiles)
        m <- list(m.out)
        names(m) <- c(exposure.var.confmat)
      }
      all.models <- c(more.models, m)
      # make them!
      mats.to.plot <- createConfMat(all.models, ground.truth = exposure.var.confmat)
      if (length(mats.to.plot) > 0) {
        output$conf.mats <- renderUI({
          plot.output.list <- lapply(1:length(mats.to.plot), function(i){
            name <- paste("plot", i, sep="")
            tableOutput(name)
          })
          do.call(tagList, plot.output.list)
        })
        for(i in 1:length(mats.to.plot)){
          local({
            idx <- i
            plotname <- paste("plot", idx, sep = "")
            # figure out how many zips are included in a dataset but not the other
            drops <- mats.to.plot[[idx]][[2]]
            msg <- ifelse(drops < 0,
                          paste("Removed", drops, "zips from", input$exposure.var, "raw data"),
                          paste("Removed", drops, "zips from", names(mats.to.plot)[idx], "raw data"))
            acc <- paste(100*signif(mats.to.plot[[idx]][[1]]$overall[1], digits=3), "% agreement in classification", sep = "")
            output[[plotname]] <- renderTable(mats.to.plot[[idx]][[1]]$table, rownames=T,
                                              colnames = T,
                                              caption = paste(msg, acc))
          })
        }
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

    # make the effect CIs
    if("effect" %in% input$stuff.to.plot) {
      output$effect <- renderUI({
        effect.list <- lapply(1:length(more.models), function(i){
          name <- paste("effect", i, sep="")
          plotOutput(name, width = "66%")#, width = paste(95/length(mats.to.plot),"%", sep=""))
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
    if("prop.hist" %in% input$stuff.to.plot) {
      output$prop.hists <- renderUI({
        effect.list <- lapply(1:length(more.models), function(i){
          name <- paste("phist", i, sep="")
          plotOutput(name, width = "66%")
        })
        do.call(tagList, effect.list)
      })
      for(i in 1:length(more.models)) {
        local({
          idx <- i
          plotname <- paste("phist",idx,sep="")
          output[[plotname]] <- renderPlot({
            title <- paste("Propensity scores for ", names(more.models)[idx], sep="")
            plotPropensityScoreHistogram2(more.models[[idx]], title)
          })
        })
      }
    } else {
      output$prop.hists <- NULL
    }
  })
}
shinyApp(ui, server)
