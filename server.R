library(shiny)
library(devtools)


source("helpers.R")
source("HerringScenariosFINWeb.r")
source("Herring Sim Function Web.r")
#source("https://raw.githubusercontent.com/SNAPwg/SNAP/master/Master.R")

## ------------------------------------------------------------------------------------ ##
## Define server logic required to run scripts
## ------------------------------------------------------------------------------------ ##
defaultVals<-c(0.1,.1,.3,.5,.5)
shinyServer(function(input, output, session) {
  
  # Subset Dataframe based on User Interface Selection.
  output$biomass <- renderPlot({
    ForageFun(input)
  })

  values <- reactiveValues()
  values$df <- data.frame(AdultHarvest = NA,EggHarvest = NA,Cutoff = NA,Magnitude = NA, Regime = NA)
  newEntry <- observe({
      newLine <- isolate(c(input$A.harv,input$E.harv,input$harvest.floor,input$CV.recruit,input$AR))
      isolate(values$df <- rbind(values$df, newLine))
  })
  
  output$table <- renderTable({values$df})
  

  
})  # End of ShinyServer
## ------------------------------------------------------------------------------------ ##










