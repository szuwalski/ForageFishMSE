library(shiny)
library(markdown)
library(ggmap)
library(shinyIncubator)
# RENDER EQUILIBRIUM USER INTERFACE
renderBanner <- function()
{
  wellPanel(
    # Logo image
    column(9),
    column(3,
           img(src="iphclogo.png",  height = 80, width = 80),
           img(src="iscamLogo.png", height = 80, width = 80)
    )
  )
}

# ----------------------------------------#
# MAIN USER INTERFACE FOR THE APPLICATION #
# ----------------------------------------#

defaultVals<-c(0.1,.1,.3,.5,.5)

shinyUI(fluidPage(navbarPage("",
                             
                             # INFORMATION INTERFACE (NEEDS TOC)
                             tabPanel("About",
                                      sidebarLayout(sidebarPanel(
                                        includeMarkdown("About.md")),
                                        mainPanel(img(src="sf_herring.jpg"))
                                      )
                                      
                             ),
                             # Defining the spatial extent 
                             tabPanel("Simulation",
                                      sidebarLayout(
                                        sidebarPanel(width=3,
                                          div(helpText("Management decisions (mouse over for description)"), style = "font-size:80%"),
                                          tags$div(title="Proportion of adults harvested",sliderInput("A.harv","Adult harvest",min=0,max=1,value=defaultVals[1],step=.01), style = "font-size:60%"),
                                          tags$div(title="Proportion of eggs harvested",sliderInput("E.harv","Egg harvest",min=0,max=1,value=defaultVals[2],step=.01), style = "font-size:60%"),
                                          tags$div(title="Fraction of unfished biomass at which no harvest occurs",sliderInput("harvest.floor","Cutoff",min=0,max=1,value=defaultVals[3],step=.01), style = "font-size:60%"),
                                          div(helpText("Environmental influence"),style="font-size:80%"),
                                          tags$div(title="Higher values equal more variability",sliderInput("CV.recruit","Magnitude of variability",min=0,max=1,value=defaultVals[4],step=.01), style = "font-size:60%"),                                         
                                          tags$div(title="Higher values equal more regime-like behavior",sliderInput("AR","Regime-like dynamics",min=0,max=1,value=defaultVals[5],step=.01), style = "font-size:60%" )                                         
                                        ),
                                        mainPanel(plotOutput("biomass",width="100%",height=520))
                                      )
                                      
                             ),
                             tabPanel("History",
                                      mainPanel(tableOutput("table")))
                           )
                        )
                      )