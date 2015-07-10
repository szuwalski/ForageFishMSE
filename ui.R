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
                                          tags$div(title="Sets the fraction of the population harvest by commercial fisheries for adult herring",sliderInput("A.harv","Adult harvest",min=0,max=1,value=defaultVals[1],step=.01), style = "font-size:60%"),
                                          tags$div(title="Sets the fraction of herring eggs that may be harvests as spawn-on-kep for commercial and traditional uses",sliderInput("E.harv","Egg harvest",min=0,max=1,value=defaultVals[2],step=.01), style = "font-size:60%"),
                                          tags$div(title="Sets the population size at which commercial fisheries for herring are closed as a fraction of unfished biomass",sliderInput("harvest.floor","Cut-off",min=0,max=1,value=defaultVals[3],step=.01), style = "font-size:60%"),
                                          div(helpText("Environmental influence"),style="font-size:80%"),
                                          tags$div(title="Sets how much herring recruitment fluctuates from year to year",sliderInput("CV.recruit","Recruitment variability",min=0,max=1,value=defaultVals[4],step=.01), style = "font-size:60%"),                                         
                                          tags$div(title="Sets how similar ocean conditions are from year to year; larger values imply more regime-like behavior",sliderInput("AR","Ocean regimes",min=0,max=1,value=defaultVals[5],step=.01), style = "font-size:60%" ),
                                          actionButton("save", "Save simulation")
                                        ),
                                        mainPanel(plotOutput("biomass",width="100%",height=520))
                                      )
                                      
                             ),
                             tabPanel("History",
                                      mainPanel(tableOutput("table")))
                           )
                        )
                      )