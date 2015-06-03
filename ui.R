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
shinyUI(fluidPage(navbarPage("Forage fish MSE",
                             
                             # INFORMATION INTERFACE (NEEDS TOC)
                             tabPanel("About",
                                      fluidRow(
                                        includeMarkdown("About.md")
                                      )
                                      
                             ),
                             # Defining the spatial extent 
                             tabPanel("Simulation",
                                      sidebarLayout(
                                        sidebarPanel(
                                          numericInput("N.sim", label = "Number of simulations", value = 100),
                                          numericInput("sim.length", label = "Length of simulations", value = 50),
                                          numericInput("Init.f.biomass", label = "Initial female biomass", value = 25000),
                                          sliderInput("A.harv","Adult harvest",min=0,max=1,value=.1,step=.01),
                                          sliderInput("E.harv","Egg harvest",min=0,max=1,value=.1,step=.01),
                                          sliderInput("CV.recruit","Recruitment CV",min=0,max=1,value=.5,step=.01),                                         
                                          sliderInput("AR","Recruitment autocorrelation",min=0,max=1,value=.5,step=.01), 
                                          sliderInput("harvest.floor","Harvest floor",min=0,max=1,value=.5,step=.01)
                                        ),
                                        mainPanel(plotOutput("biomass"))
                                      )
                                      
                             )
                           )
                        )
                      )