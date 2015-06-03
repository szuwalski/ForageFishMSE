library(shiny)
library(devtools)


source("helpers.R")
source("Herring Scenarios FIN Web.R")
source("Herring Sim Function Web.R")
#source("https://raw.githubusercontent.com/SNAPwg/SNAP/master/Master.R")

## ------------------------------------------------------------------------------------ ##
## Define server logic required to run scripts
## ------------------------------------------------------------------------------------ ##
shinyServer(function(input, output, session) {
  
  # Subset Dataframe based on User Interface Selection.
  output$biomass <- renderPlot({
    ForageFun(input)
  })

#  output$MScomp <- renderPlot({
#    
#    Graphs<-F
#    GraphsFish<-F
#    PrintLifeHistory<-F
#    Life<-read.csv("LifeHistory.csv")                          # life history characteristics
#    SimCTL<-read.csv("GrandSimCtlBLZ.csv",header=F)               # simulation controls
#    Fleets<-read.csv("Fleets.csv",header=F)                    # fleet characteristics  
#    season<-read.csv("season.csv",header=F)                    # fishing seasons by fleet
#    Samp <- read.csv("SamplingParams.csv")                     # sampling controls for management
#    NoTakeZoneNULL<-read.csv("notakezoneBLZNULL.csv",header=F)   # marine protected areas (0=open access, 1=MPA, 2=TURF?)
#    NoTakeZoneImp<-read.csv("notakezoneBLZ.csv",header=F)      # marine protected areas (0=open access, 1=MPA, 2=TURF?)
#    habitat<-read.csv("habitatBLZ.csv",header=F)                # habitat quality (recruitment suitability)
#  
#    OpenAccess<-Master(Life,SimCTL,Fleets,season,Samp,NoTakeZoneNULL,NoTakeZoneImp,habitat,Graphs,GraphsFish,PrintLifeHistory)
#    OAtotCatch<-apply(OpenAccess$CatchByFisher,2,sum,na.rm=T)
#    OAtotCost<-apply(OpenAccess$CostByFisher,2,sum,na.rm=T)
#    OAtotProfit<-apply(OpenAccess$ProfitByFisher,2,sum,na.rm=T)
#    
#    burnInt   <-SimCTL[grep('burn',SimCTL[,2]),1]
#    simTimePlt <-SimCTL[grep('simTime',SimCTL[,2]),1]
#    initManage<-SimCTL[grep('initManage',SimCTL[,2]),1]   # year in which to initiate management
#    yearMark2  <-SimCTL[grep('yearMark',SimCTL[,2]),1]      # number of time steps in a year
#    SpcRow    <-SimCTL[grep('SpaceR',SimCTL[,2]),1]	    	# Rows in the grid space
#    SpcCol	  <-SimCTL[grep('SpaceC',SimCTL[,2]),1]  			# cols in the grid spcae
#    
#    par(mfrow=c(7,1),mar=c(.1,6,.1,.1))
#    plot(OAtotCatch,type="b",xaxt='n',las=2,ylim=c(0,max(OAtotCatch,na.rm=T)),ylab="Total Catch",pch=16)
#    abline(v=initManage-burnInt,col=2,lty=2)
#    legend("topright",col=2,lty=2,"Management implemented",bty='n')
#    plot(OAtotCost,lty=2,type="b",pch=16,xaxt='n',las=2,ylim=c(0,max(OAtotCost,na.rm=T)),ylab="Total cost of fishing")
#    abline(v=initManage-burnInt,col=2,lty=2)
#    
#    plot(OAtotProfit,lty=2,type="b",pch=16,xaxt='n',las=2,ylim=c(0,max(OAtotProfit,na.rm=T)),ylab="Total profit of fishing")
#    abline(v=initManage-burnInt,col=2,lty=2)
#    
#    plot(OpenAccess$CostOfManagement[burnInt:(simTimePlt)],pch=16,type="b",xaxt='n',las=2,ylim=c(0,max(OpenAccess$CostOfManagement,na.rm=T)),
#         ylab="Cost of MPA")
#    abline(v=initManage-burnInt,col=2,lty=2)
#    
#    plot(OpenAccess$SpawningBiomass[burnInt:simTimePlt],pch=16,type="b",xaxt='n',las=2,
#         ylab="SpawningBio",ylim=c(0,max(OpenAccess$SpawningBiomass[burnInt:simTimePlt],na.rm=T)))
#    abline(v=initManage-burnInt,col=2,lty=2)
#    
#    plot(OpenAccess$ExploitableNumbers[burnInt:simTimePlt],pch=16,type="b",xaxt='n',las=2,
#         ylab="Exploitable Numbers",ylim=c(0,max(OpenAccess$ExploitableNumbers[burnInt:simTimePlt],na.rm=T)))
#    
#    plot(OpenAccess$OutsideMPAspbio[burnInt:simTimePlt],ylim=c(0,max(OpenAccess$OutsideMPAspbio[burnInt:simTimePlt],OpenAccess$InsideMPAspbio[burnInt:simTimePlt])),type='b',pch=16)
#    lines(OpenAccess$InsideMPAspbio[burnInt:simTimePlt],type='b',pch=16,col=2)
#    abline(v=initManage-burnInt,col=2,lty=2)
#    
#    legend("topright",col=c(1,2),pch=16,legend=c("Outside MPA","Inside MPA"),bty='n') 
# 
#  })



  
})  # End of ShinyServer
## ------------------------------------------------------------------------------------ ##










