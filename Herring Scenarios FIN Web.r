############################################################################
# THIS IS THE SAME AS OLE'S "HERRING SCENARIOS.TXT" FILE, CHANGED TO A .R FILE
# JAMEAL ALSO ADDED SEVERAL NEW OUTPUTS RELATED TO DURATION OF CLOSURES, BOOMS AND BUSTS
############################################################################

#rm(list=ls())
#setwd("/Users/Cody/Desktop/fwdreshiny")
#source("Herring Sim Function Web.r")

ForageFun<-function(input)
{
#### Herring simulation under different fishing regimes
Logit	<- function(a,b,x){	1 / (1+exp(-(a+b*x)))}

# Function for turning CV on the log-normal scale into standard deviations you can plug into R functions 
log.norm.cv		<- function(cv){
					 sigma2	<-	 log(cv^2 + 1)
					 return(sqrt(sigma2))
					 }

### Simulation details
N.sim				<-  input$N.sim  # Number of Simulation for each Fishing Scenario
sim.length			<-  input$sim.length
Init.f.biomass		<-  input$Init.f.biomass 

#### SET UP ADULT HARVEST PROPORTION (must be between 0,1):
A.harv		<-	input$A.harv
#### SET UP EGG HARVEST PROPORTION (must be between 0,1):
E.harv		<-	input$E.harv
#### SET UP Recruitment Variability:
CV.recruit		<- 	input$CV.recruit
#### SET UP AR1 structure for recruits.
AR 		   	<- 	input$AR
#### SET UP Harvest limit - pop size at which fishery closes (Fraction of B0)
harvest.floor	<-	input$harvest.floor

#NAME	<- paste("Harv.floor =",harvest.floor)

#########################################################################################
##########################################################################################
## --- Basic Biology Inputs
#########################################################################################
##########################################################################################
### Basic Demography
	recruit.age	=	2
	plus.age	=	10

	ages		<-	recruit.age:plus.age
### Maturity
	maturity	<-	c(0.25,0.85,0.920,0.95,0.967,0.98,0.99,0.995,0.999)

	# Approximately from Hay and McCarter (1999)... with liberal interpretation.
#   	 	plot(maturity~ages)

### Sex ratio (at birth)
	sex.ratio = 0.5

### Selectivity on adults - for each gear type.
	sel.a.seine	<- 0
	sel.b.seine	<- 1		
	seine.sel	<-	rep(1,length(ages)) #Logit(sel.a.seine,sel.b.seine,ages)

	sel.a.gillnet	<- -10
	sel.b.gillnet	<- 2.2		
	gillnet.sel	<-	Logit(sel.a.gillnet,sel.b.gillnet,ages)
	pound.sel	<-	rep(1,length=length(ages))

	# 	par(mfrow=c(2,2))
	# 	y.lim = c(0,1)
	# 	plot(seine.sel~ages,ylim=y.lim,main="Seine")
	# 	plot(gillnet.sel~ages,ylim=y.lim,main="Gillnet")
	# 	plot(pound.sel~ages,ylim=y.lim,main="Pound")
	# 	plot(rk.sel~ages,ylim=y.lim,main="Roe on Kelp")
	
# Weight at age (grams) ### Average over the last decade (2001-10) from stock assessment
	weight	<-		c(51.76, 70.51,	87.56,	104.39,	116.47,	126.41,	138.34,	144.33,	152.28)
 	par(mfrow=c(1,1))
#  	plot(weight~ages,type="b")

# Fecundity at weight (grams)  # Based on Sitka Sound data 2005
	fecund.a	<-	-3572.5
	fecund.b	<-	204

	XXX	<- 70:150
#  		plot(XXX,Fecund(fecund.a,fecund.b,XXX),ylab="Eggs",xlab="Mass(g)")
	eggs.per.gram.age	<-	(Fecund(fecund.a,fecund.b,weight)/weight)
	eggs.per.gram		<- 170

# Adult Natural Mortality
	mean.mortality = 0.67
	sd.mortality   = 0.01
	p.surv		<-	exp(-mean.mortality)

# Recruitment
	#Recruit = "Ricker"
		alpha.r	<- 	0.0002
		beta.r	<- 	0.0005
		
 	#Recruit = "BH"
  		alpha.bh	<-	4000
  		beta.bh		<-  6	

					
	## CLOSE ENOUGH BASELINES FROM Martell Stock Assessment	
		B0 = 42000
		R0 = 500 * 1e6
		h  = 0.76
					
		alpha.temp		<-	(B0 / R0) * (1-h)/(4*h)
		beta.temp		<-	(5*h-1)/(4*h*R0)		
					
		R	<-	bev.holt(alpha.temp,beta.temp,(seq(1,60000,by=1000)))

		#NEED TO CONVERT TO EGGS - Recruit relationship
		eggs	<- eggs.per.gram * seq(1,60000,by=1000)* 1e6 /2
		# kluge-y fast way to approximate egg-recruit relationship.
# 		plot(eggs,R)		

		NLL	<- function(PAR.start,Rec, B){
				ALPHA = PAR.start[1]
				BETA  = PAR.start[2]
				Recruits = bev.holt(ALPHA,BETA,B)

				return(	sum((Recruits - Rec)^2))
		}

		PAR.start	<- c(0.2,0.002)			
		OUT <- optim(PAR.start ,NLL ,Rec = R/1e6, B=eggs/1e9,
				control=list(parscale=c(1e-5,1e-5)))			

#		B= eggs/1e9
#		R2=bev.holt(OUT$par[1],OUT$par[2],B)
#		R3=bev.holt(0.2,0.002,B)
# 			
# 		par(mfrow=c(3,1))
# 		plot(eggs/1e9,R/1e6)
# 		plot(B,R2)
# 		plot(B,R3)
# 	
	alpha.bh	<-	OUT$par[1]
	beta.bh		<-	OUT$par[2]	
		### OUTPUT UNITS ARE IN 1e6 of individuals, B is in 1e9 eggs. 
			
 	#VARIABILITY in recruitment
	X		<-	1:20000
	eggs	<-	eggs.per.gram * X*1e6 / 1e9 # Billions of eggs
	RIC		<-	ricker(Alpha=alpha.r,Beta=beta.r,B=eggs)
	BH		<-	bev.holt(Alpha=alpha.bh,Beta=beta.bh,B=eggs)

	 	par(mfrow=c(1,1))
	 	y.lim=c(0,max(RIC,BH))
#	 	plot(X,RIC,type='l',col=4,ylim=y.lim,lwd=2,ylab="")
#	 	par(new=T)	
#	 	plot(X,BH,type='l',col=2,ylim=y.lim,lwd=2,ylab="")
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
 
# Outputs:
# 
# Frequency of Closures
# Average Catch, SD of Catch.
# Duration of closures, Occurrence of closures 3+ years
# Occurence of booms and busts

base.length			<-	1000

Init.f.biomass.age	<-	(p.surv^(1:length(ages)))/sum(p.surv^(1:length(ages)))*Init.f.biomass # metric tons
Init.f.numb.age		<-	(Init.f.biomass.age * 1e6) / weight
FRAC				<-  0.6 	# proportion of plus group > (age = plus group), in years before the fishery begins...

### Run a deterministic Simulation to determine B0 and harvest lower limit
rec.var <- rep(1,base.length+recruit.age)

base.sim <- H.pop.dyn(
  Sim.length	= base.length,
  Recruit.age = recruit.age,
  Recruit.rand = "FALSE",
  Mort.rand 	= "FALSE",
  Plus.age	= plus.age,
  Maturity 	= maturity,
  Select	 	= seine.sel,
  Weight	   	= weight,
  Harvest.Limit = 0,
  Egg.Harvest	= 0,
  Adult.Harvest = 0,
  Recruit.type = "BH",
  Sex.ratio	= sex.ratio,
  Init.f.numb.age = Init.f.numb.age,
  Init.f.biomass.age = Init.f.biomass.age,
  p.surv = p.surv,
  FRAC = FRAC,
  eggs.per.gram.age = eggs.per.gram.age,
  alpha.bh = alpha.bh,
  beta.bh = beta.bh,
  egg.prod = egg.prod,
  rec.var = rec.var,
  fecund.a = fecund.a,
  fecund.b = fecund.b
)
#plot(rowSums(base.sim$Mat.out))

B0			<- unlist(rowSums(base.sim$Mat.out)[nrow(base.sim$Mat.out)]);names(B0)<-"B0"
harv.limit	<- B0*harvest.floor

closure.categories <- c()
for(zzz in 1:(sim.length+2)){
  closure.categories <- c(closure.categories,paste("Closure duration",zzz,"years")) # columns headers for each closure duration
}

#### THIS IS THE data frame of simulations to simulate over.
scenario			<-	data.frame( expand.grid(adult.harvest.prop=A.harv,
                                      egg.harvest.prop=E.harv,
                                      CV.recruit=CV.recruit,
                                      AR.recruit = AR))
scenario$Sigma.recruit	<- log.norm.cv(scenario$CV.recruit)
##########################################################################################
##### -------- Simulate -------------
##########################################################################################

A <- date()
A
for(ZZ in 1:nrow(scenario)){
  print(paste(ZZ, "of",nrow(scenario)))
  # for(ZZ in 1:2){
  
  sim.summary	<- data.frame(matrix(0, N.sim, 13))
  colnames(sim.summary)	<-	c("number","mean.spawn.biomass","cv.spawn.biomass","n.closure","prop.closed",
                             "mean.catch.adult","cv.catch.adult","mean.catch.eggs","cv.catch.eggs","booms","busts","n.long.closures","duration.closures")
  
  for(YY in 1:N.sim){
    # Set up recruitment variation
      		log.rec.var	<- rep(0,sim.length+recruit.age)
      		log.rec.var[1]	<-	rnorm(1, mean= 0, sd = scenario$Sigma.recruit[ZZ])  
      for(i in 2:(sim.length+recruit.age)){
         	log.rec.var[i] <- rnorm(1, mean= log.rec.var[i-1]*scenario$AR.recruit[ZZ] , sd = scenario$Sigma.recruit[ZZ])
      }
      log.rec.var	<-	log.rec.var * sqrt( 1 - scenario$AR.recruit[ZZ]^2) - 0.5 * scenario$Sigma.recruit[ZZ]^2
	  rec.var <- exp(log.rec.var)

# Checks to make sure recruitment variability is doing what it is supposed to
#    mean(log.rec.var)
#    mean(rec.var)
#    median(log.rec.var)
#    median(rec.var)
#    var(log.rec.var)
#	var(rec.var)
#    (scenario$Sigma.recruit[ZZ])^2 / ( 1 - scenario$AR.recruit[ZZ]^2)
#	(scenario$Sigma.recruit[ZZ])^2
    
    # Run Simulation	
    Output <- H.pop.dyn(
      Sim.length	= sim.length,
      Recruit.age   = recruit.age,
      Recruit.rand  = "TRUE",
      Mort.rand 	= "FALSE",
      Plus.age	    = plus.age,
      Maturity 	    = maturity,
      Select	 	= seine.sel,
      Weight	   	= weight,
      Harvest.Limit = harv.limit,
      Egg.Harvest	= scenario$egg.harvest.prop[ZZ],
      Adult.Harvest = scenario$adult.harvest.prop[ZZ],
      Recruit.type  = "BH",
      Sex.ratio	    = sex.ratio,
 	Init.f.numb.age = Init.f.numb.age,
	Init.f.biomass.age = Init.f.biomass.age,
	p.surv = p.surv,
	FRAC = FRAC,
	eggs.per.gram.age = eggs.per.gram.age,
	alpha.bh = alpha.bh,
	beta.bh = beta.bh,
	egg.prod = egg.prod,
	rec.var = rec.var,
	fecund.a = fecund.a,
	fecund.b = fecund.b
    )

	# Save the spawning biomass metrics
    if(YY == 1){ spawn.biomass  <- rowSums(Output$Mat.out)[(recruit.age+1):(sim.length+recruit.age)]}
    if(YY > 1) { spawn.biomass	<-	cbind(spawn.biomass,rowSums(Output$Mat.out)[(recruit.age+1):(sim.length+recruit.age)])}
        
    # PUT QUANTITIES OF INTEREST HERE 
    sim.summary$number[YY] <- YY		
    
    sim.summary$mean.spawn.biomass[YY]	<- mean(rowSums(Output$Mat.out)[(recruit.age+1):(sim.length+recruit.age)])
    sim.summary$cv.spawn.biomass[YY] 	<- sd(rowSums(Output$Mat.out)[(recruit.age+1):(sim.length+recruit.age)]) /
      mean(rowSums(Output$Mat.out)[(recruit.age+1):(sim.length+recruit.age)])	
    
    sim.summary$mean.catch.adult[YY] 	<- mean(rowSums(Output$Catch.out)[(recruit.age+1):(sim.length+recruit.age)])
    sim.summary$cv.catch.adult[YY] 		<- sd(rowSums(Output$Catch.out)[(recruit.age+1):(sim.length+recruit.age)]) /
      mean(rowSums(Output$Catch.out)[(recruit.age+1):(sim.length+recruit.age)])
    sim.summary$mean.catch.eggs[YY] 	<- mean(Output$Catch.egg[(recruit.age+1):(sim.length+recruit.age)])
    sim.summary$cv.catch.eggs[YY] 		<- sd(Output$Catch.egg[(recruit.age+1):(sim.length+recruit.age)]) /
      mean(Output$Catch.egg[(recruit.age+1):(sim.length+recruit.age)])
    
    sim.summary$n.closure[YY]			<- length(which(Output$Adult.harvest.rate[(recruit.age+1):(sim.length+recruit.age)]==0))
    sim.summary$prop.closed[YY]			<- sim.summary$n.closure[YY] / sim.length
    
    # JAMEAL'S ADDITIONS HERE
    # booms
    sim.summary$booms[YY] <- length(which(rowSums(Output$Mat.out)[(recruit.age+1):(sim.length+recruit.age)] >= B0))
        
    # busts
    sim.summary$busts[YY] <- length(which(rowSums(Output$Mat.out)[(recruit.age+1):(sim.length+recruit.age)] < 0.1*B0))
    
    # summarize output related to duration of closures
    myr <- rle(Output$Adult.harvest.rate)
    duration <- myr$length[myr$values == 0]
    
    # occurrence of closures of 3+ years
    sim.summary$n.long.closures[YY] <- length(which(duration >= 3)) 
    
    # duration of all closures in the simulation
    sim.summary$duration.closures[YY] <- list(duration)
  } # End N.sim loop
  
  # calculate for summary stats for replicate runs for each scenario	
  
  # drop duration of closures from summary data frame
  sim.summary2 			<- subset(sim.summary,select=-c(duration.closures))
  temp					<- unlist(apply(sim.summary2[,2:ncol(sim.summary2)],2,mean))
  temp$cv.catch.adult	<-	mean(sim.summary2$cv.catch.adult[is.nan(sim.summary2$cv.catch.adult)==F])
  temp$cv.catch.eggs	<-	mean(sim.summary2$cv.catch.eggs[is.nan(sim.summary2$cv.catch.adult)==F])
  
  # pool closure durations for all N.sim simulations, save counts by duration category
  BREAKS	= seq(0.5,sim.length+2.5,by=1) # +2.5 to allow for maturation of final recruitment year
  AAA		<-hist(unlist(sim.summary$duration.closures),breaks=BREAKS,plot=FALSE)
  #AAA$counts    
  temp2 <- setNames(AAA$counts,closure.categories)    
  # 
  sim.biomass.mean	<-	rowMeans(spawn.biomass)
  sim.biomass.quant <-	data.frame(t(apply(spawn.biomass, 1, quantile,probs=c(0.025,0.25,0.75,0.975))))
  
  if(ZZ == 1){
    sim.output	<-	c("Sim.length"=sim.length,"SSB.F"= unlist(rowSums(Output$Mat.out)[recruit.age]),
                    "N.sim"=N.sim,
                    unlist(scenario[ZZ,]),B0,"Harvest.Floor"=harvest.floor, temp, temp2)
  }
  if(ZZ > 1){
    sim.output	<- rbind(sim.output,
                        c("Sim.length"=sim.length,"SSB.F"= unlist(rowSums(Output$Mat.out)[recruit.age]),
                          "N.sim"=N.sim, unlist(scenario[ZZ,]),B0,"Harvest.Floor"=harvest.floor,temp,temp2))
    }
}	#End Scenario loop
B <- date()

A
B	
 
# Make a some mock-up plots 
# Mean Time-Series + confidence intervals + a few example realizations

THESE	<- sample(1:N.sim,3,replace=F)
 
x.lim=c(1,sim.length)
y.lim=c(0, max(sim.biomass.quant$X97.5.,spawn.biomass[,THESE]))

plot(sim.biomass.mean,xlim=x.lim,ylim=y.lim, axes=F,type="l",col=4,lwd=2,xlab="",ylab="",yaxs="i")
par(new=T) 
plot(sim.biomass.quant$X25.,,xlim=x.lim,ylim=y.lim, axes=F,type="l",col=4,lwd=1,lty=2,xlab="",ylab="",yaxs="i")
par(new=T) 
plot(sim.biomass.quant$X75.,xlim=x.lim,ylim=y.lim, axes=F,type="l",col=4,lwd=1,lty=2,xlab="",ylab="",yaxs="i")
par(new=T) 

high	<-	data.frame(cbind(Year=1:sim.length,Q=sim.biomass.quant$X75.))
low		<-	data.frame(cbind(Year=1:sim.length,Q=sim.biomass.quant$X25.))
low		<-	low[order(low$Year,decreasing=T),]
both	<-  rbind(high,low)
polygon(x=both$Year,y=both$Q,col="#0000FF30",border=NA)

high	<-	data.frame(cbind(Year=1:sim.length,Q=sim.biomass.quant$X97.5.))
low		<-	data.frame(cbind(Year=1:sim.length,Q=sim.biomass.quant$X2.5.))
low		<-	low[order(low$Year,decreasing=T),]
both	<-  rbind(high,low)
polygon(x=both$Year,y=both$Q,col="#0000FF30",border=NA)

for(i in 1:length(THESE)){
	par(new=T) 
	plot(spawn.biomass[,THESE[i]],xlim=x.lim,ylim=y.lim, axes=F,type="l",col=1,lwd=1,lty=i+1,xlab="",ylab="",yaxs="i")
}

axis(1)
axis(2,las=T)
axis(2,las=T,at=c(B0*harvest.floor,B0),label=c(expression(B[lim]),expression(B[0])))
box(bty="l",lwd=2)
abline(h=B0*harvest.floor,lty=3,lwd=1.5) 
abline(h=B0,lty=3,lwd=1.5) 
  
 ############# THIS IS THE SIMPLY SUMMARY OF SOME OF THE ATTRIBUTES OF INTEREST
 output	<-	data.frame(values=unlist(sim.output))
 output
 #############
}

# input<-NULL
# input$N.sim<-100
# input$sim.length<-50
# input$Init.f.biomass<-25000
# input$A.harv<-.2
# input$E.harv<-.1
# input$CV.recruit<-.6
# input$AR<-.5
# input$harvest.floor<-.25
#  ForageFun(input)
#write.csv(sim.output,file=paste("Sim output test",harvest.floor,"lim.csv"),row.names=F)