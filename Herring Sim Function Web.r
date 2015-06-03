
H.pop.dyn	<-	function(
					Sim.length,	
					Recruit.age, 
					Recruit.rand,
					Mort.rand,
					Plus.age,	
					Maturity, 	
					Select,	 	
					Weight,
					Harvest.Limit,	   	
 					Egg.Harvest,	
					Adult.Harvest,
					Recruit.type, 
					Sex.ratio,
					Init.f.numb.age,
					Init.f.biomass.age,
					p.surv,
					FRAC,
					eggs.per.gram.age,
					alpha.bh,
 					beta.bh,
 					egg.prod,
					rec.var,
					fecund.a,
					fecund.b){
		
		N.out		<-	matrix(0,Sim.length+Recruit.age,length(Recruit.age:Plus.age))
		B.out		<-	N.out
		Mat.out		<-	N.out
		catch.out	<-	N.out
		
		N.out[Recruit.age,]	<-	Init.f.numb.age	
		B.out[Recruit.age,]	<-	Init.f.biomass.age

		colnames(N.out)		<- paste("age",Recruit.age:Plus.age,sep=".")
		colnames(B.out)		<- colnames(N.out)
		colnames(Mat.out)	<- colnames(N.out)
		colnames(catch.out)	<- colnames(N.out)
		
		rownames(N.out)[Recruit.age:(Recruit.age+Sim.length)]	<- paste("year",0:Sim.length,sep=".")
		rownames(N.out)[(Recruit.age-1):1]						<-  paste("year.minus",1:(Recruit.age-1),sep=".")
		rownames(B.out)		<- rownames(N.out)
		rownames(Mat.out)	<- rownames(N.out)
		rownames(catch.out)	<- rownames(N.out)

		catch.eggs	<-	rep(0,Sim.length+Recruit.age)

	# Set up harvesting regime for the whole time series 
		egg.harvest		<- rep(0,nrow(N.out))
		adult.harvest   <- rep(0,nrow(N.out))
		
	# Set up natural morality for the entire time series (make stochastic however you feel like it)
		p.surv	<- rep(p.surv,nrow(N.out)) 
		
	# Set up initial conditions (first year is the first year fishing has occurred)
		for(j in  Recruit.age:2){					
			N.out[(j-1),1:(length(Recruit.age:Plus.age)-1)]		<-  	N.out[j,2:length(Recruit.age:Plus.age)] * (p.surv[j])^(-1)
			N.out[(j-1),(length(Recruit.age:Plus.age)-1)]		<- 		N.out[(j),(length(Recruit.age:Plus.age))] * (1-FRAC)
			N.out[(j-1),length(Recruit.age:Plus.age)]			<- 		N.out[(j),(length(Recruit.age:Plus.age))] * (FRAC)
		}

		B.out	<-	t(apply(N.out,1,"*",Weight)) / 1e6
		Mat.out	<-	t(apply(B.out,1,"*",Maturity)) 

	# Simulate across years					
		for(i in Recruit.age:(Sim.length+Recruit.age-1)){

			# IF YOU WANT TO MAKE EGG HARVEST or ADULT HARVEST DYNAMIC, YOU COULD DO IT HERE instead of above
				
			# New Recruits to recruit.age
# 				egg.prod	<- 	sum(Fecund(fecund.a,fecund.b,Mat.out[i+1-Recruit.age,]*1e6)/1e9)
				egg.prod	<- 	sum((eggs.per.gram.age * Mat.out[i+1-Recruit.age,]*1e6)/1e9)


			if(egg.harvest[i+1-Recruit.age] > 0 & (i+1-Recruit.age)>Recruit.age){	# if there was a fishery.
# 				egg.prod	<- 	sum(Fecund(fecund.a,fecund.b,Mat.out[i+1-Recruit.age,]*1e6)/1e9) * (1-egg.harvest[i+1-Recruit.age])
				egg.prod	<- 	sum((eggs.per.gram.age * Mat.out[i+1-Recruit.age,]*1e6)/1e9) * (1-egg.harvest[i+1-Recruit.age])
			}				
				
					if(Recruit.type == "Ricker"){
						N.out[i+1,1]	<-	Sex.ratio * ricker(alpha.r,beta.r,egg.prod) * rec.var[i] * 1e6
					}
					if(Recruit.type == "BH"){
						N.out[i+1,1]	<-	Sex.ratio * bev.holt(alpha.bh,beta.bh,egg.prod) * rec.var[i] * 1e6
					}				
		
			# Survive to next year	
					N.out[(i+1),(2:ncol(N.out))]	<- 	N.out[ i,(1:ncol(N.out)-1)]*p.surv[i]
					N.out[(i+1),ncol(N.out)]		<-	N.out[ i,(ncol(N.out)-1)]*p.surv[i] + N.out[ i,ncol(N.out)]*p.surv[i]
			# Calculate Biomass
					B.out[i+1,]	<-	(Weight * N.out[i+1,])/1e6
			# Mature individuals 
					Mat.out[i+1,] = B.out[i+1,] * Maturity
			if(sum(Mat.out[i+1,]) > Harvest.Limit){	
					adult.harvest[i+1]  <- Adult.Harvest
					egg.harvest[i+1]	<- Egg.Harvest
					catch.eggs[i+1]		<- 	sum(Fecund(fecund.a,fecund.b,Mat.out[i+1,]*1e6)/1e9) * egg.harvest[i+1]
				# Determine Harvest Rate
					tot.harvest 	<-	 adult.harvest[i+1] * sum(Mat.out[i+1,])		
				# Fishery Happens on Mature Fish
					effect.harvest.rate		<- tot.harvest/sum(Mat.out[i+1,] * Select)
					catch.out[i+1,]			<- effect.harvest.rate * Mat.out[i+1,] * Select
				# Calibrate Mature individuals
					Mat.out[i+1,] <- Mat.out[i+1,] - catch.out[i+1,]
				# Calibrate total biomass
					B.out[i+1,]		<- B.out[i+1,] - catch.out[i+1,]
				# Calibrate N.individuals
					N.out[i+1,]		<-	B.out[i+1,] * 1e6 / Weight
			
					if(Adult.Harvest == 0){ adult.harvest[i+1] = -9999}
					if(Egg.Harvest == 0){ egg.harvest[i+1] = -9999}			
			}
		}
		
		OUT	<- list("B.out"=B.out,"N.out"=N.out,"Mat.out"=Mat.out,"Catch.out"=catch.out,
						"Catch.egg"=catch.eggs,
						"Adult.harvest.rate"=adult.harvest,"Egg.harvest.rate" = egg.harvest
						)
		return(OUT)
}	
	
	
	bev.holt	<- 	function(Alpha,Beta,B){
						Rec <- B * (Alpha + Beta*B)^(-1)
						return(Rec)
				}

	Fecund	<-	function(fecund.a,fecund.b,Weight){
					fecund.a + fecund.b * Weight
				}
	
	ricker		<-	function(Alpha,Beta,B){
						rec <- Alpha*B * exp(-Beta*B)
						return(rec)
					}
	