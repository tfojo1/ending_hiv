
persistence = 0.8
lambda = -log(persistence) / 1




time = seq(0,1,length=1000)
coverage.at.one.year = exp(-lambda); coverage.at.one.year

coverage.at.time = exp(-lambda*time)
qplot(time, coverage.at.time, geom='line') + ylim(0,1)

mean(coverage.at.time)


uptake = c(.1,.25,.50)
coverage = uptake * (1/lambda - exp(-lambda)/lambda)
round(100*cbind(uptake, coverage))



uptake.to.coverage <- function(uptake, persistence)
{
    lambda = -log(persistence)
    uptake * (1/lambda - exp(-lambda)/lambda)
}


coverages = uptake.to.coverage(c(oral=12.5, lai=12.5), c(.6,.8))
#rr = 1-efficacy
rrs = c(oral=.14, lai=.14*.34)
efficacies = 1-rrs

weights = coverages/sum(coverages)
combined.coverage = sum(coverages)
combined.rr = sum(rrs*weights)




############ Make intervention with persistence 




make.intervention <-function(Oral_Uptake,LAI_Uptake,Oral_Persistence,LAI_Persistence,Oral_Efficacy,LAI_Efficacy, combined = T,start.year,end.year, INTERVENTION.MANAGER = INTERVENTION.MANAGER.1.0){
  
  uptake.to.coverage <- function(uptake, persistence)
  {
    lambda = -log(persistence) #over one year?
    uptake * (1/lambda - exp(-lambda)/lambda)
  }
  
  if(combined){
   
    uptake = c(Oral_Uptake,LAI_Uptake)
    persistence = c(Oral_Persistence,LAI_Persistence)
    
    coverage = uptake.to.coverage(uptake, persistence)
    rrs = c(Oral_Efficacy, LAI_Efficacy)
    weights = coverage/sum(coverage)
    combined.coverage = sum(coverage)
    combined.rr = sum(rrs*weights)
    
    
    PREP = create.intervention.unit(type = "prep", start.year = start.year, 
                                    rates = combined.coverage, years = implemented.year)
    
    INJECTABLE.PREP = create.intervention.unit(type = "rr.prep", start.year = start.year, 
                                               rates = combined.rr/Oral_Efficacy, years = implemented.year, 
                                               apply.function = "multiplier", allow.less.than.otherwise = T)
    MSM.IO = create.intervention(ALL.MSM, PREP, INJECTABLE.PREP)
    INTERVENTION.MANAGER = register.intervention(MSM.IO, code=paste0('msm.oral.inj',Oral_Uptake,suffix),
                                                 name=paste0(Oral_Uptake*100,'% Uptake of oral and injectable PrEP on MSM'),
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    
  }
  else{
    
    uptake = c(Oral_Uptake,LAI_Uptake)
    persistence = c(Oral_Persistence,LAI_Persistence)

    coverage = uptake.to.coverage(uptake, persistence)
    
    PREP_O = create.intervention.unit(type = "prep", start.year = start.year, 
                                    rates = coverage[1], years = implemented.year)
    
    PREP_I = create.intervention.unit(type = "prep", start.year = start.year, 
                                      rates = coverage[2], years = implemented.year)
    
    INJECTABLE.PREP = create.intervention.unit(type = "rr.prep", start.year = start.year, 
                                                        rates = LAI_Efficacy, years = implemented.year, 
                                                        apply.function = "multiplier", allow.less.than.otherwise = T)
    
  
    MSM.I = create.intervention(ALL.MSM, PREP.I, INJECTABLE.PREP)
    INTERVENTION.MANAGER = register.intervention(MSM.I, code=paste0('msm.inj.',LAI_Uptake, suffix),
                                                 name=paste0(LAI_Uptake*100,'% Uptake of long-acting PrEP on MSM'),
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
    MSM.O = create.intervention(ALL.MSM, PREP_O)
    INTERVENTION.MANAGER = register.intervention(MSM.O, code=paste0('msm.oral.',Oral_Uptake,suffix),
                                                 name=paste0(Oral_Uptake*100,'% Uptake of oral PrEP on MSM'),
                                                 manager = INTERVENTION.MANAGER,
                                                 allow.intervention.multiple.names = T)
  }
    
  
  
  
 
  
  
}
