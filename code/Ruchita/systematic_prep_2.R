
##-- SOURCE CODE --##
source('code/source_code.R')
source('code/Ruchita/create_prep_interventions.R')

##-- ORGANIZE the INTERVENTIONS WE CARE ABOUT --##

#can add the variable PrEP if needed
ORAL.PREP.INTERVENTION.CODES = c(
    'msm.p10.oral_23_27',
    'msm.p25.oral_23_27',
    'msm.p50.oral_23_27'
)

INJ.PREP.INTERVENTION.CODES = c(
    'msm.p10.inj_23_27',
    'msm.p25.inj_23_27',
    'msm.p50.inj_23_27'
   
)

VAR.PREP.INTERVENTION.CODES = c(
  'msm.p10.inj.variable_23_27',
  'msm.p25.inj.variable_23_27',
  'msm.p50.inj.variable_23_27'
)

ALL.PREP.INTERVENTION.CODES = c(
    'noint',
    ORAL.PREP.INTERVENTION.CODES,
    INJ.PREP.INTERVENTION.CODES
)

VAR.ORAL.PREP.INTERVENTIONS = c(
  ORAL.PREP.INTERVENTION.CODES,
  VAR.PREP.INTERVENTION.CODES
)

STAGGERED.ORAL.INJ.PREP.CODES = character(2*length(ORAL.PREP.INTERVENTION.CODES))
STAGGERED.ORAL.INJ.PREP.CODES[2*(1:length(ORAL.PREP.INTERVENTION.CODES))-1] = ORAL.PREP.INTERVENTION.CODES
STAGGERED.ORAL.INJ.PREP.CODES[2*(1:length(INJ.PREP.INTERVENTION.CODES))] = INJ.PREP.INTERVENTION.CODES

##-- FUNCTION TO RUN INTERVENTIONS --##

run.prep.simulations <- function(msas=TARGET.MSAS, 
                                 intervention.codes=VAR.PREP.INTERVENTION.CODES,
                                 dst.dir = 'mcmc_runs/prep_simsets',
                                 src.dir = 'mcmc_runs/quick_simsets',
                                 run.to.year=2030,
                                 keep.years=2018:2030)
{
    for (msa in msas)
    {
        print(paste0("Running ", length(intervention.codes), " interventions for ", msa.names(msa)))
        full.filename = get.full.filename(location=msa)
        load(file.path(src.dir, full.filename))
        simset = flatten.simset(simset)
        
        run.systematic.interventions(simset = simset,
                                     interventions = lapply(intervention.codes, intervention.from.code), 
                                     dst.dir = dst.dir, overwrite = F, compress = T, 
                                     run.to.year = run.to.year, verbose = T, 
                                     save.baseline.and.seed = F
                                     )
    }
}

##-- FUNCTIONS TO MAKE A TABLE FROM SIMULATIONS --##


# Returns a data frame
# One row for each msa (plus a row for total if include.total=T)
# One column for each pair of intervention.code[i] x comparison.code[i]
make.prep.table <- function(msas=TARGET.MSAS,
                            intervention.codes,
                            comparison.codes, #could be null
                            raw.prep.results,
                            include.totals,
                            round.digits=0,
                            stat){
    if (include.totals){
      msas = c(msas, 'total')
    }
        
    
    # if there is only one comparison intervention, use that for all intervention.codes
    if (length(comparison.codes)==1){
      comparison.codes = rep(comparison.codes, length(intervention.codes))
    }
       
    
    rv = sapply(1:length(intervention.codes), function(i){
        sapply(1:length(msas), function(msa){
            int.code = intervention.codes[i]
            int.values = raw.prep.results[,msa,int.code]
            
            if (is.null(comparison.codes)){
              comp.values = rep(0, length(int.values))
            }
            else
            {            
                comp.code = comparison.codes[i]
                comp.values = raw.prep.results[,msa,comp.code]
            }
            
            if (stat=='abs.diff') {
              diff = colSums(int.values[[1]]) - colSums(comp.values[[1]]) #collapse list?
              
              mean_diff = round(mean(diff),0)
              CI_low = round(quantile(diff, probs=.025),0)
              CI_high = round(quantile(diff, probs=.975),0)
              paste0(format(mean_diff,big.mark=',')," [",CI_low," to ",CI_high,"]")
            }
            else if (stat == 'rel.diff')
            {
              diff = ((colSums(int.values[[1]])-colSums(comp.values[[1]])) / colSums(comp.values[[1]]))*100
            
              mean_diff = round(mean(diff),3)
              CI_low = round(quantile(diff, probs=.025),3)
              CI_high = round(quantile(diff, probs=.975),3)
              paste0(format(mean_diff,big.mark=','),"% [",CI_low,"% to ",CI_high,"%]")  
            }
            else 
            {
              diff = colSums(int.values[[1]])
              mean_diff = round(mean(diff),0)
              CI_low = round(quantile(diff, probs=.025),0)
              CI_high = round(quantile(diff, probs=.975),0)
              paste0(format(mean_diff,big.mark=',')," [",CI_low," to ",CI_high,"]")
            }
     
           
            
  
        })
    })
    
    rv = as.data.frame(rv)
    row.names(rv) = names(msas)
    colnames(rv) = paste(intervention.codes, comparison.codes) 
    rv
}

make.sensitivity.plot <- function(msas=TARGET.MSAS,
                                  intervention.codes = VAR.PREP.INTERVENTION.CODES,
                                  comparison.codes = ORAL.PREP.INTERVENTION.CODES, 
                                  raw.prep.results = prep.results,
                                  include.totals = F,
                                  dir = 'mcmc_runs/prep_simsets',
                                  round.digits=0,
                                  stat = 'rel.diff'){
  
  
  if (include.totals){
    msas = c(msas, 'total')
  }
  
  
  # if there is only one comparison intervention, use that for all intervention.codes
  if (length(comparison.codes)==1){
    comparison.codes = rep(comparison.codes, length(intervention.codes))
  }
  
  
  sapply(1:length(intervention.codes), function(i){
    sapply(1:length(msas), function(msa){
      int.code = intervention.codes[i]
      int.values = raw.prep.results[,msa,int.code]
  
      filename = get.simset.filename(location=msa, intervention.code=int.code)
      load(file.path(dir, msa, filename))
          
      simset = flatten.simset(simset)
      sims = simset@parameters
      sims = as.data.frame(sims)
      inj = sims$inj.rr
      
          
      
      if (is.null(comparison.codes)){
        comp.values = rep(0, length(int.values))
      }
      else
      {            
        comp.code = comparison.codes[i]
        comp.values = raw.prep.results[,msa,comp.code]
      }
      
      if (stat=='abs.diff') {
        diff = colSums(int.values[[1]]) - colSums(comp.values[[1]]) #collapse list?
      
      }
      else if (stat == 'rel.diff')
      {
        diff = ((colSums(int.values[[1]])-colSums(comp.values[[1]])) / colSums(comp.values[[1]]))*100
      
      }
      else 
      {
        diff = colSums(int.values[[1]])

      }
      
  sensitivity_plot = cbind(diff,inj)
  sensitivity_plot = as.data.frame(sensitivity_plot)
  
  plot = ggplot(sensitivity_plot,aes(x = 'inj',y='diff')) + geom_point() 
  plot + ylim((min(sensitivity_plot$diff)),(max(sensitivity_plot$diff))) + xlim((min(sensitivity_plot$inj)),(max(sensitivity_plot$inj)))
      
    })
  })
  
  

  
}



#returns a three-dimensional array
#indexed [simulation, msa, intervention.code]
aggregate.raw.prep.results <- function(msas=TARGET.MSAS,
                                       intervention.codes=VAR.ORAL.PREP.INTERVENTIONS,
                                       years=2020:2030,
                                       dir='mcmc_runs/prep_simsets',
                                       calculate.total=T)
{
    rv = sapply(intervention.codes, function(code){
        sapply(msas, function(msa){
            filename = get.simset.filename(location=msa, intervention.code=code)
            load(file.path(dir, msa, filename))

            simset = flatten.simset(simset)

            sapply(simset@simulations, project.absolute.incidence, keep.dimensions = NULL, years=years)
          
            
    
        })
    })
    
    
    dim.names = list(sim=1:(length(rv)/length(msas)/length(intervention.codes)), #produces 1? 
                     location=msas,
                     intervention=intervention.codes) 
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    if (calculate.total)
    {
        dim.names$location = c(dim.names$location, 'total')
        new.rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
        new.rv[,msas,] = rv 
        new.rv[,'total',] = apply(rv, c('intervention'), sum, na.rm=T) 
        rv = new.rv
    }
    
    rv
}
