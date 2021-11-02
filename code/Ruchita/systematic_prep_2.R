
##-- SOURCE CODE --##
source('code/source_code.R')
source('code/Ruchita/create_prep_interventions2.R')


##-- ORGANIZE the INTERVENTIONS WE CARE ABOUT --##

#can add the variable PrEP if needed
ORAL.PREP.INTERVENTION.CODES = c(
    'msm.p10.oral_23_27',
    'msm.p25.oral_23_27',
    'msm.p50.oral_23_27'
)

ORAL.VAR.PREP.INTERVENTION.CODES = c(
  'msm.p10.oral.variable_23_27',
  'msm.p25.oral.variable_23_27',
  'msm.p50.oral.variable_23_27'
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

INJ.VAR.PREP.INTERVENTION.CODES = c(
  'msm.p10.inj.variable_23_27',
  'msm.p25.inj.variable_23_27',
  'msm.p50.inj.variable_23_27',
  'msm.p10.inj_23_27',
  'msm.p25.inj_23_27',
  'msm.p50.inj_23_27'
)

ALL.PREP.INTERVENTION.CODES = c(
    'noint',
    ORAL.PREP.INTERVENTION.CODES,
    INJ.PREP.INTERVENTION.CODES
)

VAR.ORAL.PREP.INTERVENTIONS.CODES = c(
  VAR.PREP.INTERVENTION.CODES,
  ORAL.PREP.INTERVENTION.CODES
)

ALL.VAR.ORAL.PREP.INTERVENTIONS.CODES = c(
  'noint',
  'msm.p10.oral_23_27',
  'msm.p25.oral_23_27',
  'msm.p50.oral_23_27',
  'msm.p10.inj.variable_23_27',
  'msm.p25.inj.variable_23_27',
  'msm.p50.inj.variable_23_27'
)

ORAL.INJ.COMB.INTERVENTIONS.CODES = c(
  'msm.p10.oralinj.variable_23_27',
  'msm.p25.oralinj.variable_23_27',
  'msm.p50.oralinj.variable_23_27'
)

ALL.ORAL.INJ.COMB.INTERVENTIONS.CODES = c(
  'noint',
  'msm.p10.oral.variable_23_27',
  'msm.p25.oral.variable_23_27',
  'msm.p50.oral.variable_23_27',
  'msm.p10.oralinj.variable_23_27',
  'msm.p25.oralinj.variable_23_27',
  'msm.p50.oralinj.variable_23_27'
)

INJ.ORAL.2020.INTERVENTIONS.CODES = c(
  'msm.p10.oral.variable_20_20',
  'msm.p25.oral.variable_20_20',
  'msm.p50.oral.variable_20_20',
  'msm.p10.oralinj.variable_20_20',
  'msm.p25.oralinj.variable_20_20',
  'msm.p50.oralinj.variable_20_20'
)

ORAL.2020.INTERVENTIONS.CODES = c(
  'msm.p10.oral.variable_20_20',
  'msm.p25.oral.variable_20_20',
  'msm.p50.oral.variable_20_20'
)

INJ.2020.INTERVENTIONS.CODES = c(
  'msm.p10.inj.variable_20_20',
  'msm.p25.inj.variable_20_20',
  'msm.p50.inj.variable_20_20'
)

COVERAGE.25.INTERVENTIONS.CODES = c(
  'msm.p25.oralinj.variable_23_27',
  'msm.p25.oral.variable_23_27',
  'noint'
)

COVERAGE.10.INTERVENTIONS.CODES = c(
  'msm.p10.oralinj.variable_23_27',
  'msm.p10.oral.variable_23_27',
  'noint'
)


COVERAGE.50.INTERVENTIONS.CODES = c(
  'msm.p50.oralinj.variable_23_27',
  'msm.p50.oral.variable_23_27',
  'noint'
)

COVERAGE.10.NEW.INTERVENTIONS.CODES = c(
  'msm.p10.oralinj.variable.new_23_27',
  'msm.p10.oral.variable.new_23_27',
  'noint'
)

COVERAGE.25.NEW.INTERVENTIONS.CODES = c(
  'msm.p25.oralinj.variable.new_23_27',
  'msm.p25.oral.variable.new_23_27',
  'noint'
)

COVERAGE.50.NEW.INTERVENTIONS.CODES = c(
  'msm.p50.oralinj.variable.new_23_27',
  'msm.p50.oral.variable.new_23_27',
  'noint'
)


ORAL.2015.INTERVENTIONS.CODES = 'msm.p35.oral.variable_15_15'

INJ.2015.INTERVENTIONS.CODES = 'msm.p35.oralinj.variable_15_15'


INJ.ORAL.2015.INTERVENTIONS.CODES = c(
  INJ.2015.INTERVENTIONS.CODES,
  ORAL.2015.INTERVENTIONS.CODES
)

INJ.ORAL.VARIABLE.NEW.INTERVENTIONS.CODES = c(
  'noint',
  'msm.p10.oralinj.variable.new_23_27',
  'msm.p25.oralinj.variable.new_23_27',
  'msm.p50.oralinj.variable.new_23_27',
  'msm.p10.oral.variable.new_23_27',
  'msm.p25.oral.variable.new_23_27',
  'msm.p50.oral.variable.new_23_27'
  
)


ORAL.VARIABLE.NEW.INTERVENTIONS.CODES = c(
  'msm.p10.oral.variable.new_23_27',
  'msm.p25.oral.variable.new_23_27',
  'msm.p50.oral.variable.new_23_27'
  
)

INJ.VARIABLE.NEW.INTERVENTIONS.CODES = c(
  'msm.p10.oralinj.variable.new_23_27',
  'msm.p25.oralinj.variable.new_23_27',
  'msm.p50.oralinj.variable.new_23_27'
  
)

UPTAKE.INTERVENTIONS.CODES = c(
  "baseline.oral.variable.efficacy","msm.oral.10.uptake_23_27","msm.inj.10.uptake_23_27","msm.combined.10.uptake_23_27","msm.oral.25.uptake_23_27","msm.inj.25.uptake_23_27","msm.combined.25.uptake_23_27"   

)

UPTAKE.INTERVENTIONS.ORAL.CODES = c(
  'msm.oral.10.uptake_23_27',
  'msm.oral.25.uptake_23_27'
)

UPTAKE.INTERVENTIONS.INJ.CODES = c(
  'msm.inj.10.uptake_23_27',
  'msm.inj.25.uptake_23_27'
)

STAGGERED.ORAL.INJ.PREP.CODES = character(2*length(ORAL.PREP.INTERVENTION.CODES))
STAGGERED.ORAL.INJ.PREP.CODES[2*(1:length(ORAL.PREP.INTERVENTION.CODES))-1] = ORAL.PREP.INTERVENTION.CODES
STAGGERED.ORAL.INJ.PREP.CODES[2*(1:length(INJ.PREP.INTERVENTION.CODES))] = INJ.PREP.INTERVENTION.CODES

##-- FUNCTION TO RUN INTERVENTIONS --##

run.prep.simulations <- function(msas, 
                                 intervention.codes,
                                 dst.dir = 'mcmc_runs/prep_simsets',
                                 src.dir = 'mcmc_runs/quick_simsets',
                                 run.from.year=2014,
                                 run.to.year=2030,
                                 keep.years=2014:2030)
{
    for (msa in msas)
    {
        print(paste0("Running ", length(intervention.codes), " interventions for ", msa.names(msa)))
        full.filename = get.full.filename(location=msa)
        load(file.path(src.dir, full.filename))
        simset = flatten.simset(simset)
        
        run.systematic.interventions(simset = simset,
                                     interventions = lapply(intervention.codes, intervention.from.code), 
                                     dst.dir = dst.dir, overwrite = T, compress = T, 
                                     run.from.year = run.from.year,
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
                            include.totals = F,
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
            int.values = raw.prep.results[,,msa,int.code]
            
            if (is.null(comparison.codes)){
              comp.values = rep(0, length(int.values))
            }
            else
            {            
                comp.code = comparison.codes[i]
                comp.values = raw.prep.results[,,msa,comp.code]
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
            else if (stat == 'no.comparison')
            {
              diff = colSums(int.values[[1]])
              mean_diff = round(mean(diff),0)
              CI_low = round(quantile(diff, probs=.025),0)
              CI_high = round(quantile(diff, probs=.975),0)
              paste0(format(mean_diff,big.mark=',')," [",CI_low," to ",CI_high,"]")
            }
            else if (stat == 'diff')
            {
              diff = ((int.values[dim(int.values)[1],]-int.values[1,])/int.values[1,])*100
              mean_diff = round(mean(diff),0)
              CI_low = round(quantile(diff, probs=.025),0)
              CI_high = round(quantile(diff, probs=.975),0)
              paste0(format(mean_diff,big.mark=','),"% [",CI_low,"% to ",CI_high,"%]")
              
            }
            else if (stat == "percent.diff"){
              int_diff = ((int.values[dim(int.values)[1],]-int.values[1,])/int.values[1,])*100
              comp_diff = ((comp.values[dim(comp.values)[1],]-comp.values[1,])/comp.values[1,])*100
              diff = int_diff - comp_diff
              mean_diff = round(mean(diff),0)
              CI_low = round(quantile(diff, probs=.025),0)
              CI_high = round(quantile(diff, probs=.975),0)
              paste0(format(mean_diff,big.mark=','),"% [",CI_low,"% to ",CI_high,"%]")
              
            }
     
        })
    })
    
    rv = as.data.frame(rv)
    row.names(rv) = names(msas)
    colnames(rv) = paste(intervention.codes, comparison.codes) 
    rv
}

make.sensitivity.plot <- function(msas=TARGET.MSAS,
                                  intervention.codes = ORAL.INJ.COMB.INTERVENTIONS.CODES,
                                  comparison.codes = ORAL.VAR.PREP.INTERVENTION.CODES, 
                                  raw.prep.results = prep.results,
                                  include.totals = F,
                                  dir = 'mcmc_runs/prep_simsets',
                                  round.digits=0){
  
  
  
  filename = get.simset.filename(location=msas[1], intervention.code=intervention.codes[1])
  load(file.path(dir, msas[1], filename))
  
  simset = flatten.simset(simset)
  sims = simset@parameters
  sims = as.data.frame(sims)
  inj = sims$inj.rr
  
  
  if (include.totals){
    msas = c(msas, 'total')
  }
  
  
  # if there is only one comparison intervention, use that for all intervention.codes
  if (length(comparison.codes)==1){
    comparison.codes = rep(comparison.codes, length(intervention.codes))
  }
 
  
  for(i in 1:length(intervention.codes)){
    
 
  int_2020 = sapply(1:length(msas), function(msa){
      int.code = intervention.codes[i]
      int.values = raw.prep.results[msa,int.code]
      int.values[[1]][1,]
    })
    
  
  int_2030 = sapply(1:length(msas), function(msa){
      int.code = intervention.codes[i]
      int.values = raw.prep.results[msa,int.code]
      int.values[[1]][dim(int.values[[1]])[1],]
    })
    
    
    
    comp_2020 = sapply(1:length(msas), function(msa){
        
        if (is.null(comparison.codes)){
          comp.values = rep(0, length(int.values))
        }
        else
        {            
          comp.code = comparison.codes[i]
          comp.values = raw.prep.results[msa,comp.code]
        }
        
        comp.values[[1]][1,]
      })
    

    comp_2030 = sapply(1:length(msas), function(msa){
      
      if (is.null(comparison.codes)){
        comp.values = rep(0, length(int.values))
      }
      else
      {            
        comp.code = comparison.codes[i]
        comp.values = raw.prep.results[msa,comp.code]
      }
      comp.values[[1]][dim(comp.values[[1]])[1],]
    })
    
    int_2020_collapse = do.call(rbind.data.frame, int_2020)
    int_2020_collapse = int_2020_collapse[,-dim(int_2020_collapse)[2]]
    int_2030_collapse = do.call(rbind.data.frame, int_2030)
    int_2030_collapse = int_2030_collapse[,-dim(int_2030_collapse)[2]]
    comp_2020_collapse = do.call(rbind.data.frame, comp_2020)
    comp_2020_collapse = comp_2020_collapse[,-dim(comp_2020_collapse)[2]]
    comp_2030_collapse = do.call(rbind.data.frame, comp_2030)
    comp_2030_collapse = comp_2030_collapse[,-dim(comp_2030_collapse)[2]]
    
    sensitivity_plot = ((colSums(int_2030_collapse)-colSums(int_2020_collapse))/(colSums(int_2020_collapse)))*100 - ((colSums(comp_2030_collapse)-colSums(comp_2020_collapse))/(colSums(comp_2020_collapse)))*100
    sensitivity_plot = cbind(sensitivity_plot,inj)
    rownames(sensitivity_plot) <- NULL
    colnames(sensitivity_plot) <- c("Absolute Difference","Efficacy")
    sensitivity_plot = as.data.frame(sensitivity_plot)
    dev.off()
    ggplot(sensitivity_plot, aes(x =`Efficacy`, y = `Absolute Difference`)) + geom_point() +ggtitle(paste0("Sensitivity Plot "))
    
    cor(sensitivity_plot, method = "spearman")
  
  }


  
}

#Fix Correaltions As Well 

correlations <- function(msas=TARGET.MSAS,
                         intervention.codes = VAR.PREP.INTERVENTION.CODES,
                         comparison.codes = ORAL.PREP.INTERVENTION.CODES, 
                         raw.prep.results = prep.results,
                         include.totals = F,
                         dir = 'mcmc_runs/prep_simsets',
                         round.digits=0){

  
  filename = get.simset.filename(location=msas[1], intervention.code=intervention.codes[1])
  load(file.path(dir, msas[1], filename))
  
  simset = flatten.simset(simset)
  sims = simset@parameters
  sims = as.data.frame(sims)
  inj = sims$inj.rr
  
  cor.table = vector("list", length = length(intervention.codes))
  
for(i in 1:length(intervention.codes)){
  rv_cor = sapply(1:length(msas), function(msa){
    
    
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
    
    
    ((colSums(int.values[[1]])-colSums(comp.values[[1]])) / colSums(comp.values[[1]]))*100
    
    
  })
  
  cor = lapply(rv_cor,function(x){cbind(x,inj)})
  correlations = lapply(cor, function(x){cor(x[,1],x[,2],method = "spearman")})
  correlations = do.call(rbind.data.frame, correlations)
  correlations = cbind(names(msas), correlations)
  colnames(correlations) = c("City","Correlation Coefficient")
  cor.table[[i]] = correlations
  
}
 

 names(cor.table) = intervention.codes
 return(cor.table)

  
}



#returns a four-dimensional array
#indexed [simulation, msa, intervention.code]
aggregate.raw.prep.results <- function(msas,
                                       intervention.codes = COVERAGE.50.INTERVENTIONS.CODES,
                                       years=2020:2030,
                                       dir='mcmc_runs/prep_simsets',
                                       calculate.total=F)
{
    rv = sapply(intervention.codes, function(code){
        sapply(msas, function(msa){
            filename = get.simset.filename(location=msa, intervention.code=code)
            load(file.path(dir, msa, filename))

            simset = flatten.simset(simset)
            sapply(simset@simulations, project.absolute.incidence, 
                   keep.dimensions = 'year', years=years, risks = c("msm","msm_idu"))
          
            
    
        })
    })
    
    dim.names = list(year=years,
                     sim=1:(length(rv)/length(msas)/length(intervention.codes)/length(years)),
                     location=msas,
                     intervention=intervention.codes) 
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    if (calculate.total)
    {
        dim.names$location = c(dim.names$location, 'total')
        new.rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
        new.rv[,,msas,] = rv 
        new.rv[,,'total',] = apply(rv, c(1,2,4), sum, na.rm=T) 
        rv = new.rv
    }
    
    rv
}

make.figure <- function(msas=TARGET.MSAS,intervention.codes = UPTAKE.INTERVENTIONS.CODES, raw.prep.results = prep.results,round.digits=0){
 
  plots = vector("list", length= length(intervention.codes))
  mean_reduction =  vector("list", length= length(intervention.codes))
  reduction = vector("list", length= length(intervention.codes))
  for(i in 1:length(intervention.codes)){
    rv = sapply(1:11, function(j){
      int.code = intervention.codes[6]
      int.values = raw.prep.results[j,,,int.code]
      rowSums(int.values)
    })
    
    red = (rv[,11]-rv[,1])/(rv[,1])*100
  
    mean_red = mean(red)
    mean_red_low = quantile(red, probs = .025)
    mean_red_high = quantile(red, probs = .975)
    mean_red
    mean_red_low
    mean_red_high
    
    
    mean_reduction[[i]] = paste0(round(mean_red,0),"% [",round(mean_red_low,0),"% to ",round(mean_red_high,0),"%]")
    reduction[[i]] = red
    
    mean_diff = round(colMeans(rv),0)
    CI_low = round(apply(rv,2,function(x) quantile(x, probs = .025)),0)
    CI_high = round(apply(rv,2,function(x) quantile(x, probs = .975)),0)
    
    rv = rbind(mean_diff,CI_low,CI_high)
    rv = as.data.frame(rv)
    rv = cbind(t(rv),2020:2030,intervention.codes[i])
    colnames(rv) = c("Incidence","CI Low", "CI High", "Year","Intervention")
    
    plots[[i]] = rv

    
  }
  
  
  mean_red_diff = round(mean(reduction[[1]]-reduction[[2]]),0)
  mean_red_low = round(quantile(reduction[[1]]-reduction[[2]], probs = .025),0)
  mean_red_high = round(quantile(reduction[[1]]-reduction[[2]], probs = .975),0)
  paste0(mean_red_diff,"% [",mean_red_low,"% to ",mean_red_high,"%]")
  
  
  plots = do.call(rbind.data.frame,plots)
  plots$Incidence = as.numeric(as.character((plots$Incidence)))
  plots$`CI Low` = as.numeric(as.character((plots$`CI Low`)))
  plots$`CI High` = as.numeric(as.character((plots$`CI High`)))                            
  p = ggplot(plots, aes(x=Year,y=Incidence,group=Intervention)) +
    geom_ribbon(aes(ymin=`CI Low`,ymax=`CI High`, fill= Intervention), alpha = .09) +
    geom_line(aes(colour=Intervention))+scale_y_continuous(labels = scales::comma)
  p + theme_bw()
}

baseline.coverage <- function(msas = TARGET.MSAS, parameter = parameters, baseline = baseline.prep){
  
  rv = sapply(1:length(msas), function(msa){
    coverage = baseline[,,msa,1]
    uptake = coverage/parameter[,2]
    
  })
  
  colMeans(rv)
  
}



