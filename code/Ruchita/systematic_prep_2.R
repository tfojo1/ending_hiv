


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
  "baseline.oral.variable.efficacy","msm.oral.10.uptake_23_27","msm.inj.10.uptake_23_27","msm.combined.10.uptake_23_27","msm.oral.25.uptake_23_27","msm.inj.25.uptake_23_27","msm.combined.25.uptake_23_27", "msm.oral.35.uptake_23_27","msm.inj.35.uptake_23_27","msm.combined.35.uptake_23_27"      

)

UPTAKE.35.INTERVENTIONS.CODES = c("msm.oral.35.uptake_23_27","msm.inj.35.uptake_23_27","msm.combined.35.uptake_23_27")

UPTAKE.INTERVENTIONS.ORAL.CODES = c(
  'msm.oral.10.uptake_23_27',
  'msm.oral.25.uptake_23_27'
)

UPTAKE.INTERVENTIONS.INJ.CODES = c(
  'msm.inj.10.uptake_23_27',
  'msm.inj.25.uptake_23_27'
)


#New Uptake Interventions 

NEW.UPTAKE.INTERVENTIONS = c("msm.baseline.oral_23_27","msm.baseline.lai_23_27","msm.baseline.oral.plus.10.oral_23_27","msm.baseline.oral.plus.10.lai_23_27","msm.baseline.lai.plus.10.lai_23_27","msm.baseline.oral.plus.25.oral_23_27","msm.baseline.oral.plus.25.lai_23_27","msm.baseline.lai.plus.25.lai_23_27")  



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
                                     dst.dir = dst.dir, overwrite = F, compress = T, 
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
              diff = ((int.values[1,]-int.values[dim(int.values)[1],])/int.values[1,])*100
              mean_diff = round(mean(diff),0)
              CI_low = round(quantile(diff, probs=.025),0)
              CI_high = round(quantile(diff, probs=.975),0)
              #paste0(format(mean_diff,big.mark=','),"% [",CI_low,"% to ",CI_high,"%]")
              paste0(format(mean_diff,big.mark=','),"%")
              #mean_diff
              
            }
            else if (stat == "percent.diff"){
              int_diff = ((int.values[[1]][dim(int.values[[1]])[1],]-int.values[[1]][1,])/int.values[[1]][1,])*100
              comp_diff = ((comp.values[[1]][dim(comp.values[[1]])[1],]-comp.values[[1]][1,])/comp.values[[1]][1,])*100
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
    colnames(rv) = intervention.codes #change for comparison as needed
    rv
}

city.correlations <- function(msas=TARGET.MSAS,
                                  intervention.codes = "msm.combined.25.uptake_23_27",
                                  raw.prep.results = prep.results,
                                  parameters = parameter,
                                  include.totals = F,
                                  dir = 'mcmc_runs/prep_simsets',
                                  round.digits=0){
  
  int.code = intervention.codes 
  correlations = matrix(nrow = length(msas),ncol = 4)
  colnames(correlations) = colnames(parameters)
  rownames(correlations) = names(msas)
  
  for(a in 1: dim(parameters)[2]){
    p = as.numeric(parameters[,a])
   
    for(b in 1: length(msas)){
      
      int.values = raw.prep.results[,,b,int.code]
      inj.values = (int.values[11,]-int.values[1,])/int.values[1,]
      cor_matrix = cbind(p,inj.values)
      correlations[b,a] = cor(cor_matrix, method = "spearman")[2,1]
    }
    
    
  }

  }

make.sensitivity.plot <- function(msas=TARGET.MSAS,
                              intervention.codes = "msm.baseline.lai.plus.25.lai_23_27",
                              comparison.codes = "msm.baseline.oral.plus.25.oral_23_27",
                              raw.prep.results = prep.results,
                              parameter = parameters,
                              include.totals = F,
                              dir = 'mcmc_runs/prep_simsets',
                              cities = "comparison",
                              round.digits=0){
  
  int.code = intervention.codes 
  comp.code = comparison.codes
  parameter = parameter*100
  
  if(cities = "separated") {
    for(a in 1: dim(parameter)[2]){
      p = as.numeric(as.character(parameter[,a]))
      
      inj.values = vector("list", length = length(msas))
      for( b in 1:length(msas)){
        int.values = raw.prep.results[,,b,int.code]
        inj.values[[b]] = ((int.values[11,]-int.values[1,])/int.values[1,])*100   
      }
      
      inj.values = lapply(inj.values, function(x) cbind(p,x))
      inj.values = do.call(rbind.data.frame, inj.values)
      
      colnames(inj.values) = c("Parameter","Incidence Reduction")
      
      inj.values = as.data.frame(inj.values)
      inj.values = inj.values[order(inj.values$Parameter),]
      groups = split(inj.values, cut(inj.values$Parameter, 10))
      groups = Filter(NROW, groups)
      
      ID = sapply(groups, function(x) paste0(round(min(x$Parameter, na.rm=TRUE),2)," to ",round(max(x$Parameter, na.rm=TRUE),2)))
      groups <- mapply(cbind, groups, "Parameter Group"=ID, SIMPLIFY=F)
      groups = do.call(rbind.data.frame, groups)
      
      ggplot(groups, aes(x =`Parameter Group`, y =`Incidence Reduction`)) +          
        geom_boxplot() +  theme(axis.text.x = element_text(size = 20, angle = 45,vjust = 0.5, hjust=.5)) + ggtitle(paste0("Parameter: ",colnames(parameter)[a]))
      
      #correlation = round(cor(inj.values, method = "spearman")[2,1],3)
      #ggplot(inj.values, aes(x=`Parameter`, y=`Incidence Reduction`)) +
      # geom_point() + ggtitle(paste0("Parameter: ",colnames(parameters)[a], " R = ", correlation))
    }
    
  }
  
  else if (cities == "combined") {
    for(a in 1: dim(parameter)[2]){
    
    p = as.numeric(as.character(parameter[,a]))
  
 
  
    
    inj.values = vector("list", length = length(msas))
    
    int_2020 = sapply(msas, function(msa){
      int.values = raw.prep.results[,,msa,int.code]
      int.values[1,]
    })
    int_2020 = rowSums(int_2020)
    
    int_2030 = sapply(msas, function(msa){
      int.values = raw.prep.results[,,msa,int.code]
      int.values[11,]
    })
    int_2030 = rowSums(int_2030)
    
    inj.values = ((int_2020-int_2030)/int_2020)*100
    inj.values = cbind(inj.values,p)
    colnames(inj.values) = c("Incidence Reduction","Parameter")
    
    inj.values = as.data.frame(inj.values)
    inj.values = inj.values[order(inj.values$Parameter),]
    cor(inj.values, method = "spearman")[2,1]
  
    groups = vector("list", length = 5)
    for (b in 1:5){
      groups[[b]] = inj.values[((b-1)*(dim(inj.values)[1]/5)+1):(b*(dim(inj.values)[1]/5)),]
    }
    
    ID = sapply(groups, function(x) paste0(round(min(x$Parameter, na.rm=TRUE),2),"% to ",round(max(x$Parameter, na.rm=TRUE),2),"%"))
    groups <- mapply(cbind, groups, "Parameter Group"=ID, SIMPLIFY=F)
    groups = do.call(rbind.data.frame, groups)
    
    plot = ggplot(groups, aes(x =`Parameter Group`, y =`Incidence Reduction`)) + ggtitle(paste0("Parameter: ",colnames(parameter)[a])) + 
      geom_boxplot() 
      
    plot + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title=element_text(size=20), axis.text.x = element_text(size = 20, angle = 45, hjust=1), axis.text.y = element_text(size = 20)) 
    
    
    }
  }
  
  else if(cities == "comparison") {
    
  
    
    for(a in 1: dim(parameter)[2]){
      
      int.code = intervention.codes
      comp.code = comparison.codes
      p = 100 - as.numeric(as.character(parameter[,3]))
      
      #inectable values 
      inj.values = vector("list", length = length(msas))

      int_2020 = sapply(msas, function(msa){
        int.values = raw.prep.results[,,msa,int.code]
        int.values[1,]
      })
      int_2020 = rowSums(int_2020)
      
      int_2030 = sapply(msas, function(msa){
        int.values = raw.prep.results[,,msa,int.code]
        int.values[11,]
      })
      int_2030 = rowSums(int_2030)
      
      inj.values = ((int_2020-int_2030)/int_2020)*100
      inj.values = cbind(inj.values,p)
      colnames(inj.values) = c("Incidence Reduction","Parameter")
      
      #oral values 
      comp.values = vector("list", length = length(msas))
      
      comp_2020 = sapply(msas, function(msa){
        comp.values = raw.prep.results[,,msa,comp.code]
        comp.values[1,]
      })
      comp_2020 = rowSums(comp_2020)
      
      comp_2030 = sapply(msas, function(msa){
        comp.values = raw.prep.results[,,msa,comp.code]
        comp.values[11,]
      })
      comp_2030 = rowSums(comp_2030)
      
      comp.values = ((comp_2020-comp_2030)/comp_2020)*100
      comp.values = cbind(comp.values,p)
      colnames(comp.values) = c("Incidence Reduction","Parameter")
      
      values = inj.values[,1] - comp.values[,1]
      values = cbind(values,p)
      colnames(values) = c("Absolute Difference", "Parameter")
      
      
      
      values = as.data.frame(values)
      values = values[order(values$Parameter),]
      correlation = cor(values, method = "spearman")[2,1]
      
      groups = vector("list", length = 5)
      for (b in 1:5){
        groups[[b]] = values[((b-1)*(dim(values)[1]/5)+1):(b*(dim(values)[1]/5)),]
      }
     
      groups = Filter(NROW, groups)
      
      mean(groups[[1]]$`Absolute Difference`)
      quantile(groups[[1]]$`Absolute Difference`, probs = .025)
      quantile(groups[[1]]$`Absolute Difference`, probs = .975)
      
      ID = sapply(groups, function(x) paste0(round(min(x$Parameter, na.rm=TRUE),0),"% to ",round(max(x$Parameter, na.rm=TRUE),0),"%"))
      groups <- mapply(cbind, groups, "Parameter Group"=ID, SIMPLIFY=F)
      groups = do.call(rbind.data.frame, groups)
      
      plot = ggplot(groups, aes(x =`Parameter Group`, y =`Absolute Difference`)) + ggtitle(paste0("Parameter: ",colnames(parameter)[a]," ", correlation)) + 
        geom_boxplot() + ylab("Difference in Incidence Reduction")
      
      plot + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.title=element_text(size=20), axis.text.x = element_text(size = 20, angle = 45, hjust=1), axis.text.y = element_text(size = 20)) 
      
      
    }
    
    
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
                                       calculate.total=F,
                                       verbose=T)
{
    rv = sapply(intervention.codes, function(code){
        sapply(msas, function(msa){
            filename = get.simset.filename(location=msa, intervention.code=code)
            if (verbose)
                print(paste0("Loading ", filename))
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

aggregate.prep.coverage <- function(msas,
                                       intervention.codes = baseline.oral.variable.efficacy,
                                       years=2020:2030,
                                       dir='mcmc_runs/prep_simsets',
                                    calculate.total=F,
                                       verbose=T)
{
    rv = sapply(intervention.codes, function(code){
        sapply(msas, function(msa){
            filename = get.simset.filename(location=msa, intervention.code=code)
            if (verbose)
                print(paste0("Loading ", filename))
            load(file.path(dir, msa, filename))
            
            simset = flatten.simset(simset)
            sapply(simset@simulations, extract.prep.coverage, 
                   keep.dimensions = 'year', years=years, sexes='msm',
                   per.population=1)
            
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
        new.rv[,,'total',] = apply(rv, c(1,2,4), mean, na.rm=T) 
        rv = new.rv
    }
    
    rv
}




make.figure <- function(msas=TARGET.MSAS,intervention.codes = NEW.UPTAKE.INTERVENTIONS, raw.prep.results = prep.results,round.digits=0){
 

  plots = vector("list", length= length(intervention.codes))
  mean_reduction =  vector("list", length= length(intervention.codes))
  reduction = vector("list", length= length(intervention.codes))
  for(i in 1:length(intervention.codes)){
    rv = sapply(1:11, function(j){
      int.code = intervention.codes[i]
      int.values = raw.prep.results[j,,,int.code]
      #int_collapse = do.call(cbind, int.values
    })
    
    int.code = intervention.codes[i]
    int.values = raw.prep.results[1,,,int.code]
    #int_collapse = do.call(cbind, int.values)
    rv_1 = rowSums(int.values)
    
    int.code = intervention.codes[i]
    int.values = raw.prep.results[11,,,int.code]
    #int_collapse = do.call(cbind, int.values
    rv_11 = rowSums(int.values)
  
    
    red = (rv[,1]-rv[,11])/(rv[,1])*100
    red = (rv_1-rv_11)/(rv_1)*100
  
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

#Baseline PrEP

baseline_2023 = baseline.prep[4,,,1]
baseline_2023 = as.data.frame(baseline_2023)
baseline_2023 = baseline_2023/parameters[,2]
baseline_2023 = colMeans(baseline_2023)

baseline_2020 = baseline.prep[1,,,1]
baseline_2020 = as.data.frame(baseline_2020)
baseline_2020 = baseline_2020/parameters[,2]
baseline_2020 = colMeans(baseline_2020)

diff = baseline_2023-baseline_2020

baseline = rowMeans(baseline)
mean(baseline)
