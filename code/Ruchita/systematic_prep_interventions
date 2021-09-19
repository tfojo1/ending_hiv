

#Create Interventions


# Takes an msa code and a desired level of prep coverage
# loads the baseline simset for the msa from src.dir
# runs two interventions - one for oral prep and one for long-acting prep, at the given coverage
# saves the two simsets into dst.dir
run.systematic.prep <- function(msa, prep.coverage, src.dir='mcmc_runs/quick_simsets', dst.dir='mcmc_runs/prep_simsets')
{
    # load the baseline
    load(src.dir/msa) #double check syntax
    simset = prepare.simset.for.interventions(simset)
    
    num_sim = simset@n.sim #Number of simulations 
    
    # define the interventions
     prep = create.intervention.unit(type = "prep", start.year = 2023, rates = prep.coverage, years = 2027)
     prep_RR = create.intervention.unit(type = "rr.prep", start.year = 2023, rates = .34, years = 2027, apply.function = "multiplier")
     
     int_oral = create.intervention(ALL.MSM, prep) #change population as appropriate
     int_inj = create.intervention(ALL.MSM, prep, prep_RR) #change population as appropriate 
    
    # run the simulations
  
    oral.simset = run.simset.intervention(simset, int_oral, run.to.year = 2030, keep.years = 2018:2030) 
    inj.simset =  run.simset.intervention(simset, int_inj, run.to.year = 2030, keep.years = 2018:2030)
  
  
    # save
    simset = oral.simset
    save(simset, file=file.path(dst.dir, msa, paste0("prep_", 100*prep.coverage, "_oral.Rdata")
    
    simset = inj.simset
    save(simset, file=file.path(dst.dir, msa, paste0("prep_", 100*prep.coverage, "_inj.Rdata")
}

# takes a given MSA and level of prep coverage
# loads the corresponding simsets and extracts incidence for the period "years"
# returns a vector with three elements - mean, ci.lower, ci.upper
process.systematic.prep <- function(msa, prep.coverage, dir='mcmc_runs/prep_simsets', 
                                    years=2020:2030, ci.coverage=0.95)
{
    # load the pre-run simsets
    load(file.path(dir, msa, paste0("prep_", 100*prep.coverage, "_oral.Rdata")
    oral.simset = simset
    
    load(file.path(dir, msa, paste0("prep_", 100*prep.coverage, "_inj.Rdata")
    inj.simset = simset
    
    # Extract the values
    delta_prep = matrix(nrow = 1, ncol = num_sim)
  
    for(a in 1:num_sim){
    sum_oral = sum(project.absolute.incidence(oral.simset@simulations[[a]], years = years))
    sum_inj = sum(project.absolute.incidence(inj.simset@simulations[[a]], years = years))
    delta_prep[1,a] = sum_inj-sum_oral
  }
    
    # get the summary statistics
    alpha = (1-ci.coverage)/2
    return(c(mean=mean(delta_prep),
      ci.lower=quantile(delta_prep, probs=alpha),
      ci.upper=quantile(delta_prep, probs=1-alpha))) #correct to return? 
}

# loop over all the given msas and prep coverages, and run simulations for each
run.all.systematic.prep <- function(msas=TARGET.MSAS, prep.coverages=c(.1,.2,.3),
                                    src.dir='mcmc_runs/quick_simsets', dst.dir='mcmc_runs/prep_simsets')
{
    # Loop over all combinations of msas and prep.coverages and call run.systematic.prep on each
    for(a in 1:length(msas)){
      msa = msas[a] 
      for(b in 1:length(prep.coverages)){
        prep.coverage = prep.coverages[b]
        run.systematic.prep(msa, prep.coverage, src.dir, dst.dir)
      
      }
    }
    
}

# returns a data frame
# one row for each msa
# one column for each prep coverage
# the value is the mean [95% CI] delta in cases averted
make.prep.table <- function(msas=TARGET.MSAS, prep.coverages=c(.1,.2,.3), dir='mcmc_runs/prep_simsets')
{
    df = matrix(nrow = length(msas), ncol = length(prep.coverages))
    df = as.data.frame(df)
    
    dimnames(df)[1] = msa.names(msas) # this just gives them their names in words
    dimnames(df)[2] = paste0(100*prep.coverages, '%') # this names them by % prep coverage
    
    for(a in 1:length(msas)){ #avoid nested for loop?
      msa = msas[a]
      for(b in 1:length(prep.coverages)){
      prep.coverage = prep.coverage[b]
      df[a,b] = process.systematic.prep(msa, prep.coverage, dir='mcmc_runs/prep_simsets', years=2020:2030, ci.coverage=0.95)
      }
    
    }
    return(df)
}
