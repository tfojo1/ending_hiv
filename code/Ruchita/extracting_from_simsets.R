
load('mcmc_runs/quick_simsets/1.0_12580_full.Rdata')




sapply(simset@simulations, function(sim){
    project.absolute.incidence(sim, years=2010:2013, sex='msm')
})

sapply(simset@simulations, function(sim){
    project.absolute.new.diagnoses(sim, years=2010:2013, sex='msm')
})

truth = get.surveillance.data(msa.surveillance, location=DC.MSA, years=2010:2020, data.type='new', risk=T)



load('mcmc_runs/prep_simsets/12580/1.0_12580_noint.Rdata')
sapply(simset@simulations, function(sim){
    project.absolute.incidence(sim, years=2014:2030, sex='msm')
})

sapply(simset@simulations, function(sim){
    project.absolute.new.diagnoses(sim, years=2014:2030, sex='msm')
})