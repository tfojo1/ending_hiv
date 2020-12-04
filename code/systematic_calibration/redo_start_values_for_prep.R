

extend.start.values.for.prep <- function(mcmc)
{
    rv = mcmc@samples[1,mcmc@n.iter,]
    rv = c(rv,
           msm.prep.intercept.or = 1,
           non.msm.prep.intercept.or = 1,
           
           msm.prep.slope.or = 1,
           idu.prep.slope.or = 1,
           heterosexual.prep.slope.or = 1,
           
           black.prep.or = 1,
           hispanic.prep.or = 1,
           
           age1.prep.or = 1,
           age2.prep.or = 1,
           age4.prep.or = 1,
           age5.prep.or = 1
           )
    
    rv
}

dir = 'mcmc_runs/systematic_initial_pre_prep/'
files = list.files(dir)
locations = substr(files, 1,5)

dst.dir = 'mcmc_runs/start_values/'
for (i in 1:length(locations))
{
    print(paste0("Redoing ", msa.names(locations[i])))
    load(file.path(dir, files[i]))
    starting.parameters = extend.start.values.for.prep(mcmc)
    save(starting.parameters, file=file.path(dst.dir, paste0(locations[i], '.Rdata')))
}