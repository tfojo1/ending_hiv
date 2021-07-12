

source('code/targets/target_msas.R')

x = sapply(TARGET.MSAS[1:3], function(msa){
    load(file.path('mcmc_runs/start_values/', paste0(msa, '.Rdata')))
    starting.parameters
})
