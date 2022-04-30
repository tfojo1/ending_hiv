
source('code/source_code.R')
source('code/interventions/systematic_interventions.R')
source('code/targets/target_msas.R')

msas = TARGET.MSAS[21 + 1:7]
INTERVENTIONS.TO.DO = ALL.INTERVENTIONS.3Y

for (msa in msas)
{
    file = file.path('mcmc_runs/systematic_initial', paste0(msa, '.Rdata'))
    if (file.exists(file))
    {
        print("---------------------------------------------------------------------------")
        print(paste0("RUNNING FOR MSA ", toupper(msa.names(msa))))
        print("---------------------------------------------------------------------------")
     
        load(file)   
        
        simset = extract.simset(mcmc, additional.burn=520, additional.thin=6)
        simset@n.sim
        msa.names(attr(simset@simulations[[1]], 'location'))
        run.systematic.interventions(simset, 
                                     interventions = INTERVENTIONS.TO.DO,
                                     dst.dir=file.path('mcmc_runs/visualization_simsets', attr(simset@simulations[[1]], 'location')),
                                     overwrite = T)
        
        print(paste0("DONE WITH ", toupper(msa.names(msa))))
    }
    else
        print(paste0("No initial simset for ", msa.names(msa)))
    
}
