CHUNK = 1
N.CHUNKS = 4

source('code/source_code.R')
source('code/processing/pretty_table/make_systematic_table.R')
#source('code/targets/target_msas.R')
source('code/interventions/synthesize_interventions.R')
source('code/applications/expanded_continuum/melissa_croi_interventions_2022_v2.R')

# N.SIM = 100
dir = 'mcmc_runs/quick_simsets'

run.interventions.melissa = function(){
    files = list.files(dir)
    files = files[grepl("ex",files)]
    locations = substr(files,7,11)
    
    indices.to.run = (1:length(files))
    indices.to.run = indices.to.run %% N.CHUNKS # divides by n.chunks and takes remainder 
    if(CHUNK==N.CHUNKS)
        mask = indices.to.run==0
    else
        mask = indices.to.run==CHUNK
    
    files.to.run = files[mask]
    
    sapply(files.to.run, function(x){
        
        file = paste0("mcmc_runs/quick_simsets/",x)
        load(file)
        
        # simset = subset.simset(simset,1:n.sim)
        
        run.systematic.interventions(simset,
                                     dst.dir = dir,
                                     interventions = MELISSA.CROI.INTERVENTIONS.2022,
                                     save.baseline.and.seed = F, run.to.year = 2035, overwrite = T)
        
    })
}




run.interventions.melissa()






