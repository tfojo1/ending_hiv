
setwd("Ending_HIV")
source('code/source_code.R')
source('code/targets/target_msas.R')

print("RE-RUNNING SIMSETS")
print(Sys.Date())
print(Sys.time())

to.do = TARGET.MSAS
VERSION = '1.0'
REDO = F
SIMSET.DIR = file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets')
BK.DIR = file.path(SYSTEMATIC.ROOT.DIR, 'backup_simsets')

for (msa in to.do)
{
    filename = get.full.filename(location=msa, version=VERSION)
    
    if (file.exists(file.path(BK.DIR, filename)))
        print(paste0("- Skipping ", msa, " (", msa.names(msa), ") - already done."))
    else if (file.exists(file.path(SIMSET.DIR, filename)))
    {
        print(paste0("- Redoing ", msa, " (", msa.names(msa), ")..."))
        load(SYSTEMATIC.ROOT.DIR, 'start_values', paste0(msa, '.Rdata'))
        load(file.path(SIMSET.DIR, filename))
        save(simset, file=file.path(BK.DIR, filename))
        
        
        run.simulation = create.run.simulation.function(msa=msa, start.values = starting.parameters)
        simset = extend.simulations(simset, fn=function(sim, pp){
            run.simulation(pp)  
        })
        save(simset, file=file.path(SIMSET.DIR, filename))
    }
    else
        print(paste0("- Skipping ", msa, " (", msa.names(msa), ") - simset not present."))
        
}