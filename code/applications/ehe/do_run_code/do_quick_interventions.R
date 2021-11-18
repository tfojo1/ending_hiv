
source('code/source_code.R')
source("code/targets/target_msas.R")

msas = TARGET.MSAS
#interventions.to.do = MAIN.INTERVENTIONS.23.27[2]
interventions.to.do = IDU.INTERVENTIONS.23.27
OVERWRITE = T

for (msa in msas)
{
    print(paste0("DOING ", msa, " (", msa.names(msa), ')...'))
    load(file.path(SYSTEMATIC.ROOT.DIR, 'quick_simsets', paste0('1.0_', msa, '_quick.Rdata')))
    run.systematic.interventions(simset,
                                 dst.dir = file.path(SYSTEMATIC.ROOT.DIR, 'quick_simsets'),
                                 interventions=interventions.to.do,
                                 overwrite = OVERWRITE)
    
    print(paste0("...DONE with ", msa))
    
}