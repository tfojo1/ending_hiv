
source('code/source_code.R')
source('code/targets/target_msas.R')

msas = c(NYC.MSA,
         LA.MSA,
         ATLANTA.MSA,
         BALTIMORE.MSA)


for (msa in msas)
{
    print(paste0("Doing ", msa.names(msa), " (", msa, ")..."))
    load(file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets', paste0('1.0_', msa, '_full.Rdata')))
    
    simset = thin.simset(simset, thin=5)
    save.simset(simset, file.path(SYSTEMATIC.ROOT.DIR, 'melissa_croi_simsets'), full=T)
}