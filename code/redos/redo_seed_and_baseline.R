source('code/interventions/systematic_interventions.R')
source('code/systematic_calibration/file_manager.R')

for (msa in TARGET.MSAS)
{
    print(paste0("Doing ", msa.names(msa), " (", msa, ")"))
    load(file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets', paste0("1.0_", msa, '_full.Rdata')))
    run.systematic.interventions(simset, dst.dir = file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'))
}