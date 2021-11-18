
source('code/systematic_calibration/extract_and_run.R')

msas = ST.LOUIS.MSA
interventions.to.do = WEB.TOOL.INTERVENTIONS
N.SIM.FOR.VISUALIZATION = 80
OVERWRITE = F

for (msa in msas)
{
    print(paste0("DOING VISUALIZATION INTERVENTIONS FOR ", msa, " (", msa.names(msa), ')...'))
    load(file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets', get.full.filename(msa)))
    simset = do.thin.simset.to(simset, N.SIM.FOR.VISUALIZATION)
    run.systematic.interventions(simset,
                                 dst.dir = file.path(SYSTEMATIC.ROOT.DIR, 'visualization_simsets'),
                                 interventions=interventions.to.do,
                                 overwrite = OVERWRITE)
    
    print(paste0("...DONE with ", msa))
    
}