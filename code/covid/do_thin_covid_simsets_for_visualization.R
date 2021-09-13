
source('code/systematic_calibration/extract_and_run.R')
source('code/covid/prepare_and_run_covid_sims.R')

msas = TARGET.MSAS
overwrite = F
scenarios = c('base','delayed.hiv.care')

SRC.DIR = 'mcmc_runs/covid_simsets'
DST.DIR = 'mcmc_runs/covid_visualization_simsets'

THIN.TO = 500

version=paste0('covid.', COVID.VERSION)

for (msa in msas)
{
    print(paste0("Doing ", length(scenarios), " scenario(s) for '", msa, "'"))
    for (sc in scenarios)
    {
        base.filename = get.simset.filename(location=msa, intervention.code=sc, version=version)
        dst.file = file.path(DST.DIR, msa, base.filename)
        
        if (overwrite || !file.exists(dst.file))
        {
            print(paste0(" - Doing '", sc, "' for '", msa, '"'))
            
            src.file = file.path(SRC.DIR, msa, base.filename)
            load(src.file)
            simset = do.thin.simset.to(simset, thin.to=THIN.TO)
            simset = compress.simset(simset)
            simset = pare.simset.components(simset, aggressive=T)
            
            if (!dir.exists(file.path(DST.DIR, msa)))
                dir.create(file.path(DST.DIR, msa))
            
            save(simset, file=dst.file)
        }
        else
            print(paste0(" - Skipping '", sc, "' for '", msa, '"'))
        
    }
}