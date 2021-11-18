
#source('code/source_code.R')
source('code/systematic_calibration/extract_and_run.R')

SRC.DIR = 'mcmc_runs/full_simsets'
DST.DIR = 'mcmc_runs/quick_simsets'
N.SIM = 50

MSAS = TARGET.MSAS

for (i in 1:length(MSAS))
{
    msa = MSAS[i]
    
    print(paste0("Doing '", msa, "' (", i, " of ", length(MSAS), ")"))
    filename = paste0("1.0_", msa, "_full.Rdata")
    load(file.path(SRC.DIR, filename))
    simset = do.thin.simset.to(simset, thin.to=N.SIM)
    save(simset, file=file.path(DST.DIR, filename))
}