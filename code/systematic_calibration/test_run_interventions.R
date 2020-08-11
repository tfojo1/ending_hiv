
source('code/source_code.R')
source('code/systematic_calibration/systematic_settings.R')
source('code/systematic_calibration/systematic_interventions.R')


do.extract.and.save.test.simset(mcmc)
run.systematic.interventions(msa, 'test')


if (1==2) #to catch us up
{
    files = list.files('mcmc_runs/systematic_initial/')
    msas = substr(files, 1, 5)

    done = list.files('mcmc_runs/test_simset_interventions/')
    msas = setdiff(msas, done)
    
    for (msa in msas)
    {
        print(paste0("RUNNING INTERVENTIONS FOR ", msa.names(msa)))
        run.systematic.interventions(msa, 'test')
    }
}