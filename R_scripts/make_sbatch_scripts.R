
source('code/targets/target_msas.R')

OUTPUT.DIR = "Ending_HIV/R_scripts/output"

make.sbatch.script <- function(filename,
                              mem='16GB',
                              time.hours=NULL,
                              output=NULL,
                              job.name=NULL,
                              commands0 = 'ml R',
                              partition=NULL,
                              account=NULL,
                              commands)
{
    sink(filename)
    
    cat("#!/bin/bash\n\n")
    if (!is.null(job.name))
        cat("#SBATCH --job-name=", job.name, '\n', sep='')
    
    if (!is.null(mem))
        cat("#SBATCH --mem=", mem, '\n', sep='')
    
    if (!is.null(output))
        cat("#SBATCH --output=", output, '\n', sep='')
    
    if (!is.null(time.hours))
        cat("#SBATCH --time=", time.hours, ':00:00\n', sep='')
    
    if (!is.null(partition))
        cat("#SBATCH --partition=", partition, '\n', sep='')
    
    if (!is.null(account))
        cat("#SBATCH --account=", account, '\n', sep='')
    
    if (!is.null(commands0))
        cat('\n', paste0(commands0, collapse='\n'), '\n', sep='')
    
    if (!is.null(commands))
        cat(paste0(commands, collapse='\n'), sep='')
    
    sink()
}

make.run.scripts <- function(msa.indices,
                             chains=1:4,
                             dir='R_scripts/run_scripts/',
                             account='tfojo1',
                             mem=NULL)
{
    for (i in msa.indices)
    {
        for (chain in chains)
        {
            msa.name = names(TARGET.MSAS)[i]
            make.sbatch.script(filename=file.path(dir, get.run.filename(i,chain)),
                               job.name = paste0("r", chain, msa.name),
                               mem=mem,
                               output = file.path(OUTPUT.DIR, paste0("run_", msa.name, "_", chain, ".out")),
                               partition = 'unlimited',
                               time.hours = 5*24,
                               account=account,
                               commands= paste0("Rscript Ending_HIV/R_scripts/run_parallel_chain_script.R ", i, " ", chain))
        }
    }
}

make.intervention.scripts <- function(msa.indices,
                                      dir='R_scripts/intervention_scripts/',
                                      account='tfojo1',
                                      mem=NULL)
{
    msa.indices = check.msa.indices(msa.indices)
    
    for (i in msa.indices)
    {
        msa.name = names(TARGET.MSAS)[i]
        make.sbatch.script(filename=file.path(dir, get.intervention.script.filename(i)),
                           job.name = paste0("i", msa.name),
                           mem=mem,
                           output = file.path(OUTPUT.DIR, paste0("int_", msa.name, ".out")),
                           partition = 'shared',
                           time.hours = 12,
                           account=account,
                           commands= paste0("Rscript Ending_HIV/R_scripts/run_interventions_script.R ", i))
     }
}

make.setup.scripts <- function(msa.indices,
                               dir='R_scripts/setup_scripts/',
                               account='tfojo1')
{
    for (i in msa.indices)
    {
        msa.name = names(TARGET.MSAS)[i]
        make.sbatch.script(filename=file.path(dir, get.setup.filename(i)),
                           job.name = paste0("s", msa.name),
                           partition='shared',
                           account=account,
                           output = file.path(OUTPUT.DIR, paste0("setup_", msa.name, ".out")),
                           commands= paste0("Rscript Ending_HIV/R_scripts/setup_parallel_mcmc_script.R ", i))
    }
}

make.master.setup.script <- function(msa.indices,
                                     filename='R_scripts/master_scripts/setup_master.bat',
                                     path="Ending_HIV/R_scripts/setup_scripts/")
{
    sink(filename)
    contents = cat(paste0(paste0("sbatch ", path, get.setup.filename(msa.indices)),
                          collapse='\n'),
                   sep='')
    sink()
}

make.master.interventions.script <- function(msa.indices,
                                     filename='R_scripts/master_scripts/interventions_master.bat',
                                     path="Ending_HIV/R_scripts/intervention_scripts/")
{
    msa.indices = check.msa.indices(msa.indices)
    
    sink(filename)
    contents = cat(paste0(paste0("sbatch ", path, get.intervention.script.filename(msa.indices)),
                          collapse='\n'),
                   sep='')
    sink()
}

make.master.run.script <- function(msa.indices,
                                   chains=1:4,
                                     filename='R_scripts/master_scripts/run_master.bat',
                                     path="Ending_HIV/R_scripts/run_scripts/")
{
    n.msa = length(msa.indices)
    msa.indices = rep(msa.indices, each=length(chains))
    chains = rep(chains, n.msa)
    
    sink(filename)
    contents = cat(paste0(paste0("sbatch ", path, get.run.filename(msa.indices, chains)),
                          collapse='\n'),
                   sep='')
    sink()
}

get.run.filename <- function(index, chain)
{
    paste0("run_", index, "_", chain, ".bat")
}

get.setup.filename <- function(index)
{
    paste0("setup_", index, ".bat")
}

get.intervention.script.filename <- function(index)
{
    paste0("int_", index, ".bat")
}

check.msa.indices <- function(msa.indices)
{
    if (all(as.integer(msa.indices)>length(TARGET.MSAS)))
    {
        print("Treating msa.indices as msa codes")
        msa.indices = msas.to.indices(msa.indices)
        if (any(is.na(msa.indices)))
            stop("some elements of msa indices were neither valid indices nor valid msa codes")
    }
    
    msa.indices
}