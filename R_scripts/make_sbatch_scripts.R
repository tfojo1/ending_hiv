
source('code/targets/target_msas.R')

OUTPUT.DIR = "Ending_HIV/R_scripts/output"

make.sbatch.script <- function(filename,
                              mem='16GB',
                              mem.per.cpu=NULL,
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
    
    if (!is.null(mem.per.cpu))
        cat("#SBATCH --mem-per-cpu=", mem.per.cpu, '\n', sep='')
    
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
                             partition='unlimited',
                             mem='9600MB')
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
                               partition = partition,
                               time.hours = 7*24,
                               account=account,
                               commands= paste0("Rscript Ending_HIV/R_scripts/run_parallel_chain_script.R ", i, " ", chain))
        }
    }
}

make.quick.run.scripts <- function(msa.indices,
                             chains=1:4,
                             dir='R_scripts/quick_run_scripts/',
                             account='tfojo1',
                             partition='shared',
                             mem='9600MB',
                             mem.per.cpu=NULL)
{
    for (i in msa.indices)
    {
        for (chain in chains)
        {
            msa.name = names(TARGET.MSAS)[i]
            make.sbatch.script(filename=file.path(dir, get.quick.run.filename(i,chain)),
                               job.name = paste0("q", chain, msa.name),
                               mem=mem,
                               mem.per.cpu=mem.per.cpu,
                               output = file.path(OUTPUT.DIR, paste0("qrun_", msa.name, "_", chain, ".out")),
                               partition = partition,
                               time.hours = 72,
                               account=account,
                               commands= paste0("Rscript Ending_HIV/R_scripts/run_parallel_chain_script.R ", i, " ", chain))
        }
    }
}

make.flex.run.scripts <- function(msa.indices,
                                  chains=1:4,
                                  dir='R_scripts/flex_run_scripts/',
                                  account='tfojo1',
                                  partition='express',
                                  prefix='f',
                                  time.hours=12,
                                  mem='9600MB',
                                  mem.per.cpu=NULL)
{
    for (i in msa.indices)
    {
        for (chain in chains)
        {
            msa.name = names(TARGET.MSAS)[i]
            make.sbatch.script(filename=file.path(dir, get.flex.run.filename(i,chain)),
                               job.name = paste0(prefix, chain, msa.name),
                               mem=mem,
                               mem.per.cpu=mem.per.cpu,
                               output = file.path(OUTPUT.DIR, paste0("flex_", msa.name, "_", chain, ".out")),
                               partition = partition,
                               time.hours = time.hours,
                               account=account,
                               commands= paste0("Rscript Ending_HIV/R_scripts/run_parallel_chain_script.R ", i, " ", chain))
        }
    }
}


make.initial.scripts <- function(msa.indices,
                                 dir='R_scripts/initial_scripts/',
                                 partition='shared',
                                 account='tfojo1',
                                 mem=NULL)
{
    for (i in msa.indices)
    {
        msa.name = names(TARGET.MSAS)[i]
        make.sbatch.script(filename=file.path(dir, get.initial.filename(i)),
                           job.name = paste0("i", msa.name),
                           mem=mem,
                           output = file.path(OUTPUT.DIR, paste0("init_", msa.name, ".out")),
                           partition = 'shared',
                           time.hours = 48,
                           account=account,
                           commands= paste0("Rscript Ending_HIV/R_scripts/run_initial_chain_script.R ", i))
    }
}


make.intervention.scripts <- function(msa.indices,
                                      dir='R_scripts/intervention_scripts/',
                                      partition='shared',
                                      account='tfojo1',
                                      mem='9600MB')
{
    msa.indices = check.msa.indices(msa.indices)
    
    for (i in msa.indices)
    {
        msa.name = names(TARGET.MSAS)[i]
        make.sbatch.script(filename=file.path(dir, get.intervention.script.filename(i)),
                           job.name = paste0("i", msa.name),
                           mem=mem,
                           output = file.path(OUTPUT.DIR, paste0("int_", msa.name, ".out")),
                           partition = partition,
                           time.hours = 12,
                           account=account,
                           commands= paste0("Rscript Ending_HIV/R_scripts/run_interventions_script.R ", i))
     }
}

make.rerun.scripts <- function(msa.indices,
                                      dir='R_scripts/rerun_scripts/',
                                      partition='shared',
                                      account='tfojo1',
                                      mem='9600MB')
{
    msa.indices = check.msa.indices(msa.indices)
    
    for (i in msa.indices)
    {
        msa.name = names(TARGET.MSAS)[i]
        make.sbatch.script(filename=file.path(dir, get.rerun.script.filename(i)),
                           job.name = paste0("rr", msa.name),
                           mem=mem,
                           output = file.path(OUTPUT.DIR, paste0("rerun_", msa.name, ".out")),
                           partition = partition,
                           time.hours = 2,
                           account=account,
                           commands= paste0("Rscript Ending_HIV/R_scripts/rerun_simset_script.R ", i))
    }
}


make.setup.scripts <- function(msa.indices,
                               dir='R_scripts/setup_scripts/',
                               partition='express',
                               account='tfojo1')
{
    for (i in msa.indices)
    {
        msa.name = names(TARGET.MSAS)[i]
        make.sbatch.script(filename=file.path(dir, get.setup.filename(i)),
                           job.name = paste0("s", msa.name),
                           partition=partition,
                           account=account,
                           output = file.path(OUTPUT.DIR, paste0("setup_", msa.name, ".out")),
                           commands= paste0("Rscript Ending_HIV/R_scripts/setup_parallel_mcmc_script.R ", i))
    }
}

make.assemble.mcmc.script <- function(dir='R_scripts/other_scripts/',
                                      partition='express',
                                      account='tfojo1')
{
    make.sbatch.script(filename=file.path(dir, 'assemble_mcmc.bat'),
                       job.name = 'assemble',
                       partition=partition,
                       account=account,
                       output = file.path(OUTPUT.DIR, paste0("assemble_mcmc.out")),
                       commands= paste0("Rscript Ending_HIV/R_scripts/assemble_mcmc_script.R"))
}

make.assemble.quick.mcmc.script <- function(dir='R_scripts/other_scripts/',
                                            partition='express',
                                      account='tfojo1')
{
    make.sbatch.script(filename=file.path(dir, 'quick_assemble_mcmc.bat'),
                       job.name = 'q_assemble',
                       partition=partition,
                       account=account,
                       output = file.path(OUTPUT.DIR, paste0("quick_assemble_mcmc.out")),
                       commands= paste0("Rscript Ending_HIV/R_scripts/assemble_quick_script.R"))
}

##--------------------##
##-- MASTER SCRIPTS --##
##--------------------##

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


make.master.rerun.script <- function(msa.indices,
                                             filename='R_scripts/master_scripts/rerun_master.bat',
                                             path="Ending_HIV/R_scripts/rerun_scripts/")
{
    msa.indices = check.msa.indices(msa.indices)
    
    sink(filename)
    contents = cat(paste0(paste0("sbatch ", path, get.rerun.script.filename(msa.indices)),
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

make.master.quick.run.script <- function(msa.indices,
                                         chains=1:4,
                                         filename='R_scripts/master_scripts/quick_run_master.bat',
                                         path="Ending_HIV/R_scripts/quick_run_scripts/")
{
    n.msa = length(msa.indices)
    msa.indices = rep(msa.indices, each=length(chains))
    chains = rep(chains, n.msa)
    
    sink(filename)
    contents = cat(paste0(paste0("sbatch ", path, get.quick.run.filename(msa.indices, chains)),
                          collapse='\n'),
                   sep='')
    sink()
}

make.master.initial.script <- function(msa.indices,
                                         filename='R_scripts/master_scripts/initial_master.bat',
                                         path="Ending_HIV/R_scripts/initial_scripts/")
{
    sink(filename)
    contents = cat(paste0(paste0("sbatch ", path, get.initial.filename(msa.indices)),
                          collapse='\n'),
                   sep='')
    sink()
}

##-------------##
##-- HELPERS --##
##-------------##

get.run.filename <- function(index, chain)
{
    paste0("run_", index, "_", chain, ".bat")
}

get.quick.run.filename <- function(index, chain)
{
    paste0("qrun_", index, "_", chain, ".bat")
}

get.flex.run.filename <- function(index, chain)
{
    paste0("flex_", index, "_", chain, ".bat")
}

get.initial.filename <- function(index)
{
    paste0("init_", index, ".bat")
}

get.setup.filename <- function(index)
{
    paste0("setup_", index, ".bat")
}

get.intervention.script.filename <- function(index)
{
    paste0("int_", index, ".bat")
}

get.rerun.script.filename <- function(index)
{
    paste0("rerun_", index, ".bat")
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