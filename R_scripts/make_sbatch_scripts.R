
source('code/targets/target_msas.R')

make.sbatch.script <- function(filename,
                              mem='16GB',
                              time.hours=NULL,
                              output=NULL,
                              job.name=NULL,
                              commands0 = 'ml R',
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
    
    
    if (!is.null(commands0))
        cat('\n', paste0(commands0, collapse='\n'), '\n', sep='')
    
    if (!is.null(commands))
        cat(paste0(commands, collapse='\n'), sep='')
    
    sink()
}

make.setup.scripts <- function(msa.indices,
                               dir='R_scripts/setup_scripts/')
{
    for (i in msa.indices)
    {
        msa.name = names(TARGET.MSAS)[i]
        make.sbatch.script(filename=file.path(dir, get.setup.filename(i)),
                           job.name = paste0("setup_", msa.name),
                           output = paste0("Ending_HIV/mcmc_runs/output/setup_", msa.name, ".out"),
                           commands= paste0("Rscript Ending_HIV/R_scripts/setup_parallel_mcmc_script.R ", i))
    }
}

make.setup.master.script <- function(msa.indices,
                                     filename='R_scripts/master_scripts/setup_master.bat',
                                     path="Ending_HIV/R_scripts/setup_scripts/")
{
    sink(filename)
    contents = cat(paste0(paste0("sbatch ", path, get.setup.filename(msa.indices)),
                          collapse='\n'),
                   sep='')
    sink()
}

get.setup.filename <- function(index)
{
    paste0("setup_", index, ".bat")
}