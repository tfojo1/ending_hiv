
get.mcmc.done.msas <- function(dir='systematic_parallel')
{
    files = list.files(file.path(SYSTEMATIC.ROOT.DIR, dir))
    msas = substr(files, 1, 5)
    unique(msas)
}

get.simset.done.msas <- function(dir='full_simsets')
{
    files = list.files(file.path(SYSTEMATIC.ROOT.DIR, dir), include.dirs = F)
    files = files[!sapply(file.path(SYSTEMATIC.ROOT.DIR, dir, files), dir.exists)]
    msas = substr(files, 5, 9)
    unique(msas)
}

get.interventions.done.msas <- function(dir='full_simsets')
{
    files = list.dirs(file.path(SYSTEMATIC.ROOT.DIR, dir), full.names = F, recursive = F)
    files
}

check.cache.status <- function(msas=TARGET.MSAS,
                               print.missing=F)
{
    cache.dirs = list.dirs(file.path(SYSTEMATIC.ROOT.DIR, 'systematic_caches'), recursive = F)

    for (msa in msas)
    {
        mask = grepl(msa, cache.dirs)
        
        if (print.missing || any(mask))
            cat(as.character(msa.names(msa)), " (", msa, "):\n", sep='')
        
        if (!any(mask))
        {
            if (print.missing)
                cat(" - No cache set up\n")
        }
        else if (sum(mask)>1)
            cat(" - More than one cache set up\n")
        else
        {
            cache.dir = cache.dirs[mask]
            load(file.path(cache.dir, 'cache', 'global_control.Rdata'))
            chain.fracs = sapply(global.control@chain.control.filenames, function(ctrl.name){
                load(file.path(cache.dir, 'cache', ctrl.name))
                mean(chain.control@chunk.done)
            })
            
            if (all(chain.fracs==1))
                cat(" - All ", global.control@n.chains, " chains complete\n", sep='')
            else
            {
                for (chain in 1:global.control@n.chains)
                {
                    if (chain.fracs[chain]==1)
                        cat(" - Chain ", chain, ": complete\n", sep='')
                    else
                        cat(" - Chain ", chain, ": ", floor(100*chain.fracs[chain]), "% done\n", sep='')
                }
            }
        }
    }
}