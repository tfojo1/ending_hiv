
source('code/source_code.R')
source('code/execution/systematic_cache_status.R')

source('code/applications/ehe/create_ehe_intervention_presets.R')
source('code/applications/ehe/ehe_systematic_intervention_sets.R')

if (1==2)
{
    prepare.simsets.for.visualization(locations=c(DENVER.MSA))
}

prepare.simsets.for.visualization <- function(locations=TARGET.MSAS,
                                              src.dir=file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets'),
                                              dst.dir=file.path(SYSTEMATIC.ROOT.DIR, 'visualization_simsets'),
                                              verbose=T, overwrite=F,
                                              n.keep=80)
{
    
    locations = intersect(locations, get.simset.done.msas())
    print(paste0("Cutting visualization simsets for ", length(locations), " location(s)"))
    
    for (location in locations)
    {
        if (verbose)
            print(paste0("- Cutting visualization simsets for ", msa.names(location), " (", location, ')'))
        
        #-- baseline and seed --#
        baseline.exists = file.exists(file.path(dst.dir, location, get.baseline.filename(location)))
        seed.exists = file.exists(file.path(dst.dir, location, get.seed.filename(location)))
        if (!baseline.exists || !seed.exists || overwrite)
        {
            if (verbose)
                print("  - Doing baseline and seed")
            
            load(file.path(src.dir, get.full.filename(location)))
            full.simset = thin.simset.for.visualization(simset, n.keep)
        
            if (overwrite || !baseline.exists)
                save.simset(full.simset, dir=dst.dir, compress=T)
            if (overwrite || !seed.exists)
                save.seed.simset(full.simset, file.path(dst.dir))
        }
        
        #-- Intervention simsets --#
        filenames = list.files(file.path(src.dir, location))
        if (!overwrite)
            filenames = filenames[!file.exists(file.path(dst.dir, location, filenames))]
        if (verbose)
            print(paste0("  - Doing ", length(filenames), ' intervention simset(s)'))
        for (file in filenames)
        {
            load(file.path(src.dir, location, file))
            simset = thin.simset.for.visualization(simset, n.keep)
            save.simset(simset, dst.dir, compress=T)
        }
    }
}

thin.simset.for.visualization <- function(simset,
                                          n.keep=80)
{
    rv = thin.simset(simset, thin = floor(simset@n.sim/n.keep))
    indices = (rv@n.sim-n.keep+1):rv@n.sim
    rv = subset.simset(simset, indices)
    rv
}