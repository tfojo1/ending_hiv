
source('code/source_code.R')
source('code/targets/target_msas.R')
source('code/systematic_calibration/systematic_cache_status.R')

if (1==2)
{
    prepare.simsets.for.visualization()
}

prepare.simsets.for.visualization <- function(locations=TARGET.MSAS,
                                              src.dir='mcmc_runs/full_simsets/',
                                              dst.dir='mcmc_runs/visualization_simsets/',
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
                save.seed.simset(full.simset, file.path(dst.dir, location))
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