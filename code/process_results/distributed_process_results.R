

if (1==2)
{
    source('code/source_code.R')
    source('code/targets/target_msas.R')
    source('code/process_results/make_systematic_table.R')
    for (msa in TARGET.MSAS)
        do.get.raw.estimates(dir.name='quick',
                             interventions=union.intervention.lists(MAIN.INTERVENTIONS.23.27[c(1,4)], IDU.INTERVENTIONS.23.27),
                             suffix='idu.and.comp',
                             msa=msa)
    
    ests = assemble.estimates.and.intervals(dir.name='quick',
                                            suffix='idu.and.comp')
    
    
    write.systematic.table(ests, 
                           interventions = union.intervention.lists(MAIN.INTERVENTIONS.23.27[c(1,4)], IDU.INTERVENTIONS.23.27),
                           file=file.path(SAVE.TO.DIR, 'main_table_quick.xlsx'))
    
    
    
    
}


SAVE.TO.DIR = '../Manuscripts/manuscript_1/Annals Submission/revision 1/tables/'

do.get.raw.estimates <- function(dir.name=c('full','quick')[1],
                                 msa,
                                 suffix='',
                                 interventions=MAIN.INTERVENTIONS.23.27,
                                 n.sim=c(full=1000, quick=50)[dir.name],
                                 verbose=T,
                                 year1=2020,
                                 year2=2030)
{
    if (nchar(suffix) > 0 && !grepl("^_", suffix))
        suffix = paste0("_", suffix)
    
    dir = file.path(SYSTEMATIC.ROOT.DIR, paste0(dir.name, "_simsets"))
    est=get.raw.estimates(dir=dir,
                                msas=msa,
                                interventions=interventions,
                                n.sim=n.sim,
                                verbose=verbose,
                                year1=year1,
                                year2=year2)
    save(est, file=file.path(SYSTEMATIC.ROOT.DIR, '..', 'results', dir.name, paste0(msa, suffix, '.Rdata')))
}

assemble.estimates.and.intervals <- function(dir.name=c('full','quick')[1],
                                             msas=TARGET.MSAS,
                                             suffix = '',
                                             summary.stat=mean,
                                             interval.coverage=0.95,
                                             calculate.total=T)
{
    if (nchar(suffix) > 0 && !grepl("^_", suffix))
        suffix = paste0("_", suffix)
    
    ests = lapply(msas, function(msa){
        file=file.path(SYSTEMATIC.ROOT.DIR, '..', 'results', dir.name, paste0(msa, suffix, '.Rdata'))
        if (file.exists(file))
        {
            load(file)
            est
        }
        else
            NULL
    })
    
    was.missing = sapply(ests, is.null)
 #   msas = msas[!was.missing]
 #   ests = ests[!was.missing]
    
    if (all(was.missing))
        stop("No locations have been done")
    
    dim.names = dimnames(ests[!was.missing][[1]])
    dim.names$location = msas
    
    all.arr = array(NaN, dim=sapply(dim.names, length),
                    dimnames=dim.names)
    for (i in 1:length(msas))
    {
        if (!was.missing[i])
            all.arr[,,i,] = ests[[i]]
    }
    
    attr(all.arr, 'interventions') = attr(ests[[1]], 'interventions')
    
    do.crunch.estimates.and.intervals(all.arr,
                                      summary.stat=mean,
                                      interval.coverage=0.95,
                                      calculate.total=T)
}