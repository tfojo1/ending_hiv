source('code/source_code.R')
source('code/processing/pretty_table/make_systematic_table.R')
source('code/execution/distributed_process_results.R')
source('code/applications/ehe/create_ehe_intervention_presets.R')
source('code/applications/ehe/ehe_systematic_intervention_sets.R')

MSAS = c(BATON.ROUGE.MSA,
         BIRMINGHAM.MSA,
         JACKSON.MSA,
         MEMPHIS.MSA,
         NEW.ORLEANS.MSA
         )
SAVE.TO.DIR = '../../../Grants/R01 - Rural EHE/data'

for (msa in MSAS)
{
    print(paste0("Extracting for ", msa.names(msa), ' (', msa, ')'))
    do.get.raw.estimates(dir.name='full',
                         interventions=MAIN.INTERVENTIONS.23.27,
                         n.sim=1000,
                         suffix='rural',
                         msa=msa)
}

print('Done extracting. Assembling the estimates')
ests = assemble.estimates.and.intervals(dir.name='full',
                                        msas = MSAS,
                                        suffix='rural')
save(ests, 
     file=file.path(SAVE.TO.DIR, 'ests.Rdata'))


# to run on laptop
if (1==2)
{   
    source('code/processing/pretty_table/make_pretty_table.R')
    print('Done assembling. Writing systematic table')
    x=load(file.path(SAVE.TO.DIR, 'ests.Rdata'))
    
    mask = dimnames(ests$estimates)$intervention[-2]
    tab = paste0(floor(100*ests$estimates[,mask]), '%')
    dim(tab) = dim(ests$estimates[,mask])
    write.shaded.table(tab=tab,
                       color.by = ests$estimates[,mask],
                       file=file.path(SAVE.TO.DIR, 'results_table.xlsx'),
                       thresholds=c(0,0.9,1),
                       lower.threshold.colors=c('red','yellow2'),
                       upper.threshold.colors=c('green3','green4'))
}