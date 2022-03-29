source('code/source_code.R')
source('code/processing/pretty_table/make_systematic_table.R')
source('code/execution/distributed_process_results.R')
source('code/applications/ehe/create_ehe_intervention_presets.R')
source('code/applications/ehe/ehe_systematic_intervention_sets.R')

MSAS = c(DENVER.MSA, PORTLAND.MSA, SEATTLE.MSA)
SAVE.TO.DIR = '../../../Talks/Ending HIV Model v1/Talks post Annals/tables/'

for (msa in MSAS)
{
    print(paste0("Extracting for ", msa.names(msa), ' (', msa, ')'))
    do.get.raw.estimates(dir.name='full',
                         interventions=MAIN.INTERVENTIONS.23.27,
                         n.sim=225,
                         suffix='mountain.west',
                         msa=msa)
}

print('Done extracting. Assembling the estimates')
ests = assemble.estimates.and.intervals(dir.name='full',
                                        msas = MSAS,
                                        suffix='mountain.west')
save(ests, 
     file=file.path(SAVE.TO.DIR, 'ests.Rdata'))


# to run on laptop
if (1==2)
{   
    source('code/processing/pretty_table/make_pretty_table.R')
    print('Done assembling. Writing systematic table')
    x=load(file.path(SAVE.TO.DIR, 'ests.Rdata'))
    write.systematic.table(ests, 
                           interventions = c(NO.INTERVENTION, MAIN.INTERVENTIONS.23.27),
                           file=file.path(SAVE.TO.DIR, 'main_table_mountain_west.xlsx'))
}