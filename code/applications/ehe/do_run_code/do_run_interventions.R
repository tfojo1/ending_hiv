
source('code/source_code.R')
source('code/execution/extract_and_run.R')
source('code/applications/ehe/create_ehe_intervention_presets.R')
source('code/applications/ehe/ehe_systematic_intervention_sets.R')
msas = JACKSON.MSA
CHUNK = 1

N.CHUNKS = 1

INTERVENTION.CODES.TO.DO = c(setdiff(WEB.TOOL.INTERVENTION.CODES, EHE.TALK.INTERVENTION.CODES),
                             EHE.TALK.INTERVENTION.CODES)
#INTERVENTION.CODES.TO.DO = rev(c(INTERVENTION.CODES.TO.DO[!grepl('23.27', INTERVENTION.CODES.TO.DO)],
#                                 INTERVENTION.CODES.TO.DO[grepl('23.27', INTERVENTION.CODES.TO.DO)]))
INTERVENTIONS.TO.DO = lapply(INTERVENTION.CODES.TO.DO, intervention.from.code)

mask = ((1:length(INTERVENTIONS.TO.DO)-1)%%N.CHUNKS+1) == CHUNK
INTERVENTIONS.TO.DO = INTERVENTIONS.TO.DO[mask]

for (msa in msas)
{
    print("---------------------------------------------------------------------------")
    print(paste0("RUNNING FOR MSA ", toupper(msa.names(msa))))
    if (N.CHUNKS>1)
        print(paste0("Chunk ", CHUNK))
    print("---------------------------------------------------------------------------")
    
    print(qplot(1,1) + ggtitle(paste0(msa.names(msa), " (", msa, ")")) + theme(plot.title=element_text(hjust=1)))
    
    do.run.interventions(msa, interventions=INTERVENTIONS.TO.DO)
}

if (1==2)
{
    source('code/systematic_calibration/systematic_cache_status.R')
    msa.names(setdiff(get.simset.done.msas(), get.interventions.done.msas()))
    
    msa.names(setdiff(TARGET.MSAS, get.simset.done.msas()))
}