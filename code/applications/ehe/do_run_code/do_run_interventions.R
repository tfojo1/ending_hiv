
source('code/systematic_calibration/extract_and_run.R')
msas = TARGET.MSAS[6 + 8*3]#TARGET.MSAS[2 + 8*(0:3)]

INTERVENTIONS.TO.DO = NEW.A2.TO.ADD.SET.3Y[4] #ALL.INTERVENTIONS.3Y#[c(5,12)]

for (msa in msas)
{
    print("---------------------------------------------------------------------------")
    print(paste0("RUNNING FOR MSA ", toupper(msa.names(msa))))
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