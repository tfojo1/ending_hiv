
source('code/systematic_calibration/extract_and_run.R')

msas = c(MEMPHIS.MSA, CLEVELAND.MSA) #TARGET.MSAS[8 + 8*(0:3)]

INTERVENTIONS.TO.DO = ALL.INTERVENTIONS.3Y#[c(5,12)]

for (msa in msas)
{
    print("---------------------------------------------------------------------------")
    print(paste0("RUNNING FOR MSA ", toupper(msa.names(msa))))
    print("---------------------------------------------------------------------------")
    
    print(qplot(1,1) + ggtitle(paste0(msa.names(msa), " (", msa, ")")) + theme(plot.title=element_text(hjust=1)))
    
    do.run.interventions(msa, interventions=INTERVENTIONS.TO.DO)
}