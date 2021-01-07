
source('code/systematic_calibration/extract_and_run.R')

msa = CLEVELAND.MSA

print(qplot(1,1) + ggtitle(paste0(msa.names(msa), " (", msa, ")")) + theme(plot.title=element_text(hjust=1)))
do.run.interventions(msa)



