


source('code/source_code.R')
source('code/Ruchita/create_prep_interventions4.R')
source('code/Ruchita/systematic_prep_2.R')
source('code/Ruchita/create_prep_study_comparisons.R')

print("JUST DOING COMPARISONS")
INTERVENTIONS.TO.RUN = sapply(COMPARISON.INTERVENTIONS.TO.RUN, get.intervention.code)
MSAs = ATLANTA.MSA