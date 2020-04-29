
source('../code/interventions/parse_interventions.R')
source('../code/source_code.R')

summ = parse.intervention.tables(dir='../code/results/baltimore_v3/')
tab = make.text.table.two.rows.per(summ,digits = 0);tab

write.csv(tab, file='../CROI/poster/effects_table.csv')


load('../code/results/baltimore_v1/no.intervention.Rdata')
no.int = simset

load('../code/results/baltimore_v1/Black_MSM_testing_1pyr_0.9_suppressed_0.5_PrEP.Rdata')
black.supp9.prep5 = simset

simsets = list(no.int, black.supp9.prep5)
names(simsets) = c("No Interventions", "Black MSM - 90% Suppressed, 25% PrEP")
plot.calibration.total.incidence(simsets, years=2010:2030) + theme(legend.position = 'bottom')
