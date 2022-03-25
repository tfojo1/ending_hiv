
source('code/source_code.R')
source('code/applications/covid/extract_covid_results.R')


MSAS = c(SEATTLE.MSA, DENVER.MSA, PORTLAND.MSA)

outcomes.arr = process.covid.outcomes(locations=MSAS,
                                      include.baseline = T, n.sim=5)
location.names = unlist(msa.names(MSAS))


save(outcomes.arr, location.names, file='results/covid/covid_4.2_mountain_west_results.Rdata')
