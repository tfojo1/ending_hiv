

# Run the Interventions
run.prep.simulations(msas = TARGET.MSAS)

# Make the aggregate result
prep.results = aggregate.raw.prep.results(msas=TARGET.MSAS,
                                          intervention.codes = ALL.VAR.ORAL.PREP.INTERVENTIONS.CODES,
                                          years=2020:2030,
                                          dir='mcmc_runs/prep_simsets',
                                          calculate.total=F)

# Make tables
# Oral vs Inj
table.v1 = make.prep.table(msas=TARGET.MSAS,
                           intervention.codes=VAR.PREP.INTERVENTION.CODES,
                           comparison.codes=ORAL.PREP.INTERVENTION.CODES,
                           raw.prep.results=prep.results, include.totals = F, stat = 'percent.diff')

write.table(table.v1, "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita/table.v1.txt",col.names = TRUE, row.names = TRUE, sep="\t")

#Reduction in Incidence for All Interventions
table.v2 = make.prep.table(msas=TARGET.MSAS,
                           intervention.codes=ALL.VAR.ORAL.PREP.INTERVENTIONS.CODES,
                           comparison.codes='noint',
                           raw.prep.results=prep.results, include.totals = F,
                           stat='diff')


write.table(table.v2, "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita/table.v3.txt",col.names = TRUE, row.names = TRUE, sep="\t")


#Sensitivity Analysis

run.prep.simulations(msas = TARGET.MSAS, intervention.codes = VAR.PREP.INTERVENTION.CODES)

prep.results = aggregate.raw.prep.results(msas = TARGET.MSAS, intervention.codes = VAR.ORAL.PREP.INTERVENTIONS.CODES)

sensitivity.plots = make.sensitivity.plot()

correlation.table = correlations()
