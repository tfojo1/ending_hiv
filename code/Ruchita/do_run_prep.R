
load("code/Ruchita/baseline_prep_and_parameters_1K.Rdata")
load("code/Ruchita/raw_prep_results_1K.Rdata")
load("code/Ruchita/prep_results_and_parameters_1K_2021-11-09.Rdata")

# Run the Interventions
run.prep.simulations(msas = TARGET.MSAS,
                     intervention.codes = INJ.ORAL.VARIABLE.NEW.INTERVENTIONS.CODES)

# Make the aggregate result

# Run the Interventions
run.prep.simulations(msas = TARGET.MSAS,
                     intervention.codes = INJ.ORAL.VARIABLE.NEW.INTERVENTIONS.CODES)

# Make the aggregate result

# Run the Interventions
run.prep.simulations(msas = TARGET.MSAS,
                     intervention.codes = INJ.ORAL.VARIABLE.NEW.INTERVENTIONS.CODES)

# Make the aggregate result

prep.results = aggregate.raw.prep.results(msas=TARGET.MSAS,
                                          intervention.codes = COVERAGE.50.INTERVENTIONS.CODES,
                                          years=2020:2030,
                                          dir='mcmc_runs/prep_simsets',
                                          calculate.total=F)

# Make tables
# Oral vs Inj
table.v1 = make.prep.table(msas=TARGET.MSAS,
                           intervention.codes=INJ.VARIABLE.NEW.INTERVENTIONS.CODES,
                           comparison.codes=ORAL.VARIABLE.NEW.INTERVENTIONS.CODES,
                           raw.prep.results=prep.results, include.totals = F, stat = 'percent.diff')

write.table(table.v1, "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita/table.v1.txt",col.names = TRUE, row.names = TRUE, sep="\t")

#Reduction in Incidence for All Interventions
table.v2 = make.prep.table(msas=TARGET.MSAS,
                           intervention.codes=UPTAKE.INTERVENTIONS.CODES,
                           comparison.codes="baseline.oral.variable.efficacy",
                           raw.prep.results=prep.results, include.totals = F,
                           stat='diff')


write.table(table.v2, "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita/table.v3.txt",col.names = TRUE, row.names = TRUE, sep="\t")


#Sensitivity Analysis

run.prep.simulations(msas = TARGET.MSAS, intervention.codes = VAR.PREP.INTERVENTION.CODES)

prep.results = aggregate.raw.prep.results(msas = TARGET.MSAS, intervention.codes = VAR.ORAL.PREP.INTERVENTIONS.CODES)

sensitivity.plots = make.sensitivity.plot()

correlation.table = correlations()


prep.results = aggregate.raw.prep.results(msas=TARGET.MSAS,
                                          intervention.codes = COVERAGE.50.INTERVENTIONS.CODES,
                                          years=2020:2030,
                                          dir='mcmc_runs/prep_simsets',
                                          calculate.total=F)

# Make tables
# Oral vs Inj
table.v1 = make.prep.table(msas=TARGET.MSAS,
                           intervention.codes=INJ.VARIABLE.NEW.INTERVENTIONS.CODES,
                           comparison.codes=ORAL.VARIABLE.NEW.INTERVENTIONS.CODES,
                           raw.prep.results=prep.results, include.totals = F, stat = 'percent.diff')

write.table(table.v1, "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita/table.v1.txt",col.names = TRUE, row.names = TRUE, sep="\t")

#Reduction in Incidence for All Interventions
table.v2 = make.prep.table(msas=TARGET.MSAS,
                           intervention.codes=UPTAKE.INTERVENTIONS.CODES,
                           comparison.codes=UPTAKE.INTERVENTIONS.CODES[1],
                           raw.prep.results=prep.results, include.totals = F,
                           stat='diff')


write.table(table.v2, "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita/table.v3.txt",col.names = TRUE, row.names = TRUE, sep="\t")


#Sensitivity Analysis

run.prep.simulations(msas = TARGET.MSAS, intervention.codes = VAR.PREP.INTERVENTION.CODES)

prep.results = aggregate.raw.prep.results(msas = TARGET.MSAS, intervention.codes = VAR.ORAL.PREP.INTERVENTIONS.CODES)

sensitivity.plots = make.sensitivity.plot()

correlation.table = correlations()


prep.results = aggregate.raw.prep.results(msas=TARGET.MSAS,
                                          intervention.codes = COVERAGE.50.INTERVENTIONS.CODES,
                                          years=2020:2030,
                                          dir='mcmc_runs/prep_simsets',
                                          calculate.total=F)

# Make tables
# Oral vs Inj
table.v1 = make.prep.table(msas=TARGET.MSAS,
                           intervention.codes=INJ.VARIABLE.NEW.INTERVENTIONS.CODES,
                           comparison.codes=ORAL.VARIABLE.NEW.INTERVENTIONS.CODES,
                           raw.prep.results=prep.results, include.totals = F, stat = 'percent.diff')

write.table(table.v1, "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita/table.v1.txt",col.names = TRUE, row.names = TRUE, sep="\t")

#Reduction in Incidence for All Interventions
table.v2 = make.prep.table(msas=TARGET.MSAS,
                           intervention.codes=UPTAKE.INTERVENTIONS.CODES,
                           comparison.codes="baseline.oral.variable.efficacy",
                           raw.prep.results=prep.results, include.totals = F,
                           stat='diff')


write.table(table.v2, "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita/table.v3.txt",col.names = TRUE, row.names = TRUE, sep="\t")


#Sensitivity Analysis

#run.prep.simulations(msas = TARGET.MSAS, intervention.codes = VAR.PREP.INTERVENTION.CODES)

#prep.results = aggregate.raw.prep.results(msas = TARGET.MSAS, intervention.codes = VAR.ORAL.PREP.INTERVENTIONS.CODES)

sensitivity.plots = make.sensitivity.plot()

correlation.table = correlations()


source("code/process_results/make_pretty_table.R")
