

# Run the Interventions
run.prep.simulations(msas = TARGET.MSAS)

# Make the aggregate result
prep.results = aggregate.raw.prep.results()

# Make tables
# Oral vs Inj
table.v1 = make.prep.table(msas=TARGET.MSAS,
                           intervention.codes=INJ.PREP.INTERVENTION.CODES,
                           comparison.codes=ORAL.PREP.INTERVENTION.CODES,
                           raw.prep.results=prep.results, include.totals = F, stat = 'rel.diff')
# All vs No Intervention
table.v2 = make.prep.table(msas=TARGET.MSAS,
                           intervention.codes=STAGGERED.ORAL.INJ.PREP.CODES,
                           comparison.codes='noint',
                           raw.prep.results=prep.results,
                           stat='rel.diff')

