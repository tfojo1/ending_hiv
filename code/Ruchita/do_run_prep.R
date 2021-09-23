

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

write.table(table.v1, "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita/table.v1.txt",col.names = TRUE, row.names = TRUE, sep="\t")

# All vs No Intervention
table.v2 = make.prep.table(msas=TARGET.MSAS,
                           intervention.codes=STAGGERED.ORAL.INJ.PREP.CODES,
                           comparison.codes='noint',
                           raw.prep.results=prep.results, include.totals = F,
                           stat='rel.diff')


write.table(table.v2, "/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/Ruchita/table.v2.txt",col.names = TRUE, row.names = TRUE, sep="\t")
