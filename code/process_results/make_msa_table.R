

if (1==2)
{
    
    NEW.YEAR = 2017
    PREV.YEAR = 2017
    STRAT.YEAR =2017
    NEW.DENOMINATOR = 100000
    PREV.DENOMINATOR = 100000
    
    msas = get.hiv.burden(year = NEW.YEAR)$CBSA
    
    new.dx = get.surveillance.data(location.codes=msas, data.type='new', year=NEW.YEAR, aggregate.locations = F)[1,]
    prev = get.surveillance.data(location.codes=msas, data.type='prevalence', year=PREV.YEAR, aggregate.locations = F)[1,]
    pop.for.new = sapply(msas, function(msa){
        get.census.totals(ALL.DATA.MANAGERS$census.totals, location=msa, years=NEW.YEAR)
    })
    pop.for.prev = sapply(msas, function(msa){
        get.census.totals(ALL.DATA.MANAGERS$census.totals, location=msa, years=PREV.YEAR)
    })
    new.by.race = get.surveillance.data(location.codes=msas, data.type='new', year=NEW.YEAR, race=T, aggregate.locations = F)[1,,]
    frac.new.by.race = new.by.race / rowSums(new.by.race)
    
    df = data.frame(msa=as.character(msa.names(msas)),
                    new_diagnoses=format.num.and.per.pop(new.dx, pop.for.new, per=NEW.DENOMINATOR),
                    prevalence=format.num.and.per.pop(prev, pop.for.prev, per=PREV.DENOMINATOR),
                    proportion_new_black = paste0(round(100*frac.new.by.race[,'black']), '%'),
                    proportion_new_hispanic = paste0(round(100*frac.new.by.race[,'hispanic']), '%')
    )
    
    write.csv(df, file='results/tables/msa_table.csv')
}

format.num.and.per.pop <- function(num,
                                   pop,
                                   per=100000,
                                   sep=' ',
                                   digits=0,
                                   show.per=F)
{
    rv = paste0(format(num, big.mark=',', scientific = F, trim=T),
           sep,
           "(",
           format(round(per*num/pop, digits), big.mark=',', scientific = F, trim=T)
    )
    
    if (show.per)
        rv = paste0(rv, 
                    " per ",
                    format(per, big.mark=',', scientific = F, trim=T)
        )
    
    rv = paste0(rv,
                ")")
    
    rv
}