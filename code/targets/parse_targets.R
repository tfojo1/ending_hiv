

get.target.counties <- function(dir='..')
{
    df = read.csv(file.path(dir, 'data/targets/counties.csv'))
    
    counties = paste0(df$county, ', ', df$state)
    counties[df$county=='Washington, DC'] = 'District of Columbia, DC'
    
    county.fips(counties)
}

get.target.msas <- function(dir='..',
                            include.pr=F,
                            use.divisions=F)
{
    if (use.divisions)
        msas = unique(msa.or.division.for.county(get.target.counties(dir=dir)))
    else
        msas = unique(msa.for.county(get.target.counties(dir=dir)))
        
    if (!include.pr)
        msas = msas[sapply(msas, function(msa){
            all(states.for.msa(msa) != 'PR')
        })]
    
    msas
}

get.hiv.burden <- function(msas = get.target.msas(use.divisions=use.divisions),
                           year=2017,
                           as.table=T,
                           use.divisions=F)
{
    rv = sapply(msas, function(msa){
        get.surveillance.data(msa.surveillance, location.codes = msa, year=year, data.type='new')
    })
    
    names(rv) = msa.names(msas)
    
    o = order(rv, decreasing = T)
    rv = rv[o]
    msas = msas[o]
    
    if (as.table)
    {
        df = data.frame(MSA=names(rv),
                        CBSA=msas,
                   x=as.numeric(rv))
        names(df)[3] = paste0(year, '_new_diagnoses')
        df
    }
    else
        rv
}

get.hiv.burden.table <- function(new.year=2018,
                                 prev.year=2017)
{
    counties = get.target.counties()
    counties = counties[order(state.for.county(counties), county.names(counties))]
    
    msas = msa.for.county(counties)
    
    diagnoses = sapply(msas, function(msa){
        get.surveillance.data(msa.surveillance, location.codes = msa, year=new.year, data.type='new')
    })
    
    prev = sapply(msas, function(msa){
        get.surveillance.data(msa.surveillance, location.codes = msa, year=prev.year, data.type='prevalence')
    })
    
    o = order(diagnoses, decreasing = T)
    
    rv = data.frame(county=county.names(counties[o]),
                    msa=unlist(msa.names(msas[o])),
                    new_diagnoses=format(diagnoses[o], big.mark=','),
                    estimated_prevalence=format(prev[o], big.mark=','))
    
    rv
    
}