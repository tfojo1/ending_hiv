

get.target.counties <- function(dir='..')
{
    df = read.csv(file.path(dir, 'data/targets/counties.csv'))
    
    counties = paste0(df$county, ', ', df$state)
    counties[df$county=='Washington, DC'] = 'District of Columbia, DC'
    
    county.fips(counties)
}

get.target.msas <- function(dir='..',
                            include.pr=F)
{
    msas = unique(msa.or.division.for.county(get.target.counties()))
    
    if (!include.pr)
        msas = msas[sapply(msas, function(msa){
            all(states.for.msa(msa) != 'PR')
        })]
    
    msas
}

get.hiv.burden <- function(msas = get.target.msas(),
                           year=2017,
                           as.table=T)
{
    rv = sapply(msas, function(msa){
        get.surveillance.data(msa.surveillance, location.codes = msa, year=year, data.type='new')
    })
    
    names(rv) = msa.names(msas)
    
    rv = sort(rv, decreasing = T)
    
    if (as.table)
    {
        df = data.frame(MSA=names(rv),
                   x=as.numeric(rv))
        names(df)[2] = paste0(year, '_new_diagnoses')
        df
    }
    else
        rv
}