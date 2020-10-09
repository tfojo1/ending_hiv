
if (1==2)
{
    census.totals = read.census.totals()

    source('../code/baltimore.R')
    source('../code/data_managers/locale_mappings.R')
    load('../code/cached/ALL.DATA.MANAGERS.Rdata')
    baltimore.totals = get.census.totals(census.totals, BALTIMORE.MSA.COUNTIES)

    baltimore.check = get.census.data(ALL.DATA.MANAGERS$census.collapsed, fips=BALTIMORE.MSA.COUNTIES,
                                      aggregate.counties = T, aggregate.ages = T, aggregate.races = T, aggregate.sexes = T)

    range(baltimore.check - baltimore.totals[names(baltimore.check),])

    format(cbind(baltimore.check, baltimore.totals[names(baltimore.check),]), big.mark=',')
}

read.census.totals <- function(dir='../data2/Census/county_totals_sans_kids/')
{
    filenames = list.files(dir, full.names = T)

    df = NULL
    for (filename in filenames)
    {
        print(paste0("Reading ", filename))
        df = rbind(df,
                   read.table(filename, sep='\t', stringsAsFactors = F, header = T))
    }

    df$Population[df$Population == 'Missing'] = NA
    df$Population = as.numeric(df$Population)

    years = unique(as.character(df$Yearly.July.1st.Estimates))
    locations = unique(as.character(df$County.Code))

    dim.names = list(year=years, location=locations)
    rv = list(data=array(as.numeric(NA), dim=sapply(dim.names, length), dimnames=dim.names),
              years=years,
              locations=locations)

    for (i in 1:dim(df)[1])
        rv$data[as.character(df$Yearly.July.1st.Estimates[i]), as.character(df$County.Code[i])] = df$Population[i]

    rv
}

get.census.totals <- function(census.totals, location, years=census.totals$years,
                              collapse.counties=T, flatten.single.dim.array=F,
                              interpolate.missing.years=T)
{
    collapsed.name = NULL
    missing.locations = setdiff(location, census.totals$locations)
    if (length(missing.locations)>0)
    {
        location.msa.names = msa.names(location, use.division.names = T)
        if (all(!is.na(location.msa.names)))
        {
            if (length(unique(location))==1)
                collapsed.name = location

            location = counties.for.msa.or.division(location)
            missing.locations=character()
        }
    }

    location = unique(location)

    if (length(missing.locations)>0)
        stop(paste0("The following locations are not present in the census totals data:",
                    paste0(location, collapse=', ')))

    missing.years = setdiff(years, census.totals$years)
    
    if (length(missing.years)>0 && !interpolate.missing.years)
        stop(paste0("The following years are not present in the total census data: ", 
                    paste0(missing.years, collapse=', ')))
    
    rv = t(sapply(as.character(years), function(year){
        if (any(as.numeric(year)==census.totals$years))
            census.totals$data[year, location]
        else
        {
            nearest.year = census.totals$years[order(abs(as.numeric(census.totals$years)-as.numeric(year)))][1]
            census.totals$data[nearest.year, location]
        }
    }))
    
    if (length(location)==1)
    {
        if (!flatten.single.dim.array)
        {
            dim.names = list(year=years, location=location)
            dim(rv) = c(year=length(years), location=1)
            dimnames(rv) = dim.names
        }
        else
            rv = rv[1,]
    }
    else
    {
        if (collapse.counties)
        {
            rv = rowSums(rv)
            if (!flatten.single.dim.array)
            {
                dim.names = list(year=years, location=collapsed.name)
                dim(rv) = c(year=length(years), location=1)
                dimnames(rv) = dim.names
            }
        }
    }

    rv
}
