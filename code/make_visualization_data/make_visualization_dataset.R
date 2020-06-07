
source('code/source_code.R')
source('code/targets/parse_targets.R')
source('code/interventions/interventions_for_simset.R')

if (1==2)
{
    
    ##----------------------------##
    ##-- MAKE THE INTERVENTIONS --##
    ##----------------------------##

    intervention.1 = create.one.intervention("Young Black MSM",
                                             testing.frequency = 1,
                                             suppressed.proportion = .9,
                                             prep.coverage = .25,
                                             intervention.ramped.up.year = 2022)
    
    intervention.2 = create.one.intervention("All MSM or IDU",
                                             testing.frequency = 1,
                                             suppressed.proportion = .8,
                                             prep.coverage = .5,
                                             intervention.ramped.up.year = 2022)
    
    interventions = list(NULL, intervention.1, intervention.2)
    names(interventions) = as.character(1:length(interventions) - 1)
    
    
    ##---------------------------##
    ##-- PREPARE THE SIMSET(s) --##
    ##---------------------------##
    
    #load('mcmc_runs/dc.68_aids.dx_cum.mort.1x_t1=08_20000_2020-05-02.Rdata')
    load('mcmc_runs/balt.68.3_aids.dx_cum.mort.1x_t1=08_20000_2020-05-05.Rdata')
    base.simsets = list(extract.simset(mcmc, additional.burn=400, additional.thin=4))
    load('mcmc_runs/dc.68.6_aids.dx_cum.mort.1x_t1=08_20000_2020-05-06.Rdata')
    base.simsets = c(base.simsets, list(extract.simset(mcmc, additional.burn=400, additional.thin=4)))
        

    base.simsets = lapply(base.simsets, prepare.simset.for.interventions)
    
    
    ##---------------------------##
    ##-- RUN THE INTERVENTIONS --##
    ##---------------------------##
    
    intervention.simsets = run.multiple.interventions.on.multiple.simsets(base.simsets,
                                                                          interventions,
                                                                          keep.years.if.null.intervention=2000:2030,
                                                                          keep.years.for.interventions=2020:2030)
    
    ##------------------------------##
    ##-- GENERATE THE DATA FRAMES --##
    ##------------------------------##
    
    simset.df = make.multi.simset.data.frame(intervention.simsets)
    LOCATIONS = unique(simset.df$location_code)
        
    epi.df = make.epi.data.df(msa.surveillance, locations = LOCATIONS)
    
    intervention.key = make.intervention.key(interventions)
    location.key = make.location.key(LOCATIONS)
    
    
    ##---------------------##
    ##-- WRITE THE FILES --##
    ##---------------------##
    
    write.csv(simset.df, file='visualization/raw/Simulations.csv', row.names=F)
    write.csv(epi.df, file='visualization/raw/Epi_Data.csv', row.names=F)
    write.csv(intervention.key, file='visualization/raw/Interventions.csv', row.names=F)
    write.csv(location.key, file='visualization/raw/Locations.csv', row.names=F)
    
}


DATA.TYPE.NAMES = c(new='New_Diagnoses',
                    prevalence='Prevalence',
                    incidence='Incidence',
                    diagnosed='Percent_Diagnosed',
                    mortality='HIV_Mortality')

run.multiple.interventions.on.multiple.simsets <- function(simsets,
                                                           interventions,
                                                           keep.years.if.null.intervention=1970:2030,
                                                           keep.years.for.interventions=2020:2030)
{
    rv = list()
    
    for (simset in simsets)
    {
        rv = c(rv, lapply(names(interventions), function(int.name){
            int = interventions[[int.name]]
            
            if (is.null(int))
                keep.years = keep.years.if.null.intervention
            else
                keep.years = keep.years.for.interventions
            
            int.simset = run.simset.intervention(simset, intervention=int, keep.years=keep.years)
            attr(int.simset, 'intervention.id') = int.name
            
            int.simset
        }))
    }
    
    rv
}

make.multi.simset.data.frame <- function(simsets)
{
    df = NULL
    for (i in 1:length(simsets))
    {
        one.df = make.simset.data.frame(simsets[[i]])
        df = rbind(df, one.df)
    }
    
    COLUMN.NAMES = c('intervention_id', 'location_code', 'data_type',
                     'year', 'age', 'race', 'sex', 'risk',
                     'simulation', 'value')
    
    df[,COLUMN.NAMES]
}

make.interventions.key.data.frame <- function()
{
    
}
make.simset.data.frame <- function(simset,
                                   years=2000:2030)
{
    years = intersect(simset@simulations[[1]]$years, years)
    
    location = attr(simset@simulations[[1]], 'location')
    population = fill.in.population(location, years)
 
    df = NULL
    for (data.type in c('new','prevalence','incidence','mortality','diagnosed'))
    {
        one.df = get.simset.projection.df(simset,
                                          data.type=data.type,
                                          years=years,
                                          denominators = population)
        one.df$data_type = DATA.TYPE.NAMES[data.type]
        one.df = one.df[one.df$sex!='female' | (one.df$risk!='msm' & one.df$risk!='msm_idu'),]
        
        df = rbind(df, one.df)
    }
    
    df$location_code = as.character(location)
    df$intervention_id = attr(simset, 'intervention.id')
#    df$location_name = as.character(msa.names(location))
    
    
    df
}

fill.in.population <- function(location,
                               years)
{
    years = as.character(years)
    
    population = get.census.totals(ALL.DATA.MANAGERS$census.totals, location, collapse.counties = T)
    dim.names = list(year=years)

    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    present.years = dimnames(population)[['year']]

    present.and.required.years = intersect(years, present.years)
    rv[present.and.required.years] = population[present.and.required.years,]
    
    missing.years = setdiff(years, present.and.required.years)
    if (length(missing.years)>0)
    {
        closest.year = sapply(as.numeric(missing.years), function(year){
            present.years[order(abs(as.numeric(present.years)-year))][1]  
        })
        
        rv[missing.years] = population[closest.year,]
    }
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

get.simset.projection.df <- function(simset,
                                     data.type,
                                     years,
                                     denominators,
                                     dimensions=c('age','race','sex','risk'),
                                     use.cdc=T,
                                     denominator.dimensions='year',
                                     show.rates=F
)
{
    one.df = NULL
    for (i in 1:simset@n.sim)
    {
        sim = simset@simulations[[i]]
        
        values = get.sim.values(sim,
                                data.type=data.type,
                                years=years,
                                all.dimensions=c('year', dimensions),
                                use.cdc=use.cdc,
                                facet.by=NULL,
                                denominators=denominators,
                                denominator.dimensions=denominator.dimensions,
                                show.rates=show.rates)

        if (length(dimensions)==1)
        {
            dim.names = list(names(values))
            names(dim.names) = dimensions
            
            values = array(values, dim=sapply(dim.names, length), dimnames=dim.names)
            
            one.sub.df = data.frame(zz=names(values),
                                    value=values)
            names(one.sub.df)[1] = dimensions
        }
        else
            one.sub.df = melt(values)
        
        one.sub.df$simulation = i
        one.df = rbind(one.df, one.sub.df)
    }
    
    one.df
}

make.epi.data.df <- function(surv,
                             locations=get.target.msas(),
                             data.types=names(DATA.TYPE.NAMES))
{
    df = NULL
    
    for (location in locations)
    {
        for (data.type in data.types)
        {
            mask = grepl(paste0('^', data.type), names(surv))
                
            for (elem.name in names(surv[mask]))
            {
              #  print(elem.name)
                
                elem = surv[[elem.name]]
                if (length(dim(elem))==2)
                {
                    data = elem[,location]
                    dim(data) = dim(elem)[1]
                    dimnames(data) = dimnames(elem)[1]
                }
                else if (length(dim(elem))==3)
                    data = elem[,location,]
                else if (length(dim(elem))==4)
                    data = elem[,location,,]
                else if (length(dim(elem))==5)
                    data = elem[,location,,,]
                else if (length(dim(elem))==6)
                    data = elem[,location,,,,]
                else
                    stop("The elements of surv must have 2-6 dimensions")
                
                one.df = melt(data)
                one.df = one.df[!is.na(one.df$value),]
                
                if (all(names(one.df)!='sex'))
                    one.df$sex = 'all'
                if (all(names(one.df)!='risk'))
                    one.df$risk = 'all'
                if (all(names(one.df)!='race'))
                    one.df$race = 'all'
                if (all(names(one.df)!='age'))
                    one.df$age = 'all'
              
                one.df$location_code = as.character(location)
                one.df$data_type = DATA.TYPE.NAMES[data.type]
                
                if (is.null(df))
                    df = one.df
                else
                    df = rbind(df, one.df[,names(df)])
            }
        }
    }
    
    df$source = 'CDC'
    
    COLUMN.NAMES = c('location_code', 'data_type',
                     'year', 'age', 'race', 'sex', 'risk',
                     'value', 'source')
    
    df[,COLUMN.NAMES]
}

#we are assuming one level of interve,tion
make.intervention.key <- function(interventions)
{
    print("Assuming suppression, testing, and PrEP all take place on the same time frame, and that we end in 2030")
    
    rv = data.frame(id=names(interventions),
                    name=sapply(interventions, function(int){
                        if (is.null(int))
                            "No Intervention"
                        else
                            int$name
                    }),
                    target_population=sapply(interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$target.description
                    }),
                    suppressed_proportion=sapply(interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$suppressed.proportion[[1]]
                    }),
                    prep.coverage=sapply(interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$prep.coverage[[1]]
                    }),
                    testing_frequency=sapply(interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$testing.frequency[[1]]
                    }),
                    begin_implementation_year=sapply(interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$testing.years[1]-1
                    }),
                    implementation_complete_year=sapply(interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$testing.years[1]
                    }),
                    end_year=2030,
                    stringsAsFactors = F)
}

make.location.key <- function(location.codes)
{
    names.if.msa = msa.names(location.codes)
 #   names.if.state = state.name(location.codes)
    
    msa.mask = !is.na(names.if.msa)
  #  state.mask = !is.na(state)
    
    if (any(!msa.mask))
        stop("location.codes must represent MSAs")
    
    df = data.frame(code=location.codes,
                    name=as.character(names.if.msa),
                    type='MSA',
                    stringsAsFactors = F)
    
#    df$location_name[state.mask] = names.if.state[state.mask]
 #   df$location_type[state.mask] = 'state'
    
    df
}