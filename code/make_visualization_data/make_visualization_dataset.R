
source('code/source_code.R')
source('code/targets/parse_targets.R')
source('code/interventions/interventions_for_simset.R')

if (1==2)
{
    ##--------------------------##
    ##-- LOAD PRE-RUN SIMSETS --##
    ##--------------------------##
    
    intervention.simsets = list()
    load('mcmc_runs/limited_simset_interventions/33100/All_MSM_or_IDU_testing_1pyr_0.8_suppressed_0.25_PrEP.Rdata')
    intervention.simsets = c(intervention.simsets, list(simset))
    load('mcmc_runs/limited_simset_interventions/33100/No_Intervention.Rdata')
    intervention.simsets = c(intervention.simsets, list(simset))
    load('mcmc_runs/limited_simset_interventions/35620//All_MSM_or_IDU_testing_1pyr_0.8_suppressed_0.25_PrEP.Rdata')
    intervention.simsets = c(intervention.simsets, list(simset))
    load('mcmc_runs/limited_simset_interventions/35620/No_Intervention.Rdata')
    intervention.simsets = c(intervention.simsets, list(simset))
    
    base.simsets = list()
    load('mcmc_runs/limited_simsets/33100.Rdata')
    base.simsets = c(base.simsets, list(simset))
    load('mcmc_runs/limited_simsets/35620.Rdata')
    base.simsets = c(base.simsets, list(simset))
    
    ##------------------------------##
    ##-- GENERATE THE DATA FRAMES --##
    ##------------------------------##
    
    strata.key = make.strata.key(intervention.simsets[[1]])
    intervention.key = make.intervention.key(intervention.simsets)
    
    #simset.df = make.multi.simset.data.frame(intervention.simsets)
    
    summarized.df = make.summarized.multi.simset.data.frame(intervention.simsets=intervention.simsets,
                                                            base.simsets=base.simsets,
                                                            strata.key=strata.key,
                                                            intervention.key=intervention.key
                                                            )
    #LOCATIONS = unique(simset.df$location_code)
    LOCATIONS = unique(sapply(base.simsets, function(ss){attr(ss@simulations[[1]], 'location')}))
        
    epi.df = make.epi.data.df(msa.surveillance, locations = LOCATIONS)
    
    location.key = make.location.key(LOCATIONS)
    
    
    ##---------------------##
    ##-- WRITE THE FILES --##
    ##---------------------##
    
    #Keys
    write.csv(strata.key.to.data.frame(strata.key), file='visualization/raw/Strata.csv', row.names=F)
    write.csv(intervention.key.to.data.frame(intervention.key), file='visualization/raw/Interventions.csv', row.names=F)
    
    write.csv(simset.df, file='visualization/raw/Simulations.csv', row.names=F)
    
    write.csv(summarized.df, file='visualization/raw/Summarized_Simulations.csv', row.names=F)
    write.csv(epi.df, file='visualization/raw/Epi_Data.csv', row.names=F)
    write.csv(location.key, file='visualization/raw/Locations.csv', row.names=F)
    
}




DATA.TYPE.NAMES = c(new='New_Diagnoses',
                    prevalence='Prevalence',
                    incidence='Incidence',
                    diagnosed='Percent_Diagnosed',
                    mortality='HIV_Mortality')



##----------------------------------##
##-- SUMMARY STATISTIC SIMSET DFs --##
##----------------------------------##

make.summarized.multi.simset.data.frame <- function(intervention.simsets,
                                                    base.simsets,
                                                    strata.key,
                                                    intervention.key,
                                                    base.years=1970:2019,
                                                    intervention.years=2020:2030,
                                                    include.mean=T,
                                                    include.median=T,
                                                    interval.coverages=c(.95,.5),
                                                    verbose=T)
{
    total.num = length(base.simsets) + length(intervention.simsets)
    
    df = NULL
    for (i in 1:length(base.simsets))
    {
        if (verbose)
            print(paste0("Rendering simset ", i, " of ", total.num))
            
        one.df = make.summarized.simset.data.frame(base.simsets[[i]],
                                                   strata.key=strata.key,
                                                   intervention.key=intervention.key,
                                                   is.base=T,
                                                   years=base.years,
                                                   include.mean=include.mean,
                                                   include.median=include.median,
                                                   interval.coverages=interval.coverages)
        df = rbind(df, one.df)
    }
    
    for (i in 1:length(intervention.simsets))
    {
        if (verbose)
            print(paste0("Rendering simset ", i+length(base.simsets), " of ", total.num))
        
        one.df = make.summarized.simset.data.frame(intervention.simsets[[i]],
                                                   strata.key=strata.key,
                                                   intervention.key=intervention.key,
                                                   is.base=F,
                                                   years=intervention.years,
                                                   include.mean=include.mean,
                                                   include.median=include.median,
                                                   interval.coverages=interval.coverages)
        df = rbind(df, one.df)
    }
    
    COLUMN.NAMES = c('intervention_id', 'location_code', 'data_type',
                     'year', 'stratum',
                     'statistic', 'value')
    
    df[,COLUMN.NAMES]
}

make.summarized.simset.data.frame <- function(simset,
                                              strata.key,
                                              intervention.key,
                                              is.base=F,
                                              years=2000:2030,
                                              data.types=c('new','prevalence','incidence','mortality','diagnosed'),
                                              include.mean=T,
                                              include.median=T,
                                              interval.coverages=c(.95,.5))
{
    years = intersect(simset@simulations[[1]]$years, years)
    
    location = attr(simset@simulations[[1]], 'location')
    population = fill.in.population(location, years)
    
    df = NULL
    dimension.combos = get.all.dimension.combinations(c('age','race','sex','risk'))
    
    for (data.type in data.types)
    {   
        for (dim.combo in dimension.combos)
        {
            sample.values = get.sim.values(simset@simulations[[1]],
                                        data.type=data.type,
                                        years=years,
                                        all.dimensions=c('year', dim.combo),
                                        use.cdc=T,
                                        facet.by=NULL,
                                        denominators=population,
                                        denominator.dimensions='year',
                                        show.rates=F)
            if (is.null(dim(sample.values)))
            {
                dim(sample.values) = c(year=length(years))
                dimnames(sample.values) = list(year=as.character(years))
            }
            
            stratification.df = melt(sample.values)
            if (is.null(stratification.df$sex) || is.null(stratification.df$risk))
                mask = T
            else
                mask = stratification.df$sex != 'female' | (stratification.df$risk != 'msm' & stratification.df$risk != 'msm_idu')
            stratification.df = stratification.df[mask,]
            strata = get.strata.for.data.frame(strata.key, stratification.df)
            
            print(paste0(data.type, ": ", paste0(dim.combo, collapse=', ')))
            
            dist = extract.simset.distribution(simset, 
                                               fn = function(sim){
                                                   get.sim.values(sim,
                                                                  data.type=data.type,
                                                                  years=years,
                                                                  all.dimensions=c('year', dim.combo),
                                                                  use.cdc=T,
                                                                  facet.by=NULL,
                                                                  denominators=population,
                                                                  denominator.dimensions='year',
                                                                  show.rates=F)[mask]
                                               })
       
            one.df = NULL
            
            if (include.mean)
                one.df = rbind(one.df,
                               data.frame(value=get.means(dist),
                                          statistic='mean',
                                          stratum=strata,
                                          year=stratification.df$year))
            
            if (include.median)
                one.df = rbind(one.df,
                               data.frame(value=get.medians(dist),
                                          statistic='median',
                                          stratum=strata,
                                          year=stratification.df$year))
                
            for (coverage in interval.coverages)
            {
                interval = get.intervals(dist, coverage=coverage)
                one.df = rbind(one.df,
                               data.frame(value=interval[1,],
                                          statistic=paste0('interval_lower_', 100*coverage),
                                          stratum=strata,
                                          year=stratification.df$year),
                               data.frame(value=interval[2,],
                                          statistic=paste0('interval_upper_', 100*coverage),
                                          stratum=strata,
                                          year=stratification.df$year)
                               )
            }
        }
        
        one.df$data_type = DATA.TYPE.NAMES[data.type]
     #   one.df = one.df[one.df$sex!='female' | (one.df$risk!='msm' & one.df$risk!='msm_idu'),]
            
        df = rbind(df, one.df)
    }
    
    df$location_code = as.character(location)
    df$intervention_id = get.intervention.code(intervention.key, attr(simset, 'intervention'), base=is.base)
    #    df$location_name = as.character(msa.names(location))
    
    
    df
}

get.all.dimension.combinations <- function(dimensions)
{
    indicators = cbind(c(F,T))
    if (length(dimensions)>1)
    {
        for (d in 2:length(dimensions))
        {
            indicators = rbind(cbind(indicators, F),
                               cbind(indicators,T))
        }
    }
    
    lapply(1:dim(indicators)[1], function(i){
        dimensions[indicators[i,]]
    })
}

##---------------------------##
##-- SIMULATION DATA FRAME --##
##---------------------------##

make.multi.simset.data.frame <- function(simsets,
                                         data.types=c('new','prevalence','incidence','mortality','diagnosed'))
{
    df = NULL
    for (i in 1:length(simsets))
    {
        one.df = make.simset.data.frame(simsets[[i]], data.types=data.types)
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
                                   years=2000:2030,
                                   data.types=c('new','prevalence','incidence','mortality','diagnosed'))
{
    years = intersect(simset@simulations[[1]]$years, years)
    
    location = attr(simset@simulations[[1]], 'location')
    population = fill.in.population(location, years)
 
    df = NULL
    for (data.type in data.types)
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
                
                if (any(location == dimnames(elem)[['location']]))
                {
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
    }
    
    df$source = 'CDC'
    
    COLUMN.NAMES = c('location_code', 'data_type',
                     'year', 'age', 'race', 'sex', 'risk',
                     'value', 'source')
    
    df[,COLUMN.NAMES]
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

##----------------------##
##-- INTERVENTION KEY --##
##----------------------##

make.intervention.key <- function(simsets)
{
    interventions = lapply(simsets, function(simset){
        attr(simset,'intervention')
    })
    interventions = interventions[!sapply(interventions, is.null)]
    intervention.names = sapply(interventions, function(int){int$name})
    
    interventions = c(list(NULL,NULL), interventions)
    intervention.names = c('base', 'No Intervention', intervention.names)
    
    intervention.codes = 0:(length(intervention.names)-1)
    names(intervention.codes) = intervention.names
    list(codes=intervention.codes,
         interventions=interventions,
         names=intervention.names)
}

get.intervention.code <- function(key, intervention, base=F)
{
    if (base)
        int.name = 'base'
    else if (is.null(intervention))
        int.name = 'No Intervention'
    else
        int.name = intervention$name
    
    key$codes[int.name]
}

intervention.key.to.data.frame <- function(key)
{
    print("Assuming suppression, testing, and PrEP all take place on the same time frame, and that we end in 2030")
    
    rv = data.frame(id=as.numeric(key$codes),
                    name=gsub(':', ' -', key$names),
                    target_population=sapply(key$interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$target.description
                    }),
                    suppressed_proportion=sapply(key$interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$suppressed.proportion[[1]]
                    }),
                    prep.coverage=sapply(key$interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$prep.coverage[[1]]
                    }),
                    testing_frequency=sapply(key$interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$testing.frequency[[1]]
                    }),
                    begin_implementation_year=sapply(key$interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$testing.years[1]-1
                    }),
                    implementation_complete_year=sapply(key$interventions, function(int){
                        if (is.null(int))
                            NA
                        else
                            int$testing.years[1]
                    }),
                    end_year=2030,
                    stringsAsFactors = F)
    
    rv
}

##----------------##
##-- STRATA KEY --##
##----------------##

make.strata.key <- function(sim)
{
    if (is(sim, 'simset'))
        sim = simset@simulations[[1]]
    
    dim.names = dimnames(extract.incidence(sim, keep.dimensions = c('age','race','sex','risk'), use.cdc.categorizations = T))
    dim.names = lapply(dim.names, function(one.dim){
        c(one.dim, 'all')
    })
    
    array(1:prod(sapply(dim.names, length)),
          dim=sapply(dim.names, length),
          dimnames=dim.names)
}

get.strata.for.data.frame <- function(key, df)
{
    sapply(1:dim(df)[1], function(i){
        get.stratum(key, 
                    age=df$age[i],
                    race=df$race[i],
                    sex=df$sex[i],
                    risk=df$risk[i])
    })
}

get.stratum <- function(key,
                        age=NULL,
                        race=NULL,
                        sex=NULL,
                        risk=NULL)
{
    if (is.null(age))
        age='all'
    if (is.null(race))
        race='all'
    if (is.null(sex))
        sex='all'
    if (is.null(risk))
        risk='all'
    
    key[age,race,sex,risk]
}

strata.key.to.data.frame <- function(key)
{
    melt(key, value.name='stratification')
}