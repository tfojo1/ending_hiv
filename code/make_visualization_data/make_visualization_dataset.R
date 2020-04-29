
source('code/source_code.R')
if (1==2)
{
source('code/interventions/interventions_for_simset.R')

load('mcmc_runs/balt63_focus.wt.4_all.x2_prev.x5.2_20000_2020-04-20.Rdata')
simset = extract.simset(mcmc, additional.burn=400, additional.thin=8)

simset = prepare.simset.for.interventions(simset)

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

simset.0 = run.simset.intervention(simset, intervention = NULL, keep.years=2018:2030)
simset.1 = run.simset.intervention(simset, intervention=intervention.1)
simset.2 = run.simset.intervention(simset, intervention=intervention.2)

df0 = make.simset.data.frame(simset.0, years = 2000:2030)
write.csv(df0, file='visualizing_datasets/simulations/12580_simulations_0.csv', row.names=F)
df1 = make.simset.data.frame(simset.1, years = 2020:2030)
write.csv(df1, file='visualizing_datasets/simulations/12580_simulations_1.csv', row.names=F)
df2 = make.simset.data.frame(simset.2, years = 2020:2030)
write.csv(df2, file='visualizing_datasets/simulations/12580_simulations_2.csv', row.names=F)


epi.df = get.epi.data.df(msa.surveillance, location='12580')
write.csv(epi.df, file='visualizing_datasets/epidemiological_data/12580_epi_data.csv', row.names=F)

intervention.key = data.frame(id=0:2,
                              description=c('No Intervention',
                                            'Target young black MSM: yearly testing, 90% suppression, 25% PrEP Coverage',
                                            'Target all MSM or IDU: yearly testing, 80% suppression, 50% PrEP Coverage'),
                              target_population=c(NA,'Young Black MSM', 'All MSM or IDU'),
                              suppressed_proportion=c('baseline','90%','80%'),
                              prep_coverage=c('baseline','25%','50%'),
                              testing_frequency=c('baseline','yearly','yearly'),
                              location_code='12580',
                              location_name=as.character(msa.names('12580')),
                              row.names = NULL)
write.csv(intervention.key, 'visualizing_datasets/intervention_key.csv', row.names = F)
}


DATA.TYPE.NAMES = c(new='New_Diagnoses',
                    prevalence='Prevalence',
                    incidence='Incidence',
                    diagnosed='Percent_Diagnosed',
                    mortality='HIV_Mortality')

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
    df$location_name = as.character(msa.names(location))
    
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
                                all.dimensions=dimensions,
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

get.epi.data.df <- function(surv,
                            location,
                            data.types=names(DATA.TYPE.NAMES))
{
    df = NULL
    mask = rep(F, length(surv))
    for (data.type in data.types)
        mask = mask | grepl(data.type, names(surv))
    
    for (elem.name in names(surv[mask]))
    {
        print(elem.name)
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
      
        if (is.null(df))
            df = one.df
        else
            df = rbind(df, one.df[,names(df)])
    }
    
    
    df$location_code = as.character(location)
    df$location_name = as.character(msa.names(location))
    
    df
}