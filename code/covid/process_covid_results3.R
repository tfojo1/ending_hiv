
source('code/covid/prepare_and_run_covid_sims.R')
source('code/covid/prepare_covid_parameters.R')
if (1==2)
{
    source('code/source_code.R')
    source('code/targets/target_msas.R')
    
    INDICES.TO.DO = 1 + 8*0:3
    subs = lapply(1:length(INDICES.TO.DO), function(i){
        print(paste0("DOING '", TARGET.MSAS[INDICES.TO.DO[i]], "' - ", i, " OF ", length(INDICES.TO.DO)))
        outcomes.sub.arr = process.covid.outcomes(locations=TARGET.MSAS[INDICES.TO.DO[i]])
        save(outcomes.sub.arr, file=paste0('results/covid/covid_4.1_results_sub', INDICES.to.DO[i], '.Rdata'))
        outcomes.sub.arr
    })
    
    outcomes.arr = load.and.merge.covid.outcomes(prefix='covid_4.1')
    parameters = generate.covid.parameters(N=1000)
    location.names = unlist(msa.names(TARGET.MSAS))
    
    
    save(outcomes.arr, parameters, location.names, file='results/covid/covid_4.0_results.Rdata')
    
    nyc.means = apply(outcomes.arr[2,,1,,'prevalence.diagnosed',], c('scenario','year'), mean)
    nyc.means = apply(outcomes.arr[2,,1,,'suppression',]/outcomes.arr[2,,1,,'prevalence.diagnosed',], c('scenario','year'), mean)
    nyc.means = apply(outcomes.arr[2,,1,,'prevalence.diagnosed',]/outcomes.arr[2,,1,,'prevalence.all',], c('scenario','year'), mean)
    nyc.df = melt(nyc.means)
    ggplot(nyc.df, aes(x=year, y=value, color=scenario)) + geom_line(size=2)
}


FULL.DIR = 'mcmc_runs/full_simsets'
COVID.DIR = 'mcmc_runs/covid_simsets'


#returns an array indexed [location, scenario, intervention, year, metric, simulation]
# where metric is one of:
# - incidence (n)
# - new (n)
# - prevalence.diagnosed (n)
# - prevalence.undiagnosed (n)
# - population (n)
# - suppression (n)
# -
# - incidence_rate
# - new_rate
# - prevalence_rate
# - suppressed_proportion
# - diagnosed_proportion
# -
# - sex_reduction
# - sex_rebound
# - testing_reduction
# - prep_reduction
# - suppression_reduction
# 
process.covid.outcomes <- function(locations=TARGET.MSAS,
                                   scenarios=c('base','delayed.hiv.care','rebound.sexual.transmission','rebound.sex.delayed.hiv.care')[c(1,2,4)],
                                   intervention.names = NA,#c(NA, 'testing.50.6.6'),
                                   include.baseline=T,
                                   years=2010:2030,
                                   version=COVID.VERSION,
                                   n.sim=1000)
{
    if (include.baseline)
        scenarios = c('baseline', scenarios)
    
    rv = sapply(locations, function(loc){
        print(paste0("Reading from ", loc))
        sapply(scenarios, function(scenario){
            
            if (scenario=='baseline')
            {
                baseline.filename = get.full.filename(loc)
                load(file=file.path(FULL.DIR, baseline.filename))
                baseline = simset
                baseline = flatten.simset(baseline)
                
                noint.filename = get.simset.filename(location=loc, intervention=NO.INTERVENTION)
                load(file=file.path(FULL.DIR, loc, noint.filename))
                noint = simset
                noint = flatten.simset(noint)
            }
            else
                baseline = NULL

            sapply(intervention.names, function(int.name){
                if (is.na(int.name))
                    print(paste0(" - Scenario '", scenario, "' (no secondary intervention)"))
                else
                    print(paste0(" - Scenario '", scenario, "' with secondary intervention '", int.name, "'"))
                
                if (scenario=='baseline')
                    simset = noint
                else
                {
                    filename = get.covid.simset.name(loc, version, 
                                                     scenario=scenario, 
                                                     intervention.name = int.name)
                    filename = file.path(COVID.DIR, loc, filename)
                    if (file.exists(filename))
                        load(file=filename)
                    else
                    {
                        print(paste0("**WARNING: '", filename, "' does not exist. Skipping**"))
                    }
                }            
                
                sapply(1:n.sim, function(i){
                    if (is.null(simset) || 
                        (scenario=='baseline' && !is.na(int.name)))
                        matrix(NaN, nrow=length(years), ncol=length(RAW.COVID.OUTCOMES))
                    else
                    {
                        if (is.null(baseline))
                            sim2 = NULL
                        else
                            sim2 = baseline@simulations[[i]]
                        extract.covid.results(sim1=simset@simulations[[i]],
                                                sim2=sim2,
                                                years=years)
                    }
                })
            })
        })
    })
    
    
    intervention.names[is.na(intervention.names)] = 'none'
    dim.names = list(year=as.character(years),
                     outcome = RAW.COVID.OUTCOMES,
                     sim=1:n.sim,
                     intervention=intervention.names,
                     scenario=scenarios,
                     location=locations)

    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    rv = apply(rv, c('location','scenario','intervention','year','outcome','sim'), function(x){x})
    
    rv
}

merge.covid.outcomes <- function(sub.list)
{
    locations = unlist(sapply(sub.list, function(sub){
        dimnames(sub)[['location']]
    }))
    
    dim.names = dimnames(sub.list[[1]])
    dim.names[['location']] = locations
    rv = array(NaN, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (sub in sub.list)
        rv[dimnames(sub)[['location']],,,,,] = sub
    
    rv
}

load.and.merge.covid.outcomes <- function(msas=TARGET.MSAS,
                                          dir='results/covid/subs',
                                          prefix='covid_4.1')
{
    subs = lapply(msas, function(msa){
        filename = file.path(dir, paste0(prefix, '_results_', msa, '.Rdata'))
        if (file.exists(filename))
        {
            load(file=filename)
            outcomes.sub.arr
        }
        else
            NULL
    })
    subs = subs[!sapply(subs, is.null)]
    
    merge.covid.outcomes(subs)
}

extract.covid.results <- function(sim1,
                                  sim2,
                                  years)
{
    # Set up data structures
    inc = rep(NaN, length(years))
    names(inc) = as.character(years)
    prev.all = prev.diagnosed = new.dx = suppressed = pop = inc
        
    
    # Pull from sim1
    sim1.years = intersect(years, sim1$years)
    if (length(sim1.years) == 0)
        stop("The simulation does not encompass any of the requested years")
    
    inc[as.character(sim1.years)] = project.absolute.incidence(sim=sim1, keep.dimensions = 'year',
                                                               years=sim1.years)
    new.dx[as.character(sim1.years)] = project.absolute.new.diagnoses(sim=sim1, keep.dimensions = 'year',
                                                               years=sim1.years)
    prev.all[as.character(sim1.years)] = project.absolute.prevalence(sim=sim1, keep.dimensions = 'year',
                                                               years=sim1.years)
    prev.diagnosed[as.character(sim1.years)] = project.absolute.prevalence(sim=sim1, keep.dimensions = 'year',
                                                                     years=sim1.years,
                                                                     continuum=sim1$diagnosed.continuum.states)
    suppressed[as.character(sim1.years)] = extract.suppression(sim=sim1, keep.dimensions = 'year',
                                                               years=sim1.years)
    pop[as.character(sim1.years)] = get.total.population(sim=sim1, years=sim1.years)
    
    # Pull from sim2
    if (!is.null(sim2))
    {
        sim2.years = intersect(years, sim1$years)
        sim2.years = setdiff(sim2.years, sim1.years)
        if (length(sim2.years)>0)
        {
            
            inc[as.character(sim2.years)] = project.absolute.incidence(sim=sim2, keep.dimensions = 'year',
                                                                       years=sim2.years)
            new.dx[as.character(sim2.years)] = project.absolute.new.diagnoses(sim=sim2, keep.dimensions = 'year',
                                                                              years=sim2.years)
            prev.all[as.character(sim2.years)] = project.absolute.prevalence(sim=sim2, keep.dimensions = 'year',
                                                                             years=sim2.years)
            prev.diagnosed[as.character(sim2.years)] = project.absolute.prevalence(sim=sim2, keep.dimensions = 'year',
                                                                                   years=sim2.years,
                                                                                   continuum=sim2$diagnosed.continuum.states)
            suppressed[as.character(sim2.years)] = extract.suppression(sim=sim2, keep.dimensions = 'year',
                                                                       years=sim2.years)
            pop[as.character(sim2.years)] = get.total.population(sim=sim2, years=sim2.years)
        }
    }
    
    rv = cbind(incidence=inc,
               new=new.dx,
               prevalence.all=prev.all,
               prevalence.diagnosed=prev.diagnosed,
               suppression=suppressed*prev.diagnosed,
               population=pop)
    rv = rv[,RAW.COVID.OUTCOMES]
    
    rv
}

COVID.OUTCOMES = c(
    incidence="Incident Cases",
    new="Reported Cases",
    prevalence.all = "Prevalent Cases",
    prevalence.diagnosed = "Prevalent Cases Aware of Diagnosis",
    suppression = "Suppressed PWH",
    population = "Population"
)

RAW.COVID.OUTCOMES = names(COVID.OUTCOMES)[1:6]

