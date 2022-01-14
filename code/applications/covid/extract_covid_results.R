if (1==2)
{
    x = process.covid.outcomes(locations=c('35620','12580'), include.baseline = F, n.sim=5)
}


COVID.OUTCOMES = c(
    incidence="Incident Cases",
    new="Reported Cases",
    new.acute='Reported Acute Cases',
    prevalence.all = "Prevalent Cases",
    prevalence.diagnosed = "Prevalent Cases Aware of Diagnosis",
    suppression = "Suppressed PWH",
    population = "Population",
    testing = 'Testing Rates',
    prevalence.acute.all='Prevalent Acute Infections',
    prevalence.acute.undiagnosed = "Prevalent, Undiagnosed Acute Infections"
)

RAW.COVID.OUTCOMES = names(COVID.OUTCOMES)


COVID.DIR = file.path(SYSTEMATIC.ROOT.DIR, 'covid_simsets')
FULL.DIR = file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets')
NOINT.DIR = COVID.DIR

#returns an array indexed [location, scenario, year, outcome, simulation]
# where outcome is one of:
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
                                   scenarios=c('covid.delayed.mobility','covid.rapid.resumption.mobility'),
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
                load(file=file.path(NOINT.DIR, loc, noint.filename))
                noint = simset
                noint = flatten.simset(noint)
            }
            else
                baseline = NULL
            
            print(paste0(" - Scenario '", scenario, "'"))
            
            if (scenario=='baseline')
                simset = noint
            else
            {
                filename = get.simset.filename(location=loc, intervention.code=scenario)
                filename = file.path(COVID.DIR, loc, filename)
                if (file.exists(filename))
                    load(file=filename)
                else
                {
                    print(paste0("**WARNING: '", filename, "' does not exist. Skipping**"))
                }
            }            
            sapply(1:n.sim, function(i){
                if (is.null(simset))
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
    
    
    dim.names = list(year=as.character(years),
                     outcome = RAW.COVID.OUTCOMES,
                     sim=1:n.sim,
                     scenario=scenarios,
                     location=locations)
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    rv = apply(rv, c('location','scenario','year','outcome','sim'), function(x){x})
    
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
        rv[dimnames(sub)[['location']],,,,] = sub
    
    rv
}

load.and.merge.covid.outcomes <- function(msas=TARGET.MSAS,
                                          dir='results/covid/subs',
                                          prefix='covid_4.2')
{
    subs = lapply(msas, function(msa){
        filename = file.path(dir, paste0(prefix, '_results_', msa, '.Rdata'))
        if (file.exists(filename))
        {
            load(file=filename)
            outcomes.sub.arr
        }
        else
        {
            print(paste0("'", filename, "' does not exist - skipping"))
            NULL
        }
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
    prev.all = prev.diagnosed = new.dx = suppressed = pop = testing =
        prev.acute.all = prev.acute.undiagnosed = new.acute.dx = inc
    
    
    # Pull from sim1
    sim1.years = intersect(years, sim1$years[-1])
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
    prev.acute.all[as.character(sim1.years)] = project.absolute.prevalence(sim=sim1, keep.dimensions = 'year',
                                                                                 years=sim1.years,
                                                                                 cd4='acute')
    prev.acute.undiagnosed[as.character(sim1.years)] = project.absolute.prevalence(sim=sim1, keep.dimensions = 'year',
                                                                           years=sim1.years,
                                                                           cd4='acute',
                                                                           continuum=setdiff(sim1$continuum, sim1$diagnosed.continuum.states))
    
    new.acute.dx[as.character(sim1.years)] = project.absolute.new.diagnoses(sim=sim1, keep.dimensions = 'year',
                                                                      years=sim1.years,
                                                                      cd4='acute')
browser()    
    suppressed[as.character(sim1.years)] = extract.suppression(sim=sim1, keep.dimensions = 'year',
                                                               years=sim1.years)
    pop[as.character(sim1.years)] = get.total.population(sim=sim1, years=sim1.years)
    
    testing[as.character(sim1.years)] = extract.testing.rates(sim=sim1, keep.dimensions = 'year',
                                                              years=sim1.years)
    
    # Pull from sim2
    if (!is.null(sim2))
    {
        sim2.years = intersect(years, sim2$years[-1])
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
            if (any(sim2$cd4=='acute'))
            {
                prev.acute.all[as.character(sim2.years)] = project.absolute.prevalence(sim=sim2, keep.dimensions = 'year',
                                                                                       years=sim2.years,
                                                                                       cd4='acute')
                prev.acute.undiagnosed[as.character(sim2.years)] = project.absolute.prevalence(sim=sim2, keep.dimensions = 'year',
                                                                                             years=sim2.years,
                                                                                             cd4='acute',
                                                                                             continuum=setdiff(sim1$continuum, sim1$diagnosed.continuum.states))
                
                
                new.acute.dx[as.character(sim2.years)] = project.absolute.new.diagnoses(sim=sim2, keep.dimensions = 'year',
                                                                                        years=sim2.years,
                                                                                        cd4='acute')
            }
            suppressed[as.character(sim2.years)] = extract.suppression(sim=sim2, keep.dimensions = 'year',
                                                                       years=sim2.years)
            testing[as.character(sim2.years)] = extract.testing.rates(sim=sim2, keep.dimensions = 'year',
                                                                      years=sim2.years)
            
            pop[as.character(sim2.years)] = get.total.population(sim=sim2, years=sim2.years)
        }
    }
    
    rv = cbind(incidence=inc,
               new=new.dx,
               new.acute=new.acute.dx,
               prevalence.all=prev.all,
               prevalence.diagnosed=prev.diagnosed,
               suppression=suppressed*prev.diagnosed,
               population=pop,
               testing=testing,
               prevalence.acute.all=prev.acute.all,
               prevalence.acute.undiagnosed=prev.acute.undiagnosed)
    rv = rv[,RAW.COVID.OUTCOMES]
    
    rv
}
