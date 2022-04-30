
source('code/source_code.R')
source('code/calibration/parameter_mappings/calibrated_parameters_expanded_2.R')



load('mcmc_runs/start_values/12580.Rdata')

pp1 = suppressWarnings(get.medians(parameters.prior))
matching.names = intersect(names(pp1), names(starting.parameters))
pp1[matching.names] = starting.parameters[matching.names]
run.simulation = create.run.simulation.function(BALTIMORE.MSA,
                                                start.values=pp1,
                                                version='expanded_1.0', 
                                                catch.errors = F)


sim1 = run.simulation(pp1)

source('code/processing/visualization/sim_plots.R')
simplot(sim1)
simplot(sim1, data.types = c('suppression','engagement','suppression.of.engaged'))


# @Melissa, play with parameters here
pp2 = pp1
pp2['already.lost.vs.failing.proportion.lost.or'] = 4
sim2 = run.simulation(pp2)
simplot(sim1, sim2, data.types = c('suppression','engagement','suppression.of.engaged'))



# the parameters that are specific to your model are:
print(setdiff(get.parameters.prior.for.version(VERSION.MANAGER, 'expanded_1.0')@var.names,
              get.parameters.prior.for.version(VERSION.MANAGER, 'collapsed_1.0')@var.names))



extract.engaged.proportions <- function(sim,
                                        years=2010:2020,
                                        states=c('engaged_unsuppressed_naive',
                                                 'engaged_unsuppressed_failing',
                                                 'engaged_suppressed'))
{
    pops = extract.population.subset(sim, years=years,
                                     continuum = states,
                                     include.hiv.negative = F,
                                     keep.dimensions = c('year','continuum'))
    
    frac = pops / rowSums(pops)
    
    df = reshape2::melt(frac)
}

library(scales)
plot.engaged.proportions <- function(...,
                                     years=2010:2020)
{
    df = NULL
    sims = list(...)
    names(sims) = paste0('sim', 1:length(sims))
    for (i in 1:length(sims))
    {
        sim = sims[[i]]
        one.df = extract.engaged.proportions(sim, years=years)
        one.df$sim = names(sims)[i]
        df = rbind(df,
                   one.df)
    }
    
    ggplot(df, aes(year, value, color=continuum, linetype=sim)) +
        geom_line(size=1) +
        scale_y_continuous(labels=percent, limits = c(0,1))
}

plot.engaged.proportions(sim1, sim2)
plot.engaged.proportions(sim1, sim2, years=1990:2010)

