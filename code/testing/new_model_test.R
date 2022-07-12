
#-- TEST ZERO: Can we source? --#

source('code/source_code.R')

#-- FIRST TEST: Run previously established components both ways, collapsed_1.0 --#

load('../jheem_interactive/shiny/sim_cache/1.0_12060_baseline.Rdata')
comps = attr(simset@simulations[[1]], 'components')
params = simset@parameters[1,]


c.old = comps


#-- TESTING --#
c.new = comps
c.new$background.testing = NULL
c.new = do.setup.background(c.new,
                            type='testing',
                            location=attr(simset@simulations[[1]], 'location'),
                            years=c.old$background.testing$years,
                            extra.slope.after.year=2020,
                            continuum.manager=ALL.DATA.MANAGERS$continuum,
                            prep.manager=ALL.DATA.MANAGERS$prep)
comps.fn = get.components.function.for.version('collapsed_1.0')
c.new = comps.fn(parameters = params, components = c.new, data.managers = ALL.DATA.MANAGERS)
c.new = do.calculate.rates(c.new, type='testing')


length(c.old$testing.rates.and.times$times) == length(c.new$testing.rates.and.times$times) &&
    all(c.old$testing.rates.and.times$times==c.new$testing.rates.and.times$times) &&
    all(sapply(1:length(c.old$testing.rates.and.times$times), function(i){
        
        r.new = c.new$testing.rates.and.times$rates[[i]]
        r.old = c.old$testing.rates.and.times$rates[[i]][,,,,,1,,]
        dim(r.old) = dim(r.new)
        dimnames(r.old) = dimnames(r.new)
        
        length(r.old) == length(r.new) &&
            all(abs(r.old-r.new)<.00001)
    }))

#-- Suppression --#
c.new = comps

c.new$background.testing = NULL
c.new = do.setup.background(c.new,
                            type='testing',
                            location=attr(simset@simulations[[1]], 'location'),
                            years=c.old$background.testing$years,
                            extra.slope.after.year=2020,
                            continuum.manager=ALL.DATA.MANAGERS$continuum,
                            prep.manager=ALL.DATA.MANAGERS$prep)
c.new$background.suppression = NULL

c.new = setup.background.suppression(c.new,
                                     continuum.manager=ALL.DATA.MANAGERS$continuum,
                                     location=attr(simset@simulations[[1]], 'location'),
                                     years=comps$background.suppression$years)

comps.fn = get.components.function.for.version('collapsed_1.0')
c.new = comps.fn(parameters = params, components = c.new, data.managers = ALL.DATA.MANAGERS)
c.new = do.calculate.suppression(c.new)


length(c.old$testing.rates.and.times$times) == length(c.new$testing.rates.and.times$times) &&
    all(c.old$testing.rates.and.times$times==c.new$testing.rates.and.times$times) &&
    all(sapply(1:length(c.old$testing.rates.and.times$times), function(i){
        
        r.new = c.new$testing.rates.and.times$rates[[i]]
        r.old = c.old$testing.rates.and.times$rates[[i]][,,,,,1,,]
        dim(r.old) = dim(r.new)
        dimnames(r.old) = dimnames(r.new)
        
        length(r.old) == length(r.new) &&
            all(abs(r.old-r.new)<.00001)
    }))

#-- Run Sim --#
run.simulation = create.run.simulation.function(attr(simset@simulations[[1]], 'location'),
                                                version = 'collapsed_1.0',
                                                start.values = params
                                                )
sim = run.simulation(params)
c2 = attr(sim, 'components')
source('code/processing/visualization/sim_plots.R')
simplot(sim, simset@simulations[[1]])
simplot(sim, simset@simulations[[1]], data.types = 'suppression')




#-- Run Sim Expanded --#

source('code/source_code.R')
load('mcmc_runs/laart_test/12580.Rdata')
params = simset@parameters[1,]

to.redo.names = names(params)[grepl('suppressed.vs.failing.proportion', names(params))]
to.redo.values = params[to.redo.names]
params = params[setdiff(names(params), to.redo.names)]
to.redo.recent = to.redo.values
names(to.redo.recent) = paste0('recently.', to.redo.names)
to.redo.durable = to.redo.values
names(to.redo.durable) = paste0('durably.', to.redo.names)
params = c(params, to.redo.recent, to.redo.durable)


run.simulation = create.run.simulation.function(attr(simset@simulations[[1]], 'location'),
                                                version = 'expanded_1.0',
                                                start.values = params,
                                                catch.errors = F
)
sim = run.simulation(params)
source('code/processing/visualization/sim_plots.R')
simplot(sim, simset@simulations[[1]])
simplot(sim, simset@simulations[[1]], data.types = 'suppression')

