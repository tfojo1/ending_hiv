
source('code/calibration/calibrated_parameters_expanded_1.R')
mcmc = assemble.mcmc.from.cache('Q:/Ending_HIV/mcmc_runs/systematic_caches_expanded/12580_1x20K_expanded_2021-10-30',T)
simset = extract.simset(mcmc, additional.burn=250, additional.thin=5)
simset@n.sim
plot.calibration.total(simset, data.types=c('engagement','suppression','suppression.of.engaged'), years=2011:2030)
save(simset, file='mcmc_runs/baltimore_initial_simset_v4.Rdata')

trace.plot(mcmc, '*proportion.lost.or')
trace.plot(mcmc, '*proportion.lost.slope.or')

base = prepare.simset.for.interventions(simset)

noint = run.simset.intervention(base, NO.INTERVENTION)

plot.calibration.total(noint, data.types=c('engagement','suppression','suppression.of.engaged'), years=2011:2030)


base2 = thin.simset(base, 5)


noint = run.simset.intervention(base2, NO.INTERVENTION)
#simset.wr80 = run.simset.intervention(base2, WHOLEPOP.R80)
#simset.wr90 = run.simset.intervention(base2, WHOLEPOP.R90)
simset.wr95 = run.simset.intervention(base2, WHOLEPOP.R95)

plot.calibration.total(list(noint,simset.wr95), 
                       data.types=c('engagement','suppression','suppression.of.engaged'), years=2020:2030,
                       plot.individual.simset.sims = F)
plot.calibration.total(list(noint,simset.wr95), 
                       data.types=c('incidence'), years=2020:2030,
                       plot.individual.simset.sims = F)

plot.calibration(list(noint,simset.wr95), 
                 facet.by='age',
                       data.types=c('engagement'), years=2020:2030,
                       plot.individual.simset.sims = F)


inc.red = function(simset)
{
    sapply(simset@simulations, function(sim){
        inc = project.absolute.incidence(sim, years=c(2020,2030))
        (inc[1]-inc[2])/inc[1]
    })
}

mean(inc.red(noint))
mean(inc.red(simset.wr95))

simset.as95 = run.simset.intervention(base2, WHOLEPOP.AS95)
plot.calibration.total(list(noint,simset.as95), 
                       data.types=c('engagement','suppression','suppression.of.engaged','incidence'), years=2020:2030,
                       plot.individual.simset.sims = F)


mean(inc.red(simset.as95))

simset.l95 = run.simset.intervention(base2, WHOLEPOP.L95)
plot.calibration.total(list(noint,simset.l95), 
                       data.types=c('engagement','suppression','linkage','incidence'), years=2020:2030,
                       plot.individual.simset.sims = F)
extract.linkage(simset.as95@simulations[[1]])

mean(inc.red(simset.l95))

sim = simset.wr95@simulations[[1]]
components = attr(sim, 'components')

x = calculate.unsuppressed.to.disengaged.rates(components)
