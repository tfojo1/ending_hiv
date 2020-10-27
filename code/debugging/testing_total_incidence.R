
load('mcmc_runs/systematic_initial/35620_1x20K_2020-10-02.Rdata')
load('mcmc_runs/systematic_initial/33100_1x20K_2020-10-02.Rdata')
load('mcmc_runs/systematic_initial/31080_1x20K_2020-10-02.Rdata')
sim = mcmc@simulations[[length(mcmc@simulations)]]

cen.pop = get.census.totals(ALL.DATA.MANAGERS$census.totals, location = attr(sim, 'location'), flatten.single.dim.array = T)
sim.pop = extract.population.subset(sim, keep.dimensions = c('year'), years=names(cen.pop))

ref.year = '2007'

years = as.numeric(names(cen.pop))
qplot(c(years, years),
      c(cen.pop/cen.pop[ref.year], sim.pop/sim.pop[ref.year]),
      color=c(rep('census',length(cen.pop)), rep('sim', length(sim.pop))))
