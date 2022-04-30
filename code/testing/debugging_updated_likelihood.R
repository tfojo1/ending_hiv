

source('code/source_code.R')
source('code/processing/visualization/sim_plots.R')

msa = BALTIMORE.MSA
version = 'expanded_1.0'

#-- Get Start Values --#

prior = get.parameters.prior.for.version(VERSION.MANAGER, version=version)

prior.simset.filename = get.full.filename(location=msa)
prior.run.file = file.path(SYSTEMATIC.ROOT.DIR, 'quick_simsets', prior.simset.filename)
if (!file.exists(prior.run.file))
    prior.run.file = file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets', prior.simset.filename)
if (file.exists(prior.run.file))
{
    load(prior.run.file)
    starting.parameters = simset@parameters[simset@n.sim,]
}
if (!file.exists(prior.run.file))
{
    start.value.file = file.path(SYSTEMATIC.ROOT.DIR, 'start_values', paste0(msa, '.Rdata'))
    if (!file.exists(start.value.file))
        stop(paste0("Initial values have not been set for ", msa.names(msa)))
    
    load(start.value.file)
}


# If the starting parameters don't match what we need for the version
#  start at the median values
starting.parameters.to.use = suppressWarnings(get.medians(prior))
matching.names = intersect(names(starting.parameters), names(starting.parameters.to.use))
starting.parameters.to.use[matching.names] = starting.parameters[matching.names]

#-- Run Sim --#
run.simulation = create.run.simulation.function(msa, starting.parameters.to.use,
                                                version='expanded_1.0', catch.errors = F)

sim = run.simulation(starting.parameters.to.use)

#-- Likelihood --#
likelihood = create.msa.likelihood(msa,
                                   include.engagement=T, include.linkage=T)
likelihood(sim)
