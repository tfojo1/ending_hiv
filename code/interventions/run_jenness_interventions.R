
source('code/interventions/intervention_presets.R')

ALL.MSM = create.target.population(sexes='msm')
BLACK.MSM = create.target.population(sexes='msm', races='black')

TESTING.ANNUALLY = create.intervention.unit('testing', 2020, 1, 2021)
TESTING.BIANNUALLY = create.intervention.unit('testing', 2020, 2, 2021)
TESTING.QUARTERLY = create.intervention.unit('testing', 2020, 4, 2021)

JENNESS.TESTING.ALL.MSM.ANNUALLY = create.intervention(ALL.MSM, TESTING.ANNUALLY)
JENNESS.TESTING.ALL.MSM.BIANNUALLY = create.intervention(ALL.MSM, TESTING.BIANNUALLY)
JENNESS.TESTING.ALL.MSM.QUARTERLY = create.intervention(ALL.MSM, TESTING.QUARTERLY)

JENNESS.TESTING.BLACK.MSM.ANNUALLY = create.intervention(BLACK.MSM, TESTING.ANNUALLY)
JENNESS.TESTING.BLACK.MSM.BIANNUALLY = create.intervention(BLACK.MSM, TESTING.BIANNUALLY)
JENNESS.TESTING.BLACK.MSM.QUARTERLY = create.intervention(BLACK.MSM, TESTING.QUARTERLY)

JENNESS.INTERVENTIONS = list(JENNESS.TESTING.ALL.MSM.ANNUALLY,
                             JENNESS.TESTING.ALL.MSM.BIANNUALLY,
                             JENNESS.TESTING.ALL.MSM.QUARTERLY,
                             JENNESS.TESTING.BLACK.MSM.ANNUALLY,
                             JENNESS.TESTING.BLACK.MSM.BIANNUALLY,
                             JENNESS.TESTING.BLACK.MSM.QUARTERLY)

load('mcmc_runs/full_simsets/1.0_12060_full.Rdata')
simset = prepare.simset.for.interventions(simset)
jenness.simsets = lapply(1:length(JENNESS.INTERVENTIONS), function(i){
    print(paste0("Running Intervention ", i, " of ", length(JENNESS.INTERVENTIONS)))
    run.simset.intervention(simset, JENNESS.INTERVENTIONS[[i]])
})
save(jenness.simsets, file='results/jenness.simsets.Rdata')

load('mcmc_runs/full_simsets/12060/1.0_12060_noint.Rdata')
noint = simset

pias = sapply(jenness.simsets, get.infections.averted, reference.simset=noint, sexes='msm')
cbind(jenness=c(.092,
                .112,
                .122,
                .084,
                .101,
                .114), #from table 19 of the appendix),
      jheem=pias)


get.inc.dist = function(simset){extract.simset.distribution(simset, extract.incidence, include.hiv.positive.in.denominator = F, years=2030, sex='msm', per.population=100, keep.dimensions=character())}
base.inc.dist = get.inc.dist(noint); get.means(base.inc.dist)
jenness.inc.dists = lapply(jenness.simsets, get.inc.dist)
sapply(jenness.inc.dists, get.means)

if (1==2)
{
JENNESS.REDUCTION.TESTING.Q6MO = (1.09-0.99)/1.09; JENNESS.REDUCTION.TESTING.Q6MO
load('mcmc_runs/full_simsets/12060/1.0_12060_mi.tq6m.ybh.tq6m.3y.Rdata')
source('code/interventions/synthesize_interventions.R')

dist = extract.simset.distribution(simset, function(sim){
    inc=do.extract.incidence(sim, years=c(2020,2030), sex='msm',
                         per.population = NA, keep.dimensions = 'year')
    (inc[1]-inc[2])/inc[1]
})
get.means(dist)
}