
source('code/interventions/intervention_presets.R')

##-- PROCESS RESULTS --##
if (1==2)
{
    load('results/jenness.simsets.Rdata')
    load('mcmc_runs/full_simsets/12060/1.0_12060_noint.Rdata')
    noint=simset
    
    jeness.table.2 = cbind(Overall=c(Base='1.09 [0.84-1.40]',
                                     Annual='1.00 [0.75-1.27]',
                                     Biannual='0.99 [0.73-1.24]',
                                     Quarterly='0.99 [0.76-1.26]'),
                           Black=c(Base='2.17 [1.60-2.81]',
                                     Annual='1.96 [1.43-2.56]',
                                     Biannual='1.93 [1.41-2.47]',
                                     Quarterly='1.94 [1.47-2.51]'),
                           Hispanic=c(Base='0.58 [0.00-1.37]',
                                     Annual='0.57 [0.00-1.34]',
                                     Biannual='0.57 [0.00-1.36]',
                                     Quarterly='0.57 [0.00-1.31]'),
                           Other=c(Base='0.32 [0.18-0.49]',
                                     Annual='0.29 [0.14-0.46]',
                                     Biannual='0.29 [0.14-0.47]',
                                     Quarterly='0.29 [0.14-0.47]'),
                           )
    
    jheem.testing.dists = lapply(c(list(noint), jenness.simsets), get.simset.incidence.dist, 
                                   year=2030, per.population=100, include.hiv.positive.in.denominator=F, sex='msm')
#    sapply(jheem.testing.dists, get.means)
    
    
    jheem.black.testing.dists = lapply(c(list(noint), jenness.simsets), get.simset.incidence.dist, 
                                 year=2030, per.population=100, include.hiv.positive.in.denominator=F, sex='msm',
                                 race='black')
#    sapply(jheem.black.testing.dists, get.means)
    
    
    jheem.hispanic.testing.dists = lapply(c(list(noint), jenness.simsets), get.simset.incidence.dist, 
                                       year=2030, per.population=100, include.hiv.positive.in.denominator=F, sex='msm',
                                       race='hispanic')
#    sapply(jheem.hispanic.testing.dists, get.means)
    
    
    jheem.other.testing.dists = lapply(c(list(noint), jenness.simsets), get.simset.incidence.dist, 
                                          year=2030, per.population=100, include.hiv.positive.in.denominator=F, sex='msm',
                                          race='other')
#    sapply(jheem.other.testing.dists, get.means)

    jheem.df = cbind(Overall=sapply(jheem.testing.dists, make.dist.summary),
                     Black=sapply(jheem.black.testing.dists, make.dist.summary),
                     Hispanic=sapply(jheem.hispanic.testing.dists, make.dist.summary),
                     Other=sapply(jheem.other.testing.dists, make.dist.summary))
    
    write.csv(jheem.df, file='../Manuscripts/manuscript_1/appendix_1/scratch/jheem_for_jenness.csv')
}

make.dist.summary <- function(dist, digits=2)
{
    ci = format(get.intervals(dist), nsmall=digits, digits=digits)
    paste0(format(get.means(dist), nsmall=digits, digits=digits), 
           ' [',
           ci[1], 
           '-',
           ci[2],
           ']')
}

##-- RUN IT --##
if (1==2)
{
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
}

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