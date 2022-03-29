


source('code/source_code.R')
source('code/applications/prep/lai_prep_parameters.R')
source('code/applications/prep/standard_lai_prep_interventions.R')
#source('code/applications/prep/create_prep_study_comparisons.R')

RUN = T
PROCESS = F

base.index = 1
n.chunks = 4
n.per = ceiling(length(TARGET.MSAS) / n.chunks)
indices = intersect((1:n.per-1)*n.chunks + base.index, 1:length(TARGET.MSAS))
MSAs = TARGET.MSAS[indices]

INTERVENTIONS.TO.RUN = STANDARD.LAI.PREP.INTERVENTIONS
print("**just running the last intervention")
INTERVENTIONS.TO.RUN = STANDARD.LAI.PREP.INTERVENTIONS[length(STANDARD.LAI.PREP.INTERVENTION.CODES)]

dst.dir = file.path(SYSTEMATIC.ROOT.DIR, 'prep_simsets')
src.dir = file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets')
run.from.year=2014
run.to.year=2030
keep.years=2014:2030

# IF JUST DOING THE COMPARISONS

#print("JUST DOING COMPARISONS")
#INTERVENTION.CODES.TO.RUN = 'baseline.maloney' #sapply(COMPARISON.INTERVENTIONS.TO.RUN, get.intervention.code)
#MSAs = ATLANTA.MSA



if (RUN)
{
    for (msa in MSAs)
    {
        print(paste0("Running ", length(INTERVENTIONS.TO.RUN), " interventions for ", msa.names(msa)))
        full.filename = get.full.filename(location=msa)
        load(file.path(src.dir, full.filename))
        simset = flatten.simset(simset)
        
        run.systematic.interventions(simset = simset,
                                     interventions = INTERVENTIONS.TO.RUN, 
                                     dst.dir = dst.dir, overwrite = F, compress = T, 
                                     run.from.year = run.from.year,
                                     run.to.year = run.to.year, verbose = T, 
                                     save.baseline.and.seed = F
        )
    }
}

if (PROCESS)
{
    sub.results = aggregate.raw.prep.results(msas=MSAs,
                                             intervention.codes = INTERVENTION.CODES.TO.RUN,
                                             years=2020:2030,
                                             dir='Q:/Ending_HIV/mcmc_runs/prep_simsets',
                                             calculate.total=F)
    save(sub.results, file=paste0('results/prep/subs/prep_sub_', base.index, '.Rdata'))
}

if (1==2)
{
    prep.results = aggregate.raw.prep.results(msas=TARGET.MSAS,
                                              intervention.codes = INTERVENTION.CODES.TO.RUN,
                                              years=2020:2030,
                                              dir='Q:/Ending_HIV/mcmc_runs/prep_simsets',
                                              calculate.total=F)
    
    
    prep.results = NULL
    for (i in 1:6)
    {
       load(file=paste0('results/prep/subs/prep_sub_', i, '.Rdata'))
       if (is.null(prep.results))
       {
          dim.names = dimnames(sub.results)
          dim.names$location = TARGET.MSAS
          prep.results = array(0, dim=sapply(dim.names, length), dimnames = dim.names)
       }
       
       prep.results[,,dimnames(sub.results)$location,] = sub.results
    }
    
    
    
    
    load('Q:/Ending_HIV/mcmc_runs/prep_simsets/12580/1.0_12580_msm.baseline.oral_23_27.Rdata')
    parameters = simset@parameters[,simset@n.parameters-0:3]
    
    save(prep.results, parameters, file=paste0('results/prep/baseline_prep_and_parameters_1K_',Sys.Date(),'.Rdata'))
}