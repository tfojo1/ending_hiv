


source('code/source_code.R')
source('code/applications/prep/lai_prep_parameters.R')
source('code/applications/prep/standard_lai_prep_interventions.R')
source('code/applications/prep/create_prep_study_comparisons.R')

RUN = T
PROCESS = F

MSAs = ATLANTA.MSA

INTERVENTIONS.TO.RUN = COMPARISON.INTERVENTIONS.TO.RUN

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
