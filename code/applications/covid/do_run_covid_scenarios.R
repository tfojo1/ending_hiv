
source('code/source_code.R')
source('code/core_code/data_managers/mobility_data_manager.R')
source('code/applications/covid/covid_interventions.R')
source('code/applications/covid/create_covid_scenarios.R')
load('cached/MOBILITY.DATA.MANAGER.Rdata')


SCENARIOS.TO.RUN = list(
    DELAYED.CARE.MOBILITY,
    RAPID.RESUMPTION.MOBILITY,
    NO.INTERVENTION #this is in here so we can get the stratified acute vs chronic
)


locations = DENVER.MSA #TARGET.MSAS[(8-1)*4 + 1:4] #c(BALTIMORE.MSA, NYC.MSA)

test = F
run = T
process = F

if (run)
{
    for (i in 1:length(locations))
    {
        loc = locations[i]
        print(paste0("RUNNING ", length(SCENARIOS.TO.RUN), " SCENARIO(s) FOR ", loc, " (", i, " of ", length(locations), ")"))
        
        if (test)
            dir = file.path(SYSTEMATIC.ROOT.DIR, 'quick_simsets')
        else
            dir = file.path(SYSTEMATIC.ROOT.DIR, 'full_simsets')
        
        file = file.path(dir, paste0('1.0_', loc, '_full.Rdata'))
        
        load(file)
        if (test)
            simset = subset.simset(simset, 1:5)
        
        run.systematic.interventions(simset,
                                     dst.dir = file.path(SYSTEMATIC.ROOT.DIR, 'covid_simsets'),
                                     interventions = SCENARIOS.TO.RUN,
                                     compress = T,
                                     pare.components = T,
                                     compress.cd4=F)
    }
}

if (process)
{
    source('code/applications/covid/extract_covid_results.R')
    for (loc in locations)
    {
        print(paste0("Processing ", loc))
        outcomes.sub.arr = process.covid.outcomes(locations=loc)
        save(outcomes.sub.arr, file = paste0('results/covid/subs/covid_4.2_results_',loc,'.Rdata'))
    }
    
}

#for processing them after
if (1==2)
{
    source('code/source_code.R')
    source('code/applications/covid/extract_covid_results.R')
    outcomes.arr = load.and.merge.covid.outcomes()
    
    location.names = get.location.name(TARGET.MSAS)
    names(location.names) = TARGET.MSAS
    
    load(file.path(SYSTEMATIC.ROOT.DIR, 'covid_simsets/12580/1.0_12580_covid.delayed.mobility.Rdata'))
    parameters = simset@parameters[,c('sexual.transmission.reduction','suppression.reduction','testing.reduction','prep.reduction')]
    names(dimnames(parameters)) = c('simulation','variable')
    
    save(outcomes.arr, parameters, location.names, file='results/covid/covid_4.2_results.Rdata')
}

