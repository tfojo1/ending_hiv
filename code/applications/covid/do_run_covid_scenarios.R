
source('code/source_code.R')
source('code/core_code/data_managers/mobility_data_manager.R')
source('code/applications/covid/covid_interventions.R')
source('code/applications/covid/create_covid_scenarios.R')
load('cached/MOBILITY.DATA.MANAGER.Rdata')


SCENARIOS.TO.RUN = list(
    DELAYED.CARE.MOBILITY,
    RAPID.RESUMPTION.MOBILITY
)

<<<<<<< HEAD
locations = TARGET.MSAS[(8-1)*4 + 1:4] #c(BALTIMORE.MSA, NYC.MSA)

test = F
=======

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
>>>>>>> cd653bf0c14570643f9d595cd2a7ccacae9b37f3

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
                                 dst.dir = file.path(SYSTEMATIC.ROOT.DIR, 'covid_simset'),
                                 interventions = SCENARIOS.TO.RUN)
}