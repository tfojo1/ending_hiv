
##-- SOURCE CODE --##
source('code/source_code.R')
source('code/Ruchita/create_prep_interventions.R')

##-- ORGANIZE the INTERVENTIONS WE CARE ABOUT --##

# RUCHITA - These should correspond to the codes you put in in 'create_prep_interventions'
ORAL.PREP.INTERVENTION.CODES = c(
    'msm.p10.oral_23_27'
)

INJ.PREP.INTERVENTION.CODES = c(
    'msm.p10.inj_23_27'
)

ALL.PREP.INTERVENTION.CODES = c(
    'noint',
    ORAL.PREP.INTERVENTION.CODES,
    INJ.PREP.INTERVENTION.CODES
)

STAGGERED.ORAL.INJ.PREP.CODES = character(2*length(ORAL.PREP.INTERVENTION.CODES))
STAGGERED.ORAL.INJ.PREP.CODES[2*(1:length(ORAL.PREP.INTERVENTION.CODES))-1] = ORAL.PREP.INTERVENTION.CODES
STAGGERED.ORAL.INJ.PREP.CODES[2*(1:length(INJ.PREP.INTERVENTION.CODES))] = INJ.PREP.INTERVENTION.CODES

##-- FUNCTION TO RUN INTERVENTIONS --##

run.prep.simulations <- function(msas=TARGET.MSAS, 
                                 intervention.codes=ALL.PREP.INTERVENTION.CODES,
                                 dst.dir = 'mcmc_runs/prep_simsets',
                                 src.dir = 'mcmc_runs/quick_simsets',
                                 run.to.year=2030,
                                 keep.years=2018:2030)
{
    for (msa in msas)
    {
        print(paste0("Running ", length(intervention.codes), " interventions for ", msa.names(msa)))
        full.filename = get.full.filename(location=msa)
        load(file.path(src.dir, full.filename))
        
        run.systematic.interventions(simset = simset,
                                     interventions = lapply(intervention.codes, intervention.from.code), 
                                     dst.dir = dst.dir, overwrite = F, compress = T, 
                                     run.to.year = run.to.year, verbose = T, 
                                     save.baseline.and.seed = F
                                     )
    }
}

##-- FUNCTIONS TO MAKE A TABLE FROM SIMULATIONS --##

make.prep.table <- function(msas=TARGET.MSAS,
                            intervention.codes,
                            comparison.codes,
                            raw.prep.results)
{
    if (length(comparison.codes)==1)
        comparison.codes = rep(comparison.codes, length(intervention.codes))
    
    rv = sapply(1:length(intervention.codes), function(i){
        sapply(1:msas, function(msa){
            int.code = intervention.codes[i]
            comp.code = comparison.codes[i]
            
            int.values = raw.prep.results[,msa,int.code]
            comp.values = raw.prep.results[,msa,comp.code]
            
            # RUCHITA
            # do some math here and format it nicely
            # We want a string that is XX% [YY% to ZZ%]
        })
    })
    
    rv
}


#returns a three-dimensional array
#indexed [simulation, msa, intervention.code]
aggregate.raw.prep.results <- function(msas=TARGET.MSAS,
                                       intervention.codes=ALL.PREP.INTERVENTION.CODES,
                                       years=2020:2030,
                                       dir='prep_simsets')
{
    rv = sapply(intervention.codes, function(code){
        sapply(msas, function(msa){
            filename = get.simset.filename(location=msa, intervention.code=code)
            load(file.path(dir, msa, filename))
            
            project.absolute.incidence(sim, keep.dimensions = NULL, years=years)
        })
    })
    
    
    dim.names = list(sim=1:(length(rv)/length(msas)/length(intervention.codes)),
                     location=msas,
                     intervention=intervention.codes)
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

