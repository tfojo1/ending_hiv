
##-- SOURCE CODE --##
source('code/source_code.R')
source('code/Ruchita/create_prep_interventions.R')

##-- ORGANIZE the INTERVENTIONS WE CARE ABOUT --##

# RUCHITA - These should correspond to the codes you put in in 'create_prep_interventions'
ORAL.PREP.INTERVENTION.CODES = c(
    'msm.p10.oral_23_27'
    # RUCHITA - add other codes here
)

INJ.PREP.INTERVENTION.CODES = c(
    'msm.p10.inj_23_27'
    # RUCHITA - and here
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


# Returns a data frame
# One row for each msa (plus a row for total if include.total=T)
# One column for each pair of intervention.code[i] x comparison.code[i]
make.prep.table <- function(msas=TARGET.MSAS,
                            intervention.codes,
                            comparison.codes, #could be null
                            raw.prep.results,
                            include.totals=T,
                            round.digits=0,
                            stat=c('abs.diff','relative.diff')[1])
{
    if (include.totals)
        msas = c(msas, 'total')
    
    # if there is only one comparison intervention, use that for all intervnetion.codes
    if (length(comparison.codes)==1)
        comparison.codes = rep(comparison.codes, length(intervention.codes))
    
    rv = sapply(1:length(intervention.codes), function(i){
        sapply(1:msas, function(msa){
            int.code = intervention.codes[i]
            int.values = raw.prep.results[,msa,int.code]
            
            if (is.null(comparison.codes))
                comp.values = rep(0, length(int.values))
            else
            {            
                comp.code = comparison.codes[i]
                comp.values = raw.prep.results[,msa,comp.code]
            }
            
            # RUCHITA
            # do some math here and format it nicely
            # We want a string that is XX% [YY% to ZZ%]
            if (stat='abs.diff')
                diff = int.values - comp.values
            else
                diff = (int.values-comp.values) / comp.values
            
            quantile(diff, probs=.025)
            
            format(10000, big.mark=',')
        })
    })
    
    as.data.frame(rv)
}


#returns a three-dimensional array
#indexed [simulation, msa, intervention.code]
aggregate.raw.prep.results <- function(msas=TARGET.MSAS,
                                       intervention.codes=ALL.PREP.INTERVENTION.CODES,
                                       years=2020:2030,
                                       dir='prep_simsets',
                                       calculate.total=T)
{
    rv = sapply(intervention.codes, function(code){
        sapply(msas, function(msa){
            filename = get.simset.filename(location=msa, intervention.code=code)
            load(file.path(dir, msa, filename))
            
            sapply(simset@simulations, project.absolute.incidence, keep.dimensions = NULL, years=years)
        })
    })
    
    
    dim.names = list(sim=1:(length(rv)/length(msas)/length(intervention.codes)),
                     location=msas,
                     intervention=intervention.codes)
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    if (calculate.total)
    {
        dim.names$location = c(dim.names$location, 'total')
        new.rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
        new.rv[,msas,] = rv
        new.rv[,'total',] = apply(rv, c('sim','intervention'), sum, na.rm=T)
        rv = new.rv
    }
    
    rv
}

