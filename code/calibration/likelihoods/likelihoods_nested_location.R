



ALLOWED.NESTED.DATA.TYPES = c('suppression','engagement','linkage','diagnosed')
create.nested.likelihood <- function(data.type=c('suppression','engagement','linkage','diagnosed')[1],
                                        msa,
                                        msa.surveillance,
                                        state.surveillance,
                                        county.surveillance,
                                        years,
                                     
                                        include.msa = T,
                                        include.states=T,
                                        include.counties=T,
                                        
                                        observation.error.fn,
                                        error.correlation=0.5,
                                        
                                        sd.inflation = 1,
                                     
                                     
                                        probability.decreasing.slope=0.05,
                                        
                                        by.total=T,
                                        by.sex=T,
                                        by.race=T,
                                        by.age=T,
                                        by.risk=T,
                                        by.sex.age=F,
                                        by.sex.race=F,
                                        by.sex.risk=F,
                                        by.race.risk=F,
                                        
                                        ages=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                        races=c('black','hispanic','other'),
                                        sexes=c('heterosexual_male','msm','female'),
                                        risks=c('never_IDU','active_IDU','IDU_in_remission'),
                                     
                                     cached.errors=NULL,
                                     verbose=F)
{
#-- ERROR CHECKING --#
    if (all(ALLOWED.NESTED.DATA.TYPES!=data.type))
        stop(paste0("data.type must be one of: ",
                    paste0("'", ALLOWED.NESTED.DATA.TYPES, "'", collapse=', ')))
    
    
    
#-- SET OUT LOCATIONS and META-LOCATIONS --#    
    # msa - 1 metalocation
    # n counties -> n+1 metalocations, constrained sum
    # one state -> two metalocations (msa and outside msa)
    # m > 1 states -> 2m metalocations (for each state, in-msa and out-of-msa, constrained sum of m in-msa metalocations)
    # one state, n counties -> separate likelihoods: one for n counties, one for one state (they are conditionally independent given msa)
    # m>1 states, n counties -> 2m + n metalocations (n counties, m state-out-of-msa, m state-in-msa-out-of-counties)
    #                           n (counties) + m (state-in-msa-out-of-counties) with constrained sum

    if (verbose)
        cat("NSTLIK: Setting up meta-locations...")
    
    # Pull MSA data (assume if we have any data, we will have unstratified)
    msa.data = get.surveillance.data(msa.surveillance, location.codes = msa, 
                                     years = years, data.type = data.type, throw.error.if.missing.data = F)
    have.msa.data = any(!is.na(msa.data)) && include.msa
    if (!include.msa && verbose)
        cat("\n    - NOT including msa")
        
    
    # Pull state data
    if (include.states)
    {
        states = states.for.msa(msa)
        state.data = lapply(states, get.surveillance.data,
                            surv=state.surveillance, data.type=data.type, years=years,
                            throw.error.if.missing.data = F)
        have.state.data = sapply(state.data, function(st){
            !is.null(st) && any(!is.na(st))  
        })
        states = states[have.state.data]
    }
    else
    {
        states = character()
        if (verbose)
            cat("\n    - NOT including states")
    }
    
    # If this is a one-msa county, don't use county data (it will be the same as MSA)
    if (include.counties)
        include.counties = length(counties.for.msa(msa))>1
    
    # Pull county data
    if (include.counties)
    {
        counties = counties.in.msa = counties.for.msa(msa)
        county.data = lapply(counties, get.surveillance.data,
                             surv=county.surveillance, data.type=data.type, years=years,
                             throw.error.if.missing.data = F)
        have.county.data = sapply(county.data, function(cd){
            !is.null(cd) && any(!is.na(cd))
        })
        counties = counties[have.county.data]
    }
    else
    {
        counties = character()
        if (verbose)
            cat("\n    - NOT including counties")
    }
    
    # Initialize metalocation indices
    meta.indices.for.states = numeric()
    meta.index.for.msa = numeric()
    meta.indices.for.counties = numeric()
    meta.indices.for.observed.counties = numeric()
    counties.per.meta.index = list()
    
    if (length(states)==0 && length(counties)==0)
    {
        if (!have.msa.data)
            stop(paste0("There is no data on '", data.type, "' for ", msa.names(msa), 
                        " at the msa-, state-, or county-level"))
        else
            meta.index.for.msa = 1
    }
    else if (length(counties) >= 1 && length(states)<=1)
    {
        counties.per.meta.index = as.list(counties)
        meta.indices.for.observed.counties = 1:length(counties)
        if (!setequal(counties.in.msa, counties))
            counties.per.meta.index = c(counties.per.meta.index,
                                   list(setdiff(counties.in.msa, counties)))
        
        meta.indices.for.counties = 1:length(counties.per.meta.index)
    
        if (length(states)>0)
        {
            meta.indices.for.states = max(meta.indices.for.counties) + 1
            meta.index.for.msa = meta.indices.for.states + 1
        }
        else if (have.msa.data)
            meta.index.for.msa = max(meta.indices.for.counties) + 1
    }
    else if (length(states)==1) # && length(counties)==0
    {
        meta.indices.for.states = 1
        meta.index.for.msa = 2
    }
    else # length(states) > 1
    {
        counties.per.meta.index = as.list(counties)
        if (length(counties)>0)
        {
            meta.indices.for.observed.counties = 1:length(counties)
            for (state in states)
            {
                counties.in.state = intersect(counties.for.state(state), counties.in.msa)
                counties.in.state.not.separate = setdiff(counties.in.state, counties)
                if (length(counties.in.state.not.separate)>0)
                    counties.per.meta.index = c(counties.per.meta.index,
                                                list(counties.in.state.not.separate))
            }
            
            meta.indices.for.counties = 1:length(counties.per.meta.index)
        }
      
        meta.indices.for.states = max(c(0,meta.indices.for.counties)) + #add a zero in in case there are no counties
            1:length(states)
        
        if (have.msa.data)
            meta.index.for.msa = max(meta.indices.for.states) + 1
    }
    
    # initialize observation indices
    obs.msa.index = obs.state.indices = obs.county.indices = numeric()
    
    index = 0
    
    if (have.msa.data)
    {
        obs.msa.index = index + 1
        index = index + 1
    }
    
    if (length(counties)>0)
    {
        obs.county.indices = index + 1:length(counties)
        index = index + length(counties)
    }
    
    if (length(states)>0)
    {
        obs.state.indices = index + 1:length(states)
        index = index + length(states)
    }

    if (verbose)
        cat('DONE.\n')
    
#-- BUILD THE METALOCATIONS to OBS MATRIX --#

    if (verbose)
        cat('NSTLIK: Building the meta-location to obs matrix...')
    
    n.obs.locations = length(obs.msa.index) + length(obs.county.indices) + length(obs.state.indices)
    n.metalocations = length(meta.index.for.msa) + length(meta.indices.for.counties) + length(meta.indices.for.states)
    metalocation.to.obs.location.matrix = matrix(0, nrow=n.obs.locations,
                                                 ncol=n.metalocations)
    
    if (have.msa.data)
    {
        metalocation.to.obs.location.matrix[obs.msa.index, meta.index.for.msa] = 1
    }
    
    if (length(counties)>0)
    {
        for (i in 1:length(counties))
            metalocation.to.obs.location.matrix[obs.county.indices[i], meta.indices.for.observed.counties[i]] = 1
    }
    
    if (length(states)>0)
    {
        for (i in 1:length(states))
        {
            meta.counties.map.to.state = sapply(counties.per.meta.index, function(cty){
                any(state.for.county(cty)==states[i])
            })
            
            if (length(counties)>0)
                metalocation.to.obs.location.matrix[obs.state.indices[i], 
                                                    meta.indices.for.counties[meta.counties.map.to.state] ] = 1
            metalocation.to.obs.location.matrix[obs.state.indices[i], meta.indices.for.states[i] ] = 1
        }
    }
 
    if (verbose)
        cat('DONE.\n')
    
#-- SET UP OBSERVATIONS and TRANSFORMATION MATRICES --#    
    
    if (verbose)
        cat('NSTLIK: Setting up observations and transformation matrices...')
    
    obs.locations = character(n.obs.locations)
    obs.location.surveillance.managers = list(n.obs.locations)
    
    if (length(obs.msa.index)>0)
    {
        obs.locations[obs.msa.index] = msa
        obs.location.surveillance.managers[[obs.msa.index]] = msa.surveillance
    }
    
    if (length(obs.county.indices) > 0)
    {
        obs.locations[obs.county.indices] = counties
        obs.location.surveillance.managers[obs.county.indices] = lapply(1:length(counties), function(i){county.surveillance})
    }
    
    if (length(obs.state.indices) > 0)
    {
        obs.locations[obs.state.indices] = states
        obs.location.surveillance.managers[obs.state.indices] = lapply(1:length(states), function(i){state.surveillance})
    }
    
    obs.loc.likelihood.elements = lapply(1:length(obs.locations), function(i){
        create.likelihood.elements.for.data.type(data.type=data.type,
                                                 years=years,
                                                 surv=obs.location.surveillance.managers[[i]],
                                                 location=obs.locations[i],
                                                 by.total=by.total,
                                                 by.sex=by.sex,
                                                 by.race=by.race,
                                                 by.age=by.age,
                                                 by.risk=by.risk,
                                                 by.sex.age=by.sex.age,
                                                 by.sex.race=by.sex.race,
                                                 by.sex.risk=by.sex.risk,
                                                 by.race.risk=by.race.risk,
                                                 
                                                 numerator.sd=observation.error.fn,
                                                 bias.fn=function(...){0},
                                                 bias.sd=function(...){0},
                                                 numerator.year.to.year.chunk.correlation=error.correlation,
                                                 numerator.year.to.year.off.correlation=error.correlation,
                                                 numerator.chunk.years=list(years),
                                                 ages=ages,
                                                 races=races,
                                                 sexes=sexes,
                                                 risks=risks,
                                                 na.rm=F)
    }) 
       
    # Pull out the observations and errors
    observations.by.location = lapply(obs.loc.likelihood.elements, function(el){
        el$response.vector
    })
    
    observation.errors.by.location = lapply(obs.loc.likelihood.elements, function(el){
        el$numerator.covar.mat
    })
    
    observation.descriptions.by.location = lapply(obs.loc.likelihood.elements, function(el){
        el$descriptions
    })
    
    
    # Split up transformations by year and location
    transformation.matrices = lapply(1:length(years), function(i){
        year = years[i]
        
        lapply(obs.loc.likelihood.elements, function(el){
            year.for.desc = substr(el$descriptions, 1, 4)
            row.mask = year.for.desc == year
            col.mask = (1:(dim(el$transformation.matrix)[2]) - 1) %% length(years) + 1 == i
            
            rv = el$transformation.matrix[row.mask, col.mask]
            dim(rv) = c(sum(row.mask), sum(col.mask))
            
            rv
        })
    })
    
    transformation.names = lapply(1:length(years), function(i){
        year = years[i]
        
        lapply(obs.loc.likelihood.elements, function(el){
            year.for.desc = substr(el$descriptions, 1, 4)
            mask = year.for.desc == year
            el$descriptions[mask]
        })
    })

    # Pull out which years actually have any data
    # That way we won't waste computational power on years that don't have any data

    orig.years = years
    years = sort(unique(as.numeric(unique(unlist(sapply(obs.loc.likelihood.elements, function(el){
        unique(substr(el$descriptions, 1, 4))
    }))))))
    
    year.mask = sapply(orig.years, function(year){
        any(year==years)
    })
    transformation.matrices = transformation.matrices[year.mask]
    transformation.names = transformation.names[year.mask]
    
    if (verbose)
        cat('DONE.\n')
    
    
#-- SET UP N MULTIPLIERS --#
    
    if (verbose)
        cat('NSTLIK: Setting up "n-multipliers"...')
    
    n.years = length(years)
    n.strata = length(ages) * length(races) * length(sexes) * length(risks)
    
    metalocation.n.multipliers = list()# a list, one elem per year, each elem a matrix indexed [stratum, metalocation]
    metalocation.n.multiplier.variance = list()
    
    data.type.for.n = 'prevalence'
    if (data.type == 'linkage')
        data.type.for.n = 'new'

    state.n.cv = get.state.to.msa.outcome.ratio.cv(data.type.for.n,
                                                               msa.surveillance = msa.surveillance,
                                                               state.surveillance = state.surveillance,
                                                   cached.errors = cached.errors)
    county.n.cv = get.county.to.msa.outcome.ratio.cv(data.type.for.n,
                                                                 msa.surveillance = msa.surveillance,
                                                                 county.surveillance = county.surveillance,
                                                     cached.errors = cached.errors)
    n.multipliers.for.counties = lapply(counties.per.meta.index, function(cty){
        rv = calculate.outcome.differences(data.type=data.type.for.n,
                                      years=years,
                                      locations1=cty,
                                      surv1=county.surveillance,
                                      locations2=msa,
                                      surv2=msa.surveillance,         
                                      ages=ages,
                                      races=races,
                                      sexes=sexes,
                                      risks=risks)
        
        dim(rv) = c(length(years), length(rv)/length(years))
        rv
    })
    
    n.multipliers.for.states = lapply(states, function(st){
        rv = calculate.outcome.differences(data.type=data.type.for.n,
                                      years=years,
                                      locations1=st,
                                      surv1=state.surveillance,
                                      locations2=msa,
                                      surv2=msa.surveillance,         
                                      ages=ages,
                                      races=races,
                                      sexes=sexes,
                                      risks=risks)
        
        dim(rv) = c(length(years), length(rv)/length(years))
        rv
    })
    
    for (i in 1:length(years))
    {
        mults = matrix(1, nrow=n.strata, ncol=n.metalocations)
        cvs = matrix(0, nrow=n.strata, ncol=n.metalocations)
        
        if (length(meta.indices.for.counties)>0)
        {
            for (j in 1:length(meta.indices.for.counties))
                mults[,meta.indices.for.counties[j]] = n.multipliers.for.counties[[j]][i,]
        }
        
        if (length(meta.indices.for.states)>0)
        {
            for (j in 1:length(meta.indices.for.states))
                mults[,meta.indices.for.states[j]] = n.multipliers.for.states[[j]][i,]
        }
        
        cvs[,meta.indices.for.counties] = county.n.cv
        cvs[,meta.indices.for.states] = state.n.cv
        
        vars = (cvs * mults)^2
        
        metalocation.n.multipliers[[i]] = mults
        metalocation.n.multiplier.variance[[i]] = vars
    }
    
    if (verbose)
        cat('DONE.\n')
    
#-- SET UP P BIAS and VARIANCE --#
    
    if (verbose)
        cat('NSTLIK: Setting up "p-bias" and variance...')
    
    state.p.variance = get.state.to.msa.outcome.variance(data.type,
                                                         msa.surveillance = msa.surveillance,
                                                         state.surveillance = state.surveillance,
                                                         cached.errors = cached.errors)
    county.p.variance = get.county.to.msa.outcome.variance(data.type,
                                                           msa.surveillance = msa.surveillance,
                                                           county.surveillance = county.surveillance,
                                                           state.surveillance = state.surveillance,
                                                           cached.errors = cached.errors)
    state.p.bias = get.state.to.msa.outcome.bias(data.type,
                                                 msa.surveillance = msa.surveillance,
                                                 state.surveillance = state.surveillance,
                                                 cached.errors = cached.errors)
    
    # We're going to assume there is no bias on average between MSAs and the counties that they comprise
#    county.p.bias = get.state.to.msa.outcome.bias(data.type,
#                                                  msa.surveillance = msa.surveillance,
#                                                  county.surveillance = county.surveillance,
#                                                  cached.errors = cached.errors)
    
    metalocation.p.bias = lapply(1:n.years, function(i){
        rv = matrix(0, nrow=n.strata, ncol=n.metalocations)
        rv[,meta.indices.for.states] = state.p.bias
 #       rv[,meta.indices.for.counties] = county.p.bias

        rv
    })
    metalocation.p.variance = lapply(1:n.years, function(i){
        rv = matrix(0, nrow=n.strata, ncol=n.metalocations)
        rv[,meta.indices.for.states] = state.p.variance
        rv[,meta.indices.for.counties] = county.p.variance
        
        rv
    })
    
    if (verbose)
        cat('DONE.\n')
    
#-- SET UP THE EXTRACT FUNCTIONS --#
    
    if (verbose)
        cat('NSTLIK: Setting up extract functions...')
    
    if (data.type=='linkage')
    {
        total.new = get.surveillance.data(msa.surveillance, location.codes = msa, data.type='prevalence', years=years)
        total.new = unlist(interpolate.parameters(values=total.new[!is.na(total.new)],
                                                         value.times=years[!is.na(total.new)],
                                                         desired.times = years))
        
        extract.np.fn = function(sim){
            n = do.extract.new.diagnoses(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'), 
                                         per.population=NA, use.cdc.categorizations = F)
            p = extract.linkage(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'), 
                                per.population=1, use.cdc.categorizations = F)
            
            list(n=n / rowSums(n) * total.new,
                 p=p)
        }
    }
    else 
    {
        total.prevalence = get.surveillance.data(msa.surveillance, location.codes = msa, data.type='prevalence', years=years)
        total.prevalence = unlist(interpolate.parameters(values=total.prevalence[!is.na(total.prevalence)],
                                                         value.times=years[!is.na(total.prevalence)],
                                                         desired.times = years))
        
        if (data.type=='diagnosed')
            extract.np.fn = function(sim){
                denominator = do.extract.prevalence(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'), 
                                                    per.population=NA, use.cdc.categorizations = F)
                numerator = do.extract.diagnosed.hiv(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'), 
                                                     per.population=NA, use.cdc.categorizations = F)
        
                p = numerator/denominator
                
                list(n=numerator / rowSums(numerator) * total.prevalence / p,
                     p=p)
            }
        else if (data.type=='suppression')
            extract.np.fn = function(sim){
                denominator = do.extract.diagnosed.hiv(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'),
                                             per.population = NA, use.cdc.categorizations = F)
                p = extract.suppression(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'),
                                        continuum = sim$diagnosed.continuum.states,
                                        per.population = 1, use.cdc.categorizations = F)
                
                list(n=denominator / rowSums(denominator) * total.prevalence,
                     p=p)
        }
        else if (data.type=='engagement')
            extract.np.fn = function(sim){
                denominator = do.extract.diagnosed.hiv(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'),
                                                       per.population = NA, use.cdc.categorizations = F)
                p = do.extract.engagement(sim, years=years, keep.dimensions=c('year','age','race','sex','risk'),
                                          continuum = sim$diagnosed.continuum.states,
                                          per.population = 1, use.cdc.categorizations = F)
                
                list(n=denominator / rowSums(denominator) * total.prevalence,
                     p=p)
            }
        else
            stop("Invalid data.type")
    }
    
    if (verbose)
        cat('DONE.\n')
    
#-- CREATE THE FUNCTION AND RETURN --#

    if (verbose)
        cat("NSTLIK: All done. Packaging up a function and returning.")
    
    function(sim, log=T) {
        
        np = extract.np.fn(sim)
        p = np$p
        n = np$n
        dim(p) = dim(n) = c(length(years), length(p)/length(years))
        
        # The continous likelihood
        tryCatch({
        lik.continuous = nested.proportion.likelihood.sub(
            p = p,
            n = n,
            
            metalocation.n.multipliers = metalocation.n.multipliers,
            metalocation.n.multiplier.variance = metalocation.n.multiplier.variance,
            
            metalocation.p.bias = metalocation.p.bias,
            metalocation.p.variance = metalocation.p.variance,
            
            sum.index = meta.index.for.msa,
            condition.on.sum.indices = meta.indices.for.counties,
            
            metalocation.to.obs.location.matrix = metalocation.to.obs.location.matrix,
            
            observation.errors.by.location=observation.errors.by.location,
            observations.by.location=observations.by.location,
            observation.descriptions.by.location, #a list, with a character vector for each location
            observations.are.proportions=T,
            
            transformation.matrices,
            #^ a list, one elem per year. Each elem is itself a list, with one elem per obs location
            #  transformation[[i]][[j]] is a matrix for the ith year, jth location that collapses across strata
            transformation.names, 
            #^ a list of lists (elem per year and obs location) with a vector of the names of outcomes after transformation
            
            sd.inflation=sd.inflation,
            log=log
        ) },
        error=function(e){
            save(sim, data.type, file='mcmc_runs/nested_lik_error_info.Rdata')
            stop("propagating error")
        })
        
        # The binary (is decreasing) likelihood
        if (is.na(probability.decreasing.slope))
        {
            if (log)
                lik.binary = 0
            else
                lik.binary = 1
        }
        else
        {
            is.decreasing = as.numeric(p[length(years),] < p[1,])
            
            if (log)
                lik.binary = sum(is.decreasing*log(probability.decreasing.slope) + (1-is.decreasing)*log(1-probability.decreasing.slope))
            else
                lik.binary = prod(probability.decreasing.slope ^ is.decreasing * (1-probability.decreasing.slope)^(1-is.decreasing))
        }
        
        # Put them together and return
        
        if (log)
            lik.binary + lik.continuous
        else
            lik.binary * lik.continuous
    }
}


nested.proportion.likelihood.sub <- function(
    p, #array, indexed [year, stratum]
    n, #array, indexed [year, stratum]
    
    metalocation.n.multipliers, # a list, each elem a matrix indexed [stratum, metalocation]
    metalocation.n.multiplier.variance,
    
    metalocation.p.bias,
    metalocation.p.variance,
    
    sum.index,
    condition.on.sum.indices,
    
    metalocation.to.obs.location.matrix, #indexed [obs.loc, meta.loc] - ie 
    observation.errors.by.location, #a list, with a covariance matrix for each location - in year-minor, stratum-major order
    observations.by.location, #a list, with a vector for each location - in year-minor, stratum-major order
    observation.descriptions.by.location, #a list, with a character vector for each location
    observations.are.proportions=T,
    
    transformation.matrices,
    #^ a list, one elem per year. Each elem is itself a list, with one elem per obs location
    #  transformation[[i]][[j]] is a matrix for the ith year, jth location that collapses across strata
    transformation.names, 
    #^ a list of lists (elem per year and obs location) with a vector of the names of outcomes after transformation
    
    sd.inflation=1,
    log=T
)
{
#-- Some general variables --#

    n.years = dim(p)[1]
    years = dimnames(p)[[1]]
    n.strata = dim(p)[2]
    
    n.metalocations = dim(metalocation.to.obs.location.matrix)[2]
    n.obs.locations = dim(metalocation.to.obs.location.matrix)[1]

    sim.np = n*p
    sim.np.1mp = sim.np * (1-p)
    
#-- Split the n and p up by metalocation --#
    
    # each element indexed [stratum, meta-loc]
    metalocation.p = lapply(1:n.years, function(i){
        p[i,] + metalocation.p.bias[[i]]
    })
    
    # each element indexed [stratum, meta-loc]
    metalocation.n = lapply(1:n.years, function(i){
        n[i,] * metalocation.n.multipliers[[i]]
    })

    # each element indexed [stratum, meta-loc]
    metalocation.n.variance = lapply(1:n.years, function(i){
        n[i,]^2 * metalocation.n.multiplier.variance[[i]]
    })
    
    
#-- Make variances on np --#
    
    # each element indexed [stratum, meta-loc]
    metalocation.var = lapply(1:n.years, function(i){
        n = metalocation.n[[i]]
        p = metalocation.p[[i]]
        n.var = metalocation.n.variance[[i]]
        p.var = metalocation.p.variance[[i]]
        
        n*p*(1-p) + n*p.var*(n-1) + n.var*p^2 + n.var*p.var
    })

    #-- Define the mean vector and variance-covariance matrix for each year --#

        
    # one elem per year
    # each elem is a matrix [stratum, metalocation]
    metalocation.mean.by.year = lapply(1:n.years, function(i){
        rv = metalocation.n[[i]] * metalocation.p[[i]]
        
        if (length(condition.on.sum.indices)>0)
        {
            v = metalocation.var[[i]]
            total.conditioned.v = rowSums(v[,condition.on.sum.indices])
            total.conditioned.mu = rowSums(rv[,condition.on.sum.indices])
            
            for (j in condition.on.sum.indices)
            {
                non.zero.mask = v[,j] != 0
                rv[non.zero.mask,j] = (rv[,j] + v[,j] * (sim.np[i,] - total.conditioned.mu) / total.conditioned.v)[non.zero.mask]
                
            }
            
            if (length(sum.index)>0)
                rv[,sum.index] = sim.np[i,]
        }
        
        rv
    })
    
    # a list of arrays
    # one elem per year
    # each array has dimensions [n.metalocations, n.metalocations,n.strata]
    # elem[,,i] is a variance-covariance matrix for metalocations of stratum i
    metalocation.covar.by.year = lapply(1:n.years, function(i){
        v = metalocation.var[[i]] #indexed [stratum, meta-loc] 
        
        if (n.metalocations==1)
            rv = v
        else
            rv = sapply(1:n.strata, function(s){
                diag(v[s,])
            })
        dim(rv) = c(n.metalocations,n.metalocations, n.strata)
        
        # if we're conditioning on the sum, we need to
        # - overwrite the extra variances (on the diagonals)
        # - fill in the covariances (off diagonals) between elements 
        #   corresponding to the same row of v[,condition.on.sum.indices]
        if (length(condition.on.sum.indices)>0)
        {
            total.conditioned.v = rowSums(v[,condition.on.sum.indices])
           
            for (j in condition.on.sum.indices)
            {
                rv[j,j,] = v[,j] * (total.conditioned.v-v[,j]) / total.conditioned.v +
                        #^ie, for j=1, var = sigma_1^2 * sigma_2^2 + sigma_1^2 * sigma_3^2 + ...
                        # this is var(y_i | sum=k)
                    sim.np.1mp[i,] * (v[,j]/total.conditioned.v)^2
                        #^this is the extra variance for unknown k
                
                zero.mask = total.conditioned.v == 0
                rv[j,j,zero.mask] = 0
                
                if (length(sum.index)>0)
                {
                    rv[j,sum.index,] = rv[sum.index,j,] = (sim.np.1mp[i,] * v[,j] / total.conditioned.v)
                    rv[j,sum.index,zero.mask] = rv[sum.index,j,zero.mask] = 0
                }
            }
            
            if (length(sum.index)>0)
                rv[sum.index,sum.index,] = sim.np.1mp[i,]
             
            combos = combn(condition.on.sum.indices, m=2)
            for (combo.index in dim(combos)[2])
            {
                j1 = combos[1,combo.index]
                j2 = combos[2,combo.index]
                
                zero.mask = total.conditioned.v==0
                rv[j1,j2,] = rv[j2,j1,] = -v[,j1]*v[,j2] / total.conditioned.v +
                            #^ the cov conditional on sum = k
                    sim.np.1mp[i,] * v[,j1] * v[,j2] / total.conditioned.v^2
                            #^ the extra covariance incurred by unknown k
                rv[j1,j2,zero.mask] = rv[j2,j1,zero.mask] = 0
            }
        }
        
        rv
    })
    
#-- Collapse the meta-locations to observed locations --#
    
    #each elem indexed [stratum, obs.location]
    obs.location.stratified.mean.by.year = lapply(metalocation.mean.by.year, function(mean.mat){
        
        rv = apply(mean.mat, 1, function(mean.v){
            metalocation.to.obs.location.matrix %*% mean.v
        })
        
        if (n.obs.locations==1)
        {
            dim(rv) = c(n.strata, 1)
            rv
        }
        else
            t(rv)
    })
    
    if (observations.are.proportions)
        obs.location.stratified.n.by.year = lapply(metalocation.n, function(n.mat){
            rv = apply(n.mat, 1, function(n.v){
                metalocation.to.obs.location.matrix %*% n.v
            })
            if (n.obs.locations==1)
            {
                dim(rv) = c(n.strata, 1)
                rv
            }
            else
                t(rv)
        })
    
    metalocation.to.obs.location.matrix.transposed = t(metalocation.to.obs.location.matrix)
    #each elem indexed [,,stratum]
    obs.location.stratified.covar.by.year = lapply(metalocation.covar.by.year, function(covar.arr){
        rv = apply(covar.arr, 3, function(mat){
            metalocation.to.obs.location.matrix %*% mat %*% metalocation.to.obs.location.matrix.transposed
        })
        dim(rv) = c(n.obs.locations,n.obs.locations, n.strata)
        rv
    })
    
    #-- Aggregate across strata with transformation.mat --#
    
    aggregated.transformation.matrices = lapply(transformation.matrices, function(mats){
        n.rows = sum(sapply(mats, nrow))
        n.cols = sum(sapply(mats, ncol))
        
        rv = matrix(0, nrow=n.rows, ncol=n.cols)
        
        r.index = 0
        c.index = 0
        for (mat in mats)
        {
            if (!is.null(mat) && dim(mat)[1]>0)
            {
                rv[r.index + 1:(dim(mat)[1]), c.index + 1:(dim(mat)[2])] = mat
                r.index = r.index + dim(mat)[1]
                c.index = c.index + dim(mat)[2]
            }
        }
        
        rv
    })
    
    #each elem is a vector, location-major/stratum-minor order
    obs.location.mean.by.year = lapply(1:n.years, function(i){
        unlist(sapply(1:n.obs.locations, function(j){
            mat = transformation.matrices[[i]][[j]]
            
            if (is.null(mat) || dim(mat)[1]==0)
                numeric()
            else
                mat %*% obs.location.stratified.mean.by.year[[i]][,j]
        }))
    })
    
    obs.location.names.by.year = lapply(1:n.years, function(i){
        unlist(sapply(1:n.obs.locations, function(j){
            mat = transformation.matrices[[i]][[j]]
            if (is.null(mat) || dim(mat)[1]==0)
                character()
            else
                paste0(j, ' - ', transformation.names[[i]][[j]])
        }))
    })
    
    if (observations.are.proportions)
        obs.location.n.by.year = lapply(1:n.years, function(i){
            unlist(sapply(1:n.obs.locations, function(j){
                mat = transformation.matrices[[i]][[j]]
                if (is.null(mat) || dim(mat)[1]==0)
                    numeric()
                else
                    mat %*% obs.location.stratified.n.by.year[[i]][,j]
            }))
        })
    
    #each elem is a covariance matrix
    # each dimension of the covariance matrix is location-major/stratum-minor order
    obs.location.covar.by.year = lapply(1:n.years, function(i){
        collapsed.mat = obs.location.stratified.covar.by.year[[i]]
        
        expanded.mat = matrix(0, nrow=n.strata*n.obs.locations, ncol=n.strata*n.obs.locations)
        for (j1 in 1:n.obs.locations)
        {
            for (j2 in 1:n.obs.locations)
            {
                indices1 = (j1-1)*n.strata + 1:n.strata
                indices2 = (j2-1)*n.strata + 1:n.strata
                expanded.indices = indices1 + (indices2-1)*dim(expanded.mat)[1]
                expanded.mat[expanded.indices] = collapsed.mat[j1,j2,]
            }
        }
        
        aggregated.transformation.matrices[[i]] %*% expanded.mat %*% t(aggregated.transformation.matrices[[i]])
    })
    
    
    #-- Hydrate the lists into mean vector and covariance matrix --#
    obs.location.mean = unlist(obs.location.mean.by.year)
    obs.location.names = unlist(obs.location.names.by.year)
    if (observations.are.proportions)
        obs.location.n = unlist(obs.location.n.by.year)
    
    n.obs = length(obs.location.mean)
    obs.location.covar = matrix(0, nrow=n.obs, ncol=n.obs)
    index = 0
    for (year.covar in obs.location.covar.by.year)
    {
        if (dim(year.covar)[1]>0)
            obs.location.covar[index+1:(dim(year.covar)[1]), index+1:(dim(year.covar)[1])] = year.covar
        index = index + dim(year.covar)[1]
    }
    
    # Right now, the obs.location.x are in stratum-minor, location-intermediate, year-major order
    # ie, if we hydrated up into a 3d array, it would be indexed [stratum, location, year]
    #  (except it is not a regular array - there are not the same number of strata for each location*year)
    # 
    # We want to put them in the same order in which they appear in 'observations'
    # year-minor, stratum-intermediate, location-major order
    observation.descriptions.by.location.prefixed = lapply(1:n.obs.locations, function(j){
        paste0(j, ' - ', observation.descriptions.by.location[[j]])
    })
    observation.names = unlist(observation.descriptions.by.location.prefixed)
    o = 1:length(obs.location.names)
    names(o) = obs.location.names
    o = o[observation.names]
    
    obs.location.mean = obs.location.mean[o]
    obs.location.covar = obs.location.covar[o,o]
    if (observations.are.proportions)
        obs.location.n = obs.location.n[o]

    #-- Layer the observation errors onto the covariance matrices --#
    
    o = 1:length(observation.names)
    names(o) = observation.names
    for (j in 1:n.obs.locations)
    {
        indices = o[observation.descriptions.by.location.prefixed[[j]]]
        
        err = observation.errors.by.location[[j]]
        if (observations.are.proportions)
            err = err * (obs.location.n[indices] %*% t(obs.location.n[indices]))

        obs.location.covar[indices,indices] = obs.location.covar[indices,indices] * sd.inflation^2 +
            err
    }
    
    
    #-- Calculate the Density --#
    
    obs.v = unlist(observations.by.location)
    if (observations.are.proportions)
        obs.v = obs.v * obs.location.n
    
    if (any(is.na(obs.v) || any(is.na(obs.location.mean)) || any(is.na(obs.location.covar))))
    {
        stop("NA values in distribution parameters")
    }
    
    dmvnorm(x = obs.v,
            mean = obs.location.mean,
            sigma = obs.location.covar,
            log=log)
}


# for debugging
if (1==2)
{
    qplot(obs.location.mean, obs.v) + geom_abline(intercept=0, slope=1)
    
    obs.p = unlist(observations.by.location)
    range(obs.p)
    
    
    obs.location.p = obs.location.mean/obs.location.n
    qplot(obs.location.p, obs.p) + geom_abline(intercept=0, slope=1)
    
    range((obs.location.p-obs.p)/obs.p)
    
    range((obs.location.mean-obs.v)/obs.v)
    
    std.errs = (obs.v - obs.location.mean) / sqrt(diag(obs.location.covar))
    qplot(std.errs)
}


#returns an array indexed [year, stratum]
# values are: reverse.scale(scale(values1)-scale(values2))
calculate.outcome.differences <- function(data.type,
                                     years,
                                     locations1,
                                     surv1,
                                     locations2,
                                     surv2,
                                     
                                     smooth.scale=log,
                                     reverse.scale=exp,
                                     
                                     smooth=T,
                                     
                                     ages=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                     races=c('black','hispanic','other'),
                                     sexes=c('heterosexual_male','msm','female'),
                                     risks=c('never_IDU','active_IDU','IDU_in_remission')
)
{
    dimensions = list(age=ages,
                      race=races,
                      sex=sexes,
                      risk=risks)
    
    CDC.RISK = c('msm','idu', 'msm_idu', 'heterosexual')
    CDC.SEX = c('female','male')
    cdc.dimensions = list(age=ages,
                          race=races,
                          sex=CDC.SEX,
                          risk=CDC.RISK)
    
    #-- Line up which combinations of dimensions are in each "level" (num dimensions not marginalized) --#
    combinations.by.level.dimensions = lapply(0:length(dimensions), function(depth){
        combos = combn(names(dimensions), depth)
        
        n.combos = max(1, dim(combos)[2])
        
        rv = matrix(F, nrow=length(dimensions), ncol=n.combos, dimnames=list(dimension=names(dimensions), combo=NULL))
        if (depth > 0)
        {
            for (i in 1:n.combos)
            {
                for (dim in combos[,i])
                    rv[dim, i] = T
            }
        }   
        
        rv
    })
    
    #-- Pull the values and calculate a delta (and smooth if desired) --#
    combinations.by.level.values = lapply(0:length(dimensions), function(depth){
        cbl = combinations.by.level.dimensions[[depth+1]]
        lapply(1:dim(cbl)[2], function(i){
            dims = cbl[,i]
            
            values1 = get.surveillance.data(surv=surv1, location.codes=locations1, data.type=data.type, years=years,
                                            aggregate.locations = T, aggregate.years = F,
                                            age=dims['age'], race=dims['race'], sex=dims['sex'], risk=dims['risk'],
                                            throw.error.if.missing.data = F)
            values2 = get.surveillance.data(surv=surv2, location.codes=locations2, data.type=data.type, years=years,
                                            aggregate.locations = T, aggregate.years = F,
                                            age=dims['age'], race=dims['race'], sex=dims['sex'], risk=dims['risk'],
                                            throw.error.if.missing.data = F)
            
            if (is.null(values1) || is.null(values2) || all(is.na(values1) | is.na(values2)))
            {
                if (depth==0)
                    stop("No matching data for both locations available")
                NULL
            }
            else
            {
                delta = smooth.scale(values1) - smooth.scale(values2)
                if (is.null(dim(delta)) || length(dim(delta))==1)
                    dim(delta) = c(year=length(years), all=1)
                
                smoothed.delta = apply(delta, 2:length(dim(delta)), function(z){
                    if (smooth)
                    {
                        mask = !is.na(z) & !is.infinite(z)
                        if (sum(mask)==0)
                            rep(NaN, length(years))
                        else if (sum(mask)==1)
                            rep(z[!is.na(z)], length(years))
                        else
                        {
                            fit = lm(z[mask]~years[mask])
                            fit$coefficients[1] + fit$coefficients[2]*years
                        }
                    }
                    else
                        interpolate.parameters(z[!is.na(z)], years[!is.na(z)], 
                                               desired.times = years, return.list = F)
                })
                
                dim(smoothed.delta) = dim(delta)
                dimnames(smoothed.delta) = dimnames(delta)
                
                smoothed.delta
            }
        })
    })
    
    #-- Infer missing values from the previous level --#
    for (depth in (1+1:length(dimensions)))
    {
        for (i in 1:length(combinations.by.level.values[[depth]]))
        {
            values = combinations.by.level.values[[depth]][[i]]
            if (is.null(values) || any(is.na(values)))
            {
                #if the difference is missing,
                # we're going to infer it as the sum of all the differences from the
                # level up which are a subset of the dimensions here
                
                dims = combinations.by.level.dimensions[[depth]][,i]
                calculate.from.mask = apply(combinations.by.level.dimensions[[depth-1]], 2, function(dims.above){
                    all(!dims.above | dims)
                })
                
                dim.names = c(list(year=as.character(years)),
                              cdc.dimensions[dims])
                diffs = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
                
                for (diffs.above in combinations.by.level.values[[depth-1]][calculate.from.mask])
                {
                    diffs = diffs + expand.population(diffs.above, target.dim.names=dim.names)
                }
                diffs = diffs / sum(calculate.from.mask)
                
                if (is.null(values))
                    values = diffs
                else
                    values[is.na(values)] = diffs[is.na(values)]
                
                combinations.by.level.values[[depth]][[i]] = values
            }
        }
    }
    
    #-- Return --#
    
    rv = combinations.by.level.values[[1+length(dimensions)]][[1]]
    rv = recategorize.to.jheem.risk.strata(rv, proportion.idu.in.remission = 1, proportion.idu.active=1)
    rv = reverse.scale(rv)
    rv
}

##----------------------------------------------##
##-- Estimating Errors From Surveillance Data --##
##----------------------------------------------##

#-- Testing --#

if (1==2)
{
    types.to.test = c(
        'suppression',
        'diagnosed',
        'engagement',
        'linkage'
    )
    
    
    x = sapply(types.to.test, function(type){
        print(type)
        c(
            state_bias = get.state.to.msa.outcome.bias(type, msa.surveillance, state.surveillance,
                                                       cached.errors = nested.error.cache),
            county_bias = get.county.to.msa.outcome.bias(type, msa.surveillance, county.surveillance,
                                                         cached.errors = nested.error.cache),
            state_sd = get.state.to.msa.outcome.variance(type, msa.surveillance, state.surveillance,
                                                         cached.errors = nested.error.cache)^.5,
            county_sd = get.county.to.msa.outcome.variance(type, msa.surveillance, county.surveillance,
                                                           cached.errors = nested.error.cache)^.5
        )    
    })
    
    round(100*x,1)
    
    get.state.to.msa.outcome.bias('diagnosed', msa.surveillance, state.surveillance, details=T)
    get.state.to.msa.outcome.variance('linkage', msa.surveillance, state.surveillance, details=T)
    
    ratio.types.to.test = c('new','prevalence')
    y = sapply(ratio.types.to.test, function(type){
        print(type)
        c(
            state = get.state.to.msa.outcome.ratio.cv(type, msa.surveillance, state.surveillance,
                                                      cached.errors = nested.error.cache),
            county = get.county.to.msa.outcome.ratio.cv(type, msa.surveillance, county.surveillance,
                                                        cached.errors = nested.error.cache)
        )    
    })
    
    round(y,2)
    
    get.state.to.msa.outcome.ratio.cv('new', msa.surveillance, state.surveillance)
    
    nested.error.cache = make.nested.errors.cache(msa.surveillance, state.surveillance, county.surveillance,
                                                  verbose=T)
}

#-- Errors on Ratios --# 

get.state.to.msa.outcome.ratio.cv <- function(data.type,
                                                    msa.surveillance,
                                                    state.surveillance,
                                              cached.errors=NULL,
                                                    years=NULL,
                                                    details=F)
{
    #for now
    #.1^2
    
    if (!is.null(cached.errors))
    {
        rv = get.cached.nested.error(cache=cached.errors,
                                     geography = 'state',
                                     data.type = data.type,
                                     statistic = 'ratio.cv',
                                     years = years)
        
        if (!is.null(rv))
            return (rv)
    }
    
    paired.data = get.paired.msa.data(data.type=data.type,
                                      years=years,
                                      msa.surveillance=msa.surveillance,
                                      other.surveillance=state.surveillance,
                                      msa.to.other.code.mapping.fn = states.for.msa,
                                      eliminate.one.to.one.mappings=F,
                                      by.total=T,
                                      by.age=T,
                                      by.race=T,
                                      by.sex=T,
                                      by.risk=T,
                                      details=details)
    
    ratios = get.paired.ratios.and.reference(paired.data=paired.data)
    diff = ratios$ratio - ratios$reference
    rel.diff = diff/ratios$reference
    
    if (details) # for testing/debugging
        print(qplot(rel.diff, ratios$ratio) + geom_smooth() + 
                  ggtitle(paste0(data.type, ' - State Ratio CV')) + ylab("Ratio Diff") + xlab('Ratio') )
    
    
    sd(rel.diff)
}

get.county.to.msa.outcome.ratio.cv <- function(data.type,
                                                     msa.surveillance,
                                                     county.surveillance,
                                               cached.errors=NULL,
                                                     years=NULL,
                                                     details=F)
{
    #.1^2
    
    if (!is.null(cached.errors))
    {
        rv = get.cached.nested.error(cache=cached.errors,
                                     geography = 'county',
                                     data.type = data.type,
                                     statistic = 'ratio.cv',
                                     years = years)
        
        if (!is.null(rv))
            return (rv)
    }
    
    paired.data = get.paired.msa.data(data.type=data.type,
                                      years=years,
                                      msa.surveillance=msa.surveillance,
                                      other.surveillance=county.surveillance,
                                      msa.to.other.code.mapping.fn = counties.for.msa,
                                      eliminate.one.to.one.mappings=T,
                                      by.total=T,
                                      by.age=T,
                                      by.race=T,
                                      by.sex=T,
                                      by.risk=T,
                                      details=details)
    
    ratios = get.paired.ratios.and.reference(paired.data=paired.data)
    diff = ratios$ratio - ratios$reference
    rel.diff = diff/ratios$reference
    
    if (details) # for testing/debugging
        print(qplot(rel.diff, ratios$ratio) + geom_smooth() +
                  ggtitle(paste0(data.type, '- County Ratio CV')) +  ylab("Ratio Diff") + xlab('Ratio'))
    
    
    sd(rel.diff)
}

get.state.to.msa.outcome.bias <- function(data.type,
                                          msa.surveillance,
                                          state.surveillance,
                                          cached.errors=NULL,
                                          years=NULL,
                                          details=F)
{
    #0
    
    if (!is.null(cached.errors))
    {
        rv = get.cached.nested.error(cache=cached.errors,
                                     geography = 'state',
                                     data.type = data.type,
                                     statistic = 'bias',
                                     years = years)
        
        if (!is.null(rv))
            return (rv)
    }
    
    paired.data = get.paired.msa.data(data.type=data.type,
                                      years=years,
                                      msa.surveillance=msa.surveillance,
                                      other.surveillance=state.surveillance,
                                      msa.to.other.code.mapping.fn = states.for.msa,
                                      eliminate.one.to.one.mappings=F,
                                      by.total=T,
                                      by.age=T,
                                      by.race=T,
                                      by.sex=T,
                                      by.risk=T,
                                      details=details)
    
    if (details) # for testing/debugging
        print(qplot(paired.data$msa, paired.data$other) + geom_smooth() + 
                  ggtitle(paste0(data.type, ' - State Bias')) + ylab("State") + xlab('MSA'))
    
    mean(paired.data$other - paired.data$msa)
}

get.county.to.msa.outcome.bias <- function(data.type,
                                          msa.surveillance,
                                          county.surveillance,
                                          cached.errors=NULL,
                                          years=NULL,
                                          details=F)
{
    #0
    
    if (!is.null(cached.errors))
    {
        rv = get.cached.nested.error(cache=cached.errors,
                                     geography = 'county',
                                     data.type = data.type,
                                     statistic = 'bias',
                                     years = years)
        
        if (!is.null(rv))
            return (rv)
    }
    
    paired.data = get.paired.msa.data(data.type=data.type,
                                      years=years,
                                      msa.surveillance=msa.surveillance,
                                      other.surveillance=county.surveillance,
                                      msa.to.other.code.mapping.fn = counties.for.msa,
                                      eliminate.one.to.one.mappings=T,
                                      by.total=T,
                                      by.age=T,
                                      by.race=T,
                                      by.sex=T,
                                      by.risk=T,
                                      details=details)
    
    if (details) # for testing/debugging
        print(qplot(paired.data$msa, paired.data$other) + geom_smooth() + 
                  ggtitle(paste0(data.type, ' - County Bias')) + ylab("County") + xlab('MSA'))
    
    mean(paired.data$other - paired.data$msa)
}

get.state.to.msa.outcome.variance <- function(data.type,
                                              msa.surveillance,
                                              state.surveillance,
                                              cached.errors=NULL,
                                              years=NULL,
                                              details=F)
{
    #.01^2

    if (!is.null(cached.errors))
    {
        rv = get.cached.nested.error(cache=cached.errors,
                                     geography = 'state',
                                     data.type = data.type,
                                     statistic = 'variance',
                                     years = years)
        
        if (!is.null(rv))
            return (rv)
    }
    
    paired.data = get.paired.msa.data(data.type=data.type,
                                      years=years,
                                      msa.surveillance=msa.surveillance,
                                      other.surveillance=state.surveillance,
                                      msa.to.other.code.mapping.fn = states.for.msa,
                                      eliminate.one.to.one.mappings=F,
                                      by.total=T,
                                      by.age=T,
                                      by.race=T,
                                      by.sex=T,
                                      by.risk=T,
                                      details=details)
    
    if (details) # for testing/debugging
        print(qplot(paired.data$msa, paired.data$other) + geom_smooth() + 
                  ggtitle(paste0(data.type, ' - State Variance')) + ylab("State") + xlab('MSA'))
    
    var(paired.data$other - paired.data$msa)
}

get.county.to.msa.outcome.variance <- function(data.type,
                                               msa.surveillance,
                                               county.surveillance,
                                               state.surveillance, #for backup
                                               cached.errors=NULL,
                                               min.obs=15,
                                               years=NULL,
                                               details=F)
{
    #.01^2
    
    if (!is.null(cached.errors))
    {
        rv = get.cached.nested.error(cache=cached.errors,
                                     geography = 'county',
                                     data.type = data.type,
                                     statistic = 'variance',
                                     years = years)
        
        if (!is.null(rv))
            return
    }
    
    paired.data = get.paired.msa.data(data.type=data.type,
                                      years=years,
                                      msa.surveillance=msa.surveillance,
                                      other.surveillance=county.surveillance,
                                      msa.to.other.code.mapping.fn = counties.for.msa,
                                      eliminate.one.to.one.mappings=T,
                                      by.total=T,
                                      by.age=T,
                                      by.race=T,
                                      by.sex=T,
                                      by.risk=T,
                                      details=details,
                                      min.obs=0)
    
    if (length(paired.data$msa)<min.obs)
    {
        paired2 = get.paired.msa.data(data.type=data.type,
                                      years=years,
                                      msa.surveillance=msa.surveillance,
                                      other.surveillance=state.surveillance,
                                      msa.to.other.code.mapping.fn = states.for.msa,
                                      eliminate.one.to.one.mappings=F,
                                      by.total=T,
                                      by.age=T,
                                      by.race=T,
                                      by.sex=T,
                                      by.risk=T,
                                      details=details,
                                      min.obs=0)
        
        paired.data$other = c(paired.data$other, paired2$other)
        paired.data$msa = c(paired.data$msa, paired2$msa)
    }
    
    if (details) # for testing/debugging
        print(qplot(paired.data$msa, paired.data$other) + geom_smooth() + 
                  ggtitle(paste0(data.type, ' - County Variance')) + ylab("County") + xlab('MSA'))
    
    var(paired.data$other - paired.data$msa)
}

#-- For Logical Checks --#

DATA.TYPE.MIN = c(
    'linkage' = 0,
    'suppression' = 0,
    'diagnosed' = 0,
    'engagement' = 0,
    'new' = 0,
    'prevalence' = 0
)

DATA.TYPE.MAX = c(
    'linkage' = 1,
    'suppression' = 1,
    'diagnosed' = 1,
    'engagement' = 1,
    'new' = Inf,
    'prevalence' = Inf
)


#-- Helpers --#

get.paired.msa.data <- function(data.type,
                                years=NULL,
                                msa.surveillance,
                                other.surveillance,
                                msa.to.other.code.mapping.fn,
                                eliminate.one.to.one.mappings,
                                by.total=T,
                                by.age=T,
                                by.race=T,
                                by.sex=T,
                                by.risk=T,
                                min.obs=15,
                                details=F)
{
    rv = list(
        msa = numeric(),
        other = numeric(),
        location1 = character(),
        location2 = character(),
        stratification = character()
    )
    
    if (by.total)
    {
        sub.rv = get.paired.msa.data.one.stratification(data.type=data.type,
                                                        years=years,
                                                        msa.surveillance=msa.surveillance,
                                                        other.surveillance=other.surveillance,
                                                        msa.to.other.code.mapping.fn=msa.to.other.code.mapping.fn,
                                                        eliminate.one.to.one.mappings=eliminate.one.to.one.mappings,
                                                        age=F,
                                                        race=F,
                                                        sex=F,
                                                        risk=F)
        
        rv$msa = c(rv$msa, sub.rv$msa)
        rv$other = c(rv$other, sub.rv$other)
        rv$location1 = c(rv$location1, sub.rv$location1)
        rv$location2 = c(rv$location2, sub.rv$location2)
        rv$stratification = c(rv$stratification, sub.rv$stratification)
        
    }
    
    
    if (by.age)
    {
        sub.rv = get.paired.msa.data.one.stratification(data.type=data.type,
                                                        years=years,
                                                        msa.surveillance=msa.surveillance,
                                                        other.surveillance=other.surveillance,
                                                        msa.to.other.code.mapping.fn=msa.to.other.code.mapping.fn,
                                                        eliminate.one.to.one.mappings=eliminate.one.to.one.mappings,
                                                        age=T,
                                                        race=F,
                                                        sex=F,
                                                        risk=F)
        
        rv$msa = c(rv$msa, sub.rv$msa)
        rv$other = c(rv$other, sub.rv$other)
        rv$location1 = c(rv$location1, sub.rv$location1)
        rv$location2 = c(rv$location2, sub.rv$location2)
        rv$stratification = c(rv$stratification, sub.rv$stratification)
    }
    
    
    if (by.race)
    {
        sub.rv = get.paired.msa.data.one.stratification(data.type=data.type,
                                                        years=years,
                                                        msa.surveillance=msa.surveillance,
                                                        other.surveillance=other.surveillance,
                                                        msa.to.other.code.mapping.fn=msa.to.other.code.mapping.fn,
                                                        eliminate.one.to.one.mappings=eliminate.one.to.one.mappings,
                                                        age=F,
                                                        race=T,
                                                        sex=F,
                                                        risk=F)
        
        rv$msa = c(rv$msa, sub.rv$msa)
        rv$other = c(rv$other, sub.rv$other)
        rv$location1 = c(rv$location1, sub.rv$location1)
        rv$location2 = c(rv$location2, sub.rv$location2)
        rv$stratification = c(rv$stratification, sub.rv$stratification)
    }
    
    
    if (by.sex)
    {
        sub.rv = get.paired.msa.data.one.stratification(data.type=data.type,
                                                        years=years,
                                                        msa.surveillance=msa.surveillance,
                                                        other.surveillance=other.surveillance,
                                                        msa.to.other.code.mapping.fn=msa.to.other.code.mapping.fn,
                                                        eliminate.one.to.one.mappings=eliminate.one.to.one.mappings,
                                                        age=F,
                                                        race=F,
                                                        sex=T,
                                                        risk=F)
        
        rv$msa = c(rv$msa, sub.rv$msa)
        rv$other = c(rv$other, sub.rv$other)
        rv$location1 = c(rv$location1, sub.rv$location1)
        rv$location2 = c(rv$location2, sub.rv$location2)
        rv$stratification = c(rv$stratification, sub.rv$stratification)
    }
    
    
    if (by.risk)
    {
        sub.rv = get.paired.msa.data.one.stratification(data.type=data.type,
                                                        years=years,
                                                        msa.surveillance=msa.surveillance,
                                                        other.surveillance=other.surveillance,
                                                        msa.to.other.code.mapping.fn=msa.to.other.code.mapping.fn,
                                                        eliminate.one.to.one.mappings=eliminate.one.to.one.mappings,
                                                        age=F,
                                                        race=F,
                                                        sex=F,
                                                        risk=T)
        
        rv$msa = c(rv$msa, sub.rv$msa)
        rv$other = c(rv$other, sub.rv$other)
        rv$location1 = c(rv$location1, sub.rv$location1)
        rv$location2 = c(rv$location2, sub.rv$location2)
        rv$stratification = c(rv$stratification, sub.rv$stratification)
    }
    
    # check for erroneous values
    if (all(names(DATA.TYPE.MAX)!=data.type))
        stop(paste0("'", data.type, "' is not a supported data type (missing min and max)"))
    
    mask = rv$msa >= DATA.TYPE.MIN[data.type] & rv$other >= DATA.TYPE.MIN[data.type] &
        rv$msa <= DATA.TYPE.MAX[data.type] & rv$other <= DATA.TYPE.MAX[data.type]
    rv$msa = rv$msa[mask]
    rv$other = rv$other[mask]
    rv$location1 = rv$location1[mask]
    rv$location2 = rv$location2[mask]
    rv$stratification = rv$stratification[mask]
    
    # check that we have something
    if (length(rv$msa)<min.obs)
    {
        if (eliminate.one.to.one.mappings)
            geography = 'county'
        else
            geography = 'state'
        
        stop(paste0("There are only ",
                    length(rv$msa), " data points available to estimate MSA-to-",
                    geography, " variation in '", data.type, "'"))
    }
    
    if (details)
        print(paste0("Using ", length(rv$msa), " data points for ", data.type))
    
    # Return
    rv
}

get.paired.msa.data.one.stratification <- function(data.type,
                                                   years=NULL,
                                                   msa.surveillance,
                                                   other.surveillance,
                                                   msa.to.other.code.mapping.fn,
                                                   eliminate.one.to.one.mappings,
                                                   age=F,
                                                   race=F,
                                                   sex=F,
                                                   risk=F)
{
    msas = get.locations.for.surveillance.data(msa.surveillance, data.type=data.type,
                                               sex=sex, age=age, race=race, risk=risk)
    
    data.for.msas = lapply(msas, get.surveillance.data,
                           surv=msa.surveillance,
                           data.type=data.type,
                           age=age, race=race, sex=sex, risk=risk,
                           years=years,
                           throw.error.if.missing.data=F)
    
    missing.all.mask = sapply(data.for.msas, function(data){
        is.null(data) || all(is.na(data))
    })

    msas = msas[!missing.all.mask]
    data.for.msas = data.for.msas[!missing.all.mask]
    
    rv = list(
        msa = numeric(),
        other = numeric(),
        location1 = character(),
        location2 = character()
    )
    
    if (length(msas)>0)
    {
        for (i in 1:length(msas))
        {
            msa = msas[[i]]
            msa.data = data.for.msas[[i]]
            
            other.location.codes = msa.to.other.code.mapping.fn(msa)
            
            if (length(other.location.codes)>1 || !eliminate.one.to.one.mappings)
            {
                for (other.code in other.location.codes)
                {
                    other.data = get.surveillance.data(other.surveillance, 
                                                       location.codes = other.code,
                                                       data.type=data.type,
                                                       years=dimnames(msa.data)$year,
                                                       age=age, race=race, sex=sex, risk=risk,
                                                       throw.error.if.missing.data = F)
                    
                    if (!is.null(other.data) && any(!is.na(other.data)))
                    {
                        both.not.missing = !is.na(msa.data) & !is.na(other.data)
                        
                        rv$msa = c(rv$msa, 
                                   as.numeric(msa.data[both.not.missing]))
                        rv$other = c(rv$other,
                                     as.numeric(other.data[both.not.missing]))
                        rv$location1 = c(rv$location1,
                                         rep(msa, sum(both.not.missing)))
                        rv$location2 = c(rv$location2,
                                        rep(other.code, sum(both.not.missing)))
                    }
                }
            }
        }
    }
    
    strat = c('age','race','sex','risk')[c(age,race,sex,risk)]
    if (length(strat)==0)
        strat = 'all'
    else
        strat = paste0(strat, collapse='.')
    
    rv$stratification = rep(strat, length(rv$location1))
    
    rv
}

get.paired.ratios.and.reference <- function(paired.data)
{
    ratios = paired.data$other / paired.data$msa
    
    if (1==2)
    {
        locations = unique(paired.data$location[paired.data$stratification=='all'])
    reference.by.location = sapply(locations, function(loc){
        mask = paired.data$location==loc & paired.data$stratification=='all'
        mean(ratios[mask])
    })
    
    references = reference.by.location[paired.data$location]

    }
    
    references = sapply(1:length(ratios), function(i){
        mask = paired.data$location1 == paired.data$location1[i] &
            paired.data$location2 == paired.data$location2[i] &
            paired.data$stratification == paired.data$stratification[i]
        
        if (sum(mask)<2)
            NA
        else
            mean(ratios[mask])
    })
    
    
    missing = is.na(references) | is.na(ratios) |
        is.infinite(references) | is.infinite(ratios) |
        references == 0 | ratios == 0
    
    list(
        ratio = ratios[!missing],
        reference = references[!missing]
    )
}

#-- A Cache (so we don't have to calculate these values repeatedly) --#

make.nested.errors.cache <- function(msa.surveillance,
                                     state.surveillance,
                                     county.surveillance,
                                     variance.data.types=c('suppression','diagnosed','engagement','linkage'),
                                     bias.data.types=variance.data.types,
                                     ratio.cv.data.types=c('new','prevalence'),
                                     years=NULL,
                                     verbose = F)
{
    cache = list()
    
    for (data.type in variance.data.types)
    {
        if (verbose)
            print(paste0("Cacheing outcome variance for ", data.type))
        
        state.value = get.state.to.msa.outcome.variance(data.type=data.type,
                                                        msa.surveillance = msa.surveillance,
                                                        state.surveillance = state.surveillance,
                                                        years=years,
                                                        details=verbose)
        cache = cache.nested.error(cache,
                                   geography='state', data.type=data.type, statistic='variance', years=years,
                                   value=state.value)
        
        county.value = get.county.to.msa.outcome.variance(data.type=data.type,
                                                          msa.surveillance = msa.surveillance,
                                                          county.surveillance = county.surveillance,
                                                          state.surveillance = state.surveillance,
                                                          years=years,
                                                          details=verbose)
        cache = cache.nested.error(cache,
                                   geography='county', data.type=data.type, statistic='variance', years=years,
                                   value=county.value)
    }
    
    for (data.type in bias.data.types)
    {
        if (verbose)
            print(paste0("Cacheing outcome bias for ", data.type))
        
        state.value = get.state.to.msa.outcome.bias(data.type=data.type,
                                                    msa.surveillance = msa.surveillance,
                                                    state.surveillance = state.surveillance,
                                                    years=years,
                                                    details=verbose)
        cache = cache.nested.error(cache,
                                   geography='state', data.type=data.type, statistic='bias', years=years,
                                   value=state.value)
        
#        county.value = get.county.to.msa.outcome.bias(data.type=data.type,
#                                                      msa.surveillance = msa.surveillance,
#                                                      county.surveillance = county.surveillance,
#                                                      years=years,
#                                                      details=verbose)
 #       cache = cache.nested.error(cache,
#                                   geography='county', data.type=data.type, statistic='bias', years=years,
#                                   value=county.value)
    }

    for (data.type in ratio.cv.data.types)
    {
        if (verbose)
            print(paste0("Cacheing outcome ratio cv for ", data.type))
        
        state.value = get.state.to.msa.outcome.ratio.cv(data.type=data.type,
                                                        msa.surveillance = msa.surveillance,
                                                        state.surveillance = state.surveillance,
                                                        years=years,
                                                        details=verbose)
        cache = cache.nested.error(cache,
                                   geography='state', data.type=data.type, statistic='ratio.cv', years=years,
                                   value=state.value)
        
        county.value = get.county.to.msa.outcome.ratio.cv(data.type=data.type,
                                                         msa.surveillance = msa.surveillance,
                                                         county.surveillance = county.surveillance,
                                                         years=years,
                                                         details=verbose)
        cache = cache.nested.error(cache,
                                   geography='county', data.type=data.type, statistic='ratio.cv', years=years,
                                   value=county.value)
    }
    
    cache
}

cache.nested.error <- function(cache,
                               geography,
                               data.type,
                               statistic,
                               years,
                               value)
{
    cache[[get.nested.error.cache.name(geography=geography, data.type=data.type, statistic=statistic, years=years)]] = value
    cache
}

get.cached.nested.error <- function(cache,
                                    geography,
                                    data.type,
                                    statistic,
                                    years=NULL)
{
    cache[[get.nested.error.cache.name(geography=geography, data.type=data.type, statistic=statistic, years=years)]]
}

get.nested.error.cache.name <- function(geography, data.type, statistic, years)
{
    if (is.null(years))
        years = '<none>'
    else
        years = paste0(years, collapse='.')
        
    paste0(geography, "__", data.type, "__", statistic, "__", years)
}