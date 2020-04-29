
get.top.params <- function(params,
                          use.sampling.cov=T,
                          n=20)
{
    params = match.variable.names(to.match=params,
                                  possible.matches=mcmc@var.names,
                                  exact=F,
                                  to.match.name='params')

    if (use.sampling.cov)
        cov.mat = mcmc@chain.states[[1]]@cov.mat
    else
        cov.mat = cov(mcmc@samples[1,,])
    cor.mat = cov2cor(cov.mat)

    rv = lapply(params, function(param){
        match = cor.mat[param,]
        match = match[match != 1]

        match[order(abs(match), decreasing = T)][1:n]
    })
    names(rv) = params
    rv
}

match.variable.names <- function(to.match, possible.matches, exact,
                           to.match.name='var.names',
                           category.descriptor='variable')
{
    if (is(to.match, 'integer') ||
        (is(to.match, 'numeric') && all(round(to.match)==to.match)))
    {
        if (any(is.na(to.match)))
            stop(paste0("'", to.match, "' cannot contain NA values"))
        else if (any(to.match<1) || any(to.match>length(possible.matches)))
            stop("Indices for '", to.match.name, "' must be between 1 and ", length(possible.matches))

        rv = possible.matches[to.match]
    }
    else
    {
        if (!is(to.match, 'character'))
            stop(paste0(to.match.name, " must be either a character/character vector or an integer vector of indices"))
        else if (length(to.match)==0)
            stop(paste0(to.match.name, " cannot have length 0"))
        else if (any(is.na(to.match)))
            stop(paste0("'", to.match, "' cannot contain NA values"))

        if (exact)
            rv = intersect(to.match, possible.matches)
        else
        {
            regexes = gsub("\\*", ".*", to.match)
            regexes = paste0("^", regexes, ".*$")
            if (length(regexes)==1)
                matches = grepl(regexes, possible.matches, ignore.case = T)
            else
                matches = apply(sapply(regexes, function(regex){
                    grepl(regex, possible.matches, ignore.case = T)
                }), 1, any)

            rv = possible.matches[matches]
        }

        if (length(rv)==0)
        {
            if (exact)
                match.str = 'are named'
            else if (length(possible.matches)==1)
                match.str = 'match the pattern'
            else
                match.str = 'match the patterns'

            if (length(to.match)==1)
                stop(paste0("No ", category.descriptor,
                            "s in the given mcmc ",
                            match.str, " '", to.match, "'"))
            else if (length(to.match)==2)
                stop(paste0("No ", category.descriptor,
                            "s in the given mcmc ",
                            match.str, " '", to.match[1],
                            "' or '", to.match[2], "'"))
            else
                stop(paste0("No ", category.descriptor,
                            "s in the given mcmc ",
                            match.str, " ",
                            paste0("'", to.match[-length(to.match)], "'", collapse=", "),
                            ", or '", to.match[length(to.match)], "'"))
        }
    }

    unique(rv)
}

top=get.top.params(mcmc@var.names,T)
top[1]
