

check.total.smoothed.suppression <- function(sim)
{
    location = attr(sim, 'location')
    components = attr(sim, 'components')
    
    true.lo = ALL.DATA.MANAGERS$continuum$suppression$total.mean.log.odds[,location,'total']
    true.p = 1 / (1+exp(-lo))
    
    years = as.numeric(names(true.p))[!is.na(true.p)]
    true.p = true.p[as.character(years)]
    
    smoothed.supp = attr(components, 'smoothed.suppressed.proportions')[as.character(years),,,,]
    
    prev = extract.prevalence(sim, years=years, keep.dimensions = c('year','age','race','sex','risk'))
    
    smoothed.total.supp = apply(smoothed.supp * prev, 'year', sum) / apply(prev, 'year', sum)
    
    qplot(rep(years, 2),
          c(true.p, smoothed.total.supp),
          color=rep(c("True P", "Smoothed P"),each=length(years)),
          geom='line') + ylim(0,1)
}

get.model.suppression <- function(sim,
                                  keep.dimensions = c(),
                                  years=2010:2020,
                                  return.as.array=T)
{
    keep.dimensions = unique(c('year', keep.dimensions))
    components = attr(sim, 'components')
    smoothed.supp = attr(components, 'smoothed.suppressed.proportions')[as.character(years),,,,]
    
    prev = extract.prevalence(sim, years=years, keep.dimensions = c('year','age','race','sex','risk'))
    
    rv = apply(smoothed.supp * prev, keep.dimensions, sum) / apply(prev, keep.dimensions, sum)
    
    if (return.as.array && length(keep.dimensions)==1)
    {
        dim.names = dimnames(smoothed.supp)[keep.dimensions]
        dim(rv) = sapply(dim.names, length)
        dimnames(rv) = dim.names
    }
    
    rv
}

plot.model.suppression <- function(sim,
                                   facet.by = 'risk',
                                   split.by = 'age',
                                   years=2010:2020)
{
    if (is.null(facet.by))
        facet.by = character()
    if (is.null(split.by))
        split.by = character()
    
    keep.dimensions=c(facet.by, split.by)
    smoothed.supp = get.model.suppression(sim=sim,
                                          keep.dimensions = keep.dimensions,
                                          years=years,
                                          return.as.array = T)
    
    df = melt(smoothed.supp)
    
    if (length(split.by)==0)
        df$split.by = 'all'
    else if (length(split.by)==1)
        df$split.by = df[,split.by]
    else
        df$split.by = apply(df[,split.by], 1, function(z){paste0(z, collapse=', ')})
    
    if (length(facet.by)==0)
        df$facet.by = 'all'
    else if (length(facet.by)==1)
        df$facet.by = df[,facet.by]
    else
        df$facet.by = apply(df[,facet.by], 1, function(z){paste0(z, collapse=', ')})
    
    ggplot(df, aes(year, value, color=split.by)) + geom_line() + geom_point() +
        facet_wrap(~facet.by) + ylim(0,1)
}