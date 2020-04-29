library(ggplot2)

if (1==2)
{
    source('../code/check_inner_workings/run_sim_for_proportions.R')
    plot.sim.setting.proportions(sim)
    plot.sim.setting.proportions(sim, facet.by='risk')
    plot.sim.setting.proportions(sim, facet.by='sex')

    plot.sim.setting.proportions(sim, which='testing')
    plot.sim.setting.proportions(sim, which='testing', facet.by='sex')
    plot.sim.setting.proportions(sim, which='testing', facet.by='risk')
    plot.sim.setting.proportions(sim, which='testing', split.by='age')
    plot.sim.setting.proportions(sim, which='testing', facet.by='age', split.by=c('race','sex'))
}

plot.sim.setting.proportions <- function(sim,
                                         which=c('suppression','testing')[1],
                                         years=NULL,
                                         x.by='year',
                                         facet.by=character(),
                                         split.by='race'
                                         )
{
    keep.dimensions = c(x.by, facet.by, split.by)
    if (which=='suppression')
        proportions = extract.suppressed.proportions(sim,
                                                     keep.dimensions = keep.dimensions,
                                                     years = years)
    else if (which=='testing')
        proportions = extract.testing.proportions(sim,
                                                  keep.dimensions = keep.dimensions,
                                                  years = years)
    else
        stop("only suppression and testing are supported for plot.sim.setting.proportions")

    prop.name = c(suppression='Suppressed', testing='Tested (in past year)')
    y.name = paste0('Proportion ', prop.name[which])

    df = melt(proportions, value.name = 'Proportion')
    df$x = df[,x.by]

    do.split = !is.null(split.by) && length(split.by) > 0
    do.facet = !is.null(facet.by) && length(facet.by) > 0

    if (do.split)
    {
        df$Category = df[,split.by[1]]
        if (length(split.by)>1)
        {
            for (i in 2:length(split.by))
                df$Category = paste0(df$Category, ", ", df[,split.by[i]])
        }
    }

    if (do.split)
        rv = ggplot(df, aes(x, Proportion, color=Category))
    else
        rv = ggplot(df, aes(x, Proportion))

    rv = rv + geom_line(size=1) + geom_point(size=4) +
        ylab(y.name) + ylim(0,1)

    if (do.facet)
        rv = rv + facet_wrap(as.formula(paste0('~', paste0(facet.by, collapse='+'))))

    rv
}

extract.suppressed.proportions <- function(sim,
                                           keep.dimensions=c('year','race'),
                                           years=NULL)
{
    proportions = attr(sim, 'smoothed.suppressed.proportions')


    if (is.null(years))
        years = intersect(dimnames(proportions)[['year']], as.character(sim$years))
    else
        years = as.character(years)
    sim.years = intersect(years, as.character(sim$years))

    population = extract.population.subset(sim,
                                           years=sim.years,
                                           keep.dimensions = names(dimnames(proportions)),
                                           include.hiv.positive = T,
                                           include.hiv.negative = F,
                                           continuum='diagnosed')

    aggregate.proportions(proportions, population, keep.dimensions, years)

}

extract.testing.proportions <- function(sim,
                                           keep.dimensions=c('year','race'),
                                           years=NULL)
{
    proportions = attr(sim, 'smoothed.testing.proportions')


    if (is.null(years))
        years = intersect(dimnames(proportions)[['year']], as.character(sim$years))
    else
        years = as.character(years)
    sim.years = intersect(years, as.character(sim$years))

    population = extract.population.subset(sim,
                                           years=sim.years,
                                           keep.dimensions = names(dimnames(proportions)))

    aggregate.proportions(proportions, population, keep.dimensions, years)

}

aggregate.proportions <- function(proportions,
                                population,
                                keep.dimensions,
                                years=NULL)
{
    if (!setequal(years, dimnames(population)[['year']]))
    {
        new.pop.dim.names = dimnames(population)
        new.pop.dim.names[['year']] = as.character(years)

        new.pop = array(access(population, year=max(dimnames(population)[['year']])),
                        dim=sapply(new.pop.dim.names, length), dimnames = new.pop.dim.names)

        population = new.pop
    }

    dim.names = dimnames(proportions)
    dim.names[['year']] = years
    proportions = proportions[years,,,,]
    dim(proportions) = sapply(dim.names, length)
    dimnames(proportions) = dim.names

    population = apply(population, names(dimnames(proportions)), sum)
    dim(population) = sapply(dim.names, length)
    dimnames(population) = dim.names

    dim.names = dim.names[keep.dimensions]
    numerators = apply(proportions * population, keep.dimensions, sum)
    denominators = apply(population, keep.dimensions, sum)
    dim(numerators) = dim(denominators) = sapply(dim.names, length)
    dimnames(numerators) = dimnames(denominators) = dim.names

    numerators/denominators
}
