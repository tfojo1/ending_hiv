DEFAULT.TARGET.POPULATIONS = c('Young Black MSM',
                               'Black MSM',
                               #                               'Young Black MSM or IDU',
                               #                               'Black MSM or IDU',
                               'All Black <35',
                               'All <35',
                               'All MSM',
                               'All MSM or IDU',
                               'Everyone'
)

if (1==2)
{
    summ = parse.intervention.tables()
    summ = parse.intervention.tables(dir='../code/results//baltimore_v1 - Copy/')



    load('../code/results/baltimore_v1/no.intervention.Rdata')
    get.simset.incidence.reduction(simset)
    get.simset.incidence.reduction(simset, 2016,2020)

}





parse.intervention.tables <- function(target.populations = DEFAULT.TARGET.POPULATIONS,
                                      testing.rates = c(NA,NA,1,1,1,1,1),
                                      suppressed.proportions = c(NA,.8,.8,.8,.9,.9,.9),
                                      prep.coverages=c(NA,NA,.25,.5,.25,.5,.75),
                                      dir='../code/results/baltimore_v1',
                                      ci.coverage=0.95)
{
    intervention.level.names = paste0('t',testing.rates,
                                      '_s',100*suppressed.proportions,
                                      '_p',100*prep.coverages)
    n.intervention.levels = length(intervention.level.names)
    n.targets = length(target.populations)

    means = ci.lowers = ci.uppers = matrix(as.numeric(NA),
                                           nrow=n.targets,
                                           ncol=n.intervention.levels,
                                           dimnames=list(target.populations, intervention.level.names))


    for (i in 1:n.intervention.levels)
    {
        for (t.pop in target.populations)
        {
            if (is.na(testing.rates[i]) && is.na(suppressed.proportions[i]) && is.na(prep.coverages[i]))
            {
                intervention = NULL
                filename = 'no.intervention'
            }
            else
            {
                intervention = create.one.intervention(t.pop,
                                                       testing.frequency = testing.rates[i],
                                                       suppressed.proportion = suppressed.proportions[i],
                                                       prep.coverage = prep.coverages[i])
                filename = get.intervention.filename(intervention)
            }

            filename = file.path(dir,
                                 paste0(filename,
                                        ".Rdata"))

            if (file.exists(filename))
            {
                print(paste0("Parsing ", filename))
                load(filename)
                inc.red = get.simset.incidence.reduction(simset, ci.coverage=ci.coverage)

                if (is.null(intervention))
                    mask = 1:n.targets
                else
                    mask = t.pop

                means[mask,i] = inc.red['mean.reduction']
                ci.lowers[mask,i] = inc.red['ci.lower']
                ci.uppers[mask,i] = inc.red['ci.upper']

                if (is.null(intervention))
                    break
            }
            else
                print(paste0("The file '", filename, "' does not exist"))
        }
    }

    list(means=means,
         ci.lowers=ci.lowers,
         ci.uppers=ci.uppers)
}

make.text.table.one.row.per <- function(summ,
                            digits=1,
                            ci.sep=',',
                            final.pct.in.ci=T)
{
    rv = paste0(round(100*summ$means, digits=digits),
           "% [",
           round(100*summ$ci.lowers, digits=digits),
           ci.sep,
           round(100*summ$ci.uppers, digits=digits))

    if (final.pct.in.ci)
        rv = paste0(rv, '%')

    rv = paste0(rv, "]")

    rv[is.na(summ$means)] = '-'

    dim(rv) = dim(summ$means)
    dimnames(rv) = dimnames(summ$means)

    rv
}

make.text.table.two.rows.per <- function(summ,
                                        digits=1,
                                        ci.sep=',',
                                        final.pct.in.ci=T)
{
    means = paste0(round(100*summ$means, digits=digits),
                "%")
    cis = paste0("[",
                round(100*summ$ci.lowers, digits=digits),
                ci.sep,
                round(100*summ$ci.uppers, digits=digits))

    if (final.pct.in.ci)
        cis = paste0(cis, '%')

    cis = paste0(cis, "]")

    means[is.na(summ$means)] = cis[is.na(summ$means)] = '-'

    dim(means) = dim(cis) = dim(summ$means)
    dimnames(means) = dimnames(cis) = dimnames(summ$means)

    rv = matrix('', nrow=2*nrow(means), ncol=ncol(means),
                dimnames=list(paste0(rep(dimnames(means)[[1]], each=2), rep(c('','.ci'), nrow(means))),
                              dimnames(means)[[2]]))

    rv[2*(1:nrow(means))-1,] = means
    rv[2*(1:nrow(means)),] = cis
    rv
}
