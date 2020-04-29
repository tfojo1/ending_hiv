
library(ggplot2)
source('../code/setup/setup_helpers.R')

if (1==2)
{
    p = attr(components, 'smoothed.testing.proportions')
    r = attr(components, 'smoothed.testing.rates')

    plot.smoothed.proportions(r, split.by='sex', ages='35-44 years') + geom_vline(xintercept=2017)

    r2 = r * 0.25
    p2 = 1-exp(-r2)
    plot.smoothed.proportions(p2, split.by='sex', ages='35-44 years') + geom_vline(xintercept=2017)
    plot.smoothed.proportions(r2, split.by='sex', ages='35-44 years') + geom_vline(xintercept=2017)

    p3 = p * 0.5
    r3 = -log(1-p3)
    plot.smoothed.proportions(r3, split.by='sex', ages='35-44 years') + geom_vline(xintercept=2017)


    plot.smoothed.proportions(attr(components, 'smoothed.testing.proportions'), split.by='age', races='black', sexes='msm') + geom_vline(xintercept=2017)
    plot.smoothed.proportions(attr(components, 'smoothed.testing.proportions'), split.by='sex', ages='35-44 years') + geom_vline(xintercept=2017)

    plot.smoothed.proportions(attr(components, 'raw.suppressed.proportions'), split.by='age', races='black', sexes='msm') + geom_vline(xintercept=2017)
    plot.smoothed.proportions(attr(components, 'smoothed.suppressed.proportions'), split.by='age', races='black', sexes='msm') + geom_vline(xintercept=2017)
}

plot.smoothed.proportions <- function(arr,
                                      split.by='risk',
                                      facet.by=setdiff(c('age','race','sex','risk'), split.by),
                                      ages=NULL,
                                      races=NULL,
                                      sexes=NULL,
                                      risks=NULL)
{
    print(paste0("Split by: ", paste0(split.by, collapse=', ')))
    print(paste0("Facet by: ", paste0(facet.by, collapse=', ')))

    df = melt(arr)

    if (!is.null(ages))
        df = df[sapply(df$age, function(one.age){any(one.age==ages)}),]
    if (!is.null(races))
        df = df[sapply(df$race, function(one.race){any(one.race==races)}),]
    if (!is.null(sexes))
        df = df[sapply(df$sex, function(one.sex){any(one.sex==sexes)}),]
    if (!is.null(risks))
        df = df[sapply(df$risk, function(one.risk){any(one.risk==risks)}),]

    df$split.by = sapply(1:dim(df)[1], function(i){
        paste0(split.by, '=', df[i,split.by], collapse=',')
    })


    ggplot(df, aes(year, value, color=split.by, fill=split.by)) +
        geom_line() + geom_point() +
        facet_wrap(as.formula(paste0('~', paste0(facet.by, collapse='+')))) +
        ylim(0,NA)
}
