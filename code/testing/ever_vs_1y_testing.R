
source('code/core_code/data_managers/continuum_manager_3.R')
dir='cleaned_data'
dfs = read.nhbs.testing(dir=file.path(dir, 'continuum/national/testing'), verbose=T)
df.msa = dfs$msa

logit = function(p){log(p)-log(1-p)}
expit = function(lo){1/(1+exp(-lo))}
to.rate = function(p){-log(1-p)}

tsfx.fn = list(
    'identity' = function(x){x},
    'q' = function(p){1-p},
    'odds' = function(p){1/(1-p)},
    'log' = log,
    'logit' = logit,
    'rate' = function(p){-log(1-p)}
)

undo.tsfx = list(
    'identity' = function(x){x},
    'q' = function(q){1-q},
    'odds' = function(o){o/(1+o)},
    'log' = exp,
    'logit' = expit,
    'rate' = function(r){1-exp(-r)}
)

transformations = names(tsfx.fn)

#check
sapply(1:length(transformations), function(i){
    vals = c(.33, .45, .75, .97)
    undo.tsfx[[i]](tsfx.fn[[i]](vals))
})

df.to.plot = NULL

for (tsfx in transformations)
{
    df.to.plot = rbind(
        df.to.plot,
        data.frame(one.yr=tsfx.fn[[tsfx]](df.msa$frac.12mo),
                   ever=tsfx.fn[[tsfx]](df.msa$frac.ever),
                   tsfx=tsfx)
    )
}

library(ggplot2)
ggplot(df.to.plot, aes(one.yr, ever)) + geom_point() + facet_wrap(~tsfx, scales='free') + geom_smooth()

fits.for.tsfx = lapply(transformations, function(tsfx){
    df = df.to.plot[df.to.plot$tsfx==tsfx,]
    fit=lm(ever~one.yr+0, data=df)
    fit$coefficients = c(0, fit$coefficients)
    fit
})
names(fits.for.tsfx) = transformations

df.fit = NULL
for (tsfx in transformations)
{
    df.fit = rbind(
        df.fit,
        data.frame(
            int=fits.for.tsfx[[tsfx]]$coefficients[1],
            slope=fits.for.tsfx[[tsfx]]$coefficients[2],
            tsfx=tsfx
        )
    )
}


ggplot(df.to.plot, aes(one.yr, ever)) + geom_point() + facet_wrap(~tsfx, scales='free') +
    geom_abline(data=df.fit, aes(intercept = int, slope=slope))


df.fitted = NULL
for (tsfx in transformations)
{
    truth = df.msa$frac.ever
    fitted = undo.tsfx[[tsfx]](
        fits.for.tsfx[[tsfx]]$coefficients[1] +
            fits.for.tsfx[[tsfx]]$coefficients[2] * 
            tsfx.fn[[tsfx]](df.msa$frac.12mo)
    )
    df.fitted = rbind(
        df.fitted,
        data.frame(
            truth=truth,
            fitted=fitted,
            tsfx=tsfx
        )
    )
}


ggplot(df.fitted, aes(truth, fitted)) + geom_point() + facet_wrap(~tsfx, scales='free') +
    geom_smooth() + geom_abline(intercept = 0, slope=1)


rmse.by.tsfx = sapply(transformations, function(tsfx){
    pred = df.fitted$fitted[df.fitted$tsfx==tsfx]
    truth = df.fitted$truth[df.fitted$tsfx==tsfx]
    
    sqrt(mean( ((pred-truth)^2)[pred>.6] ))
});print(rmse.by.tsfx)


t(sapply(fits.for.tsfx, function(fit){fit$coefficients}))
