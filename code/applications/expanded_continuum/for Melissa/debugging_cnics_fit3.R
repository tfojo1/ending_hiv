

##-- GENERAL HELPERS --##

logit = function(p){log(p) - log(1-p)}
expit = function(lo){1/(1+exp(-lo))}


expit(-3.03 + 10 * -.04)


quick.p = function(coefs, year=2020, anchor.year=2010) {
    expit(coefs[1] + (year-anchor.year) * coefs['relative.year'])
}

if (1==2)
{
    1-quick.p(output$failing.to.lost.noslopes.coefficients)
    1-quick.p(output$naive.to.lost.noslopes.coefficients)
    1-quick.p(output$suppressed.to.lost.noslopes.coefficients)
}


##-- CHECK AGAINST --##

mean(imputed.unsuppressed.failing$lost.future)
mean(unsuppressed.failing$future.state=='lost')


mean(unsuppressed.naive$future.state=='lost')

mean(engaged.suppressed$future.state=='lost')



##-- SLOPPY TESTING --##

unsuppressed.failing$lost.future = unsuppressed.failing$future.state=='lost'

fit = geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
       data=unsuppressed.failing, id=id, family=binomial, corstr="exchangeable")

unfiltered.coefs = fit$coefficients
1-quick.p(unfiltered.coefs)


shared.columns = intersect(names(engaged.suppressed), names(unsuppressed.failing))
shared.columns = intersect(shared.columns, names(unsuppressed.naive))
big.df = rbind(
    engaged.suppressed[,shared.columns],
    unsuppressed.failing[,shared.columns],
    unsuppressed.naive[,shared.columns]
)
big.df$lost.future = big.df$future.state=='lost'

#restrict to after 2012
big.df = big.df[big.df$relative.year>2 & big.df$relative.year<8,]

#check by year
years = sort(unique(big.df$relative.year))
cbind(years, sapply(years, function(y){mean(big.df$lost.future[big.df$relative.year==y])}))


fit = geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
             data=big.df, id=id, family=binomial, corstr="exchangeable")
quick.p(fit$coefficients)
mean(big.df$lost.future)


desired.p.retained = 0.85
desired.p.lost = 1-desired.p.retained
actual.p.lost = mean(big.df$lost.future)

desired.lo.lost = logit(desired.p.lost)
actual.lo.lost = logit(actual.p.lost)

lo.correction = desired.lo.lost - actual.lo.lost


expit(fit$coefficients[1])
expit(output$failing.to.lost.noslopes.coefficients[1])


#


unfiltered.coefs = fit$coefficients