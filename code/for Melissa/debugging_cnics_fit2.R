
source('code/for Melissa/process_multinomial_fit.R')

#get from the cnics regression code:
# imputed.engaged.suppressed

sapply(sort(unique(imputed.engaged.suppressed$relative.year)), function(year){
    mask = imputed.engaged.suppressed$relative.year==year
    mean(imputed.engaged.suppressed$future.state[mask]=='remain')
})
sapply(sort(unique(imputed.engaged.suppressed$relative.year)), function(year){
    mask = imputed.engaged.suppressed$relative.year==year
    mean(imputed.engaged.suppressed$future.state[mask]=='unsuppress')
})
sapply(sort(unique(imputed.engaged.suppressed$relative.year)), function(year){
    mask = imputed.engaged.suppressed$relative.year==year
    mean(imputed.engaged.suppressed$future.state[mask]=='lost')
})


get.ref.stratum = function(arr){
    arr[3,'other','msm','never_IDU']
}

test.pred = function(fit=NULL,
                     fn=mean,#get.ref.stratum,
                     use.counts=F,
                     dataset=imputed.engaged.unsuppressed,
                     coefs=fit$coefficients)
{
    ps = generate.probs(coefs)
    
    if (use.counts)
    {
        counts = make.df.counts(df=dataset)
        print("REF: ")
        print(apply(ps$p.ref*counts, 'year', sum) / apply(counts, 'year', sum))
        
        print("Outcome 1:")
        print(apply(ps$p.1*counts, 'year', sum) / apply(counts, 'year', sum))
        
        print("Outcome 2:")
        print(apply(ps$p.2*counts, 'year', sum) / apply(counts, 'year', sum))
    }
    else
    {
        print("REF: ")
        print(apply(ps$p.ref, 'year', fn))
        
        print("Outcome 1:")
        print(apply(ps$p.1, 'year', fn))
        
        print("Outcome 2:")
        print(apply(ps$p.2, 'year', fn))
    }
}

model.engaged.suppressed <- nomLORgee(future.state ~ age.category + sex.risk + race + relative.year
                                      + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                      data=imputed.engaged.suppressed, id=id)
test.pred(model.engaged.suppressed)

model.slope.int <- nomLORgee(future.state ~ relative.year,
                                      data=imputed.engaged.suppressed, id=id)
test.pred(model.slope.int)

model.one.slope <- nomLORgee(future.state ~ age.category + sex.risk + race + relative.year
                                      data=imputed.engaged.suppressed, id=id)
test.pred(model.one.slope)

fit.multinom.slope.int = multinom(future.state ~ relative.year,
                                  model=T,
         data=imputed.engaged.suppressed)

for.prediction = data.frame(
    relative.year=0:10,
    age.category='35-45',
    sex.risk='msm',
    race='other'
)

multinom.coefs = coef(fit.multinom.slope.int)
multinom.coefs = c(
    beta10 = multinom.coefs[1,1],
    beta20 = multinom.coefs[2,1],
    'relative.year:1' = multinom.coefs[1,2],
    'relative.year:2' = multinom.coefs[2,2]
)
test.pred(coefs=multinom.coefs)

multinom.coefs = coef(fit.multinom.slope.int)
multinom.coefs = c(
    beta10 = multinom.coefs[1,1],
    beta20 = multinom.coefs[2,1],
    'relative.year:1' = multinom.coefs[1,2],
    'relative.year:2' = multinom.coefs[2,2]
)
multinom.full = multinom(future.state ~ age.category + sex.risk + race + relative.year
                          + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                          data=imputed.engaged.suppressed)

#ref is unsuppress
#1 is lost
#2 is remain
test.pred(coefs=multinom.coefs)


test.pred(coefs=old$engaged.unsuppressed.coefficients)
test.pred(coefs=output$engaged.unsuppressed.coefficients)

test.pred(coefs=old$engaged.suppressed.coefficients)
test.pred(coefs=output$engaged.suppressed.coefficients)




sapply(sort(unique(disengaged$relative.year)), function(year){
    mask = disengaged$relative.year==year
    sum(disengaged$p.truly.disengaged[mask] * disengaged$reengage[mask]) / sum(disengaged$p.truly.disengaged[mask])
})


for.prediction = data.frame(
    relative.year = 0:20,
    sex.risk='msm',
    age.category='35-45',
    race='other'
)

expit(predict(model.disengaged, for.prediction))
expit(predict(model.disengaged.simple, for.prediction))
expit(predict(model.disengaged.noslope, for.prediction))


expit(range(predict(model.disengaged.noslope, disengaged)))
