MELISSAS.FILE = "~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/CNICS/synthetic_fixed_2021-10-04.Rdata"
TODDS.FILE = '../../CNICS/cleaned_datasets/arch68/cnics_fixed_2021-10-04.Rdata'
if (file.exists(MELISSAS.FILE))
{
    load(MELISSAS.FILE)
    print("Using synthetic data")
    dataset.type='synthetic'
}
if (file.exists(TODDS.FILE))
{
    load(TODDS.FILE)
    print("Using real data")
    dataset.type='real'
}

library(nnet)
library(multgee)
library(gee)
library(geepack)

analysis = 'jheem.model'
# analysis = 'CNICS'
just.do.multinomial = T

##----------------------------------##
##---------- Data cleaning----------##
##----------------------------------##

#A quick fix to the mis-named age.category field
# (the synthetic dataset erroneously labels this 'age.category.randomized')
if (any(names(dataset)=='age.category.randomized'))
    names(dataset)[names(dataset)=='age.category.randomized'] = 'age.category'

dataset <- dataset[dataset$sex!="Intersexed",]
dataset <- dataset[dataset$age.category!="0-13",]

dataset$age.category <- factor(dataset$age.category, levels = c("35-45","13-25","25-35","45-55","55+"))
dataset$sex <- factor(dataset$sex, levels = c("Male","Female"))

anchor.year = 2010
dataset$relative.year <- dataset$date - anchor.year

## Creating sex.risk categories ##
for (i in 1:nrow(dataset)) {
    if(dataset$risk[i]=="msm")  {
        dataset$sex.risk[i]="msm"
        
    } else if(dataset$risk[i]=="msm_idu")  {
        dataset$sex.risk[i]="msm_idu"
        
    } else if(dataset$sex[i]=="Male" && dataset$risk[i]=="heterosexual")  {
        dataset$sex.risk[i]="heterosexual_male"
        
    } else if(dataset$sex[i]=="Female" && dataset$risk[i]=="heterosexual")  {
        dataset$sex.risk[i]="heterosexual_female"
        
    } else if(dataset$sex[i]=="Male" && dataset$risk[i]=="idu")  {
        dataset$sex.risk[i]="idu_male"
        
    } else if(dataset$sex[i]=="Female" && dataset$risk[i]=="idu")  {
        dataset$sex.risk[i]="idu_female"
        
    } else if(dataset$risk[i]=="other")  {
        dataset$sex.risk[i]="other"
            
    } else 
        dataset$sex.risk[i]="missing"
}

## Defining engagement criteria; disengagement time ##
print("Defining engagement criteria and disengagement time")
for (i in 1:nrow(dataset)) {
    if(dataset$vl.now[i]==TRUE && dataset$visits.now[i]==TRUE)  {
        dataset$engaged.now[i]=TRUE
 
    } else 
        dataset$engaged.now[i]=FALSE
    
}

for (i in 1:nrow(dataset)) {
    if(dataset$vl.future[i]==TRUE && dataset$visits.future[i]==TRUE)  {
        dataset$engaged.future[i]=TRUE
        
    } else 
        dataset$engaged.future[i]=FALSE
    
}

for (i in 1:nrow(dataset)) {
    if(!is.na(dataset$years.since.vl.and.visit[i]) && dataset$years.since.vl.and.visit[i]<=2)  {
        dataset$disengaged.category[i] = "0-2"
        
    } else 
        dataset$disengaged.category[i]=">2"
    
}



print("Preparing Dataset for disengaged, for fitting the probability of true disengage")
disengaged <-dataset[dataset$engaged.now==FALSE & dataset$disengaged.category=="0-2",]

for (i in 1:nrow(disengaged)) {
    if(!is.na(disengaged$engaged.future[i]) && disengaged$engaged.future[i]==FALSE)  {
        disengaged$future.state[i]="remain"
        
    } else if(!is.na(disengaged$engaged.future[i]) && !is.na(disengaged$suppressed.future[i]) &&
              disengaged$engaged.future[i]==TRUE && disengaged$suppressed.future[i]==FALSE)  {
        disengaged$future.state[i]="reengage.unsuppress"
        
    } else if(!is.na(disengaged$engaged.future[i]) && !is.na(disengaged$suppressed.future[i]) &&
              disengaged$engaged.future[i]==TRUE && disengaged$suppressed.future[i]==TRUE)  {
        disengaged$future.state[i]="reengage.suppress"
        
    } else 
        disengaged$future.state[i]="missing"
}


disengaged <-disengaged[disengaged$future.state!="missing",]
disengaged$future.state <- factor(disengaged$future.state, levels = c("reengage.unsuppress","reengage.suppress", "remain"))
disengaged$age.category <- factor(disengaged$age.category, levels = c("35-45","13-25","25-35","45-55","55+"))
disengaged$sex <- factor(disengaged$sex, levels = c("Male","Female"))
disengaged$race <- factor(disengaged$race, levels = c("other","black","hispanic"))
disengaged$risk <- factor(disengaged$risk, levels = c("heterosexual","idu","msm","msm_idu","other"))
disengaged$sex.risk <- factor(disengaged$sex.risk, levels = c("msm","msm_idu",
                                                              "heterosexual_male","heterosexual_female",
                                                              "idu_male","idu_female","other"))

disengaged.for.weights = disengaged[!is.na(disengaged$future.state) & (disengaged$future.state=='reengage.suppress' | disengaged$future.state=='reengage.unsuppress'),]

expit = function(lo){1/(1+exp(-lo))}
model.suppressed.if.engaged = geeglm(suppressed.now ~ age.category + sex.risk + race +
                                         relative.year + relative.year:age.category + relative.year:sex.risk + relative.year:race,
                                     data=dataset[dataset$engaged.now,], id=id, family = binomial, corstr = "exchangeable")

# to map prob unsuppressed if engaged to prop lost if unsuppressed:
# P(lost | unsupp) = p(unsupp | lost) * p(lost) / [p(unsupp | lost) * p(lost) + p(unsupp | engaged) * p(engaged)]
# given P(unsupp|lost) = 1
# P(lost | unsupp) = P(lost) / [P(lost) + (1-P(supp | engaged)) * (1-P(lost)) ]

prior.p.lost = 0.25 #nationally from HIV data

p.suppressed.if.engaged.for.disengaged = expit(predict.glm(model.suppressed.if.engaged, newdata=disengaged.for.weights))
disengaged.for.weights$truly.disengaged = prior.p.lost /
    (prior.p.lost + (1-prior.p.lost) * (1-p.suppressed.if.engaged.for.disengaged))
disengaged.for.weights$truly.disengaged[disengaged.for.weights$future.state=='reengage.suppress'] = 0

model.truly.disengaged <- geeglm(truly.disengaged ~ age.category + sex.risk + race + 
                                     relative.year + relative.year:age.category + relative.year:sex.risk + relative.year:race,
                                 data=disengaged.for.weights, id=id, family = binomial, corstr = "exchangeable")

if (1==2)
{
    qplot(jitter(disengaged.for.weights$relative.year), jitter(disengaged.for.weights$truly.disengaged)) + geom_smooth()
    sapply(sort(unique(disengaged.for.weights$relative.year)), function(year){
        mean(disengaged.for.weights$truly.disengaged[disengaged.for.weights$relative.year==year])
    })
}

##-- A FUNCTION TO SHOEHORN A STANDARD MULTINOMIAL FIT INSTEAD OF OUR GEE LOR --##
shoehorn.fit.multinom <- function(ff, df)
{
    new.levels = c('remain', setdiff(levels(df$future.state), 'remain'))
    df$future.state = factor(df$future.state, new.levels)
    
    fit = multinom(ff, data=df)
    
    coefs = coef(fit)
    
    shoehorned.coefs = c(
        beta10=coefs[1,1],
        beta20=coefs[2,1]
    )

    for (cname in dimnames(coefs)[[2]][-1])
    {
        shoehorned.coefs[paste0(cname, ":1")] = coefs[1,cname]
        shoehorned.coefs[paste0(cname, ":2")] = coefs[2,cname]
    }
    
    
    list(
        coefficients = shoehorned.coefs,
        robust.variance = NULL
    )
}

##------------------------------------##
##-- DATASET 1: Engaged unsuppressed--##
##------------------------------------##

print("Preparing Dataset for engaged-unsuppressed")
engaged.unsuppressed <-dataset[dataset$engaged.now==TRUE & dataset$suppressed.now==FALSE & !is.na(dataset$suppressed.now),]

for (i in 1:nrow(engaged.unsuppressed)) {
    if(!is.na(engaged.unsuppressed$engaged.future[i]) && !is.na(engaged.unsuppressed$suppressed.future[i]) &&
        engaged.unsuppressed$engaged.future[i]==TRUE && engaged.unsuppressed$suppressed.future[i]==FALSE)  {
        engaged.unsuppressed$future.state[i]="remain"
        
    } else if(!is.na(engaged.unsuppressed$engaged.future[i]) && !is.na(engaged.unsuppressed$suppressed.future[i]) &&
              engaged.unsuppressed$engaged.future[i]==TRUE && engaged.unsuppressed$suppressed.future[i]==TRUE)  {
        engaged.unsuppressed$future.state[i]="suppress"
        
    } else if(!is.na(engaged.unsuppressed$engaged.future[i]) &&
              engaged.unsuppressed$engaged.future[i]==FALSE)  {
        engaged.unsuppressed$future.state[i]="lost"
        
    } else 
        engaged.unsuppressed$future.state[i]="missing"
    
}

engaged.unsuppressed <-engaged.unsuppressed[engaged.unsuppressed$future.state!="missing",]
engaged.unsuppressed$future.state <- factor(engaged.unsuppressed$future.state, levels = c("suppress","lost", "remain"))
engaged.unsuppressed$age.category <- factor(engaged.unsuppressed$age.category, levels = c("35-45","13-25","25-35","45-55","55+"))
engaged.unsuppressed$sex <- factor(engaged.unsuppressed$sex, levels = c("Male","Female"))
engaged.unsuppressed$race <- factor(engaged.unsuppressed$race, levels = c("other","black","hispanic"))
engaged.unsuppressed$risk <- factor(engaged.unsuppressed$risk, levels = c("heterosexual","idu","msm","msm_idu","other"))
engaged.unsuppressed$sex.risk <- factor(engaged.unsuppressed$sex.risk, levels = c("msm","msm_idu",
                                                                                  "heterosexual_male","heterosexual_female",
                                                                                  "idu_male","idu_female","other"))

model.suppressed.if.not.truly.disengaged.EU <- geeglm(suppressed.future ~ age.category + sex.risk + race +
                                                       relative.year + relative.year:age.category + relative.year:sex.risk + relative.year:race,
                                                   data=engaged.unsuppressed[engaged.unsuppressed$future.state!='lost',], id=id, family = binomial, corstr = "exchangeable")


p.truly.disengaged.EU = expit(predict.glm(model.truly.disengaged, newdata=engaged.unsuppressed))
p.truly.disengaged.EU[engaged.unsuppressed$future.state=='remain'] = 0
p.truly.disengaged.EU[engaged.unsuppressed$future.state=='suppress'] = 0

p.suppressed.if.not.truly.disengaged.EU = expit(predict.glm(model.suppressed.if.not.truly.disengaged.EU, newdata=engaged.unsuppressed))
p.suppressed.if.not.truly.disengaged.EU[engaged.unsuppressed$future.state=='remain'] = 0
p.suppressed.if.not.truly.disengaged.EU[engaged.unsuppressed$future.state=='suppress'] = 1

N.IMPUTATIONS = 4
imputed.engaged.unsuppressed = NULL

for (i in 1:N.IMPUTATIONS)
{
    simulated.lost.EU=rbinom(n=length(p.truly.disengaged.EU), size=1, prob=p.truly.disengaged.EU)
    simulated.suppressed.EU=rbinom(n=length(p.suppressed.if.not.truly.disengaged.EU), size=1, prob=p.suppressed.if.not.truly.disengaged.EU)
    
    engaged.unsuppressed.sim = engaged.unsuppressed
    engaged.unsuppressed.sim$future.state[simulated.lost.EU==1] = 'lost'
    engaged.unsuppressed.sim$future.state[simulated.lost.EU==0 & simulated.suppressed.EU==1] = 'suppress'
    engaged.unsuppressed.sim$future.state[simulated.lost.EU==0 & simulated.suppressed.EU==0] = 'remain'

    imputed.engaged.unsuppressed = rbind(imputed.engaged.unsuppressed, engaged.unsuppressed.sim)   
}

# Check
if (1==2)
{
    sapply(sort(unique(imputed.engaged.unsuppressed$relative.year)), function(year){
        mask = imputed.engaged.unsuppressed$relative.year==year
        mean(imputed.engaged.unsuppressed$future.state[mask]=='suppress')
    })
    sapply(sort(unique(imputed.engaged.unsuppressed$relative.year)), function(year){
        mask = imputed.engaged.unsuppressed$relative.year==year
        mean(imputed.engaged.unsuppressed$future.state[mask]=='remain')
    })
    sapply(sort(unique(engaged.unsuppressed$relative.year)), function(year){
        mask = engaged.unsuppressed$relative.year==year
        mean(engaged.unsuppressed$future.state[mask]=='lost')
    })
    
    
    sapply(sort(unique(engaged.unsuppressed$relative.year)), function(year){
        mask = engaged.unsuppressed$relative.year==year
        mean(engaged.unsuppressed$future.state[mask]=='suppress')
    })
    
    
    
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
}

{
    
}
if (analysis=='jheem.model')
{
    if (just.do.multinomial)
    {
        print("Fitting simple multinomial Model for engaged-unsuppressed, JHEEM model version (model coefficients only), with disengagement weights")
        model.engaged.unsuppressed <- shoehorn.fit.multinom(future.state ~ age.category + sex.risk + race + relative.year
                                                + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                                df=imputed.engaged.unsuppressed)
    }
    else
    {
        print("Fitting Model for engaged-unsuppressed, JHEEM model version (model coefficients only), with disengagement weights")
        model.engaged.unsuppressed <- nomLORgee(future.state ~ age.category + sex.risk + race + relative.year
                                                + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                                data=imputed.engaged.unsuppressed, id=id)
    }
   
}
if (analysis=='CNICS')
{
    print("Fitting Model for engaged-unsuppressed, CNICS version (all coefficients)")
    model.engaged.unsuppressed <- nomLORgee(future.state ~ age.category + sex.risk + race + relative.year
                                            + site + art.naive + years.in.care + aids.defining.illness
                                            + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                            data=engaged.unsuppressed, id=id)
}


##------------------------------------##
##--- DATASET 2: Engaged suppressed---##
##------------------------------------##

print("Preparing Dataset for engaged-suppressed")
engaged.suppressed <-dataset[dataset$engaged.now==TRUE & dataset$suppressed.now==TRUE & !is.na(dataset$suppressed.now),]

for (i in 1:nrow(engaged.suppressed)) {
    if(!is.na(engaged.suppressed$engaged.future[i]) && !is.na(engaged.suppressed$suppressed.future[i]) &&
       engaged.suppressed$engaged.future[i]==TRUE && engaged.suppressed$suppressed.future[i]==TRUE)  {
        engaged.suppressed$future.state[i]="remain"
        
    } else if(!is.na(engaged.suppressed$engaged.future[i]) && !is.na(engaged.suppressed$suppressed.future[i]) &&
              engaged.suppressed$engaged.future[i]==TRUE && engaged.suppressed$suppressed.future[i]==FALSE)  {
        engaged.suppressed$future.state[i]="unsuppress"
        
    } else if(!is.na(engaged.suppressed$engaged.future[i]) &&
              engaged.suppressed$engaged.future[i]==FALSE)  {
        engaged.suppressed$future.state[i]="lost"
        
    } else 
        engaged.suppressed$future.state[i]="missing"
    
}

engaged.suppressed <-engaged.suppressed[engaged.suppressed$future.state!="missing",]
engaged.suppressed$future.state <- factor(engaged.suppressed$future.state, levels = c("unsuppress","lost","remain"))
engaged.suppressed$age.category <- factor(engaged.suppressed$age.category, levels = c("35-45","13-25","25-35","45-55","55+"))
engaged.suppressed$sex <- factor(engaged.suppressed$sex, levels = c("Male","Female"))
engaged.suppressed$race <- factor(engaged.suppressed$race, levels = c("other","black","hispanic"))
engaged.suppressed$risk <- factor(engaged.suppressed$risk, levels = c("heterosexual","idu","msm","msm_idu","other"))
engaged.suppressed$sex.risk <- factor(engaged.suppressed$sex.risk, levels = c("msm","msm_idu",
                                                                              "heterosexual_male","heterosexual_female",
                                                                              "idu_male","idu_female","other"))


model.suppressed.if.not.truly.disengaged.ES <- geeglm(suppressed.future ~ age.category + sex.risk + race +
                                                       relative.year + relative.year:age.category + relative.year:sex.risk + relative.year:race,
                                                   data=engaged.suppressed[engaged.suppressed$future.state!='lost',], id=id, family = binomial, corstr = "exchangeable")


p.truly.disengaged.ES = expit(predict.glm(model.truly.disengaged, newdata=engaged.suppressed))
p.truly.disengaged.ES[engaged.suppressed$future.state=='remain'] = 0
p.truly.disengaged.ES[engaged.suppressed$future.state=='unsuppress'] = 0

p.suppressed.if.not.truly.disengaged.ES = expit(predict.glm(model.suppressed.if.not.truly.disengaged.ES, newdata=engaged.suppressed))
p.suppressed.if.not.truly.disengaged.ES[engaged.suppressed$future.state=='remain'] = 1
p.suppressed.if.not.truly.disengaged.ES[engaged.suppressed$future.state=='unsuppress'] = 0

N.IMPUTATIONS = 4
imputed.engaged.suppressed = NULL

for (i in 1:N.IMPUTATIONS)
{
    simulated.lost.ES=rbinom(n=length(p.truly.disengaged.ES), size=1, prob=p.truly.disengaged.ES)
    simulated.suppressed.ES=rbinom(n=length(p.suppressed.if.not.truly.disengaged.ES), size=1, prob=p.suppressed.if.not.truly.disengaged.ES)
    
    engaged.suppressed.sim = engaged.suppressed
    engaged.suppressed.sim$future.state[simulated.lost.ES==1] = 'lost'
    engaged.suppressed.sim$future.state[simulated.lost.ES==0 & simulated.suppressed.ES==1] = 'remain'
    engaged.suppressed.sim$future.state[simulated.lost.ES==0 & simulated.suppressed.ES==0] = 'unsuppress'
    
    imputed.engaged.suppressed = rbind(imputed.engaged.suppressed, engaged.suppressed.sim)   
}



if (analysis=='jheem.model')
{
    if (just.do.multinomial)
    {
        
        print("Fitting Simple Multinomial Model for engaged-suppressed, JHEEM model version (model coefficients only), with disengagement weights")
        model.engaged.suppressed <- shoehorn.fit.multinom(future.state ~ age.category + sex.risk + race + relative.year
                                              + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                              df=imputed.engaged.suppressed)
    }
    else
    {
        print("Fitting Model for engaged-suppressed, JHEEM model version (model coefficients only), with disengagement weights")
        model.engaged.suppressed <- nomLORgee(future.state ~ age.category + sex.risk + race + relative.year
                                                + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                                data=imputed.engaged.suppressed, id=id)
    }
}

if (analysis=='CNICS')
{
    print("Fitting Model for engaged-suppressed, CNICS version (all coefficients)")
    model.engaged.suppressed <- nomLORgee(future.state ~ age.category + sex.risk + race + relative.year
                                          + site + art.naive + years.in.care + aids.defining.illness
                                          + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                          data=engaged.suppressed, id=id)
}


##------------------------------------##
##------- DATASET 3: Disengaged ------##
##------------------------------------##


disengaged$p.truly.disengaged = expit(predict.glm(model.truly.disengaged, newdata=disengaged))
disengaged$p.truly.disengaged[disengaged$future.state=='reengage.unsuppress'] = 1
disengaged$p.truly.disengaged[disengaged$future.state=='reengage.suppress'] = 0


if (analysis=='jheem.model')
{
    print("Removing reengagement into suppressed")
    disengaged <- disengaged[disengaged$future.state!="reengage.suppress",]
    disengaged$reengage = as.numeric(disengaged$future.state=='reengage.unsuppress')
    disengaged$future.state <- factor(disengaged$future.state, levels = c("reengage.unsuppress","remain"))
    
    print("Fitting LOGISTIC Model for disengaged, JHEEM model version (model coefficients only), with disengagement weights")
    model.disengaged <- geeglm(reengage ~ age.category + sex.risk + race + relative.year 
                               + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                               data=disengaged, id=id, family = binomial, corstr = "exchangeable",
                               weights = disengaged$p.truly.disengaged)
    model.disengaged.noslope <- geeglm(reengage ~ age.category + sex.risk + race,
                               data=disengaged, id=id, family = binomial, corstr = "exchangeable",
                               weights = disengaged$p.truly.disengaged)
    
    model.disengaged$coefficients[] = 0
    model.disengaged$coefficients[names(model.disengaged.noslope$coefficients)] = model.disengaged.noslope$coefficients
    
    #model.disengaged.simple <- glm(reengage ~ age.category + sex.risk + race + relative.year 
     #                          + age.category*relative.year + sex.risk*relative.year + race*relative.year,
      #                         data=disengaged, family = binomial, 
       #                        weights = disengaged$p.truly.disengaged)
    
}

if (analysis=='CNICS')
{
    print("Fitting MULTINOMIAL Model for disengaged, CNICS version (all coefficients)")
    model.disengaged <- nomLORgee(future.state ~ age.category + sex.risk + race + relative.year 
                                  + site + art.naive + years.in.care + aids.defining.illness,
                                  + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                  data=disengaged, id=id)  
}

##----------------------------##
##-- ADJUST P DISENGAGEMENT --##
##----------------------------##

adjust.coefficients.for.disengagement <- function(coefficients,
                                                  prior.estimate=1-mean(c(.89,.74)), #from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4334738/ 
                                                  prior.weight=0.5,
                                                  data.weight = 1-prior.weight,
                                                  disengaged.k = 2)
{
    x.betas = coefficients[c('beta10','beta20')]
    p.ref = 1/(1+sum(exp(x.betas)))
    p.data = exp(x.betas[disengaged.k]) * p.ref
    
    p.post = prior.weight * prior.estimate + data.weight * p.data
    
    rr.post = p.post / p.ref
    coefficients[paste0('beta',disengaged.k,'0')] = log(rr.post)
    
#    x.betas.new = coefficients[c('beta10','beta20')]
#    p.ref.new = 1/(1+sum(exp(x.betas.new)))
#    p.data.new = exp(x.betas[disengaged.k]) * p.ref.new
    
#    x.betas.for.slope = coefficients[c('beta10','beta20')] + coefficients[c('relative.year:1','relative.year:2')]
#    p.ref.for.slope = 1/(1+sum(exp(x.betas.for.slope)))
#    p.data.for.slope = exp(x.betas.for.slope[disengaged.k]) * p.ref.for.slope
    
#    p.post.for.slope = prior.weight * prior.estimate + data.weight * p.data.for.slope
#    rrr.post = p.post.for.slope / p.post / (p.ref.for.slope / p.ref.new)
    
#    coefficients[paste0('relative.year:',disengaged.k)] = log(rrr.post)
    
    coefficients
    
}

adjust.slope.coefficients <- function(coefficients,
                          prior.estimate=0,
                          prior.weight=0.5,
                          data.weight=1-prior.weight)
{
    mask = grepl('.+relative\\.year', names(coefficients))
    coefficients[mask] = prior.weight * prior.estimate + data.weight * coefficients[mask]
    coefficients
}
    

##------------------------------------##
##------------- Output ---------------##
##------------------------------------##

output <- list(engaged.unsuppressed.coefficients=model.engaged.unsuppressed$coefficients,
               engaged.unsuppressed.variance=model.engaged.unsuppressed$robust.variance,
               engaged.suppressed.coefficients=model.engaged.suppressed$coefficients,
               engaged.suppressed.variance=model.engaged.suppressed$robust.variance,
               disengaged.coefficients=model.disengaged$coefficients,
               disengaged.variance=model.disengaged$robust.variance,
               anchor.year=anchor.year)

output$engaged.suppressed.coefficients = adjust.coefficients.for.disengagement(output$engaged.suppressed.coefficients,
                                                                               prior.weight=0.3)
output$engaged.unsuppressed.coefficients = adjust.coefficients.for.disengagement(output$engaged.unsuppressed.coefficients,
                                                                                 prior.weight=0.3)

output$engaged.suppressed.coefficients = adjust.slope.coefficients(output$engaged.suppressed.coefficients,
                                                                   prior.weight=0.5)
output$engaged.unsuppressed.coefficients = adjust.slope.coefficients(output$engaged.unsuppressed.coefficients,
                                                                   prior.weight=0.5)


print("Done - saving output")
save(output, file=file.path('code','for Melissa', 'CNICS analysis', 
                            paste0('multinomial_output_adj_', dataset.type, '_', analysis, '_', Sys.Date())))

if (dataset.type=='real')
    save(output, file='cleaned_data/continuum/cnics_regression.Rdata')