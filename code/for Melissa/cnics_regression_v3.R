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
use.gee = F
# analysis = 'CNICS'

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

##------------------------------------##
##-- DATASET 0: Engaged unsuppressed -##
##------------------------------------##
# (Before splitting into naive or failing)

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


##-----------------------------------##
##-- DATASET 1: Unsuppressed naive --##
##-----------------------------------##
print("Preparing Dataset for unsuppressed-naive")
unsuppressed.naive <- engaged.unsuppressed[engaged.unsuppressed$art.naive.now==TRUE,]

model.suppressed.if.not.truly.disengaged.UN <- geeglm(suppressed.future ~ age.category + sex.risk + race +
                                                          relative.year + relative.year:age.category + relative.year:sex.risk + relative.year:race,
                                                      data=unsuppressed.naive[unsuppressed.naive$future.state!='lost',], id=id, family = binomial, corstr = "exchangeable")


p.truly.disengaged.UN = expit(predict.glm(model.truly.disengaged, newdata=unsuppressed.naive))
p.truly.disengaged.UN[unsuppressed.naive$future.state=='remain'] = 0
p.truly.disengaged.UN[unsuppressed.naive$future.state=='suppress'] = 0

p.suppressed.if.not.truly.disengaged.UN = expit(predict.glm(model.suppressed.if.not.truly.disengaged.UN, newdata=unsuppressed.naive))
p.suppressed.if.not.truly.disengaged.UN[unsuppressed.naive$future.state=='remain'] = 0
p.suppressed.if.not.truly.disengaged.UN[unsuppressed.naive$future.state=='suppress'] = 1

N.IMPUTATIONS = 4
imputed.unsuppressed.naive = NULL

for (i in 1:N.IMPUTATIONS)
{
    simulated.lost.UN=rbinom(n=length(p.truly.disengaged.UN), size=1, prob=p.truly.disengaged.UN)
    simulated.suppressed.UN=rbinom(n=length(p.suppressed.if.not.truly.disengaged.UN), size=1, prob=p.suppressed.if.not.truly.disengaged.UN)
    
    unsuppressed.naive.sim = unsuppressed.naive
    unsuppressed.naive.sim$future.state[simulated.lost.UN==1] = 'lost'
    unsuppressed.naive.sim$future.state[simulated.lost.UN==0 & simulated.suppressed.UN==1] = 'suppress'
    unsuppressed.naive.sim$future.state[simulated.lost.UN==0 & simulated.suppressed.UN==0] = 'remain'
    
    imputed.unsuppressed.naive = rbind(imputed.unsuppressed.naive, unsuppressed.naive.sim)   
}

# Check
if (1==2)
{
    sapply(sort(unique(imputed.unsuppressed.naive$relative.year)), function(year){
        mask = imputed.unsuppressed.naive$relative.year==year
        mean(imputed.unsuppressed.naive$future.state[mask]=='suppress')
    })
    sapply(sort(unique(imputed.unsuppressed.naive$relative.year)), function(year){
        mask = imputed.unsuppressed.naive$relative.year==year
        mean(imputed.unsuppressed.naive$future.state[mask]=='remain')
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


imputed.unsuppressed.naive$suppressed.future = as.numeric(imputed.unsuppressed.naive$future.state=='suppress')
imputed.unsuppressed.naive$lost.future = as.numeric(imputed.unsuppressed.naive$future.state=='lost')


#### Logistic models for Unsuppressed-Naive  ####

## Disengage
if (use.gee==T)
{
    ## --> Lost
    print("Fitting logistic model for unsuppressed-naive --> lost, WITH individual slopes")
    model.naive.to.lost.slopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year
                                           + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                           data=imputed.unsuppressed.naive, id=id, family=binomial, corstr="exchangeable")
    
    print("Fitting logistic model for unsuppressed-naive --> lost, WITHOUT individual slopes")
    model.naive.to.lost.noslopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
                                             data=imputed.unsuppressed.naive, id=id, family=binomial, corstr="exchangeable")
} else
{
    ## --> Lost
    print("Fitting logistic model for unsuppressed-naive --> lost, WITH individual slopes, no GEE")
    model.naive.to.lost.slopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year
                                        + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                        data=imputed.unsuppressed.naive, family=binomial)
    
    print("Fitting logistic model for unsuppressed-naive --> lost, WITHOUT individual slopes, no GEE")
    model.naive.to.lost.noslopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year,
                                          data=imputed.unsuppressed.naive, family=binomial)
}  


## Remove still naive in the future
imputed.unsuppressed.naive.start.art <- imputed.unsuppressed.naive[imputed.unsuppressed.naive$art.naive.future==FALSE,]

## Suppress 
if (use.gee==T)
{
    ## --> Suppress
    print("Fitting logistic model for unsuppressed-naive --> engaged-suppressed, WITH individual slopes")
    model.naive.to.supp.slopes <- geeglm(suppressed.future ~ age.category + sex.risk + race + relative.year
                                           + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                           data=imputed.unsuppressed.naive.start.art, id=id, family=binomial, corstr="exchangeable")
    
    print("Fitting logistic model for unsuppressed-naive --> engaged-suppressed, WITHOUT individual slopes")
    model.naive.to.supp.noslopes <- geeglm(suppressed.future ~ age.category + sex.risk + race + relative.year,
                                             data=imputed.unsuppressed.naive.start.art, id=id, family=binomial, corstr="exchangeable")
    
} else
{
    ## --> Suppress
    print("Fitting logistic model for unsuppressed-naive --> engaged-suppressed, WITH individual slopes, no GEE")
    model.naive.to.supp.slopes <- glm(suppressed.future ~ age.category + sex.risk + race + relative.year
                                        + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                        data=imputed.unsuppressed.naive.start.art, family=binomial)
    
    print("Fitting logistic model for unsuppressed-naive --> engaged-suppressed, WITHOUT individual slopes, no GEE")
    model.naive.to.supp.noslopes <- glm(suppressed.future ~ age.category + sex.risk + race + relative.year,
                                          data=imputed.unsuppressed.naive.start.art, family=binomial)
    

}  


##-------------------------------------##
##-- DATASET 2: Unsuppressed failing --##
##-------------------------------------##
print("Preparing Dataset for unsuppressed-failing")
unsuppressed.failing <- engaged.unsuppressed[engaged.unsuppressed$art.naive.now==FALSE,]

model.suppressed.if.not.truly.disengaged.UF <- geeglm(suppressed.future ~ age.category + sex.risk + race +
                                                          relative.year + relative.year:age.category + relative.year:sex.risk + relative.year:race,
                                                      data=unsuppressed.failing[unsuppressed.failing$future.state!='lost',], id=id, family = binomial, corstr = "exchangeable")


p.truly.disengaged.UF = expit(predict.glm(model.truly.disengaged, newdata=unsuppressed.failing))
p.truly.disengaged.UF[unsuppressed.failing$future.state=='remain'] = 0
p.truly.disengaged.UF[unsuppressed.failing$future.state=='suppress'] = 0

p.suppressed.if.not.truly.disengaged.UF = expit(predict.glm(model.suppressed.if.not.truly.disengaged.UF, newdata=unsuppressed.failing))
p.suppressed.if.not.truly.disengaged.UF[unsuppressed.failing$future.state=='remain'] = 0
p.suppressed.if.not.truly.disengaged.UF[unsuppressed.failing$future.state=='suppress'] = 1

N.IMPUTATIONS = 4
imputed.unsuppressed.failing = NULL

for (i in 1:N.IMPUTATIONS)
{
    simulated.lost.UF=rbinom(n=length(p.truly.disengaged.UF), size=1, prob=p.truly.disengaged.UF)
    simulated.suppressed.UF=rbinom(n=length(p.suppressed.if.not.truly.disengaged.UF), size=1, prob=p.suppressed.if.not.truly.disengaged.UF)
    
    unsuppressed.failing.sim = unsuppressed.failing
    unsuppressed.failing.sim$future.state[simulated.lost.UF==1] = 'lost'
    unsuppressed.failing.sim$future.state[simulated.lost.UF==0 & simulated.suppressed.UF==1] = 'suppress'
    unsuppressed.failing.sim$future.state[simulated.lost.UF==0 & simulated.suppressed.UF==0] = 'remain'
    
    imputed.unsuppressed.failing = rbind(imputed.unsuppressed.failing, unsuppressed.failing.sim)   
}



#### Logistic models for Engaged-Unsuppressed ####
if (use.gee==T)
{
    ## --> Suppress
    print("Fitting logistic model for unsuppressed-failing --> engaged-suppressed, WITH individual slopes")
    model.failing.to.supp.slopes <- geeglm(suppressed.future ~ age.category + sex.risk + race + relative.year
                                          + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                          data=imputed.unsuppressed.failing, id=id, family=binomial, corstr="exchangeable")
    
    print("Fitting logistic model for unsuppressed-failing --> engaged-suppressed, WITHOUT individual slopes")
    model.failing.to.supp.noslopes <- geeglm(suppressed.future ~ age.category + sex.risk + race + relative.year,
                                            data=imputed.unsuppressed.failing, id=id, family=binomial, corstr="exchangeable")
    
    ## --> Lost
    print("Fitting logistic model for unsuppressed-failing --> lost, WITH individual slopes")
    model.failing.to.lost.slopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year
                                          + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                          data=imputed.unsuppressed.failing, id=id, family=binomial, corstr="exchangeable")
    
    print("Fitting logistic model for unsuppressed-failing --> lost, WITHOUT individual slopes")
    model.failing.to.lost.noslopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
                                            data=imputed.unsuppressed.failing, id=id, family=binomial, corstr="exchangeable")
} else
{
    ## --> Suppress
    print("Fitting logistic model for unsuppressed-failing --> engaged-suppressed, WITH individual slopes, no GEE")
    model.failing.to.supp.slopes <- glm(suppressed.future ~ age.category + sex.risk + race + relative.year
                                       + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                       data=imputed.unsuppressed.failing, family=binomial)
    
    print("Fitting logistic model for unsuppressed-failing --> engaged-suppressed, WITHOUT individual slopes, no GEE")
    model.failing.to.supp.noslopes <- glm(suppressed.future ~ age.category + sex.risk + race + relative.year,
                                         data=imputed.unsuppressed.failing, family=binomial)
    
    ## --> Lost
    print("Fitting logistic model for unsuppressed-failing --> lost, WITH individual slopes, no GEE")
    model.failing.to.lost.slopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year
                                       + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                       data=imputed.unsuppressed.failing, family=binomial)
    
    print("Fitting logistic model for unsuppressed-failing --> lost, WITHOUT individual slopes, no GEE")
    model.failing.to.lost.noslopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year,
                                         data=imputed.unsuppressed.failing, family=binomial)
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


imputed.engaged.suppressed$suppressed.future = as.numeric(imputed.engaged.suppressed$future.state=='unsuppress')
imputed.engaged.suppressed$lost.future = as.numeric(imputed.engaged.suppressed$future.state=='lost')

#### Logistic models for Engaged-Suppressed ####
if (use.gee==T)
{
    ## --> Failing
    print("Fitting logistic model for engaged-suppressed --> unsuppressed-failing, WITH individual slopes")
    model.supp.to.failing.slopes <- geeglm(suppressed.future ~ age.category + sex.risk + race + relative.year
                                          + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                          data=imputed.engaged.suppressed, id=id, family=binomial, corstr="exchangeable")
    
    print("Fitting logistic model for engaged-suppressed --> engaged-suppressed, WITHOUT individual slopes")
    model.supp.to.failing.noslopes <- geeglm(suppressed.future ~ age.category + sex.risk + race + relative.year,
                                            data=imputed.engaged.suppressed, id=id, family=binomial, corstr="exchangeable")
    
    ## --> Lost
    print("Fitting logistic model for engaged-suppressed --> lost, WITH individual slopes")
    model.supp.to.lost.slopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year
                                        + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                        data=imputed.engaged.suppressed, id=id, family=binomial, corstr="exchangeable")
    
    print("Fitting logistic model for engaged-suppressed --> lost, WITHOUT individual slopes")
    model.supp.to.lost.noslopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
                                          data=imputed.engaged.suppressed, id=id, family=binomial, corstr="exchangeable")
} else
{
    ## --> Failing
    print("Fitting logistic model for engaged-suppressed --> unsuppressed-failing, WITH individual slopes, no GEE")
    model.supp.to.failing.slopes <- glm(suppressed.future ~ age.category + sex.risk + race + relative.year
                                       + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                       data=imputed.engaged.suppressed, family=binomial)
    
    print("Fitting logistic model for engaged-suppressed --> engaged-suppressed, WITHOUT individual slopes, no GEE")
    model.supp.to.failing.noslopes <- glm(suppressed.future ~ age.category + sex.risk + race + relative.year,
                                         data=imputed.engaged.suppressed, family=binomial)
    
    ## --> Lost
    print("Fitting logistic model for engaged-suppressed --> lost, WITH individual slopes, no GEE")
    model.supp.to.lost.slopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year
                                     + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                     data=imputed.engaged.suppressed, family=binomial)
    
    print("Fitting logistic model for engaged-suppressed --> lost, WITHOUT individual slopes, no GEE")
    model.supp.to.lost.noslopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year,
                                       data=imputed.engaged.suppressed, family=binomial)
    
}



##------------------------------------##
##------- DATASET 3: Disengaged ------##
##------------------------------------##

disengaged$p.truly.disengaged = expit(predict.glm(model.truly.disengaged, newdata=disengaged))
disengaged$p.truly.disengaged[disengaged$future.state=='reengage.unsuppress'] = 1
disengaged$p.truly.disengaged[disengaged$future.state=='reengage.suppress'] = 0


print("Removing reengagement into suppressed")
disengaged <- disengaged[disengaged$future.state!="reengage.suppress",]
disengaged$reengage = as.numeric(disengaged$future.state=='reengage.unsuppress')
disengaged$future.state <- factor(disengaged$future.state, levels = c("reengage.unsuppress","remain"))


#### Logistic models for Disengaged ####
if (use.gee==T)
{
    print("Fitting LOGISTIC Model for disengaged, JHEEM model version (model coefficients only), with disengagement weights")
    model.disengaged.slopes <- geeglm(reengage ~ age.category + sex.risk + race + relative.year
                                      + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                      data=disengaged, id=id, family = binomial, corstr = "exchangeable",
                                      weights = disengaged$p.truly.disengaged)
    model.disengaged.noslopes <- geeglm(reengage ~ age.category + sex.risk + race + relative.year,
                                        data=disengaged, id=id, family = binomial, corstr = "exchangeable",
                                        weights = disengaged$p.truly.disengaged)
    
    #model.disengaged$coefficients[] = 0
    #model.disengaged$coefficients[names(model.disengaged.noslope$coefficients)] = model.disengaged.noslope$coefficients
} else
{
    print("Fitting LOGISTIC Model for disengaged, JHEEM model version (model coefficients only), with disengagement weights, no GEE")
    model.disengaged.slopes <- glm(reengage ~ age.category + sex.risk + race + relative.year
                                   + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                   data=disengaged, family = binomial,
                                   weights = disengaged$p.truly.disengaged)
    model.disengaged.noslopes <- glm(reengage ~ age.category + sex.risk + race + relative.year,
                                     data=disengaged, family = binomial,
                                     weights = disengaged$p.truly.disengaged)
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

output <- list(
    #Naive, coefficients
    naive.to.suppressed.slopes.coefficients=model.naive.to.supp.slopes$coefficients,
    naive.to.suppressed.noslopes.coefficients=model.naive.to.supp.noslopes$coefficients,
    naive.to.lost.slopes.coefficients=model.naive.to.lost.slopes$coefficients,
    naive.to.lost.noslopes.coefficients=model.naive.to.lost.noslopes$coefficients,
    
    #Naive, variance
    naive.to.suppressed.slopes.variance=model.naive.to.supp.slopes$robust.variance,
    naive.to.suppressed.noslopes.variance=model.naive.to.supp.noslopes$robust.variance,
    naive.to.lost.slopes.variance=model.naive.to.lost.slopes$robust.variance,
    naive.to.lost.noslopes.variance=model.naive.to.lost.noslopes$robust.variance,
    
    #Failing, coefficients
    failing.to.suppressed.slopes.coefficients=model.failing.to.supp.slopes$coefficients,
    failing.to.suppressed.noslopes.coefficients=model.failing.to.supp.noslopes$coefficients,
    failing.to.lost.slopes.coefficients=model.failing.to.lost.slopes$coefficients,
    failing.to.lost.noslopes.coefficients=model.failing.to.lost.noslopes$coefficients,
    
    #Failing, variance
    failing.to.suppressed.slopes.variance=model.failing.to.supp.slopes$robust.variance,
    failing.to.suppressed.noslopes.variance=model.failing.to.supp.noslopes$robust.variance,
    failing.to.lost.slopes.variance=model.failing.to.lost.slopes$robust.variance,
    failing.to.lost.noslopes.variance=model.failing.to.lost.noslopes$robust.variance,
    
    #Suppressed, coefficients
    suppressed.to.failing.slopes.coefficients=model.supp.to.failing.slopes$coefficients,
    suppressed.to.failing.noslopes.coefficients=model.supp.to.failing.noslopes$coefficients,
    suppressed.to.lost.slopes.coefficients=model.supp.to.lost.slopes$coefficients,
    suppressed.to.lost.noslopes.coefficients=model.supp.to.lost.noslopes$coefficients,
    
    #Suppressed, variance
    suppressed.to.failing.slopes.variance=model.supp.to.failing.slopes$robust.variance,
    suppressed.to.failing.noslopes.variance=model.supp.to.failing.noslopes$robust.variance,
    suppressed.to.lost.slopes.variance=model.supp.to.lost.slopes$robust.variance,
    suppressed.to.lost.noslopes.variance=model.supp.to.lost.noslopes$robust.coefficients,
    
    # Disengaged
    disengaged.slopes.coefficients=model.disengaged.slopes$coefficients,
    disengaged.slopes.variance=model.disengaged.slopes$robust.variance,
    disengaged.noslopes.coefficients=model.disengaged.noslopes$coefficients,
    disengaged.noslopes.variance=model.disengaged.noslopes$robust.variance,
    
    anchor.year=anchor.year)


print("Done - saving output")
save(output, file=file.path('code','for Melissa', 'CNICS analysis', 
                            paste0('logistic_output_adj_', dataset.type, '_', analysis, '_', Sys.Date())))

if (dataset.type=='real')
    save(output, file='cleaned_data/continuum/cnics_regression.Rdata')