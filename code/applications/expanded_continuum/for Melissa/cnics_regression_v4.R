SAVE = F
MELISSAS.FILE = "~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/CNICS/synthetic_fixed_from2007_2021-12-07.Rdata"
TODDS.FILE = 'Q:/CNICS/cleaned/2021/cnics_fixed_from2007_2021-12-07.Rdata'

#to get from backup
TODDS.FILE = 'R:/WD Backup.swstor/tfojo1/NGE2NzRlYTZjOTIzNGJmYz/Volume{baccb07e-97f3-498c-89dd-d6f3a161e400}/CNICS/cleaned/2021/cnics_fixed_from2007_2022-06-18.Rdata'
TODDS.FILE = 'R:/WD Backup.swstor/tfojo1/NGE2NzRlYTZjOTIzNGJmYz/Volume{baccb07e-97f3-498c-89dd-d6f3a161e400}/CNICS/cleaned/2021/synthetic_fixed_from2007_2021-12-07.Rdata'
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
separate.durable.recent = T
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
dataset$sex <- factor(dataset$sex, levels = c("male","female"))

anchor.year = 2010
dataset$relative.year <- dataset$date - anchor.year

## Creating sex.risk categories ##
dataset$sex.risk[dataset$risk=="msm"] = "msm"
dataset$sex.risk[dataset$risk=="msm_idu"] = "msm_idu"
dataset$sex.risk[dataset$sex=="male" & dataset$risk=="heterosexual"] = "heterosexual_male"
dataset$sex.risk[dataset$sex=="female" & dataset$risk=="heterosexual"] = "heterosexual_female"
dataset$sex.risk[dataset$sex=="male" & dataset$risk=="idu"] = "idu_male"
dataset$sex.risk[dataset$sex=="female" & dataset$risk=="idu"] = "idu_female"
dataset$sex.risk[is.na(dataset$sex.risk)] = "missing"


## Defining engagement criteria; disengagement time ##
print("Defining engagement criteria and disengagement time")
dataset$engaged.now <- dataset$vl.now & dataset$visits.now
dataset$engaged.future <- dataset$vl.future & dataset$visits.future
dataset$disengaged.category[!is.na(dataset$years.since.vl.and.visit) & dataset$years.since.vl.and.visit<=2] = "0-2"
dataset$disengaged.category[!is.na(dataset$years.since.vl.and.visit) & dataset$years.since.vl.and.visit>2] = ">2"
dataset$disengaged.category[is.na(dataset$years.since.vl.and.visit)] = "missing"


dataset.2007 <- dataset
dataset <- dataset[dataset$date>=2012.5,]


print("Preparing Dataset for disengaged, for fitting the probability of true disengage")
disengaged <-dataset[!dataset$engaged.now & dataset$disengaged.category=="0-2",]

disengaged$future.state[!is.na(disengaged$engaged.future) & !disengaged$engaged.future] = "remain"
disengaged$future.state[!is.na(disengaged$engaged.future) & !is.na(disengaged$suppressed.future)
                        & disengaged$engaged.future & !disengaged$suppressed.future] = "reengage.unsuppress"
disengaged$future.state[!is.na(disengaged$engaged.future) & !is.na(disengaged$suppressed.future)
                        & disengaged$engaged.future & disengaged$suppressed.future] = "reengage.suppress"
disengaged$future.state[is.na(disengaged$future.state)] = "missing"


disengaged <-disengaged[disengaged$future.state!="missing",]
disengaged$future.state <- factor(disengaged$future.state, levels = c("reengage.unsuppress","reengage.suppress", "remain"))
disengaged$age.category <- factor(disengaged$age.category, levels = c("35-45","13-25","25-35","45-55","55+"))
disengaged$sex <- factor(disengaged$sex, levels = c("male","female"))
disengaged$race <- factor(disengaged$race, levels = c("other","black","hispanic"))
disengaged$risk <- factor(disengaged$risk, levels = c("heterosexual","idu","msm","msm_idu"))
disengaged$sex.risk <- factor(disengaged$sex.risk, levels = c("msm","msm_idu",
                                                              "heterosexual_male","heterosexual_female",
                                                              "idu_male","idu_female"))

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


##-----------------------------------##
##------- Total disengagement -------##
##-----------------------------------##
all.engaged <-dataset[!is.na(dataset$engaged.now) & dataset$engaged.now,]

all.engaged$future.state[!is.na(all.engaged$engaged.future) & !all.engaged$engaged.future]="lost"
all.engaged$future.state[!is.na(all.engaged$engaged.future) & all.engaged$engaged.future]="remain"

all.engaged$age.category <- factor(all.engaged$age.category, levels = c("35-45","13-25","25-35","45-55","55+"))
all.engaged$sex <- factor(all.engaged$sex, levels = c("male","female"))
all.engaged$race <- factor(all.engaged$race, levels = c("other","black","hispanic"))
all.engaged$risk <- factor(all.engaged$risk, levels = c("heterosexual","idu","msm","msm_idu"))
all.engaged$sex.risk <- factor(all.engaged$sex.risk, levels = c("msm","msm_idu",
                                                                              "heterosexual_male","heterosexual_female",
                                                                              "idu_male","idu_female"))

all.engaged$lost.future = as.numeric(all.engaged$future.state=='lost')


#### Logistic models for Total Disengagement  ####
if (use.gee==T)
{
    ## --> Lost
    print("Fitting logistic model for All CNICS --> lost, WITH individual slopes")
    model.all.to.lost.slopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year
                                         + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                         data=all.engaged, id=id, family=binomial, corstr="exchangeable")
    
    print("Fitting logistic model for All CNICS --> lost, WITHOUT individual slopes")
    model.all.to.lost.noslopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
                                           data=all.engaged, id=id, family=binomial, corstr="exchangeable")
} else
{
    ## --> Lost
    print("Fitting logistic model for All CNICS --> lost, WITH individual slopes, no GEE")
    model.all.to.lost.slopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year
                                      + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                      data=all.engaged, family=binomial)
    
    print("Fitting logistic model for All CNICS --> lost, WITHOUT individual slopes, no GEE")
    model.all.to.lost.noslopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year,
                                        data=all.engaged, family=binomial)
}  




##-----------------------------------##
##-- DATASET 1: Unsuppressed naive --##
##-----------------------------------##
print("Preparing Dataset for unsuppressed-naive")
naive.mask = !is.na(dataset.2007$art.naive.now) & dataset.2007$art.naive.now  & (is.na(dataset.2007$suppressed.now) | !dataset.2007$suppressed.now)
unsuppressed.naive <- dataset.2007[naive.mask,]

unsuppressed.naive$future.state[!is.na(unsuppressed.naive$art.naive.future) & unsuppressed.naive$art.naive.future 
                                & (is.na(unsuppressed.naive$suppressed.future) | !unsuppressed.naive$suppressed.future)]="remain"
unsuppressed.naive$future.state[!is.na(unsuppressed.naive$engaged.future) & !is.na(unsuppressed.naive$suppressed.future) 
                                &   unsuppressed.naive$engaged.future & unsuppressed.naive$suppressed.future]="suppress"
unsuppressed.naive$future.state[!is.na(unsuppressed.naive$engaged.future) & !unsuppressed.naive$engaged.future]="lost"
unsuppressed.naive$future.state[is.na(unsuppressed.naive$future.state)] = "missing"



unsuppressed.naive <-unsuppressed.naive[unsuppressed.naive$future.state!="missing",]
unsuppressed.naive$future.state <- factor(unsuppressed.naive$future.state, levels = c("suppress","lost", "remain"))
unsuppressed.naive$age.category <- factor(unsuppressed.naive$age.category, levels = c("35-45","13-25","25-35","45-55","55+"))
unsuppressed.naive$sex <- factor(unsuppressed.naive$sex, levels = c("male","female"))
unsuppressed.naive$race <- factor(unsuppressed.naive$race, levels = c("other","black","hispanic"))
unsuppressed.naive$risk <- factor(unsuppressed.naive$risk, levels = c("heterosexual","idu","msm","msm_idu"))
unsuppressed.naive$sex.risk <- factor(unsuppressed.naive$sex.risk, levels = c("msm","msm_idu",
                                                                                  "heterosexual_male","heterosexual_female",
                                                                                  "idu_male","idu_female"))


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
    sapply(sort(unique(imputed.unsuppressed.naive$relative.year)), function(year){
        mask = imputed.unsuppressed.naive$relative.year==year
        mean(imputed.unsuppressed.naive$future.state[mask]=='lost')
    })
    
    
    sapply(sort(unique(imputed.unsuppressed.naive$relative.year)), function(year){
        mask = imputed.unsuppressed.naive$relative.year==year
        mean(imputed.unsuppressed.naive$future.state[mask]=='suppress')
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

## Prune the dates
keep.years.mask = imputed.unsuppressed.naive$date > 2008 #& imputed.unsuppressed.naive.start.art$date < 2018
imputed.unsuppressed.naive = imputed.unsuppressed.naive[keep.years.mask,]

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
    
    unsuppressed.naive$lost.future = unsuppressed.naive$future.state=='lost'
    unimputed.naive.to.lost.noslopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
                                           data=unsuppressed.naive, id=id, family=binomial, corstr="exchangeable")
    
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
engaged.unsuppressed <-dataset[dataset$engaged.now & !dataset$suppressed.now & !is.na(dataset$suppressed.now),]
unsuppressed.failing <- engaged.unsuppressed[!engaged.unsuppressed$art.naive.now,]

unsuppressed.failing$future.state[!is.na(unsuppressed.failing$engaged.future) & !is.na(unsuppressed.failing$suppressed.future) 
                                  & unsuppressed.failing$engaged.future & !unsuppressed.failing$suppressed.future]="remain"
unsuppressed.failing$future.state[!is.na(unsuppressed.failing$engaged.future) & !is.na(unsuppressed.failing$suppressed.future) 
                                & unsuppressed.failing$engaged.future & unsuppressed.failing$suppressed.future]="suppress"
unsuppressed.failing$future.state[!is.na(unsuppressed.failing$engaged.future) & !unsuppressed.failing$engaged.future] ="lost"
unsuppressed.failing$future.state[is.na(unsuppressed.failing$future.state)] = "missing"


unsuppressed.failing <-unsuppressed.failing[unsuppressed.failing$future.state!="missing",]
unsuppressed.failing$future.state <- factor(unsuppressed.failing$future.state, levels = c("suppress","lost", "remain"))
unsuppressed.failing$age.category <- factor(unsuppressed.failing$age.category, levels = c("35-45","13-25","25-35","45-55","55+"))
unsuppressed.failing$sex <- factor(unsuppressed.failing$sex, levels = c("male","female"))
unsuppressed.failing$race <- factor(unsuppressed.failing$race, levels = c("other","black","hispanic"))
unsuppressed.failing$risk <- factor(unsuppressed.failing$risk, levels = c("heterosexual","idu","msm","msm_idu"))
unsuppressed.failing$sex.risk <- factor(unsuppressed.failing$sex.risk, levels = c("msm","msm_idu",
                                                                                  "heterosexual_male","heterosexual_female",
                                                                                  "idu_male","idu_female"))


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

imputed.unsuppressed.failing$suppressed.future = as.numeric(imputed.unsuppressed.failing$future.state=='suppress')
imputed.unsuppressed.failing$lost.future = as.numeric(imputed.unsuppressed.failing$future.state=='lost')

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
    
    unsuppressed.failing$lost.future = unsuppressed.failing$future.state=='lost'
    unimputed.failing.to.lost.noslopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
                                             data=unsuppressed.failing, id=id, family=binomial, corstr="exchangeable")
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
##--- DATASET 3: Engaged suppressed---##
##------------------------------------##

print("Preparing Dataset for engaged-suppressed")
engaged.suppressed <-dataset[dataset$engaged.now & dataset$suppressed.now & !is.na(dataset$suppressed.now),]

# Add in durable or not
n.intervals.for.durable = 4
n.suppressed = dim(engaged.suppressed)[1]
length.of.suppression = sapply(1:n.suppressed, function(i){
    id = engaged.suppressed$id[i]
    date = engaged.suppressed$date[i]
    dates.we.want = date - (1:n.intervals.for.durable-1) * .5
    
    dataset.indices.for.id = (1:dim(dataset)[1])[dataset$id==id]
    dataset.indices = sapply(dates.we.want, function(d){
        mask = dataset$date[dataset.indices.for.id] == d
        if (any(mask))
            dataset.indices.for.id[mask][1]
        else
            NA
    })
    
    suppressed = dataset$suppressed.now[dataset.indices]
    naive = dataset$suppressed.now[dataset.indices]
    
    # if all values are suppressed (not missing) - durable
    # if any values are not suppressed and not naive - recent
    # otherwise (some values missing, but all present are suppressed) - we don't know
    
    if (all(!is.na(suppressed) & suppressed))
        'durable'
    else if (any((!is.na(suppressed) & !suppressed) | 
                 (!is.na(naive) & !naive)))
        'recent'
    else
        NA
})
engaged.suppressed$length.of.suppression = length.of.suppression

# some code to check what is above
if (1==2)
{
    cbind(engaged.suppressed$id[1:length(length.of.suppression)],length.of.suppression)
    
    i = 100
    cbind(engaged.suppressed[i,c('id','date','suppressed.now','art.naive.now')], projected=length.of.suppression[i])
    mask=dataset$id==engaged.suppressed$id[i];z=dataset[mask,c('id','date','suppressed.now','art.naive.now')];z=z[order(z$date),];z
}

# Split the dataset into durable and recent
# and do all the code below for both

if(separate.durable.recent==T){
    recent.suppressed = engaged.suppressed[!is.na(engaged.suppressed$length.of.suppression) & 
                                               engaged.suppressed$length.of.suppression=="recent",]
    
    durable.suppressed = engaged.suppressed[!is.na(engaged.suppressed$length.of.suppression) & 
                                               engaged.suppressed$length.of.suppression=="durable",]
    
    
    ##-------------------------------------##
    ##--- DATASET 3.1: Recent suppressed---##
    ##-------------------------------------##
    
    recent.suppressed$future.state[!is.na(recent.suppressed$engaged.future) & !is.na(recent.suppressed$suppressed.future) 
                                    & recent.suppressed$engaged.future & recent.suppressed$suppressed.future] ="remain"
    recent.suppressed$future.state[!is.na(recent.suppressed$engaged.future) & !is.na(recent.suppressed$suppressed.future) &
                                        recent.suppressed$engaged.future & !recent.suppressed$suppressed.future]="unsuppress"
    recent.suppressed$future.state[!is.na(recent.suppressed$engaged.future) & !recent.suppressed$engaged.future] ="lost"
    recent.suppressed$future.state[is.na(recent.suppressed$future.state)] = "missing"
    
    
    recent.suppressed <-recent.suppressed[recent.suppressed$future.state!="missing",]
    recent.suppressed$future.state <- factor(recent.suppressed$future.state, levels = c("unsuppress","lost","remain"))
    recent.suppressed$age.category <- factor(recent.suppressed$age.category, levels = c("35-45","13-25","25-35","45-55","55+"))
    recent.suppressed$sex <- factor(recent.suppressed$sex, levels = c("male","female"))
    recent.suppressed$race <- factor(recent.suppressed$race, levels = c("other","black","hispanic"))
    recent.suppressed$risk <- factor(recent.suppressed$risk, levels = c("heterosexual","idu","msm","msm_idu"))
    recent.suppressed$sex.risk <- factor(recent.suppressed$sex.risk, levels = c("msm","msm_idu",
                                                                                  "heterosexual_male","heterosexual_female",
                                                                                  "idu_male","idu_female"))
    
    
    model.suppressed.if.not.truly.disengaged.RS <- geeglm(suppressed.future ~ age.category + sex.risk + race +
                                                              relative.year + relative.year:age.category + relative.year:sex.risk + relative.year:race,
                                                          data=recent.suppressed[recent.suppressed$future.state!='lost',], id=id, family = binomial, corstr = "exchangeable")
    
    
    p.truly.disengaged.RS = expit(predict.glm(model.truly.disengaged, newdata=recent.suppressed))
    p.truly.disengaged.RS[recent.suppressed$future.state=='remain'] = 0
    p.truly.disengaged.RS[recent.suppressed$future.state=='unsuppress'] = 0
    
    p.suppressed.if.not.truly.disengaged.RS = expit(predict.glm(model.suppressed.if.not.truly.disengaged.RS, newdata=recent.suppressed))
    p.suppressed.if.not.truly.disengaged.RS[recent.suppressed$future.state=='remain'] = 1
    p.suppressed.if.not.truly.disengaged.RS[recent.suppressed$future.state=='unsuppress'] = 0
    
    N.IMPUTATIONS = 4
    imputed.recent.suppressed = NULL
    
    for (i in 1:N.IMPUTATIONS)
    {
        simulated.lost.RS=rbinom(n=length(p.truly.disengaged.RS), size=1, prob=p.truly.disengaged.RS)
        simulated.suppressed.RS=rbinom(n=length(p.suppressed.if.not.truly.disengaged.RS), size=1, prob=p.suppressed.if.not.truly.disengaged.RS)
        
        recent.suppressed.sim = recent.suppressed
        recent.suppressed.sim$future.state[simulated.lost.RS==1] = 'lost'
        recent.suppressed.sim$future.state[simulated.lost.RS==0 & simulated.suppressed.RS==1] = 'remain'
        recent.suppressed.sim$future.state[simulated.lost.RS==0 & simulated.suppressed.RS==0] = 'unsuppress'
        
        imputed.recent.suppressed = rbind(imputed.recent.suppressed, recent.suppressed.sim)   
    }
    
    
    imputed.recent.suppressed$suppressed.future = as.numeric(imputed.recent.suppressed$future.state=='unsuppress')
    imputed.recent.suppressed$lost.future = as.numeric(imputed.recent.suppressed$future.state=='lost')
    
    #### Logistic models for Recent-Suppressed ####
    if (use.gee==T)
    {
        ## --> Failing
        print("Fitting logistic model for recent-suppressed --> unsuppressed-failing, WITH individual slopes")
        model.recent.to.failing.slopes <- geeglm(suppressed.future ~ age.category + sex.risk + race + relative.year
                                               + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                               data=imputed.recent.suppressed, id=id, family=binomial, corstr="exchangeable")
        
        print("Fitting logistic model for recent-suppressed --> unsuppressed-failing, WITHOUT individual slopes")
        model.recent.to.failing.noslopes <- geeglm(suppressed.future ~ age.category + sex.risk + race + relative.year,
                                                 data=imputed.recent.suppressed, id=id, family=binomial, corstr="exchangeable")
        
        ## --> Lost
        print("Fitting logistic model for recent-suppressed --> lost, WITH individual slopes")
        model.recent.to.lost.slopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year
                                            + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                            data=imputed.recent.suppressed, id=id, family=binomial, corstr="exchangeable")
        
        print("Fitting logistic model for recent-suppressed --> lost, WITHOUT individual slopes")
        model.recent.to.lost.noslopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
                                              data=imputed.recent.suppressed, id=id, family=binomial, corstr="exchangeable")
        
        recent.suppressed$lost.future = recent.suppressed$future.state=='lost'
        unimputed.recent.to.lost.noslopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
                                                  data=recent.suppressed, id=id, family=binomial, corstr="exchangeable")
    } else
    {
        ## --> Failing
        print("Fitting logistic model for recent-suppressed --> unsuppressed-failing, WITH individual slopes, no GEE")
        model.recent.to.failing.slopes <- glm(suppressed.future ~ age.category + sex.risk + race + relative.year
                                            + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                            data=imputed.recent.suppressed, family=binomial)
        
        print("Fitting logistic model for recent-suppressed --> unsuppressed-failing, WITHOUT individual slopes, no GEE")
        model.recent.to.failing.noslopes <- glm(suppressed.future ~ age.category + sex.risk + race + relative.year,
                                              data=imputed.recent.suppressed, family=binomial)
        
        ## --> Lost
        print("Fitting logistic model for recent-suppressed --> lost, WITH individual slopes, no GEE")
        model.recent.to.lost.slopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year
                                         + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                         data=imputed.recent.suppressed, family=binomial)
        
        print("Fitting logistic model for recent-suppressed --> lost, WITHOUT individual slopes, no GEE")
        model.recent.to.lost.noslopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year,
                                           data=imputed.recent.suppressed, family=binomial)
        
    }
    
    
    ##--------------------------------------##
    ##--- DATASET 3.2: Durable suppressed---##
    ##--------------------------------------##
    
    durable.suppressed$future.state[!is.na(durable.suppressed$engaged.future) & !is.na(durable.suppressed$suppressed.future) 
                                   & durable.suppressed$engaged.future & durable.suppressed$suppressed.future] ="remain"
    durable.suppressed$future.state[!is.na(durable.suppressed$engaged.future) & !is.na(durable.suppressed$suppressed.future) &
                                       durable.suppressed$engaged.future & !durable.suppressed$suppressed.future]="unsuppress"
    durable.suppressed$future.state[!is.na(durable.suppressed$engaged.future) & !durable.suppressed$engaged.future] ="lost"
    durable.suppressed$future.state[is.na(durable.suppressed$future.state)] = "missing"
    
    
    durable.suppressed <-durable.suppressed[durable.suppressed$future.state!="missing",]
    durable.suppressed$future.state <- factor(durable.suppressed$future.state, levels = c("unsuppress","lost","remain"))
    durable.suppressed$age.category <- factor(durable.suppressed$age.category, levels = c("35-45","13-25","25-35","45-55","55+"))
    durable.suppressed$sex <- factor(durable.suppressed$sex, levels = c("male","female"))
    durable.suppressed$race <- factor(durable.suppressed$race, levels = c("other","black","hispanic"))
    durable.suppressed$risk <- factor(durable.suppressed$risk, levels = c("heterosexual","idu","msm","msm_idu"))
    durable.suppressed$sex.risk <- factor(durable.suppressed$sex.risk, levels = c("msm","msm_idu",
                                                                                "heterosexual_male","heterosexual_female",
                                                                                "idu_male","idu_female"))
    
    
    model.suppressed.if.not.truly.disengaged.DS <- geeglm(suppressed.future ~ age.category + sex.risk + race +
                                                              relative.year + relative.year:age.category + relative.year:sex.risk + relative.year:race,
                                                          data=durable.suppressed[durable.suppressed$future.state!='lost',], id=id, family = binomial, corstr = "exchangeable")
    
    
    p.truly.disengaged.DS = expit(predict.glm(model.truly.disengaged, newdata=durable.suppressed))
    p.truly.disengaged.DS[durable.suppressed$future.state=='remain'] = 0
    p.truly.disengaged.DS[durable.suppressed$future.state=='unsuppress'] = 0
    
    p.suppressed.if.not.truly.disengaged.DS = expit(predict.glm(model.suppressed.if.not.truly.disengaged.DS, newdata=durable.suppressed))
    p.suppressed.if.not.truly.disengaged.DS[durable.suppressed$future.state=='remain'] = 1
    p.suppressed.if.not.truly.disengaged.DS[durable.suppressed$future.state=='unsuppress'] = 0
    
    N.IMPUTATIONS = 4
    imputed.durable.suppressed = NULL
    
    for (i in 1:N.IMPUTATIONS)
    {
        simulated.lost.DS=rbinom(n=length(p.truly.disengaged.DS), size=1, prob=p.truly.disengaged.DS)
        simulated.suppressed.DS=rbinom(n=length(p.suppressed.if.not.truly.disengaged.DS), size=1, prob=p.suppressed.if.not.truly.disengaged.DS)
        
        durable.suppressed.sim = durable.suppressed
        durable.suppressed.sim$future.state[simulated.lost.DS==1] = 'lost'
        durable.suppressed.sim$future.state[simulated.lost.DS==0 & simulated.suppressed.DS==1] = 'remain'
        durable.suppressed.sim$future.state[simulated.lost.DS==0 & simulated.suppressed.DS==0] = 'unsuppress'
        
        imputed.durable.suppressed = rbind(imputed.durable.suppressed, durable.suppressed.sim)   
    }
    
    
    imputed.durable.suppressed$suppressed.future = as.numeric(imputed.durable.suppressed$future.state=='unsuppress')
    imputed.durable.suppressed$lost.future = as.numeric(imputed.durable.suppressed$future.state=='lost')
    
    #### Logistic models for Durable-Suppressed ####
    if (use.gee==T)
    {
        ## --> Failing
        print("Fitting logistic model for durable-suppressed --> unsuppressed-failing, WITH individual slopes")
        model.durable.to.failing.slopes <- geeglm(suppressed.future ~ age.category + sex.risk + race + relative.year
                                                 + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                                 data=imputed.durable.suppressed, id=id, family=binomial, corstr="exchangeable")
        
        print("Fitting logistic model for durable-suppressed --> unsuppressed-failing, WITHOUT individual slopes")
        model.durable.to.failing.noslopes <- geeglm(suppressed.future ~ age.category + sex.risk + race + relative.year,
                                                   data=imputed.durable.suppressed, id=id, family=binomial, corstr="exchangeable")
        
        ## --> Lost
        print("Fitting logistic model for durable-suppressed --> lost, WITH individual slopes")
        model.durable.to.lost.slopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year
                                              + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                              data=imputed.durable.suppressed, id=id, family=binomial, corstr="exchangeable")
        
        print("Fitting logistic model for durable-suppressed --> lost, WITHOUT individual slopes")
        model.durable.to.lost.noslopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
                                                data=imputed.durable.suppressed, id=id, family=binomial, corstr="exchangeable")
        
        durable.suppressed$lost.future = durable.suppressed$future.state=='lost'
        unimputed.durable.to.lost.noslopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
                                                    data=durable.suppressed, id=id, family=binomial, corstr="exchangeable")
    } else
    {
        ## --> Failing
        print("Fitting logistic model for durable-suppressed --> unsuppressed-failing, WITH individual slopes, no GEE")
        model.durable.to.failing.slopes <- glm(suppressed.future ~ age.category + sex.risk + race + relative.year
                                              + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                              data=imputed.durable.suppressed, family=binomial)
        
        print("Fitting logistic model for durable-suppressed --> unsuppressed-failing, WITHOUT individual slopes, no GEE")
        model.durable.to.failing.noslopes <- glm(suppressed.future ~ age.category + sex.risk + race + relative.year,
                                                data=imputed.durable.suppressed, family=binomial)
        
        ## --> Lost
        print("Fitting logistic model for durable-suppressed --> lost, WITH individual slopes, no GEE")
        model.durable.to.lost.slopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year
                                           + age.category*relative.year + sex.risk*relative.year + race*relative.year,
                                           data=imputed.durable.suppressed, family=binomial)
        
        print("Fitting logistic model for durable-suppressed --> lost, WITHOUT individual slopes, no GEE")
        model.durable.to.lost.noslopes <- glm(lost.future ~ age.category + sex.risk + race + relative.year,
                                             data=imputed.durable.suppressed, family=binomial)
        
    }
    
    
    
    
    
    
    
} else # old code for engaged.suppressed as one dataset
{
    
    engaged.suppressed$future.state[!is.na(engaged.suppressed$engaged.future) & !is.na(engaged.suppressed$suppressed.future) 
                                    & engaged.suppressed$engaged.future & engaged.suppressed$suppressed.future] ="remain"
    engaged.suppressed$future.state[!is.na(engaged.suppressed$engaged.future) & !is.na(engaged.suppressed$suppressed.future) &
                                        engaged.suppressed$engaged.future & !engaged.suppressed$suppressed.future]="unsuppress"
    engaged.suppressed$future.state[!is.na(engaged.suppressed$engaged.future) & !engaged.suppressed$engaged.future] ="lost"
    engaged.suppressed$future.state[is.na(engaged.suppressed$future.state)] = "missing"
    
    
    engaged.suppressed <-engaged.suppressed[engaged.suppressed$future.state!="missing",]
    engaged.suppressed$future.state <- factor(engaged.suppressed$future.state, levels = c("unsuppress","lost","remain"))
    engaged.suppressed$age.category <- factor(engaged.suppressed$age.category, levels = c("35-45","13-25","25-35","45-55","55+"))
    engaged.suppressed$sex <- factor(engaged.suppressed$sex, levels = c("male","female"))
    engaged.suppressed$race <- factor(engaged.suppressed$race, levels = c("other","black","hispanic"))
    engaged.suppressed$risk <- factor(engaged.suppressed$risk, levels = c("heterosexual","idu","msm","msm_idu"))
    engaged.suppressed$sex.risk <- factor(engaged.suppressed$sex.risk, levels = c("msm","msm_idu",
                                                                                  "heterosexual_male","heterosexual_female",
                                                                                  "idu_male","idu_female"))
    
    
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
        
        print("Fitting logistic model for engaged-suppressed --> unsuppressed-failing, WITHOUT individual slopes")
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
        
        engaged.suppressed$lost.future = engaged.suppressed$future.state=='lost'
        unimputed.supp.to.lost.noslopes <- geeglm(lost.future ~ age.category + sex.risk + race + relative.year,
                                                  data=engaged.suppressed, id=id, family=binomial, corstr="exchangeable")
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
    
}

stop("let's stop here for now")

##------------------------------------##
##------- DATASET 4: Disengaged ------##
##------------------------------------##

disengaged$p.truly.disengaged = expit(predict.glm(model.truly.disengaged, newdata=disengaged))

mask = disengaged$future.state=='reengage.unsuppress'
p.suppressed.if.engaged.for.disengaged = expit(predict.glm(model.suppressed.if.engaged, newdata=disengaged[mask,]))
disengaged$p.truly.disengaged[mask] = prior.p.lost /
    (prior.p.lost + (1-prior.p.lost) * (1-p.suppressed.if.engaged.for.disengaged))

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
    naive.to.lost.noslopes.coefficients=(model.naive.to.lost.noslopes$coefficients +
                                             unimputed.naive.to.lost.noslopes$coefficients)/2,
    
    #Naive, variance
    naive.to.suppressed.slopes.variance=model.naive.to.supp.slopes$robust.variance,
    naive.to.suppressed.noslopes.variance=model.naive.to.supp.noslopes$robust.variance,
    naive.to.lost.slopes.variance=model.naive.to.lost.slopes$robust.variance,
    naive.to.lost.noslopes.variance=model.naive.to.lost.noslopes$robust.variance,
    
    #Failing, coefficients
    failing.to.suppressed.slopes.coefficients=model.failing.to.supp.slopes$coefficients,
    failing.to.suppressed.noslopes.coefficients=model.failing.to.supp.noslopes$coefficients,
    failing.to.lost.slopes.coefficients=model.failing.to.lost.slopes$coefficients,
    failing.to.lost.noslopes.coefficients=(model.failing.to.lost.noslopes$coefficients +
                                               unimputed.failing.to.lost.noslopes$coefficients)/2,
    
    #Failing, variance
    failing.to.suppressed.slopes.variance=model.failing.to.supp.slopes$robust.variance,
    failing.to.suppressed.noslopes.variance=model.failing.to.supp.noslopes$robust.variance,
    failing.to.lost.slopes.variance=model.failing.to.lost.slopes$robust.variance,
    failing.to.lost.noslopes.variance=model.failing.to.lost.noslopes$robust.variance,
    
    # Disengaged
    disengaged.slopes.coefficients=model.disengaged.slopes$coefficients,
    disengaged.slopes.variance=model.disengaged.slopes$robust.variance,
    disengaged.noslopes.coefficients=model.disengaged.noslopes$coefficients,
    disengaged.noslopes.variance=model.disengaged.noslopes$robust.variance,
    
    anchor.year=anchor.year)


if(separate.durable.recent==T){
    #Recent, coefficients
    output$recent.to.failing.slopes.coefficients=model.recent.to.failing.slopes$coefficients
    output$recent.to.failing.noslopes.coefficients=model.recent.to.failing.noslopes$coefficients
    output$recent.to.lost.slopes.coefficients=model.recent.to.lost.slopes$coefficients
    output$recent.to.lost.noslopes.coefficients=(model.recent.to.lost.noslopes$coefficients +
                                                         unimputed.recent.to.lost.noslopes$coefficients)/2
    
    #Recent, variance
    output$recent.to.failing.slopes.variance=model.recent.to.failing.slopes$robust.variance
    output$recent.to.failing.noslopes.variance=model.recent.to.failing.noslopes$robust.variance
    output$recent.to.lost.slopes.variance=model.recent.to.lost.slopes$robust.variance
    output$recent.to.lost.noslopes.variance=model.recent.to.lost.noslopes$robust.coefficients
    
    #Durable, coefficients
    output$durable.to.failing.slopes.coefficients=model.durable.to.failing.slopes$coefficients
    output$durable.to.failing.noslopes.coefficients=model.durable.to.failing.noslopes$coefficients
    output$durable.to.lost.slopes.coefficients=model.durable.to.lost.slopes$coefficients
    output$durable.to.lost.noslopes.coefficients=(model.durable.to.lost.noslopes$coefficients +
                                                     unimputed.durable.to.lost.noslopes$coefficients)/2
    
    #Durable, variance
    output$durable.to.failing.slopes.variance=model.durable.to.failing.slopes$robust.variance
    output$durable.to.failing.noslopes.variance=model.durable.to.failing.noslopes$robust.variance
    output$durable.to.lost.slopes.variance=model.durable.to.lost.slopes$robust.variance
    output$durable.to.lost.noslopes.variance=model.durable.to.lost.noslopes$robust.coefficients
    
} else
{
    #Suppressed, coefficients
    output$suppressed.to.failing.slopes.coefficients=model.supp.to.failing.slopes$coefficients
    output$suppressed.to.failing.noslopes.coefficients=model.supp.to.failing.noslopes$coefficients
    output$suppressed.to.lost.slopes.coefficients=model.supp.to.lost.slopes$coefficients
    output$suppressed.to.lost.noslopes.coefficients=(model.supp.to.lost.noslopes$coefficients +
                                                  unimputed.supp.to.lost.noslopes$coefficients)/2
    
    #Suppressed, variance
    output$suppressed.to.failing.slopes.variance=model.supp.to.failing.slopes$robust.variance
    output$suppressed.to.failing.noslopes.variance=model.supp.to.failing.noslopes$robust.variance
    output$suppressed.to.lost.slopes.variance=model.supp.to.lost.slopes$robust.variance
    output$suppressed.to.lost.noslopes.variance=model.supp.to.lost.noslopes$robust.coefficients
}


##-- Correct proportion reengaged --##

if(separate.durable.recent==T){
    imputed.all = rbind(
        imputed.recent.suppressed,
        imputed.durable.suppressed,
        imputed.unsuppressed.failing,
        imputed.unsuppressed.naive[imputed.unsuppressed.naive$relative.year>2,]
    )
} else{
    imputed.all = rbind(
        imputed.engaged.suppressed,
        imputed.unsuppressed.failing,
        imputed.unsuppressed.naive[imputed.unsuppressed.naive$relative.year>2,]
    )
}

predict.from.coefficients <- function(coefs, df)
{
    lo = coefs[1] +
        coefs["age.category13-25"] * as.numeric(df$age.category=='13-25') +
        coefs["age.category25-35"] * as.numeric(df$age.category=='25-35') +
        coefs["age.category45-55"] * as.numeric(df$age.category=='45-55') +
        coefs["age.category55+"] * as.numeric(df$age.category=='55+') +
        coefs["sex.riskmsm_idu"] * as.numeric(df$sex.risk=='msm_idu') +
        coefs["sex.riskheterosexual_male"] * as.numeric(df$sex.risk=='heterosexual_male') +
        coefs["sex.riskheterosexual_female"] * as.numeric(df$sex.risk=='heterosexual_female') +
        coefs["sex.riskidu_male"] * as.numeric(df$sex.risk=='idu_male') +
        coefs["sex.riskidu_female"] * as.numeric(df$sex.risk=='idu_female') +
        coefs["raceblack"] * as.numeric(df$race=='black') +
        coefs["racehispanic"] * as.numeric(df$race=='hispanic') +
        coefs["relative.year"] * as.numeric(df$relative.year)
    
    1 / (1+exp(-lo))
}

DESIRED.OVERALL.P.REENGAGED = mean(c(
    .5759, #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4120656/
    .6284, #https://link.springer.com/article/10.1186/s12981-021-00398-0
    .0991 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4607589/ 
    ))

actual.overall.p.reengaged = mean(predict.from.coefficients(output$disengaged.noslopes.coefficients, imputed.all))

reengaged.correction = logit(DESIRED.OVERALL.P.REENGAGED) - logit(actual.overall.p.reengaged)

output$disengaged.noslopes.coefficients[1] = output$disengaged.noslopes.coefficients[1] + reengaged.correction

##-- ONE LAST CORRECTION for proportion lost --##

print("CORRECTING THE LOST INTERCEPTS")

DESIRED.OVERALL.P.RETAINED = 0.775 
#this is the average of all four metric for https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4334738/
# (since we're not taking a stance on which metric is most valid)

desired.p.lost = 1-DESIRED.OVERALL.P.RETAINED
#calculate the actual p lost


desired.lo.lost = log(desired.p.lost) - log(1-desired.p.lost)
actual.p.lost = mean(imputed.all$lost.future)
actual.lo.lost = log(actual.p.lost) - log(1-actual.p.lost)

if(separate.durable.recent==T){
    probs = c(
        predict.from.coefficients(output$naive.to.lost.noslopes.coefficients, imputed.unsuppressed.naive[imputed.unsuppressed.naive$relative.year>2,]),
        predict.from.coefficients(output$failing.to.lost.noslopes.coefficients, imputed.unsuppressed.failing),
        predict.from.coefficients(output$recent.to.lost.noslopes.coefficients, imputed.recent.suppressed),
        predict.from.coefficients(output$durable.to.lost.noslopes.coefficients, imputed.durable.suppressed)
    ) 

} else{
    probs = c(
        predict.from.coefficients(output$naive.to.lost.noslopes.coefficients, imputed.unsuppressed.naive[imputed.unsuppressed.naive$relative.year>2,]),
        predict.from.coefficients(output$failing.to.lost.noslopes.coefficients, imputed.unsuppressed.failing),
        predict.from.coefficients(output$suppressed.to.lost.noslopes.coefficients, imputed.engaged.suppressed)
    )
}


mean.indiv.correction = mean(desired.lo.lost - 
                                 log(probs) + log(1-probs))

mean.correction = desired.lo.lost - actual.lo.lost

lo.correction = (mean.indiv.correction + mean.correction) / 2 
                   
# apply the correction

if(separate.durable.recent==T){
    output$naive.to.lost.noslopes.coefficients[1] = output$naive.to.lost.noslopes.coefficients[1] + lo.correction
    output$failing.to.lost.noslopes.coefficients[1] = output$failing.to.lost.noslopes.coefficients[1] + lo.correction
    output$recent.to.lost.noslopes.coefficients[1] = output$recent.to.lost.noslopes.coefficients[1] + lo.correction
    output$durable.to.lost.noslopes.coefficients[1] = output$durable.to.lost.noslopes.coefficients[1] + lo.correction
    
} else{
    output$naive.to.lost.noslopes.coefficients[1] = output$naive.to.lost.noslopes.coefficients[1] + lo.correction
    output$failing.to.lost.noslopes.coefficients[1] = output$failing.to.lost.noslopes.coefficients[1] + lo.correction
    output$suppressed.to.lost.noslopes.coefficients[1] = output$suppressed.to.lost.noslopes.coefficients[1] + lo.correction
}






##-- SAVE --##
if (!SAVE)
    print("Done - NOT SAVING HERE")

if (SAVE)
{
    print("Done - saving output")
    save(output, file=file.path('code','for Melissa', 'CNICS analysis', 
                                paste0('logistic_output_adj_', dataset.type, '_', analysis, '_', Sys.Date())))
    
    if (dataset.type=='real')
        save(output, file='cleaned_data/continuum/cnics_regression.Rdata')
}