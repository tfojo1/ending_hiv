MELISSAS.FILE = "~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/CNICS/synthetic_fixed_2021-08-06.Rdata"
TODDS.FILE = '../../CNICS/cleaned_datasets/arch68/cnics_fixed_2021-08-06.Rdata'
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

#A quick fix to the mis-named age.category field
# (the synthetic dataset erroneously labels this 'age.category.randomized')
if (any(names(dataset)=='age.category.randomized'))
    names(dataset)[names(dataset)=='age.category.randomized'] = 'age.category'

library(nnet)
library(multgee)

anchor.year = 2010

dataset <- dataset[!dataset$sex=="Intersexed",]
dataset$relative.year <- dataset$date - anchor.year

# Creating three datasets
engaged <-dataset[dataset$engaged.now==TRUE,]
#Putting NA's in the unsuppressed for now. I think we said we'd treat as missing? 


##------------------------------------##
##-- DATASET 1: Engaged unsuppressed--##
##------------------------------------##

print("Preparing Dataset for engaged-unsuppressed")
engaged.unsuppressed <-dataset[dataset$engaged.now==TRUE & dataset$suppressed.now==FALSE,]

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

engaged.unsuppressed$future.state <- factor(engaged.unsuppressed$future.state, levels = c("suppress","lost","missing", "remain"))
# engaged.unsuppressed$future.state <- relevel(engaged.unsuppressed$future.state, ref="remain")


print("Fitting Model for engaged-unsuppressed")
## multgee version 
model.engaged.unsuppressed <- nomLORgee(future.state ~ age.category + sex + race + risk + relative.year
                                        + site + art.naive + years.in.care + aids.defining.illness,
                                        data=engaged.unsuppressed, id=id)

exp(model.engaged.unsuppressed$coefficients[grepl('beta',names(model.engaged.unsuppressed$coefficients))])
# exp(model.engaged.unsuppressed$coefficients[c(1,25,49)])
table(engaged.unsuppressed$future.state)

## nnet version
# model1 <- multinom(future.state ~ sex, data=engaged.unsuppressed)
# summary(model1)

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


engaged.suppressed$future.state <- factor(engaged.suppressed$future.state, levels = c("unsuppress","lost","missing", "remain"))
# engaged.suppressed$future.state <- relevel(engaged.suppressed$future.state, ref="remain")


## multgee version 
print("Fitting Model for engaged-suppressed")
model.engaged.suppressed <- nomLORgee(future.state ~ age.category + sex + race + risk + relative.year
                                      + site + art.naive + years.in.care + aids.defining.illness,
                                      data=engaged.suppressed, id=id)

## Problem is clearly with age.category - only runs when I remove it; doesn't even run if I *only* include it (and nothing else)


## nnet version
# model2 <- multinom(future.state ~ sex, data=engaged.suppressed)
# summary(model2)

##------------------------------------##
##------- DATASET 3: Disengaged ------##
##------------------------------------##

print("Preparing Dataset for disengaged")
disengaged <-dataset[dataset$engaged.now==FALSE,]

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


disengaged$future.state <- factor(disengaged$future.state, levels = c("reengage.unsuppress","reengage.suppress","missing", "remain"))
# disengaged$future.state <- relevel(disengaged$future.state, ref="remain")

## multgee version 
print("Fitting Model for disengaged")
model.disengaged <- nomLORgee(future.state ~ age.category + sex + race + risk + relative.year 
                              + site + art.naive + years.in.care + aids.defining.illness,
                              data=disengaged, id=id)

## Problem is clearly with age.category - only runs when I remove it; doesn't even run if I *only* include it (and nothing else)


## nnet version
# model3 <- multinom(future.state ~ sex, data=disengaged)
# summary(model3)

output <- list(engaged.unsuppressed.coefficients=model.engaged.unsuppressed$coefficients,
               engaged.unsuppressed.variance=model.engaged.unsuppressed$robust.variance,
               engaged.suppressed.coefficients=model.engaged.suppressed$coefficients,
               engaged.suppressed.variance=model.engaged.suppressed$robust.variance,
               disengaged.coefficients=model.disengaged$coefficients,
               disengaged.variance=model.disengaged$robust.variance)

print("Done - saving output")
save(output, file=file.path('code','for Melissa', 'CNICS analysis', 
                            paste0('multinomial_output_', dataset.type, '_', Sys.Date())))

### Old way of doing this
# engaged.unsuppressed$remain = as.numeric(engaged.unsuppressed$engaged.future==TRUE 
#                                          & engaged.unsuppressed$suppressed.future==FALSE)
# engaged.unsuppressed$suppress = as.numeric(engaged.unsuppressed$engaged.future==TRUE 
#                                            & engaged.unsuppressed$suppressed.future==TRUE)
# engaged.unsuppressed$lost = as.numeric(engaged.unsuppressed$engaged.future==FALSE)
# 
# for (i in 1:nrow(engaged.unsuppressed)) {
#     if(engaged.unsuppressed$remain[i]==1 && !is.na(engaged.unsuppressed$remain[i])) {
#         engaged.unsuppressed$future.state[i]="remain"
#         
#     } else if(engaged.unsuppressed$suppress[i]==1 && !is.na(engaged.unsuppressed$suppress[i])){
#         engaged.unsuppressed$future.state[i]="suppress"
#         
#     } else if(engaged.unsuppressed$lost[i]==1 && !is.na(engaged.unsuppressed$lost[i])){
#         engaged.unsuppressed$future.state[i]="lost"
#         
#     } else 
#         engaged.unsuppressed$future.state[i]="NA"
#     
# }
# 
# 
# engaged.suppressed$remain = as.numeric(engaged.suppressed$engaged.future==TRUE 
#                                        & engaged.suppressed$suppressed.future==TRUE)
# engaged.suppressed$unsuppress = as.numeric(engaged.suppressed$engaged.future==TRUE 
#                                            & engaged.unsuppressed$suppressed.future==FALSE)
# engaged.suppressed$lost = as.numeric(engaged.suppressed$engaged.future==FALSE)
