load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/CNICS/synthetic_fixed_2021-08-06.Rdata")

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
        engaged.unsuppressed$future.state[i]="NA"
    
}

engaged.unsuppressed$future.state <- factor(engaged.unsuppressed$future.state)
engaged.unsuppressed$future.state <- relevel(engaged.unsuppressed$future.state, ref="remain")


## multgee version 
model.engaged.unsuppressed <- nomLORgee(future.state ~ sex + relative.year, data=engaged.unsuppressed, id=id)
summary(model.engaged.unsuppressed)
model.engaged.unsuppressed$coefficients

## nnet version
# model1 <- multinom(future.state ~ sex, data=engaged.unsuppressed)
# summary(model1)

##------------------------------------##
##--- DATASET 2: Engaged suppressed---##
##------------------------------------##
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
        engaged.suppressed$future.state[i]="NA"
    
}

engaged.suppressed$future.state <- factor(engaged.suppressed$future.state)
engaged.suppressed$future.state <- relevel(engaged.suppressed$future.state, ref="remain")


## multgee version 
model.engaged.suppressed <- nomLORgee(future.state ~ sex+ relative.year, data=engaged.suppressed, id=id)
summary(model.engaged.suppressed)
model.engaged.suppressed$coefficients

## nnet version
# model2 <- multinom(future.state ~ sex, data=engaged.suppressed)
# summary(model2)

##------------------------------------##
##------- DATASET 3: Disengaged ------##
##------------------------------------##
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
        disengaged$future.state[i]="NA"
    
}


disengaged$future.state <- factor(disengaged$future.state)
disengaged$future.state <- relevel(disengaged$future.state, ref="remain")

## multgee version 
model.disengaged <- nomLORgee(future.state ~ sex + relative.year, data=disengaged, id=id)
summary(model.disengaged)
model.disengaged$coefficients

## nnet version
# model3 <- multinom(future.state ~ sex, data=disengaged)
# summary(model3)

output <- list(engaged.unsuppressed.coefficients=model.engaged.unsuppressed$coefficients,
               engaged.unsuppressed.variance=model.engaged.unsuppressed$robust.variance,
               engaged.suppressed.coefficients=model.engaged.suppressed$coefficients,
               engaged.suppressed.variance=model.engaged.suppressed$robust.variance,
               disengaged.coefficients=model.disengaged$coefficients,
               disengaged.variance=model.disengaged$robust.variance)


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
