

generate.probs <- function(coefficients)
{
    x.beta.1 = make.x.beta(coefficients, k=1)
    x.beta.2 = make.x.beta(coefficients, k=2)
    
    p.ref = 1/(1+exp(x.beta.1)+exp(x.beta.2))
    p.1 = exp(x.beta.1)*p.ref
    p.2 = exp(x.beta.2)*p.ref
    
    return(list(p.ref=p.ref,p.1=p.1,p.2=p.2))
    
}

make.x.beta <- function(coefficients,
                        risks = c("never_IDU","active_IDU","IDU_in_remission"),
                        sexes = c("msm","heterosexual_male","female"),
                        races = c("black","hispanic","other"),
                        ages = c("13-24 years","25-34 years","35-44 years","45-54 years", "55+ years"),
                        years = 2010:2020,
                        anchor.year = 2010,
                        k){

    x = coefficients
    x[] = 0
   
     if(k==1)
    x['beta10'] = 1
    
    if(k==2)
    x['beta20'] = 1
    
rv =  sapply(risks, function(risk){
        sapply(sexes, function(sex){
            sapply(races, function(race){
              sapply(ages, function(age){
                sapply(years, function(year){
                  x[paste0("relative.year:",k)] = year - anchor.year
                  
                  if(age=="13-24 years") {
                    x[paste0("age.category13-25:",k)] = 1
                    x[paste0("age.category13-25:relative.year:",k)] = year - anchor.year
                  }
                  
                  if(age=="25-34 years") {
                    x[paste0("age.category25-35:",k)] = 1
                    x[paste0("age.category25-35:relative.year:",k)] = year - anchor.year
                  }
                  if(age=="45-54 years") {
                    x[paste0("age.category45-55:",k)] = 1
                    x[paste0("age.category45-55:relative.year:",k)] = year - anchor.year
                  }
                  if(age=="55+ years") {
                    x[paste0("age.category55+:",k)] = 1
                    x[paste0("age.category55+:relative.year:",k)] = year - anchor.year
                  }
                  if(race=="black") {
                    x[paste0("raceblack:",k)] = 1
                    x[paste0("raceblack:relative.year:",k)] = year - anchor.year
                  }
                  if(race=="hispanic") {
                    x[paste0("racehispanic:",k)] = 1
                    x[paste0("racehispanic:relative.year:",k)] = year - anchor.year
                  }
                  
                  if(risk!="never_IDU" && sex=="msm"){
                    x[paste0("sex.riskmsm_idu:",k)] = 1
                    x[paste0("sex.riskmsm_idu:relative.year:",k)] = year - anchor.year
                  }
                  if(risk=="never_IDU" && sex=="heterosexual_male"){
                    x[paste0("sex.riskheterosexual_male:",k)] = 1
                    x[paste0("sex.riskheterosexual_male:relative.year:",k)] = year - anchor.year
                  }
                  if(risk=="never_IDU" && sex=="female"){
                    x[paste0("sex.riskheterosexual_female:",k)] = 1
                    x[paste0("sex.riskheterosexual_female:relative.year:",k)] = year - anchor.year
                  }
                  if(risk!="never_IDU" && sex=="heterosexual_male"){
                    x[paste0("sex.riskidu_male:",k)] = 1
                    x[paste0("sex.riskidu_male:relative.year:",k)] = year - anchor.year
                  }
                  if(risk!="never_IDU" && sex=="female"){
                    x[paste0("sex.riskidu_female:",k)] = 1
                    x[paste0("sex.riskidu_female:relative.year:",k)] = year - anchor.year
                  }
                  
                    sum(x*coefficients)
                        })
                })
            })
        })
    })

dim.names = list(year = as.character(years),
                 age = ages,
                 race = races,
                 sex = sexes,
                 risk = risks)

dim(rv) = sapply(dim.names,length)
dimnames(rv) = dim.names

rv
}



