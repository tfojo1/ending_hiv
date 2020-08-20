

#Return a list with two elements
# $logit.intercept - an array indexed [age, race, sex, risk]
# $logit.slope - an array indexed [age, race, sex, risk]

create.strata.logistic.models <- function(age.proportions, #an array indexed[year, age]
                                          race.proportions, #an array indexed[year, race]
                                          sex.proportions, #an array indexed[year, sex]
                                          risk.proportions) #an array indexed[year, risk]
{
    
}


#Return a 4d array indexed
# age
# race
# sex
# risk
create.one.year.proportions <- function(age.proportions, #a vector with elements named 'age1', 'age2', 'age3', 'age4', 'age5',
                                        race.proportions, #a vector with elements named 'black', 'hispanic', 'other',
                                        sex.proportions, #a vector with elements named 'male','female',
                                        risk.proportions) #a vector with elements names 'msm', 'idu', 'msm_idu', 'heterosexual)
{
    or.age1 = age.proportions[1] * (1-age.proportions[3]) / (1-age.proportions[1]) / age.proportions[3]
}