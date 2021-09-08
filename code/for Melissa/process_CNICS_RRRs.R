load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/Ending_HIV/ending_hiv/code/for Melissa/CNICS analysis/multinomial_output_real_2021-09-07")

## Engaged unsuppressed
covariates.EU = names(output$engaged.unsuppressed.coefficients[1:(length(output$engaged.unsuppressed.coefficients)/2)])
future.states.EU = c("suppress","lost") 
standard.error.EU = sqrt(diag(output$engaged.unsuppressed.variance))
ci.upper.EU = exp(output$engaged.unsuppressed.coefficients - qnorm(.025)*standard.error.EU)
ci.lower.EU = exp(output$engaged.unsuppressed.coefficients + qnorm(.025)*standard.error.EU)

tab = paste0(format(exp(output$engaged.unsuppressed.coefficients), digits = 2, nsmall=2),
             " [", format(ci.lower.EU, digits=2, nsmall=2),
             " to ", format(ci.upper.EU, digit=2, nsmall=2), "]")

engaged.unsuppressed.table = matrix(tab, (length(output$engaged.unsuppressed.coefficients)/2),2, 
                                    dimnames = list(covariates.EU,future.states.EU))


## Engaged suppressed
covariates.ES = names(output$engaged.suppressed.coefficients[1:(length(output$engaged.suppressed.coefficients)/2)])
future.states.ES = c("unsuppress","lost") 
standard.error.ES = sqrt(diag(output$engaged.suppressed.variance))
ci.upper.ES = exp(output$engaged.suppressed.coefficients - qnorm(.025)*standard.error.ES)
ci.lower.ES = exp(output$engaged.suppressed.coefficients + qnorm(.025)*standard.error.ES)

tab = paste0(format(exp(output$engaged.suppressed.coefficients), digits = 1, nsmall=1),
             " [", format(ci.lower.ES, digits=1, nsmall=1),
             " to ", format(ci.upper.ES, digit=1, nsmall=1), "]")

engaged.suppressed.table = matrix(tab, (length(output$engaged.suppressed.coefficients)/2),2, 
                                    dimnames = list(covariates.ES,future.states.ES))



## Disengaged
covariates.DE = names(output$disengaged.coefficients[1:(length(output$disengaged.coefficients)/2)])
future.states.DE = c("reengage.unsuppress","reengage.suppress") 
standard.error.DE = sqrt(diag(output$disengaged.variance))
ci.upper.DE = exp(output$disengaged.coefficients - qnorm(.025)*standard.error.DE)
ci.lower.DE = exp(output$disengaged.coefficients + qnorm(.025)*standard.error.DE)

tab = paste0(format(exp(output$disengaged.coefficients), digits = 1, nsmall=1),
             " [", format(ci.lower.DE, digits=1, nsmall=1),
             " to ", format(ci.upper.DE, digit=1, nsmall=1), "]")

disengaged.table = matrix(tab, (length(output$disengaged.coefficients)/2),2, 
                                  dimnames = list(covariates.DE,future.states.DE))


## Save tables
write.csv(engaged.unsuppressed.table, file=file.path('code','for Melissa', 'CNICS analysis', 
                                                   "engaged.unsuppressed.RRRs.csv"))

write.csv(engaged.suppressed.table, file=file.path('code','for Melissa', 'CNICS analysis', 
                                                     "engaged.suppressed.RRRs.csv"))

write.csv(disengaged.table, file=file.path('code','for Melissa', 'CNICS analysis', 
                                                     "disengaged.RRRs.csv"))

