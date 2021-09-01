load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/Ending_HIV/ending_hiv/code/for Melissa/CNICS analysis/multinomial_output_synthetic_2021-09-01")

covariates.EU = names(output$engaged.unsuppressed.coefficients[1:(length(output$engaged.unsuppressed.coefficients)/3)])
future.states.EU = c("suppress","lost","missing") 
engaged.unsuppressed.table = matrix(exp(output$engaged.unsuppressed.coefficients),
                                    (length(output$engaged.unsuppressed.coefficients)/3),3, 
                                    dimnames = list(covariates.EU,future.states.EU))

covariates.ES = names(output$engaged.suppressed.coefficients[1:(length(output$engaged.suppressed.coefficients)/3)])
future.states.ES = c("unsuppress","lost","missing") 
engaged.suppressed.table = matrix(exp(output$engaged.suppressed.coefficients),
                                    (length(output$engaged.suppressed.coefficients)/3),3, 
                                    dimnames = list(covariates.ES,future.states.ES))

covariates.DE = names(output$disengaged.coefficients[1:(length(output$disengaged.coefficients)/3)])
future.states.DE = c("reengage.unsuppress","reengage.suppress","missing") 
disengaged.table = matrix(exp(output$disengaged.coefficients),
                                  (length(output$disengaged.coefficients)/3),3, 
                                  dimnames = list(covariates.DE,future.states.DE))

write.csv(engaged.unsuppressed.table, file=file.path('code','for Melissa', 'CNICS analysis', 
                                                   "engaged.unsuppressed.RRRs.csv"))

write.csv(engaged.suppressed.table, file=file.path('code','for Melissa', 'CNICS analysis', 
                                                     "engaged.suppressed.RRRs.csv"))

write.csv(disengaged.table, file=file.path('code','for Melissa', 'CNICS analysis', 
                                                     "disengaged.RRRs.csv"))

