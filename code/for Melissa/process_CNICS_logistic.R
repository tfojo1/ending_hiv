#load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/Ending_HIV/ending_hiv/code/for Melissa/CNICS analysis/logistic_output_adj_synthetic_jheem.model_2021-12-15")


##--------------------------------------##
##-  Unsuppressed naive --> suppressed -##
##--------------------------------------##

# With slopes
# (Need to fix SE estimates)
SE.naive.to.supp.slopes = sqrt(diag(output$naive.to.suppressed.slopes.variance))
ci.upper.naive.to.supp.slopes = exp(output$naive.to.suppressed.slopes.coefficients - qnorm(.025)*SE.naive.to.supp.slopes)
ci.lower.naive.to.supp.slopes = exp(output$naive.to.suppressed.slopes.coefficients + qnorm(.025)*SE.naive.to.supp.slopes)

tab = paste0(format(exp(output$naive.to.suppressed.slopes.coefficients), digits = 2, nsmall=2),
             " [", format(ci.upper.naive.to.supp.slopes, digits=2, nsmall=2),
             " to ", format(ci.lower.naive.to.supp.slopes, digit=2, nsmall=2), "]")

naive.to.supp.slopes.table = matrix(tab, (length(output$naive.to.suppressed.slopes.coefficients)),1, 
                                      dimnames = list(names(output$naive.to.suppressed.slopes.coefficients),"estimate (slopes)"))

# Without slopes    
coefs = output$naive.to.suppressed.slopes.coefficients
coefs[] = 0
coefs[names(output$naive.to.suppressed.noslopes.coefficients)] = output$naive.to.suppressed.noslopes.coefficients

SE.naive.to.supp.noslopes = sqrt(diag(output$naive.to.suppressed.noslopes.variance))
ci.upper.naive.to.supp.noslopes = exp(output$naive.to.suppressed.noslopes.coefficients - qnorm(.025)*SE.naive.to.supp.noslopes)
ci.lower.naive.to.supp.noslopes = exp(output$naive.to.suppressed.noslopes.coefficients + qnorm(.025)*SE.naive.to.supp.noslopes)

tab = paste0(format(exp(coefs), digits = 2, nsmall=2),
             " [", format(ci.upper.naive.to.supp.noslopes, digits=2, nsmall=2),
             " to ", format(ci.lower.naive.to.supp.noslopes, digit=2, nsmall=2), "]")

naive.to.supp.noslopes.table = matrix(tab, (length(coefs)),1, 
                                        dimnames = list(names(coefs),"estimate (no slopes)"))

naive.to.supp.combined.table = cbind(naive.to.supp.slopes.table, naive.to.supp.noslopes.table)



##--------------------------------------##
##----  Unsuppressed naive --> lost ----##
##--------------------------------------##

# With slopes
# (Need to fix SE estimates)
SE.naive.to.lost.slopes = sqrt(diag(output$naive.to.lost.slopes.variance))
ci.upper.naive.to.lost.slopes = exp(output$naive.to.lost.slopes.coefficients - qnorm(.025)*SE.naive.to.lost.slopes)
ci.lower.naive.to.lost.slopes = exp(output$naive.to.lost.slopes.coefficients + qnorm(.025)*SE.naive.to.lost.slopes)

tab = paste0(format(exp(output$naive.to.lost.slopes.coefficients), digits = 2, nsmall=2),
             " [", format(ci.upper.naive.to.lost.slopes, digits=2, nsmall=2),
             " to ", format(ci.lower.naive.to.lost.slopes, digit=2, nsmall=2), "]")

naive.to.lost.slopes.table = matrix(tab, (length(output$naive.to.lost.slopes.coefficients)),1, 
                                      dimnames = list(names(output$naive.to.lost.slopes.coefficients),"estimate (slopes)"))

# Without slopes    
coefs = output$naive.to.lost.slopes.coefficients
coefs[] = 0
coefs[names(output$naive.to.lost.noslopes.coefficients)] = output$naive.to.lost.noslopes.coefficients

SE.naive.to.lost.noslopes = sqrt(diag(output$naive.to.lost.noslopes.variance))
ci.upper.naive.to.lost.noslopes = exp(output$naive.to.lost.noslopes.coefficients - qnorm(.025)*SE.naive.to.lost.noslopes)
ci.lower.naive.to.lost.noslopes = exp(output$naive.to.lost.noslopes.coefficients + qnorm(.025)*SE.naive.to.lost.noslopes)

tab = paste0(format(exp(coefs), digits = 2, nsmall=2),
             " [", format(ci.upper.naive.to.lost.noslopes, digits=2, nsmall=2),
             " to ", format(ci.lower.naive.to.lost.noslopes, digit=2, nsmall=2), "]")

naive.to.lost.noslopes.table = matrix(tab, (length(coefs)),1, 
                                        dimnames = list(names(coefs),"estimate (no slopes)"))

naive.to.lost.combined.table = cbind(naive.to.lost.slopes.table, naive.to.lost.noslopes.table)





##----------------------------------------##
##-  Unsuppressed failing --> suppressed -##
##----------------------------------------##

# With slopes
# (Need to fix SE estimates)
SE.failing.to.supp.slopes = sqrt(diag(output$failing.to.suppressed.slopes.variance))
ci.upper.failing.to.supp.slopes = exp(output$failing.to.suppressed.slopes.coefficients - qnorm(.025)*SE.failing.to.supp.slopes)
ci.lower.failing.to.supp.slopes = exp(output$failing.to.suppressed.slopes.coefficients + qnorm(.025)*SE.failing.to.supp.slopes)

tab = paste0(format(exp(output$failing.to.suppressed.slopes.coefficients), digits = 2, nsmall=2),
             " [", format(ci.upper.failing.to.supp.slopes, digits=2, nsmall=2),
             " to ", format(ci.lower.failing.to.supp.slopes, digit=2, nsmall=2), "]")

failing.to.supp.slopes.table = matrix(tab, (length(output$failing.to.suppressed.slopes.coefficients)),1, 
                                     dimnames = list(names(output$failing.to.suppressed.slopes.coefficients),"estimate (slopes)"))

# Without slopes    
coefs = output$failing.to.suppressed.slopes.coefficients
coefs[] = 0
coefs[names(output$failing.to.suppressed.noslopes.coefficients)] = output$failing.to.suppressed.noslopes.coefficients

SE.failing.to.supp.noslopes = sqrt(diag(output$failing.to.suppressed.noslopes.variance))
ci.upper.failing.to.supp.noslopes = exp(output$failing.to.suppressed.noslopes.coefficients - qnorm(.025)*SE.failing.to.supp.noslopes)
ci.lower.failing.to.supp.noslopes = exp(output$failing.to.suppressed.noslopes.coefficients + qnorm(.025)*SE.failing.to.supp.noslopes)

tab = paste0(format(exp(coefs), digits = 2, nsmall=2),
             " [", format(ci.upper.failing.to.supp.noslopes, digits=2, nsmall=2),
             " to ", format(ci.lower.failing.to.supp.noslopes, digit=2, nsmall=2), "]")

failing.to.supp.noslopes.table = matrix(tab, (length(coefs)),1, 
                                       dimnames = list(names(coefs),"estimate (no slopes)"))
    
failing.to.supp.combined.table = cbind(failing.to.supp.slopes.table, failing.to.supp.noslopes.table)




##----------------------------------------##
##----  Unsuppressed failing --> lost ----##
##----------------------------------------##

# With slopes
# (Need to fix SE estimates)
SE.failing.to.lost.slopes = sqrt(diag(output$failing.to.lost.slopes.variance))
ci.upper.failing.to.lost.slopes = exp(output$failing.to.lost.slopes.coefficients - qnorm(.025)*SE.failing.to.lost.slopes)
ci.lower.failing.to.lost.slopes = exp(output$failing.to.lost.slopes.coefficients + qnorm(.025)*SE.failing.to.lost.slopes)

tab = paste0(format(exp(output$failing.to.lost.slopes.coefficients), digits = 2, nsmall=2),
             " [", format(ci.upper.failing.to.lost.slopes, digits=2, nsmall=2),
             " to ", format(ci.lower.failing.to.lost.slopes, digit=2, nsmall=2), "]")

failing.to.lost.slopes.table = matrix(tab, (length(output$failing.to.lost.slopes.coefficients)),1, 
                                     dimnames = list(names(output$failing.to.lost.slopes.coefficients),"estimate (slopes)"))

# Without slopes    
coefs = output$failing.to.lost.slopes.coefficients
coefs[] = 0
coefs[names(output$failing.to.lost.noslopes.coefficients)] = output$failing.to.lost.noslopes.coefficients

SE.failing.to.lost.noslopes = sqrt(diag(output$failing.to.lost.noslopes.variance))
ci.upper.failing.to.lost.noslopes = exp(output$failing.to.lost.noslopes.coefficients - qnorm(.025)*SE.failing.to.lost.noslopes)
ci.lower.failing.to.lost.noslopes = exp(output$failing.to.lost.noslopes.coefficients + qnorm(.025)*SE.failing.to.lost.noslopes)

tab = paste0(format(exp(coefs), digits = 2, nsmall=2),
             " [", format(ci.upper.failing.to.lost.noslopes, digits=2, nsmall=2),
             " to ", format(ci.lower.failing.to.lost.noslopes, digit=2, nsmall=2), "]")

failing.to.lost.noslopes.table = matrix(tab, (length(coefs)),1, 
                                       dimnames = list(names(coefs),"estimate (no slopes)"))

failing.to.lost.combined.table = cbind(failing.to.lost.slopes.table, failing.to.lost.noslopes.table)




##-----------------------------------##
##-  Engaged suppressed --> failing -##
##-----------------------------------##

# With slopes
# (Need to fix SE estimates)
SE.supp.to.failing.slopes = sqrt(diag(output$suppressed.to.failing.slopes.variance))
ci.upper.supp.to.failing.slopes = exp(output$suppressed.to.failing.slopes.coefficients - qnorm(.025)*SE.supp.to.failing.slopes)
ci.lower.supp.to.failing.slopes = exp(output$suppressed.to.failing.slopes.coefficients + qnorm(.025)*SE.supp.to.failing.slopes)

tab = paste0(format(exp(output$suppressed.to.failing.slopes.coefficients), digits = 2, nsmall=2),
             " [", format(ci.upper.supp.to.failing.slopes, digits=2, nsmall=2),
             " to ", format(ci.lower.supp.to.failing.slopes, digit=2, nsmall=2), "]")

supp.to.failing.slopes.table = matrix(tab, (length(output$suppressed.to.failing.slopes.coefficients)),1, 
                                     dimnames = list(names(output$suppressed.to.failing.slopes.coefficients),"estimate (slopes)"))

# Without slopes    
coefs = output$suppressed.to.failing.slopes.coefficients
coefs[] = 0
coefs[names(output$suppressed.to.failing.noslopes.coefficients)] = output$suppressed.to.failing.noslopes.coefficients

SE.supp.to.failing.noslopes = sqrt(diag(output$suppressed.to.failing.noslopes.variance))
ci.upper.supp.to.failing.noslopes = exp(output$suppressed.to.failing.noslopes.coefficients - qnorm(.025)*SE.supp.to.failing.noslopes)
ci.lower.supp.to.failing.noslopes = exp(output$suppressed.to.failing.noslopes.coefficients + qnorm(.025)*SE.supp.to.failing.noslopes)

tab = paste0(format(exp(coefs), digits = 2, nsmall=2),
             " [", format(ci.upper.supp.to.failing.noslopes, digits=2, nsmall=2),
             " to ", format(ci.lower.supp.to.failing.noslopes, digit=2, nsmall=2), "]")

supp.to.failing.noslopes.table = matrix(tab, (length(coefs)),1, 
                                       dimnames = list(names(coefs),"estimate (no slopes)"))

supp.to.failing.combined.table = cbind(supp.to.failing.slopes.table, supp.to.failing.noslopes.table)




##----------------------------------------##
##-----  Engaged suppressed --> lost -----##
##----------------------------------------##

# With slopes
# (Need to fix SE estimates)
SE.supp.to.lost.slopes = sqrt(diag(output$suppressed.to.lost.slopes.variance))
ci.upper.supp.to.lost.slopes = exp(output$suppressed.to.lost.slopes.coefficients - qnorm(.025)*SE.supp.to.lost.slopes)
ci.lower.supp.to.lost.slopes = exp(output$suppressed.to.lost.slopes.coefficients + qnorm(.025)*SE.supp.to.lost.slopes)

tab = paste0(format(exp(output$suppressed.to.lost.slopes.coefficients), digits = 2, nsmall=2),
             " [", format(ci.upper.supp.to.lost.slopes, digits=2, nsmall=2),
             " to ", format(ci.lower.supp.to.lost.slopes, digit=2, nsmall=2), "]")

supp.to.lost.slopes.table = matrix(tab, (length(output$suppressed.to.lost.slopes.coefficients)),1, 
                                     dimnames = list(names(output$suppressed.to.lost.slopes.coefficients),"estimate (slopes)"))

# Without slopes    
coefs = output$suppressed.to.lost.slopes.coefficients
coefs[] = 0
coefs[names(output$suppressed.to.lost.noslopes.coefficients)] = output$suppressed.to.lost.noslopes.coefficients

SE.supp.to.lost.noslopes = sqrt(diag(output$suppressed.to.lost.noslopes.variance))
ci.upper.supp.to.lost.noslopes = exp(output$suppressed.to.lost.noslopes.coefficients - qnorm(.025)*SE.supp.to.lost.noslopes)
ci.lower.supp.to.lost.noslopes = exp(output$suppressed.to.lost.noslopes.coefficients + qnorm(.025)*SE.supp.to.lost.noslopes)

tab = paste0(format(exp(coefs), digits = 2, nsmall=2),
             " [", format(ci.upper.supp.to.lost.noslopes, digits=2, nsmall=2),
             " to ", format(ci.lower.supp.to.lost.noslopes, digit=2, nsmall=2), "]")

supp.to.lost.noslopes.table = matrix(tab, (length(coefs)),1, 
                                       dimnames = list(names(coefs),"estimate (no slopes)"))

supp.to.lost.combined.table = cbind(supp.to.lost.slopes.table, supp.to.lost.noslopes.table)




##---------------------------------##
##----------  Disengaged ----------##
##---------------------------------##

# With slopes
# (Need to fix SE estimates)
SE.disengaged.slopes = sqrt(diag(output$disengaged.slopes.variance))
ci.upper.disengaged.slopes = exp(output$disengaged.slopes.coefficients - qnorm(.025)*SE.disengaged.slopes)
ci.lower.disengaged.slopes = exp(output$disengaged.slopes.coefficients + qnorm(.025)*SE.disengaged.slopes)

tab = paste0(format(exp(output$disengaged.slopes.coefficients), digits = 2, nsmall=2),
             " [", format(ci.upper.disengaged.slopes, digits=2, nsmall=2),
             " to ", format(ci.lower.disengaged.slopes, digit=2, nsmall=2), "]")

disengaged.slopes.table = matrix(tab, (length(output$disengaged.slopes.coefficients)),1, 
                                   dimnames = list(names(output$disengaged.slopes.coefficients),"estimate (slopes)"))

# Without slopes    
coefs = output$disengaged.slopes.coefficients
coefs[] = 0
coefs[names(output$disengaged.noslopes.coefficients)] = output$disengaged.noslopes.coefficients

SE.disengaged.noslopes = sqrt(diag(output$disengaged.noslopes.variance))
ci.upper.disengaged.noslopes = exp(output$disengaged.noslopes.coefficients - qnorm(.025)*SE.disengaged.noslopes)
ci.lower.disengaged.noslopes = exp(output$disengaged.noslopes.coefficients + qnorm(.025)*SE.disengaged.noslopes)

tab = paste0(format(exp(coefs), digits = 2, nsmall=2),
             " [", format(ci.upper.disengaged.noslopes, digits=2, nsmall=2),
             " to ", format(ci.lower.disengaged.noslopes, digit=2, nsmall=2), "]")

disengaged.noslopes.table = matrix(tab, (length(coefs)),1, 
                                     dimnames = list(names(coefs),"estimate (no slopes)"))

disengaged.combined.table = cbind(disengaged.slopes.table, disengaged.noslopes.table)


full.col.names = c("naive -> supp (slopes)", "naive -> supp (noslopes)",
                   "naive -> lost (slopes)", "naive -> lost (noslopes)",
                   "failing -> supp (slopes)", "failing -> supp (noslopes)",
                   "failing -> lost (slopes)", "failing -> lost (noslopes)",
                   "supp -> failing (slopes)", "supp -> failing (noslopes)",
                   "supp -> lost (slopes)", "supp -> lost (noslopes)",
                   "disengaged (slopes)", "disengaged (noslopes)")

full.table = matrix(data = c(naive.to.supp.combined.table, naive.to.lost.combined.table, 
                             failing.to.supp.combined.table, failing.to.lost.combined.table, 
                             supp.to.failing.combined.table, supp.to.lost.combined.table, 
                             disengaged.combined.table), length(output$failing.to.suppressed.slopes.coefficients), 14, 
                    dimnames = list(names(output$failing.to.suppressed.slopes.coefficients), full.col.names))
                    
                  

write.csv(full.table, file=file.path('code','for Melissa', 'CNICS analysis', 
                                                     "full.logistic.output.synthetic.csv"))
