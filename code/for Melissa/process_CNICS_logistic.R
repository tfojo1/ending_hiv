load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/Ending_HIV/ending_hiv/code/for Melissa/CNICS analysis/logistic_output_adj_real_jheem.model_2021-11-12")


##----------------------------------------##
##-  Engaged unsuppressed --> suppressed -##
##----------------------------------------##

# With slopes
# (Need to fix SE estimates)
SE.unsupp.to.supp.slopes = sqrt(diag(output$unsuppressed.to.suppressed.slopes.variance))
ci.upper.unsupp.to.supp.slopes = exp(output$unsuppressed.to.suppressed.slopes.coefficients - qnorm(.025)*SE.unsupp.to.supp.slopes)
ci.lower.unsupp.to.supp.slopes = exp(output$unsuppressed.to.suppressed.slopes.coefficients + qnorm(.025)*SE.unsupp.to.supp.slopes)

tab = paste0(format(exp(output$unsuppressed.to.suppressed.slopes.coefficients), digits = 2, nsmall=2),
             " [", format(ci.upper.unsupp.to.supp.slopes, digits=2, nsmall=2),
             " to ", format(ci.lower.unsupp.to.supp.slopes, digit=2, nsmall=2), "]")

unsupp.to.supp.slopes.table = matrix(tab, (length(output$unsuppressed.to.suppressed.slopes.coefficients)),1, 
                                     dimnames = list(names(output$unsuppressed.to.suppressed.slopes.coefficients),"estimate (slopes)"))

# Without slopes    
coefs = output$unsuppressed.to.suppressed.slopes.coefficients
coefs[] = 0
coefs[names(output$unsuppressed.to.suppressed.noslopes.coefficients)] = output$unsuppressed.to.suppressed.noslopes.coefficients

SE.unsupp.to.supp.noslopes = sqrt(diag(output$unsuppressed.to.suppressed.noslopes.variance))
ci.upper.unsupp.to.supp.noslopes = exp(output$unsuppressed.to.suppressed.noslopes.coefficients - qnorm(.025)*SE.unsupp.to.supp.noslopes)
ci.lower.unsupp.to.supp.noslopes = exp(output$unsuppressed.to.suppressed.noslopes.coefficients + qnorm(.025)*SE.unsupp.to.supp.noslopes)

tab = paste0(format(exp(coefs), digits = 2, nsmall=2),
             " [", format(ci.upper.unsupp.to.supp.noslopes, digits=2, nsmall=2),
             " to ", format(ci.lower.unsupp.to.supp.noslopes, digit=2, nsmall=2), "]")

unsupp.to.supp.noslopes.table = matrix(tab, (length(coefs)),1, 
                                       dimnames = list(names(coefs),"estimate (no slopes)"))
    
unsupp.to.supp.combined.table = cbind(unsupp.to.supp.slopes.table, unsupp.to.supp.noslopes.table)




##----------------------------------------##
##----  Engaged unsuppressed --> lost ----##
##----------------------------------------##

# With slopes
# (Need to fix SE estimates)
SE.unsupp.to.lost.slopes = sqrt(diag(output$unsuppressed.to.lost.slopes.variance))
ci.upper.unsupp.to.lost.slopes = exp(output$unsuppressed.to.lost.slopes.coefficients - qnorm(.025)*SE.unsupp.to.lost.slopes)
ci.lower.unsupp.to.lost.slopes = exp(output$unsuppressed.to.lost.slopes.coefficients + qnorm(.025)*SE.unsupp.to.lost.slopes)

tab = paste0(format(exp(output$unsuppressed.to.lost.slopes.coefficients), digits = 2, nsmall=2),
             " [", format(ci.upper.unsupp.to.lost.slopes, digits=2, nsmall=2),
             " to ", format(ci.lower.unsupp.to.lost.slopes, digit=2, nsmall=2), "]")

unsupp.to.lost.slopes.table = matrix(tab, (length(output$unsuppressed.to.lost.slopes.coefficients)),1, 
                                     dimnames = list(names(output$unsuppressed.to.lost.slopes.coefficients),"estimate (slopes)"))

# Without slopes    
coefs = output$unsuppressed.to.lost.slopes.coefficients
coefs[] = 0
coefs[names(output$unsuppressed.to.lost.noslopes.coefficients)] = output$unsuppressed.to.lost.noslopes.coefficients

SE.unsupp.to.lost.noslopes = sqrt(diag(output$unsuppressed.to.lost.noslopes.variance))
ci.upper.unsupp.to.lost.noslopes = exp(output$unsuppressed.to.lost.noslopes.coefficients - qnorm(.025)*SE.unsupp.to.lost.noslopes)
ci.lower.unsupp.to.lost.noslopes = exp(output$unsuppressed.to.lost.noslopes.coefficients + qnorm(.025)*SE.unsupp.to.lost.noslopes)

tab = paste0(format(exp(coefs), digits = 2, nsmall=2),
             " [", format(ci.upper.unsupp.to.lost.noslopes, digits=2, nsmall=2),
             " to ", format(ci.lower.unsupp.to.lost.noslopes, digit=2, nsmall=2), "]")

unsupp.to.lost.noslopes.table = matrix(tab, (length(coefs)),1, 
                                       dimnames = list(names(coefs),"estimate (no slopes)"))

unsupp.to.lost.combined.table = cbind(unsupp.to.lost.slopes.table, unsupp.to.lost.noslopes.table)




##----------------------------------------##
##-  Engaged suppressed --> unsuppressed -##
##----------------------------------------##

# With slopes
# (Need to fix SE estimates)
SE.supp.to.unsupp.slopes = sqrt(diag(output$suppressed.to.unsuppressed.slopes.variance))
ci.upper.supp.to.unsupp.slopes = exp(output$suppressed.to.unsuppressed.slopes.coefficients - qnorm(.025)*SE.supp.to.unsupp.slopes)
ci.lower.supp.to.unsupp.slopes = exp(output$suppressed.to.unsuppressed.slopes.coefficients + qnorm(.025)*SE.supp.to.unsupp.slopes)

tab = paste0(format(exp(output$suppressed.to.unsuppressed.slopes.coefficients), digits = 2, nsmall=2),
             " [", format(ci.upper.supp.to.unsupp.slopes, digits=2, nsmall=2),
             " to ", format(ci.lower.supp.to.unsupp.slopes, digit=2, nsmall=2), "]")

supp.to.unsupp.slopes.table = matrix(tab, (length(output$suppressed.to.unsuppressed.slopes.coefficients)),1, 
                                     dimnames = list(names(output$suppressed.to.unsuppressed.slopes.coefficients),"estimate (slopes)"))

# Without slopes    
coefs = output$suppressed.to.unsuppressed.slopes.coefficients
coefs[] = 0
coefs[names(output$suppressed.to.unsuppressed.noslopes.coefficients)] = output$suppressed.to.unsuppressed.noslopes.coefficients

SE.supp.to.unsupp.noslopes = sqrt(diag(output$suppressed.to.unsuppressed.noslopes.variance))
ci.upper.supp.to.unsupp.noslopes = exp(output$suppressed.to.unsuppressed.noslopes.coefficients - qnorm(.025)*SE.supp.to.unsupp.noslopes)
ci.lower.supp.to.unsupp.noslopes = exp(output$suppressed.to.unsuppressed.noslopes.coefficients + qnorm(.025)*SE.supp.to.unsupp.noslopes)

tab = paste0(format(exp(coefs), digits = 2, nsmall=2),
             " [", format(ci.upper.supp.to.unsupp.noslopes, digits=2, nsmall=2),
             " to ", format(ci.lower.supp.to.unsupp.noslopes, digit=2, nsmall=2), "]")

supp.to.unsupp.noslopes.table = matrix(tab, (length(coefs)),1, 
                                       dimnames = list(names(coefs),"estimate (no slopes)"))

supp.to.unsupp.combined.table = cbind(supp.to.unsupp.slopes.table, supp.to.unsupp.noslopes.table)




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


full.col.names = c("unsupp -> supp (slopes)", "unsupp -> supp (noslopes)",
                   "unsupp -> lost (slopes)", "unsupp -> lost (noslopes)",
                   "supp -> unsupp (slopes)", "supp -> unsupp (noslopes)",
                   "supp -> lost (slopes)", "supp -> lost (noslopes)",
                   "disengaged (slopes)", "disengaged (noslopes)")

full.table = matrix(data = c(unsupp.to.supp.combined.table, unsupp.to.lost.combined.table, 
                   supp.to.unsupp.combined.table, supp.to.lost.combined.table, 
                   disengaged.combined.table), length(output$unsuppressed.to.suppressed.slopes.coefficients), 10, 
                   dimnames = list(names(output$unsuppressed.to.suppressed.slopes.coefficients), full.col.names))

write.csv(full.table, file=file.path('code','for Melissa', 'CNICS analysis', 
                                                     "full.logistic.output.csv"))
