load("~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/Ending_HIV/ending_hiv/code/for Melissa/CNICS analysis/multinomial_output_real_jheem.model_2021-10-25")
source('~/Dropbox/Documents_local/Hopkins/PhD/Dissertation/EHE/Ending_HIV/ending_hiv/code/for Melissa/process_multinomial_fit.R')

expit = function(lo){1/(1+exp(-lo))}

engaged.unsuppressed.p = generate.probs(output$engaged.unsuppressed.coefficients)
engaged.suppressed.p = generate.probs(output$engaged.suppressed.coefficients)
disengaged.p = expit(output$disengaged.coefficients)

range(engaged.unsuppressed.p)
range(engaged.suppressed.p)
range(disengaged.p)

engaged.suppressed.remain.p = engaged.suppressed.p[["p.ref"]]
engaged.suppressed.unsuppress.p = engaged.suppressed.p[["p.1"]]
engaged.suppressed.lost.p = engaged.suppressed.p[["p.2"]]

range(engaged.suppressed.remain.p)
range(engaged.suppressed.unsuppress.p)
range(engaged.suppressed.lost.p)

View(engaged.suppressed.unsuppress.p)
qplot(engaged.suppressed.remain.p)
qplot(engaged.suppressed.unsuppress.p)
qplot(engaged.suppressed.lost.p)


engaged.unsuppressed.remain.p = engaged.unsuppressed.p[["p.ref"]]
engaged.unsuppressed.suppress.p = engaged.unsuppressed.p[["p.1"]]
engaged.unsuppressed.lost.p = engaged.unsuppressed.p[["p.2"]]

range(engaged.unsuppressed.remain.p)
range(engaged.unsuppressed.suppress.p)
range(engaged.unsuppressed.lost.p)

# 
qplot(engaged.unsuppressed.remain.p)
qplot(engaged.unsuppressed.suppress.p)
qplot(engaged.unsuppressed.lost.p)
