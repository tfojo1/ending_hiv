

coefs = output$engaged.suppressed.coefficients

slopes1 = coefs[grepl("relative\\.year:2", names(coefs))]


source('code/for Melissa/process_multinomial_fit.R')
engaged.suppressed.p = generate.probs(output$engaged.suppressed.coefficients)
engaged.unsuppressed.p = generate.probs(output$engaged.unsuppressed.coefficients)

apply(engaged.suppressed.p$p.ref, 'year', mean)
apply(engaged.suppressed.p$p.1, 'year', mean)
apply(engaged.suppressed.p$p.2, 'year', mean)

apply(engaged.unsuppressed.p$p.ref, 'year', mean)
apply(engaged.unsuppressed.p$p.1, 'year', mean)
apply(engaged.unsuppressed.p$p.2, 'year', mean)



get.ref.stratum = function(arr){
    arr[3,'other','msm','never_IDU']
}

apply(engaged.suppressed.p$p.ref, 'year', get.ref.stratum)
apply(engaged.suppressed.p$p.1, 'year', get.ref.stratum)
apply(engaged.suppressed.p$p.2, 'year', get.ref.stratum)

apply(engaged.unsuppressed.p$p.ref, 'year', get.ref.stratum)
apply(engaged.unsuppressed.p$p.1, 'year', get.ref.stratum)
apply(engaged.unsuppressed.p$p.2, 'year', get.ref.stratum)
