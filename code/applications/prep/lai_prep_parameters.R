
##----------------------##
##-- HELPER FUNCTIONS --##
##----------------------##

persistence.to.coverage.fraction <- function(persistence)
{
    if (persistence==1)
        1
    else
    {
        lambda = -log(persistence)
        (1/lambda - exp(-lambda)/lambda)
    }
}

discontinuation.to.coverage.fraction <- function(discontinuation.rate)
{
    persistence.to.coverage.fraction(1-discontinuation.rate)
}


##--------------------------##
##-- DEFINE DISTRIBUTIONS --##
##--------------------------##


#ORAL PREP, from:
# https://pubmed.ncbi.nlm.nih.gov/26364263/
# (PROUD Study in MSM)

oral.efficacy.est = 0.86
oral.efficacy.lower = 0.64
oral.efficacy.upper = 0.96

#these are the ci relative to the estimate - ie log(bound/estimate) = log(bound) - log(estimate)
oral.ci.lower = log(1-oral.efficacy.upper)# - log(1-oral.efficacy.est)
oral.ci.upper = log(1-oral.efficacy.lower)# - log(1-oral.efficacy.est)

oral.log.sd = (oral.ci.upper - oral.ci.lower) / 2 / 1.96

DEFAULT.ORAL.PREP.MSM.RR.DIST = Lognormal.Distribution(meanlog = (oral.ci.upper+oral.ci.lower)/2, 
                                               sdlog = oral.log.sd, var.name = 'oral.prep.rr')

# INJECTABLE PREP, from:
# 
inj.ci.lower = log(.18)
inj.ci.upper = log(.62)

inj.log.sd = (inj.ci.upper - inj.ci.lower) / 2 / 1.96
inj.log.mean = (inj.ci.lower + inj.ci.upper)/2

DEFAULT.INJ.PREP.HR.DIST = Lognormal.Distribution(meanlog = inj.log.mean, sdlog = inj.log.sd, var.name = 'inj.vs.oral.hr')


# @Ruchita - find a better evidence-based mean and sd https://onlinelibrary.wiley.com/doi/full/10.1002/jia2.25252

oral.persistence.mean = .56
oral.prep.persistence.ci.upper = oral.persistence.mean + 1.96*sqrt((oral.persistence.mean*(1-oral.persistence.mean))/7148)
oral.prep.persistence.ci.lower = oral.persistence.mean-1.96*sqrt((oral.persistence.mean*(1-oral.persistence.mean))/7148) 

#oral.persistence.sd = (oral.prep.persistence.ci.upper - oral.prep.persistence.ci.lower) /3.92
oral.persistence.sd = .21 / 2 / 1.96
DEFAULT.ORAL.PREP.PERSISTENCE.DIST = Normal.Distribution(mean=oral.persistence.mean,
                                                 sd=oral.persistence.sd,
                                                 lower=0,
                                                 upper=1,
                                                 var.name='oral.prep.persistence')

DEFAULT.INJ.VS.ORAL.DISCONTINUATION.RR.DIST = Uniform.Distribution(min=.25,
                                                           max=1,
                                                           var.name='inj.vs.oral.discontinuation.rr')