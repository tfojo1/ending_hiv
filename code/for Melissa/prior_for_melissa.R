

prior = join.distributions(
#-- Suppression --#
heterosexual.linked.or = Lognormal.Distribution(0, log(2)),
msm.linked.or = Lognormal.Distribution(0, log(2)),
idu.linked.or = Lognormal.Distribution(0, log(2)),
msm.idu.linked.or = Lognormal.Distribution(0, log(2)),

black.linked.or = Lognormal.Distribution(0, log(2)),
hispanic.linked.or = Lognormal.Distribution(0, log(2)),

age1.linked.or = Lognormal.Distribution(0, log(2)),
age2.linked.or = Lognormal.Distribution(0, log(2)),
age4.linked.or = Lognormal.Distribution(0, log(2)),
age5.linked.or = Lognormal.Distribution(0, log(2)),

heterosexual.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
msm.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
idu.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
msm.idu.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),

black.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
hispanic.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),

age1.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
age2.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
age4.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5),
age5.linked.slope.or = Lognormal.Distribution(0, 0.5*log(2)/5)
)

#A list with two components
# $intercepts = An array indexed [age,race,sex,risk] of intercepts on the log scale
# $slopes = An array indexed [age,race,sex,risk] of slopes on the log scale
get.linkage.model <- function(alphas, betas)
{
    
}

#Return an array indexed [year, age, race, sex, risk]
get.linked.proportions <- function(model, years)
{
    
}

calculate.linkage.likelihood <- function(model,
                                         total.linked,
                                         age.linked,
                                         race.linked,
                                         sex.linked,
                                         risk.linked,
                                         sim)
{
    p = sum(p.by.stratum * simulated.n) / sum(simulated.n)
}