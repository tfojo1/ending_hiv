
set.seed(12345)

N = 1000
ids = as.character(sample(1:(N/4), N, replace=T))
unique.ids = unique(ids)
n = length(unique.ids)

POTENTIAL.OUTCOMES = c('engaged_unsuppressed','engaged_suppressed','disengaged')
POTENTIAL.RACES = c('black','hispanic','other')
POTENTIAL.SEX.RISK = c('msm','msm_idu','idu_male','idu_female','heterosexual_male','heterosexual_female')

ages = rnorm(n, 50, 10)
races = POTENTIAL.RACES[ceiling(runif(n,0.0001, length(POTENTIAL.RACES)))]
risk.sexes = POTENTIAL.SEX.RISK[ceiling(runif(n,0.0001, length(POTENTIAL.SEX.RISK)))]
aids.illness = rbinom(n, 1, .1)
    
names(ages) = names(races) = names(risk.sexes) = names(aids.illness) = unique.ids
dates = runif(N, 0, 8)

df = data.frame(
    id = ids,
    date = dates,
    outcome = POTENTIAL.OUTCOMES[ceiling(runif(N,0.0001,length(POTENTIAL.OUTCOMES)))],
    age = ages[ids] + dates,
    race = races[ids],
    risk.sex = risk.sexes[ids],
    aids.defining.illness = aids.illness[ids]
)