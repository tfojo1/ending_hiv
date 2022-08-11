
##----------------------------------##
##--     OUTCOME_FORMATTING.R     --##
## 
##   Constants to format outcomes --##
##   for plots/tables in a        --##
##   user-intelligible manner     --##
##----------------------------------##

DATA.TYPE.NAMES = c(new='Reported Cases',
                    prevalence='Estimated Prevalence',
                    diagnosed='Knowledge of Status',
                    suppression='Viral Suppression',
                    mortality='Mortality Among PWH',
                    testing.rate='HIV Testing Rate',
                    testing='HIV Testing', #proportions
                    incidence='Incidence',
                    population='Population Size',
                    hiv.positive.population='HIV-Positive Population Size',
                    testing.period='HIV Testing Frequency',
                    prep='PrEP Uptake',
                    engagement='Engagement',
                    linkage='Linkage',
                    retention='Retention in Care',
                    suppression.of.engaged='Suppression among Engaged PWH',
                    gain.of.suppression='Gain of Suppression among Engaged PWH',
                    aids.diagnoses='Historical AIDS Diagnoses')


DATA.TYPE.AXIS.LABELS = c(
    incidence='Incident Infections (n)',
    new='Reported Cases (n)',
    prevalence='Prevalent Cases (n)',
    mortality='Deaths in PWH (n)',
    suppression='Proportion Suppressed (%)',
    diagnosed='Proportion Aware (%)',
    testing.rate='Avg. Tests per Person per Year',
    testing='Proportion Tested in the Past Year (%)',
    population='Population (n)',
    hiv.positive.population='Population (n)',
    testing.period='Years Between Tests',
    prep='Proportion on PrEP (%)',
    engagement='Proportion Engaged (%)',
    linkage='Proportion Linked (%)',
    suppression.of.engaged='Proportion Suppressed (%)',
    retention='Proportion Retained (%)',
    gain.of.suppression='Proportion Gaining Suppression (%)',
    aids.diagnoses='Reported AIDS Cases (n)'
)

DATA.TYPE.UNITS = c(
    incidence = 'Cases',
    new = 'Cases',
    prevalence = 'Cases',
    mortality = 'Deaths',
    suppression = 'Suppressed',
    diagnosed = 'Aware',
    testing.rate = 'Tests per year',
    testing = 'Tested',
    population = 'People',
    hiv.positive.population = 'People',
    testing.period = 'Years between tests',
    prep = 'Coverage',
    engagement='Engaged',
    linkage='Linked',
    suppression.of.engaged='Suppressed',
    retention='Retained',
    gain.of.suppression='Gained Suppression',
    aids.diagnoses = 'Cases'
)

DATA.TYPE.UNIT.DESCRIPTOR = c(
    incidence = 'infections',
    new = 'cases',
    prevalence = 'cases',
    mortality = 'deaths',
    suppression = '%',
    diagnosed = '%',
    testing.rate = 'tests per year',
    testing = '%',
    population = 'people',
    hiv.positive.population = 'people',
    testing.period = 'years between tests',
    prep = '%',
    engagement='%',
    linkage='%',
    suppression.of.engaged='%',
    retention='%',
    gain.of.suppression='%',
    aids.diagnoses='cases'
)

DATA.TYPE.VALUE.TYPE = c(
    incidence = 'count',
    new = 'count',
    prevalence = 'count',
    mortality = 'count',
    suppression = 'proportion',
    diagnosed = 'proportion',
    testing.rate = 'rate',
    testing = 'proportion',
    population = 'count',
    hiv.positive.population = 'count',
    testing.period = 'period',
    prep = 'proportion',
    engagement='proportion',
    linkage='proportion',
    suppression.of.engaged='proportion',
    retention='proportion',
    gain.of.suppression='proportion',
    aids.diagnoses='count'
)

CUMULATIVE.APPLIES.TO.DATA.TYPE = c(
    incidence = T,
    new = T,
    prevalence = F,
    mortality = T,
    suppression = F,
    diagnosed = F,
    testing.rate = F,
    testing = F,
    population = F,
    hiv.positive.population = F,
    testing.perio = F,
    prep = F,
    engagement=F,
    linkage=F,
    suppression.of.engaged=F,
    retention=F,
    gain.of.suppression=F,
    aids.diagnoses=T
)

CHANGE.STATISTIC.IS.RELATIVE = c(
    time.diff.relative=T,
    time.diff.absolute=F,
    cumulative=F,
    cumulative.diff.absolute=F,
    cumulative.diff.relative=T
)

CHANGE.STATISTIC.IS.DELTA = c(
    time.diff.relative=T,
    time.diff.absolute=T,
    cumulative=F,
    cumulative.diff.absolute=T,
    cumulative.diff.relative=T
)

CHANGE.STATISTIC.IS.CUMULATIVE = c(
    time.diff.relative=F,
    time.diff.absolute=F,
    cumulative=T,
    cumulative.diff.absolute=T,
    cumulative.diff.relative=T
)

DIMENSION.NAMES = c(age='Age',
                    race='Race',
                    subpopulation='Subpopulation',
                    sex='Sex',
                    risk='Risk Factor',
                    continuum='Continuum of Care Stage')

PRETTY.NAMES = list(age=c("13-24 years"="13-24", 
                          "25-34 years"="25-34",
                          "35-44 years"="35-44",
                          "45-54 years"="45-54",
                          "55+ years"="55+"),
                    race=c(black='Black', hispanic='Hispanic', other='Other'),
                    risk=c(msm='MSM', idu='IDU', msm_idu='MSM+IDU', heterosexual='Heterosexual'),
                    sex=c(male='Male', female='Female', heterosexual_male='Heterosexual Male', msm='MSM'))

get.pretty.dimension.names <- function(values)
{
    pull.names.from = PRETTY.NAMES
    names(pull.names.from) = NULL
    unlist(pull.names.from)[values]
}
