
##---------------------##
##-- BALTIMORE CODES --##
##---------------------##

#source('code/data_managers/locale_mappings.R')
BALTIMORE.CITY = 24510
BALTIMORE.MSA = msa.for.county(BALTIMORE.CITY)
BALTIMORE.MSA.COUNTIES = counties.for.msa(BALTIMORE.MSA)

if (1==2)
{
    BALTIMORE.POPULATION = get.census.data(ALL.DATA.MANAGERS$census.collapsed.msm,
                                           years=ALL.DATA.MANAGERS$census.collapsed.msm$years,
                                           fips=BALTIMORE.MSA.COUNTIES,
                                           aggregate.counties = T
    )
    BALTIMORE.POPULATION = collapse.races(BALTIMORE.POPULATION)
    BALTIMORE.POPULATION.TOTALS = get.census.totals(ALL.DATA.MANAGERS$census.totals, BALTIMORE.MSA)

    idu.30d.prevalence = get.idu.prevalence(ALL.DATA.MANAGERS$idu, ALL.DATA.MANAGERS$census.full.msm, age.cutoffs = SETTINGS$AGE_CUTOFFS,
                                            use.30d=T, counties=BALTIMORE.MSA.COUNTIES, aggregate.counties = T)
#    idu.30d.prevalence = access(idu.30d.prevalence, sex=SETTINGS$SEXES)
    idu.ever.prevalence = get.idu.prevalence(ALL.DATA.MANAGERS$idu, ALL.DATA.MANAGERS$census.full.msm, age.cutoffs = SETTINGS$AGE_CUTOFFS,
                                             use.ever=T, counties=BALTIMORE.MSA.COUNTIES, aggregate.counties = T)
#    idu.ever.prevalence = access(idu.ever.prevalence, sex=SETTINGS$SEXES)

    BALTIMORE.POPULATION = stratify.population.idu(population = BALTIMORE.POPULATION,
                                                   active.idu.prevalence = idu.30d.prevalence,
                                                   idu.ever.prevalence = idu.ever.prevalence)
    BALTIMORE.POPULATION = apply(BALTIMORE.POPULATION, c('year','age','race','sex','risk'), function(x){x})

    BALTIMORE.POPULATION.CDC = recategorize.to.cdc.risk.strata(BALTIMORE.POPULATION)

    save(BALTIMORE.POPULATION, BALTIMORE.POPULATION.CDC, BALTIMORE.POPULATION.TOTALS, file='../code/cached/baltimore.population.Rdata')
}
load('code/cached/baltimore.population.Rdata')

#https://phpa.health.maryland.gov/OIDEOR/CHSE/Shared%20Documents/Baltimore-City.pdf
BALTIMORE.OLDER.NEW.DX = data.frame(year=1985:2011,
                                    total=c(309,433,570,878,1149,1193,1352,1336,1251,1139,1073,980,1032,992,904,840,931,1038,917,937,990,978,898,830,619,597,424),
                                    frac.msm=c(0.409,0.406,0.372,0.304,0.261,0.255,0.194,0.172,0.170,0.143,0.153,0.129,0.160,0.125,0.128,0.142,0.129,0.140,0.167,0.227,0.206,0.220,0.292,0.303,0.355,0.397,0.520),
                                    frac.idu=c(0.399,0.446,0.478,0.527,0.558,0.576,0.625,0.655,0.624,0.620,0.596,0.637,0.606,0.618,0.602,0.547,0.587,0.516,0.501,0.443,0.438,0.423,0.362,0.318,0.215,0.169,0.119),
                                    frac.het=c(0.047,0.052,0.059,0.074,0.090,0.068,0.120,0.112,0.149,0.196,0.191,0.193,0.195,0.210,0.226,0.281,0.251,0.306,0.291,0.303,0.320,0.309,0.316,0.355,0.411,0.388,0.345)
)
BALTIMORE.OLDER.NEW.DX$msm = BALTIMORE.OLDER.NEW.DX$total * BALTIMORE.OLDER.NEW.DX$frac.msm
BALTIMORE.OLDER.NEW.DX$idu = BALTIMORE.OLDER.NEW.DX$total * BALTIMORE.OLDER.NEW.DX$frac.idu
BALTIMORE.OLDER.NEW.DX$het = BALTIMORE.OLDER.NEW.DX$total * BALTIMORE.OLDER.NEW.DX$frac.het

if (1==2)
{
    ggplot(melt(BALTIMORE.OLDER.NEW.DX[,c('year','msm','idu','het')], id.vars = 'year'),
           aes(year,value, color=variable)) + geom_line() + geom_point()
}
