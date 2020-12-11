
#source('../code/data_managers/locale_mappings.R')
#source('../code/setup/setup_jheem_components.R')

#source('../code/setup/default_jheem_settings.R')
#source('../code/parameters.R')

setup.components.for.msa <- function(msa,
                                     surv=msa.surveillance,
                                     population.year=2012,
                                     smooth.from.year=2010,
                                     smooth.to.year=2025,
                                     verbose=F,
                                     data.managers = ALL.DATA.MANAGERS,
                                     settings = SETTINGS,
                                     parameters = BASE_PARAMETER_VALUES,
                                     fix.strata.sizes=T,
                                     idu.transitions=NULL,
                                     seed.prevalence=T,
                                     zero.suppression.year=1996,
                                     zero.prep.year=2011,
                                     suppression.smooth.from.year=2010,
                                     suppression.z.scores=DEFAULT.SUPPRESSION.Z.SCORES,
                                     max.smoothed.suppressed.proportion=0.9,
                                     last.full.spike.year=1980,
                                     end.spike.year=1990,
                                     change.ratio.to.year=2020,
                                     start.spike.mortality.year=1970,
                                     peak.mortality.year=1995,
                                     end.spike.mortality.year=2000,
                                     include.hiv=T,
                                     idu.transition.years=c(2000,2020),
                                     allow.non.optimized.idu.transitions=F,
                                     idu.transition.end.year=2030)
{
    msa.counties = counties.for.msa(msa)

    ##----------------------##
    ##-- GET BEST GUESSES --##
    ##----------------------##

    if (verbose)
        print('**Pulling best guesses for MSM and IDU parameters')

    #-- MSM --#
    best.guess.msm.by.race = get.best.guess.msm.proportions.by.race(msa.counties,
                                                   data.managers$census.collapsed,
                                                   population.year)

    #-- IDU Prevalence --#
    idu.30d.prevalence = get.idu.prevalence(data.managers$idu, data.managers$census.full.msm, age.cutoffs = settings$AGE_CUTOFFS,
                                            use.30d=T, counties=msa.counties, aggregate.counties = T)
   # idu.30d.prevalence = access(idu.30d.prevalence, sex=settings$SEXES)
    idu.ever.prevalence = get.idu.prevalence(data.managers$idu, data.managers$census.full.msm, age.cutoffs = settings$AGE_CUTOFFS,
                                             use.ever=T, counties=msa.counties, aggregate.counties = T)
   # idu.ever.prevalence = access(idu.ever.prevalence, sex=settings$SEXES)

    #-- IDU Transitions --##
    if (is.null(idu.transitions))
    {
        optimized.idu.file = paste0('../code/setup/idu_transitions_by_msa/', msa, '.Rdata')
        if (1==2 && file.exists(optimized.idu.file))
        {
            load(optimized.idu.file)
        }
        else if (1==1 || allow.non.optimized.idu.transitions)
        {
            incident.idu = get.incident.idu.rates(data.managers$idu,
                                                  census=data.managers$census.full.msm,
                                                  counties=msa.counties,
                                                  age.cutoffs=settings$AGE_CUTOFFS,
                                                  race.mapping=BLACK.HISPANIC.OTHER.MAPPING)
            incident.idu = incident.idu[,settings$RACES,settings$SEXES]

            idu.remission = get.idu.remission.rates(data.managers$idu,
                                                    census=data.managers$census.full.msm,
                                                    counties=msa.counties,
                                                    age.cutoffs=settings$AGE_CUTOFFS,
                                                    race.mapping=BLACK.HISPANIC.OTHER.MAPPING)
            idu.remission = idu.remission[,settings$RACES,settings$SEXES]

            idu.relapse = get.idu.relapse.rates(data.managers$idu,
                                                census=data.managers$census.full.msm,
                                                counties=msa.counties,
                                                age.cutoffs=settings$AGE_CUTOFFS,
                                                race.mapping=BLACK.HISPANIC.OTHER.MAPPING)
            idu.relapse = idu.relapse[,settings$RACES, settings$SEXES]
        }
        else
            stop(paste0("No IDU transition parameters have been optimized for the ",
                        msa.names(msa),
                        " MSA. If you want to allow a rough guess, set allow.non.optimized.idu.transitions=TRUE"))
    }
    else
    {
        incident.idu = idu.transitions$incident.idu
        idu.remission = idu.transitions$idu.remission
        idu.relapse = idu.transitions$idu.relapse
    }

    ##------------------##
    ##-- RUN THE TEST --##
    ##------------------##

    #-- Initialize --#
    if (verbose)
        print('**Initializing Components')
    comps = initialize.jheem.components(settings=settings,
                                       fips=msa.counties,
                                       data.managers = data.managers,
                                       population.year=population.year,
                                       model.hiv.transmission = include.hiv)

    #-- Proportion MSM --#
    if (verbose)
        print('**Setting MSM Proportions')
    comps = set.proportions.msm.of.male(comps, black.proportion = best.guess.msm.by.race['black'],
                                        hispanic.proportion = best.guess.msm.by.race['hispanic'],
                                        other.proportion = best.guess.msm.by.race['other'],
                                        overwrite.base.proportions = T)

    #-- IDU Proportions --#
    if (verbose)
        print('**Setting IDU Proportions')
    comps = set.idu.proportions(comps, data.managers,
                                active.idu.prevalence = idu.30d.prevalence,
                                idu.ever.prevalence = idu.ever.prevalence)


    #-- Births already done by default --##

    #-- Mortality --#
    if (verbose)
        print('**Setting up mortality')
    comps = setup.general.mortality(comps, data.managers, parameters['idu.mortality'])
    comps = setup.hiv.mortality.rates(comps,
                                      r.peak=2*parameters['untreated.hiv.mortality'],
                                      r0=parameters['untreated.hiv.mortality'],
                                      r1=parameters['untreated.hiv.mortality'],
                                      r2=parameters['untreated.hiv.mortality'])

    #-- Initial Population already done by default --#
    if (seed.prevalence)
    {
        prevalence.sex.age = get.surveillance.data.rate(data.managers$msa.surveillance,
                                                        location.codes=msa,
                                                        data.type='prevalence',
                                                        census=data.managers$census.collapsed,
                                                        age=T, sex=T)

        prevalence.sex.race = get.surveillance.data.rate(data.managers$msa.surveillance,
                                                        location.codes=msa,
                                                        data.type='prevalence',
                                                        census=data.managers$census.collapsed,
                                                        race=T, sex=T)

        prev.years = as.numeric(dimnames(prevalence.sex.age)[['year']])
        diff.to.pop.year = abs(prev.years - population.year)
        prev.year = prev.years[diff.to.pop.year==min(diff.to.pop.year)][1]

        min.prevalence = min(access(prevalence.sex.age, year=as.character(prev.year)),
                             access(prevalence.sex.race, year=as.character(prev.year), race=c('black','hispanic','white')))

        comps = setup.initial.population(comps,
                                 seed.rate.per.stratum = min.prevalence)
    }


    #-- Transitions --#
    if (verbose)
        print('**Setting up IDU and HIV transitions')

    comps = setup.idu.transition.times(comps, idu.transition.years, end.year=idu.transition.end.year)

    comps = set.idu.transitions(comps,
                                incident.idu = incident.idu,
                                idu.remission = idu.remission,
                                idu.relapse = idu.relapse,
                                overwrite.elements = T)
    comps = set.hiv.transitions(comps,
                                acute.hiv.duration = parameters['acute.infection.duration'],
                                prep.screening.frequency = parameters['prep.screening.frequency'])
    
    aids.dx = get.surveillance.data(location.codes = msa, data.type='aids.diagnoses', years=1985:1993)
    fit = lm(log(aids.dx) ~ attr(aids.dx, 'years'))
    ramp.yearly.increase = exp(fit$coefficients[2])
    ramp.yearly.increase = min(2, max(1.2, ramp.yearly.increase))
    comps = setup.background.hiv.testing(comps,
                                         continuum.manager = data.managers$continuum,
                                         location=msa,
                                         years=smooth.from.year:smooth.to.year)
    comps = set.background.hiv.testing.ramp.up(comps,
                                               testing.ramp.up.yearly.increase=as.numeric(ramp.yearly.increase))
    comps = set.background.hiv.testing.ors(comps,
                                           msm.or.intercept=1,
                                           heterosexual.or.intercept=1,
                                           idu.or.intercept=1,
                                           black.or.intercept=1,
                                           hispanic.or.intercept=1,
                                           other.or.intercept=1,
                                           age1.or.intercept=1,
                                           age2.or.intercept=1,
                                           age3.or.intercept=1,
                                           age4.or.intercept=1,
                                           age5.or.intercept=1,
                                           
                                           total.or.slope=1,
                                           msm.or.slope=1,
                                           heterosexual.or.slope=1,
                                           idu.or.slope=1,
                                           black.or.slope=1,
                                           hispanic.or.slope=1,
                                           other.or.slope=1,
                                           age1.or.slope=1,
                                           age2.or.slope=1,
                                           age3.or.slope=1,
                                           age4.or.slope=1,
                                           age5.or.slope=1)
    
    #-- Suppression --#
    if (verbose)
        print('**Setting up suppression proportions')
    
    comps = setup.background.suppression(comps,
                                         continuum.manager=data.managers$continuum,
                                         location=msa,
                                         years=smooth.from.year:smooth.to.year)
    
    comps = set.background.suppression.ors(comps,
                                           msm.or.intercept=1,
                                           heterosexual.or.intercept=1,
                                           idu.or.intercept=1,
                                           black.or.intercept=1,
                                           hispanic.or.intercept=1,
                                           other.or.intercept=1,
                                           age1.or.intercept=1,
                                           age2.or.intercept=1,
                                           age3.or.intercept=1,
                                           age4.or.intercept=1,
                                           age5.or.intercept=1,
                                           
                                           total.or.slope=1,
                                           msm.or.slope=1,
                                           heterosexual.or.slope=1,
                                           idu.or.slope=1,
                                           black.or.slope=1,
                                           hispanic.or.slope=1,
                                           other.or.slope=1,
                                           age1.or.slope=1,
                                           age2.or.slope=1,
                                           age3.or.slope=1,
                                           age4.or.slope=1,
                                           age5.or.slope=1)
    
    comps = set.future.background.slopes(comps,
                                         future.supression.slope.or=1,
                                         future.testing.slope.or=1,
                                         future.prep.slope.or=1,
                                         after.year=2020)
    
    #-- PrEP --#
    if (verbose)
        print('**Setting up PrEP')
    prep.model = get.prep.model(ALL.DATA.MANAGERS$prep)
    if (prep.model$mixed.linear)
        comps = setup.background.prep(comps,
                                      prep.manager=ALL.DATA.MANAGERS$prep,
                                      years = max(prep.model$anchor.year, smooth.from.year):smooth.to.year,
                                      zero.prep.year=zero.prep.year)
    else
        comps = setup.background.prep(comps,
                                      prep.manager=ALL.DATA.MANAGERS$prep,
                                      years = max(zero.prep.year+1, smooth.from.year):smooth.to.year,
                                      zero.prep.year=zero.prep.year)
    
    comps = set.background.change.to.years(comps,
                                           testing.change.to.year=smooth.to.year,
                                           suppression.change.to.year=smooth.to.year,
                                           prep.change.to.year=smooth.to.year)
    
    #-- Transmission --#
    if (verbose)
        print('**Setting up Transmission')
    
    
    comps = setup.prep.susceptibility(comps,
                                      prep.rr.heterosexual = parameters['prep.rr.heterosexual'],
                                      prep.rr.msm = parameters['prep.rr.msm'],
                                      prep.rr.idu = parameters['prep.rr.idu'],
                                      prep.persistence = parameters['prep.persistence'])
    comps = setup.sexual.susceptibility(comps)
    
    comps = setup.transmissibility(comps,
                                   acute.transmissibility.rr=parameters['acute.transmissibility.rr'],
                                   diagnosed.needle.sharing.rr=parameters['diagnosed.needle.sharing.rr'],
                                   diagnosed.het.male.condomless.rr=parameters['diagnosed.het.male.condomless.rr'],
                                   diagnosed.female.condomless.rr=parameters['diagnosed.female.condomless.rr'],
                                   diagnosed.msm.condomless.rr=parameters['diagnosed.msm.condomless.rr'])
    
    comps = setup.sex.by.age(comps,
                             heterosexual.male.age.model = data.managers$pairing$sex.age.models[['heterosexual_male']],
                             female.age.model = data.managers$pairing$sex.age.models[['female']],
                             msm.age.model = data.managers$pairing$sex.age.models[['msm']],
                             overwrite.base.models=T)
    
    sex.by.race.oes = c(data.managers$pairing$msm.sex.by.race.oe, data.managers$pairing$het.sex.by.race.oe)
    mean.oe = sex.by.race.oes[[1]]
    for (i in 1:length(sex.by.race.oes))
        mean.oe = mean.oe + sex.by.race.oes[[i]]
    mean.oe = mean.oe / length(sex.by.race.oes)
    
    comps = setup.sex.by.race(comps,
                              black.black.oe=mean.oe['black','black'],
                              hispanic.hispanic.oe = mean.oe['hispanic','hispanic'],
                              other.other.oe = mean.oe['other','other'])
    #                              race.oes = mean.oe)
    comps = setup.sex.by.sex(comps,
                             fraction.msm.pairings.with.female = mean(data.managers$pairing$msm.sex.with.female.estimates),
                             oe.female.pairings.with.msm = parameters['proportion.msm.sex.with.female'],
                             fraction.heterosexual.male.pairings.with.male = 0.004)
    
    comps = setup.sex.by.idu(comps,
                             never.idu.sexual.oe=1,
                             idu.sexual.oe=1)
    
    comps = setup.idu.by.age(comps, age.model = data.managers$pairing$idu.age.model)
    comps = setup.idu.by.race(comps,
                              black.black.oe = data.managers$pairing$idu.oe.race['black','black'],
                              hispanic.hispanic.oe = data.managers$pairing$idu.oe.race['hispanic','hispanic'],
                              other.other.oe = data.managers$pairing$idu.oe.race['other','other'])
    comps = setup.idu.by.sex(comps, sex.oes = data.managers$pairing$idu.oe.sex)
    
    #-- Sexual availability --#
    
    #From Abma 2017
    comps = setup.sexual.availability.by.age(comps,
                                             age.index=1,
                                             availability.by.age = c('13'=.076, #from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6547075/
                                                                     '14'=.108, #halfway between
                                                                     '15'=.13,
                                                                     '16'=.26,
                                                                     '17'=.41,
                                                                     '18'=.55,
                                                                     '19'=.68) / .75)
    
    #from Tessler 2008
    # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2426743/
    older.sexual.availability = c(sapply(as.character(65:74), function(i){mean(c(67.0/83.7,39.5/61.6))}),
                                  sapply(as.character(75:85), function(i){mean(c(38.5/83.7,16.7/61.6))}))
    comps = setup.sexual.availability.by.age(comps,
                                             age.index=5,
                                             availability.by.age = older.sexual.availability)
    
    #from NSDUH 2015-2018
    #get.idu.availability.13.24()
    comps = setup.idu.availability.by.age(comps,
                                          age.index=1,
                                          availability.by.age = c('13'=.02,
                                                                  '14'=.02,
                                                                  '15'=.18,
                                                                  '16'=.18,
                                                                  '17'=.18,
                                                                  '18'=.18))
    
    #NSDUH 2015 and 2016 - 
    # https://www.samhsa.gov/data/report/results-2016-national-survey-drug-use-and-health-detailed-tables
    older.idu.availability = sapply(as.character(65:85), function(i){
        (4.2 + 5.3) / (14.1+10.1  + 10.0+15.0)
    })
    comps = setup.idu.availability.by.age(comps,
                                          age.index=5,
                                          availability.by.age = older.idu.availability)
    
    #-- Fix Pop Size --#
    if (fix.strata.sizes)
        comps = setup.fix.strata.sizes(comps,
                                       fix.strata.sizes = c(T,F),
                                       times=c(-Inf, population.year))
    
    
    attr(comps, 'location') = msa
    
    #-- Return --#
    comps
}
