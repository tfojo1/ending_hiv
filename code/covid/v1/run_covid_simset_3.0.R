
BASE.START.NORMALIZE.TIME = 2021 + 66/365 #March 8 - CDC advisory for safe activities for those vaccinated
BASE.NORMAL.TIME = 2021 + 184/365 #July 4
DELAYED.START.NORMALIZE.TIME = BASE.START.NORMALIZE.TIME + 0.5
DELAYED.NORMAL.TIME = BASE.NORMAL.TIME + 0.5

run.covid.for.simset <- function(simset,
                                 scenario=c('base',
                                            'delayed.hiv.care',
                                            'rebound.sexual.transmission')[1],
                                 run.to=2030,
                                 intervention=NULL)
{
    if (scenario=='delayed.hiv.care')
        do.run.covid.for.simset(simset,
                                run.to=run.to,
                                suppression.start.normalize.time = DELAYED.START.NORMALIZE.TIME,
                                suppression.normal.time = DELAYED.NORMAL.TIME,
                                prep.start.normalize.time = DELAYED.START.NORMALIZE.TIME,
                                prep.normal.time = DELAYED.NORMAL.TIME,
                                testing.start.normalize.time = DELAYED.START.NORMALIZE.TIME,
                                testing.normal.time = DELAYED.NORMAL.TIME,
                                sexual.transmission.start.normalize.time = BASE.START.NORMALIZE.TIME,
                                sexual.transmission.normal.time = BASE.NORMAL.TIME,
                                intervention=intervention)
    else if (scenario=='rebound.sexual.transmission')
        do.run.covid.for.simset(simset,
                                run.to=run.to,
                                suppression.start.normalize.time = BASE.START.NORMALIZE.TIME,
                                suppression.normal.time = BASE.NORMAL.TIME,
                                prep.start.normalize.time = BASE.START.NORMALIZE.TIME,
                                prep.normal.time = BASE.NORMAL.TIME,
                                testing.start.normalize.time = BASE.START.NORMALIZE.TIME,
                                testing.normal.time = BASE.NORMAL.TIME,
                                sexual.transmission.start.normalize.time = BASE.NORMAL.TIME,
                                sexual.transmission.normal.time = DELAYED.NORMAL.TIME,
                                model.sexual.transmission.increase = T,
                                sexual.transmission.start.increase.time=BASE.START.NORMALIZE.TIME,
                                sexual.transmission.increase.maximal.time=BASE.NORMAL.TIME,
                                intervention=intervention)
    else if (scenario=='rebound.sex.delayed.hiv.care')
        do.run.covid.for.simset(simset,
                                run.to=run.to,
                                suppression.start.normalize.time = DELAYED.START.NORMALIZE.TIME,
                                suppression.normal.time = DELAYED.NORMAL.TIME,
                                prep.start.normalize.time = DELAYED.START.NORMALIZE.TIME,
                                prep.normal.time = DELAYED.NORMAL.TIME,
                                testing.start.normalize.time = DELAYED.START.NORMALIZE.TIME,
                                testing.normal.time = DELAYED.NORMAL.TIME,
                                sexual.transmission.start.normalize.time = BASE.NORMAL.TIME,
                                sexual.transmission.normal.time = DELAYED.NORMAL.TIME,
                                model.sexual.transmission.increase = T,
                                sexual.transmission.start.increase.time=BASE.START.NORMALIZE.TIME,
                                sexual.transmission.increase.maximal.time=BASE.NORMAL.TIME,
                                intervention=intervention)
    else
        do.run.covid.for.simset(simset,
                                run.to=run.to,
                                suppression.start.normalize.time = BASE.START.NORMALIZE.TIME,
                                suppression.normal.time = BASE.NORMAL.TIME,
                                prep.start.normalize.time = BASE.START.NORMALIZE.TIME,
                                prep.normal.time = BASE.NORMAL.TIME,
                                testing.start.normalize.time = BASE.START.NORMALIZE.TIME,
                                testing.normal.time = BASE.NORMAL.TIME,
                                sexual.transmission.start.normalize.time = BASE.START.NORMALIZE.TIME,
                                sexual.transmission.normal.time = BASE.NORMAL.TIME,
                                intervention=intervention)
    
}

do.run.covid.for.simset <- function(simset,
                                    run.to=2030,
                                    suppression.start.normalize.time=NULL,
                                    suppression.normal.time=NULL,
                                    prep.start.normalize.time=NULL,
                                    prep.normal.time=NULL,
                                    testing.start.normalize.time=NULL,
                                    testing.normal.time=NULL,
                                    sexual.transmission.start.normalize.time=NULL,
                                    sexual.transmission.normal.time=NULL,
                                    model.sexual.transmission.increase=F,
                                    sexual.transmission.start.increase.time=NULL,
                                    sexual.transmission.increase.maximal.time=NULL,
                                    intervention=NULL
                                    )
{
    fn = function(sim, parameters){
        run.covid.sim(sim, 
                      parameters,
                      run.to=run.to,
                      suppression.start.normalize.time=suppression.start.normalize.time,
                      suppression.normal.time=suppression.normal.time,
                      prep.start.normalize.time=prep.start.normalize.time,
                      prep.normal.time=prep.normal.time,
                      testing.start.normalize.time=testing.start.normalize.time,
                      testing.normal.time=testing.normal.time,
                      sexual.transmission.start.normalize.time=sexual.transmission.start.normalize.time,
                      sexual.transmission.normal.time=sexual.transmission.normal.time,
                      model.sexual.transmission.increase=model.sexual.transmission.increase,
                      sexual.transmission.start.increase.time=sexual.transmission.start.increase.time,
                      sexual.transmission.increase.maximal.time=sexual.transmission.increase.maximal.time,
                      intervention=intervention)
    }
    
    extend.simulations(simset, fn)
}

#format is <descriptor>.<pct tested>.<start_time_in_mo_from_jan_1_2022>.<duration_in_mo>
#ignores the first descriptor for now
parse.secondary.intervention <- function(str,
                                         allow.foreground.less=F)
{
    args = strsplit(str, '.', fixed=T)[[1]]
    
    proportion.tested = as.numeric(args[2])/100
    start.time = 2022 + as.numeric(args[3])/12
    duration = as.numeric(args[4])/12
    
    testing.rate = -log(1-proportion.tested)/duration
    end.time = start.time + duration
    
    create.intervention(TPOP.ALL,
                        create.intervention.unit(type='testing',
                                                 start.year=start.time,
                                                 end.year=end.time+1/365,
                                                 rates=c(testing.rate, testing.rate),
                                                 years = c(start.time + 1/365, end.time),
                                                 allow.less.than.otherwise = allow.foreground.less))
}

secondary.intervention.name <- function(str)
{
    args = strsplit(str, '.', fixed=T)[[1]]
    
    proportion.tested = as.numeric(args[2])/100
    start.mo = as.numeric(args[3])
    duration.mo = as.numeric(args[4])
    end.mo = start.mo + duration.mo
    
    start.year = 2022 + floor((start.mo-1)/12)
    end.year = 2022 + floor((end.mo-1)/12)
    
    start.mo = 1 + (start.mo - 1) %% 12
    end.mo = 1 + (end.mo - 1) %% 12
    
    MONTHS = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
    if (start.year==end.year)
        rv = paste0(MONTHS[start.mo], "-", MONTHS[end.mo], ", ", start.year)
    else
        rv = paste0(MONTHS[start.mo], ", ", start.year, " - ", MONTHS[end.mo], ", ", end.year)
    
    rv = paste0("Test ", proportion.tested * 100, "% from ", rv)
    rv
}
