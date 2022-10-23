
#-- LOAD UP STUFF --#
source('code/source_code.R')
SRC.DIRECTORY = file.path(SIMULATIONS.DIR, 'baseline_quick_expanded')
DST.DIRECTORY = file.path(SIMULATIONS.DIR, 'baseline_quick_laart')

source('code/processing/visualization/sim_plots.R')
source('code/settings_and_files/setup_versions.R')
source('code/applications/laart/laart_jheem_settings.R')
source('code/applications/laart/laart_parameters.R')
source('code/applications/laart/laart_parameter_mapping.R')
#source('code/applications/laart/laart_interventions.R')
load('baltimore.RData')
#load('simulations/laart_test/12580.Rdata')

src.filename = get.full.filename(location=MIAMI.MSA, version='ex1.0')
load(file.path(SRC.DIRECTORY, src.filename))

LOCATIONS = c(MIAMI.MSA)
prepared = prepare.simset.for.intervention(simset = simset, update.version = 'laart')
optim.set = prepare.simset.for.intervention(subset.simset(simset, simset@n.sim-9:0), update.version = 'laart')


#-- MAKE FUNCTIONS TO SUMMARIZE RESULTS --#

get.proportions.durable <- function(sim, years=2035)
{
    pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
    
    of.interest = pop[c('engaged_durably_suppressed','laart_durably_suppressed','resistant_durably_suppressed')]
    of.interest / sum(of.interest)
}

get.proportions.unsuppressed <- function(sim, years=2035)
{
    pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
    
    of.interest = pop[c('engaged_unsuppressed_failing','laart_unsuppressed','resistant_unsuppressed')]
    of.interest / sum(of.interest)
}

get.proportions.engaged <- function(sim, years=2035)
{
    pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
    
    of.interest = pop[get.settings.for.version('laart')$ENGAGED_STATES]
    of.interest / sum(of.interest)
}

get.proportions.engaged.by.laart <- function(sim, years=2035)
{
    pop = do.extract.population.subset(sim, years=years, keep.dimensions=c('continuum'), include.hiv.negative = F)
    
    engaged.states = get.settings.for.version('laart')$ENGAGED_STATES
    laart.states = c('laart_unsuppressed','laart_recently_suppressed', 'laart_durably_suppressed')
    resistant.states = c('resistant_unsuppressed','resistant_recently_suppressed', 'resistant_durably_suppressed')
    oral.states = setdiff(engaged.states, c(laart.states, resistant.states))
    
    c(oral=sum(pop[oral.states]), laart=sum(pop[laart.states]), resistant=sum(pop[resistant.states])) / sum(pop[engaged.states])
}


###OPTIMIZATION FUNCTION###
min.loss <-function(param, percentage, start.year, implemented.year){
  start.year = start.year
  implemented.year = implemented.year
  switch.coefficient = param[1]
  discontinuation.coefficient = param[2]
  rate = -log(1-percentage)/(implemented.year-start.year)
  rate = rate * switch.coefficient
  u.DURABLE.LAART = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                             start.year = start.year,
                                             years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                             rates = expression(c(rate, rate, discontinuation.coefficient* (percentage / (1-percentage)) *laart.discontinuation)),
                                             scale = 'rate',
                                             apply.function = 'absolute',
                                             expression.parameters = list(rate=rate, discontinuation.coefficient = discontinuation.coefficient, percentage = percentage))
  intervention.to.test = create.intervention(WHOLE.POPULATION, 
                                             u.DURABLE.LAART)
  source('code/core_code/interventions/intervention_defaults.R')
  
  INTERVENTION.MANAGER.1.0 = register.intervention(intervention.to.test, code=paste0('test.intervention'),
                                                   name='test',
                                                   manager = INTERVENTION.MANAGER.1.0, allow.intervention.multiple.names = T)
  
  #-- RUN AND EXPLORE THE INTERVENTION --#
  projected = run.simset.intervention(optim.set, 
                                      intervention=intervention.to.test,
                                      run.to.year=2035,
                                      keep.years=2015:2035)
  
  columns = c("implemented_year_laart_percentage","2035_laart_percentage") 
  df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  colnames(df) = columns
  for (i in 1:projected@n.sim){
    df[nrow(df) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
      rowMeans(sapply(projected@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
    })),1)[toString(implemented.year-1), 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
      rowMeans(sapply(projected@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
    })),1)['2035', 'laart'])
  }
  
  loss = sum((df-(percentage*100))^2)/projected@n.sim
  return(loss)
} 


#-- DEFINE A TEST INTERVENTION --#
start.year = 2025
implemented.year = 2028
# optim.output<- optim(par = c(1.73211, 3.875102), fn = min.loss, percentage = 0.1, start.year = 2025, implemented.year = 2028, control = list(maxit = 50))
# switch.coefficient.10 = optim.output$par[1]
# discontinuation.coefficient.10 = optim.output$par[2]
# rate.10 = -log(1-0.1)/(implemented.year-start.year)
# rate.10 = rate.10 * switch.coefficient.10
# u.DURABLE.LAART.10 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
#                                               start.year = start.year,
#                                               years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
#                                               rates = expression(c(rate.10, rate.10, (0.1 / (1-.1)) * laart.discontinuation * discontinuation.coefficient.10)),
#                                               scale = 'rate',
#                                               apply.function = 'absolute',
#                                               expression.parameters = list(rate.10=rate.10, discontinuation.coefficient.10 = discontinuation.coefficient.10))
optim.output<- optim(par = c(1.829965, 4.273775), fn = min.loss, percentage = 0.25, start.year = 2025, implemented.year = 2028, control = list(maxit = 50))
switch.coefficient.25 = optim.output$par[1]
discontinuation.coefficient.25 = optim.output$par[2]
rate.25 = -log(1-0.25)/(implemented.year-start.year)
rate.25 = rate.25 * switch.coefficient.25
u.DURABLE.LAART.25 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                              start.year = start.year,
                                              years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                              rates = expression(c(rate.25, rate.25, (0.25 / (1-.25)) * laart.discontinuation * discontinuation.coefficient.25)),
                                              scale = 'rate',
                                              apply.function = 'absolute',
                                              expression.parameters = list(rate.25=rate.25, discontinuation.coefficient.25 = discontinuation.coefficient.25))
optim.output<- optim(par = c(2.27722, 5.069739), fn = min.loss, percentage = 0.50, start.year = 2025, implemented.year = 2028, control = list(maxit = 50))
switch.coefficient.50 = optim.output$par[1]
discontinuation.coefficient.50 = optim.output$par[2]
rate.50 = -log(1-0.50)/(implemented.year-start.year)
rate.50 = rate.50 * switch.coefficient.50
u.DURABLE.LAART.50 = create.intervention.unit(type = 'engaged.durably.suppressed.switch.to.laart',
                                              start.year = start.year,
                                              years = c(start.year+0.0001, implemented.year, implemented.year+0.001),#engaged.durably.suppressed.switch.to.laart
                                              rates = expression(c(rate.50, rate.50, (0.50 / (1-.50)) * laart.discontinuation * discontinuation.coefficient.50)),
                                              scale = 'rate',
                                              apply.function = 'absolute',
                                              expression.parameters = list(rate.50=rate.50, discontinuation.coefficient.50 = discontinuation.coefficient.50))

# rate.10 = -log(1-0.1)
# u.UNSUPPRESSED.LAART.10 = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
#                                                    start.year = start.year,
#                                                    years = c(implemented.year),
#                                                    rates = expression(c(rate.10)),
#                                                    scale = 'rate',
#                                                    apply.function = 'absolute',
#                                                    expression.parameters = list(rate.10=rate.10)) 
rate.25 = -log(1-0.25)
u.UNSUPPRESSED.LAART.25 = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                   start.year = start.year,
                                                   years = c(implemented.year),
                                                   rates = expression(c(rate.25)),
                                                   scale = 'rate',
                                                   apply.function = 'absolute',
                                                   expression.parameters = list(rate.25=rate.25))
rate.50 = -log(1-0.50)
u.UNSUPPRESSED.LAART.50 = create.intervention.unit(type = 'engaged.unsuppressed.switch.to.laart',
                                                   start.year = start.year,
                                                   years = c(implemented.year),
                                                   rates = expression(c(rate.50)),
                                                   scale = 'rate',
                                                   apply.function = 'absolute',
                                                   expression.parameters = list(rate.50=rate.50))


rate.25 = -log(1-0.25)
u.NAIVE.LAART.25 = create.intervention.unit(type = 'engaged.naive.switch.to.laart',
                                            start.year = start.year,
                                            years = c(implemented.year),
                                            rates = expression(c(rate.25)),
                                            scale = 'rate',
                                            apply.function = 'absolute',
                                            expression.parameters = list(rate.25=rate.25))

rate.50 = -log(1-0.50)
u.NAIVE.LAART.50 = create.intervention.unit(type = 'engaged.naive.switch.to.laart',
                                                   start.year = start.year,
                                                   years = c(implemented.year),
                                                   rates = expression(c(rate.50)),
                                                   scale = 'rate',
                                                   apply.function = 'absolute',
                                                   expression.parameters = list(rate.50=rate.50))


#-- Create Interventions --#
durable.laart.25 = create.intervention(WHOLE.POPULATION, 
                                       u.DURABLE.LAART.25)

durable.laart.50 = create.intervention(WHOLE.POPULATION, 
                                       u.DURABLE.LAART.50)

unsuppressed.laart.25 = create.intervention(WHOLE.POPULATION, 
                                            u.UNSUPPRESSED.LAART.25)

unsuppressed.laart.50 = create.intervention(WHOLE.POPULATION, 
                                            u.UNSUPPRESSED.LAART.50)

naive.laart.25 = create.intervention(WHOLE.POPULATION, 
                                     u.NAIVE.LAART.25)

naive.laart.50 = create.intervention(WHOLE.POPULATION, 
                                            u.NAIVE.LAART.50)

everything.laart.25 = create.intervention(WHOLE.POPULATION, 
                                          u.NAIVE.LAART.25, u.DURABLE.LAART.25, u.UNSUPPRESSED.LAART.25)

everything.laart.50 = create.intervention(WHOLE.POPULATION, 
                                     u.NAIVE.LAART.50, u.DURABLE.LAART.50, u.UNSUPPRESSED.LAART.50)


INTERVENTION.MANAGER.1.0 = register.intervention(durable.laart.25, code=paste0('durable.laart.25'),
                                             name='25% of durably suppressed start LAART',
                                             manager = INTERVENTION.MANAGER.1.0)

INTERVENTION.MANAGER.1.0 = register.intervention(durable.laart.50, code=paste0('durable.laart.50'),
                                             name='50% of durably suppressed start LAART',
                                             manager = INTERVENTION.MANAGER.1.0)

INTERVENTION.MANAGER.1.0 = register.intervention(unsuppressed.laart.25, code=paste0('unsuppressed.laart.25'),
                                             name='25% of unsuppressed start LAART',
                                             manager = INTERVENTION.MANAGER.1.0)
INTERVENTION.MANAGER.1.0 = register.intervention(unsuppressed.laart.50, code=paste0('unsuppressed.laart.50'),
                                             name='50% of unsuppressed start LAART',
                                             manager = INTERVENTION.MANAGER.1.0)

INTERVENTION.MANAGER.1.0 = register.intervention(naive.laart.25, code=paste0('naive.laart.25'),
                                                 name='25% of unsuppressed naive start LAART',
                                                 manager = INTERVENTION.MANAGER.1.0)

INTERVENTION.MANAGER.1.0 = register.intervention(naive.laart.50, code=paste0('naive.laart.50'),
                                                 name='50% of unsuppressed naive start LAART',
                                                 manager = INTERVENTION.MANAGER.1.0)

INTERVENTION.MANAGER.1.0 = register.intervention(everything.laart.25, code=paste0('everything.laart.25'),
                                                 name='25% of unsuppressed naive/engaged and durable  start LAART',
                                                 manager = INTERVENTION.MANAGER.1.0)

INTERVENTION.MANAGER.1.0 = register.intervention(everything.laart.50, code=paste0('everything.laart.50'),
                                                 name='50% of unsuppressed naive/engaged and durable  start LAART',
                                                 manager = INTERVENTION.MANAGER.1.0)

LAART.INTERVENTION.CODES = c("noint", "durable.laart.25", "durable.laart.50", "unsuppressed.laart.25", "unsuppressed.laart.50", "naive.laart.25", "naive.laart.50", "everything.laart.25", "everything.laart.50")
LAART.INTERVENTIONS = lapply(LAART.INTERVENTION.CODES, intervention.from.code)

#-- RUN AND EXPLORE THE INTERVENTION --#
noint = run.simset.intervention(prepared, 
                                    intervention=NO.INTERVENTION,
                                    run.to.year=2035,
                                    keep.years=2015:2035)

print(round(100*t(sapply(as.character(2024:2035), function(year){rowMeans(sapply(noint@simulations, get.proportions.engaged.by.laart, years=as.numeric(year)))}))))
columns = c("2027_laart_percentage","2035_laart_percentage") 
df_no = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_no) = columns
for (i in 1:noint@n.sim){
  df_no[nrow(df_no) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(noint@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2027', 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(noint@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2035', 'laart'])
}
hist(df_no$`2027_laart_percentage`)
hist(df_no$`2035_laart_percentage`)

int.durable.25 = run.simset.intervention(prepared, 
                                         intervention=durable.laart.25,
                                         run.to.year=2035,
                                         keep.years=2015:2035)

print(round(100*t(sapply(as.character(2024:2035), function(year){rowMeans(sapply(int.durable.25@simulations, get.proportions.engaged.by.laart, years=as.numeric(year)))}))))
columns = c("2027_laart_percentage","2035_laart_percentage") 
df_durable_25 = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_durable_25) = columns
for (i in 1:int.durable.25@n.sim){
  df_durable_25[nrow(df_durable_25) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.durable.25@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2027', 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.durable.25@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2035', 'laart'])
}
hist(df_durable_25$`2027_laart_percentage`)
hist(df_durable_25$`2035_laart_percentage`)

int.durable.50 = run.simset.intervention(prepared, 
                                         intervention=durable.laart.50,
                                         run.to.year=2035,
                                         keep.years=2015:2035)
int.durable.50 = allsimsets[[3]]

print(round(100*t(sapply(as.character(2024:2035), function(year){rowMeans(sapply(int.durable.50@simulations, get.proportions.engaged.by.laart, years=as.numeric(year)))}))))
columns = c("2027_laart_percentage","2035_laart_percentage") 
df_durable_50 = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_durable_50) = columns
for (i in 1:int.durable.50@n.sim){
  df_durable_50[nrow(df_durable_50) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.durable.50@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2027', 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.durable.50@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2035', 'laart'])
}
hist(df_durable_50$`2027_laart_percentage`)
hist(df_durable_50$`2035_laart_percentage`)

int.unsuppressed.25 = run.simset.intervention(prepared, 
                                              intervention=unsuppressed.laart.25,
                                              run.to.year=2035,
                                              keep.years=2015:2035)

print(round(100*t(sapply(as.character(2024:2035), function(year){rowMeans(sapply(int.unsuppressed.25@simulations, get.proportions.engaged.by.laart, years=as.numeric(year)))}))))
columns = c("2027_laart_percentage","2035_laart_percentage") 
df_unsuppressed_25 = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_unsuppressed_25) = columns
for (i in 1:int.unsuppressed.25@n.sim){
  df_unsuppressed_25[nrow(df_unsuppressed_25) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.unsuppressed.25@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2027', 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.unsuppressed.25@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2035', 'laart'])
}
hist(df_unsuppressed_25$`2027_laart_percentage`)
hist(df_unsuppressed_25$`2035_laart_percentage`)


int.unsuppressed.50 = run.simset.intervention(prepared, 
                                              intervention=unsuppressed.laart.50,
                                              run.to.year=2035,
                                              keep.years=2015:2035)

print(round(100*t(sapply(as.character(2024:2035), function(year){rowMeans(sapply(int.unsuppressed.50@simulations, get.proportions.engaged.by.laart, years=as.numeric(year)))}))))
columns = c("2027_laart_percentage","2035_laart_percentage") 
df_unsuppressed_50 = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_unsuppressed_50) = columns
for (i in 1:int.unsuppressed.50@n.sim){
  df_unsuppressed_50[nrow(df_unsuppressed_50) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.unsuppressed.50@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2027', 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.unsuppressed.50@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2035', 'laart'])
}
hist(df_unsuppressed_50$`2027_laart_percentage`)
hist(df_unsuppressed_50$`2035_laart_percentage`)

int.naive.25 = run.simset.intervention(prepared, 
                                       intervention=naive.laart.25,
                                       run.to.year=2035,
                                       keep.years=2015:2035)
print(round(100*t(sapply(as.character(2024:2035), function(year){rowMeans(sapply(int.naive.25@simulations, get.proportions.engaged.by.laart, years=as.numeric(year)))}))))
columns = c("2027_laart_percentage","2035_laart_percentage") 
df_naive_25 = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_naive_25) = columns
for (i in 1:int.naive.25@n.sim){
  df_naive_25[nrow(df_naive_25) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.naive.25@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2027', 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.naive.25@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2035', 'laart'])
}
hist(df_naive_25$`2027_laart_percentage`)
hist(df_naive_25$`2035_laart_percentage`)

int.naive.50 = run.simset.intervention(prepared, 
                                               intervention=naive.laart.50,
                                               run.to.year=2035,
                                               keep.years=2015:2035)
print(round(100*t(sapply(as.character(2024:2035), function(year){rowMeans(sapply(int.naive.50@simulations, get.proportions.engaged.by.laart, years=as.numeric(year)))}))))
columns = c("2027_laart_percentage","2035_laart_percentage") 
df_naive_50 = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_naive_50) = columns
for (i in 1:int.naive.50@n.sim){
  df_naive_50[nrow(df_naive_50) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.naive.50@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2027', 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.naive.50@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2035', 'laart'])
}
hist(df_naive_50$`2027_laart_percentage`)
hist(df_naive_50$`2035_laart_percentage`)

int.everything.25 = run.simset.intervention(prepared, 
                                            intervention=everything.laart.25,
                                            run.to.year=2035,
                                            keep.years=2015:2035)
print(round(100*t(sapply(as.character(2024:2035), function(year){rowMeans(sapply(int.everything.25@simulations, get.proportions.engaged.by.laart, years=as.numeric(year)))}))))
columns = c("2027_laart_percentage","2035_laart_percentage") 
df_everything_25 = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_everything_25) = columns
for (i in 1:int.everything.25@n.sim){
  df_everything_25[nrow(df_everything_25) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.everything.25@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2027', 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.everything.25@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2035', 'laart'])
}
hist(df_everything_25$`2027_laart_percentage`)
hist(df_everything_25$`2035_laart_percentage`)

int.everything.50 = run.simset.intervention(prepared, 
                                       intervention=everything.laart.50,
                                       run.to.year=2035,
                                       keep.years=2015:2035)
print(round(100*t(sapply(as.character(2024:2035), function(year){rowMeans(sapply(int.everything.50@simulations, get.proportions.engaged.by.laart, years=as.numeric(year)))}))))
columns = c("2027_laart_percentage","2035_laart_percentage") 
df_everything_50 = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df_everything_50) = columns
for (i in 1:int.everything.50@n.sim){
  df_everything_50[nrow(df_everything_50) + 1,] = c(round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.everything.50@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2027', 'laart'], round(100*t(sapply(as.character(2024:2035), function(year){
    rowMeans(sapply(int.everything.50@simulations[i], get.proportions.engaged.by.laart, years=as.numeric(year)))
  })),1)['2035', 'laart'])
}
hist(df_everything_50$`2027_laart_percentage`)
hist(df_everything_50$`2035_laart_percentage`)



###EXTRACT RESULTS###
allsimsets = list(noint,int.durable.25, int.durable.50, int.unsuppressed.25, int.unsuppressed.50, int.naive.25, int.naive.50, int.everything.25, int.everything.50)
values = sapply(allsimsets, function(simset){
  sapply(simset@simulations, function(sim){
    inc = project.absolute.incidence(sim, years = c(2025, 2035))
    (inc[1]-inc[2])/inc[1]
  })
})
print(apply(values, 2, mean))
print(apply(values, 2, quantile, probs=c(0.025, 0.975)))

print(apply(values-values[ ,1], 2, mean))
print(apply(values-values[ ,1], 2, quantile, probs=c(0.025, 0.975)))

save(allsimsets, file = "miami.RData")


