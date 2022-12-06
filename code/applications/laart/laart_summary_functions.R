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
