
#-- TEST ZERO: Can we source? --#

source('code/source_code.R')

#-- FIRST TEST: Run previously established components both ways, collapsed_1.0 --#

load('../jheem_interactive/shiny/sim_cache/1.0_12060_baseline.Rdata')
comps = attr(simset@simulations[[1]], 'components')
params = simset@parameters[1,]

comps = clear.dependent.values(comps, dependent.on = c('testing'))

NEW.WAY = F
c.old = do.setup.continuum.transitions(comps)

NEW.WAY = T
c.new = do.setup.continuum.transitions(comps)


length(c.old$continuum.transition.years) == length(c.new$continuum.transition.years) &&
    all(c.old$continuum.transition.years==c.new$continuum.transition.years) &&
    all(sapply(1:length(c.old$continuum.transition.years), function(i){
        length(c.old$continuum.transitions[[i]]) == length(c.new$continuum.transitions[[i]]) &&
            all(abs(c.old$continuum.transitions[[i]]-c.new$continuum.transitions[[i]])<.00001)
    }))

if (1==2)
{
    range(unlist(sapply(1:length(c.old$continuum.transition.years), function(i){
        c.old$continuum.transitions[[i]]-c.new$continuum.transitions[[i]]
    })))
    
    # debugging
    sapply(c.new$continuum.transitions, length)
}

#-- SECOND TEST: Can we set up de-novo components, collapsed_1.0 --#

run.simulation = create.run.simulation.function(msa=attr(simset@simulations[[1]], 'location'),
                                                version = 'collapsed_1.0',
                                                start.values = params, catch.errors = F)
sim = run.simulation(params)
NEW.WAY = T
c.de.novo = attr(sim, 'components')
#c.de.novo = unfix.jheem.components(c.de.novo)
#c.de.novo = clear.dependent.values(c.de.novo, dependent.on = c('testing'))
c.de.novo = do.setup.continuum.transitions(c.de.novo)

length(c.old$continuum.transition.years) == length(c.de.novo$continuum.transition.years) &&
    all(c.old$continuum.transition.years==c.de.novo$continuum.transition.years) &&
    all(sapply(1:length(c.old$continuum.transition.years), function(i){
        length(c.old$continuum.transitions[[i]]) == length(c.de.novo$continuum.transitions[[i]]) &&
            all(abs(c.old$continuum.transitions[[i]]-c.de.novo$continuum.transitions[[i]])<.00001)
    }))


#-- THIRD TEST - expanded_1.0 --#

load('mcmc_runs/laart_test/12580.Rdata')
comps = attr(simset@simulations[[1]], 'components')
params = simset@parameters[1,]

NEW.WAY = F
#c.old = comps
c.old = do.setup.continuum.transitions(unfix.jheem.components(comps))

NEW.WAY = T
run.simulation = create.run.simulation.function(msa=attr(simset@simulations[[1]], 'location'),
                                                version = 'expanded_1.0',
                                                start.values = params, catch.errors = F)
sim = run.simulation(params)
c.de.novo = attr(sim, 'components'); c.de.novo = do.setup.continuum.transitions(c.de.novo)

length(c.old$continuum.transition.years) == length(c.de.novo$continuum.transition.years) &&
    all(c.old$continuum.transition.years==c.de.novo$continuum.transition.years) &&
    all(sapply(1:length(c.old$continuum.transition.years), function(i){
        length(c.old$continuum.transitions[[i]]) == length(c.de.novo$continuum.transitions[[i]]) &&
            all(abs(c.old$continuum.transitions[[i]]-c.de.novo$continuum.transitions[[i]])<.00001)
    }))


if (1==2)
{
    setdiff(c.old$continuum.transition.years, c.de.novo$continuum.transition.years)
    
    round(sapply(1:length(c.old$continuum.transition.years), function(i){
            max(abs(c.old$continuum.transitions[[i]]-c.de.novo$continuum.transitions[[i]]))
    }),5)
    
    failing= !sapply(1:length(c.old$continuum.transition.years), function(i){
        length(c.old$continuum.transitions[[i]]) == length(c.de.novo$continuum.transitions[[i]]) &&
            all(abs(c.old$continuum.transitions[[i]]-c.de.novo$continuum.transitions[[i]])<.00001)
    });c.de.novo$continuum.transition.years[failing]
    
    year = 2012
    index = (1:length(c.old$continuum.transition.years))[c.old$continuum.transition.years==year]
    
    t.old = c.old$continuum.transitions[[index]]
    t.new = c.de.novo$continuum.transitions[[index]]
    
    apply(t.new!=t.old, c('continuum.from','continuum.to'), mean)
    
    (t.new-t.old)[1,1,1,1,1,,1,1,]
    
    
    all(c.old$naive.to.suppressed.rates.and.times$times==c.de.novo$naive.to.suppressed.rates.and.times$times)
    
    year = 2012
    index2 = (1:length(c.old$naive.to.suppressed.rates.and.times$times))[c.old$naive.to.suppressed.rates.and.times$times==year]
    
    r.old = c.old$naive.to.suppressed.rates.and.times$rates[[index2]]
    r.new = c.de.novo$naive.to.suppressed.rates.and.times$rates[[index2]]
    
    range(r.new - r.old)
    range(r.new-convert.transition.element.type(r.old[,,1,,,1,1,1], 'proportion','time'))
    
    calc.de.novo = c.de.novo$naive.to.suppressed.rates.and.times$rates[[9]][,,1,,,1,1,1]/
        (c.de.novo$time.to.suppression.on.art.rates.and.times$rates+c.de.novo$start.art.rates.and.times$rates[[11]])
    start.art.rate = -log(1-c.old$start.art.rates.and.times$rates[[4]])
    calc.old = c.old$naive.to.disengaged.rates.and.times
    
    range(c.de.novo$start.art.rates.and.times$rates[[11]] - 1/start.art.rate[,,1,,,1,1,1])
    
    range()
    
    # check if start.art is the same
    sapply(1:length(c.old$start.art.rates.and.times$times), function(i){
        mean(c.old$start.art.rates.and.times$rates[[i]][,,1,,,1,1,1]==1/c.de.novo$start.art.rates.and.times$rates[[i]])
    })
    
    x=sapply(1:length(c.old$start.art.rates.and.times$times), function(i){
        c(old=c.old$start.art.rates.and.times$rates[[i]][1],
          de.novo=c.de.novo$start.art.rates.and.times$rates[[i]][1])
    });x
    
    -1/log(1-x[1,]) == x[2,]
}
