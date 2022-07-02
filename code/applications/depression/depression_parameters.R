dist = join.distributions(
    suppression.untreated.vs.no.depression.or = Lognormal.Distribution(-.0834, .0193),
    suppression.treated.vs.untreated.depression.or = Lognormal.Distribution(0, 1),
    
    lost.untreated.vs.no.depression.or = Lognormal.Distribution(0, 1),
    lost.treated.vs.untreated.depression.or = Lognormal.Distribution(0, 1),
    
    depression.incidence.rr = Lognormal.Distribution(0, log(2)/2),
    depression.remission.rr = Lognormal.Distribution(0, log(2)/2),
    
    depression.rr.age1 = Lognormal.Distribution(0, log(2)/2),
    #    depression.rr.age2 = Lognormal.Distribution(0, log(2)/2),
    #    depression.rr.age4 = Lognormal.Distribution(0, log(2)/2),
    #    depression.rr.age5 = Lognormal.Distribution(0, log(2)/2),
    
    depression.incidence.hiv.vs.uninfected.rr = Lognormal.Distribution(0, log(2)/2, lower = 1),
    depression.remission.hiv.vs.uninfected.rr = Lognormal.Distribution(0, log(2)/2, upper = 1),
    
    depression.treatment.initiation.rr = Lognormal.Distribution(0, log(2)/2),
    depression.treatment.discontinuation.rr = Lognormal.Distribution(0, log(2)/2),
    
    depression.treatment.initiation.hiv.vs.uninfected.rr = Lognormal.Distribution(0, log(2)/2),
    depression.treatment.discontinuation.hiv.vs.uninfected.rr = Lognormal.Distribution(0, log(2)/2),
    
    sexual.susceptibility.untreated.vs.no.depression.rr = Lognormal.Distribution(0, 1),
    sexual.susceptibility.treated.vs.untreated.depression.rr = Lognormal.Distribution(0, 1),
    
    idu.susceptibility.untreated.vs.no.depression.rr = Lognormal.Distribution(0, 1),
    idu.susceptibility.treated.vs.untreated.depression.rr = Lognormal.Distribution(0, 1),
    
    testing.untreated.vs.no.depression.or = Lognormal.Distribution(0, 1),
    testing.treated.vs.untreated.depression.or = Lognormal.Distribution(0, 1),
    
    idu.incidence.untreated.vs.no.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.remission.untreated.vs.no.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.relapse.untreated.vs.no.depression.rr= Lognormal.Distribution(0, log(2)/2),
    
    idu.incidence.treated.vs.untreated.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.remission.treated.vs.untreated.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.relapse.treated.vs.untreated.depression.rr= Lognormal.Distribution(0, log(2)/2)
)