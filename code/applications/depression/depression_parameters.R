dist = join.distributions(
    suppression.untreated.vs.no.depression.or = Lognormal.Distribution(.92, .73),
    suppression.treated.vs.untreated.depression.or = Lognormal.Distribution(1.04, .0077),
    
    lost.untreated.vs.no.depression.or = Lognormal.Distribution(1.13, .32),
    lost.treated.vs.untreated.depression.or = Lognormal.Distribution(.685, .025),
    
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
    
    sexual.susceptibility.untreated.vs.no.depression.male.or = Lognormal.Distribution(.88, .107),
    sexual.susceptibility.untreated.vs.no.depression.female.or = Lognormal.Distribution(.76, .077),
    sexual.susceptibility.treated.vs.untreated.depression.or = Lognormal.Distribution(0, 1), #Could not find
    
    idu.susceptibility.untreated.vs.no.depression.male.rr = Lognormal.Distribution(.88, .107),
    idu.susceptibility.untreated.vs.no.depression.female.rr = Lognormal.Distribution(.76, .077),
    idu.susceptibility.treated.vs.untreated.depression.rr = Lognormal.Distribution(0, 1), #Could not find
    
    testing.untreated.vs.no.depression.or = Lognormal.Distribution(1.34, .1),
    testing.treated.vs.untreated.depression.or = Lognormal.Distribution(0, 1), #Could not find
    
    idu.incidence.untreated.vs.no.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.remission.untreated.vs.no.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.relapse.untreated.vs.no.depression.rr= Lognormal.Distribution(0, log(2)/2),
    
    idu.incidence.treated.vs.untreated.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.remission.treated.vs.untreated.depression.rr= Lognormal.Distribution(0, log(2)/2),
    idu.relapse.treated.vs.untreated.depression.rr= Lognormal.Distribution(0, log(2)/2)
)