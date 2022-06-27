

LAART.PARAMETER.DISTRIBUTION = join.distributions(
    
    or.disengagement.laart.vs.oral,
    
    p.resistance.up.front.laart.recently.suppressed,
    p.resistance.up.front.laart.durably.suppressed = Beta.Distribution(alpha=0.5 + 3, beta=0.5+279),
    p.resistance.up.front.laart.unsuppressed,
    
    rr.resistance.ongoing.vs.upfront.laart,
    rr.resistance.disengage.vs.upfront.laart,
    
    or.gain.suppression.resistance.vs.oral,
    or.lose.suppression.resistance.vs.oral.recently.suppressed,
    or.lose.suppression.resistance.vs.oral.durably.suppressed,
    
    or.disengagement.resistance.vs.oral,
)


hr.of.viral.rebound

est.hr = 4.01
ci.hr = c(1.16, 13.77)

log.est.hr = log(est.hr)
log.ci.hr = log(ci.hr)

log.mu = log.est.hr
log.sigma = (log.ci.hr[2] - log.ci.hr[1]) / 2 / 1.96

rands = rlnorm(100000, log.mu, log.sigma)
mean(rands)
median(rands)
quantile(rands, c(.025, .975))


rands = rbeta(10000, 0.5+0, 0.5+23)
mean(rands)
round(quantile(rands, c(.025,.975)),3)
qplot(rands)
