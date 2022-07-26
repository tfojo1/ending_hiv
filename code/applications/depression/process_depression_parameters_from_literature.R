

##-- RR suppression, dep vs none --##
cis.suppression.dep.vs.no = rbind(
    c(0.89, 0.96), # https://link.springer.com/article/10.1007/s10461-019-02613-6
    1/c(1.43,1.06), # https://jamanetwork.com/journals/jamapsychiatry/fullarticle/2672209
    1/c(1.53,1.07) # https://academic.oup.com/cid/article/68/3/475/5036558
)

log.means.suppression.dep.vs.no = rowMeans(log(cis.suppression.dep.vs.no))
log.sds.suppression.dep.vs.no = (log(cis.suppression.dep.vs.no[,2]) - log(cis.suppression.dep.vs.no[,1])) / 2 / qnorm(0.975)

final.log.mean.suppression.dep.vs.no = mean(log.means.suppression.dep.vs.no)
final.log.sd.suppression.dep.vs.no = mean(log.sds.suppression.dep.vs.no)

# summarize the resulting pooled distribution
print(paste0("RR of suppression (depressed vs not): ",
             round(exp(final.log.mean.suppression.dep.vs.no),2),
             " [",
             round(exp(final.log.mean.suppression.dep.vs.no + qnorm(0.025)*final.log.sd.suppression.dep.vs.no),2),
             " - ",
             round(exp(final.log.mean.suppression.dep.vs.no + qnorm(0.975)*final.log.sd.suppression.dep.vs.no),2),
             "]"))

Lognormal.Distribution(meanlog=final.log.mean.suppression.dep.vs.no, sdlog=final.log.sd.suppression.dep.vs.no)

##-- RR Suppression, treated vs untreated --##

cis.suppression.treated.vs.no = rbind(
    c(1.03, 1.06), # https://link.springer.com/article/10.1007/s10461-019-02613-6
    c(1.12,1.20) # https://diginole.lib.fsu.edu/islandora/object/fsu:642089/datastream/PDF/download/citation.pdf
)

log.means.suppression.treated.vs.no = rowMeans(log(cis.suppression.treated.vs.no))
log.sds.suppression.treated.vs.no = (log(cis.suppression.treated.vs.no[,2]) - log(cis.suppression.treated.vs.no[,1])) / 2 / qnorm(0.975)

final.log.mean.suppression.treated.vs.no = mean(log.means.suppression.treated.vs.no)
final.log.sd.suppression.treated.vs.no = mean(log.sds.suppression.treated.vs.no)

# summarize the resulting pooled distribution
print(paste0("RR of suppression (treated vs untreated depression): ",
             round(exp(final.log.mean.suppression.treated.vs.no),2),
             " [",
             round(exp(final.log.mean.suppression.treated.vs.no + qnorm(0.025)*final.log.sd.suppression.treated.vs.no),2),
             " - ",
             round(exp(final.log.mean.suppression.treated.vs.no + qnorm(0.975)*final.log.sd.suppression.treated.vs.no),2),
             "]"))


##-- Rate of Discontinuing Treatment --##
overall.proportion.discontinued = 84/147
treatment.discontinuation.rate = -log(1-overall.proportion.discontinued)

ci.proportion.discontinued = qbeta(c(0.025, 0.975), 0.5+84, 0.5 + 147-84)
ci.treatment.discontinuation.rate = -log(1-ci.proportion.discontinued)
log.mean.proportion.discontinued = mean(log(ci.treatment.discontinuation.rate))
log.sd.proportion.discontinued = (log(ci.treatment.discontinuation.rate[2])-log(ci.treatment.discontinuation.rate[1])) / 2 / qnorm(0.975)

dist1 = Lognormal.Distribution(log.mean.proportion.discontinued, log.sd.proportion.discontinued)
dist2 = Lognormal.Distribution(log(treatment.discontinuation.rate), log(4)/2)
