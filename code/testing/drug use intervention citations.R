
# Needle exchange
# 
# WHO summary
#   https://www.who.int/hiv/pub/prev_care/effectivenesssterileneedle.pdf
# Specific refs
# - https://journals.lww.com/jaids/Abstract/2000/09010/Prevention_of_HIV_Infection_in_Street_Recruited.9.aspx
# - https://pubmed.ncbi.nlm.nih.gov/11064506/
# - 
# 
# CDC Summary
# https://www.cdc.gov/ssp/syringe-services-programs-faq.html
# they link to a review, which cites other articles
# https://www.tandfonline.com/doi/full/10.1080/10826080600669579

# MOUD
# 
# Lit Review
# - https://onlinelibrary.wiley.com/doi/full/10.1111/ajad.13051#ajad13051-bib-0015


##--------------------------------------##
##-- PROPORTION of IDU who use HEROIN --##
##--------------------------------------##

numerator.and.denominator = ALL.DATA.MANAGERS$idu$heroin.vs.idu.by.urbanization[c('within_30d'),'large_metro',]
#numerator.and.denominator = colSums(ALL.DATA.MANAGERS$idu$heroin.vs.idu.by.urbanization[c('within_30d','30d_to_1yr'),'large_metro',])
numerator.and.denominator['within_1yr'] / sum(numerator.and.denominator)


##--------------------------------------##
##-- NEEDLE EXCHANGE - HIV Rate Ratio --##
##--------------------------------------##


needle.exchange.rate.ratio = c(
    des.jarlais = 1/3.35,
    monterroso = 0.57, #because there were 19 out of 1914 incident infections
    bruneau = 1.7 # HR3 from table 3
)

needle.exchange.rate.ratio.ci.lower = c(
    des.jarlais = 1/8.65,
    monterroso = 0.24,
    bruneau = 1.0
)

needle.exchange.rate.ratio.ci.upper = c(
    des.jarlais = 1/1.29,
    monterroso = 1.4,
    bruneau = 2.7
)

needle.exchange.rate.ratio.log.sd = (log(needle.exchange.rate.ratio.ci.upper)-log(needle.exchange.rate.ratio.ci.lower)) / 2 / 1.96

needle.exchange.n = c(
    des.jarlais = 601,
    monterroso = 1914,
    bruneau = 974
)

inv.var.weighted.needle.exchange.rr = exp(sum(log(needle.exchange.rate.ratio) / needle.exchange.rate.ratio.log.sd^2) / sum(1/needle.exchange.rate.ratio.log.sd^2))
n.weighted.needle.exchange.rr = exp(sum(log(needle.exchange.rate.ratio) * needle.exchange.n) / sum(needle.exchange.n))
#n.weighted.needle.exchange.rr = sum(needle.exchange.rate.ratio * needle.exchange.n) / sum(needle.exchange.n)

##---------------------------------##
##-- NEEDLE EXCHANGE - cessation --##
##---------------------------------##

# Hagan - 11027894
# Strathdee - 10609594
# Huo - 17034440

needle.exchange.cessation.rr = c(
    hagan = 2.5, #the relative risk
    strathdee = 1.38, #the seronegative, adjusted OR
    huo = 0.98 #the adjusted OR
)

needle.exchange.cessation.n = c(
    hagan = 219 + 232,
    strathdee = 1056,
    huo = 901
)

n.weighted.needle.exchange.cessation.rr = exp(sum(log(needle.exchange.cessation.rr) * needle.exchange.cessation.n) / sum(needle.exchange.cessation.n))

##----------##
##-- MOUD --##
##----------##

moud.rate.ratios = c(
    lee = 5/10.5, # under exponential distribution, rate = ln(2)/median --> r1/r2 = median2/median1
    krupitsky = log(.36) / log(.23), # from the total abstinence numbers - 23 vs 36%, assuming exponential distribution
    rosenthal = log(.964) / log(.867),
    fiellin = 6.7/mean(c(5.5, 5.7)), # The mean of the two bup arms. From exponential rate = 1/mean --> r1/r2 = mean2/mean1
    haight = log(mean(c(0.29,0.29))) / log(0.02), # The >=80% abstinent outcomes
    kunoe = log(11/29) / log(5/27),
    woody = mean(c(log(1-.26)/log(1-.61), log(1-.23) / log(1-.54), log(1-.43) / log(1-.51))),
    wang = 0.38 #the cox HR
)

moud.n = c(
    lee = 153,
    krupitsky = 250,
    rosenthal = 177,
    fiellin = 166,
    haight = 489,
    kunoe = 29+27,
    woody = 152,
    wang = 260
)

n.weighted.moud.rr = exp(sum(log(moud.rate.ratios) * moud.n) / sum(moud.n))
