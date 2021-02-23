
source('code/source_code.R')
source('code/process_results/make_systematic_table.R')
source('code/targets/target_msas.R')
source('code/interventions/melissa_croi_interventions.R')

#### New set of interventions - 11/19/20 ####

location = LA.MSA
if (1==2)
{
  location = LA.MSA
  load(file.path('mcmc_runs/quick_simsets', location, get.full.filename(location)))
  
  run.systematic.interventions(simset,
                               dst.dir = 'mcmc_runs/quick_simsets',
                               interventions = MELISSA.INTERVENTIONS.2,
                               save.baseline.and.seed = F)
  
}

if (1==2)
{
  
  location = LA.MSA

  intervention.code.table = matrix(c('noint','ymsm.p10', 'ymsm.p25', 'ymsm.p50', 'ymsm.s80', 'ymsm.s85', 'ymsm.s90',
                                    'msm.p10', 'msm.p25', 'msm.p50', 'msm.s80', 'msm.s85', 'msm.s90',
                                    'ymsm.p25.msm.s80', 'ymsm.p25.msm.s85', 'ymsm.p50.msm.s90'),
                                    ncol=16)
  
  tab = get.estimates.for.interventions(intervention.codes=intervention.code.table,
                                        location=location,
                                        dir='mcmc_runs/quick_simsets')
  
  ## Todd add credible interval code here ## 
  lower = get.quantiles.for.interventions(intervention.codes=intervention.code.table,
                                          location=location,
                                          prob=0.025,
                                          dir='mcmc_runs/quick_simsets')
  upper = get.quantiles.for.interventions(intervention.codes=intervention.code.table,
                                          location=location,
                                          prob=0.975,
                                          dir='mcmc_runs/quick_simsets')
  
  
  # intervention.code.table = matrix(c('ymsm.p25.msm.s80', 'ymsm.p25.msm.s85', 'ymsm.p50.msm.s90', 'noint'),
  #                                  ncol=4)
  # intervention.code.table = matrix(c('ymsm.p10', 'ymsm.p25', 'ymsm.p50', 'ymsm.s80', 'ymsm.s85', 'ymsm.s90',
  #                                    'msm.p10', 'msm.p25', 'msm.p50', 'msm.s80', 'msm.s85', 'msm.s90'),
  #                                  ncol=12)
  
}

write.csv(tab, file = "Melissa_CROI_v3.csv")


croi.locations =  c(LA.MSA, NYC.MSA, ATLANTA.MSA, BALTIMORE.MSA)
croi.results = matrix(0,4,16, dimnames = list(croi.locations,intervention.code.table))
croi.lower = matrix(0,4,16, dimnames = list(croi.locations,intervention.code.table))
croi.upper = matrix(0,4,16, dimnames = list(croi.locations,intervention.code.table))
for (i in croi.locations) {
  croi.results[i,] = get.estimates.for.interventions(intervention.codes=intervention.code.table,
                                    location=i,
                                    dir='mcmc_runs/quick_simsets')
  croi.lower[i,] = get.quantiles.for.interventions(intervention.codes=intervention.code.table,
                                          location=i,
                                          prob=0.025,
                                          dir='mcmc_runs/quick_simsets')
  croi.upper[i,] = get.quantiles.for.interventions(intervention.codes=intervention.code.table,
                                          location=i,
                                          prob=0.975,
                                          dir='mcmc_runs/quick_simsets')
  
}


#### croi figures ####

library(RColorBrewer)
library(ggplot2)

cex.legend=1
cex.axis=1
# scenarios=c("no intervention", "young/prep", "all/prep", "young/supp", "all/supp")
par(mfrow = c(1, 1))
display.brewer.all()

#### 1: PrEP v. Suppression - Within MSA ####
cols <- c(brewer.pal(9,"Reds")[2], rep(brewer.pal(9,"Greens")[3:5],2), rep(brewer.pal(9, "Blues")[3:5],2))
int <-barplot(100*croi.results[1,c(1:4,8:10,5:7,11:13)], col = cols, 
                      axes=F, axisnames = F,
                      ylim = c(0,100)
              )
ylabs <- seq(0, 100, by = 20)
axis(side = 2, at = ylabs, labels = paste0(ylabs, "%"))
title(ylab="Percent reduction in incidence", line=2.4, cex.lab=1)
xlabs <- c("none",rep(c("young MSM","all MSM"),2))
axis(side=1, at = c(0.7, 3.5, 6.5, 10.5, 14), labels=xlabs, tick = F, cex.axis = 0.75, line = -1)
axis(side=1, at = c(4.5, 12.5), labels=c("PrEP", "Suppression"), tick = F, cex.axis = 1, line = 0.75)
legendcols <- c(brewer.pal(9,"Reds")[2], brewer.pal(9,"Greens")[3:5], brewer.pal(9, "Blues")[3:5])
legend("topright",  legend = c( "no intervention", "10% prep","25% prep","50% prep","80% supp","85% supp","90% supp"), 
       fill = legendcols,bty = "n", cex=.75)


# PrEP v. Suppression - All MSM only 
cols <- c(brewer.pal(9,"Reds")[2], brewer.pal(9,"Greens")[3:5], brewer.pal(9, "Blues")[3:5])
par(mar=c(8, 4.1, 4.1, 2.1))
int <-barplot(100*croi.results[1,c(1,8:10,11:13)], col = cols, 
              axes=F, axisnames = F,
              ylim = c(0,100)
)
# abline(h=90, lty=2, col="Red")
ylabs <- seq(0, 100, by = 20)
axis(side = 2, at = ylabs, labels = paste0(ylabs, "%"))
title(ylab="Percent reduction in incidence", line=2.4, cex.lab=1.5)
xlabs <- c("no intervention", "10% PrEP", "25% PrEP", "50% PrEP", "80% \n Suppression", "85% \n Suppression", "90% \n Suppression")
axis(side=1, at = c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.8), labels=xlabs, tick = F, cex.axis = 1, line = -.5, las = 2)
title(xlab="Intervention Scenario", line=6, cex.lab=1.5)
 title(main="Los Angeles", cex.main=2, line=-1)
# title(main="New York City ", cex.main=2, line=-1)
# title(main="Atlanta", cex.main=2, line=-1)
# title(main="Baltimore", cex.main=2, line=-1)


 # PrEP v. Suppression - Young MSM only 
 cols <- c(brewer.pal(9,"Reds")[2], brewer.pal(9,"Greens")[3:5], brewer.pal(9, "Blues")[3:5])
 par(mar=c(8, 4.1, 4.1, 2.1))
 int <-barplot(100*croi.results[1,c(1:4,5:7)], col = cols, 
               axes=F, axisnames = F,
               ylim = c(0,100)
 )
 # abline(h=90, lty=2, col="Red")
 ylabs <- seq(0, 100, by = 20)
 axis(side = 2, at = ylabs, labels = paste0(ylabs, "%"))
 title(ylab="Percent reduction in incidence", line=2.4, cex.lab=1.5)
 xlabs <- c("no intervention", "10% PrEP", "25% PrEP", "50% PrEP", "80% \n Suppression", "85% \n Suppression", "90% \n Suppression")
 axis(side=1, at = c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.8), labels=xlabs, tick = F, cex.axis = 1, line = -.5, las = 2)
 title(xlab="Intervention Scenario", line=6, cex.lab=1.5)
  title(main="Los Angeles", cex.main=2, line=-1)
 # title(main="New York City ", cex.main=2, line=-1)
 # title(main="Atlanta", cex.main=2, line=-1)
 # title(main="Baltimore", cex.main=2, line=-1)


  
#### 2: Young --> All MSM ####
prep.improve <- c(100*(croi.results[,9]-croi.results[,3]))
supp.improve <- c(100*(croi.results[,12]-croi.results[,6]))
prep.young <- c(100*(croi.results[,3]))
supp.young <- c(100*(croi.results[,6]))

improve <- expand.grid(msa = c("LA","NYC","Atlanta","Baltimore"),
                       int = c("prep","supp"),
                       group = c("all", "young"))
improve$value <- c(prep.improve,supp.improve,prep.young,supp.young)
improve$forColor <-
  factor(c(rep("PrEP 25%, All MSM",4),
           rep("Suppression 85%, All MSM",4),
           rep("PrEP 25%, Young MSM Only",4),
           rep("Suppression 85%, Young MSM Only",4)),
         levels = c("PrEP 25%, All MSM","Suppression 85%, All MSM","PrEP 25%, Young MSM Only","Suppression 85%, Young MSM Only"))

ggplot(improve,aes(x=int,y=value,fill=forColor))+
  geom_bar(stat = "identity")+
  facet_wrap(~msa,nrow=1,strip.position = "bottom") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = 'white'),
    strip.background =element_rect(fill="white"),
    #axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    text = element_text(size=15),
    #legend.position = c(0.8, 0.8), 
    legend.position = "none", 
    #legend.title = element_blank(),
    #legend.key.size = unit(1, "cm"),
    #legend.key.width = unit(1,"cm"),
    plot.title = element_text(hjust = 0.5, margin = margin(1, 0, -0.5, 0, "cm"))) + 
  scale_y_continuous(breaks = seq(0,100, by = 20), 
                     labels = function(x) paste0(x, "%"), 
                     limits = c(0,100), expand = c(0, 0)) +
  scale_fill_manual(values=c(brewer.pal(9,"Greens")[5], 
                             brewer.pal(9,"Blues")[5], 
                             brewer.pal(9,"Greens")[4], 
                             brewer.pal(9,"Blues")[4])) +
  labs(title = " ") + 
  xlab("Metropolitan Statistical Area (MSA)") + 
  ylab("Percent reduction in incidence") 
# scale_fill_discrete(breaks=c("PrEP, young MSM","PrEP, all MSM","Supp, young MSM","Supp, all MSM")

  
  


#### 3: Combined Interventions ####
msa <- c(rep(c("LA","NYC","Atlanta","Baltimore"),2))
int <- c(rep("Young MSM 25% PrEP; All MSM 85% suppressed",4),rep("Young MSM 50% PrEP; All MSM 90% suppressed",4))
values <- c(croi.results[,c(15:16)])
lower <- c(croi.lower[,c(15:16)])
upper <- c(croi.upper[,c(15:16)])
croi.df <- data.frame(msa,int,values, lower, upper)
croi.df$msa <- factor(croi.df$msa,levels = c("LA","NYC","Atlanta","Baltimore"))
croi.df$int <- factor(croi.df$int, levels = c("Young MSM 25% PrEP; All MSM 85% suppressed","Young MSM 50% PrEP; All MSM 90% suppressed"))

dodge <- position_dodge(width=0.9)
ggplot(croi.df, aes(fill=int, x=msa, y=values)) + 
  geom_col(position=dodge) + 
  geom_errorbar(aes(x=msa, ymin=lower, ymax=upper),
                position = dodge, width=0.25) + 
  # ylim(0,100) +
  scale_y_continuous(breaks = seq(0,1, by = .2), labels = scales::percent, limits = c(0,1), expand = c(0, 0)) +
  theme_classic(base_size = 16, base_line_size = 0.25) + 
  # theme(legend.position="top") +
  theme(legend.position = c(0.8, 1), 
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm") , 
        axis.text.x = element_text(margin = margin(0, 0, 0.5, 0, "cm")),
        plot.title = element_text(hjust = 0.5, margin = margin(1, 0, 1.5, 0, "cm"))
  ) +
  scale_fill_discrete(labels = c("Young MSM 25% PrEP;\n All MSM 85% suppressed","Young MSM 50% PrEP;\n All MSM 90% suppressed")) +
  labs(title = "Combined Interventions") + 
  xlab("Metropolitan Statistical Area (MSA)") + 
  ylab("Percent reduction in incidence") + 
  geom_hline(yintercept=.9, linetype="dashed", color = "red")

#### Old figure versions ####
# Relative improvement
rel.improve <- 100*(croi.results[,8:13]-croi.results[,2:7])/croi.results[,2:7]
avg.rel.improve <- rel.improve[,c(mean(1:3),mean(4:6))]
colnames(avg.rel.improve) <- c("prep","suppression")

cols3 <- c(brewer.pal(9,"Greens")[5], brewer.pal(9,"Blues")[5]) 
int <-barplot(t(avg.rel.improve), col = cols3, 
              ylim = c(0,100),
              axes=F, 
              beside=T, 
              names.arg = c("LA","NYC","Atlanta","Baltimore")
)
ylabs <- seq(0, 100, by = 20)
axis(side = 2, at = ylabs, labels = paste0(ylabs, "%"))
title(main="Added benefit from young --> all MSM")
title(ylab="Relative improvement in incidence reduction", line=2.4, cex.lab=1)
legend("topright",  legend = c( "PrEP", "Suppression"), 
       fill = cols3,bty = "n")

# Absolute improvement
colnames(abs.improve) <- c("prep","suppression")
cols3 <- c(brewer.pal(9,"Greens")[5], brewer.pal(9,"Blues")[5]) 
int <-barplot(t(abs.improve), col = cols3, 
              ylim = c(0,100),
              axes=F, 
              beside=F, 
              names.arg = c("LA","NYC","Atlanta","Baltimore")
)
ylabs <- seq(0, 100, by = 20)
axis(side = 2, at = ylabs, labels = paste0(ylabs, "%"))
title(main="Added benefit from young --> all MSM")
title(ylab="Absolute improvement in incidence reduction", line=2.4, cex.lab=1)
legend("topright",  legend = c( "PrEP", "Suppression"), 
       fill = cols3,bty = "n")

# Fig 2 - Summary 
cols2 <- c(brewer.pal(9,"YlGnBu")[5], brewer.pal(9,"YlOrRd")[6]) 
int <-barplot(100*t(croi.results[,c(15:16)]), col = cols2, 
              ylim = c(0,100),
              axes=F, 
              beside=T, 
              names.arg = c("LA","NYC","Atlanta","Baltimore")
)
ylabs <- seq(0, 100, by = 20)
axis(side = 2, at = ylabs, labels = paste0(ylabs, "%"))
title(ylab="Percent reduction in incidence", line=2.4, cex.lab=1)
legend("topright",  legend = c( "Young MSM 25% PrEP; All MSM 85% suppressed", "Young MSM 50% PrEP; All MSM 90% suppressed"), 
       fill = cols2,bty = "n", cex=.75)






#### Original analysis - 11/9/2020 ####

location = LA.MSA
#This code will run your list of interventions
if (1==2)
{
    location = LA.MSA
    load(file.path('mcmc_runs/quick_simsets', location, get.full.filename(location)))
    
    run.systematic.interventions(simset,
                                 dst.dir = 'mcmc_runs/quick_simsets',
                                 interventions = MELISSA.INTERVENTIONS,
                                 save.baseline.and.seed = F)
}

#This code is going to make you a nice table,
# then write it to a pretty excel file
if (1==2)
{
    location = LA.MSA
    # MS: I updated this based on the order I listed in the previous code 
    # MELISSA - you need to change the 'INT.TTT' to 'm.ttt' (or analagous) for each of these, like I did below
    intervention.code.table = matrix(c('m.ttt', 'm.stt', 'm.ptt', 'm.tts', 'm.sts',  'm.pts', 'm.ttp',  'm.stp', 'm.ptp', 
                                        'm.tst', 'm.sst', 'm.pst', 'm.tss', 'm.sss', 'm.pss', 'm.tsp', 'm.ssp', 'm.psp', 
                                        'm.tpt', 'm.spt', 'm.ppt', 'm.tps', 'm.sps', 'm.pps', 'm.tpp', 'm.spp', 'm.ppp'),
                                     ncol=3)
    
    #Melissa, this should be a table with the intervention codes listed in the
    # order (row/column) in which you want the results to appear
    #I think this matches what you had, but I could be wrong
    # intervention.code.table = matrix(c('INT.TTT', 'INT.TTP', 'INT.TTS', 'INT.PTT', 'INT.PTP', 'INT.PTS', 'INT.STT', 'INT.STP', 'INT.STS',
    #                                    'INT.TPT', 'INT.TPP', 'INT.TPS', 'INT.PPT', 'INT.PPP', 'INT.PPS', 'INT.SPT', 'INT.SPP', 'INT.SPS',
    #                                    'INT.TST', 'INT.TSP', 'INT.TSS', 'INT.PST', 'INT.PSP', 'INT.PSS', 'INT.SST', 'INT.SSP', 'INT.SSS'),
    #                                  ncol=3)
    

    tab = get.estimates.for.interventions(intervention.codes=intervention.code.table,
                                          location=location,
                                          dir='mcmc_runs/quick_simsets')
    
    write.shaded.table(tab, file='whatever file you want to save to.xlsx')
    
}

#Todd's test - melissa, you can ignore this
# (just making sure the code I gave you was correct)
if (1==2)
{
    int.code.table = matrix(INTERVENTION.MANAGER.1.0$code[1:4], nrow=2)
    test.tab = get.estimates.for.interventions(intervention.codes = int.code.table,
                                               location = NYC.MSA,
                                               dir = 'mcmc_runs/visualization_simsets/')
    write.shaded.table(test.tab, file='../temp/test.xlsx')
    
    intervention.code.table = matrix(c('m.ttt', 'm.tst'),
                                     ncol=2)
  
    tab = get.estimates.for.interventions(intervention.codes=intervention.code.table,
                                          location=location,
                                          dir='mcmc_runs/quick_simsets')
}