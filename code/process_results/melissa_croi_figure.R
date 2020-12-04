library(scales)
tab = as.matrix(read.csv('results/melissa_croi_v3.csv', header = F))

interventions = paste0(c(paste0(c(10,25,50), '% PrEP'),
                         paste0(c(80,85,90), '% Suppression')),
                       ", ",
                       rep(c("MSM <35yo", "All MSM"), each=6))
msas = c("LA", "NYC", "Atlanta", "Baltimore")
dimnames(tab) = list(MSA=msas, Intervention=interventions)


df = melt(tab)
df$Reduction = -round(df$value,2)

png('results/melissa_croi_by_msa.png', width=6, height=4, res=300, units='in')
ggplot(df) + geom_bar(aes(x=MSA, y=Reduction, fill=Intervention), stat='identity', position='dodge') +
   scale_y_continuous(labels=percent, limits = c(-1,0)) + scale_x_discrete(position='top') +
    ylab("Change in Total Incidence")
dev.off()


png('results/melissa_croi_by_intervention.png', width=6, height=4, res=300, units='in')
ggplot(df) + geom_bar(aes(x=Intervention, y=Reduction, fill=MSA), stat='identity', position='dodge') +
    scale_y_continuous(labels=percent, limits = c(-1,0)) + scale_x_discrete(position='top') +
    ylab("Change in Total Incidence") +
    theme(axis.text.x = element_text(angle = 60, hjust=0))
dev.off()
