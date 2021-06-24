
source('code/source_code.R')
source('code/covid/prepare_and_run_covid_sims.R')
source('code/targets/target_msas.R')
source('code/plots.R')

library(scales)

load('mcmc_runs/full_simsets/12580/1.0_12580_noint.Rdata')
ss.noint = simset

load('mcmc_runs/covid_simsets/1.0_12580_COVID.to.2022.Rdata')
ss.2022 = simset

load('mcmc_runs/covid_simsets/1.0_12580_COVID.to.2023.Rdata')
ss.2023 = simset


load('mcmc_runs/covid_simsets/1.0_12580_COVID.to.2022-2023.Rdata')
ss.2022.2023 = simset

THEME = theme(text=element_text(size=20))
#plot 2022

#800x500
plot.calibration.total(list(Without_COVID=ss.noint, COVID_Normal_by_2022=ss.2022), 
                       data.types='incidence', years=2019:2025, plot.individual.simset.sims = F) + THEME

#1200 x 500
plot.calibration.total(list(Without_COVID=ss.noint, COVID_Normal_by_2022=ss.2022), 
                       data.types=c('incidence','new','diagnosed','prevalence'), 
                       years=2019:2025, plot.individual.simset.sims = F) + THEME
    
#800 x 500
plot.calibration.total(list(COVID_Normal_by_2023=ss.2022.2023, COVID_Normal_by_2022=ss.2022), 
                       data.types='incidence', years=2019:2025, plot.individual.simset.sims = F, sim1.color=GREEN) + THEME


#1200 x 400
THEME2 = theme(text=element_text(size=15), plot.title = element_text(hjust = 0.5))
delta.2022.noint = get.incidence.change(ss.2022, ss.noint)
qplot(ss.2023@parameters[,'sexual.transmission.reduction'], delta.2022.noint) + 
    xlab('Change in Sexual Transmission (%)') + ggtitle('Change in Sexual Transmission') + 
    ylab("Change in Projected Incident Cases (n)") + scale_x_continuous(labels = percent) + geom_hline(yintercept = 0) + THEME2
qplot(ss.2023@parameters[,'testing.reduction'], delta.2022.noint) + 
    xlab('Change in HIV Testing (%)') + ggtitle('Change in HIV Testing') + 
    ylab("Change in Projected Incident Cases (n)") + scale_x_continuous(labels = percent) + geom_hline(yintercept = 0) + THEME2
qplot(ss.2023@parameters[,'prep.reduction'], delta.2022.noint)+ 
    xlab('Change in PrEP Uptake (%)') + ggtitle('Change in PrEP Uptake') + 
    ylab("Change in Projected Incident Cases (n)") + scale_x_continuous(labels = percent) + geom_hline(yintercept = 0) + THEME2
qplot(ss.2023@parameters[,'suppression.reduction'], delta.2022.noint)+ 
    xlab('Change in Viral Suppression (%)') + ggtitle('Change in Viral Suppression') + 
    ylab("Change in Projected Incident Cases (n)") + scale_x_continuous(labels = percent) + geom_hline(yintercept = 0) + THEME2


delta.2023.2022 = get.incidence.change(ss.2022.2023, ss.2022)
qplot(ss.2023@parameters[,'sexual.transmission.reduction'], delta.2023.2022) + 
    xlab('Change in Sexual Transmission (%)') + ggtitle('Change in Sexual Transmission') + 
    ylab("Change in Projected Incident Cases (n)") + scale_x_continuous(labels = percent) + geom_hline(yintercept = 0) + THEME2
qplot(ss.2023@parameters[,'testing.reduction'], delta.2023.2022) + 
    xlab('Change in HIV Testing (%)') + ggtitle('Change in HIV Testing') + 
    ylab("Change in Projected Incident Cases (n)") + scale_x_continuous(labels = percent) + geom_hline(yintercept = 0) + THEME2
qplot(ss.2023@parameters[,'prep.reduction'], delta.2023.2022)+ 
    xlab('Change in PrEP Uptake (%)') + ggtitle('Change in PrEP Uptake') + 
    ylab("Change in Projected Incident Cases (n)") + scale_x_continuous(labels = percent) + geom_hline(yintercept = 0) + THEME2
qplot(ss.2023@parameters[,'suppression.reduction'], delta.2023.2022)+ 
    xlab('Change in Viral Suppression (%)') + ggtitle('Change in Viral Suppression') + 
    ylab("Change in Projected Incident Cases (n)") + scale_x_continuous(labels = percent) + geom_hline(yintercept = 0) + THEME2


qplot(delta.2023.2022)

delta.2023b.2022 = get.incidence.change(ss.2023, ss.2022)
qplot(delta.2023b.2022)
