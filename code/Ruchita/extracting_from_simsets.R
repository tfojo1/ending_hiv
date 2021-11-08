
load('mcmc_runs/prep_simsets/47900/1.0_47900_full.Rdata')



#Incidence Pre
rv_1_pre = sapply(simset@simulations, function(sim){
    project.absolute.incidence(sim, years=2010:2013, sex='msm')
})

#Diagnoses Pre
rv_2_pre = sapply(simset@simulations, function(sim){
    project.absolute.new.diagnoses(sim,use.cdc.categorizations = F,years=2010:2013, sex='msm')
})

truth = get.surveillance.data(msa.surveillance, location=DC.MSA, years=2010:2020, data.type='new', risk=T)
truth = c(truth[,1],rep(NA,10))


#Incidence Future 
load('mcmc_runs/prep_simsets/47900/1.0_47900_baseline.oral.variable.efficacy.Rdata')
rv_1_post = sapply(simset@simulations, function(sim){
    project.absolute.incidence(sim, years=2014:2030, sex='msm')
})

#Diagnoses Future
rv_2_post = sapply(simset@simulations, function(sim){
    project.absolute.new.diagnoses(sim,use.cdc.categorizations = F, years=2014:2030, sex='msm')
})


load('mcmc_runs/prep_simsets/47900/1.0_47900_msm.combined.25.uptake_23_27.Rdata')

#Incidence 25% Uptake

Incidence_25 = sapply(simset@simulations, function(sim){
  project.absolute.incidence(sim,keep.dimensions = 'year', years=2020:2030, sex='msm')
})

#Diagnoses 25% Uptake
Diagnoses_25 = sapply(simset@simulations, function(sim){
  project.absolute.new.diagnoses(sim,use.cdc.categorizations = F, years=2020:2030, sex='msm')
})

load('mcmc_runs/prep_simsets/47900/1.0_47900_msm.oral.10.uptake_23_27.Rdata')

#Incidence 10% Uptake
Incidence_10 = sapply(simset@simulations, function(sim){
  project.absolute.incidence(sim,keep.dimensions = 'year', years=2020:2030, sex='msm')
})

#Diagnoses 10% Uptake
Diagnoses_10 = sapply(simset@simulations, function(sim){
  project.absolute.new.diagnoses(sim,use.cdc.categorizations = F, years=2020:2030, sex='msm')
})




#Incidence
rv_1 = rbind(rv_1_pre,rv_1_post)

mean_diff = round(rowMeans(rv_1),0)

CI_low = round(apply(rv_1,1,function(x) quantile(x, probs = .025)),0)
CI_high = round(apply(rv_1,1,function(x) quantile(x, probs = .975)),0)

mean_diff_i_10 = round(rowMeans(Incidence_10),0)
CI_low_i_10 = round(apply(Incidence_10,1,function(x) quantile(x, probs = .025)),0)
CI_high_i_10 = round(apply(Incidence_10,1,function(x) quantile(x, probs = .975)),0)

mean_diff_i_25 = round(rowMeans(Incidence_25),0)
CI_low_i_25 = round(apply(Incidence_25,1,function(x) quantile(x, probs = .025)),0)
CI_high_i_25 = round(apply(Incidence_25,1,function(x) quantile(x, probs = .975)),0)

plots_no = cbind(mean_diff,CI_low,CI_high,2010:2030,"No Intervention")
plots_i_25 = cbind(mean_diff_i_25,CI_low_i_25,CI_high_i_25,2020:2030,"25% Uptake")
plots_i_10 = cbind(mean_diff_i_10,CI_low_i_10,CI_high_i_25,2020:2030,"10% Uptake")
plots_1 = rbind(plots_no,plots_i_25,plots_i_10)
colnames(plots_1) = c("Incidence","CI Low", "CI High", "Year", "Intervention")
plots_1 = as.data.frame(plots_1)
plots_1$Incidence = as.numeric(as.character(plots_1$Incidence))
plots_1$`CI Low` = as.numeric(as.character(plots_1$`CI Low`))
plots_1$`CI High` = as.numeric(as.character(plots_1$`CI High`))




#Diagnoses

rv_2 = rbind(rv_2_pre,rv_2_post)

mean_diff = round(rowMeans(rv_2),0)
CI_low = round(apply(rv_2,1,function(x) quantile(x, probs = .025)),0)
CI_high = round(apply(rv_2,1,function(x) quantile(x, probs = .975)),0)

mean_diff_d_10 = round(rowMeans(Diagnoses_10),0)
CI_low_d_10 = round(apply(Diagnoses_10,1,function(x) quantile(x, probs = .025)),0)
CI_high_d_10 = round(apply(Diagnoses_10,1,function(x) quantile(x, probs = .975)),0)

mean_diff_d_25 = round(rowMeans(Diagnoses_25),0)
CI_low_d_25 = round(apply(Diagnoses_25,1,function(x) quantile(x, probs = .025)),0)
CI_high_d_25 = round(apply(Diagnoses_25,1,function(x) quantile(x, probs = .975)),0)

plots_no = cbind(mean_diff,CI_low,CI_high,2010:2030,"No Intervention",truth)
plots_d_25 = cbind(mean_diff_d_25,CI_low_d_25,CI_high_d_25,2020:2030,"25% Uptake",NA)
plots_d_10 = cbind(mean_diff_d_10,CI_low_d_10,CI_high_d_25,2020:2030,"10% Uptake",NA)
plots_2 = rbind(plots_no,plots_d_25,plots_d_10)
colnames(plots_2) = c("Diagnoses","CI Low", "CI High", "Year", "Intervention","Actual")
plots_2 = as.data.frame(plots_2)
plots_2$Diagnoses = as.numeric(as.character(plots_2$Diagnoses))
plots_2$`CI Low` = as.numeric(as.character(plots_2$`CI Low`))
plots_2$`CI High` = as.numeric(as.character(plots_2$`CI High`))
plots_2$Actual = as.numeric(as.character(plots_2$Actual))

p = ggplot(plots_1, aes(x=Year,y=Incidence,group=Intervention)) +
  geom_ribbon(aes(ymin=`CI Low`,ymax=`CI High`, fill= Intervention), alpha = .09) +
  geom_line(aes(colour=Intervention)) +  theme(text = element_text(size=20),axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=.5))
p + theme_bw()


p = ggplot(plots_2, aes(x=Year,y=Diagnoses,group=Intervention)) +
  geom_ribbon(aes(ymin=`CI Low`,ymax=`CI High`, fill= Intervention), alpha = .09) +
  geom_line(aes(colour=Intervention)) +  theme(text = element_text(size=20),axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=.5))+ geom_point(aes(x = Year, y =Actual))
p + theme_bw()









