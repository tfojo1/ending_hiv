

load('code/Ruchita/msm.results.by.race_2022-03-23.Rdata')

inc.per.pop = msm.results.by.race[,,,'incidence',,] / msm.results.by.race[,,,'population',,]

b.vs.o = inc.per.pop[,'black',,,] / inc.per.pop[,'other',,,]
h.vs.o = inc.per.pop[,'hispanic',,,] / inc.per.pop[,'other',,,]

black.disparity.ratios=cbind(colMeans(b.vs.o['2019',,,1]),colMeans(b.vs.o['2030',,,]));dimnames(black.disparity.ratios)[[2]]=NULL;round(black.disparity.ratios,1)
hispanic.disparity.ratios=cbind(colMeans(h.vs.o['2019',,,1]),colMeans(h.vs.o['2030',,,]));dimnames(hispanic.disparity.ratios)[[2]]=NULL;round(hispanic.disparity.ratios,1)



#Ratio of Incident Cases per Population in 2030 


#Black

load("code/Ruchita/msm.results.by.race_2022-03-23.Rdata")
load("code/Ruchita/prep.parameters_2022-03-23.Rdata")
load("code/Ruchita/total.msm.results_2022-03-23.Rdata")

Table_V2_black_ratio = matrix(nrow = 33, ncol = 8)
colnames(Table_V2_black_ratio) = dimnames(msm.results.by.race)$intervention
rownames(Table_V2_black_ratio) = c(dimnames(msm.results.by.race)$location,"Total")



for(a in 1:8){
  int.code = a
  for(b in 1:32){
    msa = b
    x_black = msm.results.by.race['2030','black',,'incidence',msa,int.code]/msm.results.by.race['2030','black',,'population',msa,int.code]
    x_other = msm.results.by.race['2030','other',,'incidence',msa,int.code]/msm.results.by.race['2030','other',,'population',msa,int.code]
    Table_V2_black_ratio[b,a] =mean(x_black/x_other) #We have taken the mean of the ratio of incident cases per population across 1000 simulations  
    
    
  }
  
  Total_Row_other= rowSums(msm.results.by.race['2030','other',,'incidence',,int.code])/rowSums(msm.results.by.race['2030','other',,'population',,int.code]) 
  Total_Row_black= rowSums(msm.results.by.race['2030','black',,'incidence',,int.code])/rowSums(msm.results.by.race['2030','black',,'population',,int.code]) 
  
  Table_V2_black_ratio[33,a] = mean(Total_Row_black/Total_Row_other)
  
}

x_black_ratio_2020 = matrix(nrow = 33, ncol =1)
colnames(x_black_ratio_2020) = "2020_incidence_ratio"
for(b in 1:32){
  msa = b
  x_black_2020 = msm.results.by.race['2020','black',,'incidence',msa,1]/msm.results.by.race['2020','black',,'population',msa,1]
  x_other_2020 = msm.results.by.race['2020','other',,'incidence',msa,1]/msm.results.by.race['2020','other',,'population',msa,1]
  x_black_ratio_2020[b,1] =mean(x_black_2020/x_other_2020) #We have taken the mean of the ratio of incident cases per population across 1000 simulations  
  
  
}


Total_Row_other_2020= rowSums(msm.results.by.race['2020','other',,'incidence',,1])/rowSums(msm.results.by.race['2020','other',,'population',,1]) 
Total_Row_black_2020= rowSums(msm.results.by.race['2020','black',,'incidence',,1])/rowSums(msm.results.by.race['2020','black',,'population',,1]) 
x_black_ratio_2020[33,1] = mean(Total_Row_black_2020/Total_Row_other_2020)


x_black_ratio_2019 = matrix(nrow = 33, ncol =1)
colnames(x_black_ratio_2019) = "2019_incidence_ratio"
for(b in 1:32){
  msa = b
  x_black_2019 = msm.results.by.race['2019','black',,'incidence',msa,1]/msm.results.by.race['2019','black',,'population',msa,1]
  x_other_2019 = msm.results.by.race['2019','other',,'incidence',msa,1]/msm.results.by.race['2019','other',,'population',msa,1]
  x_black_ratio_2019[b,1] =mean(x_black_2019/x_other_2019) #We have taken the mean of the ratio of incident cases per population across 1000 simulations  
  
  
}




Table_V2_black_ratio = cbind(x_black_ratio_2019,x_black_ratio_2020,Table_V2_black_ratio)

Table_V2_black_ratio = round( Table_V2_black_ratio,2)

#Hispanic

Table_V2_hispanic_ratio = matrix(nrow = 33, ncol = 8)
colnames(Table_V2_hispanic_ratio) = dimnames(msm.results.by.race)$intervention
rownames(Table_V2_hispanic_ratio) = c(dimnames(msm.results.by.race)$location,"Total")


for(a in 1:8){
  int.code = a
  for(b in 1:32){
    msa = b
    x_hispanic = msm.results.by.race['2030','hispanic',,'incidence',msa,int.code]/msm.results.by.race['2030','hispanic',,'population',msa,int.code]
    x_other = msm.results.by.race['2030','other',,'incidence',msa,int.code]/msm.results.by.race['2030','other',,'population',msa,int.code]
    Table_V2_hispanic_ratio[b,a] =mean(x_hispanic/x_other) #We have taken the mean of the ratio of incident cases per population across 1000 simulations  
    
    
  }
  
  Total_Row_other= rowSums(msm.results.by.race['2030','other',,'incidence',,int.code])/rowSums(msm.results.by.race['2030','other',,'population',,int.code]) 
  Total_Row_hispanic= rowSums(msm.results.by.race['2030','hispanic',,'incidence',,int.code])/rowSums(msm.results.by.race['2030','hispanic',,'population',,int.code]) 
  
  Table_V2_hispanic_ratio[33,a] = mean(Total_Row_hispanic/Total_Row_other)
  
}

x_hispanic_ratio_2020 = matrix(nrow = 33, ncol =1)
colnames(x_hispanic_ratio_2020) = "2020_incidence_ratio"
for(b in 1:32){
  msa = b
  x_hispanic_2020 = msm.results.by.race['2020','hispanic',,'incidence',msa,1]/msm.results.by.race['2020','hispanic',,'population',msa,1]
  x_other_2020 = msm.results.by.race['2020','other',,'incidence',msa,1]/msm.results.by.race['2020','other',,'population',msa,1]
  x_hispanic_ratio[b,1] =mean(x_hispanic_2020/x_other_2020) #We have taken the mean of the ratio of incident cases per population across 1000 simulations  
  
  
}


Total_Row_other_2020= rowSums(msm.results.by.race['2020','other',,'incidence',,1])/rowSums(msm.results.by.race['2020','other',,'population',,1]) 
Total_Row_hispanic_2020= rowSums(msm.results.by.race['2020','hispanic',,'incidence',,1])/rowSums(msm.results.by.race['2020','hispanic',,'population',,1]) 
x_hispanic_ratio_2020[33,1] = mean(Total_Row_hispanic_2020/Total_Row_other_2020)



x_hispanic_ratio_2019 = matrix(nrow = 33, ncol =1)
colnames(x_hispanic_ratio_2019) = "2019_incidence_ratio"
for(b in 1:32){
  msa = b
  x_hispanic_2019 = msm.results.by.race['2019','hispanic',,'incidence',msa,1]/msm.results.by.race['2019','hispanic',,'population',msa,1]
  x_other_2019 = msm.results.by.race['2019','other',,'incidence',msa,1]/msm.results.by.race['2019','other',,'population',msa,1]
  x_hispanic_ratio_2019[b,1] =mean(x_hispanic_2019/x_other_2019) #We have taken the mean of the ratio of incident cases per population across 1000 simulations  
  
  
}


Table_V2_hispanic_ratio = cbind(x_hispanic_ratio_2019,x_hispanic_ratio,Table_V2_hispanic_ratio)

Table_V2_hispanic_ratio = round( Table_V2_hispanic_ratio,2)

