

#testing prior
if (1==2)
{
    cm = ALL.DATA.MANAGERS$continuum
    v = as.list(cm$testing$log.ors)
    
    x = exp(rbind(MSM=v$base + v$msm + v$other + v$age3,
          MSM_slope=v$msm_slope + v$other_slope + v$age3_slope,
          Heterosexual=v$base + v$heterosexual + v$heterosexual_male + v$other + v$age3,
          Heterosexual_slope=v$heterosexual_slope + v$heterosexual_male_slope + v$other_slope + v$age3_slope,
          IDU = v$base + v$idu + v$heterosexual_male + v$other + v$age3,
          IDU_Slope = v$idu_slope + v$heterosexual_male_slope + v$other_slope + v$age3_slope,
          MSM_IDU = v$base + v$msm + v$other + v$age3,
          MSM_IDU_slope = v$msm_slope + v$other_slope + v$age3_slope,
          Black = v$black - v$other,
          Black_slope = v$black_slope - v$other_slope,
          Hispanic = v$hispanic - v$other,
          Hispanic_slope = v$hispanic_slope - v$other_slope,
          Age1 = v$age1 - v$age3,
          Age1_slope = v$age1_slope - v$age3_slope,
          Age2 = v$age2 - v$age3,
          Age2_slope = v$age2_slope - v$age3_slope,
          Age4 = v$age4 - v$age3,
          Age4_slope = v$age4_slope - v$age3_slope,
          Age5 = v$age5 - v$age3,
          Age5_slope = v$age5_slope - v$age3_slope,
          Female = v$female - v$heterosexual_male,
          Female_slope = v$female_slope - v$heterosexual_male_slope
          ))
    
    x=round(x,3)
    write.csv(x, file='../Manuscripts/manuscript_1/tables/testing_ors.csv')
}



#suppression prior
if (1==2)
{
    cm = ALL.DATA.MANAGERS$continuum
    v = as.list(cm$suppression$log.ors)
    
    x = exp(rbind(MSM=v$msm + v$other + v$age3,
                  MSM_slope=v$msm_slope + v$other_slope + v$age3_slope,
                  Heterosexual=v$heterosexual_male + v$other + v$age3,
                  Heterosexual_slope = v$heterosexual_male_slope + v$other_slope + v$age3_slope,
                  IDU = v$idu_male + v$other + v$age3,
                  IDU_slope = v$idu_male_slope + v$other_slope + v$age3_slope,
                  MSM_IDU = v$msm_idu + v$other + v$age3,
                  MSM_IDU_slope = v$msm_idu_slope + v$other_slope + v$age3_slope,
                  Black = v$black - v$other,
                  Black_slope = v$black_slope - v$other_slope,
                  Hispanic = v$hispanic - v$other,
                  Hispanic_slope = v$hispanic_slope - v$other_slope,
                  Age1 = v$age1 - v$age3,
                  Age1_slope = v$age1_slope - v$age3_slope,
                  Age2 = v$age2 - v$age3,
                  Age2_slope = v$age2_slope - v$age3_slope,
                  Age4 = v$age4 - v$age3,
                  Age4_slope = v$age4_slope - v$age3_slope,
                  Age5 = v$age5 - v$age3,
                  Age5_slope = v$age5_slope - v$age3_slope,
                  Heterosexual_female = v$heterosexual_female - v$heterosexual_male,
                  Heterosexual_female_slope = v$heterosexual_female_slope - v$heterosexual_male_slope,
                  IDU_female = v$idu_female - v$idu_male,
                  IDU_female_slope = v$idu_female_slope - v$idu_male_slope
                  ))
    
    slope.mask = grepl("slope", dimnames(x)[[1]])
    intercept.mask = !slope.mask
    
    x[intercept.mask,] = x[intercept.mask,] / (x[slope.mask,]^10)
    
    x=round(x,3)
    write.csv(x, file='../Manuscripts/manuscript_1/tables/suppression_ors.csv')
}