

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

#PrEP
if (1==2)
{
    model = get.prep.model(ALL.DATA.MANAGERS$prep)
    v = as.list(model$log.or.vector)
    
    expit = function(lo){1 / (1+exp(-lo))}
    x = rbind(
        expit(rbind(MSM=v$msm + v$other + v$age3,
                    MSM_slope=v$msm_slope + v$other_slope,
                    Heterosexual=v$heterosexual + v$other + v$age3,
                    Heterosexual_slope = v$heterosexual_slope + v$other_slope,
                    IDU = v$idu + v$other + v$age3,
                    IDU_slope = v$idu_slope + v$other_slope,
                    MSM_IDU = v$msm_idu + v$other + v$age3,
                    MSM_IDU_slope = v$msm_idu_slope + v$other_slope)),
        exp(rbind(Black = v$black - v$other,
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
                  Female = v$female - v$male))
    )
    
    #    slope.mask = grepl("slope", dimnames(x)[[1]])
    #   intercept.mask = !slope.mask
    
    #  x[intercept.mask,] = x[intercept.mask,] / (x[slope.mask,]^10)
    
    x=round(x,4)
    write.csv(x, file='../Manuscripts/manuscript_1/tables/prep_ors.csv')
}

#for prep indications
if (1==2)
{
    #-- Get probability of PrEP Indication (conditioned on sexual activity for MSM and HET) --#
    data = get.prep.lit.data(as.proportion.of.indicated = T)
    msm = make.marginal.rr.array(p=data$nhbs$msm.2017$p.indicated,
                                 n=data$nhbs$msm.2017$n)
    idu = make.marginal.rr.array(p=data$nhbs$idu.2018$p.indicated,
                                 n=data$nhbs$idu.2018$n.sex.age)
    het = make.marginal.rr.array(p=data$nhbs$het.2016$p.indicated,
                                 n=data$nhbs$het.2016$n.sex.age)
    
    #-- Multiply MSM and HET by prob of sexual activity --#
    
    # female age1-4 from NSFG 2002, table 2
    #   https://www.cdc.gov/nchs/data/ad/ad362.pdf
    #   
    # male age1-4 from NSFG 2002,
    #   https://www.cdc.gov/nchs/data/ad/ad362.pdf  
    #   
    # age5 from Lindau 2007 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2426743/
    
    p.active.male.by.age = c(
        age1=((1-.403)*10208 + (1-.09)*9883)/(10208+9883)*10/12,
        age2=1-.047,
        age3=((1-.027)*10138+(1-.019)*10557)/(10138+10557),
        age4=1-.018,
        age5=(.837*528 + .670*547 + .385*380)/1455
    )
    p.active.female.by.age = c(
        age1=((1-.367)*9834+(1-.087)*9840)/(9384+9840)*10/12,
        #reduce by 10/12 assuming no activity from 13-14yo
        age2=1-.025,
        age3=((1-.018)*10272+(1-.01)*10853)/(10272+10853),
        age4=1-.013,
        age5=(.616*492 + .395*545 + .167*513)/1550
    )
    
    cat(make.latex.table(msm[,,1]))
    cat(make.latex.table(idu[,,1]))
    cat(make.latex.table(het[,,1]))
    
    p.active = cbind(Female=p.active.female.by.age, 
                     Male=p.active.male.by.age)
    cat(make.latex.table(p.active))
}


make.latex.table <- function(mat, digits=3)
{
    mat = format(mat, digits=3)
    
    row.names = rownames(mat)
    col.names = colnames(mat)
    
    rv = paste0(' & ', paste0('\\textbf{', col.names, '}', collapse=' & '), ' \\\\',
                '\n \\hline')
    for (i in 1:length(row.names))
        rv = paste0(rv,
                    '\n',
                    '\\textbf{', row.names[i], '} & ',
                    paste0(mat[i,], collapse=' & '),
                    ' \\\\')
    
    rv
}