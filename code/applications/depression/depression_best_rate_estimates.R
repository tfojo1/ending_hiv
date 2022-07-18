

# Return an array indexed [age, race, sex, risk]
# Where age is: 13-24 years, 25-34 years, 35-44 years, 45-54 years, 55+ years
#       race is: black, hispanic, other
#       sex is: heterosexual_male, msm, female
#       risk is: never_IDU, active_IDU, IDU_in_remission

#figure out age, race, sex, risk lengths/breakdowns, still need IDU status 

setup.depression.array.skeleton <- function(version)
{
  settings = get.settings.for.version(version)
  ages = settings$AGES$labels
  races = settings$RACES
  sexes = settings$SEXES
  risks = settings$RISK_STRATA
  
  dim.names = list(age=ages,
                   race=races,
                   sex=sexes,
                   risk=risks)
  rv = array(0, dim=sapply(dim.names, length), dimnames = dim.names)
  
  rv
}

get.estimated.depression.incidence <- function(version='expanded_1.0',
                                               dir="/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/code/applications/depression/",
                                               Prop.less.1.year = .71,
                                               MSM.Ratio.age1 = 2.142288557, 
                                               MSM.Ratio.age2 = 2.108974359,
                                               Male.age1.prevIDU.ratio = 3.886486,
                                               Male.age1.activeIDU.ratio = 7.218919,
                                               Female.age1.prevIDU.ratio =  1.857143,
                                               Female.age1.activeIDU.ratio = 3.906475,
                                               Male.age2.prevIDU.ratio = 4.158218,
                                               Male.age2.activeIDU.ratio = 6.9170511,
                                               Female.age2.prevIDU.ratio = 4.158218,
                                               Female.age2.activeIDU.ratio = 6.9170511
                                               )
{
    incidence_white = read.csv(paste0(dir,"White_Incidence_Never_IDU.csv"),header = TRUE, row.names = 1) 
    incidence_black = read.csv(paste0(dir,"Black_Incidence_Never_IDU.csv"),header = TRUE, row.names = 1) 
    incidence_hispanic = read.csv(paste0(dir,"Hispanic_Incidence_Never_IDU.csv"),header = TRUE, row.names = 1)
    
    incidence_white = incidence_white[,c(1:2)] 
    incidence_black = incidence_black[,c(1:2)]
    incidence_hispanic = incidence_hispanic[,c(1:2)]
    
    
    
    rv = setup.depression.array.skeleton(version)

   
    #White incidence Never IDU
    for ( i in 1: length(dimnames(rv)$age)){
      for(j in 1:length(dimnames(rv)$sex)){
        
        
        if(j == 1){
          rv[i,"other",j,"never_IDU"] = -log(1-(incidence_white[i,j]*Prop.less.1.year))
        } else if (j == 2){
          if (i == 1){
            rv[i,"other",j,"never_IDU"] = -log(1-(incidence_white[i,1]*Prop.less.1.year)*MSM.Ratio.age1) 
          } else{
            rv[i,"other",j,"never_IDU"] = -log(1-(incidence_white[i,1]*Prop.less.1.year)*MSM.Ratio.age2) 
          }
          
        } else if (j == 3) {
          rv[i,"other",j,"never_IDU"] = -log(1-(incidence_white[i,j-1]*Prop.less.1.year))
        }
        
        
      }
    }
      
    
    
    #Black incidence Never IDU
    for(i in 1:length(dimnames(rv)$age)){
      for(j in 1:length(dimnames(rv)$sex)){
        if(j == 1){
          rv[i,"black",j,"never_IDU"] = -log(1-(incidence_black[i,j]*Prop.less.1.year))
        } else if (j == 2){
          if (i == 1){
            rv[i,"black",j,"never_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*MSM.Ratio.age1) 
          } else{
            rv[i,"black",j,"never_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*MSM.Ratio.age2) 
          }
          
        } else if (j == 3) {
          rv[i,"black",j,"never_IDU"] = -log(1-(incidence_black[i,j-1]*Prop.less.1.year))
        }
        
        
      }
      
    }
    
    #Hispanic incidence Never IDU
    for(i in 1:length(dimnames(rv)$age)){
      for(j in 1:length(dimnames(rv)$sex)){
        if(j == 1){
          rv[i,"hispanic",j,"never_IDU"] = -log(1-(incidence_hispanic[i,j]*Prop.less.1.year))
        } else if (j == 2){
          if (i == 1){
            rv[i,"hispanic",j,"never_IDU"] = -log(1-(incidence_hispanic[i,1]*Prop.less.1.year)*MSM.Ratio.age1) 
          } else{
            rv[i,"hispanic",j,"never_IDU"] = -log(1-(incidence_hispanic[i,1]*Prop.less.1.year)*MSM.Ratio.age2) 
          }
          
        } else if (j == 3) {
          rv[i,"hispanic",j,"never_IDU"] = -log(1-(incidence_hispanic[i,j-1]*Prop.less.1.year))
        }
        
        
      }
      
    }
    
    #Active/Previous IDU usage 
    for (i in 1:length(dimnames(rv)$age)){
      for(j in 1:length(dimnames(rv)$sex)){
        if(i == 1){
          if(j ==1){
            rv[i,"other",j,"IDU_in_remission"] = -log(1-(incidence_white[i,1]*Prop.less.1.year)*Male.age1.prevIDU.ratio) 
            rv[i,"black",j,"IDU_in_remission"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age1.prevIDU.ratio) 
            rv[i,"hispanic",j,"IDU_in_remission"] = -log(1-(incidence_hispanic[i,1]*Prop.less.1.year)*Male.age1.prevIDU.ratio) 
              
            rv[i,"other",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age1.activeIDU.ratio) 
            rv[i,"black",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age1.activeIDU.ratio) 
            rv[i,"hispanic",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age1.activeIDU.ratio) 
          } else if (j == 2){
            
            rv[i,"other",j,"IDU_in_remission"] = -log(1-(incidence_white[i,1]*Prop.less.1.year)*Male.age1.prevIDU.ratio*MSM.Ratio.age1) 
            rv[i,"black",j,"IDU_in_remission"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age1.prevIDU.ratio*MSM.Ratio.age1) 
            rv[i,"hispanic",j,"IDU_in_remission"] = -log(1-(incidence_hispanic[i,1]*Prop.less.1.year)*Male.age1.prevIDU.ratio*MSM.Ratio.age1) 
            
            rv[i,"other",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age1.activeIDU.ratio*MSM.Ratio.age1) 
            rv[i,"black",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age1.activeIDU.ratio*MSM.Ratio.age1) 
            rv[i,"hispanic",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age1.activeIDU.ratio*MSM.Ratio.age1) 
          } else if ( j == 3){
            rv[i,"other",j,"IDU_in_remission"] = -log(1-(incidence_white[i,1]*Prop.less.1.year)*Female.age1.prevIDU.ratio) 
            rv[i,"black",j,"IDU_in_remission"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Female.age1.prevIDU.ratio) 
            rv[i,"hispanic",j,"IDU_in_remission"] = -log(1-(incidence_hispanic[i,1]*Prop.less.1.year)*Female.age1.prevIDU.ratio) 
            
            rv[i,"other",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Female.age1.activeIDU.ratio) 
            rv[i,"black",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Female.age1.activeIDU.ratio) 
            rv[i,"hispanic",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Female.age1.activeIDU.ratio) 
          }
          
        }
        else{ 
          if(j == 1){
            rv[i,"other",j,"IDU_in_remission"] = -log(1-(incidence_white[i,1]*Prop.less.1.year)*Male.age2.prevIDU.ratio) 
            rv[i,"black",j,"IDU_in_remission"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age2.prevIDU.ratio) 
            rv[i,"hispanic",j,"IDU_in_remission"] = -log(1-(incidence_hispanic[i,1]*Prop.less.1.year)*Male.age2.prevIDU.ratio) 
            
            rv[i,"other",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age2.activeIDU.ratio) 
            rv[i,"black",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age2.activeIDU.ratio) 
            rv[i,"hispanic",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age2.activeIDU.ratio) 
          } else if(j == 2){
            rv[i,"other",j,"IDU_in_remission"] = -log(1-(incidence_white[i,1]*Prop.less.1.year)*Male.age2.prevIDU.ratio*MSM.Ratio.age2) 
            rv[i,"black",j,"IDU_in_remission"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age2.prevIDU.ratio*MSM.Ratio.age2) 
            rv[i,"hispanic",j,"IDU_in_remission"] = -log(1-(incidence_hispanic[i,1]*Prop.less.1.year)*Male.age2.prevIDU.ratio*MSM.Ratio.age2) 
            
            rv[i,"other",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age2.activeIDU.ratio*MSM.Ratio.age2) 
            rv[i,"black",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age2.activeIDU.ratio*MSM.Ratio.age2) 
            rv[i,"hispanic",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Male.age2.activeIDU.ratio*MSM.Ratio.age2) 
            
            
          } else if ( j == 3){
            rv[i,"other",j,"IDU_in_remission"] = -log(1-(incidence_white[i,1]*Prop.less.1.year)*Female.age2.prevIDU.ratio) 
            rv[i,"black",j,"IDU_in_remission"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Female.age2.prevIDU.ratio) 
            rv[i,"hispanic",j,"IDU_in_remission"] = -log(1-(incidence_hispanic[i,1]*Prop.less.1.year)*Female.age2.prevIDU.ratio) 
            
            rv[i,"other",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Female.age2.activeIDU.ratio) 
            rv[i,"black",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Female.age2.activeIDU.ratio) 
            rv[i,"hispanic",j,"active_IDU"] = -log(1-(incidence_black[i,1]*Prop.less.1.year)*Female.age2.activeIDU.ratio) 
          }
          
          
        }
        
      }
    }
    
    return(rv)
    
  
    
    # @Ruchita: populate this array
    
    # to load a file - called example.csv
    # put it in the dir referenced above
    # data = read.csv(file.path(dir, 'example.csv))
    
    # Return
    
}

get.estimated.depression.remission <- function(version='expanded_1.0',
                                               dir='code/applications/depression/')
{
    rv = setup.depression.array.skeleton(version)
    rv[] = .05
    return(rv)
    
  
}

get.estimated.depression.treatment.initiation <- function(version='expanded_1.0',
                                                          dir='code/applications/depression/')
{
    rv = setup.depression.array.skeleton(version)
    rv[] = .753
    return(rv)
}

get.estimated.depression.treatment.discontinuation <- function(version='expanded_1.0',
                                                          dir='code/applications/depression/')
{
    rv = setup.depression.array.skeleton(version)
    rv[] = .47
    return(rv)
}

