

# Return an array indexed [age, race, sex, risk]
# Where age is: 13-24 years, 25-34 years, 35-44 years, 45-54 years, 55+ years
#       race is: black, hispanic, other
#       sex is: heterosexual_male, msm, female
#       risk is: never_IDU, active_IDU, IDU_in_remission

#figure out age, race, sex, risk lengths/breakdowns, still need IDU status 
get.estimated.depression.incidence <- function(version='expanded_1.0',
                                               dir='/code/applications/depression/',
                                               Prop.less.1.year = .71,
                                               MSM.Ratio.age1 = 2.142288557, 
                                               MSM.Ratio.age2 = 2.108974359)
{
    incidence_white = read.csv(paste0(dir,"White_Incidence.csv")) #try to reconvert the actual incidence file instead for the proper age groups
    incidence_black = read.csv(paste0(dir,"Black_Incidence.csv")) #read files correctly
    incidence_hispanic = read.csv(paste0(dir,"Hispanic_Incidence.csv")) #initialize matrix correctly 
    rv = setup.depression.array.skeleton(version)
   
    #White incidence 
    sapply(1:length(age), function(i){
      sapply(1:length(sex), function(j){
        if(j == 1){
          rv[i,"other",j,] = -log(1-(incdence_white[i,j]*Prop.less.1.year))
        } else if (j == 2){
          if (i = 1){
            rv[i,"other",j,] = -log(1-(incdence_white[i,1]*Prop.less.1.year)*MSM.Ratio.age1) 
          } else{
            rv[i,"other",j,] = -log(1-(incdence_white[i,1]*Prop.less.1.year)*MSM.Ratio.age2) 
          }
          
        } else if (j == 3) {
          rv[i,"other",j,] = -log(1-(incdence_white[i,j-1]*Prop.less.1.year))
        }
        
        
      })
      
    })
    
    #Black incidence 
    sapply(1:length(age), function(i){
      sapply(1:length(sex), function(j){
        if(j == 1){
          rv[i,"black",j,] = -log(1-(incdence_black[i,j]*Prop.less.1.year))
        } else if (j == 2){
          if (i = 1){
            rv[i,"black",j,] = -log(1-(incdence_black[i,1]*Prop.less.1.year)*MSM.Ratio.age1) 
          } else{
            rv[i,"black",j,] = -log(1-(incdence_black[i,1]*Prop.less.1.year)*MSM.Ratio.age2) 
          }
          
        } else if (j == 3) {
          rv[i,"black",j,] = -log(1-(incdence_black[i,j-1]*Prop.less.1.year))
        }
        
        
      })
      
    })
    
    #Hispanic incidence 
    sapply(1:length(age), function(i){
      sapply(1:length(sex), function(j){
        if(j == 1){
          rv[i,"hispanic",j,] = -log(1-(incdence_hispanic[i,j]*Prop.less.1.year))
        } else if (j == 2){
          if (i = 1){
            rv[i,"hispanic",j,] = -log(1-(incdence_hispanic[i,1]*Prop.less.1.year)*MSM.Ratio.age1) 
          } else{
            rv[i,"hispanic",j,] = -log(1-(incdence_hispanic[i,1]*Prop.less.1.year)*MSM.Ratio.age2) 
          }
          
        } else if (j == 3) {
          rv[i,"hispanic",j,] = -log(1-(incdence_hispanic[i,j-1]*Prop.less.1.year))
        }
        
        
      })
      
    })
    return(rv)
    
  
    
    # @Ruchita: populate this array
    
    # to load a file - called example.csv
    # put it in the dir referenced above
    # data = read.csv(file.path(dir, 'example.csv))
    
    # Return
    rv
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
                                                          dir=dir='code/applications/depression/')
{
    rv = setup.depression.array.skeleton(version)
    rv[] = .47
    return(rv)
}

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