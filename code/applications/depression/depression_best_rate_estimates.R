

# Return an array indexed [age, race, sex, risk]
# Where age is: 13-24yo, 

#figure out age, race, sex, risk lengths/breakdowns 
get.estimated.depression.incidence <- function(version='expanded_1.0',
                                               dir='code/applications/depression/',
                                               Prop.less.1.year = .71,
                                               MSM.Ratio.age1 = 2.142288557, 
                                               MSM.Ratio.age2 = 2.108974359)
{
    incidence_white = read.csv(paste0(dir,"White_Incidence.csv"))
    incidence_black = read.csv(paste0(dir,"Black_Incidence.csv"))
    incidence_hispanic = read.csv(paste0(dir,"Hispanic_Incidence.csv"))
    rv = setup.depression.array.skeleton(version)
    dim.names = list(age=age,
                     race = race,
                     sex=sex,
                     risk=risk) 
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    return(rv)
    

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