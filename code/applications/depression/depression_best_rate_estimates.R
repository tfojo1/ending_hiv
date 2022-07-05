

# Return an array indexed [age, race, sex, risk]
# Where age is: 13-24 years, 25-34 years, 35-44 years, 45-54 years, 55+ years
#       race is: black, hispanic, other
#       sex is: heterosexual_male, msm, female
#       risk is: never_IDU, active_IDU, IDU_in_remission

get.estimated.depression.incidence <- function(version='expanded_1.0',
                                               dir='cleaned_data/depression')
{
    rv = setup.depression.array.skeleton(version)
    
    age = '13-24 years'
    race = 'black'
    sex = 'heterosexual_male'
    risk = 'never_IDU'
    values = c(.1413, .1, .2)
    
    rv[age, race, sex, risk] = values[1]
    
    rv[, races, sex, risk]
    
    # @Ruchita: populate this array
    
    # to load a file - called example.csv
    # put it in the dir referenced above
    # data = read.csv(file.path(dir, 'example.csv))
    
    # Return
    rv
}

get.estimated.depression.remission <- function(version='expanded_1.0',
                                               dir='cleaned_data/depression')
{
    rv = setup.depression.array.skeleton(version)
    
    # @Ruchita: populate this array
    rv[] = 2
    
    # to load a file - called example.csv
    # put it in the dir referenced above
    # data = read.csv(file.path(dir, 'example.csv))
    
    # Return
    rv
}

get.estimated.depression.treatment.initiation <- function(version='expanded_1.0',
                                               dir='cleaned_data/depression')
{
    rv = setup.depression.array.skeleton(version)
    
    # @Ruchita: populate this array
    
    # to load a file - called example.csv
    # put it in the dir referenced above
    # data = read.csv(file.path(dir, 'example.csv))
    
    # Return
    rv
}

get.estimated.depression.treatment.discontinuation <- function(version='expanded_1.0',
                                                          dir='cleaned_data/depression')
{
    rv = setup.depression.array.skeleton(version)
    
    # @Ruchita: populate this array
    
    # to load a file - called example.csv
    # put it in the dir referenced above
    # data = read.csv(file.path(dir, 'example.csv))
    
    # Return
    rv
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