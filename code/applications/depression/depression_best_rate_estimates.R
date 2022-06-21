

# Return an array indexed [age, race, sex, risk]
# Where age is: 13-24yo, 

get.estimated.depression.incidence <- function(version='expanded_1.0',
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

get.estimated.depression.remission <- function(version='expanded_1.0',
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