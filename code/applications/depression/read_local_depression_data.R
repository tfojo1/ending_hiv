

# return a a list with three elements,
# each element is an array indexed [year, substate regtion+state code, age]
# where the codes are generated with the function get.substate.region.codes
# $prevalence
# $prevalence.lower
# $prevalence.upper
read.depression.prevalence <- function(dir)
{
    
}

get.substate.region.codes <- function(state.abbreviations,
                                      regions)
{
    paste0(state.abbreviations, "_", regions)
}