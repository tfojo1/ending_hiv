

create.comorbidities.manager <- function()
{
    rv = list()
    class(rv) = 'comorbidities.manager'
    
    rv
}

get.comorbidities.model <- function(comorbidities.manager,
                                    type,
                                    location)
{
    comorbidities.manager[[type]]
}

