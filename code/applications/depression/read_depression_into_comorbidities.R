

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


read.depression.data.into.comorbidities.manager <- function(comorbidities.manager,
                                                            dir='cleaned_data/depression/',
                                                            base.version='expanded_1.0')
{
   inc = get.estimated.depression.incidence(dir, version=base.version) 
   comorbidities.manager[['depression.incidence']] = create.log.rate.model(intercept=log(inc),
                                                                           slope=NULL,
                                                                           anchor.year=2020)
   
   remission = get.estimated.depression.remission(dir, version=base.version)
   comorbidities.manager[['depression.remission.rate']] = create.log.rate.model(intercept=log(remission),
                                                                           slope=NULL,
                                                                           anchor.year=2020)
   
   treatment = get.estimated.depression.treatment.initiation(dir, version=base.version)
   comorbidities.manager[['depression.treatment.rate']] = create.log.rate.model(intercept=log(treatment),
                                                                           slope=NULL,
                                                                           anchor.year=2020)
   
   discontinuation = get.estimated.depression.treatment.discontinuation(dir, version=base.version)
   comorbidities.manager[['depression.treatment.discontinuation.rate']] = create.log.rate.model(intercept=log(discontinuation),
                                                                           slope=NULL,
                                                                           anchor.year=2020)
   
   comorbidities.manager
}