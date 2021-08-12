

predict.new.prob <- function(model,
                             outcome=c('suppress','lost','remain')[1],
                             year,
                             age,
                             race,
                             sex,
                             risk)
{
    #put math here
}


if (1==2)
{
    sapply(risks, function(risk){
        sapply(sexes, function(sex){
            sapply(races, function(race){
                sapply(ages, function(age){
                    sapply(years, function(year){
                        predict.new.prob(...)
                    })
                })
            })
        })
    })
}