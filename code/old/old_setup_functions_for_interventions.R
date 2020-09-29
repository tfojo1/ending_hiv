get.rates.from.background.and.foreground <- function(background.rates,
                                                     background.times,
                                                     foreground.rates,
                                                     foreground.times,
                                                     foreground.start.times,
                                                     max.background.time=Inf,
                                                     allow.foreground.less=F)
{
    if (is.null(foreground.times))
        list(rates=background.rates[background.times<=max.background.time],
             times=background.times[background.times<=max.background.time])
    else
    {
        if (is.null(foreground.rates))
            stop("foreground.rates is NULL")
        
        background.rates = background.rates[background.times<=max.background.time]
        background.times = background.times[background.times<=max.background.time]
        
        if (max(background.times) < (min(foreground.times)-1))
        {
            background.times = c(background.times, min(foreground.times)-1)
            background.rates = c(background.rates,
                                 background.rates[length(background.rates)])
        }
        
        rates.and.times = merge.rates(rates1 = background.rates,
                                      times1 = background.times,
                                      rates2 = foreground.rates,
                                      times2 = foreground.times)
        
        list(rates=lapply(1:length(rates.and.times$rates1), function(i){
            foreground = rates.and.times$rates2[[i]]
            rates = background = rates.and.times$rates1[[i]]
            if (rates.and.times$times[i] >= min(foreground.times))
            {
                mask = !is.na(foreground)
                if (!allow.foreground.less)
                    mask = mask & foreground >= background
                rates[mask] = foreground[mask]
            }
            
            rates
        }),
        times=rates.and.times$times)
    }
}