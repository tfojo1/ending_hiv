
get.timespan.text <- function(seconds,
                              max.spans.to.list=2,
                              digits.for.last.span=1,
                              allowed.spans = c('week','day','hour','minute','second'),
                              show.zeros=T)
{
    mult.from.previous.span = c(week=7,
                                day=24,
                                hour=60,
                                minute=60,
                                second=1)

    total.mult.for.span = rev(cumprod(rev(mult.from.previous.span)))

    total.mult.for.span = total.mult.for.span[intersect(names(total.mult.for.span), allowed.spans)]

    sapply(seconds, function(one.seconds){

        spans = one.seconds / total.mult.for.span

        first.keep.span = c((1:length(spans))[spans>1], length(spans))[1]
        keep.spans = intersect(1:length(spans), first.keep.span + 1:max.spans.to.list - 1)

        spans[-1] = spans[-1] %% mult.from.previous.span[-length(spans)]

        spans = spans[keep.spans]

        spans[-length(spans)] = floor(spans[-length(spans)])
        round.factor = 10^digits.for.last.span
        spans[length(spans)] = floor(round.factor * spans[length(spans)]) / round.factor

        if (!show.zeros)
            spans = spans[span>0 || (1:length(spans))==length(spans)]

        span.text = sapply(1:length(spans), function(i){
            paste0(format(spans[i], big.mark=','), " ",
                   names(spans)[i],
                   ifelse(spans[i]==1, '', 's')
            )
        })

        paste0(span.text, collapse=", ")
    })

}
