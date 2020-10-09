
#Testing
if (1==2)
{
    library(ggplot2)
    span1 = 5
    x = seq(0,(3*span1),by=1)#length=100)
    r1=1;r2=2;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))
    r1=1;r2=5;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))
    r1=5;r2=5;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))
    r1=5;r2=2.5;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))
    r1=5;r2=.5;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))

    r1=.5;r2=.5;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))
    r1=1;r2=.5;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))
    r1=.5;r2=.75;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))
    r1=.5;r2=2;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))


    r1=2;r2=3;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))
    r1=2;r2=4;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))
    r1=4;r2=5;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))


    r1=.8;r2=.2;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))

    r1=.3;r2=.2;qplot(x, calculate.change.ratios.two.logistic(r1=r1, r2=r2, t1=span1, t2=2*span1, times=x)) +
        ylim(0, NA) + geom_vline(xintercept=c(span1, 2*span1))
}

##-----------------------##
##-- THE MAIN FUNCTION --##
##-----------------------##

calculate.change.ratios.two.logistic <- function(r1,r2,
                                                 times=0:t2,
                                                 r0=1,
                                                 t0=0,t1=5,t2=10,
                                                 fraction.of.asymptote.after.end=0.05,
                                                 fraction.of.asymptote.before.start=0.025,
                                                 fraction.of.asymptote.for.change.dir=0.02)
{
    r0 = round(r0, 10)
    r1 = round(r1, 10)
    r2 = round(r2, 10)

    if ((fraction.of.asymptote.after.end+fraction.of.asymptote.before.start)>=1)
        stop("The sum of fraction.of.asymptote.after.end and fraction.of.asymptote.before.start must be less than 1")

    if (r0==0 && r1==0)
        r0.to.1 = 1
    else
        r0.to.1 = r1/r0
    
    if (r0==0 && r2==0)
        r0.to.2 = 1
    else
        r0.to.2 = r2/r0
    
    if (r1==0 && r2==0)
        r1.to.2 = 1
    else
        r1.to.2 = r0.to.2 / r0.to.1

    if (r0.to.1 == 1 && r1.to.2 == 1)
    {
        model.1 = model.2 = get.no.change.logistic.model(r0)
    }
    else if ((r0.to.1 > 1 && r1.to.2 > 1) ||
             (r0.to.1 < 1 && r1.to.2 < 1)) #both in the same directions
    {
        delta.K.A = (r2 - r0) / (1 - fraction.of.asymptote.for.change.dir - fraction.of.asymptote.before.start)
        A.overall = r0 - delta.K.A * fraction.of.asymptote.before.start
        K.overall = r2 + delta.K.A * fraction.of.asymptote.for.change.dir

        first.is.smaller = abs(r1-r0) < abs(r2-r1)
        model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.overall, r0=r0, t0=t0)
        model.2 = fit.logistic.model(r1=r2, t1=t2, A=A.overall, K=K.overall, r0=r1, t0=t1)

        if (get.logistic.slope(model.1, t1)==0 || get.logistic.slope(model.2, t1)==0)
        {}
        else if (first.is.smaller)
        {
            A.2 = r1 + (A.overall-r1) * get.logistic.slope(model.1, t1) / get.logistic.slope(model.2, t1)
            model.2 = fit.logistic.model(r1=r2, t1=t2, A=A.2, K=K.overall, r0=r1, t0=t1)
        }
        else
        {
            K.1 = r1 + (K.overall-r1) * get.logistic.slope(model.2, t1) / get.logistic.slope(model.1, t1)
            model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.1, r0=r0, t0=t0)
        }
    }
    else #change direction
    {
        if (r0.to.1==1)
            model.1 = get.no.change.logistic.model(r0)
        else
        {
            delta.K.A = (r1 - r0) / (1 - fraction.of.asymptote.for.change.dir - fraction.of.asymptote.before.start)
            A = r0 - delta.K.A * fraction.of.asymptote.before.start
            K = r1 + delta.K.A * fraction.of.asymptote.for.change.dir

            model.1 = fit.logistic.model(r1=r1, t1=t1, A=A, K=K, r0=r0, t0=t0)
        }

        if (r1.to.2==1)
            model.2 = get.no.change.logistic.model(r1)
        else
        {
            delta.K.A = (r2 - r1) / (1 - fraction.of.asymptote.after.end - fraction.of.asymptote.for.change.dir)
            A = r1 - delta.K.A * fraction.of.asymptote.for.change.dir
            K = r2 + delta.K.A * fraction.of.asymptote.after.end

            model.2 = fit.logistic.model(r1=r2, t1=t2, A=A, K=K, r0=r1, t0=t1)
        }
    }

    times.1 = times[times<t1]
    times.2 = setdiff(times, times.1)

    c(get.logistic.points(model.1, times.1), get.logistic.points(model.2, times.2))
}

calculate.change.ratios.logistic <- function(r0=1,r1,
                                             times=t0:t1,
                                             t0=0,t1=5,
                                                 fraction.of.asymptote.after.end=0.05,
                                                 fraction.of.asymptote.before.start=0.025)
{
    r0 = round(r0, 10)
    r1 = round(r1, 10)

    if ((fraction.of.asymptote.after.end+fraction.of.asymptote.before.start)>=1)
        stop("The sum of fraction.of.asymptote.after.end and fraction.of.asymptote.before.start must be less than 1")

    r0.to.1 = r1/r0

    if (r0.to.1 == 1)
        model.1 = get.no.change.logistic.model(r0)
    else
    {
        delta.K.A = (r1 - r0) / (1 - fraction.of.asymptote.after.end - fraction.of.asymptote.before.start)
        A.overall = r0 - delta.K.A * fraction.of.asymptote.before.start
        K.overall = r1 + delta.K.A * fraction.of.asymptote.after.end

        model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.overall, r0=r0, t0=t0)
    }

    get.logistic.points(model.1, times)
}

calculate.change.ratios.logistic.array <- function(r0.arr,
                                                   r1.arr,
                                             times=t0:t1,
                                             t0=0,t1=5,
                                             fraction.of.asymptote.after.end=0.05,
                                             fraction.of.asymptote.before.start=0.025)
{
    if (any(dim(r0.arr) != dim(r1.arr)))
        stop("r0.arr and r1.arr must have the same dimensions")

    if ((fraction.of.asymptote.after.end+fraction.of.asymptote.before.start)>=1)
        stop("The sum of fraction.of.asymptote.after.end and fraction.of.asymptote.before.start must be less than 1")

    if (all(r0.arr==r1.arr))
        return (lapply(times, function(i){r0.arr}))
    
    models = lapply(1:length(r0.arr), function(i){

        r0 = round(r0.arr[i], 10)
        r1 = round(r1.arr[i], 10)

        r0.to.1 = r1/r0

        if (r0.to.1 == 1)
            model.1 = get.no.change.logistic.model(r0)
        else
        {
            delta.K.A = (r1 - r0) / (1 - fraction.of.asymptote.after.end - fraction.of.asymptote.before.start)
            A.overall = r0 - delta.K.A * fraction.of.asymptote.before.start
            K.overall = r1 + delta.K.A * fraction.of.asymptote.after.end

            model.1 = fit.logistic.model(r1=r1, t1=t1, A=A.overall, K=K.overall, r0=r0, t0=t0)
        }

        model.1
    })

    lapply(times, function(t){
        arr = sapply(models, get.logistic.points, times=t)
        dim(arr) = dim(r0.arr)
        dimnames(arr) = dimnames(r0.arr)
        arr
    })
}


##-- LOWER-LEVEL LOGISTIC MODELS --##

get.logistic.points <- function(logistic.model, times)
{
    rv = logistic.model$A + (logistic.model$K - logistic.model$A) /
        (1 + logistic.model$Q*exp(-logistic.model$B * (times-logistic.model$t0)))
    names(rv) = times
    rv
}

get.logistic.slope <- function(logistic.model, times)
{
    rv = (logistic.model$K - logistic.model$A) *
        logistic.model$B * logistic.model$Q * exp(-logistic.model$B * times) /
        (1 + logistic.model$Q * exp(-logistic.model$B * (times-logistic.model$t0)))^2
    names(rv) = times
    rv
}

fit.logistic.model <- function(r1, t1, A, K, r0=1, t0=0)
{
    # Solve for Q, B
    Q = (K-r0)/(r0-A)
    B = (log((r1-A)/(K-r1)) + log(Q)) / (t1-t0)
    if (is.na(B))
        stop("NA values generated in fitting logistic model")


    # Store in an object and return
    list(K=K,
         A=A,
         B=B,
         Q=Q,
         t0=t0)
}

get.no.change.logistic.model <- function(r0=1)
{
    list(K=r0,
         A=0,
         Q=0,
         B=0,
         t0=0)
}


# f(t) = A + (K - A) / (1 + Q*e^(-B*t))
smooth.logistic.one.point <- function(r1, t1, times=t0:t1, r0=1, t0 = 0,
                                      A=NA, K=NA,
                                      r.span.of.total=0.9, min.r=0, max.r=Inf)
{
    if (is.na(A) || is.na(K))
    {
        # Set up upper and lower asymptotes
        r.span = abs(r1-r0)
        r.span.total = r.span / r.span.of.total
        r.extra = r.span.total - r.span

        A = min(r1, r0) - r.extra/2
        A = max(min.r, A)

        K = max(r1, r0) + r.extra/2
        K = min(max.r, K)
    }

    # Plug and chug
}

