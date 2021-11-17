


##-- THE ORIGINAL FUNCTION --##
generate.covid.parameters <- function(N=1000,
                                      sexual.transmission.range=c(0,0.5),
                                      suppression.range=c(0,0.40),#c(0.025,0.40),
                                      testing.range=c(0,0.50),#c(0.08,0.50),
                                      prep.range=c(0,0.30),#c(0.04,0.30),
                                      sexual.transmission.increase.range=c(0,0.25),
                                      sexual.transmission.timeframe=c(0.25,1+(7-3)/12),
                                      tps.timeframe=sexual.transmission.timeframe,
                                      max.time.from.sex.to.tps=c(-0.5,1),
                                      sexual.ramp.down.time = 0.25,
                                      tps.ramp.down.time = 0.25,
                                      seed=123343456, #12334345
                                      sample.time.frames=F
                                      )
{
    # Set seed
    if (!is.null(seed) && !is.na(seed))
        set.seed(seed)
    
    # The changes
    change.dist = join.distributions(
        sexual.transmission.reduction = Uniform.Distribution(sexual.transmission.range[1], sexual.transmission.range[2]),
        testing.reduction = Uniform.Distribution(testing.range[1], testing.range[2]),
        prep.reduction = Uniform.Distribution(prep.range[1], prep.range[2]),
        suppression.reduction = Uniform.Distribution(suppression.range[1], suppression.range[2]),
        sexual.transmission.increase = Uniform.Distribution(sexual.transmission.increase.range[1],
                                                            sexual.transmission.increase.range[2])
    )
    change.samples = generate.random.samples(change.dist, n=N)
    change.bounds = get.support.bounds(change.dist@support)
    
    if (sample.time.frames)
    {
        # The start normalize times
        raw.start.normalize.times = generate.linked.uniform.samples(n=N,
                                                                    range1=sexual.transmission.timeframe,
                                                                    range2=tps.timeframe,
                                                                    range.1.minus.2 = max.time.from.sex.to.tps)
        
        start.normalize.times = cbind(
            sexual.transmission.start.normalize.time = raw.start.normalize.times[,1],
            testing.start.normalize.time = raw.start.normalize.times[,2],
            prep.start.normalize.time = raw.start.normalize.times[,2],
            suppression.start.normalize.time = raw.start.normalize.times[,2]
        )
        start.normalize.time.bounds = cbind(sexual.transmission.timeframe,
                                            tps.timeframe,
                                            tps.timeframe,
                                            tps.timeframe)
        
        # Normal times
        
        normal.times = cbind(
            sexual.transmission.normal.time = start.normalize.times[,'sexual.transmission.start.normalize.time'] + sexual.ramp.down.time,
            testing.normal.time = start.normalize.times[,'testing.start.normalize.time'] + tps.ramp.down.time,
            prep.normal.time = start.normalize.times[,'prep.start.normalize.time'] + tps.ramp.down.time,
            suppression.normal.time = start.normalize.times[,'suppression.start.normalize.time'] + tps.ramp.down.time
        )
        normal.time.bounds = cbind(sexual.transmission.timeframe + sexual.ramp.down.time,
                                   tps.timeframe + tps.ramp.down.time,
                                   tps.timeframe + tps.ramp.down.time,
                                   tps.timeframe + tps.ramp.down.time)
        
        # Add Bounds and Return
        rv = cbind(change.samples,
                   start.normalize.times,
                   normal.times)
        
        bounds = cbind(change.bounds, start.normalize.time.bounds, normal.time.bounds)
        dimnames(bounds)[[2]] = dimnames(rv)[[2]]
    }
    else
    {
        rv = change.samples
        bounds = change.bounds
    }
    
    attr(rv, 'bounds') = bounds
    
    rv
}

generate.linked.uniform.samples <- function(n,
                                            range1,
                                            range2,
                                            range.1.minus.2)
{
    samples = cbind(
        runif(n, range1[1], range1[2]),
        runif(n, range2[1], range2[2])
    )
    
    diff = samples[,1] - samples[,2]
    
    mask = diff < range.1.minus.2[1] | diff > range.1.minus.2[2]
    if (any(mask))
        samples[mask,] = generate.linked.uniform.samples(n=sum(mask),
                                                         range1=range1,
                                                         range2=range2,
                                                         range.1.minus.2=range.1.minus.2)
    
    samples
}