
##-----------------##
##-- SOURCE CODE --##
##-----------------##


library(scales)
library(cowplot)
library(xlsx)

#source('code/source_code.R')
source('code/targets/parse_targets.R')
#source('code/interventions/systematic_interventions.R')
source('code/interventions/synthesize_interventions.R')

##--------------------------------------------##
##-- CODE TO RUN OUR MAIN ANALYSIS - BY MSA --##
##--------------------------------------------##

if (1==2)
{

    #-- FOR ANALYSIS 1: --#
    
    est = get.estimates.and.intervals('mcmc_runs/visualization_simsets/', 
                                    msas=get.hiv.burden()$CBSA,
                                    interventions=A1.INTERVENTION.SET,
                                    n.sim=80)
    save(est, file='results/quick_estimated_a1.Rdata')
#    load('results/quick_estimated_a1.Rdata')
    write.systematic.table(est, file='results/table3_raw.xlsx')
    

    #-- FOR ANALYSIS 2: --#
    
    est = get.estimates.and.intervals('mcmc_runs/visualization_simsets/', 
                                      msas=get.hiv.burden()$CBSA,
                                      interventions=c(list(NO.INTERVENTION), A2.INTERVENTION.SET),
                                      n.sim=80)
    save(est, file='results/quick_estimated_a2.Rdata')
    #load('results/quick_estimated_a2.Rdata')
    write.systematic.table(est, file='results/table_s1_raw.xlsx')
    
    
}

##------------------------------------------------------------##
##-- GENERATING AND WRITING ESTIMATES FOR THE MAIN ANALYSIS --##
##------------------------------------------------------------##

get.estimates.and.intervals <- function(dir,
                                        msas=get.hiv.burden()$CBSA,
                                        interventions=INTERVENTION.SET,
                                        n.sim,
                                        verbose=T,
                                        year1=2020,
                                        year2=2030,
                                        summary.stat=mean,
                                        interval.coverage=0.95,
                                        calculate.total=T)
{
    dir = file.path(dir)
    all.arr = sapply(1:length(interventions), function(i){
        int = interventions[[i]]
        if (verbose)
            print(paste0("Reading ", length(msas), " locations for intervention ", i, " of ", length(interventions),
                         ": ", get.intervention.filename(int)))
        sapply(as.character(msas), function(msa){
            filename = file.path(dir, msa, get.simset.filename(location=msa, intervention=int))
            if (file.exists(filename))
            {
                if (verbose)
                    print(paste0(" - ", msa.names(msa)))
                
                load(filename)
                
                if (sum(simset@weights)<n.sim)
                    stop(paste0("The simset for ", msa.names(msa), " on intervention '",
                                get.intervention.name(int), "' does not have ",
                                n.sim, " simulations"))
                
                indices = unlist(sapply(1:simset@n.sim, function(i){
                    rep(i, simset@weights[i])
                }))
                indices = indices[1:n.sim]
                
                values = sapply(simset@simulations[unique(indices)], get.sim.absolute.incidence, years=c(year1,year2))
                values = values[,indices]
                values
            }
            else
                rep(NA, n.sim*2)
        })
    })
    
    dim.names = list(year=c(year1,year2),
                     sim=1:n.sim,
                     location=msas,
                     intervention=sapply(interventions, get.intervention.code))
    dim(all.arr) = sapply(dim.names, length)
    dimnames(all.arr) = dim.names
    
    if (calculate.total)
    {
        dim.names$location = c(dim.names$location, 'Total')
        new.all.arr = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
        new.all.arr[,,dimnames(all.arr)$location,] = all.arr
        new.all.arr[,,'Total',] = apply(all.arr, c(1,2,4), sum, na.rm=T)
        all.arr = new.all.arr
    }
    
    #indexed [sim, msa, intervention]
    rel.change = -(all.arr[2,,,] - all.arr[1,,,]) / all.arr[1,,,]

    alpha = (1-interval.coverage)/2
    rv=list(estimates=apply(rel.change, 2:3, summary.stat),
            ci.lower=apply(rel.change, 2:3, quantile, probs=alpha),
            ci.upper=apply(rel.change, 2:3, quantile, probs=1-alpha)
            )
    
    attr(rv, 'interventions') = interventions
    attr(rv, 'locations') = dim.names$location
    
    rv
}

write.systematic.table <- function(estimates,
                                   file,
                                  interventions=attr(estimates, 'interventions'),
                                  include.interval=F,
                                  below.threshold.min.color='red',
                                  below.threshold.max.color='yellow2',
                                  above.threshold.min.color='green3',
                                  above.threshold.max.color='green4',
                                  na.color='gray',
                                  threshold=0.9,
                                  digits=0)
{
    x = estimates$estimates
    
    dimnames(x)[[1]] = unlist(msa.names(as.character(attr(estimates, 'location'))))
    dimnames(x)[[2]] = sapply(interventions, get.intervention.name)

    
    write.shaded.table(x=x,
                       file=file,
                       threshold=threshold,
                       below.threshold.min.color = below.threshold.min.color,
                       below.threshold.max.color = below.threshold.max.color,
                       above.threshold.min.color = above.threshold.min.color,
                       above.threshold.max.color = above.threshold.max.color,
                       na.color = na.color,
                       digits=digits,
                       as.pct=T)
}


##-------------------------------------------------------------------------##
##-- GETTING ARBITRARY ESTIMATES FOR AN ARBITRARY TABLE OF INTERVENTIONS --##
##-------------------------------------------------------------------------##

#'@param intervention.codes can be an array, table, whatever
#'@param location can be either a scalar value or an object with the same dimension as intervention.codes 
#'@param dir the directory holding the simsets
#'@param summary.stat A function that creates a summary statistic from a vector of outcomes (one from each simulation)
#'@param outcome.fn A function that takes one argument, a simulation, and returns the output desired
get.estimates.for.interventions <- function(intervention.codes,
                                            location,
                                            dir,
                                            summary.stat = mean,
                                            outcome.fn = extract.total.incidence.reduction.20.30)
{
    rv = sapply(1:length(intervention.codes), function(i){
        int.code = intervention.codes[i]
        int = intervention.from.code(int.code)
        if (length(location)==1)
            loc = location
        else
            loc = location[i]
        
        filename = file.path(dir, loc, get.simset.filename(location=loc, intervention=int))
        load(filename)
        
        sim.outcomes = sapply(simset@simulations, outcome.fn)
        sim.outcomes = unlist(sapply(1:simset@n.sim, function(i){
            rep(sim.outcomes[i], simset@weights[i])
        }))
        summary.stat(sim.outcomes)
    })
    
    if (is.null(dim(intervention.codes)))
    {
        if (is.null(names(intervention.codes)))
            names(rv) = intervention.codes
        else
            names(rv) = names(intervention.codes)
    }
    else
    {
        dim(rv) = dim(intervention.codes)
        dimnames(rv) = dimnames(intervention.codes)
    }
    
    rv
}

##--------------------------------##
##-- MAKING SHADED EXCEL TABLES --##
##--------------------------------##

write.shaded.table <- function(x,
                               file,
                               threshold=0.9,
                               below.threshold.min.color='red',
                               below.threshold.max.color='yellow2',
                               above.threshold.min.color='green3',
                               above.threshold.max.color='green4',
                               na.color='gray',
                               digits=0,
                               as.pct=T,
                               write.row.names=T,
                               write.col.names=T)
{
    if (as.pct)
    {
        mult = 100
        digits = digits + 2
        suffix = '%'
    }
    else
    {
        mult = 1
        suffix = ''
    }
    
    labels = apply(x, 1:2, function(val){
        if (is.na(val))
            NA
        else
            paste0(mult*floor(val * 10^digits)/10^digits, suffix)
    })
    
    below.threshold.color = colorRamp(c(below.threshold.min.color, below.threshold.max.color))
    above.threshold.color = colorRamp(c(above.threshold.min.color, above.threshold.max.color))
    
    colors = apply(x, 1:2, function(val){
        if (is.na(val))
            na.color
        else if (val>=threshold)
        {
            scaled.val = (min(1,val)-threshold)/(1-threshold)
            rgb.to.hex(above.threshold.color(scaled.val), prepend='#')
        }
        else
        {
            scaled.val = max(0,val)/threshold
            rgb.to.hex(below.threshold.color(scaled.val), prepend='#')
        }
    })
    
    dimnames(labels)[[1]] = dimnames(x)[[1]]
    dimnames(labels)[[2]] = dimnames(x)[[2]]
    
    do.write.shaded.xlsx(x=labels,
                         colors=colors,
                         file=file,
                         na=NULL,
                         row.names=write.row.names,
                         col.names=write.col.names)
    
}

do.write.shaded.xlsx <- function(x,
                              colors,
                              file,
                              row.names=T,
                              col.names=T,
                              na='NA')
{
    start.row = 1-row.names
    start.col = 1-col.names
    
    wb = createWorkbook()
    sheet = createSheet(wb, "Sheet1")
    if (col.names)
    {
        names.for.column = colnames(x)
        if (is.null(names.for.column))
            names.for.column = 1:dim(x)[2]
        row = createRow(sheet, rowIndex=1)
        for (j in 1:length(names.for.column))
        {
            cell = createCell(row, colIndex = j + row.names)[[1,1]]
            setCellValue(cell, names.for.column[j])
        }
    }
    
    if (row.names)
    {
        names.for.row=rownames(x)
        if (is.null(names.for.row))
            names.for.row = 1:dim(x)[1]
    }
    for (i in 1:nrow(x))
    {
        row = createRow(sheet, rowIndex=i+col.names)
        if (row.names)
        {
            cell = createCell(row, colIndex=1)[[1,1]]
            setCellValue(cell, names.for.row[i])
        }
        
        for (j in 1:ncol(x))
        {
            if (!is.na(x[i,j]) || !is.null(na))
            {
                cell = createCell(row, colIndex=j+row.names)[[1,1]]
                if (is.na(x[i,j]))
                    setCellValue(cell, na)
                else                
                    setCellValue(cell, x[i,j])
                
                cell.style <- CellStyle(wb) +
                    Fill(backgroundColor=colors[i,j], foregroundColor=colors[i,j],
                         pattern="SOLID_FOREGROUND") 
                setCellStyle(cell, cell.style) 
            }
        }
    }
    
    saveWorkbook(wb, file = file)
}


rgb.to.hex <- function(rgb, prepend='')
{
    rgb = round(rgb)
    if (is.null(dim(rgb)))
        rv = toupper(paste0(format(as.hexmode(rgb[1]), width=2), 
                            format(as.hexmode(rgb[2]), width=2), 
                            format(as.hexmode(rgb[3]), width=2)))
    else   
        rv = toupper(paste0(format(as.hexmode(rgb[,1]), width=2), 
                            format(as.hexmode(rgb[,2]), width=2), 
                            format(as.hexmode(rgb[,3]), width=2)))
    
    paste0(prepend, rv)
}

##---------------------------------##
##--             PLOTS           --##
##-- (Not really using this now) --##
##---------------------------------##

plot.systematic.table <- function(estimates,
                                  interventions=attr(estimates, 'interventions'),
                                  include.interval=F,
                                  no.change.color='red',
                                  full.change.color='green3',
                                  threshold.color='yellow3',
                                  threshold=0.9,
                                  threshold.shift=0.5,
                                  digits=0,
                                  label.size=4)
{
    df = melt(estimates$estimates)
    df$location = msa.names(as.character(df$location))
    df$location = factor(df$location, levels = rev(msa.names(attr(estimates, 'locations'))))
    #    df$label = paste0(round(100*df$value, digits), '%')
    df$label = paste0(floor(100*df$value * 10^digits)/10^digits, '%')
    df$label[is.na(df$value)] = ''
    df$value = pmin(1, pmax(0, df$value))
    #    above.threshold = !is.na(df$value) & df$value >= threshold
    #    df$value[above.threshold] = 1#df$value[above.threshold] + threshold.shift
    
    ggplot(df, aes(x=intervention, y=location, fill=value)) +
        geom_tile() +
        geom_text(aes(label=label), size=label.size) +
        scale_fill_gradientn(labels=function(x){paste0(round(100*x, digits), '%')},
                             colors=c(no.change.color, threshold.color, full.change.color, full.change.color),
                             values=c(0,threshold-.001,threshold,1),
                             limits=c(0,1))
}


make.systematic.legend <- function(below.threshold.min.color='red',
                                   below.threshold.max.color='yellow2',
                                   above.threshold.min.color='green3',
                                   above.threshold.max.color='green4',
                                   threshold=0.9)
{
    df = data.frame(x=1:4,
                    y=10,
                    d=c(0,threshold-.01,threshold,1))
    ggplot(df, aes(x=x, y=y, fill=d)) +
        geom_bar(stat='identity') +
        scale_fill_gradientn(labels=function(x){paste0(round(100*x, 0), '%')},
                             colors=c(below.threshold.min.color, below.threshold.max.color, above.threshold.min.color, above.threshold.max.color),
                             values=c(0,threshold-.001,threshold,1),
                             limits=c(0,1),
                             name=NULL) + 
        theme(legend.position = 'bottom')
}
