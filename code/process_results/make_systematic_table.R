
IMAGE.DIR = '../Manuscripts/manuscript_1/images/'


if (1==2)
{
#    df = make.systematic.table('mcmc_runs/visualization_simsets//')
#    write.csv(df, file='results/table2_test.csv')
    
#    df = read.csv('results/table2_test.csv')
#    df.full = df
#    df = df[!apply(df,1,function(x){any(x=='')}),]
    
    est = get.estimates.and.intervals('mcmc_runs/visualization_simsets/', 
                                    msas=get.hiv.burden()$CBSA,
                                    interventions=INTERVENTION.SET)
    save(est, file='results/quick_estimated.Rdata')
    load('results/quick_estimated.Rdata')
    
    write.systematic.table(est, file='results/table3_raw.xlsx')
    
    #The old way with a plot
    plot.systematic.table(est)
    
    png(file.path(IMAGE.DIR, 'Figure 3.png'), pointsize=10, width=3.92, height=5.01, res=300, units='in')
    plot.systematic.table(est, label.size = 3) + theme_nothing() + 
        scale_x_discrete(expand=c(0,0)) +
        scale_y_discrete(expand=c(0,0)) +
        labs(x = NULL, y = NULL)
        #theme(axis.title=element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = 'none' )
    dev.off()
    

    
}

library(scales)
library(cowplot)
library(xlsx)

source('code/source_code.R')
source('code/targets/parse_targets.R')
source('code/interventions/systematic_interventions.R')
source('code/interventions/synthesize_interventions.R')

make.systematic.table <- function(dir,
                                  msas=get.hiv.burden()$CBSA,
                                  interventions=INTERVENTION.SET,
                                  ci.spacer='-',
                                  digits=0,
                                  mean.pct='%',
                                  ci.pct='',
                                  year1=2020,
                                  year2=2030,
                                  ci.opening='[',
                                  ci.closing=']',
                                  verbose=T)
{
    dir = file.path(dir)
    rv = sapply(1:length(interventions), function(i){
        int = interventions[[i]]
        if (verbose)
            print(paste0("Reading ", length(msas), " locations for intervention ", i, " of ", length(interventions),
                         ": ", get.intervention.name(int)))
        sapply(as.character(msas), function(msa){
            filename = file.path(dir, msa, get.simset.filename(location=msa, intervention=int))
            if (file.exists(filename))
            {
                if (verbose)
                    print(paste0(" - ", msa.names(msa)))
                
                load(filename)
                reduction = get.simset.incidence.reduction(simset, year1=year1, year2=year2)
                c(paste0(round(100*reduction['mean.reduction'], digits), mean.pct),
                  paste0(ci.opening, round(100*reduction['ci.lower'], digits), ci.pct,
                         ci.spacer, round(100*reduction['ci.upper'], digits), ci.pct, ci.closing))
            }
            else
                c('','')
        })
    })
    
    rv = cbind(rep(msa.names(msas), each=2),
               rv)
    
    attr(rv, 'interventions') = interventions
    attr(rv, 'locations') = msas
#    rv = as.data.frame(rv)
    rv
}


get.estimates.and.intervals <- function(dir,
                                        msas=get.hiv.burden()$CBSA,
                                        interventions=INTERVENTION.SET,
                                        verbose=T,
                                        year1=2020,
                                        year2=2030)
{
    dir = file.path(dir)
    all.mat = sapply(1:length(interventions), function(i){
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
                reduction = get.simset.incidence.reduction(simset, year1=year1, year2=year2)
                reduction
            }
            else
                rep(NA, 4)
        })
    })
    
    dim(all.mat)=c(4,length(msas),length(interventions))
    dim.names = list(1:4, location=as.character(msas), intervention=sapply(interventions, get.intervention.filename))
    dimnames(all.mat) = dim.names
    
    rv=list(estimates=all.mat[1,,],
            ci.lower=all.mat[2,,],
            ci.upper=all.mat[3,,],
            probability=all.mat[4,,])
    
    attr(rv, 'interventions') = interventions
    attr(rv, 'locations') = msas
    rv
}

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
    values = estimates$estimates
    labels = apply(values, 1:2, function(val){
        if (is.na(val))
            NA
        else
            paste0(floor(100*val * 10^digits)/10^digits, '%')
    })
    
    below.threshold.color = colorRamp(c(below.threshold.min.color, below.threshold.max.color))
    above.threshold.color = colorRamp(c(above.threshold.min.color, above.threshold.max.color))
    colors = apply(values, 1:2, function(val){
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
    
    dimnames(labels)[[1]] = unlist(msa.names(as.character(attr(estimates, 'location'))))
    dimnames(labels)[[2]] = sapply(interventions, get.intervention.name)
    
    write.shaded.xlsx(x=labels,
                      colors=colors,
                      file=file,
                      na=NULL)
}

write.shaded.xlsx <- function(x,
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
