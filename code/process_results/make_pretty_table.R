library(xlsx)

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
    
    int.names = sapply(interventions, get.intervention.name)
    int.codes = sapply(interventions, get.intervention.code)
    x = x[,int.codes]
    
    dim.names = list(location = unlist(msa.names(as.character(attr(estimates, 'location')))),
                     intervention = int.names)
    
    dim(x) = sapply(dim.names, length)
    dimnames(x) = dim.names
    #    dimnames(x)[[1]] = 
    #   dimnames(x)[[2]] = sapply(interventions, get.intervention.name)
    
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
                               write.col.names=T,
                               lowers=NULL,
                               uppers=NULL,
                               use.floor.not.round=T,
                               interval.open='[',
                               interval.close=']',
                               interval.spacer='-',
                               label.lower=NULL,
                               min.value=0,
                               max.value=1)
{
    if (as.pct)
    {
        mult = 100
#        digits = digits + 2
        suffix = '%'
    }
    else
    {
        mult = 1
        suffix = ''
    }
    
    if (use.floor.not.round)
        format.function = function(z) {
            na.mask = is.na(z)
            rv = floor(mult * as.numeric(val) * 10^digits) / 10^digits
            rv = format(rv, nsmall=digits)
            rv[na.mask] = NA
            rv
        }
    else
        format.function = function(z) {
            na.mask = is.na(z)
            rv = format(round(mult*z, digits=digits), nsmall=digits)
            rv[na.mask] = NA
            rv
        }
    
    
    if (is.null(lowers) || is.null(uppers))
        labels = paste0(format.function(x), suffix)
    else
        labels = paste0(format.function(x), suffix, " ",
                        interval.open, format.function(lowers), 
                        interval.spacer,
                        format.function(uppers), interval.close)
            
    if (!is.null(label.lower))
        labels[x<label.lower] = paste0("<", format.function(label.lower), suffix)
    
    dim(labels) = dim(x)
    dimnames(labels) = dimnames(x)
    
#    labels = apply(x, 1:2, function(val){
 #       if (is.na(val))
  #          NA
   #     else
    #        paste0(mult*floor(as.numeric(val) * 10^digits)/10^digits, suffix)
    #})
    
    below.threshold.color = colorRamp(c(below.threshold.min.color, below.threshold.max.color))
    above.threshold.color = colorRamp(c(above.threshold.min.color, above.threshold.max.color))
    
    colors = apply(x, 1:2, function(val){
        if (is.na(val))
            na.color
        else if (val>=threshold)
        {
            scaled.val = (min(max.value,val)-threshold)/(max.value-threshold)
            rgb.to.hex(above.threshold.color(scaled.val), prepend='#')
        }
        else
        {
            scaled.val = max(min.value,as.numeric(val))/(threshold-min.value)
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
