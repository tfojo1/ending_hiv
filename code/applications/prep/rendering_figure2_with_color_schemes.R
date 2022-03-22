
BAD.COLOR = 'red'
GOOD.COLOR = 'purple'

library(reshape2)

TEST.COLORS = T

# for picking the color
if (TEST.COLORS)
{
   #read your table here
   
    file = 'results/prep/Figure_2_Updated.csv' #change as needed
    tab = read.csv(file)
    tab = tab[,-1]
    
    
    df = data.frame(
        val = as.numeric(as.matrix(tab)),
        row = rep(1:nrow(tab), ncol(tab)),
        col = rep(1:ncol(tab), each=nrow(tab))
    )
    df$label = paste0(floor(100*df$val), '%')
    df$val = pmax(0,df$val)
    
    
    print(ggplot(df, aes(col,row,fill=val)) + geom_tile() + geom_text(aes(label=label)) +
        scale_fill_gradient(low=BAD.COLOR, high=GOOD.COLOR, limits=c(0,1)))
}


# for Todd to execute 
if (1==2)
{
    source('code/processing/pretty_table/make_pretty_table.R')
    source('code/processing/pretty_table/pretty_table_colors.R')
    
    tab = read.csv('results/prep/Figure_2_Updated.csv')
    tab = tab[,-1]
    
    
    pretty = apply(tab, 2, function(x){
        paste0(floor(100*x), '%')
    })
    
    
    write.shaded.table(tab=pretty, color.by=tab, 
                       file='results/prep/Figure_2_shaded.xlsx',
                       thresholds=c(0,1),
                       lower.threshold.colors = PRETTY.ERROR.COLORS['bad'],
                       upper.threshold.colors = PRETTY.ERROR.COLORS['good'])
}