
BAD.COLOR = 'red2'
GOOD.COLOR = 'green2'

library(reshape2)
source('code/processing/pretty_table/make_pretty_table.R')

TEST.COLORS = T

# for picking the color
if (TEST.COLORS)
{
   #read your table here
   
    file = 'results/prep/Figure_2_Updated.csv' #change as needed
    tab = read.csv(file)
    tab = tab[,-1]
    
    plot.shaded.table(round(tab,2), thresholds = c(0,.6), 
                      lower.threshold.colors = c(BAD.COLOR),
                      upper.threshold.colors = c(GOOD.COLOR))
    
    plot.shaded.table(round(tab,2), thresholds = c(0,.5,1), 
                      lower.threshold.colors = c(BAD.COLOR,GOOD.COLOR),
                      upper.threshold.colors = c(GOOD.COLOR,'green4'))
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