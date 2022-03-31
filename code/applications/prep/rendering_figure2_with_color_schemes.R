

#For Figure 2
BAD.COLOR = 'red2'
GOOD.COLOR = 'green2'



library(reshape2)
source('code/processing/pretty_table/make_pretty_table.R')

TEST.COLORS = T

# for picking the color
if (TEST.COLORS)
{
   #read your table here
   
    file = 'code/Ruchita/Figure_2_Updated.csv' #change as needed
    tab_figure2 = read.csv(file)
    tab_figure2 = tab_figure2[,-1]
    
    tab_disparity = black.disparity.ratios
    
    plot.shaded.table(round(tab,2), thresholds = c(0,.6), 
                      colors = c(BAD.COLOR, GGOOD.COLOR))
    
    plot.shaded.table(round(tab_figure2,2), thresholds = c(0,.6,1), 
                      colors = c(BAD.COLOR,GOOD.COLOR,GOOD.COLOR))
    
    
    plot.shaded.table(round(tab_disparity,2), thresholds = c(1,8), 
                      colors = c(GOOD.COLOR,BAD.COLOR))
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