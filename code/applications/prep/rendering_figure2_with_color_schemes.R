
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
                      colors = c(BAD.COLOR, GOOD.COLOR))
    
    plot.shaded.table(round(tab,2), thresholds = c(0,.5,1), 
                      colors = c(BAD.COLOR,GOOD.COLOR,'green4'))
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
                       thresholds = c(0,.6,1), 
                       colors = c('red2','green2','green4'))
    
    
    
    tab.black=read.csv('results/prep/black.disparity.ratios.csv')
    tab.black = tab.black[,-1]
    pretty.b = format(round(tab.black,1), nsmall=1)
    
    plot.shaded.table(tab=pretty.b, color.by=tab.black, 
                       thresholds = c(1,8), 
                       colors = c('green2','red2'))
    write.shaded.table(tab=pretty.b, color.by=tab.black, 
                       file='results/prep/disparities_black_shaded.xlsx',
                       thresholds = c(1,8), 
                       colors = c('green2','red2'))
    
    
    
    tab.hisp=read.csv('results/prep/hispanic.disparity.ratios.csv')
    tab.hisp = tab.hisp[,-1]
    pretty.h = format(round(tab.hisp,1), nsmall=1)
    
    plot.shaded.table(tab=pretty.h, color.by=tab.hisp, 
                      thresholds = c(1,8), 
                      colors = c('green2','red2'))
    write.shaded.table(tab=pretty.h, color.by=tab.hisp, 
                       file='results/prep/disparities_hispanic_shaded.xlsx',
                       thresholds = c(1,8), 
                       colors = c('green2','red2'))
}