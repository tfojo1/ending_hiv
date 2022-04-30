df = rbind(read.csv('raw_data/covid/2020_US_Region_Mobility_Report.csv', stringsAsFactors = F),
           read.csv('raw_data/covid/2021_US_Region_Mobility_Report.csv', stringsAsFactors = F))

dim(df)

source('code/source_code.R')
source('code/targets/target_msas.R')
county.names(counties.for.msa(BALTIMORE.MSA))
county = 24510 #baltimore city

mask = !is.na(df$census_fips_code) & df$census_fips_code==county; sum(mask, na.rm=T)

reductions = c("retail_and_recreation_percent_change_from_baseline",
               "grocery_and_pharmacy_percent_change_from_baseline",
               "parks_percent_change_from_baseline",
               "transit_stations_percent_change_from_baseline",
               "workplaces_percent_change_from_baseline",      
                "residential_percent_change_from_baseline")
plot.df = melt(df[mask,c('date',reductions)], id.vars = 'date')

ggplot(plot.df, aes(date, value)) + geom_point() + 
    geom_hline(yintercept = 0, linetype='dotted') +
    facet_wrap(~variable)


loc.df = df[mask,c('date',reductions)]

loc.df$month = as.numeric(gsub('\\d\\d\\d\\d-(\\d\\d)-\\d\\d', '\\1', loc.df$date))
loc.df$year = as.numeric(gsub('(\\d\\d\\d\\d)-\\d\\d-\\d\\d', '\\1', loc.df$date))


months.of.interest = c(3:12,1:6)
years.of.interest = c(rep(2020,10), rep(2021,6))

mean.monthly.changes = sapply(1:length(months.of.interest), function(i){
    mask = loc.df$month == months.of.interest[i] & loc.df$year == years.of.interest[i]
    colMeans(loc.df[mask,reductions])
})
dimnames(mean.monthly.changes)[[2]] = paste0(months.of.interest, '-', years.of.interest)
dimnames(mean.monthly.changes)[[2]] = years.of.interest + (months.of.interest-1)/12

monthly.plot.df = melt(mean.monthly.changes)

ggplot(monthly.plot.df, aes(x=Var2, y=value)) + geom_line() + geom_point() + 
    geom_hline(yintercept = c(0,-100,100), linetype='dashed') +
    facet_wrap(~Var1) 

library(scales)
synthesized.df = (mean.monthly.changes/mean.monthly.changes[,2])[-3,]
synthesized.df = rbind(synthesized.df, total=colMeans(synthesized.df))
monthly.plot.df = melt(synthesized.df)
ggplot(monthly.plot.df, aes(x=Var2, y=value)) + geom_line() + geom_point() + 
    geom_hline(yintercept = c(0,1), linetype='dashed') +
    facet_wrap(~Var1) + scale_y_continuous(labels = percent) +
    scale_x_continuous(breaks=c(2020.167,2020.583,2021.0,2021.417), 
                       labels=c('March 1, 2020', 'Aug 1, 2020', 'Jan 1, 2021', 'June 1, 2021')) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
