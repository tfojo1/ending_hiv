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
