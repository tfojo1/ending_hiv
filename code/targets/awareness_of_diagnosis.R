
source('code/source_code.R')

target.counties = get.target.counties()
target.county.names = county.names(target.counties)

df = read.csv('../data2/HIV_Surveillance/by_msa/ethe_counties_awareness_of_diagnosis.csv',
                stringsAsFactors = F)
df$just.county = gsub('(^.+),.+$', '\\1', df$Geography)
df$fips = sapply(df$just.county, function(county){
    mask = grepl(county, target.county.names, ignore.case = T)  
    if (any(mask))
        target.counties[mask][1]
    else
        NA
})
df$msa = msa.for.county(df$fips)

df$frac.aware = as.numeric(substr(df$Percent..95..CI.RSE., 1, 4))/100
df$msa.frac.aware = sapply(1:dim(df)[1], function(i){
    if (is.na(df$msa[i]))
        NA
    else
        get.surveillance.data(location.codes=df$msa[i], years = df$Year[i], data.type='diagnosed')
})

head(df)

qplot(df$fraeate.aware, df$msa.frac.aware) + geom_abline(slope=1, intercept=0) +
    xlab("Fraction Aware by State-Level Data") + ylab("Fraction Aware by EtHE County") + xlim(.75,1) + ylim(.75,1) +
    theme(text=element_text(size=20))
range(df$frac.aware-df$msa.frac.aware, na.rm=T)
qplot(df$frac.aware-df$msa.frac.aware) + xlab("EtHE minus State-Level Fraction Aware")+
    theme(text=element_text(size=20))

df$diff = (df$frac.aware - df$msa.frac.aware)
o = order(abs(df$diff), decreasing = T)
df[o,c('Year','Geography','frac.aware', 'msa.frac.aware','diff')]
df[o,c('Year','Geography','frac.aware', 'msa.frac.aware','diff')][1:10,]

df$year.to.year.diff = NA
df$year.to.year.diff[-1] = df$frac.aware[-1] - df$frac.aware[1:(dim(df)[1]-1)]
df$year.to.year.diff[(1:dim(df)[1])%%2==1]=df$year.to.year.diff[(1:dim(df)[1])%%2==0]

o = order(abs(df$year.to.year.diff), decreasing=T)
df[o,c('Year','Geography','frac.aware', 'msa.frac.aware','year.to.year.diff')]
