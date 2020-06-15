

run.testing.regressions <- function(dir='cleaned_data',
                                   verbose=T,
                                   anchor.year=2020)
{
    dfs = read.nhbs.testing(dir=file.path(dir, 'continuum/national/testing'), verbose=verbose)
    brfss.msa = read.brfss.msa(file.path(dir, 'continuum/msa/BRFSS_msa.csv'))
    
    #The total regression
    df.msa = dfs$msa
    df.msa$brfss.frac.ever = sapply(1:dim(df.msa)[1], function(i){
        msa = df.msa$msa[i]
        year = df.msa$year[i]
        
        msa.mask = brfss.msa$msa==msa
        if (!any(msa.mask))
        {
            print(paste0("No matches in BRFSS data for msa '", msa.names(msa), "'"))
            return(NA)
#            stop("No matches in BRFSS data for msa '", msa.names(msa), "'")
        }
        
        brfss.subset = brfss.msa[msa.mask,]
        
        year.diff = abs(as.numeric(year)-brfss.subset$year)
        match.year = max(brfss.subset$year[year.diff==min(year.diff)])
        mask = brfss.subset$year==match.year
        
        brfss.subset$frac.ever[mask][1]
    })
    
    
    df.all = dfs$all
    df.all$rate = -log(1-df.all$frac.12mo)
    
    #massage our variables
    df.all$race = factor(df.all$race, levels = c('all','black','hispanic','other'))
    df.all$age = factor(df.all$age, levels=c('all',paste0('age',1:5)))
    df.all$sex = factor(df.all$sex, levels=c('all','female','male'))
    df.all$year = df.all$year-anchor.year
    df.all$outcome = log(df.all$rate)
    
    #add brfss data
    
    fit = lm(log(rate) ~ risk + year + race + age + sex, data=df.all)
}

read.brfss.msa <- function(file='cleaned_data/continuum/msa/BRFSS_msa.csv')
{
    df = read.csv(file, stringsAsFactors = F)
    df = df[grepl('hiv', df$Question, ignore.case = T), ]
    df = df[df$Response=='Yes',]
    
    
    msa.n = msa.names(df$LocationID)
    mask = !is.na(msa.n)
    
    rv = data.frame(msa=cbsa.for.msa.name(msa.n[mask]),
               year=df$ï..Year[mask],
               frac.ever = df$Data_value[mask]/100)
    rv[!is.na(rv$msa),]
}

read.brfss.state <- function(file='cleaned_data/continuum/msa/')
{
    
}

read.nhbs.testing <- function(dir='cleaned_data/continuum/national/testing/',
                             verbose=T)
{
    files = list.files(dir)
    year = as.numeric(substr(files, 1, 4))
    risk = rep('heterosexual', length(files))
    risk[grepl('msm', files, ignore.case = T)] = 'msm'
    risk[grepl('idu', files, ignore.case = T)] = 'idu'
    
    all.dfs = lapply(1:length(files), function(i){
        if (verbose)
            print(paste0("Reading NHBS file on testing for ", year[i]))
        read.nhbs.testing.file(file.path(dir, files[i]), year=year[i], risk=risk[i])
    })
    
    if (verbose)
        print("Joining NHBS testing files")
    
    rv = all.dfs[[1]]
    if (length(all.dfs)>1)
    {
        for (i in 2:length(all.dfs))
        {
            for (name in names(rv))
            {
#                print(i)
#                print(name)
                rv[[name]] = rbind(rv[[name]], all.dfs[[i]][[name]])
            }
        }
    }
    
    rv$all = rbind(rv$race, rv$age, rv$sex)
    
    rv
}

read.nhbs.testing.file <- function(file,
                                   year,
                                   risk)
{
    df = read.csv(file, stringsAsFactors = F)
    
    for (i in 2:dim(df)[2])
    {
        df[,i] = gsub(',', '', df[,i])
        df[!is.na(df[,i]) & df[,i] == '', i] = NA
        df[,i] = as.numeric(df[,i])
    }
    
    N = dim(df)[1]
    header.rows = (1:N)[is.na(df$n.ever) & is.na(df$n.12mo)]
    total.row = (1:N)[grepl('Total', df[,1], ignore.case = T)][1]
    
    #-- Age --#
    age.index = (1:N)[grepl('age', df[,1], ignore.case = T)][1]
    last.age.index = header.rows[header.rows>first.age.index][1]-1
    
    if ((last.age.index-age.index)==6)
    {
        age1.indices = age.index + 1:2
        age1.mult = c(1,1)
        age2.indices = age.index + 3:4
        age2.mult = c(1,.5)
        age3.indices = age.index + 4:5
        age3.mult = c(.5, .5)
        age4.indices = age.index + 5:6
        age4.mult = c(.5, .25)
        age5.indices = age.index + 6
        age5.mult = .75
    }
    else
    {
        age1.indices = age.index + 1
        age1.mult = 1
        age2.indices = age.index + 2:3
        age2.mult = c(1,.5)
        age3.indices = age.index + 3:4
        age3.mult = c(.5, .5)
        age4.indices = age.index + 4:5
        age4.mult = c(.5, .25)
        age5.indices = age.index + 5
        age5.mult = .75
    }
    
    denominators = c(sum(df$n.total[age1.indices] * age1.mult),
                     sum(df$n.total[age2.indices] * age2.mult),
                     sum(df$n.total[age3.indices] * age3.mult),
                     sum(df$n.total[age4.indices] * age4.mult),
                     sum(df$n.total[age5.indices] * age5.mult))
    numerators.12 = c(sum(df$n.12mo[age1.indices] * age1.mult),
                      sum(df$n.12mo[age2.indices] * age2.mult),
                      sum(df$n.12mo[age3.indices] * age3.mult),
                      sum(df$n.12mo[age4.indices] * age4.mult),
                      sum(df$n.12mo[age5.indices] * age5.mult))
    numerators.ever = c(sum(df$n.ever[age1.indices] * age1.mult),
                        sum(df$n.ever[age2.indices] * age2.mult),
                        sum(df$n.ever[age3.indices] * age3.mult),
                        sum(df$n.ever[age4.indices] * age4.mult),
                        sum(df$n.ever[age5.indices] * age5.mult))
    
    df.age = data.frame(n=denominators,
                        frac.12mo=numerators.12/denominators,
                        frac.ever=numerators.ever/denominators,
                        year=year,
                        age=paste0('age',1:5),
                        race='all',
                        sex='all',
                        risk=risk,
                        msa='all')
    
    
    
    #-- Race--#
    first.race.index = (1:N)[grepl('race', df[,1], ignore.case = T)][1]+1
    last.race.index = header.rows[header.rows>first.race.index][1]-1
    
    black.index = (first.race.index:last.race.index)[grepl('black', df[first.race.index:last.race.index,1], ignore.case = T)]
    hispanic.index = (first.race.index:last.race.index)[grepl('hispanic', df[first.race.index:last.race.index,1], ignore.case = T)]
    other.indices = setdiff(first.race.index:last.race.index, c(black.index, hispanic.index))
    
    denominators = c(df$n.total[black.index], df$n.total[hispanic.index], sum(df$n.total[other.indices]))
    numerators.12 = c(df$n.12mo[black.index], df$n.12mo[hispanic.index], sum(df$n.12mo[other.indices]))
    numerators.ever = c(df$n.ever[black.index], df$n.ever[hispanic.index], sum(df$n.ever[other.indices]))
    
    
    df.race = data.frame(n=denominators,
                        frac.12mo=numerators.12/denominators,
                        frac.ever=numerators.ever/denominators,
                        year=year,
                        age='all',
                        race=c('black','hispanic','other'),
                        sex='all',
                        risk=risk,
                        msa='all')
    
    #Sex
    if (risk=='msm')
        df.sex = data.frame(n=df$n.total[total.row],
                             frac.12mo=df$n.12mo[total.row]/df$n.total[total.row],
                             frac.ever=df$n.ever[total.row]/df$n.total[total.row],
                             year=year,
                             age='all',
                             race='all',
                             sex='male',
                             risk=risk,
                             msa='all')
    else if (any(names(df)=='n.ever.male'))
        df.sex = data.frame(n=c(df$n.total.female[total.row], df$n.total.male[total.row]),
                             frac.12mo = c(df$n.12mo.female[total.row], df$n.12mo.male[total.row])/c(df$n.total.female[total.row], df$n.total.male[total.row]),
                             frac.ever = c(df$n.ever.female[total.row], df$n.ever.male[total.row])/c(df$n.total.female[total.row], df$n.total.male[total.row]),
                             year=year,
                             age='all',
                             race='all',
                            sex=c('female','male'),
                             risk=risk,
                             msa='all')
    else
    {
        female.mask = grepl('female', df[,1], ignore.case = T)
        male.mask = grepl('male', df[,1], ignore.case = T) & !female.mask
        
        df.sex = data.frame(n=c(df$n.total[female.mask], df$n.total[male.mask]),
                            frac.12mo=c(df$n.12mo[female.mask], df$n.12mo[male.mask])/c(df$n.total[female.mask], df$n.total[male.mask]),
                            frac.ever=c(df$n.ever[female.mask], df$n.ever[male.mask])/c(df$n.total[female.mask], df$n.total[male.mask]),
                            year=year,
                            age='all',
                            race='all',
                            sex=c('female','male'),
                            risk=risk,
                            msa='all')
    }
    
    #Risk
    df.risk = data.frame(n=df$n.total[total.row],
                        frac.12mo=df$n.12mo[total.row]/df$n.total[total.row],
                        frac.ever=df$n.ever[total.row]/df$n.total[total.row],
                        year=year,
                        age='all',
                        race='all',
                        sex='all',
                        risk=risk,
                        msa='all')
    
    #MSA
    first.msa.index = (1:N)[grepl('Metropolitan', df[,1], ignore.case = T) | grepl("City", df[,1], ignore.case = T)][1]+1
    last.msa.index = total.row-1
    msas = cbsa.for.msa.name(gsub('â€“', '-', df[first.msa.index:last.msa.index,1]))
    msa.indices = (first.msa.index:last.msa.index)[!is.na(msas)]
    msas = msas[!is.na(msas)]
    
    df.msa = data.frame(n=df$n.total[msa.indices],
                        frac.12mo=df$n.12mo[msa.indices]/df$n.total[msa.indices],
                        frac.ever=df$n.ever[msa.indices]/df$n.total[msa.indices],
                        year=year,
                        age='all',
                        race='all',
                        sex='all',
                        risk=risk,
                        msa=msas,
                        stringsAsFactors = i
                        )
    
    list(race=df.race,
         age=df.age,
         sex=df.sex,
         risk=df.risk,
         msa=df.msa)
}