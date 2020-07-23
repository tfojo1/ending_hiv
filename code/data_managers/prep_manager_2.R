
#The data based on zip3s is good enough

read.prep.manager <- function(dir='cleaned_data/prep/')
{
    files = list.files(file.path(dir, 'county'))
    year = substr(files, 1, 4)
    
    
}

read.prep.file <- function(file)
{
    df = read.csv(file, stringsAsFactors = F)
    
    rate.to.numeric = function(r){rv=as.numeric(r); rv[rv==-1]=NA; rv}
    
    rv = data.frame(location=df$GEO.ID,
                    male=rate.to.numeric(df$Male.PrEP.Users),
                    female=rate.to.numeric(df$Female.PrEP.Users),
                    age1=rate.to.numeric(df$Age.LE.24.PrEP.Users),
                    age2=rate.to.numeric(df$Age.25.34.PrEP.Users),
                    age3=rate.to.numeric(df$Age.35.44.PrEP.Users),
                    age4=rate.to.numeric(df$Age.45.54.PrEP.Users),
                    age5=rate.to.numeric(df$Age.55..PrEP.Users))
}