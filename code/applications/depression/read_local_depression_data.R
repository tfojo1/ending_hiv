

# return a a list with three elements,
# each element is an array indexed [year, substate regtion+state code, age]
# where the codes are generated with the function get.substate.region.codes
# $prevalence
# $prevalence.lower
# $prevalence.upper

#Example dir
dir = c("~/Documents/JHU/HIV Compartmental Model/Depression/map_data_2010_2012.csv","~/Documents/JHU/HIV Compartmental Model/Depression/map_data_2012_2014.csv","~/Documents/JHU/HIV Compartmental Model/Depression/map_data_2014_2016.csv","~/Documents/JHU/HIV Compartmental Model/Depression/map_data_2016_2018.csv")
read.depression.prevalence <- function(dir)
{
  #Read dir
  map_data = vector("list", length = length(dir))
  #Extract year and read csvs
  year = vector(length = length(dir))
  for (a in 1:length(dir)){
    map_data[[a]] = read.csv(dir[a])
    year[a] = map_data[[a]]$years[1]
  }
  #Extract age 
  age = unique(map_data[[1]]$age_group)
  
  #Extract region 
  
  states = cbind(state.name,state.abb)
  geography = as.character(unique(map_data[[1]]$geography))
  rexp <- "^(\\w+)\\s?(.*)$"
  geography_split = cbind(sub(rexp,"\\1",geography),sub(rexp,"\\2",geography))
  geography_split[,1] <-states[match(geography_split[,1],states[,1]),2]
  geography_split = as.data.frame( geography_split)
  geography_split <-geography_split %>% mutate_all(na_if,"")
  geography_split = geography_split[!is.na(geography_split[,1]),]
  geography_split = geography_split[!is.na(geography_split[,2]),]
  codes = state.and.region.name.to.nsduh.region.code(geography_split[,1],geography_split[,2]) #proper mapping to code? not all region codes split properly 
  
  #Create empty array 

  
  dim.names = list(year=year,
                   geography = geography,
                   age=age) 
  dim(rv$prevalence) = sapply(dim.names, length)
  dimnames(rv$prevalence) = dim.names
  dim(rv$prevalence.upper) = sapply(dim.names, length)
  dimnames(rv$prevalence.upper) = dim.names
  dim(rv$prevalence.lower) = sapply(dim.names, length)
  dimnames(rv$prevalence.lower) = dim.names
  
  for(a in 1:length(year)){
    for(b in 1:length(geography)){
      for(c in 1:length(age)){
        matrix = map_data[which(map_data$geography == geography[a] && map_data$age_group == age[b]),]

      }
    }
    
  }
  
  return(rv)

}
