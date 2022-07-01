

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
  age = unique(map_data$age_group)
  #Extract region 
  geography = unique(map_data$geography)
  codes = get.substate.region.codes(geography) #proper mapping to code? cannot find function
  
  rv = array(0, dim = c(length(year),length(age),length(geography)))
  dim.names = list(year=year,
                   geography = geography,
                   age=age) 
  dim(rv) = sapply(dim.names, length)
  dimnames(rv) = dim.names
  for(a in 1:length(year)){
    for(b in 1:length(geography)){
      for(c in 1:length(age)){
        matrix = map_data[which(map_data$geography == geography[a] && map_data$age_group == age[b]),]
        rv[a,b,c] = matrix[,c(5:7)] #How to specify length of matrix in third element of array? (1X3 matrix)
        
      }
    }
    
  }
  
  return(rv)

}



get.substate.region.codes <- function(state.abbreviations,
                                      regions)
{
    paste0(state.abbreviations, "_", regions)
}