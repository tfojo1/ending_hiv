

# return a a list with three elements,
# each element is an array indexed [year, substate regtion+state code, age]
# where the codes are generated with the function get.substate.region.codes
# $prevalence
# $prevalence.lower
# $prevalence.upper
load("/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/cached/DEFAULT.LOCALE.MAPPING.Rdata")
dir = c("~/Documents/JHU/HIV Compartmental Model/Depression/map_data_2010_2012.csv","~/Documents/JHU/HIV Compartmental Model/Depression/map_data_2012_2014.csv","~/Documents/JHU/HIV Compartmental Model/Depression/map_data_2014_2016.csv","~/Documents/JHU/HIV Compartmental Model/Depression/map_data_2016_2018.csv")
read.depression.prevalence <- function(dir)
{
  #Read dir
  map_data = vector("list", length = length(dir))
  #Extract year and read csvs
  year = vector(length = length(dir))
  for (a in 1:length(dir)){
    map_data[[a]] = read.csv(dir[a])[-c(1:20),]
    year[a] = as.character(map_data[[a]]$years[1])
  }
  

  #Extract age 
  age = unique(map_data[[1]]$age_group)
  
  #Extract region 
  
  states = cbind(state.name,state.abb)
  geography = as.character(unique(map_data[[1]]$geography))
  rexp <- "^(\\w+)\\s?(.*)$"
  geography_split = cbind(sub(rexp,"\\1",geography),sub(rexp,"\\2",geography))
  geography_split_2nd = sub(rexp,"\\1",geography_split[,2])
  geography_split_3rd = sub(rexp,"\\2",geography_split[,2])
  
  #For DC 
  geography_split_DC = sub(rexp,"\\2",geography_split_3rd)
  
  #Fix multiword state names
  geography_split[which(geography_split[,1] == "District"),1] <- "District of Columbia"
  geography_split[which(geography_split[,1] == "District of Columbia"),2] <- geography_split_DC[which(geography_split[,1] == "District of Columbia")]
  geography_split[which(geography_split[,1] == "West"),1] <- "West Virginia"
  geography_split[which(geography_split[,1] == "West Virginia"),2] <- geography_split_3rd[which(geography_split[,1] == "West Virginia")]
  geography_split[which(geography_split[,1] == "Rhode"),1] <- "Rhode Island"
  geography_split[which(geography_split[,1] == "Rhode Island"),2] <- geography_split_3rd[which(geography_split[,1] == "Rhode Island")]
  geography_split[which(geography_split_2nd == "Hampshire"),1] <- "New Hampshire"
  geography_split[which(geography_split[,1] == "New Hampshire"),2] <- geography_split_3rd[which(geography_split[,1] == "New Hampshire")]
  geography_split[which(geography_split_2nd == "Jersey"),1] <- "New Jersey"
  geography_split[which(geography_split[,1] == "New Jersey"),2] <- geography_split_3rd[which(geography_split[,1] == "New Jersey")]
  geography_split[which(geography_split_2nd == "York"),1] <- "New York"
  geography_split[which(geography_split[,1] == "New York"),2] <- geography_split_3rd[which(geography_split[,1] == "New York")]
  geography_split[which(geography_split_2nd == "Mexico"),1] <- "New Mexico"
  geography_split[which(geography_split[,1] == "New Mexico"),2] <- geography_split_3rd[which(geography_split[,1] == "New Mexico")]
  geography_split[intersect(which(geography_split_2nd == "Carolina"),which(geography_split[,1] == "North")),1] <- "North Carolina"
  geography_split[which(geography_split[,1] == "North Carolina"),2] <- geography_split_3rd[which(geography_split[,1] == "North Carolina")]
  geography_split[intersect(which(geography_split_2nd == "Carolina"),which(geography_split[,1] == "South")),1] <- "South Carolina"
  geography_split[which(geography_split[,1] == "South Carolina"),2] <- geography_split_3rd[which(geography_split[,1] == "South Carolina")]
  geography_split[intersect(which(geography_split_2nd == "Dakota"),which(geography_split[,1] == "North")),1] <- "North Dakota"
  geography_split[which(geography_split[,1] == "North Dakota"),2] <- geography_split_3rd[which(geography_split[,1] == "North Dakota")]
  geography_split[intersect(which(geography_split_2nd == "Dakota"),which(geography_split[,1] == "South")),1] <- "South Dakota"
  geography_split[which(geography_split[,1] == "South Dakota"),2] <- geography_split_3rd[which(geography_split[,1] == "South Dakota")]

  
  geography_split[,1] <-state.name.to.abbreviation(geography_split[,1])
  geography_split = as.data.frame(geography_split)
  geography_split <-geography_split %>% mutate_all(na_if,"")
  
  geography = geography[!is.na(geography_split[,2])]
  geography_split = geography_split[!is.na(geography_split[,2]),]
  
  codes = state.and.region.name.to.nsduh.region.code(geography_split[,1],geography_split[,2]) 
   
  #Create empty array 
  rv = list(
    prevalence = array(NA, dim = c(length(year),length(age),length(codes))),
    prevalence.lower = array(NA, dim = c(length(year),length(age),length(codes))),
    prevalence.upper = array(NA, dim = c(length(year),length(age),length(codes)))
  )
  dim.names = list(year=year,
                   codes = codes,
                   age=age) 
  dim(rv$prevalence) = sapply(dim.names, length)
  dimnames(rv$prevalence) = dim.names
  dim(rv$prevalence.upper) = sapply(dim.names, length)
  dimnames(rv$prevalence.upper) = dim.names
  dim(rv$prevalence.lower) = sapply(dim.names, length)
  dimnames(rv$prevalence.lower) = dim.names
  
  for(a in 1:length(year)){
    map_data_yr = map_data[[a]]
    for(b in 1:length(codes)){
      for(c in 1:length(age)){
        data = map_data_yr[intersect(which(map_data_yr$age_group == age[c]),which(map_data_yr$geography == geography[b])),]
        if(dim(data)[1]>0){
          rv$prevalence[a,b,c] = data[,5]
          rv$prevalence.lower[a,b,c] = data[,6]
          rv$prevalence.upper[a,b,c] = data[,7]
        }
        
      }
    }
    
  }
  
  return(rv)

}
