

# return a a list with three elements,
# each element is an array indexed [year, substate regtion+state code, age]
# where the codes are generated with the function get.substate.region.codes
# $prevalence
# $prevalence.lower
# $prevalence.upper
#load("/Users/Ruchita/Documents/JHU/HIV Compartmental Model/ending_hiv/cached/DEFAULT.LOCALE.MAPPING.Rdata")

library(dplyr)

read.depression.prevalence <- function(dir = 'code/applications/depression/local_depression_data/')
{
  #Read dir
  files = file.path(dir, list.files(dir))
  map_data = vector("list", length = length(files))
  year = vector(length = length(files))
  for (a in 1:length(files)){
    map_data[[a]] = read.csv(files[a], stringsAsFactors = F)[-c(1:20),]
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
        data[data=='suppressed'] = NA
        if(dim(data)[1]>0){
          rv$prevalence[a,b,c] = as.numeric(data[,5])
          rv$prevalence.lower[a,b,c] = as.numeric(data[,6])
          rv$prevalence.upper[a,b,c] = as.numeric(data[,7])
        }
        
      }
    }
    
  }
  
  print("for now we are stripping out unmatched codes - we need to come back and fix this")
  rv$prevalence = rv$prevalence[,!is.na(codes),]
  rv$prevalence.lower = rv$prevalence.lower[,!is.na(codes),]
  rv$prevalence.upper = rv$prevalence.upper[,!is.na(codes),]
  
  return(rv)

}

get.msa.depression.prevalence.under.26 <- function(msa,
                                                   year.lower,
                                                   year.upper,
                                                   local.data=LOCAL.DEPRESSION.DATA,
                                                   census=ALL.DATA.MANAGERS$census.full)
{
    # for each county in the MSA
    # get the prevalences that correspond to the NSDUH region the county falls in
    # aggregate population weighted
    
    year.range.label = paste0(year.lower, '-', substr(as.character(year.upper+1), 3, 4))
    counties = counties.for.msa(msa)
    nsduh.regions.for.counties = ndsuh.substate.regions.for.counties(counties)
    if (any(is.na(nsduh.regions.for.counties)))
        stop(paste0("Cannot find the NSDUH substate regions for these counties: ",
                    paste0("'", counties[is.na(nsduh.regions.for.counties)], "'", collapse=', ')))
    
    missing.codes = setdiff(nsduh.regions.for.counties, dimnames(local.data$prevalence)$code)
    if (length(missing.codes)>0)
        stop(paste0("Missing local depression data for NSDUH region(s):",
                    paste0("'", missing.codes, "'", collapse=', ')))
    
    # get the prevalences
    prevalence.12.to.17 = local.data$prevalence[year.range.label, nsduh.regions.for.counties, '12 to 17']
    prevalence.18.to.25 = local.data$prevalence[year.range.label, nsduh.regions.for.counties, '18 to 25']
    
    # get the sds
    log.sds.12.to.17 = (log(local.data$prevalence.upper[year.range.label, nsduh.regions.for.counties, '12 to 17']) -
                            log(local.data$prevalence.lower[year.range.label, nsduh.regions.for.counties, '12 to 17'])) /
        2 / qnorm(.975)
    log.mean.12.to.17 = (log(local.data$prevalence.upper[year.range.label, nsduh.regions.for.counties, '12 to 17']) +
                             log(local.data$prevalence.lower[year.range.label, nsduh.regions.for.counties, '12 to 17'])) / 2
    var.12.to.17 = (exp(log.sds.12.to.17^2) - 1) * exp(2*log.mean.12.to.17 + log.sds.12.to.17^2)
    
    log.sds.18.to.25 = (log(local.data$prevalence.upper[year.range.label, nsduh.regions.for.counties, '18 to 25']) -
                            log(local.data$prevalence.lower[year.range.label, nsduh.regions.for.counties, '18 to 25'])) /
        2 / qnorm(.975)
    log.mean.18.to.25 = (log(local.data$prevalence.upper[year.range.label, nsduh.regions.for.counties, '18 to 25']) +
                             log(local.data$prevalence.lower[year.range.label, nsduh.regions.for.counties, '18 to 25'])) / 2
    var.18.to.25 = (exp(log.sds.18.to.25^2) - 1) * exp(2*log.mean.18.to.25 + log.sds.18.to.25^2)
    
    
    
    population.12.to.17 = get.census.data(census, years=year.lower:year.upper, 
                                          fips=counties, ages = 12:17, 
                                          aggregate.years = T, aggregate.ages = T,
                                          aggregate.races = T, aggregate.sexes = T)
    population.18.to.25 = get.census.data(census, years=year.lower:year.upper, 
                                          fips=counties, ages = 18:25, 
                                          aggregate.years = T, aggregate.ages = T,
                                          aggregate.races = T, aggregate.sexes = T)
    
    prevalence = sum(prevalence.12.to.17 * population.12.to.17 + prevalence.18.to.25 * population.18.to.25) / sum(population.12.to.17 + population.18.to.25)
    variance = sum(var.12.to.17 * population.12.to.17^2 + var.18.to.25 * population.18.to.25^2) / sum(population.12.to.17 + population.18.to.25)^2
    
    list(prevalence = prevalence,
         sd = sqrt(variance))
}

get.msa.depression.prevalence.over.26 <- function(msa,
                                                   year.lower,
                                                   year.upper,
                                                  local.data=LOCAL.DEPRESSION.DATA,
                                                   census=ALL.DATA.MANAGERS$census.full)
{
    
}

LOCAL.DEPRESSION.DATA = read.depression.prevalence()