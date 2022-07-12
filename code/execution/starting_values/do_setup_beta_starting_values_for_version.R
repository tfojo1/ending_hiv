

source('code/source_code.R')
source('code/execution/starting_values/systematic_find_starting_values.R')

if (COMPUTER.LOCATION != 'desktop')
    stop("Run this on the big desktop")
    
version = 'expanded_1.0'
locations = TARGET.MSAS

prepare.beta.start.values(version=version, locations=locations)
