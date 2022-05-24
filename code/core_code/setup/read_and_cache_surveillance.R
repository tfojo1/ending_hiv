
#source('../code/data_managers/hiv_surveillance_manager.R')
#  There's some strange error when I source this remotely. Load it up and click source
library(jheem)
load('cached/DEFAULT.LOCALE.MAPPING.Rdata')
source('code/core_code/data_managers/locale_mappings.R')
source('code/core_code/data_managers/prep_manager_2.R')
source('code/core_code/data_managers/hiv_surveillance_manager_reader.R')


#-- START WITH COUNTY-LEVEL --#
county.surveillance = read.county.surveillance.manager()
county.surveillance = add.all.local.county.data(county.surveillance)
county.surveillance = finalize.surveillance.manager(county.surveillance)
save(county.surveillance, file='cached/county.surveillance.Rdata')

#-- MSA --#
##Estimated, corrected to county
msa.surveillance = read.msa.surveillance.manager(use.adjusted.estimate = T, verbose=T)
bk = msa.surveillance
msa.surveillance = aggregate.surveillance.race.as.bho(msa.surveillance)
msa.surveillance = aggregate.surveillance.other.risk.as.heterosexual(msa.surveillance)
msa.surveillance = correct.to.county.totals(msa.surveillance,
                                            county.surveillance=county.surveillance)
prep.manager = read.prep.manager()
msa.surveillance = add.prep.data(msa.surveillance, prep.manager)

msa.surveillance = add.all.local.msa.data(msa.surveillance)

bk2 = msa.surveillance
msa.surveillance = import.one.county.msa.data(msa.surveillance,
                                              geography='msa',
                                              county.surveillance = county.surveillance)

msa.surveillance = read.testing.from.brfss(msa.surveillance,
                                           geography = 'msa',
                                           dir='Q:JHEEM/large_cleaned_data/brfss/brfss_msa/',
                                           include.only.at.risk = F,
                                           use.brfss.weights = T)


msa.surveillance = finalize.surveillance.manager(msa.surveillance)
#save(msa.surveillance, file='cached_surveillance/msa.surveillance.estimated.correct.to.county.Rdata')
save(msa.surveillance, file='cached/msa.surveillance.Rdata')



##-- STATE --##

state.surveillance = read.state.surveillance.manager()
state.surveillance = add.all.local.state.data(state.surveillance)

state.surveillance = read.testing.from.brfss(state.surveillance,
                                             geography='state',
                                             dir='Q:JHEEM/large_cleaned_data/brfss/brfss_state',
                                             include.only.at.risk = F,
                                             use.brfss.weights = T)

state.surveillance = finalize.surveillance.manager(state.surveillance)
save(state.surveillance, file='cached/state.surveillance.Rdata')


#testing
if (1==2)
{
    state.surveillance = read.state.surveillance.manager(test=T)
    state.surveillance = add.all.local.state.data(state.surveillance)
    
    get.surveillance.data(state.surveillance, 'OH', 'suppression', get.source=T)
    
    get.surveillance.data.details(state.surveillance, 'OH', 'suppression')
}
