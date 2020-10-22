
##-----------------##
##-- SOURCE CODE --##
##-----------------##

library(jheem)
source('../code/data_managers/census_manager.R')
source('../code/data_managers/mortality_manager.R')
source('../code/data_managers/natality_manager.R')
source('../code/data_managers/idu_manager.R')
source('../code/data_managers/pairing_manager.R')
source('../code/data_managers/prep_manager.R')
source('../code/data_managers/continuum_manager.R')
source('../code/data_managers/hiv_surveillance_manager.R')

##---------------------------##
##-- LOAD UP DATA MANAGERS --##
##---------------------------##

if (!exists('ALL.DATA.MANAGERS'))
{
    print("READING IN DATA MANAGERS...")
    ALL.DATA.MANAGERS = list()

    #Census
    print('Loading Census Data...')
    load('../data2/parsed/five_age_census_msm.Rdata')
    ALL.DATA.MANAGERS$census.collapsed.msm = census

    load('../data2/parsed/five_age_census.Rdata')
    ALL.DATA.MANAGERS$census.collapsed = census

    load('../data2/parsed/full_age_census_msm.Rdata')
    ALL.DATA.MANAGERS$census.full.msm = census

    load('../data2/parsed/full_age_census.Rdata')
    ALL.DATA.MANAGERS$census.full = census

    census = NULL

    ALL.DATA.MANAGERS$census.totals = read.census.totals()

    #Load Mortality
    print('Reading Mortality Manager...')
    ALL.DATA.MANAGERS$mortality = read.mortality(dir='../data2/Mortality/Metro Mortality 2007-2017/', suffix=' mortality metro 2007-2017.txt')

    #Load Fertility
    print('Reading Fertility Manager...')
    ALL.DATA.MANAGERS$fertility = read.fertility('../data2/Natality/', census=ALL.DATA.MANAGERS$census.full)

    #IDU
    print('Reading IDU Manager...')
    idu2016 = read.idu.manager('../data2/IDU/NSDUH_2016', 2016, verbose = F)
    idu2015 = read.idu.manager('../data2/IDU/NSDUH_2015', 2015, verbose = F)
    ALL.DATA.MANAGERS$idu = combine.idu.managers(idu2016, idu2015)

    #PAIRING
    print('Reading Pairing Manager...')
    ALL.DATA.MANAGERS$pairing = read.pairing.manager('../data2/Pairing')

    #PrEP
    print('Reading PrEP Manager...')
    ALL.DATA.MANAGERS$prep = read.prep.manager(dir='../data2/PrEP/Zip3 PrEP/', census=ALL.DATA.MANAGERS$census.collapsed.msm)

    #Continuum
    print('Reading Continuum Manager...')
    ALL.DATA.MANAGERS$continuum = read.continuum.manager(dir='../data2/Continuum/')

    #Surveillance
    print('Reading HIV Surveillance...')
    ALL.DATA.MANAGERS$msa.surveillance = read.msa.surveillance(dir='../data2/HIV_Surveillance/by_msa')

    #MSM Proportions
    print('Reading MSM Proportions...')
    ALL.DATA.MANAGERS$msm.propotions = read.msm.proportions()
    
    
    print("DONE READING DATA MANAGERS")
}
