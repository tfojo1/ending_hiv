
##-----------------------------------##
##-- ALL MSAs DEFINED AS CONSTANTS --##
##-----------------------------------##

#-- EHE MSAs --#
NYC.MSA = '35620'
MIAMI.MSA = '33100'
LA.MSA = '31080'
ATLANTA.MSA = '12060'

HOUSTON.MSA = '26420'
DALLAS.MSA = '19100'
CHICAGO.MSA = '16980'
DC.MSA = '47900'

PHILADELPHIA.MSA = '37980'
ORLANDO.MSA = '36740'
SF.MSA = '41860'
PHOENIX.MSA = '38060'

TAMPA.MSA = '45300'
RIVERSIDE.MSA = '40140'
DETROIT.MSA = '19820'
BALTIMORE.MSA = '12580'

VEGAS.MSA = '29820'
BOSTON.MSA = '14460'
SAN.DIEGO.MSA = '41740'
CHARLOTTE.MSA = '16740'

SAN.ANTONIO.MSA = '41700'
JACKSONVILLE.MSA = '27260'
NEW.ORLEANS.MSA = '35380'
MEMPHIS.MSA = '32820'

SEATTLE.MSA = '42660'
AUSTIN.MSA = '12420'
INDIANAPOLIS.MSA = '26900'
CINCINATTI.MSA = '17140'

COLUMBUS.MSA = '18140'
BATON.ROUGE.MSA = '12940'
SACRAMENTO.MSA = '40900'
CLEVELAND.MSA = '17460'

#-- Non-EHE MSAs --#
ST.LOUIS.MSA = '41180'
DENVER.MSA = '19740'
PORTLAND.MSA = '38900'

BIRMINGHAM.MSA = '13820'
MOBILE.MSA = '33660'
JACKSON.MSA = '27140'

##----------------------------##
##-- LUMP THEM INTO VECTORS --##
##----------------------------##

# Every MSA we have represented in the code anywhere
ALL.MSAS = c(NYC=NYC.MSA,
             Miami=MIAMI.MSA,
             LA=LA.MSA,
             Atlanta=ATLANTA.MSA,
             Houston=HOUSTON.MSA,
             Dallas=DALLAS.MSA,
             Chicago=CHICAGO.MSA,
             DC=DC.MSA,
             Philadelphia=PHILADELPHIA.MSA,
             Orlando=ORLANDO.MSA,
             SF=SF.MSA,
             Phoenix=PHOENIX.MSA,
             Tampa=TAMPA.MSA,
             Riverside=RIVERSIDE.MSA,
             Detroit=DETROIT.MSA,
             Baltimore=BALTIMORE.MSA,
             Vegas=VEGAS.MSA,
             Boston=BOSTON.MSA,
             San_Diego=SAN.DIEGO.MSA,
             Charlotte=CHARLOTTE.MSA,
             San_Antonio=SAN.ANTONIO.MSA,
             Jacksonville=JACKSONVILLE.MSA,
             New_Orleans=NEW.ORLEANS.MSA,
             Memphis=MEMPHIS.MSA,
             Seattle=SEATTLE.MSA,
             Austin=AUSTIN.MSA,
             Indianapolis=INDIANAPOLIS.MSA,
             Cincinatti=CINCINATTI.MSA,
             Columbus=COLUMBUS.MSA,
             Baton_Rouge=BATON.ROUGE.MSA,
             Sacramento=SACRAMENTO.MSA,
             Cleveland=CLEVELAND.MSA,
             St_Louis=ST.LOUIS.MSA)

# Just the ones for EHE counties
TARGET.MSAS = ALL.MSAS[1:32]


##-- FUNCTIONS (map to indices) --##

msas.to.indices <- function(msas)
{
    sapply(msas, function(msa){
        mask = ALL.MSAS==as.character(msa)
        if (any(mask))
            (1:length(ALL.MSAS))[mask]
        else
            as.integer(NA)
    })
}

##-- SHORT NAMES --##
MSA.BRIEF.NAMES = c(
    '35620' = 'New York City',
    '33100' = 'Miami',
    '31080' = 'Los Angeles',
    '12060' = 'Atlanta',
    '26420' = 'Houston',
    '19100' = 'Dallas',
    '16980' = 'Chicago',
    '47900' = 'Washington D.C.',
    '37980' = 'Philadelphia',
    '36740' = 'Orlando',
    '41860' = 'San Francisco',
    '38060' = 'Phoenix',
    '45300' = 'Tampa',
    '40140' = 'Riverside',
    '19820' = 'Detroit',
    '12580' = 'Baltimore',
    '29820' = 'Las Vegas',
    '14460' = 'Boston',
    '41740' = 'San Diego',
    '16740' = 'Charlotte',
    '41700' = 'San Antonio',
    '27260' = 'Jacksonville',
    '35380' = 'New Orleans',
    '32820' = 'Memphis',
    '42660' = 'Seattle',
    '12420' = 'Austin',
    '26900' = 'Indianapolis',
    '17140' = 'Cincinatti',
    '18140' = 'Columbus',
    '12940' = 'Baton Rouge',
    '40900' = 'Sacramento',
    '17460' = 'Cleveland',
    '41180' = 'St. Louis',
    '19740' = 'Denver',
    '38900' = 'Portland'
)