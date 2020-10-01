##--------------------------------------##
##--             SETTINGS             --##
##-- (for the structure of the model) --##
##--------------------------------------##

BLACK.HISPANIC.OTHER = c('black','hispanic','other')
SETTINGS = list(AGE_CUTOFFS = c(13,25,35,45,55,Inf),
                RACES = BLACK.HISPANIC.OTHER,
                RISK_STRATA = c('never_IDU', 'active_IDU', 'IDU_in_remission'),
                CONTINUUM_OF_CARE = c('undiagnosed', 'undiagnosed_from_prep', 'diagnosed'), # 'unsuppressed', 'suppressed'),
                CD4_STRATA = c('acute', 'chronic'),#c('acute','chronic','AIDS'),#c('acute','chronic'),
                SUBPOPULATIONS = NULL,
                SEXES = c('heterosexual_male', 'msm', 'female'))
SETTINGS$ACTIVE_IDU_STATE = SETTINGS$RISK_STRATA[2]
#SETTINGS$SUPPRESSED_STATES = SETTINGS$CONTINUUM_OF_CARE[3]
SETTINGS$CHRONIC_STATES = SETTINGS$CD4_STRATA[2]
SETTINGS$ACUTE_STATES = setdiff(SETTINGS$CD4_STRATA, SETTINGS$CHRONIC_STATES)
SETTINGS$UNDIAGNOSED_NO_PREP = SETTINGS$CONTINUUM_OF_CARE[1]
SETTINGS$UNDIAGNOSED_FROM_PREP = SETTINGS$CONTINUUM_OF_CARE[2]
SETTINGS$UNDIAGNOSED_STATES = c(SETTINGS$UNDIAGNOSED_NO_PREP, SETTINGS$UNDIAGNOSED_FROM_PREP)
SETTINGS$FIRST_DIAGNOSED_STATE = SETTINGS$CONTINUUM_OF_CARE[3]
SETTINGS$AGES = make.age.strata(SETTINGS$AGE_CUTOFFS)
SETTINGS$DIAGNOSED_STATES = setdiff(SETTINGS$CONTINUUM_OF_CARE, SETTINGS$UNDIAGNOSED_STATES)
