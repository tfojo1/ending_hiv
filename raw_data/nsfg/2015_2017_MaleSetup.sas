/*--------------------------------------------------------------------------
 |
 |           NATIONAL SURVEY OF FAMILY GROWTH (NSFG), 2015-2017
 |
 |                       SAS Male Data Setup File
 |
 |
 | SAS setup sections are provided for the ASCII version of this data
 | collection.  These sections are listed below:
 |
 | PROC FORMAT:  creates user-defined formats for the variables. Formats
 | replace original value codes with value code descriptions. Only
 | variables with user-defined formats are included in this section.
 |
 | DATA:  begins a SAS data step and names an output SAS data set.
 |
 | INFILE:  identifies the input file to be read with the input statement.
 | Users must replace the "data-filename" with a filename specifying the
 | directory on the user's computer system in which the downloaded and
 | unzipped data file is physically located (e.g., "c:\temp\data.dat").
 |
 | INPUT:  assigns the name, type, decimal specification (if any), and
 | specifies the beginning and ending column locations for each variable
 | in the data file.
 |
 | LABEL:  assigns descriptive labels to all variables. Variable labels
 | and variable names may be identical for some variables.
 |
 | MISSING VALUE NOTE:  To maintain the original meaning of missing codes,
 | users may want to take advantage of the SAS missing values (the letters
 | A-Z or an underscore preceded by a period).
 |
 | An example of a standard missing value recode:
 |
 |   IF (RELATION = 98 OR RELATION = 99) THEN RELATION = .; 
 |
 | The same example using special missing value recodes:
 |
 |    IF RELATION = 98 THEN RELATION = .A;
 |    IF RELATION = 99 THEN RELATION = .B;
 |
 | FORMAT:  associates the formats created by the PROC FORMAT step with
 | the variables named in the INPUT statement.
 |
 | Users should modify this setup file to suit their specific needs.
 | To include these sections in the final SAS setup, users should remove the
 | SAS comment indicators from the desired section(s).
 |
 |-------------------------------------------------------------------------*/

* SAS PROC FORMAT;

/*
PROC FORMAT;
   value ACASILANG
      1="English"
      2="Spanish"
      7="Not ascertained" ;
   value ACASINUM
      0="NONE"
      1="1 PREGNANCY"
      2="2 PREGNANCIES"
      3="3 PREGNANCIES"
      4-95="4 OR MORE PREGNANCIES"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value ADDEXP
      0="NO ADDITIONAL BIRTHS"
      5=".5 ADDITIONAL BIRTHS"
      10="1 ADDITIONAL BIRTH"
      15="1.5 ADDITIONAL BIRTHS"
      20="2 ADDITIONAL BIRTHS"
      25="2.5 ADDITIONAL BIRTHS"
      30="3 ADDITIONAL BIRTHS"
      35="3.5 ADDITIONAL BIRTHS"
      40="4 ADDITIONAL BIRTHS"
      45="4.5 ADDITIONAL BIRTHS"
      50="5 ADDITIONAL BIRTHS"
      55="5.5 ADDITIONAL BIRTHS"
      60="6 ADDITIONAL BIRTHS"
      65="6.5 ADDITIONAL BIRTHS"
      70="7 ADDITIONAL BIRTHS"
      78="7.5 ADDITIONAL BIRTHS"
      80="8 ADDITIONAL BIRTHS"
      85="8.5 ADDITIONAL BIRTHS"
      90="9 ADDITIONAL BIRTHS"
      95="9.5 ADDITIONAL BIRTHS"
      100-995="10 OR MORE ADDITIONAL BIRTHS" ;
   value ADOPKIDS
      0="None"
      1-95="1 or more children"
      98="Refused"
      99="Don't know" ;
   value ADPTCHLIVF
      1="In this household full-time"
      2="In this household part-time"
      3="With his/her biological parent(s)"
      4="Away at school or college"
      5="Living on own"
      6="Living with other relatives"
      7="Deceased"
      8="Someplace else"
      98="Refused"
      99="Don't know" ;
   value ADPTGUAR2F
      1="Yes, trying to adopt"
      3="Yes, trying to become guardian"
      5="No, neither"
      8="Refused"
      9="Don't know" ;
   value ADPTGUARF
      1="Yes, adopted"
      3="Yes, became guardian"
      5="No, neither"
      8="Refused"
      9="Don't know" ;
   value AGDGFMT
      1="Strongly agree"
      2="Agree"
      3="Disagree"
      4="Strongly disagree"
      5="If R insists: Neither agree nor disagree"
      8="Refused"
      9="Don't know" ;
   value AGE2SELF
      0-19="Under 20 years"
      20-24="20-24 years"
      25-29="25-29 years"
      30-49="30-49 years"
      9998="Refused"
      9999="Don't know" ;
   value AGEBABY
      0-19="Under 20 years"
      20-24="20-24 years"
      25-29="25-29 years"
      30-49="30-49 years" ;
   value AGECANCER
      0-17="UNDER 18 YEARS"
      18-24="18-24 YEARS"
      25-49="25-49 YEARS"
      98="Refused"
      99="Don't know" ;
   value AGEFMT
      15="15 years"
      16="16 years"
      17="17 years"
      18="18 years"
      19="19 years"
      20="20 years"
      21="21 years"
      22="22 years"
      23="23 years"
      24="24 years"
      25="25 years"
      26="26 years"
      27="27 years"
      28="28 years"
      29="29 years"
      30="30 years"
      31="31 years"
      32="32 years"
      33="33 years"
      34="34 years"
      35="35 years"
      36="36 years"
      37="37 years"
      38="38 years"
      39="39 years"
      40="40 years"
      41="41 years"
      42="42 years"
      43="43 years"
      44="44 years"
      45="45 years"
      46="46 years"
      47="47 years"
      48="48 years"
      49-50="49 years"
      98="Refused"
      99="Don't know" ;
   value AGEGPRW
      1="Less than 5 years old"
      2="5-18 years old"
      3="19 years or older"
      8="Refused"
      9="Don't know" ;
   value AGEMOMBF
      1="LESS THAN 18 YEARS"
      2="18-19 YEARS"
      3="20-24 YEARS"
      4="25-29 YEARS"
      5="30 OR OLDER"
      95="NO MOTHER OR MOTHER-FIGURE"
      96="MOTHER-FIGURE HAD NO CHILDREN" ;
   value AGER
      15="15 YEARS"
      16="16 YEARS"
      17="17 YEARS"
      18="18 YEARS"
      19="19 YEARS"
      20="20 YEARS"
      21="21 YEARS"
      22="22 YEARS"
      23="23 YEARS"
      24="24 YEARS"
      25="25 YEARS"
      26="26 YEARS"
      27="27 YEARS"
      28="28 YEARS"
      29="29 YEARS"
      30="30 YEARS"
      31="31 YEARS"
      32="32 YEARS"
      33="33 YEARS"
      34="34 YEARS"
      35="35 YEARS"
      36="36 YEARS"
      37="37 YEARS"
      38="38 YEARS"
      39="39 YEARS"
      40="40 YEARS"
      41="41 YEARS"
      42="42 YEARS"
      43="43 YEARS"
      44="44 YEARS"
      45="45 YEARS"
      46="46 YEARS"
      47="47 YEARS"
      48="48 YEARS"
      49-50="49 YEARS" ;
   value AGESCRN
      15="15 years"
      16="16 years"
      17="17 years"
      18="18 years"
      19="19 years"
      20="20 years"
      21="21 years"
      22="22 years"
      23="23 years"
      24="24 years"
      25="25 years"
      26="26 years"
      27="27 years"
      28="28 years"
      29="29 years"
      30="30 years"
      31="31 years"
      32="32 years"
      33="33 years"
      34="34 years"
      35="35 years"
      36="36 years"
      37="37 years"
      38="38 years"
      39="39 years"
      40="40 years"
      41="41 years"
      42="42 years"
      43="43 years"
      44="44 years"
      45="45 years"
      46="46 years"
      47="47 years"
      48="48 years"
      49="49 years" ;
   value AGESELF
      0-19="UNDER 20 YEARS"
      20-24="20-24 YEARS"
      25-29="25-29 YEARS"
      30-49="30 YEARS OR OLDER"
      98="Refused"
      99="Don't know" ;
   value AGEWFPRT2F
      0-19="UNDER 20 YEARS"
      20-24="20-24 YEARS"
      25-29="25-29 YEARS"
      30-95="30 YEARS OR OLDER"
      98="Refused"
      99="Don't know" ;
   value AIDSTALKF
      1="How HIV/AIDS is transmitted"
      2="Other sexually transmitted diseases like gonorrhea, herpes, or Hepatitis C"
      3="The correct use of condoms"
      4="Needle cleaning/using clean needles"
      5="Dangers of needle sharing"
      6="Abstinence from sex (not having sex)"
      7="Reducing your number of sexual partners"
      8="Condom use to prevent HIV or STD transmission"
      9='"Safe sex" practices (abstinence, condom use, etc)'
      10="Getting tested and knowing your HIV status"
      20="Some other topic - not shown separately"
      98="Refused"
      99="Don't know" ;
   value AKIDNONE
      0="None"
      1="1 child"
      2="2 children"
      3="3 children"
      4="4 children"
      5-95="5 or more children"
      98="Refused"
      99="Don't know" ;
   value AKIDNUM
      0="None"
      1-10="1 or more children"
      98="Refused"
      99="Don't know" ;
   value ALCORISK
      1="A lot"
      2="A little"
      3="Not at all"
      4="No opinion"
      8="Refused"
      9="Don't know" ;
   value ATTNDFMT
      1="More than once a week"
      2="Once a week"
      3="2-3 times a month"
      4="Once a month (about 12 times a year)"
      5="3-11 times a year"
      6="Once or twice a year"
      7="Never"
      8="Refused"
      9="Don't know" ;
   value ATTRACT
      1="Only attracted to females"
      2="Mostly attracted to females"
      3="Equally attracted to females and males"
      4="Mostly attracted to males"
      5="Only attracted to males"
      6="Not sure"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value B1PREMAR
      1="Yes (1st biological child born before 1st marriage)"
      2="No (1st biological child born in same month as or later than 1st marriage)" ;
   value BARRIERF
      1="I did not need to see a doctor in the last year"
      2="I did not know where to go for care"
      3="I could not afford to pay for a visit"
      4="I was afraid to hear bad news"
      5="I had privacy/confidentiality concerns"
      6="I could not take time off from work"
      7="Insurance"
      8="Not sexually active"
      9="Time/busy"
      10="Didn't make appt"
      11="Don't like/trust doctors"
      20="Something else - not shown separately"
      98="Refused"
      99="Don't know" ;
   value BIOADPTKD
      0="NONE"
      1="1 CHILD"
      2="2 CHILDREN"
      3="3 CHILDREN"
      4-10="4 OR MORE CHILDREN" ;
   value BIOCHLIV1F
      1="In this household full-time"
      2="In this household part-time"
      3="With his/her mother"
      4="Away at school or college"
      5="Living on own"
      6="Living with other relatives"
      7="Deceased"
      8="Placed for adoption or adopted"
      9="Placed in foster care"
      10="Someplace else"
      98="Refused"
      99="Don't know" ;
   value BIOCHRFMT
      0="NONE"
      1="1 CHILD"
      2="2 CHILDREN"
      3="3 CHILDREN"
      4-95="4 OR MORE CHILDREN" ;
   value BIOCHRFMT2F
      0="NONE"
      1="1 CHILD"
      2="2 CHILDREN"
      3="3 CHILDREN"
      4="4 OR MORE CHILDREN" ;
   value BIOHWSNF
      1-60="1-60 months"
      61-995="61 months or more"
      998="Refused"
      999="Don't know" ;
   value BIOLIVNGF
      1="In this household full-time"
      2="In this household part-time"
      3="With his/her mother"
      4="Away at school or college"
      5="Living on own"
      6="Living with other relatives"
      7="Deceased"
      8="Placed for adoption or adopted"
      9="Placed in foster care"
      10="Someplace else"
      98="Refused"
      99="Don't know" ;
   value BMI
      15-60="15-60"
      95="Could not be defined" ;
   value BNGE30T
      0="NONE"
      1="1 TIME"
      2="2 TIMES"
      3="3 TIMES"
      4="4 TIMES"
      5-95="5 OR MORE TIMES"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value CANCTYPEF
      1="Bladder cancer"
      2="Bone cancer"
      3="Brain cancer or tumor, spinal cord cancer, or other cancer of the central nervous system"
      4="Breast cancer"
      5="BLANK"
      6="Colon cancer"
      7="BLANK"
      8="Head and neck cancer"
      9="Heart cancer"
      10="Leukemia/blood cancer"
      11="Liver cancer"
      12="Lung cancer"
      13="Lymphoma including Hodgkins disease/Lymphoma and non-Hodgkins lymphomas"
      14="Melanoma"
      15="Neuroblastoma"
      16="Oral (mouth/tongue/lip) cancer"
      17="BLANK"
      18="Pancreatic (pancreas) cancer"
      19="Pharyngeal (throat) cancer"
      20="Prostate cancer"
      21="Rectal (rectum) cancer"
      22="Renal (kidney) cancer"
      23="Stomach cancer"
      24="Thyroid cancer"
      25="Other"
      26="Blood"
      27="Esophageal (esophagus) cancer"
      29="Gallbladder cancer"
      30="Laryngeal (larynx/windpipe) cancer"
      31="Skin cancer (non-melanoma)"
      32="Skin cancer (DK what kind)"
      33="Soft tissue (muscle or fat) sarcoma"
      34="Testicular (testis) cancer"
      98="Refused"
      99="Don't know" ;
   value CASEID
      70622-80715="Respondent ID number" ;
   value CASISMK
      0="NEVER SMOKED REGULARLY"
      11="11 YEARS OR YOUNGER"
      12="12 YEARS OLD"
      13="13 YEARS OLD"
      14="14 YEARS OLD"
      15="15 YEARS OLD"
      16="16 YEARS OLD"
      17="17 YEARS OLD"
      18="18 YEARS OLD"
      19="19 YEARS OLD"
      20-24="20-24 YEARS OLD"
      25-29="25-29 YEARS OLD"
      30-49="30-49 YEARS OLD"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value CEBOW
      0="None"
      1="1 child"
      2="2 children"
      3="3 children"
      4="4 children"
      5-95="5 or more" ;
   value CHBOTHER
      1="A great deal"
      2="Some"
      3="A little"
      4="Not at all"
      8="Refused"
      9="Don't know" ;
   value CMFMT
      378-1340="Before Sept 2011"
      1341-1352="Sept 2011-Aug 2012"
      1353-1364="Sept 2012-Aug 2013"
      1365-1376="Sept 2013-Aug 2014"
      1377-1388="Sept 2014-Aug 2015"
      1389-1400="Sept 2015-Aug 2016"
      1401-1413="Sept 2016-Sept 2017"
      9997="Not ascertained"
      9998="Refused"
      9999="Don't know" ;
   value CNCALLU5FF
      0="NONE"
      1="1 CHILD"
      2="2 OR MORE CHILDREN"
      98="Refused"
      99="Don't know" ;
   value COHEVER
      1="Yes, ever cohabited (lived with a woman outside of marriage)"
      2="No, never cohabited (lived with a woman outside of marriage)" ;
   value COHNUM
      0="None"
      1="One"
      2="Two"
      3-HIGH="Three or more" ;
   value COHOUT
      1="Intact cohabitation"
      2="Intact marriage"
      3="Dissolved marriage"
      4="Dissolved cohabitation" ;
   value COHSTAT
      1="Never cohabited outside of marriage"
      2="First cohabited before first marriage"
      3="First cohabited after first marriage" ;
   value COMPREG
      0="NONE"
      1="1 PREGNANCY"
      2="2 PREGNANCIES"
      3="3 PREGNANCIES"
      4="4 PREGNANCIES"
      5-95="5 OR MORE PREGNANCIES" ;
   value CONDPCT
      0-10="10% OR LESS"
      11-20="11-20%"
      21-30="21-30%"
      31-40="31-40%"
      41-50="41-50%"
      51-60="51-60%"
      61-70="61-70%"
      71-80="71-80%"
      81-90="81-90%"
      91-100="91-100%"
      998="Refused"
      999="Don't know" ;
   value COVERHOWF
      1="Private health insurance plan (from employer or workplace; purchased directly; through a state or local government program or community program)"
      2="Medicaid-additional name(s) for Medicaid in this state: [MEDICAID_FILL]"
      3="Medicare"
      4="Medi-GAP"
      5="Military health care including: the VA, CHAMPUS, TRICARE, CHAMP-VA"
      6="Indian Health Service, or Single Service Plan"
      7="CHIP"
      8="State-sponsored health plan"
      9="Other government health care"
      98="Refused"
      99="Don't know" ;
   value CRALL3F
      0="NONE"
      1="1 CHILD"
      2="2 CHILDREN"
      3="3 OR MORE CHILDREN"
      98="Refused"
      99="Don't know" ;
   value CRALL4F
      0="NONE"
      1="1 CHILD"
      2="2 CHILDREN"
      3="3 CHILDREN"
      4="4 OR MORE CHILDREN"
      98="Refused"
      99="Don't know" ;
   value CSPBIO
      0="No joint biological children"
      1="1 joint biological child"
      2="2 joint biological children"
      3="3 or more joint biological children" ;
   value CSPNOT
      0="No such child"
      1="1 or more such children" ;
   value CURR_INS
      1="Currently covered by private health insurance or Medi-Gap"
      2="Currently covered by Medicaid, CHIP, or a state-sponsored health plan"
      3="Currently covered by Medicare, military health care, or other government health care"
      4="Currently covered only by a single-service plan, only by the Indian Health Service, or currently not covered by health insurance" ;
   value CURR_RSD
      1="None"
      2="Catholic"
      3="Baptist/Southern Baptist"
      4="Methodist, Lutheran, Presbyterian, Episcopal"
      5="Fundamentalist Protestant"
      6="Other Protestant denomination"
      7="Protestant - No specific denomination"
      8="Other religion"
      9="Refused"
      10="Don't know" ;
   value CURRPREGF
      0="INAPP/DK/RF"
      1="YES"
      8="Refused"
      9="Don't know" ;
   value CURRPRTF
      0="INAPP/DK/RF"
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS"
      8="Refused"
      9="Don't know" ;
   value CWPCHLIVF
      1="In this household full-time"
      2="In this household part-time"
      3="Away at school or college"
      4="Living on own"
      5="Living with other relatives"
      6="Deceased"
      7="Placed for adoption or adopted"
      8="Placed in foster care"
      9="Someplace else"
      98="Refused"
      99="Don't know" ;
   value CWPNBLIVF
      1="In this household full-time"
      2="In this household part-time"
      3="Away at school or college"
      4="Living on own"
      5="Living with other relatives"
      6="Deceased"
      7="Someplace else"
      8="Refused"
      9="Don't know" ;
   value CWPNBRELF
      1="Yes, by blood"
      3="Yes, by marriage"
      5="No"
      8="Refused"
      9="Don't know" ;
   value CWPTRYLG
      0-12="1 year or less"
      13-24="13-24 months"
      25-36="25-36 months"
      37-48="37-48 months"
      49-60="49-60 months"
      61-995="More than 5 years"
      998="Refused"
      999="Don't know" ;
   value CWPTYPOPF
      1="Tubal ligation or tubal sterilization"
      2="Hysterectomy"
      3="Something else"
      8="Refused"
      9="Don't know" ;
   value DADTYP518F
      1="ONLY CORESIDENTIAL CHILDREN 5 TO 18"
      2="ONLY NON-CORESIDENTIAL CHILDREN 5 TO 18"
      3="BOTH CORESIDENTIAL AND NONCORESIDENTIAL CHILDREN 5 TO 18"
      4="NEITHER CORESIDENTIAL OR NONCORESIDENTIAL CHILDREN 5 TO 18, NO CHILDREN AT ALL, OR NEVER HAD SEXUAL INTERCOURSE" ;
   value DADTYPE
      1="ONLY CORESIDENTIAL CHILDREN"
      2="ONLY NON-CORESIDENTIAL CHILDREN"
      3="BOTH CORESIDENTIAL AND NON-CORESIDENTIAL CHILDREN"
      4="NO CHILDREN 18 OR YOUNGER, NO CHILDREN AT ALL, OR NEVER HAD SEXUAL INTERCOURSE" ;
   value DADTYPU5F
      1="ONLY CORESIDENTIAL CHILDREN UNDER 5"
      2="ONLY NON-CORESIDENTIAL CHILDREN UNDER 5"
      3="BOTH CORESIDENTIAL AND NONCORESIDENTIAL CHILDREN UNDER 5"
      4="NEITHER CORESIDENTIAL OR NONCORESIDENTIAL CHILDREN UNDER 5, NO CHILDREN, OR NEVER HAD SEXUAL INTERCOURSE" ;
   value DEFPROBF
      1="Definitely yes"
      2="Probably yes"
      3="Probably no"
      4="Definitely no"
      8="Refused"
      9="Don't know" ;
   value DEGREES
      1="Associate's degree"
      2="Bachelor's degree"
      3="Master's degree"
      4="Doctorate degree"
      5="Professional school degree"
      8="Refused"
      9="Don't know" ;
   value DIPGED
      1="High school diploma"
      2="GED only"
      3="Both"
      5="Neither"
      8="Refused"
      9="Don't know" ;
   value DOLASTWKF
      1="Working"
      2="Working - Paternity leave or temp leave"
      3="Not working, looking for work"
      4="Keeping house or taking care of family"
      5="In school"
      6="Other"
      8="Refused"
      9="Don't know" ;
   value DRINKF
      1="Never"
      2="Once or twice during the year"
      3="Several times during the year"
      4="About once a month"
      5="About once a week"
      6="About once a day"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value DRNK30D
      0="0 DAYS"
      1="1 DAY"
      2="2 DAYS"
      3-30="3 OR MORE DAYS"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value DRNKNUM
      1="1 DRINK"
      2="2 DRINKS"
      3="3 DRINKS"
      4="4 DRINKS"
      5="5 DRINKS"
      6-95="6 OR MORE DRINKS"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value DRUGFRF
      1="Never"
      2="Once or twice during the year"
      3="Several times during the year"
      4="About once a month or more"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value DURFSTER
      1="Less than six months"
      2="At least six months, but less than a year"
      3="At least a year but less than two years"
      4="At least two years but less than three years"
      5="Three years or more"
      8="Refused"
      9="Don't know" ;
   value EARN
      1="Under $96 (weekly)/Under $417 (monthly)/Under $5,000 (yearly)"
      2="$96-$143 (weekly)/$417-624 (monthly)/$5,000-7,499 (yearly)"
      3="$144-191 (weekly)/$625-832 (monthly)/$7,500-9,999 (yearly)"
      4="$192-239 (weekly)/$833-1,041 (monthly)/$10,000-12,499 (yearly)"
      5="$240-288 (weekly)/$1,042-1,249 (monthly)/$12,500-14,999 (yearly)"
      6="$289-384 (weekly)/$1,250-1,666 (monthly)/$15,000-19,999 (yearly)"
      7="$385-480 (weekly)/$1,667-2,082 (monthly)/$20,000-24,999 (yearly)"
      8="$481-576 (weekly)/$2,083-2,499 (monthly)/$25,000-29,999 (yearly)"
      9="$577-672 (weekly)/$2,500-2,916 (monthly)/$30,000-34,999 (yearly)"
      10="$673-768 (weekly)/$2,917-3,332 (monthly)/$35,000-39,999 (yearly)"
      11="$769-961 (weekly)/$3,333-4,166 (monthly)/$40,000-49,999 (yearly)"
      12="$962-1,153 (weekly)/$4,167-4,999 (monthly)/$50,000-59,999 (yearly)"
      13="$1,154-1,441 (weekly)/$5,000-6,249 (monthly)/$60,000-74,999 (yearly)"
      14="$1,442-1,922 (weekly)/$6,250-8,332 (monthly)/$75,000-99,999 (yearly)"
      15="$1,923 or more (weekly)/$8,333 or more (monthly)/$100,000 or more (yearly)"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value EDUCAT2F
      9="9TH GRADE OR LESS"
      10="10TH GRADE"
      11="11TH GRADE"
      12="12TH GRADE"
      13="1 YEAR OF COLLEGE/GRAD SCHOOL"
      14="2 YEARS OF COLLEGE/GRAD SCHOOL"
      15="3 YEARS OF COLLEGE/GRAD SCHOOL"
      16="4 YEARS OF COLLEGE/GRAD SCHOOL"
      17="5 YEARS OF COLLEGE/GRAD SCHOOL"
      18="6 YEARS OF COLLEGE/GRAD SCHOOL"
      19="7+ YEARS OF COLLEGE/GRAD SCHOOL" ;
   value EDUCMOMF
      1="Less than high school"
      2="High school graduate or GED"
      3="Some college, including 2-year degrees"
      4="Bachelor's degree or higher"
      95="No mother/mother-figure identified" ;
   value EDUCNF
      1="Less than high school"
      2="High school graduate or GED"
      3="Some college but no degree"
      4="2-year college degree (e.g., Associate's degree)"
      5="4-year college graduate (e.g., BA, BS)"
      6="Graduate or professional school"
      8="Refused"
      9="Don't know" ;
   value ELAPSED
      0="0"
      1-995="Number of months" ;
   value ENGAGF
      1="YES, ENGAGED TO BE MARRIED"
      3="NOT ENGAGED BUT HAD DEFINITE PLANS TO GET MARRIED"
      5="NO, NEITHER ENGAGED NOR HAD DEFINITE PLANS"
      8="REFUSED"
      9="DON'T KNOW" ;
   value ENGSPEAK
      1="Very well"
      2="Well"
      3="Not well"
      4="Not at all"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value EVHIVTST
      0="NO HIV TEST REPORTED"
      1="YES, ONLY AS PART OF BLOOD DONATION"
      2="YES, ONLY OUTSIDE OF BLOOD DONATION"
      3="YES, IN BOTH CONTEXTS" ;
   value EVMARCOH
      1="Yes, ever married or cohabited"
      2="No, never married or cohabited" ;
   value EVRCHL3F
      1="1 child"
      2="2 children"
      3="3 children"
      4-95="4 or more children"
      98="Refused"
      99="Don't know" ;
   value EVRMARRY
      0="NEVER MARRIED"
      1="EVER MARRIED" ;
   value EVRNOPAT
      1="YES, 1 OR MORE CHILDREN OUT OF WEDLOCK, BUT NO ESTABLISHED PATERNITY"
      2="NO, 1 OR MORE CHILDREN OUT OF WEDLOCK, BUT ESTABLISHED PATERNITY" ;
   value EXPECTSF
      0="NONE"
      1="1 CHILD"
      2-95="2 OR MORE CHILDREN"
      98="Refused"
      99="Don't know" ;
   value EXPGRADE2F
      9="9th grade or less"
      10="10th grade"
      11="11th grade"
      12="12th grade"
      13="1 year of college or less"
      14="2 years of college"
      15="3 years of college"
      16="4 years of college/grad school"
      17="5 years of college/grad school"
      18="6 years of college/grad school"
      19="7 or more years of college and/or grad school"
      98="Refused"
      99="Don't know" ;
   value EXRELAT
      0="FORMER COHAB PARTNER"
      1="FORMER WIFE"
      8="Refused"
      9="Don't know" ;
   value FIRST
      1="Yes, [NAME OF LAST P]"
      2="Yes, [NAME OF 2ND-TO-LAST P]"
      3="Yes, [NAME OF 3RD-TO-LAST P]"
      5="No"
      8="Refused"
      9="Don't know" ;
   value FMARITAL
      1="MARRIED TO A PERSON OF THE OPPOSITE SEX"
      2="WIDOWED"
      3="DIVORCED OR ANNULLED"
      4="SEPARATED"
      5="NEVER MARRIED" ;
   value FMARITF
      0="DK/RF"
      1="MARRIED"
      2="WIDOWED"
      3="DIVORCED"
      4="SEPARATED"
      5="NEVER MARRIED" ;
   value FMARNO
      0="NEVER BEEN MARRIED"
      1="1 TIME"
      2="2 TIMES"
      3="3 TIMES"
      4-95="4 OR MORE TIMES" ;
   value FMARSTAT
      3="Widowed"
      4="Divorced or annulled"
      5="Separated, because you and your spouse are not getting along"
      6="Never been married"
      8="Refused"
      9="Don't know" ;
   value FMINCDK1F
      1="Less than $50,000"
      5="$50,000 or more"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value FPAGE
      0-14="UNDER 15 YEARS"
      15-17="15-17 YEARS"
      18-19="18-19 YEARS"
      20-49="20 YEARS OR OLDER"
      98="Refused"
      99="Don't know" ;
   value FPAGE15F
      1="Less than 15"
      2="15 years or older"
      8="Refused"
      9="Don't know" ;
   value FPAGE18F
      1="Less than 18"
      2="18 years or older"
      8="Refused"
      9="Don't know" ;
   value FPAGE20F
      1="Less than 20"
      2="20 years or older"
      8="Refused"
      9="Don't know" ;
   value FPFLAG
      1="cmfstsex - CM of first sex ever, based on DL series"
      2="cmlsxp - CM when R last had sex with most recent partner"
      3="cmlsxp2 - CM when R last had sex with 2nd-to-last partner"
      4="cmlsxp3 - CM when R last had sex with 3rd-to-last partner"
      5="cmfsxp - CM when R first had sex with most recent partner"
      6="cmfsxp2 - CM when R first had sex with 2nd-to-last partner"
      7="cmfsxp3 - CM when R first had sex with 3rd-to-last partner"
      8="cmfsxcwp - CM when R first had sex with CWP"
      9="unable to determine: raw variable(s) missing" ;
   value FREQ2VF
      1="Never"
      2="Once or twice during the year"
      3="Several times during the year"
      4="About once a month"
      5="About once a week"
      6="About once a day or more"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value FRQJAIL
      1="Only one time"
      2="Or more than one time"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value FRQJAIL2F
      1="One month or less"
      2="More than one month but less than one year"
      3="One year"
      4="More than one year"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value FSEXRLTN
      1="MARRIED TO HER"
      2="ENGAGED TO HER, AND LIVING TOGETHER"
      3="ENGAGED TO HER, BUT NOT LIVING TOGETHER"
      4="LIVING TOGETHER IN A SEXUAL RELATIONSHIP, BUT NOT ENGAGED"
      5="GOING OUT WITH HER OR GOING STEADY"
      6="GOING OUT WITH HER ONCE IN A WHILE"
      7="JUST FRIENDS"
      8="HAD JUST MET HER"
      9="SOMETHING ELSE" ;
   value FTPTSEF
      1="Full-time"
      2="Part-time"
      3="Some of each"
      8="Refused"
      9="Don't know" ;
   value FUNDAMF
      1="A born again Christian"
      2="A charismatic"
      3="An evangelical"
      4="A fundamentalist"
      5="None of the above"
      8="Refused"
      9="Don't know" ;
   value GENHEALT
      1="Excellent"
      2="Very good"
      3="Good"
      4="Fair"
      5="Poor"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value GRADSUSP
      1-6="GRADES 1-6"
      7-9="GRADES 7-9"
      10-12="GRADES 10-12"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value HADSEX
      1="YES, R EVER HAD INTERCOURSE"
      2="NO, R NEVER HAD INTERCOURSE" ;
   value HHFAMTYP
      1="No spouse/partner and no child(ren) (of R) 18 or younger"
      2="Spouse/partner, but no child(ren) (of R) 18 or younger"
      3="Spouse and R's child(ren) 18 or younger"
      4="Cohabiting partner and R's child(ren) 18 or younger"
      5="No spouse/partner, but child(ren) of R, 18 or younger" ;
   value HHKIDTYP
      0="No child(ren) 18 or younger in HH or only older child(ren)"
      1="At least one biological child (of R's) under 18 in HH, no nonbiological child(ren)"
      2="Any non-biological child (of R's) 18 or younger in HH" ;
   value HHPARTYP
      1="Both biological or both adoptive parents"
      2="Biological and step- or adoptive parent"
      3="Single parent (biological, adoptive, or stepparent)"
      4="Other" ;
   value HIEDUC
      5="9TH GRADE OR LESS"
      6="10TH GRADE"
      7="11TH GRADE"
      8="12TH GRADE, NO DIPLOMA (NOR GED)"
      9="HIGH SCHOOL GRADUATE (DIPLOMA OR GED)"
      10="SOME COLLEGE BUT NO DEGREE"
      11="ASSOCIATE DEGREE IN COLLEGE/UNIVERSITY"
      12="BACHELOR'S DEGREE"
      13="MASTER'S DEGREE"
      14="DOCTORATE DEGREE"
      15="PROFESSIONAL DEGREE" ;
   value HIGHBP
      1="Yes"
      5="No"
      6="If vol: Not told results"
      8="Refused"
      9="Don't know" ;
   value HIGRADE
      9="9th grade or less"
      10="10th grade"
      11="11th grade"
      12="12th grade"
      13="1 year of college or less"
      14="2 years of college"
      15="3 years of college"
      16="4 years of college/grad school"
      17="5 years of college/grad school"
      18="6 years of college/grad school"
      19="7 or more years of college and/or grad school"
      98="Refused"
      99="Don't know" ;
   value HISCHGRD
      1="1st grade"
      2="2nd grade"
      3="3rd grade"
      4="4th grade"
      5="5th grade"
      6="6th grade"
      7="7th grade"
      8="8th grade"
      9="9th grade"
      10="10th grade"
      11="11th grade"
      12="12th grade"
      98="Refused"
      99="Don't know" ;
   value HISPANIC
      1="HISPANIC"
      2="NON-HISPANIC" ;
   value HISPGRPF
      1="Mexican, Mexican American, or Chicano, only"
      2="All other Hispanic or Latino groups, including mulitple responses"
      8="Refused"
      9="Don't know" ;
   value HISPRACE2F
      1="Hispanic"
      2="Non-Hispanic White, Single Race"
      3="Non-Hispanic Black, Single Race"
      4="Non-Hispanic Other or Multiple Race" ;
   value HIVTST
      1="Part of a medical checkup or surgical procedure (a doctor or medical provider asked for the test)"
      2="Required for health or life insurance coverage"
      3="Required for marriage license or to get married"
      4="Required for military service or job"
      5="You wanted to find out if infected or not (you were the one who asked for the test)"
      6="Someone else suggested you should be tested (followed by WHOSUGG question)"
      7="Intentionally blank (a code used only for females, prenatal testing)"
      8="You might have been exposed through sex or drug use"
      9="You might have been exposed in some other way"
      20="Some other reason - not shown separately"
      21="Required for immigration or travel"
      22="Required for incarceration"
      23="Required for school"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value HSPLSXPRC
      1="Hispanic"
      2="Non-Hispanic White"
      3="Non-Hispanic Black"
      4="Non-Hispanic Other" ;
   value IMPFLG
      0="QUESTIONNAIRE DATA (NOT IMPUTED)"
      1="MULTIPLE REGRESSION IMPUTATION"
      2="LOGICAL IMPUTATION" ;
   value INCHES
      64="64 inches or less"
      65="65 inches"
      66="66 inches"
      67="67 inches"
      68="68 inches"
      69="69 inches"
      70="70 inches"
      71="71 inches"
      72="72 inches"
      73="73 inches"
      74="74 inches"
      75="75 inches"
      76="76 inches or more"
      96="Could not be defined" ;
   value INCWMYF
      1="Week"
      2="Month"
      3="Year"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value INFEVER
      1="YES"
      2="NO" ;
   value INFRTHISNF
      1="Low sperm count or no sperm"
      2="Varicocele"
      3="Genetic disorder that alters sperm production"
      4="Low testosterone level"
      5="Other"
      6="None of the above"
      8="Refused"
      9="Don't know" ;
   value INFSVCSF
      1="Advice"
      2="Infertility testing"
      3="Drugs to improve ovulation"
      4="Surgery to correct blocked tubes"
      5="Artificial insemination"
      6="Treatment for varicocele"
      7="Other types of medical help"
      8="Refused"
      9="Don't know" ;
   value INFTEST
      1="You"
      2="Her"
      3="Both of you"
      8="Refused"
      9="Don't know" ;
   value INTCTFAM
      1="TWO BIOLOGICAL OR ADOPTIVE PARENTS FROM BIRTH"
      2="ANYTHING OTHER THAN 2 BIOLOGICAL OR ADOPTIVE PARENTS FROM BIRTH" ;
   value INTENT
      1="R intends to have (more) children"
      2="R does not intend to have (more) children"
      3="R does not know his intent" ;
   value INTNEXT
      1="Within the next 2 years"
      2="2-5 years from now"
      3="More than 5 years from now"
      8="Refused"
      9="Don't know" ;
   value INTVLNGTH
      13.04-149.51="13.04-149.51" ;
   value JSUREINT
      1="Very sure"
      2="Somewhat sure"
      3="Not at all sure"
      8="Refused"
      9="Don't know" ;
   value KAGEGRPNAF
      1="Under 5 years"
      2="5-18 years"
      3="19 or older"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value KDAGFNAF
      0-4="Under 5 years"
      5-18="5-18 years"
      19-95="19 or older"
      997="Not ascertained"
      998="Refused"
      999="Don't know" ;
   value KIDAGEF2D
      0-4="Under 5 years"
      5-18="5-18 years"
      19-95="19 or older"
      98="Refused"
      99="Don't know" ;
   value KIDAGEF2F
      0-4="Under 5 years"
      5-18="5-18 years"
      19-95="19 or older"
      998="Refused"
      999="Don't know" ;
   value KIDHHF
      1="In household"
      2="Alive, not adopted/foster, but not in HH"
      3="Dead or adopted/foster or DK/RF"
      8="Refused"
      9="Don't know" ;
   value KNOWSFMT
      1="Knows everything"
      2="Knows most things"
      3="Knows some things"
      4="Knows a little"
      5="Knows nothing"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value LABORFOR
      1="Working full-time"
      2="Working part-time"
      3="Working, but on vacation, strike, or had temporary illness"
      4="Working - paternity or family leave"
      5="Not working but looking for work"
      6="In school"
      7="Keeping house"
      8="Caring for family"
      9="Other" ;
   value LIFEPRT
      1="One"
      2="Two"
      3="Three"
      4="Four"
      5="Five"
      6="Six"
      7="Seven or more"
      8="Refused"
      9="Don't know" ;
   value LIFPRTNR
      0="0 PARTNERS"
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS"
      4="4 PARTNERS"
      5="5 PARTNERS"
      6="6 PARTNERS"
      7="7 PARTNERS"
      8="8 PARTNERS"
      9="9 PARTNERS"
      10-19="10-19 PARTNERS"
      20-49="20-49 PARTNERS"
      50="50 OR MORE PARTNERS" ;
   value LRNPRGF
      1="During the pregnancy"
      2="After the child was born"
      8="Refused"
      9="Don't know" ;
   value LSEXRAGE
      0-17="UNDER 18 YEARS"
      18-19="18-19 YEARS"
      20-24="20-24 YEARS"
      25-34="25-34 YEARS"
      35-50="35-49 YEARS" ;
   value LSEXRLTN
      1="Married to her"
      3="Living together in a sexual relationship"
      4="Going out with her or going steady"
      5="Going out with her once in a while"
      6="Just friends"
      7="Had just met her"
      8="Something else"
      9="Engaged to her: only asked of a subset of Rs" ;
   value LSEXUSEF
      1="CONDOM"
      2="WITHDRAWAL"
      3="VASECTOMY"
      4="PILL"
      5="FEMALE STERILIZATION"
      6="INJECTION -- DEPO-PROVERA/LUNELLE"
      7="SPERMICIDAL FOAM/JELLY/CREAM/FILM/SUPPOSITORY"
      8="HORMONAL IMPLANT -- NORPLANT/IMPLANON/NEXPLANON"
      9="RHYTHM OR SAFE PERIOD"
      10="CONTRACEPTIVE PATCH"
      11="VAGINAL CONTRACEPTIVE RING"
      12="IUD, COIL LOOP"
      13="SOMETHING ELSE"
      95="R USED NO METHOD; R DOES NOT KNOW IF PARTNER USED A METHOD"
      96="NO METHOD USED AT LAST SEX" ;
   value LSTGRADE
      0="No formal schooling"
      1-5="5TH GRADE OR LOWER"
      6-8="6TH, 7TH, OR 8TH GRADE"
      9="9TH GRADE"
      10="10TH GRADE"
      11="11TH GRADE"
      12="12TH GRADE"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value LSXUSEPF
      0="NONE OR NO METHOD IDENTIFIED"
      1="ANY METHOD IDENTIFIED"
      8="Refused"
      9="Don't know" ;
   value LVSIT14F
      1="Biological mother or adoptive mother"
      2="Other mother figure"
      3="No mother figure"
      8="Refused"
      9="Don't know" ;
   value MALFEMF
      1="Male"
      2="Female"
      8="Refused"
      9="Don't know" ;
   value MALFEMNAF
      1="Male"
      2="Female"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value MALFIGF
      1="Biological father or adoptive father"
      2="Step-father"
      3="No father figure"
      4="Other father figure"
      8="Refused"
      9="Don't know" ;
   value MALPRT
      0="NONE"
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS"
      4="4 PARTNERS"
      5="5 PARTNERS"
      6="6 PARTNERS"
      7="7 PARTNERS"
      8="8 PARTNERS"
      9="9 PARTNERS"
      10="10 OR MORE PARTNERS"
      997="Not ascertained"
      998="Refused"
      999="Don't know" ;
   value MARBABYF
      1="Yes, married to child's mother"
      2="No, not married to child's mother" ;
   value MARCATA
      1="0-12 months"
      2="13-24 months"
      3="25-36 months"
      4="37-48 months"
      5="More than 4 years" ;
   value MARCATF
      1="0-12 months"
      2="13-24 months"
      3="25-36 months"
      4="More than 3 years"
      5="First intercourse after first marriage" ;
   value MARCATG
      1="0-12 months"
      2="13-24 months"
      3="25-36 months"
      4="More than 3 years"
      5="First intercourse after first union" ;
   value MAREND
      1="DIVORCED OR ANNULLED"
      2="SEPARATED"
      3="WIDOWED" ;
   value MARRENDF
      1="Death of wife"
      2="Divorce"
      3="Annulment"
      4="Separation"
      8="Refused"
      9="Don't know" ;
   value MARSTAT
      1="Married to a person of the opposite sex"
      2="Not married but living together with a partner of the opposite sex"
      3="Widowed"
      4="Divorced or annulled"
      5="Separated, because you and your spouse are not getting along"
      6="Never been married"
      8="Refused"
      9="Don't know" ;
   value MDDEGRE
      1="Less than high school"
      2="High school graduate or GED"
      3="Some college, including 2-year degrees"
      4="Bachelor's degree or higher"
      8="Refused"
      9="Don't know" ;
   value METH12MF
      1="CONDOM"
      2="WITHDRAWAL"
      3="VASECTOMY"
      4="PILL"
      5="FEMALE STERILIZATION"
      6="INJECTION -- DEPO-PROVERA/LUNELLE"
      7="SPERMICIDAL FOAM/JELLY/CREAM/FILM/SUPPOSITORY"
      8="HORMONAL IMPLANT -- NORPLANT/IMPLANON/NEXPLANON"
      9="RHYTHM OR SAFE PERIOD"
      10="CONTRACEPTIVE PATCH"
      11="VAGINAL CONTRACEPTIVE RING"
      12="IUD, COIL LOOP"
      13="SOMETHING ELSE"
      95="R USED NO METHOD; R DOES NOT KNOW IF PARTNER USED A METHOD"
      96="NO METHOD USED AT LAST SEX IN THE PAST 12 MONTHS" ;
   value METH3MF
      1="CONDOM"
      2="WITHDRAWAL"
      3="VASECTOMY"
      4="PILL"
      5="FEMALE STERILIZATION"
      6="INJECTION -- DEPO-PROVERA/LUNELLE"
      7="SPERMICIDAL FOAM/JELLY/CREAM/FILM/SUPPOSITORY"
      8="HORMONAL IMPLANT -- NORPLANT/IMPLANON/NEXPLANON"
      9="RHYTHM OR SAFE PERIOD"
      10="CONTRACEPTIVE PATCH"
      11="VAGINAL CONTRACEPTIVE RING"
      12="IUD, COIL LOOP"
      13="SOMETHING ELSE"
      95="R USED NO METHOD; R DOES NOT KNOW IF PARTNER USED A METHOD"
      96="NO METHOD USED AT LAST SEX IN THE PAST 3 MONTHS" ;
   value METRO
      1="PRINCIPAL CITY OF MSA"
      2="OTHER MSA"
      3="NOT MSA" ;
   value MILESFMT
      0-5="5 miles or less"
      6-25="6-25 miles"
      26-50="26-50 miles"
      51-100="51-100 miles"
      101-12000="Over 100 miles"
      99998="Refused"
      99999="Don't know" ;
   value MLFMNAF2D
      1="Male"
      2="Female"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value MNTHFMT
      1-12="January-December"
      13="Winter"
      14="Spring"
      15="Summer"
      16="Fall"
      98="Refused"
      99="Don't know" ;
   value MNYFSTER
      1="1 setting or location"
      2="2 settings or locations"
      3="3 settings or locations"
      4-20="4 or more settings or locations"
      98="Refused"
      99="Don't know" ;
   value MOM18F
      1="Under 18"
      2="18-19"
      3="20-24"
      4="25 or older"
      8="Refused"
      9="Don't know" ;
   value MOMFSTCH
      1="LESS THAN 18 YEARS"
      2="18-19 YEARS"
      3="20-24 YEARS"
      4="25-29 YEARS"
      5="30 OR OLDER"
      96="MOTHER-FIGURE HAD NO CHILDREN"
      98="Refused"
      99="Don't know" ;
   value MOMWORKD
      1="Full-time"
      2="Part-time"
      3="Equal amounts full-time and part-time"
      4="Not at all (for pay)"
      8="Refused"
      9="Don't know" ;
   value MON12PRT
      0="None"
      1="One"
      2="Two"
      3="Three"
      4="Four"
      5="Five"
      6="Six"
      7="Seven or more"
      8="Refused"
      9="Don't know" ;
   value MONYRF
      1="Months"
      2="Years"
      8="Refused"
      9="Don't know" ;
   value MSAMEREL
      1="Married to him"
      2="Engaged to him"
      3="Living together in a sexual relationship, but not engaged"
      4="Going with him or going steady"
      5="Going out with him once in a while"
      6="Just friends"
      7="Had just met him"
      8="Something else"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value MSMSORT12F
      1="Yes, usually"
      3="Yes, some of the time"
      5="No"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value MTHDSV1F
      1="Condom or rubber"
      2="Withdrawal or pulling out"
      3="Vasectomy or male sterilization"
      4="Pill"
      5="Tubal sterilization or other female sterilization"
      6="Injection (Depo-Provera or Lunelle)"
      7="Spermicidal foam/jelly/cream/film/suppository"
      8="Hormonal implant (Norplant, Implanon or Nexplanon)"
      9="Rhythm or safe period"
      10="Contraceptive patch (Ortho-Evra)"
      11="Vaginal contraceptive ring (Nuva Ring)"
      12="IUD, coil, or loop"
      13="Something else"
      98="Refused"
      99="Don't know" ;
   value MTHDSV2F
      1="Condom or rubber"
      2="Withdrawal or pulling out"
      3="Vasectomy or male sterilization"
      10="Something else"
      98="Refused"
      99="Don't know" ;
   value MTHDSV3F
      4="Pill"
      5="Tubal sterilization or other female sterilization"
      6="Injection (Depo-Provera or Lunelle)"
      7="Spermicidal foam/jelly/cream/film/suppository"
      8="Hormonal implant (Norplant, Implanon or Nexplanon)"
      9="Rhythm or safe period"
      10="Contraceptive patch (Ortho-Evra)"
      11="Vaginal contraceptive ring (Nuva Ring)"
      12="IUD, coil, or loop"
      13="Something else"
      98="Refused"
      99="Don't know" ;
   value MTONCEPF
      0="NOT ASCERTAINED"
      1="YES (MORE THAN ONCE)"
      2="NO (ONCE)" ;
   value N0Y1CF
      0="NO"
      1="YES" ;
   value N0Y1RDF
      0="NO"
      1="YES"
      8="Refused"
      9="Don't know" ;
   value NCALLU5F
      0="NONE"
      1="1 CHILD"
      2-95="2 OR MORE CHILDREN"
      98="Refused"
      99="Don't know" ;
   value NCHILDHH
      0="No children of respondent age 18 or younger in the household"
      1="1 of respondent's children 18 or younger in the household"
      2="2 of respondent's children 18 or younger in the household"
      3="3 or more of respondent's children 18 or younger in the household" ;
   value NCINEXCHF
      0="NONE"
      1="1 CHILD"
      2="2 CHILDREN"
      3="3 CHILDREN"
      4-95="4 OR MORE CHILDREN"
      98="Refused"
      99="Don't know" ;
   value NFORMCOH
      0="NONE"
      1="1 FORMER COHABITING PARTNER"
      2="2 FORMER COHABITING PARTNERS"
      3-95="3 OR MORE FORMER COHABITING PARTNERS"
      998="Refused"
      999="Don't know" ;
   value NFORMWIFE
      0="NONE"
      1="1 FORMER WIFE"
      2="2 FORMER WIVES"
      3="3 FORMER WIVES"
      4="4 FORMER WIVES"
      998="Refused"
      999="Don't know" ;
   value NNONMONOG1F
      1="1 partner"
      2="2 or more partners"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value NNONMONOG2F
      1="1 other partner besides you"
      2="2 other partners besides you"
      3="3 or more other partners besides you"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value NOHIVTST
      1="You have never been offered an HIV test"
      2="You are worried about what other people would think if you got tested for HIV"
      3="It's unlikely you've been exposed to HIV"
      4="You were afraid to find out if you were HIV positive (that you had HIV)"
      5="You don't like needles"
      20="Some other reason"
      21="R reported spouse or partner tested negative"
      22="Never had sexual intercourse"
      23="No health insurance or coverage, or R couldn't afford an HIV test"
      24="Part of blood donation"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value NONLIVEB
      0="NONE"
      1="1 PREGNANCY"
      2="2 PREGNANCIES"
      3-95="3 OR MORE PREGNANCIES"
      98="REFUSED"
      99="DON'T KNOW" ;
   value NRACE
      1="Hispanic"
      2="Non-Hispanic White, Single Race"
      3="Non-Hispanic Black, Single Race"
      4="Non-Hispanic Other or Multiple Race"
      5="NA/DK/RF" ;
   value NRCHSUPPYR
      0="none/amt not reported"
      1="1-3000"
      2="3001-5000"
      3="5001-9000"
      4="9001+" ;
   value NREG
      1="Regular basis"
      2="Once in a while"
      8="Refused"
      9="Don't know" ;
   value NRSATVISF
      0="Very dissatisfied"
      1="1"
      2="2"
      3="3"
      4="4"
      5="5"
      6="6"
      7="7"
      8="8"
      9="9"
      10="Very satisfied"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value NUMCOH2F
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS"
      4="4 PARTNERS"
      5="5 PARTNERS"
      6-95="6 PARTNERS OR MORE"
      98="Refused"
      99="Don't know" ;
   value NUMCOHAB
      0="NO PARTNERS"
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS"
      4="4 PARTNERS"
      5="5 PARTNERS"
      6-95="6 PARTNERS OR MORE"
      998="Refused"
      999="Don't know" ;
   value NUMF
      1="1 TIME"
      2="2 TIMES"
      3="3 TIMES"
      4-95="4 OR MORE TIMES"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value NUMFMHH
      0="NO FAMILY MEMBERS"
      1="1 FAMILY MEMBER"
      2="2 FAMILY MEMBERS"
      3="3 FAMILY MEMBERS"
      4="4 FAMILY MEMBERS"
      5="5 FAMILY MEMBERS"
      6="6 FAMILY MEMBERS"
      7="7 FAMILY MEMBERS OR MORE" ;
   value NUMKDHH
      0="NO CHILDREN"
      1="1 CHILD"
      2="2 CHILDREN"
      3="3 CHILDREN"
      4="4 CHILDREN OR MORE" ;
   value NUMLIFE
      7-10="7-10 PARTNERS"
      11-15="11-15 PARTNERS"
      16-20="16-20 PARTNERS"
      21-49="21-49 PARTNERS"
      50="50 OR MORE PARTNERS"
      998="Refused"
      999="Don't know" ;
   value NUMNEVF
      0="NEVER"
      1="1 TIME"
      2="2 TIMES"
      3="3 TIMES"
      4-95="4 OR MORE TIMES"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value NUMNOCOV
      1="1 Month"
      2="2 Months"
      3="3 Months"
      4="4 Months"
      5="5 Months"
      6="6 Months"
      7="7 Months"
      8="8 Months"
      9="9 Months"
      10="10 Months"
      11="11 Months"
      12="12 Months"
      98="Refused"
      99="Don't know" ;
   value NUMP3MOS
      0="0 PARTNERS"
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS EXACTLY"
      4="3, POSSIBLY MORE PARTNERS" ;
   value NUMPARTF
      0="NONE"
      1="1 partner"
      2="2 partners"
      3="3 partners"
      4="4 partners"
      5="5 partners"
      6="6 partners"
      7="7 or more partners"
      8="Refused"
      9="Don't know" ;
   value NUMVISIT
      1="1 VISIT"
      2="2 VISITS"
      3="3 VISITS"
      4="4 VISITS"
      5-95="5 OR MORE VISITS"
      98="Refused"
      99="Don't know" ;
   value NUMWIFE
      0="NEVER MARRIED"
      1="1 TIME MARRIED"
      2="2 TIMES MARRIED"
      3="3 TIMES MARRIED"
      4-95="MARRIED 4 TIMES OR MORE"
      998="Refused"
      999="Don't know" ;
   value OFTCHLDF
      1="Not at all"
      2="Less than once a week"
      3="About once a week"
      4="Several times a week"
      5="Every day (at least once a day)"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value OFTIMEF
      1="Every time"
      2="Most of the time"
      3="About half of the time"
      4="Some of the time"
      5="None of the time"
      8="Refused"
      9="Don't know" ;
   value ORIENT_A
      1="Heterosexual or straight"
      2="Homosexual or gay"
      3="Bisexual"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value ORIENT_B
      1="Gay"
      2="Straight, that is, not gay"
      3="Bisexual"
      4="Something else"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value OTACHILN
      1="1 child"
      2-95="2 or more children"
      98="Refused"
      99="Don't know" ;
   value OTLOSSN
      0="NONE"
      1-95="1 OR MORE PREGNANCIES"
      98="Refused"
      99="Don't know" ;
   value OTN2BNUM
      1-95="1 or more children"
      98="Refused"
      99="Don't know" ;
   value OTPRGEND
      1="Miscarriage"
      2="Stillbirth"
      3="Abortion"
      8="Refused"
      9="Don't know" ;
   value OTPRGN
      1="1 PREGNANCY"
      2="2 PREGNANCIES"
      3-95="3 OR MORE PREGNANCIES"
      98="Refused"
      99="Don't know" ;
   value OTPRGS
      0="NONE"
      1="1 PREGNANCY"
      2="2 PREGNANCIES"
      3-95="3 OR MORE PREGNANCIES"
      98="Refused"
      99="Don't know" ;
   value PARAGE14F
      1="R LIVED WITH BOTH BIOLOGICAL OR BOTH ADOPTIVE PARENTS AT AGE 14"
      2="R LIVED WITH BIOLOGICAL MOTHER AND STEPFATHER AT AGE 14"
      3="R LIVED IN ANY OTHER PARENTAL SITUATION OR A NONPARENTAL SITUATION AT AGE 14" ;
   value PARTDUR
      0-995="Number of months between first and most recent sex with this partner"
      997="Only had sex once with partner" ;
   value PARTLIF
      0="NONE"
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS"
      4="4 PARTNERS"
      5="5 PARTNERS"
      6="6 PARTNERS"
      7="7 PARTNERS"
      8="8 PARTNERS"
      9="9 PARTNERS"
      10="10 PARTNERS"
      11-19="11-19 PARTNERS"
      20-49="20-49 PARTNERS"
      50="50 OR MORE PARTNERS"
      997="Not ascertained"
      998="Refused"
      999="Don't know" ;
   value PARTLIF2F
      0="NONE"
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS"
      4="4 PARTNERS"
      5="5 PARTNERS"
      6="6 PARTNERS"
      7="7 PARTNERS"
      8="8 PARTNERS"
      9="9 PARTNERS"
      10="10 PARTNERS"
      11-19="11-19 PARTNERS"
      20-49="20-49 PARTNERS"
      50-995="50 OR MORE PARTNERS"
      997="Not ascertained"
      998="Refused"
      999="Don't know" ;
   value PARTS12MB
      0="0 PARTNERS"
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS"
      4="4 PARTNERS"
      5="5 PARTNERS"
      6="6 PARTNERS"
      7="7 PARTNERS"
      8="8 PARTNERS"
      9="9 PARTNERS"
      10="10 PARTNERS"
      11-19="11-19 PARTNERS"
      20="20 OR MORE PARTNERS"
      997="Not ascertained"
      998="Refused"
      999="Don't know" ;
   value PARTS1YR
      0="NONE"
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS"
      4="4 PARTNERS"
      5="5 PARTNERS"
      6="6 PARTNERS"
      7="7 OR MORE PARTNERS" ;
   value PLCHIV
      1="Private doctor's office"
      2="HMO facility"
      3="Community health clinic, community clinic, public health clinic"
      4="Family planning or Planned Parenthood clinic"
      5="Employer or company clinic"
      6="School or school-based clinic (including college or university)"
      7="Hospital outpatient clinic"
      8="Hospital emergency room"
      9="Hospital regular room"
      10="Urgent care center, urgi-care, or walk-in facility"
      11="Your worksite"
      12="Your home"
      13="Military induction or military service site"
      14="Sexually transmitted disease (STD) clinic"
      15="Laboratory or blood bank"
      20="Some other place not shown separately"
      21="Prison or jail"
      22="Mobile testing or community testing site"
      23="Drug, alcohol or rehabilitation treatment center"
      98="Refused"
      99="Don't know" ;
   value PLCSTROP
      1="Private doctor's office"
      2="HMO Facility"
      3="Community health clinic, community clinic, public health clinic"
      4="Family planning or Planned Parenthood clinic"
      5="Employer or company clinic"
      6="School or school-based clinic"
      7="Hospital outpatient clinic"
      8="Hospital emergency room"
      9="Hospital regular room"
      10="Urgent care center, urgi-care, or walk-in facility"
      11="In-store health clinic (like CVS, Target, or Walmart)"
      20="Some other place"
      98="Refused"
      99="Don't know" ;
   value PLCVISF
      1="Private doctor's office or HMO"
      2="Community health clinic, community clinic, public health clinic"
      3="Family planning or Planned Parenthood Clinic"
      4="Employer or company clinic"
      5="School or school-based clinic"
      6="Hospital outpatient clinic"
      7="Hospital emergency room"
      8="Hospital regular room"
      9="Urgent care center, urgi-care, or walk-in facility"
      10="Sexually transmitted disease (STD) clinic"
      11="In-store health clinic (like CVS, Target, or Walmart)"
      20="Some other place"
      98="Refused"
      99="Don't know" ;
   value POVERTY
      0-99="0-99 percent of poverty level"
      100-199="100-199 percent of poverty level"
      200-299="200-299 percent of poverty level"
      300-399="300-399 percent of poverty level"
      400-499="400-499 percent of poverty level"
      500="500 percent of poverty level or greater" ;
   value PREGFMT
      0="NONE"
      1="1 PREGNANCY"
      2-95="2 OR MORE PREGNANCIES" ;
   value PREGSNOW
      0="INAPP/DK/RF"
      1="1 woman"
      2="2 women"
      3="3 women"
      98="Refused"
      99="Don't know" ;
   value PRIMLANGF
      1="English"
      2="Spanish"
      7="Other"
      8="Refused"
      9="Don't know" ;
   value PROBWANT
      1="Probably want"
      2="Probably do not want"
      8="Refused"
      9="Don't know" ;
   value PRT12F
      0="0 PARTNERS"
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS"
      4="4 PARTNERS"
      5="5 PARTNERS"
      6="6 OR MORE PARTNERS"
      997="Not ascertained"
      998="Refused"
      999="Don't know" ;
   value PRT12F2F
      0="0 PARTNERS"
      1="1 PARTNER"
      2="2 PARTNERS"
      3="3 PARTNERS"
      4="4 PARTNERS"
      5="5 PARTNERS"
      6-995="6 OR MORE PARTNERS"
      997="Not ascertained"
      998="Refused"
      999="Don't know" ;
   value PRTAGE2NAF
      1="Older"
      2="Younger"
      3="Same age"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value PSTRSTAT
      0="NOT STERILE"
      1="SURGICALLY STERILE"
      2="NONSURGICALLY STERILE" ;
   value PUBASSIS
      1="Yes (received public assistance in [INTERVIEW YEAR -1])"
      2="No (did not receive public assistance in [INTERVIEW YEAR -1])" ;
   value PUBASTYPF
      1="Public assist prog, e.g. AFDC or ADC"
      2="General/Emergency/Other assistance"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value PXANYCHN
      1="1 CHILD"
      2="2 CHILDREN"
      3-95="3 OR MORE CHILDREN"
      98="Refused"
      99="Don't know" ;
   value PXCHILDN
      1="1 child"
      2="2 children"
      3-95="3 or more children"
      98="Refused"
      99="Don't know" ;
   value PXRELATF
      1="CURRENT WIFE, NOT SEPARATED"
      2="CURRENT WIFE, SEPARATED"
      3="CURRENT COHABITING PARTNER"
      4="FORMER WIFE"
      5="FORMER COHABITING PARTNER"
      6="NEVER IN A MARITAL OR COHAB UNION WITH P"
      8="Refused"
      9="Don't know" ;
   value R_FOSTER
      1="YES FOSTER"
      3="NO FOSTER, NOT INTACT"
      5="NO FOSTER, INTACT" ;
   value RACE
      1="BLACK"
      2="WHITE"
      3="OTHER" ;
   value RACEFMT
      1="Black"
      2="White"
      3="Other"
      4="NA/DK/RF" ;
   value REACTSLF
      1="Very upset"
      2="A little upset"
      3="A little pleased"
      4="Very pleased"
      5="If R insists: He wouldn't care"
      8="Refused"
      9="Don't know" ;
   value RECNEVKNF
      1="Don't recall"
      2="Never knew"
      8="Refused"
      9="Don't know" ;
   value RELATBIOF
      1="Current wife"
      2="Current cohabiting partner"
      3="Recent or last partner (up to 3 most recent in last 12 months), also an former wife or cohabiting partner"
      4="Recent or last partner (up to 3 most recent in last 12 months), not a former wife or cohabiting partner"
      5="Former wife (reported in Section E)"
      6="First cohabiting partner (reported in Section E)"
      7="Other sexual partner, not otherwise classified in codes 1-6" ;
   value RELDLIFE
      1="Very important"
      2="Somewhat important"
      3="Not important"
      8="Refused"
      9="Don't know" ;
   value RELIGION
      1="No religion"
      2="Catholic"
      3="Protestant"
      4="Other religion" ;
   value RELTRAD
      1="Evangelical Prot"
      2="Mainline Prot"
      3="Black Prot"
      4="Catholic"
      5="Other religion"
      6="No religious affiilation"
      8="Refused"
      9="Don't know" ;
   value RFSXAGEGP
      0="0"
      1="Less than 15"
      2="15 or older but less than 18"
      3="18 or older but less than 20"
      4="20 or older" ;
   value RHADSEX
      0="don't know/refused"
      1="YES"
      2="NO" ;
   value RHHIVT2F
      1="I didn't want to get tested by a doctor or at an HIV testing site"
      2="I didn't want other people to know I am getting tested"
      3="I wanted to get tested together with someone, before we had sex"
      4="I wanted to get tested by myself, before having sex"
      5="I wanted to get tested by myself, after having sex"
      6="A sex partner asked me to take a rapid home HIV test"
      20="Other reason"
      98="Refused"
      99="Don't know" ;
   value RLTNFMT
      1="Married to her"
      2="Engaged to her, and living together"
      3="Engaged to her, but not living together"
      4="Living together in a sexual relationship, but not engaged"
      5="Going with her or going steady"
      6="Going out with her once in a while"
      7="Just friends"
      8="Had just met her"
      9="Something else"
      98="Refused"
      99="Don't know" ;
   value RMARITAL
      1="CURRENTLY MARRIED TO A PERSON OF THE OPPOSITE SEX"
      2="NOT MARRIED BUT LIVING WITH OPP SEX PARTNER"
      3="WIDOWED"
      4="DIVORCED OR ANNULLED"
      5="SEPARATED FOR REASONS OF MARITAL DISCORD"
      6="NEVER BEEN MARRIED" ;
   value RNUMJOB
      0="No jobs"
      1="1 job"
      2="2 jobs"
      3="3 jobs"
      4="4 jobs"
      5="5 jobs"
      6="6 jobs"
      8="Refused"
      9="Don't know" ;
   value ROSCNT
      1="1 HOUSEHOLD MEMBER"
      2="2 HOUSEHOLD MEMBERS"
      3="3 HOUSEHOLD MEMBERS"
      4="4 HOUSEHOLD MEMBERS"
      5="5 HOUSEHOLD MEMBERS"
      6="6 HOUSEHOLD MEMBERS"
      7="7 HOUSEHOLD MEMBERS"
      8="8 OR MORE HOUSEHOLD MEMBERS"
      98="Refused"
      99="Don't know" ;
   value RSCRRACE
      1-3="Other race groups"
      4="Black or African American"
      5="White"
      6="Hispanic"
      8="Refused"
      9="Don't know" ;
   value RSTRSTAT
      0="NOT STERILE"
      1="SURGICALLY STERILE"
      2="NONSURGICALLY STERILE" ;
   value RWEIGHT
      130="130 POUNDS OR LESS"
      131-279="131-279 POUNDS"
      280="280 POUNDS OR MORE"
      997="Not ascertained"
      998="Refused"
      999="Don't know" ;
   value SCALEF
      0="Very unhappy"
      1-4="1-4"
      5="5"
      6-9="6-9"
      10="Very happy"
      98="Refused"
      99="Don't know" ;
   value SEDBFAFF
      1="Before"
      2="After"
      8="Refused"
      9="Don't know" ;
   value SEDGRDF
      1="1st grade"
      2="2nd grade"
      3="3rd grade"
      4="4th grade"
      5="5th grade"
      6="6th grade"
      7="7th grade"
      8="8th grade"
      9="9th grade"
      10="10th grade"
      11="11th grade"
      12="12th grade"
      13="1st year of college"
      14="2nd year of college"
      15="3rd year of college"
      16="4th year of college"
      96="Not in school when received instruction"
      98="Refused"
      99="Don't know" ;
   value SEX1AGEF
      0-14="UNDER 15 YEARS"
      15-17="15-17 YEARS"
      18-19="18-19 YEARS"
      20-95="20 YEARS OR OLDER"
      98="Refused"
      99="Don't know" ;
   value SEX1AGENAF
      0-14="UNDER 15 YEARS"
      15-17="15-17 YEARS"
      18-19="18-19 YEARS"
      20-95="20 YEARS OR OLDER"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value SEX1MTHDF
      1="Condom"
      2="Withdrawal"
      3="Vasectomy"
      4="Pill"
      5="Female sterilization"
      6="Injection -- Depo-Provera/Lunelle"
      7="Spermicidal foam/jelly/cream/film/suppository"
      8="Hormonal implant -- Norplant/Implanon/Nexplanon"
      9="Rhythm or safe period"
      10="Contraceptive Patch"
      11="Vaginal contraceptive ring"
      12="IUD, coil, loop"
      13="Something else"
      96="No method used at first intercourse" ;
   value SEX3MO
      1="YES, HAD INTERCOURSE"
      2="NO, DID NOT HAVE INTERCOURSE" ;
   value SEXFREQ
      0="NONE"
      1="1"
      2="2"
      3="3"
      4="4"
      5="5"
      6-10="6-10"
      11-15="11-15"
      16-20="16-20"
      21-995="21 or more"
      998="Refused"
      999="Don't know" ;
   value SEXONCE
      1="YES (R HAS HAD SEX ONLY ONCE)"
      2="NO (R HAS HAD SEX MORE THAN ONCE)" ;
   value SEXSTAT
      0="NEVER HAD SEX"
      1="1 PARTNER EVER/SEX IN LAST 12 MOS/SEX ONLY ONCE"
      2="1 PARTNER EVER/SEX IN LAST 12 MOS/SEX > ONCE"
      3="1 PARTNER EVER/NO SEX IN LAST 12 MOS/SEX ONLY ONCE"
      4="1 PARTNER EVER/NO SEX IN LAST 12 MOS/SEX > ONCE"
      5=">1 PARTNER EVER/NO SEX IN LAST 12 MOS"
      6=">1 PARTNER EVER/SEX IN LAST 12 MOS"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value SMOKE12F
      1="None"
      2="About one cigarette a day or less"
      3="Just a few cigarettes a day, between 2 to 4 cigarettes"
      4="About half a pack a day, between 5 to 14 cigarettes"
      5="About a pack a day, between 15 to 24 cigarettes"
      6="More than a pack a day, 25 or more cigarettes"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value SMSX1F
      0-14="UNDER 15 YEARS"
      15-19="15-19 YEARS"
      20-49="20-49 YEARS"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value SOONNVF
      1-100="1-100"
      998="Refused"
      999="Don't know" ;
   value SPLSTWKF
      1="Working"
      2="Working - Maternity leave or temp leave"
      3="Not working, looking for work"
      4="Keeping house or taking care of family"
      5="Other"
      8="Refused"
      9="Don't know" ;
   value SPNUMJOB
      1="1 job"
      2="2 jobs"
      3="3 jobs"
      4="4 jobs"
      5="5 jobs"
      6="6 jobs"
      8="Refused"
      9="Don't know" ;
   value SUPP12MO
      1="CONTRIBUTED CHILD SUPPORT ON A REGULAR BASIS IN LAST 12 MONTHS"
      2="CONTRIBUTED CHILD SUPPORT ONCE IN A WHILE IN LAST 12 MONTHS"
      3="DID NOT CONTRIBUTE CHILD SUPPORT IN LAST 12 MONTHS" ;
   value SVC12MOF
      1="A testicular exam (had your testicles examined)"
      2="Testing for sexually transmitted disease"
      3="Treatment for sexually transmitted disease"
      4="Information or advice about using condoms"
      5="Information or advice about your partner using female methods of birth control"
      6="Information or advice about you getting a vasectomy (surgically sterilized)"
      7="Information or advice about HIV or AIDS"
      8="Information or advice about other sexually transmitted infections, such as gonorrhea, Chlamydia, syphilis, or genital herpes"
      9="None of the above"
      98="Refused"
      99="Don't know" ;
   value SVCPAYF
      1="Insurance"
      2="Co-payment"
      3="Out-of-pocket payment (not including copay)"
      4="Medicaid"
      5="No payment required"
      6="Some other way"
      8="Refused"
      9="Don't know" ;
   value SXEDWHRINS
      1="School"
      2="Church"
      3="A community center"
      4="Some other place"
      8="Refused"
      9="Don't know" ;
   value TALKPARF
      1="How to say no to sex"
      2="Methods of birth control"
      3="Where to get birth control"
      4="Sexually transmitted diseases"
      5="How to prevent HIV/AIDS"
      6="How to use a condom"
      8="Waiting until marriage to have sex"
      95="None of the above"
      98="Refused"
      99="Don't know" ;
   value TALKSA
      1="Yes"
      5="No"
      7="If vol: Provider already knew R's status"
      8="Refused"
      9="Don't know" ;
   value TELLWGHT
      1="Underweight"
      2="Normal weight"
      3="Overweight"
      4="Obese"
      5="Not told"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value TIMALON
      1="Yes"
      5="No"
      6="Did not have a health care visit in the past 12 months"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value TIMESMAR
      1="1 TIME"
      2="2 TIMES"
      3="3 TIMES"
      4-95="4 TIMES OR MORE"
      98="Refused"
      99="Don't know" ;
   value TIMING
      1="Before first vaginal intercourse"
      3="After first vaginal intercourse"
      5="Same occasion"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value TIMINGF
      1="Too soon"
      2="Right time"
      3="Later"
      4="Didn't care"
      8="Refused"
      9="Don't know" ;
   value TOTINCR
      1="Under $5000"
      2="$5000-$7499"
      3="$7500-$9999"
      4="$10,000-$12,499"
      5="$12,500-$14,999"
      6="$15,000-$19,999"
      7="$20,000-$24,999"
      8="$25,000-$29,999"
      9="$30,000-$34,999"
      10="$35,000-$39,999"
      11="$40,000-$49,999"
      12="$50,000-$59,999"
      13="$60,000-$74,999"
      14="$75,000-$99,999"
      15="$100,000 or more" ;
   value TOTPRG
      0="NONE"
      1="1 PREGNANCY"
      2="2 PREGNANCIES"
      3="3 PREGNANCIES"
      4="4 PREGNANCIES"
      5="5 PREGNANCIES"
      6-95="6 OR MORE PREGNANCIES"
      98="Refused"
      99="Don't know" ;
   value TOTPRG2F
      0="NONE"
      1="1 PREGNANCY"
      2="2 PREGNANCIES"
      3="3 PREGNANCIES"
      4="4 PREGNANCIES"
      5="5 PREGNANCIES"
      6-95="6 OR MORE PREGNANCIES"
      998="Refused"
      999="Don't know" ;
   value TRYLONG
      0-240="0-240 months"
      998="Refused"
      999="Don't know" ;
   value TYPEOPER
      1="Vasectomy"
      2="Other operation"
      3="Vasectomy failed"
      4="Vasectomy already surgically reversed"
      8="Refused"
      9="Don't know" ;
   value TYPSEXF
      0="NONE"
      1-995="1 OR MORE PARTNERS"
      997="Not ascertained"
      998="Refused"
      999="Don't know" ;
   value UNIT30D
      1="Days per week"
      5="Days per month"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value VISIT12MOF
      1="A routine physical exam"
      2="A physical exam for sports or work"
      3="A doctor visit when you were sick or hurt"
      4="Did not have any visits to a doctor"
      8="Refused"
      9="Don't know" ;
   value VRY1STAG
      0-14="UNDER 15 YEARS"
      15-17="15-17 YEARS"
      18-19="18-19 YEARS"
      20-49="20 YEARS OR OLDER" ;
   value WANTBF
      1="LATER, OVERDUE"
      2="RIGHT TIME"
      3="TOO SOON, MISTIMED"
      4="DIDN'T CARE, INDIFFERENT"
      5="UNWANTED"
      6="DON'T KNOW, NOT SURE"
      7="R DID NOT KNOW ABOUT THE PREGNANCY LEADING TO THE BIRTH" ;
   value WANTSEX1F
      1="I really didn't want it to happen at the time"
      2="I had mixed feelings -- part of me wanted it to happen at the time and part of me didn't"
      3="I really wanted it to happen at the time"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value WGTFMT
      1924.916-106774.4="1924.916-106774.4" ;
   value WHATHAPP
      1="Did it turn out that she was pregnant and you were the father,"
      2="Or was she pregnant but you were not the father,"
      3="Or did it turn out that she was not pregnant?"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value WHENGOFP
      1="In the last 12 months"
      2="More than 12 months ago"
      8="Refused"
      9="Don't know" ;
   value WHNHIVMF
      1-12="January-December"
      13="Winter"
      14="Spring"
      15="Summer"
      16="Fall"
      97="Not ascertained"
      98="Refused"
      99="Don't know" ;
   value WHOINSEM
      1="You only"
      2="Some other donor only"
      3="Both"
      8="Refused"
      9="Don't know" ;
   value WHOSUGG
      1="Doctor or other medical care provider"
      2="Sexual partner"
      3="Someone else"
      8="Refused"
      9="Don't know" ;
   value WHYCONDF
      1="To prevent pregnancy"
      2="To prevent diseases like syphilis, gonorrhea or AIDS"
      3="For both reasons"
      4="For some other reason"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value WHYNOGET
      1="You thought the testing site would contact you"
      2="You were afraid to find out if you were HIV positive (that you had HIV)"
      3="You didn't want to know your HIV test results"
      4="You didn't know where or how to get your test result"
      20="Some other reason"
      98="Refused"
      99="Don't know" ;
   value WHYPSTD
      1="Could walk in or get same-day appointment"
      2="Cost"
      3="Privacy concern"
      4="Expert care here"
      5="Embarrassed to go to usual provider"
      6="Other"
      8="Refused"
      9="Don't know" ;
   value WOMRASDU
      1="Biological mother"
      2="Other mother figure"
      3="No mother figure"
      8="Refused"
      9="Don't know" ;
   value WOMREL
      1="WIFE"
      2="PARTNER" ;
   value WPLOCALE
      1="In household"
      2="Lives elsewhere"
      8="Refused"
      9="Don't know" ;
   value WRK12MOS
      0="No months"
      1="1 month"
      2="2 months"
      3="3 months"
      4="4 months"
      5="5 months"
      6="6 months"
      7="7 months"
      8="8 months"
      9="9 months"
      10="10 months"
      11="11 months"
      12="12 months"
      98="Refused"
      99="Don't know" ;
   value WTHPARNW
      1="Both biological or adoptive parents"
      2="Other or no parental figures"
      8="Refused"
      9="Don't know" ;
   value Y1N2C
      1="Yes"
      2="No" ;
   value Y1N2RDF
      1="Yes"
      2="No"
      8="Refused"
      9="Don't know" ;
   value Y1N2RECF
      1="YES"
      2="NO" ;
   value Y1N5C
      1="Yes"
      5="No" ;
   value Y1N5NAC
      1="YES"
      5="NO"
      7="Not ascertained" ;
   value Y1N5RDF
      1="Yes"
      5="No"
      8="Refused"
      9="Don't know" ;
   value YEARFMT
      1931-2017="1931-2017"
      9997="Not ascertained"
      9998="Refused"
      9999="Don't know" ;
   value YESNONAF
      1="Yes"
      5="No"
      7="Not ascertained"
      8="Refused"
      9="Don't know" ;
   value YNOSEX
      1="Against religion or morals"
      2="Don't want to get a female pregnant"
      3="Don't want to get a sexually transmitted disease"
      4="Haven't found the right person yet"
      5="In a relationship, but waiting for the right time"
      6="Other"
      8="Refused"
      9="Don't know" ;
   value YOUFPSVCF
      1="Physical exam"
      2="Information or advice on birth control methods, including condoms"
      3="HIV testing"
      4="Testing for sexually transmitted infection other than HIV"
      5="Treatment for sexually transmitted infection other than HIV"
      6="Some other service"
      8="Refused"
      9="Don't know" ;
   value $PHASE
      "1"="First 10 weeks of quarter"
      "2"="Last 2 weeks of quarter (double sample)" ;
   value $QUARTER
      "17"="17"
      "18"="18"
      "19"="19"
      "20"="20"
      "21"="21"
      "22"="22"
      "23"="23"
      "24"="24" ;
   value $YEARF
      "2015"="2015"
      "2016"="2016"
      "2017"="2017" ;
*/


* SAS DATA, INFILE, INPUT STATEMENTS;

DATA;
INFILE "data-filename" LRECL=4170;
INPUT
  CASEID  1-5              RSCRNINF  6              RSCRAGE  7-8          
   RSCRHISP  9              RSCRRACE  10             AGE_A  11-12          
   AGE_R  13-14             AGESCRN  15-16           HISP  17              
   HISPGRP  18              PRIMLANG1  19            PRIMLANG2  20         
   PRIMLANG3  21            ROSCNT  22               MARSTAT  23           
   FMARSTAT  24             FMARIT  25               EVRMARRY  26          
   WPLOCALE  27             WOMREL  28               GOSCHOL  29           
   VACA  30                 HIGRADE  31-32           COMPGRD  33           
   DIPGED  34               EARNHS_Y  35-38          HISCHGRD  39-40       
   LSTGRADE  41-42          MYSCHOL_Y  43-46         HAVEDEG  47           
   DEGREES  48              EARNBA_Y  49-52          EXPSCHL  53           
   EXPGRADE  54-55          WTHPARNW  56             ONOWN  57             
   ONOWN18  58              INTACT  59               PARMARR  60           
   INTACT18  61             LVSIT14F  62             LVSIT14M  63          
   WOMRASDU  64             MOMDEGRE  65             MOMWORKD  66          
   MOMFSTCH  67-68          MOM18  69                MANRASDU  70          
   R_FOSTER  71             EVRFSTER  72             MNYFSTER  73-74       
   DURFSTER  75             TIMESMAR  76             EVCOHAB1  77          
   NUMCOH1  78-79           EVCOHAB2  80             NUMCOH2  81-82        
   EVRCOHAB  83             NUMWIFE  84-86           NUMCOHAB  87-89       
   EVERSEX  90              RHADSEX  91              SXMTONCE  92          
   YNOSEX  93               TALKPAR1  94-95          TALKPAR2  96          
   TALKPAR3  97             TALKPAR4  98             TALKPAR5  99          
   TALKPAR6  100            TALKPAR7  101            SEDNO  102            
   SEDNOG  103-104          SEDNOSX  105             SEDBC  106            
   SEDBCLC1  107            SEDBCLC2  108            SEDBCLC3  109         
   SEDBCLC4  110            SEDBCG  111-112          SEDBCSX  113          
   SEDWHBC  114             SEDWHBCG  115-116        SEDWBCSX  117         
   SEDCOND  118             SEDCONDG  119-120        SEDCONSX  121         
   SEDSTD  122              SEDSTDG  123-124         SEDSTDSX  125         
   SEDHIV  126              SEDHIVG  127-128         SEDHIVSX  129         
   SEDABST  130             SEDABLC1  131            SEDABLC2  132         
   SEDABLC3  133            SEDABLC4  134            SEDABSTG  135-136     
   SEDABSSX  137            EVEROPER  138            TYPEOPER  139         
   STEROPER  140            VASEC_Y  141-144         PLCSTROP  145-146     
   RVRSVAS  147             VASREV_Y  148            RSURGSTR  149         
   FATHPOSS  150            FATHDIFF  151            RSTRSTAT  152         
   LIFEPRT  153             LIFEPRTS  154            SXMON12  155          
   MON12PRT  156            MON12PRTS  157           SEXSTAT  158          
   P12MOCONO  159           P12MOCON  160            SEXFREQ  161-163      
   CONFREQ  164-166         P1RLTN1  167             P1CURRWIFE  168       
   P1CURRSEP  169           P1RLTN2  170             P1COHABIT  171        
   P1SXLAST_M  172-173      P1SXLAST_Y  174-177      CMLSXP1  178-181      
   P2RLTN1  182             P2CURRWIFE  183          P2CURRSEP  184        
   P2RLTN2  185             P2COHABIT  186           P2SXLAST_M  187-188   
   P2SXLAST_Y  189-192      CMLSXP2  193-196         P3RLTN1  197          
   P3CURRWIFE  198          P3CURRSEP  199           P3RLTN2  200          
   P3COHABIT  201           P3SXLAST_M  202-203      P3SXLAST_Y  204-207   
   CMLSXP3  208-211         P1RELATION  212          P2RELATION  213       
   P3RELATION  214          FIRST  215               MARRDATE_Y  216-219   
   HISAGEM  220-221         LIVTOGWF  222            STRTWFCP_Y  223-226   
   HISAGEC  227-228         CMSTRTWP  229-232        ENGATHEN  233         
   WILLMARR  234            CWPDOB_Y  235-238        CWPAGE  239-240       
   CWPRACE  241             CWPNRACE  242            CWPEDUCN  243         
   CWPBORN  244             CWPMARBF  245            CWPSX1WN_M  246-247   
   CWPSX1WN_Y  248-251      CWPSX1AG  252-253        CMFSXCWP  254-257     
   AGEFSXCWP  258-259       CWPSX1RL  260-261        CWPFUSE  262          
   CWPFMET01  263-264       CWPFMET02  265-266       CWPFMET03  267-268    
   CWPFMET04  269-270       CWPFMET05  271           CWPOPSTR  272         
   CWPTYPOP1  273           CWPTYPOP2  274           CWPTOTST  275         
   CWPREVST  276            PSURGSTR  277            CWPPOSS  278          
   CWPDIFF  279             PSTRSTAT  280            CWPLSXWN_M  281-282   
   CWPLSXWN_Y  283-286      CMLSXCWP  287-290        CWPLUSE1  291         
   CWPLMET11  292-293       CWPLMET12  294-295       CWPLMET13  296        
   CWPLUSE2  297            DKCWPLUSE  298           CWPLMET201  299-300   
   CWPLMET202  301-302      DKCWPLMET  303           CWPLSXUSE  304        
   CWPRECBC  305            CWPALLBC01  306-307      CWPALLBC02  308-309   
   CWPALLBC03  310-311      CWPALLBC04  312-313      CWPALLBC05  314-315   
   CWPBCMST  316-317        CONDFREQ  318-320        CWPNOFRQ  321         
   CWPBIOKD  322            CWPNUMKD  323-324        PARTFATH  325         
   CWPCHSEX  326            CWPCHDOB_Y  327-330      CWPCHMAR  331         
   CWPCHRES  332            CWPCHLRN  333            CWPCHLIV1  334        
   CWPCHLIV2  335           CWPCHAGE  336            CWPCHSIG  337         
   CWPCHCRT  338            CWPCHGEN  339            CWPCHEVR  340         
   CWPCHFAR  341-345        CWPCHWNT  346            CWPCHSON  347         
   CWPSOONN  348-350        CWPSOONMY  351           CWPCHHPY  352-353     
   CWPCHSEX2  354           CWPCHDOB_Y2  355-358     MULTBIRT2  359        
   CWPCHMAR2  360           CWPCHRES2  361           CWPCHLRN2  362        
   CWPCHLIV10  363          CWPCHLIV11  364          CWPCHAGE2  365        
   CWPCHSIG2  366           CWPCHCRT2  367           CWPCHGEN2  368        
   CWPCHEVR2  369           CWPCHFAR2  370-374       CWPCHWNT2  375        
   CWPCHSON2  376           CWPSOONN2  377           CWPSOONMY2  378       
   CWPCHHPY2  379-380       CWPCHSEX3  381           CWPCHDOB_Y3  382-385  
   MULTBIRT3  386           CWPCHMAR3  387           CWPCHRES3  388        
   CWPCHLRN3  389           CWPCHLIV19  390          CWPCHLIV20  391       
   CWPCHAGE3  392           CWPCHSIG3  393           CWPCHCRT3  394        
   CWPCHGEN3  395           CWPCHEVR3  396           CWPCHFAR3  397-400    
   CWPCHWNT3  401           CWPCHSON3  402           CWPSOONN3  403-405    
   CWPSOONMY3  406          CWPCHHPY3  407-408       CWPCHSEX4  409        
   CWPCHDOB_Y4  410-413     MULTBIRT4  414           CWPCHMAR4  415        
   CWPCHRES4  416           CWPCHLRN4  417           CWPCHLIV28  418       
   CWPCHLIV29  419          CWPCHAGE4  420           CWPCHSIG4  421        
   CWPCHCRT4  422           CWPCHGEN4  423           CWPCHEVR4  424        
   CWPCHFAR4  425-427       CWPCHWNT4  428           CWPCHSON4  429        
   CWPSOONN4  430           CWPSOONMY4  431          CWPCHHPY4  432-433    
   CWPCHSEX5  434           CWPCHDOB_Y5  435-438     MULTBIRT5  439        
   CWPCHMAR5  440           CWPCHRES5  441           CWPCHLRN5  442        
   CWPCHLIV37  443          CWPCHLIV38  444          CWPCHAGE5  445        
   CWPCHSIG5  446           CWPCHCRT5  447           CWPCHGEN5  448        
   CWPCHEVR5  449           CWPCHFAR5  450-453       CWPCHWNT5  454        
   CWPCHSON5  455           CWPSOONN5  456           CWPSOONMY5  457       
   CWPCHHPY5  458-459       CWPCHSEX6  460           CWPCHDOB_Y6  461-464  
   MULTBIRT6  465           CWPCHMAR6  466           CWPCHRES6  467        
   CWPCHLRN6  468           CWPCHLIV46  469          CWPCHLIV47  470       
   CWPCHAGE6  471           CWPCHSIG6  472           CWPCHCRT6  473        
   CWPCHGEN6  474           CWPCHEVR6  475           CWPCHFAR6  476        
   CWPCHWNT6  477           CWPCHSON6  478           CWPSOONN6  479        
   CWPSOONMY6  480          CWPCHHPY6  481-482       CWPCHSEX7  483        
   CWPCHDOB_Y7  484-487     MULTBIRT7  488           CWPCHMAR7  489        
   CWPCHRES7  490           CWPCHLRN7  491           CWPCHLIV55  492       
   CWPCHLIV56  493          CWPCHAGE7  494           CWPCHSIG7  495        
   CWPCHCRT7  496           CWPCHGEN7  497           CWPCHEVR7  498        
   CWPCHFAR7  499           CWPCHWNT7  500           CWPCHSON7  501        
   CWPSOONN7  502           CWPSOONMY7  503          CWPCHHPY7  504-505    
   CWPPRGNW  506            CWPTRYPG  507            CWPTRYLG  508-510     
   CWPCPWNT  511            CWPCPSON  512            CWPCPSNN  513-515     
   CWPCPSNMY  516           CWPCPHPY  517-518        C_OKAKIDS  519        
   CWPOTKID  520            CWPOKNUM  521            CWPOKWTH  522         
   CWPOKWTHN  523           CWPOKSEX  524            CWPOKAD  525          
   CWPOKTRY  526            CWPOKTHR  527            CWPOKLIV1  528        
   CWPOKLIV2  529           CWPOKFAR  530-534        CWPOKAGE  535-536     
   CWPOKSEX2  537           CWPOKAD2  538            CWPOKTRY2  539        
   CWPOKTHR2  540           CWPOKLIV8  541           CWPOKLIV9  542        
   CWPOKFAR2  543-546       CWPOKAGE2  547-548       CWPOKSEX3  549        
   CWPOKAD3  550            CWPOKTRY3  551           CWPOKTHR3  552        
   CWPOKLIV15  553          CWPOKLIV16  554          CWPOKFAR3  555-558    
   CWPOKAGE3  559-560       CWPOKSEX4  561           CWPOKAD4  562         
   CWPOKTRY4  563           CWPOKTHR4  564           CWPOKLIV22  565       
   CWPOKLIV23  566          CWPOKFAR4  567-569       CWPOKAGE4  570-571    
   CWPOKSEX5  572           CWPOKAD5  573            CWPOKTRY5  574        
   CWPOKTHR5  575           CWPOKLIV29  576          CWPOKLIV30  577       
   CWPOKFAR5  578-580       CWPOKAGE5  581-582       C_NBAKIDS  583        
   CWPNBEVR  584            CWPNBNUM  585-586        CWPNBREL  587         
   CWPNBFOS  588            CWPNBSEX  589            CWPNBAD  590          
   CWPNBTRY  591            CWPNBTHR  592            CWPNBLIV1  593        
   CWPNBLIV2  594           CWPNBLIV3  595           CWPNBFAR  596-600     
   CWPNBAGE  601-602        CWPNBREL2  603           CWPNBFOS2  604        
   CWPNBSEX2  605           CWPNBAD2  606            CWPNBTRY2  607        
   CWPNBTHR2  608           CWPNBLIV8  609           CWPNBLIV9  610        
   CWPNBLIV10  611          CWPNBFAR2  612-616       CWPNBAGE2  617-618    
   CWPNBREL3  619           CWPNBFOS3  620           CWPNBSEX3  621        
   CWPNBAD3  622            CWPNBTRY3  623           CWPNBTHR3  624        
   CWPNBLIV15  625          CWPNBLIV16  626          CWPNBLIV17  627       
   CWPNBFAR3  628-631       CWPNBAGE3  632-633       CWPNBREL4  634        
   CWPNBFOS4  635           CWPNBSEX4  636           CWPNBAD4  637         
   CWPNBTRY4  638           CWPNBTHR4  639           CWPNBLIV22  640       
   CWPNBLIV23  641          CWPNBLIV24  642          CWPNBFAR4  643-645    
   CWPNBAGE4  646-647       CWPNBREL5  648           CWPNBFOS5  649        
   CWPNBSEX5  650           CWPNBAD5  651            CWPNBTRY5  652        
   CWPNBTHR5  653           CWPNBLIV29  654          CWPNBLIV30  655       
   CWPNBLIV31  656          CWPNBFAR5  657-658       CWPNBAGE5  659-660    
   CWPNBREL6  661           CWPNBFOS6  662           CWPNBSEX6  663        
   CWPNBAD6  664            CWPNBTRY6  665           CWPNBTHR6  666        
   CWPNBLIV36  667          CWPNBLIV37  668          CWPNBLIV38  669       
   CWPNBFAR6  670-671       CWPNBAGE6  672-673       CWPNBREL7  674        
   CWPNBFOS7  675           CWPNBSEX7  676           CWPNBAD7  677         
   CWPNBTRY7  678           CWPNBTHR7  679           CWPNBLIV43  680       
   CWPNBLIV44  681          CWPNBLIV45  682          CWPNBFAR7  683-684    
   CWPNBAGE7  685-686       CWPNBREL8  687           CWPNBFOS8  688        
   CWPNBSEX8  689           CWPNBAD8  690            CWPNBTRY8  691        
   CWPNBTHR8  692           CWPNBLIV50  693          CWPNBLIV51  694       
   CWPNBLIV52  695          CWPNBFAR8  696-700       CWPNBAGE8  701-702    
   CWPNBREL9  703           CWPNBFOS9  704           CWPNBSEX9  705        
   CWPNBAD9  706            CWPNBTRY9  707           CWPNBTHR9  708        
   CWPNBLIV57  709          CWPNBLIV58  710          CWPNBLIV59  711       
   CWPNBFAR9  712-716       CWPNBAGE9  717-718       CWPNBREL10  719       
   CWPNBFOS10  720          CWPNBSEX10  721          CWPNBAD10  722        
   CWPNBTRY10  723          CWPNBTHR10  724          CWPNBLIV64  725       
   CWPNBLIV65  726          CWPNBLIV66  727          CWPNBFAR10  728-732   
   CWPNBAGE10  733-734      MARDATEN_Y  735-738      AGEMARR  739-740      
   LIVTOGN  741             STRTLIVE_Y  742-745      AGELIV  746-747       
   CMUNIONP  748-751        ENGAGTHN  752            MARREND  753          
   WIFEDIED_Y  754-757      DIVORFIN_Y  758-761      ANNULLED_Y  762       
   STOPLIVE_Y  763-766      MARDATEN_Y2  767-770     AGEMARR2  771-772     
   LIVTOGN2  773            STRTLIVE_Y2  774-777     AGELIV2  778-779      
   ENGAGTHN2  780           MARREND2  781            WIFEDIED_Y2  782-785  
   DIVORFIN_Y2  786-789     ANNULLED_Y2  790         STOPLIVE_Y2  791-794  
   MARDATEN_Y3  795-798     AGEMARR3  799            LIVTOGN3  800         
   STRTLIVE_Y3  801-804     AGELIV3  805-806         ENGAGTHN3  807        
   MARREND3  808            WIFEDIED_Y3  809         DIVORFIN_Y3  810-813  
   ANNULLED_Y3  814         STOPLIVE_Y3  815-818     CURRPRTS  819         
   PXCURR  820              PXCURRPRT  821           PXMARRY  822          
   PXCURR2  823             PXCURRPRT2  824          PXMARRY2  825         
   PXCURR3  826             PXCURRPRT3  827          PXMARRY3  828         
   PXLRUSE  829             PXLRMETH1  830-831       PXLRMETH2  832-833    
   PXLRMETH3  834-835       PXLPUSE  836             DKPXLPUSE  837        
   PXLPMETH01  838-839      PXLPMETH02  840-841      PXLPMETH03  842-843   
   DKPXLPMETH  844          LSXUSEP  845             MTONCEP  846          
   PXLSXPRB  847            PXMTONCE  848            PXFRLTN1  849-850     
   P1YRACE1  851            P1YNRACE1  852           PXLRUSE2  853         
   PXLRMETH5  854-855       PXLRMETH6  856-857       PXLRMETH7  858-859    
   PXLPUSE2  860            DKPXLPUSE2  861          PXLPMETH11  862-863   
   PXLPMETH12  864-865      PXLPMETH13  866-867      DKPXLPMETH2  868      
   LSXUSEP2  869            MTONCEP2  870            PXLSXPRB2  871        
   PXMTONCE2  872           PXFRLTN3  873            P1YRACE2  874         
   P1YNRACE2  875           PXLRUSE3  876            PXLRMETH9  877-878    
   PXLRMETH10  879          PXLRMETH11  880          PXLPUSE3  881         
   DKPXLPUSE3  882          PXLPMETH21  883-884      PXLPMETH22  885       
   PXLPMETH23  886          DKPXLPMETH3  887         LSXUSEP3  888         
   MTONCEP3  889            PXLSXPRB3  890           PXMTONCE3  891        
   PXFRLTN5  892-893        P1YRACE3  894            P1YNRACE3  895        
   PXDOB_Y  896-899         PXEDUC  900              PXMARBF  901          
   PXANYCH  902             PXANYCHN  903-904        PXABLECH  905         
   PXDOB_Y2  906-909        PXEDUC2  910             PXMARBF2  911         
   PXANYCH2  912            PXANYCHN2  913           PXABLECH2  914        
   PXDOB_Y3  915-918        PXEDUC3  919             PXMARBF3  920         
   PXANYCH3  921            PXANYCHN3  922           PXABLECH3  923        
   PXSXFRST_M  924-925      PXSXFRST_Y  926-929      CMFSXP  930-933       
   AGEFSXP  934-937         PXAGFRST  938-939        PXFRLTN2  940-941     
   PXFUSE  942              PXFMETH01  943-944       PXFMETH02  945-946    
   PXFMETH03  947-948       PXFMETH04  949           PXSXFRST_M2  950-951  
   PXSXFRST_Y2  952-955     CMFSXP2  956-959         AGEFSXP2  960-963     
   PXAGFRST2  964-965       PXFRLTN4  966            PXFUSE2  967          
   PXFMETH14  968-969       PXFMETH15  970-971       PXFMETH16  972-973    
   PXFMETH17  974           PXSXFRST_M3  975-976     PXSXFRST_Y3  977-980  
   CMFSXP3  981-984         AGEFSXP3  985-988        PXAGFRST3  989-990    
   PXFRLTN6  991-992        PXFUSE3  993             PXFMETH27  994-995    
   PXFMETH28  996-997       PXFMETH29  998-999       PXFMETH30  1000       
   PXANYUSE  1001           PXMETHOD01  1002-1003    PXMETHOD02  1004-1005 
   PXMETHOD03  1006-1007    PXMETHOD04  1008-1009    PXMETHOD05  1010      
   PXMSTUSE  1011-1012      PXCONFRQ  1013-1015      PXNOFREQ  1016        
   PXANYUSE2  1017          PXMETHOD14  1018         PXMETHOD15  1019-1020 
   PXMETHOD16  1021-1022    PXMETHOD17  1023         PXMETHOD18  1024      
   PXMSTUSE2  1025-1026     PXCONFRQ2  1027-1029     PXNOFREQ2  1030       
   PXANYUSE3  1031          PXMETHOD27  1032-1033    PXMETHOD28  1034      
   PXMETHOD29  1035-1036    PXMETHOD30  1037         PXMETHOD31  1038      
   PXMSTUSE3  1039          PXCONFRQ3  1040-1042     PXNOFREQ3  1043       
   PXCHILD  1044            PXCHILDN  1045           PXCXSEX  1046         
   PXCXBORN_Y  1047-1050    MULTBIRT11  1051         PXCXMARB  1052        
   PXCXRES  1053            PXCXKNOW  1054           PXCXLIV01  1055-1056  
   PXCXLIV02  1057          PXCXLIV03  1058          PXCXAGE  1059         
   PXCXSIG  1060            PXCXCRT  1061            PXCXGEN  1062         
   PXCXEVER  1063           PXCXFAR  1064-1068       PXWANT  1069          
   PXSOON  1070             PXSOONN  1071-1072       PXSOONMY  1073        
   PXHPYPG  1074-1075       PXCXSEX2  1076           PXCXBORN_Y2  1077-1080
   MULTBIRT12  1081         PXCXMARB2  1082          PXCXRES2  1083        
   PXCXKNOW2  1084          PXCXLIV11  1085-1086     PXCXLIV12  1087       
   PXCXLIV13  1088          PXCXAGE2  1089           PXCXSIG2  1090        
   PXCXCRT2  1091           PXCXGEN2  1092           PXCXEVER2  1093       
   PXCXFAR2  1094-1098      PXWANT2  1099            PXSOON2  1100         
   PXSOONN2  1101           PXSOONMY2  1102          PXHPYPG2  1103-1104   
   PXCXSEX3  1105           PXCXBORN_Y3  1106-1109   MULTBIRT13  1110      
   PXCXMARB3  1111          PXCXRES3  1112           PXCXKNOW3  1113       
   PXCXLIV21  1114          PXCXLIV22  1115          PXCXLIV23  1116       
   PXCXAGE3  1117           PXCXSIG3  1118           PXCXCRT3  1119        
   PXCXGEN3  1120           PXCXEVER3  1121          PXCXFAR3  1122-1125   
   PXWANT3  1126            PXSOON3  1127            PXSOONN3  1128        
   PXSOONMY3  1129          PXHPYPG3  1130-1131      PXCXSEX4  1132        
   PXCXBORN_Y4  1133-1136   MULTBIRT14  1137         PXCXMARB4  1138       
   PXCXRES4  1139           PXCXKNOW4  1140          PXCXLIV31  1141       
   PXCXLIV32  1142          PXCXLIV33  1143          PXCXAGE4  1144        
   PXCXSIG4  1145           PXCXCRT4  1146           PXCXGEN4  1147        
   PXCXEVER4  1148          PXCXFAR4  1149           PXWANT4  1150         
   PXSOON4  1151            PXSOONN4  1152           PXSOONMY4  1153       
   PXHPYPG4  1154           PXCHILD2  1155           PXCHILDN2  1156       
   PXCXSEX11  1157          PXCXBORN_Y11  1158-1161   MULTBIRT21  1162      
   PXCXMARB11  1163         PXCXRES11  1164          PXCXKNOW11  1165      
   PXCXLIV101  1166         PXCXLIV102  1167         PXCXAGE11  1168       
   PXCXSIG11  1169          PXCXCRT11  1170          PXCXGEN11  1171       
   PXCXEVER11  1172         PXCXFAR11  1173-1174     PXWANT11  1175        
   PXSOON11  1176           PXSOONN11  1177          PXSOONMY11  1178      
   PXHPYPG11  1179-1180     PXCXSEX12  1181          PXCXBORN_Y12  1182-1185
   MULTBIRT22  1186         PXCXMARB12  1187         PXCXRES12  1188       
   PXCXKNOW12  1189         PXCXLIV111  1190-1191    PXCXLIV112  1192      
   PXCXAGE12  1193          PXCXSIG12  1194          PXCXCRT12  1195       
   PXCXGEN12  1196          PXCXEVER12  1197         PXCXFAR12  1198-1200  
   PXWANT12  1201           PXSOON12  1202           PXSOONN12  1203       
   PXSOONMY12  1204         PXHPYPG12  1205-1206     PXCXSEX13  1207       
   PXCXBORN_Y13  1208-1211   MULTBIRT23  1212         PXCXMARB13  1213      
   PXCXRES13  1214          PXCXKNOW13  1215         PXCXLIV121  1216      
   PXCXLIV122  1217         PXCXAGE13  1218          PXCXSIG13  1219       
   PXCXCRT13  1220          PXCXGEN13  1221          PXCXEVER13  1222      
   PXCXFAR13  1223          PXWANT13  1224           PXSOON13  1225        
   PXSOONN13  1226          PXSOONMY13  1227         PXHPYPG13  1228-1229  
   PXCXSEX14  1230          PXCXBORN_Y14  1231-1234   MULTBIRT24  1235      
   PXCXMARB14  1236         PXCXRES14  1237          PXCXKNOW14  1238      
   PXCXLIV131  1239         PXCXLIV132  1240         PXCXAGE14  1241       
   PXCXSIG14  1242          PXCXCRT14  1243          PXCXGEN14  1244       
   PXCXEVER14  1245         PXCXFAR14  1246          PXWANT14  1247        
   PXSOON14  1248           PXSOONN14  1249          PXSOONMY14  1250      
   PXHPYPG14  1251-1252     PXCHILD3  1253           PXCHILDN3  1254       
   PXCXSEX21  1255          PXCXBORN_Y21  1256-1259   MULTBIRT31  1260      
   PXCXMARB21  1261         PXCXRES21  1262          PXCXKNOW21  1263      
   PXCXLIV201  1264         PXCXLIV202  1265         PXCXAGE21  1266       
   PXCXSIG21  1267          PXCXCRT21  1268          PXCXGEN21  1269       
   PXCXEVER21  1270         PXCXFAR21  1271-1273     PXWANT21  1274        
   PXSOON21  1275           PXSOONN21  1276          PXSOONMY21  1277      
   PXHPYPG21  1278-1279     PXCXSEX22  1280          PXCXBORN_Y22  1281-1284
   MULTBIRT32  1285         PXCXMARB22  1286         PXCXRES22  1287       
   PXCXKNOW22  1288         PXCXLIV211  1289         PXCXLIV212  1290      
   PXCXAGE22  1291          PXCXSIG22  1292          PXCXCRT22  1293       
   PXCXGEN22  1294          PXCXEVER22  1295         PXCXFAR22  1296-1297  
   PXWANT22  1298           PXSOON22  1299           PXSOONN22  1300       
   PXSOONMY22  1301         PXHPYPG22  1302-1303     PXCXSEX23  1304       
   PXCXBORN_Y23  1305-1308   MULTBIRT33  1309         PXCXMARB23  1310      
   PXCXRES23  1311          PXCXKNOW23  1312         PXCXLIV221  1313      
   PXCXLIV222  1314         PXCXAGE23  1315          PXCXSIG23  1316       
   PXCXCRT23  1317          PXCXGEN23  1318          PXCXEVER23  1319      
   PXCXFAR23  1320          PXWANT23  1321           PXSOON23  1322        
   PXSOONN23  1323          PXSOONMY23  1324         PXHPYPG23  1325       
   PXCXSEX24  1326          PXCXBORN_Y24  1327-1330   MULTBIRT34  1331      
   PXCXMARB24  1332         PXCXRES24  1333          PXCXKNOW24  1334      
   PXCXLIV231  1335         PXCXLIV232  1336         PXCXAGE24  1337       
   PXCXSIG24  1338          PXCXCRT24  1339          PXCXGEN24  1340       
   PXCXEVER24  1341         PXCXFAR24  1342          PXWANT24  1343        
   PXSOON24  1344           PXSOONN24  1345          PXSOONMY24  1346      
   PXHPYPG24  1347          PXCXSEX25  1348          PXCXBORN_Y25  1349-1352
   MULTBIRT35  1353         PXCXMARB25  1354         PXCXRES25  1355       
   PXCXKNOW25  1356         PXCXLIV241  1357         PXCXLIV242  1358      
   PXCXAGE25  1359          PXCXSIG25  1360          PXCXCRT25  1361       
   PXCXGEN25  1362          PXCXEVER25  1363         PXCXFAR25  1364       
   PXWANT25  1365           PXSOON25  1366           PXSOONN25  1367       
   PXSOONMY25  1368         PXHPYPG25  1369          PXCPREG  1370         
   PXTRYING  1371           PXTRYLONG  1372-1373     PXRWANT  1374         
   PXRSOON  1375            PXRSOONN  1376           PXRSOONMY  1377       
   PXCPFEEL  1378-1379      PXCPREG2  1380           PXTRYING2  1381       
   PXTRYLONG2  1382         PXRWANT2  1383           PXRSOON2  1384        
   PXRSOONN2  1385-1386     PXRSOONMY2  1387         PXCPFEEL2  1388       
   PXCPREG3  1389           PXTRYING3  1390          PXTRYLONG3  1391      
   PXRWANT3  1392           PXRSOON3  1393           PXRSOONN3  1394       
   PXRSOONMY3  1395         PXCPFEEL3  1396          CURRPREG  1397        
   D_OKAKIDS  1398          PXOTKID  1399            PXOKNUM  1400         
   PXOKWTH  1401            PXOKWTHN  1402           PXOKSEX  1403         
   PXOKAD  1404             PXOKLIV1  1405           PXOKFAR  1406         
   PXOKAGE  1407-1408       PXOKSEX2  1409           PXOKAD2  1410         
   PXOKLIV9  1411           PXOKFAR2  1412           PXOKAGE2  1413-1414   
   PXOKSEX3  1415           PXOKAD3  1416            PXOKLIV17  1417       
   PXOKFAR3  1418           PXOKAGE3  1419-1420      PXOKSEX4  1421        
   PXOKAD4  1422            PXOKLIV25  1423          PXOKFAR4  1424        
   PXOKAGE4  1425-1426      D_OKAKIDS2  1427         PXOTKID2  1428        
   PXOKNUM2  1429           PXOKWTH2  1430           PXOKWTHN2  1431       
   PXOKSEX11  1432          PXOKAD11  1433           PXOKLIV81  1434       
   PXOKFAR11  1435-1437     PXOKAGE11  1438-1439     PXOKSEX12  1440       
   PXOKAD12  1441           PXOKLIV89  1442          PXOKFAR12  1443       
   PXOKAGE12  1444-1445     PXOKSEX13  1446          PXOKAD13  1447        
   PXOKLIV97  1448          PXOKFAR13  1449          PXOKAGE13  1450-1451  
   PXOKSEX14  1452          PXOKAD14  1453           PXOKLIV105  1454      
   PXOKFAR14  1455          PXOKAGE14  1456          PXOKSEX15  1457       
   PXOKAD15  1458           PXOKLIV113  1459         PXOKFAR15  1460       
   PXOKAGE15  1461          D_OKAKIDS3  1462         PXOTKID3  1463        
   PXOKNUM3  1464           PXOKWTH3  1465           PXOKWTHN3  1466       
   PXOKSEX21  1467          PXOKAD21  1468           PXOKAGE21  1469-1470  
   PXOKSEX22  1471          PXOKAD22  1472           PXOKAGE22  1473-1474  
   PXOKSEX23  1475          PXOKAD23  1476           PXOKAGE23  1477-1478  
   PXOKSEX24  1479          PXOKAD24  1480           PXOKAGE24  1481-1482  
   PXOKSEX25  1483          PXOKAD25  1484           PXOKAGE25  1485-1486  
   D_NBAKIDS  1487          PXNBEVR  1488            PXNBNUM  1489         
   PXNBREL  1490            PXNBFOS  1491            PXNBSEX  1492         
   PXNBAD  1493             PXNBLIV1  1494           PXNBLIV2  1495        
   PXNBFAR  1496-1499       PXNBAGE  1500-1501       PXNBREL2  1502        
   PXNBFOS2  1503           PXNBSEX2  1504           PXNBAD2  1505         
   PXNBLIV9  1506           PXNBLIV10  1507          PXNBFAR2  1508        
   PXNBAGE2  1509-1510      PXNBREL3  1511           PXNBFOS3  1512        
   PXNBSEX3  1513           PXNBAD3  1514            PXNBLIV17  1515       
   PXNBLIV18  1516          PXNBFAR3  1517           PXNBAGE3  1518        
   D_NBAKIDS2  1519         PXNBEVR2  1520           PXNBNUM2  1521        
   PXNBREL11  1522          PXNBFOS11  1523          PXNBSEX11  1524       
   PXNBAD11  1525           PXNBAGE11  1526-1527     PXNBREL12  1528       
   PXNBFOS12  1529          PXNBSEX12  1530          PXNBAD12  1531        
   PXNBAGE12  1532-1533     PXNBREL13  1534          PXNBFOS13  1535       
   PXNBSEX13  1536          PXNBAD13  1537           PXNBAGE13  1538-1539  
   PXNBREL14  1540          PXNBFOS14  1541          PXNBSEX14  1542       
   PXNBAD14  1543           PXNBAGE14  1544-1545     PXNBREL15  1546       
   PXNBFOS15  1547          PXNBSEX15  1548          PXNBAD15  1549        
   PXNBAGE15  1550-1551     PXNBREL16  1552          PXNBFOS16  1553       
   PXNBSEX16  1554          PXNBAD16  1555           PXNBAGE16  1556-1557  
   PXNBREL17  1558          PXNBFOS17  1559          PXNBSEX17  1560       
   PXNBAD17  1561           PXNBAGE17  1562-1563     PXNBREL18  1564       
   PXNBFOS18  1565          PXNBSEX18  1566          PXNBAD18  1567        
   PXNBAGE18  1568-1569     D_NBAKIDS3  1570         PXNBEVR3  1571        
   FPFIRST_M  1572-1573     FPFIRST_Y  1574-1577     CMFSTSEX  1578-1581   
   FSTSEXAGE  1582-1583     FPAGE  1584-1585         FPAGE18  1586         
   FPAGE15  1587            FPAGE20  1588            RFSXAGEGP  1589       
   FPRLTN  1590-1591        FPUSE  1592              FPMETH01  1593-1594   
   FPMETH02  1595-1596      FPMETH03  1597           FPMETH04  1598        
   FPPROBE  1599            NFORMWIFE  1600-1602     NFORMCOHAB  1603-1605 
   FWVERIFY  1606           FWVER  1607              FWVERIFY2  1608       
   FWVER2  1609             FWVERIFY3  1610          FWVER3  1611          
   FWVERIFY4  1612          FWVER4  1613             FCVER  1614           
   FCVERIFY  1615           EXRELATION  1616         FWMAREND_Y  1617-1620 
   AGEMARRN  1621-1622      LIVTOGN4  1623           STRTLIVE_Y4  1624-1627
   AGELIV4  1628-1629       CMUNIONW  1630-1633      ENGAGTHN4  1634       
   MARREND4  1635           WIFEDIED_Y4  1636-1639   DIVORFIN_Y4  1640-1643
   ANNULLED_Y4  1644-1647   STOPLIVE_Y4  1648-1651   EXRELATION2  1652     
   FWMAREND_Y2  1653-1656   AGEMARRN2  1657-1658     LIVTOGN5  1659        
   STRTLIVE_Y5  1660-1663   AGELIV5  1664-1665       ENGAGTHN5  1666       
   MARREND5  1667           WIFEDIED_Y5  1668-1671   DIVORFIN_Y5  1672-1675
   ANNULLED_Y5  1676        STOPLIVE_Y5  1677-1680   EXRELATION3  1681     
   FWMAREND_Y3  1682-1685   AGEMARRN3  1686-1687     LIVTOGN6  1688        
   STRTLIVE_Y6  1689-1692   AGELIV6  1693-1694       ENGAGTHN6  1695       
   MARREND6  1696           WIFEDIED_Y6  1697-1700   DIVORFIN_Y6  1701-1704
   ANNULLED_Y6  1705        STOPLIVE_Y6  1706-1709   EXRELATION4  1710     
   FWMAREND_Y4  1711-1714   AGEMARRN4  1715          LIVTOGN7  1716        
   STRTLIVE_Y7  1717-1720   AGELIV7  1721            ENGAGTHN7  1722       
   MARREND7  1723           WIFEDIED_Y7  1724        DIVORFIN_Y7  1725-1728
   ANNULLED_Y7  1729        STOPLIVE_Y7  1730-1733   EXRELATION11  1734    
   STRTLIVE_Y14  1735-1738   CMCOHFC11  1739-1742     AGELIV14  1743-1744   
   ENGAGTHN14  1745         STOPLIVE_Y14  1746-1749   FWPDOB_Y  1750-1753   
   FWPAGE  1754-1755        WIF1RACE  1756           WIF1NRACE  1757       
   FWPMARBF  1758           FWPDOB_Y2  1759-1762     FWPAGE2  1763-1764    
   FWPMARBF2  1765          FWPDOB_Y3  1766-1769     FWPAGE3  1770-1771    
   FWPMARBF3  1772          FWPDOB_Y4  1773-1776     FWPAGE4  1777         
   FWPMARBF4  1778          FWPDOB_Y11  1779-1782    FWPAGE11  1783-1784   
   COH1RACE  1785           COH1NRACE  1786          FWPMARBF11  1787      
   FWPBIOKD  1788           FWPNUMKD  1789           FWPCHSEX  1790        
   FWPCHDOB_Y  1791-1794    FWCHMARB  1795           FWPCHRES  1796        
   FWPCHLRN  1797           FWPCHLIV01  1798-1799    FWPCHLIV02  1800      
   FWPCHLIV03  1801         FWPCHAGE  1802           FWPCHSIG  1803        
   FWPCHCRT  1804           FWPCHGEN  1805           FWPCHEVR  1806        
   FWPCHFAR  1807-1811      FWPRWANT  1812           FWPSOON  1813         
   FWPSOONN  1814-1816      FWPSOONMY  1817          FWPHPYPG  1818-1819   
   FWPCHSEX2  1820          FWPCHDOB_Y2  1821-1824   MULTBIRT42  1825      
   FWCHMARB2  1826          FWPCHRES2  1827          FWPCHLRN2  1828       
   FWPCHLIV11  1829-1830    FWPCHLIV12  1831         FWPCHLIV13  1832      
   FWPCHAGE2  1833          FWPCHSIG2  1834          FWPCHCRT2  1835       
   FWPCHGEN2  1836          FWPCHEVR2  1837          FWPCHFAR2  1838-1842  
   FWPRWANT2  1843          FWPSOON2  1844           FWPSOONN2  1845-1847  
   FWPSOONMY2  1848         FWPHPYPG2  1849-1850     FWPCHSEX3  1851       
   FWPCHDOB_Y3  1852-1855   MULTBIRT43  1856         FWCHMARB3  1857       
   FWPCHRES3  1858          FWPCHLRN3  1859          FWPCHLIV21  1860-1861 
   FWPCHLIV22  1862         FWPCHLIV23  1863         FWPCHAGE3  1864       
   FWPCHSIG3  1865          FWPCHCRT3  1866          FWPCHGEN3  1867       
   FWPCHEVR3  1868          FWPCHFAR3  1869-1873     FWPRWANT3  1874       
   FWPSOON3  1875           FWPSOONN3  1876-1877     FWPSOONMY3  1878      
   FWPHPYPG3  1879-1880     FWPCHSEX4  1881          FWPCHDOB_Y4  1882-1885
   MULTBIRT44  1886         FWCHMARB4  1887          FWPCHRES4  1888       
   FWPCHLRN4  1889          FWPCHLIV31  1890         FWPCHLIV32  1891      
   FWPCHLIV33  1892         FWPCHAGE4  1893          FWPCHSIG4  1894       
   FWPCHCRT4  1895          FWPCHGEN4  1896          FWPCHEVR4  1897       
   FWPCHFAR4  1898-1901     FWPRWANT4  1902          FWPSOON4  1903        
   FWPSOONN4  1904          FWPSOONMY4  1905         FWPHPYPG4  1906-1907  
   FWPCHSEX5  1908          FWPCHDOB_Y5  1909-1912   MULTBIRT45  1913      
   FWCHMARB5  1914          FWPCHRES5  1915          FWPCHLRN5  1916       
   FWPCHLIV41  1917         FWPCHLIV42  1918         FWPCHLIV43  1919      
   FWPCHAGE5  1920          FWPCHSIG5  1921          FWPCHCRT5  1922       
   FWPCHGEN5  1923          FWPCHEVR5  1924          FWPCHFAR5  1925-1928  
   FWPRWANT5  1929          FWPSOON5  1930           FWPSOONN5  1931       
   FWPSOONMY5  1932         FWPHPYPG5  1933-1934     FWPBIOKD2  1935       
   FWPNUMKD2  1936          FWPCHSEX11  1937         FWPCHDOB_Y11  1938-1941
   FWCHMARB11  1942         FWPCHRES11  1943         FWPCHLRN11  1944      
   FWPCHLIV101  1945-1946   FWPCHLIV102  1947        FWPCHAGE11  1948      
   FWPCHSIG11  1949         FWPCHCRT11  1950         FWPCHGEN11  1951      
   FWPCHEVR11  1952         FWPCHFAR11  1953-1957    FWPRWANT11  1958      
   FWPSOON11  1959          FWPSOONN11  1960-1961    FWPSOONMY11  1962     
   FWPHPYPG11  1963-1964    FWPCHSEX12  1965         FWPCHDOB_Y12  1966-1969
   MULTBIRT52  1970         FWCHMARB12  1971         FWPCHRES12  1972      
   FWPCHLRN12  1973         FWPCHLIV111  1974        FWPCHLIV112  1975     
   FWPCHAGE12  1976         FWPCHSIG12  1977         FWPCHCRT12  1978      
   FWPCHGEN12  1979         FWPCHEVR12  1980         FWPCHFAR12  1981-1982 
   FWPRWANT12  1983         FWPSOON12  1984          FWPSOONN12  1985-1987 
   FWPSOONMY12  1988        FWPHPYPG12  1989-1990    FWPCHSEX13  1991      
   FWPCHDOB_Y13  1992-1995   MULTBIRT53  1996         FWCHMARB13  1997      
   FWPCHRES13  1998         FWPCHLRN13  1999         FWPCHLIV121  2000     
   FWPCHLIV122  2001        FWPCHAGE13  2002         FWPCHSIG13  2003      
   FWPCHCRT13  2004         FWPCHGEN13  2005         FWPCHEVR13  2006      
   FWPCHFAR13  2007-2008    FWPRWANT13  2009         FWPSOON13  2010       
   FWPSOONN13  2011         FWPSOONMY13  2012        FWPHPYPG13  2013-2014 
   FWPCHSEX14  2015         FWPCHDOB_Y14  2016-2019   MULTBIRT54  2020      
   FWCHMARB14  2021         FWPCHRES14  2022         FWPCHLRN14  2023      
   FWPCHLIV131  2024        FWPCHLIV132  2025        FWPCHAGE14  2026      
   FWPCHSIG14  2027         FWPCHCRT14  2028         FWPCHGEN14  2029      
   FWPCHEVR14  2030         FWPCHFAR14  2031-2032    FWPRWANT14  2033      
   FWPSOON14  2034          FWPSOONN14  2035         FWPSOONMY14  2036     
   FWPHPYPG14  2037-2038    FWPBIOKD3  2039          FWPNUMKD3  2040       
   FWPCHSEX21  2041         FWPCHDOB_Y21  2042-2045   FWCHMARB21  2046      
   FWPCHRES21  2047         FWPCHLRN21  2048         FWPCHLIV201  2049     
   FWPCHLIV202  2050        FWPCHAGE21  2051         FWPCHSIG21  2052      
   FWPCHCRT21  2053         FWPCHGEN21  2054         FWPCHEVR21  2055      
   FWPCHFAR21  2056-2058    FWPRWANT21  2059         FWPSOON21  2060       
   FWPSOONN21  2061         FWPSOONMY21  2062        FWPHPYPG21  2063-2064 
   FWPCHSEX22  2065         FWPCHDOB_Y22  2066-2069   MULTBIRT62  2070      
   FWCHMARB22  2071         FWPCHRES22  2072         FWPCHLRN22  2073      
   FWPCHLIV211  2074        FWPCHLIV212  2075        FWPCHAGE22  2076      
   FWPCHSIG22  2077         FWPCHCRT22  2078         FWPCHGEN22  2079      
   FWPCHEVR22  2080         FWPCHFAR22  2081-2083    FWPRWANT22  2084      
   FWPSOON22  2085          FWPSOONN22  2086         FWPSOONMY22  2087     
   FWPHPYPG22  2088-2089    FWPCHSEX23  2090         FWPCHDOB_Y23  2091-2094
   MULTBIRT63  2095         FWCHMARB23  2096         FWPCHRES23  2097      
   FWPCHLRN23  2098         FWPCHLIV221  2099        FWPCHLIV222  2100     
   FWPCHAGE23  2101         FWPCHSIG23  2102         FWPCHCRT23  2103      
   FWPCHGEN23  2104         FWPCHEVR23  2105         FWPCHFAR23  2106      
   FWPRWANT23  2107         FWPSOON23  2108          FWPSOONN23  2109      
   FWPSOONMY23  2110        FWPHPYPG23  2111-2112    FWPBIOKD4  2113       
   FWPNUMKD4  2114          FWPBIOKD11  2115         FWPNUMKD11  2116      
   FWPCHSEX101  2117        FWPCHDOB_Y101  2118-2121   FWPCHRES101  2122     
   FWPCHLRN101  2123        FWPCHLIV1001  2124-2125   FWPCHLIV1002  2126    
   FWPCHLIV1003  2127       FWPCHAGE101  2128        FWPCHSIG101  2129     
   FWPCHCRT101  2130        FWPCHGEN101  2131        FWPCHEVR101  2132     
   FWPCHFAR101  2133-2137   FWPRWANT101  2138        FWPSOON101  2139      
   FWPSOONN101  2140-2142   FWPSOONMY101  2143       FWPHPYPG101  2144-2145
   FWPCHSEX102  2146        FWPCHDOB_Y102  2147-2150   MULTBIRT142  2151     
   FWPCHRES102  2152        FWPCHLRN102  2153        FWPCHLIV1011  2154-2155
   FWPCHLIV1012  2156       FWPCHLIV1013  2157       FWPCHAGE102  2158     
   FWPCHSIG102  2159        FWPCHCRT102  2160        FWPCHGEN102  2161     
   FWPCHEVR102  2162        FWPCHFAR102  2163-2167   FWPRWANT102  2168     
   FWPSOON102  2169         FWPSOONN102  2170        FWPSOONMY102  2171    
   FWPHPYPG102  2172-2173   FWPCHSEX103  2174        FWPCHDOB_Y103  2175-2178
   MULTBIRT143  2179        FWPCHRES103  2180        FWPCHLRN103  2181     
   FWPCHLIV1021  2182-2183   FWPCHLIV1022  2184       FWPCHLIV1023  2185    
   FWPCHAGE103  2186        FWPCHSIG103  2187        FWPCHCRT103  2188     
   FWPCHGEN103  2189        FWPCHEVR103  2190        FWPCHFAR103  2191-2193
   FWPRWANT103  2194        FWPSOON103  2195         FWPSOONN103  2196     
   FWPSOONMY103  2197       FWPHPYPG103  2198-2199   FWPCHSEX104  2200     
   FWPCHDOB_Y104  2201-2204   MULTBIRT144  2205        FWPCHRES104  2206     
   FWPCHLRN104  2207        FWPCHLIV1031  2208       FWPCHLIV1032  2209    
   FWPCHLIV1033  2210       FWPCHAGE104  2211        FWPCHSIG104  2212     
   FWPCHCRT104  2213        FWPCHGEN104  2214        FWPCHEVR104  2215     
   FWPCHFAR104  2216-2218   FWPRWANT104  2219        FWPSOON104  2220      
   FWPSOONN104  2221        FWPSOONMY104  2222       FWPHPYPG104  2223-2224
   E_OKAKIDS  2225          FWPOTKID  2226           FWPOKNUM  2227        
   FWPOKWTH  2228           FWPOKWTHN  2229          FWPOKSEX  2230        
   FWPOKAD  2231            FWPOKLIV1  2232          FWPOKLIV2  2233       
   FWPOKFAR  2234-2237      FWPOKAGE  2238-2239      FWPOKSEX2  2240       
   FWPOKAD2  2241           FWPOKLIV9  2242          FWPOKLIV10  2243      
   FWPOKFAR2  2244-2245     FWPOKAGE2  2246-2247     FWPOKSEX3  2248       
   FWPOKAD3  2249           FWPOKLIV17  2250         FWPOKLIV18  2251      
   FWPOKFAR3  2252          FWPOKAGE3  2253-2254     FWPOKSEX4  2255       
   FWPOKAD4  2256           FWPOKLIV25  2257         FWPOKLIV26  2258      
   FWPOKFAR4  2259          FWPOKAGE4  2260-2261     FWPOKSEX5  2262       
   FWPOKAD5  2263           FWPOKLIV33  2264         FWPOKLIV34  2265      
   FWPOKFAR5  2266          FWPOKAGE5  2267-2268     E_OKAKIDS2  2269      
   FWPOTKID2  2270          FWPOKNUM2  2271          FWPOKWTH2  2272       
   FWPOKWTHN2  2273         FWPOKSEX11  2274         FWPOKAD11  2275       
   FWPOKLIV81  2276         FWPOKFAR11  2277-2279    FWPOKAGE11  2280-2281 
   FWPOKSEX12  2282         FWPOKAD12  2283          FWPOKLIV89  2284      
   FWPOKFAR12  2285-2286    FWPOKAGE12  2287-2288    FWPOKSEX13  2289      
   FWPOKAD13  2290          FWPOKLIV97  2291         FWPOKFAR13  2292-2293 
   FWPOKAGE13  2294-2295    FWPOKSEX14  2296         FWPOKAD14  2297       
   FWPOKLIV105  2298        FWPOKFAR14  2299         FWPOKAGE14  2300-2301 
   FWPOKSEX15  2302         FWPOKAD15  2303          FWPOKLIV113  2304     
   FWPOKFAR15  2305         FWPOKAGE15  2306-2307    E_OKAKIDS3  2308      
   FWPOTKID3  2309          FWPOKNUM3  2310          FWPOKWTH3  2311       
   FWPOKWTHN3  2312         FWPOKSEX21  2313         FWPOKAD21  2314       
   FWPOKLIV161  2315        FWPOKFAR21  2316         FWPOKAGE21  2317-2318 
   FWPOKSEX22  2319         FWPOKAD22  2320          FWPOKLIV169  2321     
   FWPOKFAR22  2322         FWPOKAGE22  2323-2324    E_OKAKIDS4  2325      
   FWPOTKID4  2326          FWPOKNUM4  2327          FWPOKWTH4  2328       
   FWPOKWTHN4  2329         FWPOKSEX31  2330         FWPOKAD31  2331       
   FWPOKAGE31  2332-2333    E_OKAKIDS11  2334        FWPOTKID11  2335      
   FWPOKNUM11  2336         FWPOKWTH11  2337         FWPOKWTHN11  2338     
   FWPOKSEX101  2339        FWPOKAD101  2340         FWPOKLIV801  2341-2342
   FWPOKFAR101  2343-2347   FWPOKAGE101  2348-2349   FWPOKSEX102  2350     
   FWPOKAD102  2351         FWPOKLIV809  2352-2353   FWPOKFAR102  2354-2357
   FWPOKAGE102  2358-2359   FWPOKSEX103  2360        FWPOKAD103  2361      
   FWPOKLIV817  2362-2363   FWPOKFAR103  2364-2367   FWPOKAGE103  2368-2369
   FWPOKSEX104  2370        FWPOKAD104  2371         FWPOKLIV825  2372-2373
   FWPOKFAR104  2374        FWPOKAGE104  2375-2376   FWPOKSEX105  2377     
   FWPOKAD105  2378         FWPOKLIV833  2379        FWPOKFAR105  2380     
   FWPOKAGE105  2381-2382   FWPOKSEX106  2383        FWPOKAD106  2384      
   FWPOKLIV841  2385        FWPOKFAR106  2386        FWPOKAGE106  2387-2388
   E_NBAKIDS  2389          FWPNBEVR  2390           FWPNBNUM  2391-2392   
   FWPNBREL  2393           FWPNBFOS  2394           FWPNBSEX  2395        
   FWPNBAD  2396            FWPNBLIV1  2397          FWPNBLIV2  2398       
   FWPNBFAR  2399-2402      FWPNBAGE  2403-2404      FWPNBREL2  2405       
   FWPNBFOS2  2406          FWPNBSEX2  2407          FWPNBAD2  2408        
   FWPNBLIV9  2409          FWPNBLIV10  2410         FWPNBFAR2  2411-2413  
   FWPNBAGE2  2414-2415     FWPNBREL3  2416          FWPNBFOS3  2417       
   FWPNBSEX3  2418          FWPNBAD3  2419           FWPNBLIV17  2420      
   FWPNBLIV18  2421         FWPNBFAR3  2422          FWPNBAGE3  2423-2424  
   FWPNBREL4  2425          FWPNBFOS4  2426          FWPNBSEX4  2427       
   FWPNBAD4  2428           FWPNBLIV25  2429         FWPNBLIV26  2430      
   FWPNBFAR4  2431          FWPNBAGE4  2432-2433     E_NBAKIDS2  2434      
   FWPNBEVR2  2435          FWPNBNUM2  2436          FWPNBREL11  2437      
   FWPNBFOS11  2438         FWPNBSEX11  2439         FWPNBAD11  2440       
   FWPNBLIV81  2441         FWPNBFAR11  2442         FWPNBAGE11  2443-2444 
   FWPNBREL12  2445         FWPNBFOS12  2446         FWPNBSEX12  2447      
   FWPNBAD12  2448          FWPNBLIV89  2449         FWPNBFAR12  2450      
   FWPNBAGE12  2451-2452    E_NBAKIDS3  2453         FWPNBEVR3  2454       
   FWPNBNUM3  2455          FWPNBREL21  2456         FWPNBFOS21  2457      
   FWPNBSEX21  2458         FWPNBAD21  2459          FWPNBLIV161  2460     
   FWPNBFAR21  2461-2462    FWPNBAGE21  2463-2464    FWPNBREL22  2465      
   FWPNBFOS22  2466         FWPNBSEX22  2467         FWPNBAD22  2468       
   FWPNBLIV169  2469        FWPNBFAR22  2470         FWPNBAGE22  2471-2472 
   E_NBAKIDS4  2473         FWPNBEVR4  2474          E_NBAKIDS11  2475     
   FWPNBEVR11  2476         FWPNBNUM11  2477-2478    FWPNBREL101  2479     
   FWPNBFOS101  2480        FWPNBSEX101  2481        FWPNBAD101  2482      
   FWPNBLIV801  2483        FWPNBFAR101  2484-2487   FWPNBAGE101  2488-2489
   FWPNBREL102  2490        FWPNBFOS102  2491        FWPNBSEX102  2492     
   FWPNBAD102  2493         FWPNBLIV809  2494        FWPNBFAR102  2495-2497
   FWPNBAGE102  2498-2499   FWPNBREL103  2500        FWPNBFOS103  2501     
   FWPNBSEX103  2502        FWPNBAD103  2503         FWPNBLIV817  2504     
   FWPNBFAR103  2505        FWPNBAGE103  2506-2507   FWPNBREL104  2508     
   FWPNBFOS104  2509        FWPNBSEX104  2510        FWPNBAD104  2511      
   FWPNBLIV825  2512        FWPNBFAR104  2513        FWPNBAGE104  2514-2515
   OTBCHIL  2516            OTBPROBE  2517           OTBCHILN  2518-2519   
   OTBSAME  2520            OBCSEXX  2521            OBCDOB_Y  2522-2525   
   OBCMAGEX  2526-2527      OBCMLIV  2528            OBCKNOWX  2529        
   OBCLIVEX01  2530-2531    OBCLIVEX02  2532         OBCLIVEX03  2533      
   OBCAGE  2534             OBCCHSIG  2535           OBCCHCRT  2536        
   OBCCHGEN  2537           OBCEVER  2538            OBCFAR  2539-2543     
   OBCRWANX  2544           OBCSOONX  2545           OBCSOONN  2546-2548   
   OBCSOONMY  2549          OBCHPYX  2550-2551       OBCSEXX2  2552        
   OBCDOB_Y2  2553-2556     MULTBIRT152  2557        OBCMAGEX2  2558-2559  
   OBCMLIV2  2560           OBCKNOWX2  2561          OBCLIVEX11  2562-2563 
   OBCLIVEX12  2564         OBCLIVEX13  2565         OBCAGE2  2566         
   OBCCHSIG2  2567          OBCCHCRT2  2568          OBCCHGEN2  2569       
   OBCEVER2  2570           OBCFAR2  2571-2575       OBCRWANX2  2576       
   OBCSOONX2  2577          OBCSOONN2  2578-2580     OBCSOONMY2  2581      
   OBCHPYX2  2582-2583      OBCSEXX3  2584           OBCDOB_Y3  2585-2588  
   MULTBIRT153  2589        OBCMAGEX3  2590-2591     OBCMLIV3  2592        
   OBCKNOWX3  2593          OBCLIVEX21  2594-2595    OBCLIVEX22  2596      
   OBCLIVEX23  2597         OBCAGE3  2598            OBCCHSIG3  2599       
   OBCCHCRT3  2600          OBCCHGEN3  2601          OBCEVER3  2602        
   OBCFAR3  2603-2607       OBCRWANX3  2608          OBCSOONX3  2609       
   OBCSOONN3  2610-2611     OBCSOONMY3  2612         OBCHPYX3  2613-2614   
   OBCSEXX4  2615           OBCDOB_Y4  2616-2619     MULTBIRT154  2620     
   OBCMAGEX4  2621-2622     OBCMLIV4  2623           OBCKNOWX4  2624       
   OBCLIVEX31  2625-2626    OBCLIVEX32  2627         OBCLIVEX33  2628      
   OBCAGE4  2629            OBCCHSIG4  2630          OBCCHCRT4  2631       
   OBCCHGEN4  2632          OBCEVER4  2633           OBCFAR4  2634-2638    
   OBCRWANX4  2639          OBCSOONX4  2640          OBCSOONN4  2641       
   OBCSOONMY4  2642         OBCHPYX4  2643-2644      OBCSEXX5  2645        
   OBCDOB_Y5  2646-2649     MULTBIRT155  2650        OBCMAGEX5  2651-2652  
   OBCMLIV5  2653           OBCKNOWX5  2654          OBCLIVEX41  2655-2656 
   OBCLIVEX42  2657         OBCLIVEX43  2658         OBCAGE5  2659         
   OBCCHSIG5  2660          OBCCHCRT5  2661          OBCCHGEN5  2662       
   OBCEVER5  2663           OBCFAR5  2664-2666       OBCRWANX5  2667       
   OBCSOONX5  2668          OBCSOONN5  2669          OBCSOONMY5  2670      
   OBCHPYX5  2671-2672      OBCSEXX6  2673           OBCDOB_Y6  2674-2677  
   MULTBIRT156  2678        OBCMAGEX6  2679-2680     OBCMLIV6  2681        
   OBCKNOWX6  2682          OBCLIVEX51  2683         OBCLIVEX52  2684      
   OBCLIVEX53  2685         OBCAGE6  2686            OBCCHSIG6  2687       
   OBCCHCRT6  2688          OBCCHGEN6  2689          OBCEVER6  2690        
   OBCFAR6  2691-2692       OBCRWANX6  2693          OBCSOONX6  2694       
   OBCSOONN6  2695          OBCSOONMY6  2696         OBCHPYX6  2697-2698   
   OBCSEXX7  2699           OBCDOB_Y7  2700-2703     MULTBIRT157  2704     
   OBCMAGEX7  2705-2706     OBCMLIV7  2707           OBCKNOWX7  2708       
   OBCLIVEX61  2709         OBCLIVEX62  2710         OBCLIVEX63  2711      
   OBCAGE7  2712            OBCCHSIG7  2713          OBCCHCRT7  2714       
   OBCCHGEN7  2715          OBCEVER7  2716           OBCFAR7  2717-2718    
   OBCRWANX7  2719          OBCSOONX7  2720          OBCSOONN7  2721       
   OBCSOONMY7  2722         OBCHPYX7  2723           OBCSEXX8  2724        
   OBCDOB_Y8  2725-2728     MULTBIRT158  2729        OBCMAGEX8  2730-2731  
   OBCMLIV8  2732           OBCKNOWX8  2733          OBCLIVEX71  2734      
   OBCLIVEX72  2735         OBCLIVEX73  2736         OBCAGE8  2737         
   OBCCHSIG8  2738          OBCCHCRT8  2739          OBCCHGEN8  2740       
   OBCEVER8  2741           OBCFAR8  2742-2743       OBCRWANX8  2744       
   OBCSOONX8  2745          OBCSOONN8  2746          OBCSOONMY8  2747      
   OBCHPYX8  2748           OBCSEXX9  2749           OBCDOB_Y9  2750-2753  
   MULTBIRT159  2754        OBCMAGEX9  2755-2756     OBCMLIV9  2757        
   OBCKNOWX9  2758          OBCLIVEX81  2759         OBCLIVEX82  2760      
   OBCLIVEX83  2761         OBCAGE9  2762            OBCCHSIG9  2763       
   OBCCHCRT9  2764          OBCCHGEN9  2765          OBCEVER9  2766        
   OBCFAR9  2767-2769       OBCRWANX9  2770          OBCSOONX9  2771       
   OBCSOONN9  2772          OBCSOONMY9  2773         OBCHPYX9  2774        
   OBCSEXX10  2775          OBCDOB_Y10  2776-2779    MULTBIRT160  2780     
   OBCMAGEX10  2781-2782    OBCMLIV10  2783          OBCKNOWX10  2784      
   OBCLIVEX91  2785         OBCLIVEX92  2786         OBCLIVEX93  2787      
   OBCAGE10  2788           OBCCHSIG10  2789         OBCCHCRT10  2790      
   OBCCHGEN10  2791         OBCEVER10  2792          OBCFAR10  2793-2795   
   OBCRWANX10  2796         OBCSOONX10  2797         OBCSOONN10  2798      
   OBCSOONMY10  2799        OBCHPYX10  2800          F_AKIDS  2801         
   OTACHIL  2802            OTACHILN  2803-2804      OTNBREL  2805         
   OTNBFOS  2806            OTNBSEX  2807            OTNBAD  2808          
   OTNBLIV1  2809           OTNBLIV2  2810           OTNBFAR  2811-2814    
   OTNBAGE  2815-2816       OTNBREL2  2817           OTNBFOS2  2818        
   OTNBSEX2  2819           OTNBAD2  2820            OTNBLIV9  2821        
   OTNBLIV10  2822          OTNBFAR2  2823-2824      OTNBAGE2  2825-2826   
   OTNBREL3  2827           OTNBFOS3  2828           OTNBSEX3  2829        
   OTNBAD3  2830            OTNBLIV17  2831          OTNBLIV18  2832       
   OTNBFAR3  2833           OTNBAGE3  2834-2835      OTNBREL4  2836        
   OTNBFOS4  2837           OTNBSEX4  2838           OTNBAD4  2839         
   OTNBLIV25  2840          OTNBLIV26  2841          OTNBFAR4  2842        
   OTNBAGE4  2843-2844      OTNBREL5  2845           OTNBFOS5  2846        
   OTNBSEX5  2847           OTNBAD5  2848            OTNBLIV33  2849       
   OTNBLIV34  2850          OTNBFAR5  2851           OTNBAGE5  2852-2853   
   OTNBREL6  2854           OTNBFOS6  2855           OTNBSEX6  2856        
   OTNBAD6  2857            OTNBLIV41  2858          OTNBLIV42  2859       
   OTNBFAR6  2860           OTNBAGE6  2861-2862      OTNBREL7  2863        
   OTNBFOS7  2864           OTNBSEX7  2865           OTNBAD7  2866         
   OTNBLIV49  2867          OTNBLIV50  2868          OTNBFAR7  2869        
   OTNBAGE7  2870-2871      OTNBREL8  2872           OTNBFOS8  2873        
   OTNBSEX8  2874           OTNBAD8  2875            OTNBLIV57  2876       
   OTNBLIV58  2877          OTNBFAR8  2878           OTNBAGE8  2879-2880   
   OTPREG  2881             OTPRGPRB  2882           OTPRGN  2883-2884     
   OTPRGEND  2885           OTMSN  2886              OTSTN  2887           
   OTABN  2888-2889         TOTPRG  2890-2891        OTPREGS  2892-2893    
   TOTPREGS_C  2894-2895    TOTPREGS_R  2896-2898    BIOKIDS  2899-2900    
   ADOPKIDS  2901           ANYKIDS  2902            BIOADOPT  2903        
   PREGSNOW  2904           NUMLIFE  2905-2907       BIODOB1  2908-2911    
   BIODOB2  2912-2915       BIODOB3  2916-2919       BIODOB4  2920-2923    
   BIODOB5  2924-2927       BIODOB6  2928-2931       BIODOB7  2932-2935    
   BIODOB8  2936-2939       BIODOB9  2940-2943       BIODOB10  2944-2947   
   BIOSEX1  2948            BIOSEX2  2949            BIOSEX3  2950         
   BIOSEX4  2951            BIOSEX5  2952            BIOSEX6  2953         
   BIOSEX7  2954            BIOSEX8  2955            BIOSEX9  2956         
   BIOSEX10  2957           BIOAGE1  2958-2960       BIOAGE2  2961-2963    
   BIOAGE3  2964-2966       BIOAGE4  2967-2969       BIOAGE5  2970-2972    
   BIOAGE6  2973-2975       BIOAGE7  2976-2978       BIOAGE8  2979-2981    
   BIOAGE9  2982-2983       BIOAGE10  2984-2985      BIOAGEGP1  2986-2987  
   BIOAGEGP2  2988-2989     BIOAGEGP3  2990-2991     BIOAGEGP4  2992-2993  
   BIOAGEGP5  2994-2995     BIOAGEGP6  2996-2997     BIOAGEGP7  2998       
   BIOAGEGP8  2999          BIOAGEGP9  3000          BIOAGEGP10  3001      
   BIOHH1  3002             BIOHH2  3003             BIOHH3  3004          
   BIOHH4  3005             BIOHH5  3006             BIOHH6  3007          
   BIOHH7  3008             BIOHH8  3009             BIOHH9  3010          
   BIOHH10  3011            BIOMOM1  3012            BIOMOM2  3013         
   BIOMOM3  3014            BIOMOM4  3015            BIOMOM5  3016         
   BIOMOM6  3017            BIOMOM7  3018            BIOMOM8  3019         
   BIOMOM9  3020            BIOMOM10  3021           BIOMAR1  3022         
   BIOMAR2  3023            BIOMAR3  3024            BIOMAR4  3025         
   BIOMAR5  3026            BIOMAR6  3027            BIOMAR7  3028         
   BIOMAR8  3029            BIOMAR9  3030            BIOMAR10  3031        
   BIOCOHB1  3032           BIOCOHB2  3033           BIOCOHB3  3034        
   BIOCOHB4  3035           BIOCOHB5  3036           BIOCOHB6  3037        
   BIOCOHB7  3038           BIOCOHB8  3039           BIOCOHB9  3040        
   BIOCOHB10  3041          BIOLRNPG1  3042          BIOLRNPG2  3043       
   BIOLRNPG3  3044          BIOLRNPG4  3045          BIOLRNPG5  3046       
   BIOLRNPG6  3047          BIOLRNPG7  3048          BIOLRNPG8  3049       
   BIOLRNPG9  3050          BIOLRNPG10  3051         BIOLIVNG11  3052-3053 
   BIOLIVNG12  3054         BIOLIVNG13  3055         BIOLIVNG21  3056-3057 
   BIOLIVNG22  3058         BIOLIVNG23  3059         BIOLIVNG31  3060-3061 
   BIOLIVNG32  3062         BIOLIVNG33  3063         BIOLIVNG41  3064-3065 
   BIOLIVNG42  3066         BIOLIVNG43  3067         BIOLIVNG51  3068-3069 
   BIOLIVNG52  3070         BIOLIVNG53  3071         BIOLIVNG61  3072-3073 
   BIOLIVNG62  3074         BIOLIVNG63  3075         BIOLIVNG71  3076      
   BIOLIVNG72  3077         BIOLIVNG73  3078         BIOLIVNG81  3079      
   BIOLIVNG82  3080         BIOLIVNG83  3081         BIOLIVNG91  3082      
   BIOLIVNG92  3083         BIOLIVNG93  3084         BIOLIVNG101  3085     
   BIOLIVNG102  3086        BIOLIVNG103  3087        BIOCHSIG1  3088       
   BIOCHSIG2  3089          BIOCHSIG3  3090          BIOCHSIG4  3091       
   BIOCHSIG5  3092          BIOCHSIG6  3093          BIOCHSIG7  3094       
   BIOCHSIG8  3095          BIOCHSIG9  3096          BIOCHSIG10  3097      
   BIOCHCRT1  3098          BIOCHCRT2  3099          BIOCHCRT3  3100       
   BIOCHCRT4  3101          BIOCHCRT5  3102          BIOCHCRT6  3103       
   BIOCHCRT7  3104          BIOCHCRT8  3105          BIOCHCRT9  3106       
   BIOCHCRT10  3107         BIOCHGEN1  3108          BIOCHGEN2  3109       
   BIOCHGEN3  3110          BIOCHGEN4  3111          BIOCHGEN5  3112       
   BIOCHGEN6  3113          BIOCHGEN7  3114          BIOCHGEN8  3115       
   BIOCHGEN9  3116          BIOCHGEN10  3117         BIOLVEVR1  3118       
   BIOLVEVR2  3119          BIOLVEVR3  3120          BIOLVEVR4  3121       
   BIOLVEVR5  3122          BIOLVEVR6  3123          BIOLVEVR7  3124       
   BIOLVEVR8  3125          BIOLVEVR9  3126          BIOLVEVR10  3127      
   BIOHWFAR1  3128-3132     BIOHWFAR2  3133-3137     BIOHWFAR3  3138-3142  
   BIOHWFAR4  3143-3147     BIOHWFAR5  3148-3151     BIOHWFAR6  3152-3156  
   BIOHWFAR7  3157-3159     BIOHWFAR8  3160-3161     BIOHWFAR9  3162-3164  
   BIOHWFAR10  3165-3167    BIOWANT1  3168           BIOWANT2  3169        
   BIOWANT3  3170           BIOWANT4  3171           BIOWANT5  3172        
   BIOWANT6  3173           BIOWANT7  3174           BIOWANT8  3175        
   BIOWANT9  3176           BIOWANT10  3177          BIOHSOON1  3178       
   BIOHSOON2  3179          BIOHSOON3  3180          BIOHSOON4  3181       
   BIOHSOON5  3182          BIOHSOON6  3183          BIOHSOON7  3184       
   BIOHSOON8  3185          BIOHSOON9  3186          BIOHSOON10  3187      
   BIOHOWSN1  3188-3190     BIOHOWSN2  3191-3193     BIOHOWSN3  3194-3196  
   BIOHOWSN4  3197-3199     BIOHOWSN5  3200-3202     BIOHOWSN6  3203-3205  
   BIOHOWSN7  3206-3207     BIOHOWSN8  3208-3209     BIOHOWSN9  3210       
   BIOHOWSN10  3211         BIOHPYPG1  3212-3213     BIOHPYPG2  3214-3215  
   BIOHPYPG3  3216-3217     BIOHPYPG4  3218-3219     BIOHPYPG5  3220-3221  
   BIOHPYPG6  3222-3223     BIOHPYPG7  3224-3225     BIOHPYPG8  3226-3227  
   BIOHPYPG9  3228-3229     BIOHPYPG10  3230         CRALL  3231           
   CRALLU5  3232            CRALL518  3233           CRMALU5  3234         
   CRMAL518  3235           CRFEMU5  3236            CRFEM518  3237        
   NCALL  3238              NCALLU5  3239            NCALL518  3240        
   NCMALU5  3241            NCMAL518  3242           NCFEMU5  3243         
   NCFEM518  3244           RFAGE  3245-3247         RFSEX  3248-3249      
   ROUTG04  3250            RMEAL04  3251            RERRAND04  3252       
   RPLAY04  3253            RREAD04  3254            RAFFECT04  3255       
   RPRAISE04  3256          RFEED04  3257            RBATH04  3258         
   RDIAPER04  3259          RBED04  3260             RAPPT04  3261         
   RDISC04  3262            ROUTG518  3263           RMEAL518  3264        
   RERRAND518  3265         RAFFECT518  3266         RPRAISE518  3267      
   RTAKE518  3268           RAPPT518  3269           RHELP518  3270        
   RDISC518  3271           RCLFR518  3272           RDO518  3273          
   NRFAGE  3274-3276        NRFSEX  3277-3278        NRVISIT04  3279       
   NRSATVIS04  3280-3281    NROUTG04  3282           NRMEAL04  3283        
   NRERRAND04  3284         NROVRNT04  3285          NRPLAY04  3286        
   NRREAD04  3287           NRAFFECT04  3288         NRPRAISE04  3289      
   NRFEED04  3290           NRBATH04  3291           NRDIAPER04  3292      
   NRBED04  3293            NRAPPT04  3294           NRDISC04  3295        
   NRVISIT518  3296         NRSATVIS518  3297-3298   NROUTG518  3299       
   NRMEAL518  3300          NRERRAND518  3301        NROVRNT518  3302      
   NRAFFECT518  3303        NRPRAISE518  3304        NRTAKE518  3305       
   NRAPPT518  3306          NRHELP518  3307          NRDISC518  3308       
   NRCLFR518  3309          NRDO518  3310            NRMONEY  3311         
   NREG  3312               NRAGREE  3313            NRCHSUPPYR  3314      
   COPARENT  3315           RWANT  3316              PROBWANT  3317        
   JINTEND  3318            JSUREINT  3319           JINTENDN  3320-3321   
   JEXPECTL  3322-3323      JEXPECTS  3324           JINTNEXT  3325        
   INTEND  3326             INTENDN  3327-3328       EXPECTL  3329-3330    
   EXPECTS  3331-3332       INTNEXT  3333            USUALCAR  3334        
   USLPLACE  3335-3336      USL12MOS  3337           CURRCOV  3338         
   COVERHOW01  3339-3340    COVERHOW02  3341         COVERHOW03  3342      
   COVERHOW04  3343         PARINSUR  3344           INS_EXCH  3345        
   INS_PREM  3346           COVER12  3347            NUMNOCOV  3348-3349   
   YOUGOFPC  3350           WHENGOFP  3351           YOUFPSVC1  3352       
   YOUFPSVC2  3353          YOUFPSVC3  3354          YOUFPSVC4  3355       
   YOUFPSVC5  3356          YOUFPSVC6  3357          DEAF  3358            
   BLIND  3359              DIFDECIDE  3360          DIFWALK  3361         
   DIFDRESS  3362           DIFOUT  3363             EVRCANCER  3364       
   AGECANCER  3365-3366     CANCTYPE  3367-3368      ALCORISK  3369        
   VISIT12MO1  3370         VISIT12MO2  3371         VISIT12MO3  3372      
   SVC12MO1  3373-3374      SVC12MO2  3375           SVC12MO3  3376        
   SVC12MO4  3377           SVC12MO5  3378           SVC12MO6  3379        
   SVC12MO7  3380           SVC12MO8  3381           NUMVISIT  3382-3383   
   PLACEVIS01  3384-3385    PLACEVIS02  3386-3387    PLACEVIS03  3388-3389 
   PLACEVIS04  3390-3391    PLACEVIS05  3392-3393    PLACEVIS06  3394      
   SVCPAY1  3395            SVCPAY2  3396            SVCPAY3  3397         
   SVCPAY4  3398            SVCPAY5  3399            TALKSA  3400          
   TALKEC  3401             TALKDM  3402             WHYPSTD  3403         
   BARRIER1  3404-3405      BARRIER2  3406-3407      BARRIER3  3408        
   BARRIER4  3409           BLDPRESS  3410           HIGHBP  3411          
   BPMEDS  3412             ASKSMOKE  3413           INFHELP  3414         
   INFSVCS1  3415           INFSVCS2  3416           INFSVCS3  3417        
   INFSVCS4  3418           INFSVCS5  3419           INFSVCS6  3420        
   INFTEST  3421            WHOINSEM  3422           INFHLPNW  3423        
   LASTVIS_M  3424-3425     LASTVIS_Y  3426-3429     CMINFVIS  3430-3433   
   INFRTHIS_1  3434         INFRTHIS_2  3435         DONBLOOD  3436        
   HIVTEST  3437            NOHIVTST  3438-3439      WHENHIV_M  3440-3441  
   WHENHIV_Y  3442-3445     CMHIVTST  3446-3449      HIVTSTYR  3450        
   HIVRESULT  3451          WHYNOGET  3452-3453      PLCHIV  3454-3455     
   RHHIVT1  3456            RHHIVT21  3457-3458      HIVTST  3459-3460     
   WHOSUGG  3461            TALKDOCT  3462           AIDSTALK01  3463-3464 
   AIDSTALK02  3465-3466    AIDSTALK03  3467-3468    AIDSTALK04  3469-3470 
   AIDSTALK05  3471-3472    AIDSTALK06  3473-3474    AIDSTALK07  3475-3476 
   AIDSTALK08  3477-3478    AIDSTALK09  3479-3480    AIDSTALK10  3481-3482 
   AIDSTALK11  3483-3484    SAMEADD  3485            CNTRY10  3486         
   BRNOUT  3487             YRSTRUS  3488-3491       RELRAISD  3492-3493   
   ATTND14  3494            RELCURR  3495-3496       RELTRAD  3497         
   FUNDAM1  3498            FUNDAM2  3499            FUNDAM3  3500         
   FUNDAM4  3501            RELDLIFE  3502           ATTNDNOW  3503        
   MILSVC  3504             WRK12MOS  3505-3506      FPT12MOS  3507        
   DOLASTWK1  3508          DOLASTWK2  3509          DOLASTWK3  3510       
   DOLASTWK4  3511          DOLASTWK5  3512          DOLASTWK6  3513       
   RWRKST  3514             WORKP12  3515            RPAYJOB  3516         
   RNUMJOB  3517            RFTPTX  3518             REARNTY  3519         
   SPLSTWK1  3520           SPLSTWK2  3521           SPLSTWK3  3522        
   SPLSTWK4  3523           SPLSTWK5  3524           SPWRKST  3525         
   SPPAYJOB  3526           SPNUMJOB  3527           SPFTPTX  3528         
   SPEARNTY  3529           SAMESEX  3530            CHSUPPOR  3531        
   REACTSLF  3532           CHBOTHER  3533           SEXNEEDS  3534        
   WHENSICK  3535           SHOWPAIN  3536           PMARCOHB  3537        
   COHCHANCE  3538          MARRCHANCE  3539         PMARCOH  3540         
   ACASILANG  3541          GENHEALT  3542           INCHES  3543-3544     
   RWEIGHT  3545-3547       BMI  3548-3549           DRWEIGH  3550         
   TELLWGHT  3551           WGHTSCRN  3552           ENGSPEAK  3553        
   NOBEDYR  3554            STAYREL  3555            JAILED  3556          
   JAILED2  3557            FRQJAIL  3558            FRQJAIL2  3559        
   EVSUSPEN  3560           GRADSUSP  3561-3562      SMK100  3563          
   AGESMK  3564-3565        SMOKE12  3566            SMKSTOP  3567         
   DRINK12  3568            UNIT30D  3569            DRINK30D  3570-3571   
   DRINKDAY  3572-3573      BINGE30  3574-3575       DRNKMOST  3576-3577   
   BINGE12  3578            POT12  3579              COC12  3580           
   CRACK12  3581            CRYSTMTH12  3582         INJECT12  3583        
   MADEPREG  3584           PREGTOT2  3585-3586      PREGACASI  3587-3588  
   NUMABORT  3589-3590      NUMLIVEB  3591-3592      TOLDPREG  3593        
   WHATHAPP  3594           FEMTOUCH  3595           VAGSEX  3596          
   AGEVAGR  3597-3598       CONDVAG  3599            COND1BRK  3600        
   COND1OFF  3601           WHYCONDL  3602           GETORALF  3603        
   CONDFELL  3604           GIVORALF  3605           ANYORAL  3606         
   TIMING  3607             ANALSEX  3608            CONDANAL  3609        
   OPPSEXANY  3610          OPPSEXGEN  3611          CONDSEXL  3612        
   WANTSEX1  3613           HOWOLD  3614-3615        EVRFORCD  3616        
   AGEFORC1  3617-3618      GIVNDRG2  3619           SHEBIGOL  3620        
   ENDRELA2  3621           WRDPRES2  3622           THRTPHY2  3623        
   PHYSHRT2  3624           HELDDWN2  3625           PARTSLIF_1  3626-3628 
   PARTSLFV  3629           PARTSLIF_2  3630-3632    OPPLIFENUM  3633-3635 
   PARTS12_1  3636-3638     PARTS12V  3639           PARTS12_2  3640-3642  
   OPPYEARNUM  3643-3645    NEWYEAR  3646-3648       NEWLIFE  3649-3651    
   VAGNUM12  3652-3654      ORALNUM12  3655-3657     ANALNUM12  3658-3660  
   NONMONOG  3661           NNONMONOG1  3662         NNONMONOG2  3663      
   NNONMONOG3  3664         FEMSHT12  3665           JOHNFREQ  3666        
   PROSTFRQ  3667           HIVFEM12  3668           GIVORALM  3669        
   GETORALM  3670           ORALCONDM  3671          ANALSEX2  3672        
   ANALCONDM1  3673         ANALSEX3  3674           ANALCONDM2  3675      
   MALESEX  3676            SAMESEXANY  3677         MALPRTAGE  3678       
   MALPRTHRACE  3679        EVRFORC2  3680           AGEFORC2  3681-3682   
   GIVNDRG3  3683           HEBIGOLD  3684           ENDRELA3  3685        
   WRDPRES3  3686           THRTPHY3  3687           PHYSHRT3  3688        
   HELDDWN3  3689           MALEPRTS_1  3690-3692    MALEPRTSV  3693       
   MALEPRTS_2  3694-3696    SAMLIFENUM  3697-3699    MALPRT12_1  3700-3702 
   MALPRT12V  3703          MALPRT12_2  3704-3706    SAMYEARNUM  3707-3709 
   SAMORAL12  3710-3712     RECEPANAL12  3713-3715   INSERANAL12  3716-3718
   SAMESEX1  3719-3720      MSAMEREL  3721-3722      MSMNONMON  3723-3725  
   MALSHT12  3726           JOHN2FRQ  3727           PROS2FRQ  3728        
   HIVMAL12  3729           MSMWEB12  3730           MSMSORT12  3731       
   CNDLSMAL  3732           CONDALLS  3733           MFLASTP  3734         
   WHYCOND  3735            ATTRACT  3736            ORIENT_A  3737        
   ORIENT_B  3738           CONFCONC  3739           TIMALON  3740         
   RISKCHEK1  3741          RISKCHEK2  3742          RISKCHEK3  3743       
   RISKCHEK4  3744          RECTDOUCH  3745          STDTST12  3746        
   STDSITE12  3747          STDTRT12  3748           GON  3749             
   CHLAM  3750              HERPES  3751             GENWARTS  3752        
   SYPHILIS  3753           EVRINJECT  3754          EVRSHARE  3755        
   EARNTYPE  3756           EARN  3757-3758          EARNDK1  3759         
   EARNDK2  3760            EARNDK3  3761            EARNDK4  3762         
   TOINCWMY  3763           TOTINC  3764-3765        FMINCDK1  3766        
   FMINCDK2  3767           FMINCDK3  3768           FMINCDK4  3769        
   FMINCDK5  3770           PUBASST  3771            PUBASTYP1  3772       
   FOODSTMP  3773           WIC  3774                HLPTRANS  3775        
   HLPCHLDC  3776           HLPJOB  3777             FREEFOOD  3778        
   HUNGRY  3779             MED_COST  3780           AGER  3781-3782       
   FMARITAL  3783           RMARITAL  3784           EDUCAT  3785-3786     
   HIEDUC  3787-3788        HISPANIC  3789           RACE  3790            
   HISPRACE  3791           HISPRACE2  3792          NUMKDHH  3793         
   NUMFMHH  3794            HHFAMTYP  3795           HHPARTYP  3796        
   NCHILDHH  3797           HHKIDTYP  3798           CSPBBHH  3799         
   CSPSBHH  3800            CSPOKDHH  3801           INTCTFAM  3802        
   PARAGE14  3803           EDUCMOM  3804-3805       AGEMOMB1  3806-3807   
   FMARNO  3808             AGER_I  3809             FMARITAL_I  3810      
   RMARITAL_I  3811         EDUCAT_I  3812           HIEDUC_I  3813        
   HISPANIC_I  3814         RACE_I  3815             HISPRACE_I  3816      
   HISPRACE2_I  3817        NUMKDHH_I  3818          NUMFMHH_I  3819       
   HHFAMTYP_I  3820         HHPARTYP_I  3821         NCHILDHH_I  3822      
   HHKIDTYP_I  3823         CSPBBHH_I  3824          CSPSBHH_I  3825       
   CSPOKDHH_I  3826         INTCTFAM_I  3827         PARAGE14_I  3828      
   EDUCMOM_I  3829          AGEMOMB1_I  3830         FMARNO_I  3831        
   HADSEX  3832             SEXONCE  3833            VRY1STSX  3834-3837   
   FIRSTPFLAG  3838         VRY1STAG  3839-3840      ELAPSED  3841-3843    
   SEXMAR  3844             SEXUNION  3845           FSEXRLTN  3846        
   SEX1MTHD1  3847-3848     SEX1MTHD2  3849-3850     SEX1MTHD3  3851-3852  
   SEX1MTHD4  3853-3854     LSEXDATE  3855-3858      SEX3MO  3859          
   SEX12MO  3860            LSEXRAGE  3861-3862      LSEXPRAC  3863        
   LSEXRLTN  3864           LSEXUSE1  3865-3866      LSEXUSE2  3867-3868   
   LSEXUSE3  3869-3870      LSEXUSE4  3871-3872      METH12M1  3873-3874   
   METH12M2  3875-3876      METH12M3  3877-3878      METH12M4  3879-3880   
   METH3M1  3881-3882       METH3M2  3883-3884       METH3M3  3885-3886    
   METH3M4  3887-3888       NUMP3MOS  3889           LIFPRTNR  3890-3891   
   PARTS1YR  3892-3893      PARTDUR1  3894-3896      PARTDUR2  3897-3899   
   PARTDUR3  3900-3902      COHEVER  3903            EVMARCOH  3904        
   PMARRNO  3905-3906       NONMARR  3907-3908       TIMESCOH  3909-3910   
   MARDAT01  3911-3914      MARDAT02  3915-3918      MARDAT03  3919-3922   
   MARDAT04  3923-3926      MAREND01  3927           MAREND02  3928        
   MAREND03  3929           MAREND04  3930           MARDIS01  3931-3934   
   MARDIS02  3935-3938      MARDIS03  3939-3942      MARDIS04  3943-3946   
   MAR1DISS  3947           PREMARW1  3948           COHAB1  3949-3952     
   COHSTAT  3953            COHOUT  3954             COH1DUR  3955         
   CSPBIOKD  3956           DATBABY1  3957-3960      AGEBABY1  3961-3962   
   B1PREMAR  3963           MARBABY1  3964           CEBOW  3965-3966      
   CEBOWC  3967             CEBOWP  3968             EVRNOPAT  3969        
   NONLIVEB  3970-3971      COMPREG  3972-3973       ABORTION  3974-3975   
   LOSSNUM  3976            PARENT01  3977           PARENT02  3978        
   PARENT03  3979           PARENT04  3980           PARENT05  3981        
   PARENT06  3982           PARENT07  3983           PARENT08  3984        
   PARENT09  3985           PARENT10  3986           WANTB01  3987         
   WANTB02  3988            WANTB03  3989            WANTB04  3990         
   WANTB05  3991            WANTB06  3992            WANTB07  3993         
   WANTB08  3994            WANTB09  3995            WANTB10  3996         
   HADSEX_I  3997           SEXONCE_I  3998          VRY1STSX_I  3999      
   VRY1STAG_I  4000         SEXMAR_I  4001           SEXUNION_I  4002      
   FSEXRLTN_I  4003         SEX1MTHD1_I  4004        SEX1MTHD2_I  4005     
   SEX1MTHD3_I  4006        SEX1MTHD4_I  4007        LSEXDATE_I  4008      
   SEX3MO_I  4009           SEX12MO_I  4010          LSEXRAGE_I  4011      
   LSEXRLTN_I  4012         LSEXUSE1_I  4013         LSEXUSE2_I  4014      
   LSEXUSE3_I  4015         LSEXUSE4_I  4016         METH12M1_I  4017      
   METH12M2_I  4018         METH12M3_I  4019         METH12M4_I  4020      
   METH3M1_I  4021          METH3M2_I  4022          METH3M3_I  4023       
   METH3M4_I  4024          NUMP3MOS_I  4025         LIFPRTNR_I  4026      
   PARTS1YR_I  4027         PARTDUR1_I  4028         PARTDUR2_I  4029      
   PARTDUR3_I  4030         COHEVER_I  4031          EVMARCOH_I  4032      
   PMARRNO_I  4033          NONMARR_I  4034          TIMESCOH_I  4035      
   MARDAT01_I  4036         MARDAT02_I  4037         MARDAT03_I  4038      
   MARDAT04_I  4039         MAREND01_I  4040         MAREND02_I  4041      
   MAREND03_I  4042         MAREND04_I  4043         MARDIS01_I  4044      
   MARDIS02_I  4045         MARDIS03_I  4046         MARDIS04_I  4047      
   MAR1DISS_I  4048         PREMARW1_I  4049         COHAB1_I  4050        
   COHSTAT_I  4051          COHOUT_I  4052           COH1DUR_I  4053       
   CSPBIOKD_I  4054         DATBABY1_I  4055         AGEBABY1_I  4056      
   B1PREMAR_I  4057         MARBABY1_I  4058         CEBOW_I  4059         
   CEBOWC_I  4060           CEBOWP_I  4061           EVRNOPAT_I  4062      
   NONLIVEB_I  4063         COMPREG_I  4064          ABORTION_I  4065      
   LOSSNUM_I  4066          PARENT01_I  4067         PARENT02_I  4068      
   PARENT03_I  4069         PARENT04_I  4070         PARENT05_I  4071      
   PARENT06_I  4072         PARENT07_I  4073         PARENT08_I  4074      
   PARENT09_I  4075         PARENT10_I  4076         WANTB01_I  4077       
   WANTB02_I  4078          WANTB03_I  4079          WANTB04_I  4080       
   WANTB05_I  4081          WANTB06_I  4082          WANTB07_I  4083       
   WANTB08_I  4084          WANTB09_I  4085          WANTB10_I  4086       
   DADTYPE  4087            DADTYPU5  4088           DADTYP518  4089       
   NUMCRU18  4090           NUMNCU18  4091           SUPP12MO  4092        
   DADTYPE_I  4093          DADTYPU5_I  4094         DADTYP518_I  4095     
   NUMCRU18_I  4096         NUMNCU18_I  4097         SUPP12MO_I  4098      
   INTENT  4099             ADDEXP  4100-4102        INTENT_I  4103        
   ADDEXP_I  4104           CURR_INS  4105           INFEVER  4106         
   EVHIVTST  4107           CURR_INS_I  4108         INFEVER_I  4109       
   EVHIVTST_I  4110         METRO  4111              RELIGION  4112        
   LABORFOR  4113           METRO_I  4114            RELIGION_I  4115      
   LABORFOR_I  4116         POVERTY  4117-4119       TOTINCR  4120-4121    
   PUBASSIS  4122           POVERTY_I  4123          TOTINCR_I  4124       
   PUBASSIS_I  4125         WGT2015_2017  4126-4141   SECU  4142            
   SEST  4143-4145          CMINTVW  4146-4149       CMLSTYR  4150-4153    
   CMFIVYR  4154-4157       QUARTER $ 4158-4159      PHASE $ 4160          
   INTVWYEAR $ 4161-4164    INTVLNGTH  4165-4170   ;                       


* SAS LABEL STATEMENT;

LABEL
   CASEID = "Respondent ID number"
   RSCRNINF = "Whether R was also the screener informant"
   RSCRAGE = "R's age as reported in screener"
   RSCRHISP = "R's Hispanic origin as reported in screener"
   RSCRRACE = "R's race as reported in screener"
   AGE_A = "AA-1 R's age at interview"
   AGE_R = "R's age at interview"
   AGESCRN = "R's age at screener"
   HISP = "AC-1 R is of Hispanic/Latino origin"
   HISPGRP = "AC-2 Hispanic/Latino origin -- group"
   PRIMLANG1 = "AC-6 Language spoken in home - 1st mention"
   PRIMLANG2 = "AC-6 Language spoken in home - 2nd mention"
   PRIMLANG3 = "AC-6 Language spoken in home - 3rd mention"
   ROSCNT = "Total number of HH members based on HH roster"
   MARSTAT = "AD-7b R's marital status"
   FMARSTAT = "AD-7c R's formal marital status"
   FMARIT = "Formal marital status at time of interview"
   EVRMARRY = "Whether R was ever married"
   WPLOCALE = "AD-8 Where R's current wife/partner lives"
   WOMREL = "Woman in HH is R's wife or cohabiting partner"
   GOSCHOL = "AE-1 R currently enrolled in regular school"
   VACA = "AE-2 R on vacation from school"
   HIGRADE = "AE-3 R's current grade in school or highest grade/year attended"
   COMPGRD = "AE-4 R completed current grade or highest grade/year attended"
   DIPGED = "AE-6 Which - high school diploma, GED, or both - does R have?"
   EARNHS_Y = "AE-7 Year high school diploma obtained"
   HISCHGRD = "AE-8 R not in sch, no dipl-last grade of elem/jr/hi sch attended"
   LSTGRADE = "Highest grade of elem/junior/high school R attended"
   MYSCHOL_Y = "AE-9 Year R last attended elem/jr/hi sch-R not in sch/no dipl"
   HAVEDEG = "AE-10 R has college or university degree"
   DEGREES = "AE-11 R's highest college or university degree received"
   EARNBA_Y = "AE-12 Year R received his Bachelor's degree"
   EXPSCHL = "AE-13 Expect to go back to school"
   EXPGRADE = "AE-14 Highest grade/degree expect to complete"
   WTHPARNW = "R is living with parents or parent figures - based on HH roster"
   ONOWN = "AF-0a Did R ever live away from parents/guardians before age 18"
   ONOWN18 = "Whether R lived on own before age 18"
   INTACT = "AF-1 R always lived with both biological/adoptive parents"
   PARMARR = "AF-2 R's biological parents married at R's birth"
   INTACT18 = "Whether R lived in intact family from birth to age 18"
   LVSIT14F = "AF-3 Female parent/parent-figure at age 14 - fam not intact thru 18"
   LVSIT14M = "AF-4 Male parent/parent-figure at age 14 - fam not intact thru 18"
   WOMRASDU = "AF-5 Woman who raised R during teens - fam not intact thru 18"
   MOMDEGRE = "AF-6 Highest level of education female parent (figure) completed"
   MOMWORKD = "AF-7 Female parent (figure) worked full/parttime when R was 5-15"
   MOMFSTCH = "AF-9 Age of female parent (figure) at her first birth"
   MOM18 = "AF-10 Estimated age of female parent (figure) at her first birth"
   MANRASDU = "AF-11 Man who raised R during teens - fam not intact thru 18"
   R_FOSTER = "Whether R is living/has lived with a foster parent (FC A-24a)"
   EVRFSTER = "AF-13 R ever lived in a foster home"
   MNYFSTER = "AF-14 Number of foster homes R lived in"
   DURFSTER = "AF-15 Total time spent in foster care"
   TIMESMAR = "AG-2 How many times R has been married - R ever married"
   EVCOHAB1 = "AG-3 R ever cohabited (excluding wives) - R ever married"
   NUMCOH1 = "AG-4 Number of women R has lived with-excl wives-R ever married"
   EVCOHAB2 = "AG-5 R ever cohabited - R never married, not curr cohabiting"
   NUMCOH2 = "AG-6 Number of women R has lived with-R never marr & ever cohab"
   EVRCOHAB = "Whether R ever cohabited with a woman (excluding wives)"
   NUMWIFE = "Number of wives"
   NUMCOHAB = "Number of female cohabiting partners (excluding wives)"
   EVERSEX = "BA-1 Ever had sexual intercourse with a female"
   RHADSEX = "Whether R Has Ever Had Sex (Vaginal Intercourse)"
   SXMTONCE = "BA-2 Has R had sexual intercourse more than once"
   YNOSEX = "BA-3 Main Reason Why R Has Not Had Intercourse with a Female"
   TALKPAR1 = "BA-4 Sex Ed Topics R Has Discussed with Parents -1st Mention"
   TALKPAR2 = "BA-4 Sex Ed Topics R Has Discussed with Parents -2nd Mention"
   TALKPAR3 = "BA-4 Sex Ed Topics R Has Discussed with Parents -3rd Mention"
   TALKPAR4 = "BA-4 Sex Ed Topics R Has Discussed with Parents -4th Mention"
   TALKPAR5 = "BA-4 Sex Ed Topics R Has Discussed with Parents -5th Mention"
   TALKPAR6 = "BA-4 Sex Ed Topics R Has Discussed with Parents -6th Mention"
   TALKPAR7 = "BA-4 Sex Ed Topics R Has Discussed with Parents -7th Mention"
   SEDNO = "BA-5 Formal Sex Ed Before 18: How to Say No to Sex"
   SEDNOG = "BA-6 Grade R Received Sex Ed on How to Say No to Sex"
   SEDNOSX = "BA-7 R rec'd instruction-how say no to sex before/after 1st sex?"
   SEDBC = "BA-8 Formal Sex Ed Before 18: Methods of Birth Control"
   SEDBCLC1 = "BA-8a Place where received instructions on methods of birth control-1st Mention"
   SEDBCLC2 = "BA-8a Place where received instructions on methods of birth control-2nd Mention"
   SEDBCLC3 = "BA-8a Place where received instructions on methods of birth control-3rd Mention"
   SEDBCLC4 = "BA-8a Place where received instructions on methods of birth control-4th Mention"
   SEDBCG = "BA-9 R's Grade When Received Instruction on Birth Control"
   SEDBCSX = "BA-10 R rec'd instruction-birth control before/after 1st sex?"
   SEDWHBC = "BA-11 Formal Sex Ed Before 18: Where to Get Birth Control"
   SEDWHBCG = "BA-12 R's Grade When Received Instruction on Where to Get Birth Control"
   SEDWBCSX = "BA-13 R rec'd Instruction-Where to Get Birth Control before/after 1st sex"
   SEDCOND = "BA-14 Formal Sex Ed Before 18: How to Use a Condom"
   SEDCONDG = "BA-15 R's Grade When Received Instruction on How to Use a Condom"
   SEDCONSX = "BA-16 R rec'd Instruction-How to Use a Condom before/after 1st sex"
   SEDSTD = "BA-17 Formal Sex Ed Before 18: STDs"
   SEDSTDG = "BA-18 R's Grade When Received Instruction on STDs"
   SEDSTDSX = "BA-19 R rec'd instruction on STDs before/after 1st sex?"
   SEDHIV = "BA-20 Formal Sex Ed Before 18: how prevent HIV/AIDS"
   SEDHIVG = "BA-21 R's Grade When Received Instruction on how prevent HIV/AIDS"
   SEDHIVSX = "BA-22 R's instruction on how prevent HIV/AIDS before/after 1st sex?"
   SEDABST = "BA-23 Formal Sex Ed Before 18: Waiting Until Marriage"
   SEDABLC1 = "BA-23a Place where received instruction about waiting until marriage to have sex-1st Mention"
   SEDABLC2 = "BA-23a Place where received instruction about waiting until marriage to have sex-2nd Mention"
   SEDABLC3 = "BA-23a Place where received instruction about waiting until marriage to have sex-3rd Mention"
   SEDABLC4 = "BA-23a Place where received instruction about waiting until marriage to have sex-4th Mention"
   SEDABSTG = "BA-24 R s Grade When Received Instruction on Waiting Until Marriage"
   SEDABSSX = "BA-25 R rec d instruction-Waiting Until Marriage before/after 1st sex"
   EVEROPER = "BB-1 Ever had vasectomy or other male sterilizing operation"
   TYPEOPER = "BB-2 Type of sterilizing operation R had"
   STEROPER = "BB-3 R completely sterile from sterilizing operation"
   VASEC_Y = "BB-4 Year R had vasectomy or other sterilizing operation"
   PLCSTROP = "BB-5 Place where vasectomy/other sterilizing operation was done"
   RVRSVAS = "BB-6 Ever had surgery to reverse vasectomy"
   VASREV_Y = "BB-7 Year when R had vasectomy reversed"
   RSURGSTR = "Whether R is Surgically Sterile at Interview"
   FATHPOSS = "BB-8 Whether R is physically able to father a child"
   FATHDIFF = "BB-9 Whether R would have physical difficulty fathering a child"
   RSTRSTAT = "R's Sterility Status at Time of Interview"
   LIFEPRT = "BC-6 # of female sex partners in lifetime"
   LIFEPRTS = "Number of female sex partners in lifetime"
   SXMON12 = "BC-7 Has R had sex in last 12 mos (if only 1 partner in life)"
   MON12PRT = "BC-8 # female sex partners in last 12 mos (if >1 in life)"
   MON12PRTS = "Number of female sex partners in last 12 months"
   SEXSTAT = "Summary of R's lifetime and recent sexual experience"
   P12MOCONO = "BC-8a use a condom that time"
   P12MOCON = "BC-8b How many times used condom during sex last 12 mons"
   SEXFREQ = "BC-9 # times had sex with female in last 4 weeks"
   CONFREQ = "BC-10 # times used condom when had sex with female in last 4 wks"
   P1RLTN1 = "BD-2 Ever married to last sex partner"
   P1CURRWIFE = "BD-3 Verifying if current wife = last sex partner"
   P1CURRSEP = "BD-4 Verifying if separated wife = last sex partner"
   P1RLTN2 = "BD-5 Ever cohabited with last sex partner"
   P1COHABIT = "BD-6 Verifying if current cohab partner = last sex partner"
   P1SXLAST_M = "BD-7 Month when R last sex with last sexual partner"
   P1SXLAST_Y = "BD-8 Year when R last had sex with last sexual partner"
   CMLSXP1 = "CM for date of last sex with last sexual partner"
   P2RLTN1 = "BD-10 Ever married to 2nd-to-last sex partner in past 12 mons"
   P2CURRWIFE = "BD-11 Verifying if curr wife=2nd-to-last sex partner, past 12 mos"
   P2CURRSEP = "BD-12 Verify separated wife=2nd-to-last sex partner, past 12 mos"
   P2RLTN2 = "BD-13 Ever lived with 2nd-to-last sex partner in past 12 mos"
   P2COHABIT = "BD-14 Verify curr cohab partner=2nd-to-last sex partner, < 12 mos"
   P2SXLAST_M = "BD-15 Month when R last sex with 2nd-to-last partner in last 12 mos"
   P2SXLAST_Y = "BD-16 Yr R last had sex with 2nd-to-last partner in last 12 mos"
   CMLSXP2 = "CM for date of last sex with 2nd-to-last partner in last 12 mos"
   P3RLTN1 = "BD-18 Ever married to 3rd-to-last sex partner in past 12 mons"
   P3CURRWIFE = "BD-19 Verifying if curr wife=3rd-to-last sex partner, past 12 mos"
   P3CURRSEP = "BD-20 Verify separated wife=3rd-to-last sex partner, past 12 mos"
   P3RLTN2 = "BD-21 Ever lived with 3rd-to-last sex partner in past 12 mos"
   P3COHABIT = "BD-22 Verify curr cohab partner=3rd-to-last sex partner, < 12 mos"
   P3SXLAST_M = "BD-23 Month when R last sex with 3rd-to-last partner in last 12 mos"
   P3SXLAST_Y = "BD-24 Yr R last had sex with 3rd-to-last partner in last 12 mos"
   CMLSXP3 = "CM for date of last sex with 3rd-to-last partner in last 12 mos"
   P1RELATION = "Relationship of R's most recent sex partner to R"
   P2RELATION = "Relationship of 2nd most recent sex partner in last 12 mos to R"
   P3RELATION = "Relationship of 3rd most recent sex partner in last 12 mos to R"
   FIRST = "BD-25 Verify if 1st sex partner = one of the recent sex partners"
   MARRDATE_Y = "CA-2 Year of marriage to current wife"
   HISAGEM = "CA-3 R's age when married - current wife"
   LIVTOGWF = "CA-4 Ever cohabited premaritally with current wife"
   STRTWFCP_Y = "CA-5 Year 1st began living together w/ current W/P"
   HISAGEC = "CA-6 R's age when began living together w/ current W/P"
   CMSTRTWP = "Year current union began"
   ENGATHEN = "CA-7 Engaged when began living together w/ current W/P"
   WILLMARR = "CA-8 R thinks he will marry current cohabiting partner"
   CWPDOB_Y = "CB-1 Year when R's current W/P was born"
   CWPAGE = "CB-2 Current age of R's current W/P"
   CWPRACE = "RACE-recode-like variable for current wife or cohabiting partner"
   CWPNRACE = "HISPRACE2-recode-like variable for current wife or cohabiting partner"
   CWPEDUCN = "CB-6 Highest level of education - current W/P"
   CWPBORN = "CB-7 Born outside U.S. - current W/P"
   CWPMARBF = "CB-8 Married before she married R - current W/P"
   CWPSX1WN_M = "CC-1 Month R first had sex with current W/P"
   CWPSX1WN_Y = "CC-1 Year R first had had sex with current W/P"
   CWPSX1AG = "CC-2 R's age when 1st had sex with W/P"
   CMFSXCWP = "CM of 1st sex with current wife/cohabiting partner"
   AGEFSXCWP = "R's age at 1st sex with current wife/cohabiting partner"
   CWPSX1RL = "CC-3 Relationship to current W/P at time of 1st sex"
   CWPFUSE = "CC-4 Whether R or CWP used contraception at 1st sex"
   CWPFMET01 = "CC-5 Contraceptive method R or CWP used at 1st sex-1st mention"
   CWPFMET02 = "CC-5 Contraceptive method R or CWP used at 1st sex-2nd mention"
   CWPFMET03 = "CC-5 Contraceptive method R or CWP used at 1st sex-3rd mention"
   CWPFMET04 = "CC-5 Contraceptive method R or CWP used at 1st sex-4th mention"
   CWPFMET05 = "CC-5 cont. method R or CWP used at 1st sex-5th mention"
   CWPOPSTR = "CD-1 Current W/P ever had sterilizing operation"
   CWPTYPOP1 = "CD-2 Type of sterilizing operation CWP had -1st mentioned"
   CWPTYPOP2 = "CD-2 Type of sterilizing operation CWP had -2nd mentioned"
   CWPTOTST = "CD-3 CWP completely sterile from sterilizing operation"
   CWPREVST = "CD-4 Current W/P ever had reversal of tubal sterilization"
   PSURGSTR = "Whether R's Current W/P is Surgically Sterile at Interview"
   CWPPOSS = "CD-5 Physically possible for current W/P to have a baby"
   CWPDIFF = "CD-6 Physically difficult for current W/P to have a baby"
   PSTRSTAT = "R's Current W/P's Sterility Status at Time of Interview"
   CWPLSXWN_M = "CE-2 Month of last sex with current wife/cohabiting partner (when last partner(s) not the CWP)"
   CWPLSXWN_Y = "CE-2 Year of last sex with current wife/cohabiting partner (when last partner(s) not the CWP)"
   CMLSXCWP = "CM date of last sex with current W/P"
   CWPLUSE1 = "CE-5 R use method at last sex with CWP"
   CWPLMET11 = "CE-6 Method R used at last sex with CWP-1st"
   CWPLMET12 = "CE-6 Method R used at last sex with CWP-2nd"
   CWPLMET13 = "CE-6 Method R used at last sex with CWP-3rd"
   CWPLUSE2 = "CE-7 CWP use contraceptive method at last sex w/R?"
   DKCWPLUSE = "CE-7b (DK followup) Don't recall or never knew (whether CWP used method at last sex)?"
   CWPLMET201 = "CE-8 Method CWP used at last sex with R -1st"
   CWPLMET202 = "CE-8 Method CWP used at last sex with R -2nd"
   DKCWPLMET = "CE-8b (DK followup) Don't recall or never knew (method CWP used at last sex)?"
   CWPLSXUSE = "Contraceptive use at last sex with current W/P"
   CWPRECBC = "CF-1 Last 12 mos R or CWP used any contraceptive method"
   CWPALLBC01 = "CF-2 Methods R or curr W/P used in last 12 mos-1st mentioned"
   CWPALLBC02 = "CF-2 Methods R or curr W/P used in last 12 mos-2nd mentioned"
   CWPALLBC03 = "CF-2 Methods R or curr W/P used in last 12 mos-3rd mentioned"
   CWPALLBC04 = "CF-2 Methods R or curr W/P used in last 12 mos-4th mentioned"
   CWPALLBC05 = "CF-2 Methods R or curr W/P used in last 12 mos-5th mentioned"
   CWPBCMST = "CF-3 Method R or current W/P used most often in last 12 months"
   CONDFREQ = "CF-4 Percent of time R and CWP used a condom in last 12 mos"
   CWPNOFRQ = "CF-5 Freq of method use when R & CWP had sex in last 12 mos"
   CWPBIOKD = "CG-1 R and current W/P ever have a biological child together"
   CWPNUMKD = "CG-2 # biological children R and current W/P have had together"
   PARTFATH = "Whether R had biological child with current W/P"
   CWPCHSEX = "CG-5 Sex of bio child w/current W/P - 1st child"
   CWPCHDOB_Y = "CG-6 Year of birth of bio child w/curr W/P - 1st child"
   CWPCHMAR = "CG-8 Married to curr wife at time of the birth - 1st bio child"
   CWPCHRES = "CG-9 Living with curr W/P at time of the birth - 1st bio child"
   CWPCHLRN = "CG-10 When R found out curr W/P was pregnant - 1st bio child"
   CWPCHLIV1 = "CG-11 Where 1st bio child w/CWP usually lives now -1st ment"
   CWPCHLIV2 = "CG-11 Where 1st bio child w/CWP usually lives now -2nd ment"
   CWPCHAGE = "CG-12 Current age of 1st bio child w/current W/P"
   CWPCHSIG = "CG-13a Established legal paternity by signing application for birth certificate - 1st bio child"
   CWPCHCRT = "CG-13b Established legal paternity by going to court - 1st bio child"
   CWPCHGEN = "CG-14: Established legal paternity by blood or other genetic test - 1st bio child"
   CWPCHEVR = "CG-15 Did R ever live with 1st bio child w/curr W/P"
   CWPCHFAR = "CG-16 How far away 1st bio child w/curr W/P lives now"
   CWPCHWNT = "CG-17 R wanted child before CWP got pregnant w/1st child"
   CWPCHSON = "CG-18 Pregnancy timing of 1st child w/current W/P"
   CWPSOONN = "CG-18a How much sooner pregnancy of 1st child occur w/curr W/P - Unit"
   CWPSOONMY = "CG-18b How much sooner pregnancy of 1st child occur w/curr W/P - M/Y"
   CWPCHHPY = "CG-19 R's happiness about pregnancy w/1st child w/curr W/P"
   CWPCHSEX2 = "CG-5 Sex of bio child w/current W/P - 2nd child"
   CWPCHDOB_Y2 = "CG-6 Year of birth of bio child w/curr W/P - 2nd child"
   MULTBIRT2 = "CG-7 Verifying if multiple birth - 2nd child w/curr W/P"
   CWPCHMAR2 = "CG-8 Married to curr wife at time of the birth - 2nd bio child"
   CWPCHRES2 = "CG-9 Living with curr W/P at time of the birth - 2nd bio child"
   CWPCHLRN2 = "CG-10 When R found out curr W/P was pregnant - 2nd bio child"
   CWPCHLIV10 = "CG-11 Where 2nd bio child w/CWP usually lives now -1st ment"
   CWPCHLIV11 = "CG-11 Where 2nd bio child w/CWP usually lives now -2nd ment"
   CWPCHAGE2 = "CG-12 Current age of 2nd bio child w/current W/P"
   CWPCHSIG2 = "CG-13a Established legal paternity by signing application for birth certificate - 2nd bio child"
   CWPCHCRT2 = "CG-13b Established legal paternity by going to court - 2nd bio child"
   CWPCHGEN2 = "CG-14 Established legal paternity by blood or other genetic test - 2nd bio child"
   CWPCHEVR2 = "CG-15 Did R ever live with 2nd bio child w/curr W/P"
   CWPCHFAR2 = "CG-16 How far away 2nd bio child w/curr W/P lives now"
   CWPCHWNT2 = "CG-17 R wanted child before CWP got pregnant w/2nd child"
   CWPCHSON2 = "CG-18 Pregnancy timing of 2nd child w/current W/P"
   CWPSOONN2 = "CG-18a How much sooner pregnancy of 2nd child occur w/curr W/P - Unit"
   CWPSOONMY2 = "CG-18b How much sooner pregnancy of 2nd child occur w/curr W/P - M/Y"
   CWPCHHPY2 = "CG-19 R's happiness about pregnancy w/2nd child w/curr W/P"
   CWPCHSEX3 = "CG-5 Sex of bio child w/current W/P - 3rd child"
   CWPCHDOB_Y3 = "CG-6 Year of birth of bio child w/curr W/P - 3rd child"
   MULTBIRT3 = "CG-7 Verifying if multiple birth - 3rd child w/curr W/P"
   CWPCHMAR3 = "CG-8 Married to curr wife at time of the birth - 3rd bio child"
   CWPCHRES3 = "CG-9 Living with curr W/P at time of the birth - 3rd bio child"
   CWPCHLRN3 = "CG-10 When R found out curr W/P was pregnant - 3rd bio child"
   CWPCHLIV19 = "CG-11 Where 3rd bio child w/CWP usually lives now -1st ment"
   CWPCHLIV20 = "CG-11 Where 3rd bio child w/CWP usually lives now -2nd ment"
   CWPCHAGE3 = "CG-12 Current age of 3rd bio child w/current W/P"
   CWPCHSIG3 = "CG-13a Established legal paternity by signing application for birth certificate - 3rd bio child"
   CWPCHCRT3 = "CG-13b Established legal paternity by going to court - 3rd bio child"
   CWPCHGEN3 = "CG-14 Established legal paternity by blood or other genetic test - 3rd bio child"
   CWPCHEVR3 = "CG-15 Did R ever live with 3rd bio child w/curr W/P"
   CWPCHFAR3 = "CG-16 How far away 3rd bio child w/curr W/P lives now"
   CWPCHWNT3 = "CG-17 R wanted child before CWP got pregnant w/3rd child"
   CWPCHSON3 = "CG-18 Pregnancy timing of 3rd child w/current W/P"
   CWPSOONN3 = "CG-18a How much sooner pregnancy of 3rd child occur w/curr W/P - Unit"
   CWPSOONMY3 = "CG-18b How much sooner pregnancy of 3rd child occur w/curr W/P - M/Y"
   CWPCHHPY3 = "CG-19 R's happiness about pregnancy w/3rd child w/curr W/P"
   CWPCHSEX4 = "CG-5 Sex of bio child w/current W/P - 4th child"
   CWPCHDOB_Y4 = "CG-6 Year of birth of bio child w/curr W/P - 4th child"
   MULTBIRT4 = "CG-7 Verifying if multiple birth - 4th child w/curr W/P"
   CWPCHMAR4 = "CG-8 Married to curr wife at time of the birth - 4th bio child"
   CWPCHRES4 = "CG-9 Living with curr W/P at time of the birth - 4th bio child"
   CWPCHLRN4 = "CG-10 When R found out curr W/P was pregnant - 4th bio child"
   CWPCHLIV28 = "CG-11 Where 4th bio child w/CWP usually lives now -1st ment"
   CWPCHLIV29 = "CG-11 Where 4th bio child w/CWP usually lives now -2nd ment"
   CWPCHAGE4 = "CG-12 Current age of 4th bio child w/current W/P"
   CWPCHSIG4 = "CG-13a Established legal paternity by signing application for birth certificate - 4th bio child"
   CWPCHCRT4 = "CG-13b Established legal paternity by going to court - 4th bio child"
   CWPCHGEN4 = "CG-14 Established legal paternity by blood or other genetic test - 4th bio child"
   CWPCHEVR4 = "CG-15 Did R ever live with 4th bio child w/curr W/P"
   CWPCHFAR4 = "CG-16 How far away 4th bio child w/curr W/P lives now"
   CWPCHWNT4 = "CG-17 R wanted child before CWP got pregnant w/4th child"
   CWPCHSON4 = "CG-18 Pregnancy timing of 4th child w/current W/P"
   CWPSOONN4 = "CG-18a How much sooner pregnancy of 4th child occur w/curr W/P - Unit"
   CWPSOONMY4 = "CG-18b How much sooner pregnancy of 4th child occur w/curr W/P - M/Y"
   CWPCHHPY4 = "CG-19 R's happiness about pregnancy w/4th child w/curr W/P"
   CWPCHSEX5 = "CG-5 Sex of bio child w/current W/P - 5th child"
   CWPCHDOB_Y5 = "CG-6 Year of birth of bio child w/curr W/P - 5th child"
   MULTBIRT5 = "CG-7 Verifying if multiple birth - 5th child w/curr W/P"
   CWPCHMAR5 = "CG-8 Married to curr wife at time of the birth - 5th bio child"
   CWPCHRES5 = "CG-9 Living with curr W/P at time of the birth - 5th bio child"
   CWPCHLRN5 = "CG-10 When R found out curr W/P was pregnant - 5th bio child"
   CWPCHLIV37 = "CG-11 Where 5th bio child w/CWP usually lives now -1st ment"
   CWPCHLIV38 = "CG-11 Where 5th bio child w/CWP usually lives now -2nd ment"
   CWPCHAGE5 = "CG-12 Current age of 5th bio child w/current W/P"
   CWPCHSIG5 = "CG-13a Established legal paternity by signing application for birth certificate - 5th bio child"
   CWPCHCRT5 = "CG-13b Established legal paternity by going to court - 5th bio child"
   CWPCHGEN5 = "CG-14 Established legal paternity by blood or other genetic test - 5th bio child"
   CWPCHEVR5 = "CG-15 Did R ever live with 5th bio child w/curr W/P"
   CWPCHFAR5 = "CG-16 How far away 5th bio child w/curr W/P lives now"
   CWPCHWNT5 = "CG-17 R wanted child before CWP got pregnant w/5th child"
   CWPCHSON5 = "CG-18 Pregnancy timing of 5th child w/current W/P"
   CWPSOONN5 = "CG-18a How much sooner pregnancy of 5th child occur w/curr W/P - Unit"
   CWPSOONMY5 = "CG-18b How much sooner pregnancy of 5th child occur w/curr W/P - M/Y"
   CWPCHHPY5 = "CG-19 R's happiness about pregnancy w/5th child w/curr W/P"
   CWPCHSEX6 = "CG-5 Sex of bio child w/current W/P - 6th child"
   CWPCHDOB_Y6 = "CG-6 Year of birth of bio child w/curr W/P - 6th child"
   MULTBIRT6 = "CG-7 Verifying if multiple birth - 6th child w/curr W/P"
   CWPCHMAR6 = "CG-8 Married to curr wife at time of the birth - 6th bio child"
   CWPCHRES6 = "CG-9 Living with curr W/P at time of the birth - 6th bio child"
   CWPCHLRN6 = "CG-10 When R found out curr W/P was pregnant - 6th bio child"
   CWPCHLIV46 = "CG-11 Where 6th bio child w/CWP usually lives now -1st ment"
   CWPCHLIV47 = "CG-11 Where 6th bio child w/CWP usually lives now -2nd ment"
   CWPCHAGE6 = "CG-12 Current age of 6th bio child w/current W/P"
   CWPCHSIG6 = "CG-13a Established legal paternity by signing application for birth certificate - 6th bio child"
   CWPCHCRT6 = "CG-13b Established legal paternity by going to court - 6th bio child"
   CWPCHGEN6 = "CG-14 Established legal paternity by blood or other genetic test - 6th bio child"
   CWPCHEVR6 = "CG-15 Did R ever live with 6th bio child w/curr W/P"
   CWPCHFAR6 = "CG-16 How far away 6th bio child w/curr W/P lives now"
   CWPCHWNT6 = "CG-17 R wanted child before CWP got pregnant w/6th child"
   CWPCHSON6 = "CG-18 Pregnancy timing of 6th child w/current W/P"
   CWPSOONN6 = "CG-18a How much sooner pregnancy of 6th child occur w/curr W/P - Unit"
   CWPSOONMY6 = "CG-18b How much sooner pregnancy of 6th child occur w/curr W/P - M/Y"
   CWPCHHPY6 = "CG-19 R's happiness about pregnancy w/6th child w/curr W/P"
   CWPCHSEX7 = "CG-5 Sex of bio child w/current W/P - 7th child"
   CWPCHDOB_Y7 = "CG-6 Year of birth of bio child w/curr W/P - 7th child"
   MULTBIRT7 = "CG-7 Verifying if multiple birth - 7th child w/curr W/P"
   CWPCHMAR7 = "CG-8 Married to curr wife at time of the birth - 7th bio child"
   CWPCHRES7 = "CG-9 Living with curr W/P at time of the birth - 7th bio child"
   CWPCHLRN7 = "CG-10 When R found out curr W/P was pregnant - 7th bio child"
   CWPCHLIV55 = "CG-11 Where 7th bio child w/CWP usually lives now -1st ment"
   CWPCHLIV56 = "CG-11 Where 7th bio child w/CWP usually lives now -2nd ment"
   CWPCHAGE7 = "CG-12 Current age of 7th bio child w/current W/P"
   CWPCHSIG7 = "CG-13a Established legal paternity by signing application for birth certificate - 7th bio child"
   CWPCHCRT7 = "CG-13b Established legal paternity by going to court - 7th bio child"
   CWPCHGEN7 = "CG-14 Established legal paternity by blood or other genetic test - 7th bio child"
   CWPCHEVR7 = "CG-15 Did R ever live with 7th bio child w/curr W/P"
   CWPCHFAR7 = "CG-16 How far away 7th bio child w/curr W/P lives now"
   CWPCHWNT7 = "CG-17 R wanted child before CWP got pregnant w/7th child"
   CWPCHSON7 = "CG-18 Pregnancy timing of 7th child w/current W/P"
   CWPSOONN7 = "CG-18a How much sooner pregnancy of 7th child occur w/curr W/P - Unit"
   CWPSOONMY7 = "CG-18b How much sooner pregnancy of 7th child occur w/curr W/P - M/Y"
   CWPCHHPY7 = "CG-19 R's happiness about pregnancy w/7th child w/curr W/P"
   CWPPRGNW = "CH-1 Current W/P is pregnant with R's child now"
   CWPTRYPG = "CH-2 R and CWP currently trying to get pregnant"
   CWPTRYLG = "CH-3 How long R and CWP trying to get pregnant"
   CWPCPWNT = "CH-4 Did R want a child before curr W/P became pregnant (curr preg)"
   CWPCPSON = "CH-5 Pregnancy timing - current pregnancy w/CWP"
   CWPCPSNN = "CH-5a How much sooner pregnancy occur w/CWP - Unit"
   CWPCPSNMY = "CH-5b How much sooner pregnancy occur w/CWP - Month/Year"
   CWPCPHPY = "CH-6 Happiness about current pregnancy w/CWP"
   C_OKAKIDS = "Number of CWP's other children that R adopted"
   CWPOTKID = "CI-1 CWP had any other children when R began living w/her"
   CWPOKNUM = "CI-2 # of other children CWP had when R began living w/her"
   CWPOKWTH = "CI-3 Did any of these other children live with R?"
   CWPOKWTHN = "CI-4 # of other children who lived with R"
   CWPOKSEX = "CI-6 Sex of CWP's child R lived with - 1st child"
   CWPOKAD = "CI-7 R legally adopted/became legal guardian to CWP's child - 1st child"
   CWPOKTRY = "CI-8 R trying to adopt CWP's child - 1st child"
   CWPOKTHR = "CI-9 R trying to adopt/become legal guardian of CWP's child - 1st child"
   CWPOKLIV1 = "CI-10 Where CWP's child who R lived with lives now-1st child, 1st ment"
   CWPOKLIV2 = "CI-10 Where CWP's child who R lived with lives now-1st child, 2nd ment"
   CWPOKFAR = "CI-11 How far away CWP's child R lived with lives now - 1st child"
   CWPOKAGE = "CI-12 Current age of CWP's child R lived with - 1st child"
   CWPOKSEX2 = "CI-6 Sex of CWP's child R lived with - 2nd child"
   CWPOKAD2 = "CI-7 R legally adopted/became legal guardian of CWP's child - 2nd child"
   CWPOKTRY2 = "CI-8 R trying to adopt CWP's child - 2nd child"
   CWPOKTHR2 = "CI-9 R trying to adopt/become legal guardian of CWP's child - 2nd child"
   CWPOKLIV8 = "CI-10 Where CWP's child who R lived with lives now-2nd child, 1st ment"
   CWPOKLIV9 = "CI-10 Where CWP's child who R lived with lives now-2nd child, 2nd ment"
   CWPOKFAR2 = "CI-11 How far away CWP's child R lived with lives now - 2nd child"
   CWPOKAGE2 = "CI-12 Current age of CWP's child R lived with - 2nd child"
   CWPOKSEX3 = "CI-6 Sex of CWP's child R lived with - 3rd child"
   CWPOKAD3 = "CI-7 R legally adopted/became legal guardian of CWP's child - 3rd child"
   CWPOKTRY3 = "CI-8 R trying to adopt CWP's child - 3rd child"
   CWPOKTHR3 = "CI-9 R trying to adopt/become legal guardian of CWP's child - 3rd child"
   CWPOKLIV15 = "CI-10 Where CWP's child who R lived with lives now-3rd child, 1st ment"
   CWPOKLIV16 = "CI-10 Where CWP's child who R lived with lives now-3rd child, 2nd ment"
   CWPOKFAR3 = "CI-11 How far away CWP's child R lived with lives now - 3rd child"
   CWPOKAGE3 = "CI-12 Current age of CWP's child R lived with - 3rd child"
   CWPOKSEX4 = "CI-6 Sex of CWP's child R lived with - 4th child"
   CWPOKAD4 = "CI-7 R legally adopted/became legal guardian of CWP's child - 4th child"
   CWPOKTRY4 = "C-8 R trying to adopt CWP's child - 4th child"
   CWPOKTHR4 = "C-9 R trying to adopt/become legal guardian of CWP's child - 4th child"
   CWPOKLIV22 = "CI-10 Where CWP's child who R lived with lives now-4th child, 1st ment"
   CWPOKLIV23 = "CI-10 Where CWP's child who R lived with lives now-4th child, 2nd ment"
   CWPOKFAR4 = "CI-11 How far away CWP's child R lived with lives now - 4th child"
   CWPOKAGE4 = "CI-12 Current age of CWP's child R lived with - 4th child"
   CWPOKSEX5 = "CI-6 Sex of CWP's child R lived with - 5th child"
   CWPOKAD5 = "CI-7 R legally adopted/became legal guardian of CWP's child - 5th child"
   CWPOKTRY5 = "CI-8 R trying to adopt CWP's child - 5th child"
   CWPOKTHR5 = "CI-9 R trying to adopt/become legal guardian of CWP's child - 5th child"
   CWPOKLIV29 = "CI-10 Where CWP's child who R lived with lives now-5th child, 1st ment"
   CWPOKLIV30 = "CI-10 Where CWP's child who R lived with lives now-5th child, 2nd ment"
   CWPOKFAR5 = "CI-11 How far away CWP's child R lived with lives now - 5th child"
   CWPOKAGE5 = "CI-12 Current age of CWP's child R lived with - 5th child"
   C_NBAKIDS = "Number of other nonbiological children R adopted with CWP"
   CWPNBEVR = "CJ-1 R and CWP ever had any other nonbio children under care"
   CWPNBNUM = "CJ-2 # other nonbio children under R and CWP's care"
   CWPNBREL = "CJ-4 Other nonbio child R lived with was related by blood/marriage - 1st child"
   CWPNBFOS = "CJ-5 Other nonbio child R lived with was foster child placed by soc svcs - 1st child"
   CWPNBSEX = "CJ-6 Sex of other nonbio child R lived with - 1st child"
   CWPNBAD = "CJ-7 R legally adopted/became legal guardian of this other nonbio child -1st child"
   CWPNBTRY = "CJ-8 R trying to adopt this other nonbio child - 1st child"
   CWPNBTHR = "CJ-9 R trying to adopt/become legal guardian of this other nonbio child - 1st child"
   CWPNBLIV1 = "CJ-10 Where other nonbio child R lived with lives now - 1st child, 1st ment"
   CWPNBLIV2 = "CJ-10 Where other nonbio child R lived with lives now - 1st child, 2nd ment"
   CWPNBLIV3 = "CJ-10 Where other nonbio child R lived with lives now - 1st child, 3rd ment"
   CWPNBFAR = "CJ-11 How far away other nonbio child R lived with lives now - 1st child"
   CWPNBAGE = "CJ-12 Current age of other nonbio child R lived with - 1st child"
   CWPNBREL2 = "CJ-4 Other nonbio child R lived with was related by blood/marriage - 2nd child"
   CWPNBFOS2 = "CJ-5 Other nonbio child a foster child placed by soc svcs - 2nd child"
   CWPNBSEX2 = "CJ-6 Sex of other nonbio child R lived with - 2nd child"
   CWPNBAD2 = "CJ-7 R adopted/became legal guardian of this other nonbio child - 2nd child"
   CWPNBTRY2 = "CJ-8 R trying to adopt this other nonbio child - 2nd child"
   CWPNBTHR2 = "CJ-9 R trying to adopt/become legal guardian of this other nonbio child - 2nd child"
   CWPNBLIV8 = "CJ-10 Where other nonbio child R lived with lives now - 2nd child, 1st ment"
   CWPNBLIV9 = "CJ-10 Where other nonbio child R lived with lives now - 2nd child, 2nd ment"
   CWPNBLIV10 = "CJ-10 Where other nonbio child R lived with lives now - 2nd child, 3rd ment"
   CWPNBFAR2 = "CJ-11 How far away other nonbio child R lived with lives now - 2nd child"
   CWPNBAGE2 = "CJ-12 Current age of other nonbio child R lived with - 2nd child"
   CWPNBREL3 = "CJ-4 Other nonbio child related by blood/marriage - 3rd child"
   CWPNBFOS3 = "CJ-5 Other nonbio child a foster child placed by soc svcs - 3rd child"
   CWPNBSEX3 = "CJ-6 Sex of other nonbio child R lived with - 3rd child"
   CWPNBAD3 = "CJ-7 R adopted/became legal guardian of this other nonbio child - 3rd child"
   CWPNBTRY3 = "CJ-8 R trying to adopt this other nonbio child - 3rd child"
   CWPNBTHR3 = "CJ-9 R trying to adopt/become legal guardian of this other nonbio child - 3rd child"
   CWPNBLIV15 = "CJ-10 Where other nonbio child R lived with lives now - 3rd child, 1st ment"
   CWPNBLIV16 = "CJ-10 Where other nonbio child R lived with lives now - 3rd child, 2nd ment"
   CWPNBLIV17 = "CJ-10 Where other nonbio child R lived with lives now - 3rd child, 3rd ment"
   CWPNBFAR3 = "CJ-11 How far away other nonbio child R lived with lives now - 3rd child"
   CWPNBAGE3 = "CJ-12 Current age of other nonbio child R lived with - 3rd child"
   CWPNBREL4 = "CJ-4 Other nonbio child related by blood/marriage - 4th child"
   CWPNBFOS4 = "CJ-5 Other nonbio child a foster child placed by soc svcs - 4th child"
   CWPNBSEX4 = "CJ-6 Sex of other nonbio child R lived with - 4th child"
   CWPNBAD4 = "CJ-7 R adopted/became legal guardian of this other nonbio child - 4th child"
   CWPNBTRY4 = "CJ-8 R trying to adopt this other nonbio child - 4th child"
   CWPNBTHR4 = "CJ-9 R trying to adopt/become legal guardian of this other nonbio child - 4th child"
   CWPNBLIV22 = "CJ-10 Where other nonbio child R lived with lives now - 4th child, 1st ment"
   CWPNBLIV23 = "CJ-10 Where other nonbio child R lived with lives now - 4th child, 2nd ment"
   CWPNBLIV24 = "CJ-10 Where other nonbio child R lived with lives now - 4th child, 3rd ment"
   CWPNBFAR4 = "CJ-11 How far away other nonbio child R lived with lives now - 4th child"
   CWPNBAGE4 = "CJ-12 Current age of other nonbio child R lived with - 4th child"
   CWPNBREL5 = "CJ-4 Other nonbio child related by blood/marriage - 5th child"
   CWPNBFOS5 = "CJ-5 Other nonbio child a foster child placed by soc svcs - 5th child"
   CWPNBSEX5 = "CJ-6 Sex of other nonbio child R lived with - 5th child"
   CWPNBAD5 = "CJ-7 R adopted/became legal guardian of this other nonbio child - 5th child"
   CWPNBTRY5 = "CJ-8 R trying to adopt this other nonbio child - 5th child"
   CWPNBTHR5 = "CJ-9 R trying to adopt/become legal guardian of this other nonbio child - 5th child"
   CWPNBLIV29 = "CJ-10 Where other nonbio child R lived with lives now - 5th child, 1st ment"
   CWPNBLIV30 = "CJ-10 Where other nonbio child R lived with lives now - 5th child, 2nd ment"
   CWPNBLIV31 = "CJ-10 Where other nonbio child R lived with lives now - 5th child, 3rd ment"
   CWPNBFAR5 = "CJ-11 How far away other nonbio child R lived with lives now - 5th child"
   CWPNBAGE5 = "CJ-12 Current age of other nonbio child R lived with - 5th child"
   CWPNBREL6 = "CJ-4 Other nonbio child related by blood/marriage - 6th child"
   CWPNBFOS6 = "CJ-5 Other nonbio child a foster child placed by soc svcs - 6th child"
   CWPNBSEX6 = "CJ-6 Sex of other nonbio child R lived with - 6th child"
   CWPNBAD6 = "CJ-7 R adopted/became legal guardian of this other nonbio child - 6th child"
   CWPNBTRY6 = "CJ-8 R trying to adopt this other nonbio child - 6th child"
   CWPNBTHR6 = "CJ-9 R trying to adopt/become legal guardian of this other nonbio child - 6th child"
   CWPNBLIV36 = "CJ-10 Where other nonbio child R lived with lives now - 6th child, 1st ment"
   CWPNBLIV37 = "CJ-10 Where other nonbio child R lived with lives now - 6th child, 2nd ment"
   CWPNBLIV38 = "CJ-10 Where other nonbio child R lived with lives now - 6th child, 3rd ment"
   CWPNBFAR6 = "CJ-11 How far away other nonbio child R lived with lives now - 6th child"
   CWPNBAGE6 = "CJ-12 Current age of other nonbio child R lived with - 6th child"
   CWPNBREL7 = "CJ-4 Other nonbio child related by blood/marriage - 7th child"
   CWPNBFOS7 = "CJ-5 Other nonbio child a foster child placed by soc svcs - 7th child"
   CWPNBSEX7 = "CJ-6 Sex of other nonbio child R lived with - 7th child"
   CWPNBAD7 = "CJ-7 R adopted/became legal guardian of this other nonbio child - 7th child"
   CWPNBTRY7 = "CJ-8 R trying to adopt this other nonbio child - 7th child"
   CWPNBTHR7 = "CJ-9 R trying to adopt/become legal guardian of this other nonbio child - 7th child"
   CWPNBLIV43 = "CJ-10 Where other nonbio child R lived with lives now - 7th child, 1st ment"
   CWPNBLIV44 = "CJ-10 Where other nonbio child R lived with lives now - 7th child, 2nd ment"
   CWPNBLIV45 = "CJ-10 Where other nonbio child R lived with lives now - 7th child, 3rd ment"
   CWPNBFAR7 = "CJ-11 How far away other nonbio child R lived with lives now - 7th child"
   CWPNBAGE7 = "CJ-12 Current age of other nonbio child R lived with - 7th child"
   CWPNBREL8 = "CJ-4 Other nonbio child related by blood/marriage - 8th child"
   CWPNBFOS8 = "CJ-5 Other nonbio child a foster child placed by soc svcs - 8th child"
   CWPNBSEX8 = "CJ-6 Sex of other nonbio child R lived with - 8th child"
   CWPNBAD8 = "CJ-7 R adopted/became legal guardian of this other nonbio child - 8th child"
   CWPNBTRY8 = "CJ-8 R trying to adopt this other nonbio child - 8th child"
   CWPNBTHR8 = "CJ-9 R trying to adopt/become legal guardian of this other nonbio child - 8th child"
   CWPNBLIV50 = "CJ-10 Where other nonbio child R lived with lives now - 8th child, 1st ment"
   CWPNBLIV51 = "CJ-10 Where other nonbio child R lived with lives now - 8th child, 2nd ment"
   CWPNBLIV52 = "CJ-10 Where other nonbio child R lived with lives now - 8th child, 3rd ment"
   CWPNBFAR8 = "CJ-11 How far away other nonbio child R lived with lives now - 8th child"
   CWPNBAGE8 = "CJ-12 Current age of other nonbio child R lived with - 8th child"
   CWPNBREL9 = "CJ-4 Other nonbio child related by blood/marriage - 9th child"
   CWPNBFOS9 = "CJ-5 Other nonbio child a foster child placed by soc svcs - 9th child"
   CWPNBSEX9 = "CJ-6 Sex of other nonbio child R lived with - 9th child"
   CWPNBAD9 = "CJ-7 R adopted/became legal guardian of this other nonbio child - 9th child"
   CWPNBTRY9 = "CJ-8 R trying to adopt this other nonbio child - 9th child"
   CWPNBTHR9 = "CJ-9 R trying to adopt/become legal guardian of this other nonbio child - 9th child"
   CWPNBLIV57 = "CJ-10 Where other nonbio child R lived with lives now - 9th child, 1st ment"
   CWPNBLIV58 = "CJ-10 Where other nonbio child R lived with lives now - 9th child, 2nd ment"
   CWPNBLIV59 = "CJ-10 Where other nonbio child R lived with lives now - 9th child, 3rd ment"
   CWPNBFAR9 = "CJ-11 How far away other nonbio child R lived with lives now - 9th child"
   CWPNBAGE9 = "CJ-12 Current age of other nonbio child R lived with - 9th child"
   CWPNBREL10 = "CJ-4 Other nonbio child related by blood/marriage - 10th child"
   CWPNBFOS10 = "CJ-5 Other nonbio child a foster child placed by soc svcs - 10th child"
   CWPNBSEX10 = "CJ-6 Sex of other nonbio child R lived with - 10th child"
   CWPNBAD10 = "CJ-7 R adopted/became legal guardian of this other nonbio child - 10th child"
   CWPNBTRY10 = "CJ-8 R trying to adopt this other nonbio child - 10th child"
   CWPNBTHR10 = "CJ-9 R trying to adopt/become legal guardian of this other nonbio child - 10th child"
   CWPNBLIV64 = "CJ-10 Where other nonbio child R lived with lives now - 10th child, 1st ment"
   CWPNBLIV65 = "CJ-10 Where other nonbio child R lived with lives now - 10th child, 2nd ment"
   CWPNBLIV66 = "CJ-10 Where other nonbio child R lived with lives now - 10th child, 3rd ment"
   CWPNBFAR10 = "CJ-11 How far away other nonbio child R lived with lives now - 10th child"
   CWPNBAGE10 = "CJ-12 Current age of other nonbio child R lived with - 10th child"
   MARDATEN_Y = "DB-1y Year of marriage to former wife (most recent partner)"
   AGEMARR = "DB-2 Age at marriage to former wife (most recent partner)"
   LIVTOGN = "DB-3 Cohabited premaritally w/former wife (most recent partner)"
   STRTLIVE_Y = "DB-4y Year began cohab with former W/P (most recent partner)"
   AGELIV = "DB-5 Age when began cohabiting with former W/P (most recent partner)"
   CMUNIONP = "Year of beginning of cohab or marriage (if no cohab) with former W/P (most recent partner)"
   ENGAGTHN = "DB-6 Engaged when began cohabiting w/former W/P (most recent partner)"
   MARREND = "DB-7 How did marriage end w/former wife (most recent partner)"
   WIFEDIED_Y = "DB-8y Year former wife died (most recent partner)"
   DIVORFIN_Y = "DB-9y Year divorce from former wife was finalized (most recent partner)"
   ANNULLED_Y = "DB-10y Year marriage with former wife was annulled (most recent partner)"
   STOPLIVE_Y = "DB-11y Year last lived with former W/P (most recent partner)"
   MARDATEN_Y2 = "DB-1y Year R married former wife(2nd most recent partner in last 12 mos)"
   AGEMARR2 = "DB-2 Age when R married former wife (2nd most recent partner in last 12 mos)"
   LIVTOGN2 = "DB-3 Cohabited premaritally w/former wife (2nd most recent partner in last 12 mos )"
   STRTLIVE_Y2 = "DB-4y Year began cohab w/ former W/P (2nd most recent partner in last 12 mos)"
   AGELIV2 = "DB-5 Age when began cohabiting with former W/P (2nd most recent partner in last 12 mos)"
   ENGAGTHN2 = "DB-6 Engaged when began cohabiting w/former W/P(2nd most recent partner in last 12 mos)"
   MARREND2 = "DB-7 How did marriage end w/former wife (2nd most recent partner in last 12 mos)"
   WIFEDIED_Y2 = "DB-8y Year wife died(2nd most recent partner in last 12 mos)"
   DIVORFIN_Y2 = "DB-9y Year divorce from former wife was final(2nd most recent partner)"
   ANNULLED_Y2 = "DB-10y Year of annulment from former wife (2nd most recent partner)"
   STOPLIVE_Y2 = "DB-11y Year last lived with former W/P (2nd most recent partner)"
   MARDATEN_Y3 = "DB-1y Year R married former wife(3rd most recent partner in last 12 mos)"
   AGEMARR3 = "DB-2 Age when R married former wife (3rd most recent partner)"
   LIVTOGN3 = "DB-3 Cohabited premaritally w/former wife (3rd most recent partner)"
   STRTLIVE_Y3 = "DB-4y Year began cohab w/ former W/P(3rd most recent partner in last 12 mos)"
   AGELIV3 = "DB-5 Age when began cohabiting with former W/P (3rd most recent partner)"
   ENGAGTHN3 = "DB-6 Engaged when began cohabiting w/former W/P(3rd most recent partner pst 12 m)"
   MARREND3 = "DB-7 How did marriage end w/former wife (3rd most recent partner in last 12 mos)"
   WIFEDIED_Y3 = "DB-8y Year former wife died(3rd most recent partner in last 12 mos)"
   DIVORFIN_Y3 = "DB-9y Year divorce from former wife was final (3rd most recent partner)"
   ANNULLED_Y3 = "DB-10y Year of annulment from former wife (3rd most recent partner)"
   STOPLIVE_Y3 = "DB-11y Year last lived with former W/P (3rd most recent partner)"
   CURRPRTS = "# of current nonmarital/noncohabiting partners"
   PXCURR = "DC-1 Does R consider her a current sexual partner (most recent partner)"
   PXCURRPRT = "Whether R considers her a current sexual partner (most recent partner)"
   PXMARRY = "DC-2 R thinks he will marry most recent partner"
   PXCURR2 = "DC-1 Does R consider her a current sexual partner (2nd most recent partner)"
   PXCURRPRT2 = "Whether sexual partner is current (2nd most recent partner in last 12 mos)"
   PXMARRY2 = "DC-2 R thinks he will marry 2nd most recent partner"
   PXCURR3 = "DC-1 Does R consider her a current sexual partner (3rd most recent partner)"
   PXCURRPRT3 = "Whether sexual partner is current (3rd most recent P in last 12 mos)"
   PXMARRY3 = "DC-2 R thinks he will marry 3rd most recent partner"
   PXLRUSE = "DD-5 R use method at last sex w/P (most recent partner)"
   PXLRMETH1 = "DD-6 Method R used, last sex w/P -1st (most recent partner)"
   PXLRMETH2 = "DD-6 Method R used, last sex w/P -2nd (most recent partner)"
   PXLRMETH3 = "DD-6 Method R used, last sex w/P -3rd (most recent partner)"
   PXLPUSE = "DD-7 P used a method at last sex w/R (most recent partner)"
   DKPXLPUSE = "DD-7b (DK followup) Don't recall or never knew (whether most recent P used method at last sex)?"
   PXLPMETH01 = "DD-8 Method P used, last sex w/R -1st (most recent partner)"
   PXLPMETH02 = "DD-8 Method P used, last sex w/R -2nd (most recent partner)"
   PXLPMETH03 = "DD-8 Method P used, last sex w/R -3rd (most recent partner)"
   DKPXLPMETH = "DD-8b (DK followup) Don't recall or never knew (method most recent P used at last sex)?"
   LSXUSEP = "Method used at last sex (most recent partner)"
   MTONCEP = "Whether had sex with most recent partner more than once"
   PXLSXPRB = "DD-9 Could P have used method at last sex & R not know (most recent partner)"
   PXMTONCE = "DD-10 Did R have sex more than once with most recent partner"
   PXFRLTN1 = "DD-14 Rel'p with partner at last (or only) sex (most recent partner)"
   P1YRACE1 = "RACE-recode-like variable for most recent partner"
   P1YNRACE1 = "HISPRACE2-recode-like variable for most recent partner"
   PXLRUSE2 = "DD-5 R use method at last sex with 2nd-to-last P"
   PXLRMETH5 = "DD-6 Method R used, last sex with 2nd-to-last P -1st"
   PXLRMETH6 = "DD-6 Method R used, last sex with 2nd-to-last P -2nd"
   PXLRMETH7 = "DD-6 Method R used, last sex with 2nd-to-last P -3rd"
   PXLPUSE2 = "DD-7 2nd-to-last P used a method at last sex w/R"
   DKPXLPUSE2 = "DD-7b (DK followup) Don't recall or never knew (whether 2nd- to-last P used method at last sex)?"
   PXLPMETH11 = "DD-8 Method 2nd-to-last P used, last sex with R-1st"
   PXLPMETH12 = "DD-8 Method 2nd-to-last P used, last sex with R-2nd"
   PXLPMETH13 = "DD-8 Method 2nd-to-last P used, last sex with R-3rd"
   DKPXLPMETH2 = "DD-8b (DK followup) Don't recall or never knew (method 2nd-to-last P used at last sex)?"
   LSXUSEP2 = "Method used at last sex with 2nd-to-last sexual partner"
   MTONCEP2 = "Did R have sex more than once w/2nd-to-last partner"
   PXLSXPRB2 = "DD-9 Could 2nd-to-last P have used method at last sex & R not know"
   PXMTONCE2 = "DD-10 Did R have sex more than once with 2nd-to-last partner"
   PXFRLTN3 = "DD-14 Relationship at last sex with 2nd-to-last partner"
   P1YRACE2 = "RACE-recode-like variable for 2nd-to-last partner in last 12 mos"
   P1YNRACE2 = "HISPRACE2-recode-like variable for 2nd-to-last partner in last 12 mos"
   PXLRUSE3 = "DD-5 R use method at last sex with 3rd-to-last P"
   PXLRMETH9 = "DD-6 Method R used, last sex with 3rd-to-last P -1st"
   PXLRMETH10 = "DD-6 Method R used, last sex with 3rd-to-last P -2nd"
   PXLRMETH11 = "DD-6 Method R used, last sex with 3rd-to-last P -3rd"
   PXLPUSE3 = "DD-7 3rd-to-last P used a method at last sex w/R"
   DKPXLPUSE3 = "DD-7b (DK followup) Don't recall or never knew (whether 3rd- to-last P used method at last sex)?"
   PXLPMETH21 = "DD-8 Method 3rd-to-last P used, last sex with R-1st"
   PXLPMETH22 = "DD-8 Method 3rd-to-last P used, last sex with R-2nd"
   PXLPMETH23 = "DD-8 Method 3rd-to-last P used, last sex with R-3rd"
   DKPXLPMETH3 = "DD-8b (DK followup) Don't recall or never knew (method 3rd-to-last P used at last sex)?"
   LSXUSEP3 = "Method used at last sex with 3rd-to-last sexual partner"
   MTONCEP3 = "Did R have sex more than once w/3rd-to-last partner"
   PXLSXPRB3 = "DD-9 Could 3rd-to-last P have used method at last sex & R not know"
   PXMTONCE3 = "DD-10 Did R have sex more than once with 3rd-to-last partner"
   PXFRLTN5 = "DD-14 Relationship at last sex with 3rd-to-last partner"
   P1YRACE3 = "RACE-recode-like variable for 3rd-to-last partner in last 12 mos"
   P1YNRACE3 = "HISPRACE2-recode-like variable for 3rd-to-last partner in last 12 mos"
   PXDOB_Y = "DE-1 Year former W/P was born (most recent partner)"
   PXEDUC = "DE-2 Education level of most recent partner"
   PXMARBF = "DE-3 Partner ever been married before (most recent partner)"
   PXANYCH = "DE-4 Partner had any children when rel'p began (most recent partner)"
   PXANYCHN = "DE-5 # of childrn (bio/adop/fos) partner had whn rel'p began (last p)"
   PXABLECH = "DE-6 Physically possible for partner to have baby (most recent partner)"
   PXDOB_Y2 = "DE-1 Year 2nd-to-last partner (former W/P) was born"
   PXEDUC2 = "DE-2 Education level of 2nd-to-last partner in last 12 mons"
   PXMARBF2 = "DE-3 2nd-to-last partner ever been married before"
   PXANYCH2 = "DE-4 P had childrn whn rel'p began (2nd-to-last P last 12 mos)"
   PXANYCHN2 = "DE-5 # of children (bio/adopted/foster) 2nd-to-last partner had"
   PXABLECH2 = "DE-6 Physically possible for 2nd-to-last partner to have a baby"
   PXDOB_Y3 = "DE-1 Year 3rd-to-last partner (former W/P) was born"
   PXEDUC3 = "DE-2 Education level of 3rd-to-last partner in last 12 mons"
   PXMARBF3 = "DE-3 3rd-to-last partner ever been married before"
   PXANYCH3 = "DE-4 P had childrn whn rel'p began (3rd-to-last P last 12 mos)"
   PXANYCHN3 = "DE-5 # of children (bio/adopted/foster) 3rd-to-last partner had"
   PXABLECH3 = "DE-6 Physically possible for 3rd-to-last partner to have a baby"
   PXSXFRST_M = "DF-1 Month of 1st sex with partner (most recent partner)"
   PXSXFRST_Y = "DF-1 Year of 1st sex with partner (most recent partner)"
   CMFSXP = "CM of first sex with most recent partner"
   AGEFSXP = "Age at first sex with most recent partner"
   PXAGFRST = "DF-2 R's age at 1st sex with most recent partner"
   PXFRLTN2 = "DF-3 Relationship with partner at first sex (most recent partner)"
   PXFUSE = "DF-4 Did R or partner use a method at first sex (most recent partner)"
   PXFMETH01 = "DF-5 Method R or partner used at first sex - 1st (most recent partner)"
   PXFMETH02 = "DF-5 Method R or partner used at first sex - 2nd (most recent partner)"
   PXFMETH03 = "DF-5 Method R or partner used at first sex - 3rd (most recent partner)"
   PXFMETH04 = "DF-5 Method R or partner used at first sex - 4th (most recent partner)"
   PXSXFRST_M2 = "DF-1 Month 1st sex with 2nd-to-last partner in last 12 mos"
   PXSXFRST_Y2 = "DF-1 Year 1st sex with 2nd-to-last partner in last 12 mos"
   CMFSXP2 = "CM of first sex with 2nd-to-last partner in last 12 mons"
   AGEFSXP2 = "Age of first sex with 2nd-to-last sexual partner in last 12 mos"
   PXAGFRST2 = "DF-2 R's age at 1st sex with 2nd-to-last partner in last 12 mos"
   PXFRLTN4 = "DF-3 Relationship at 1st sex w/2nd-to-last partner in last 12 mos"
   PXFUSE2 = "DF-4 Did R or 2nd-to-last partner use a method at 1st sex"
   PXFMETH14 = "DF-5 Method R or 2nd-to-last partner used at first sex -1st"
   PXFMETH15 = "DF-5 Method R or 2nd-to-last partner used at first sex -2nd"
   PXFMETH16 = "DF-5 Method R or 2nd-to-last partner used at first sex -3rd"
   PXFMETH17 = "DF-5 Method R or 2nd-to-last partner used at first sex -4th"
   PXSXFRST_M3 = "DF-1 Month 1st sex with 3rd-to-last partner in last 12 mos"
   PXSXFRST_Y3 = "DF-1 Year 1st sex with 3rd-to-last partner in last 12 mos"
   CMFSXP3 = "CM of first sex with 3rd-to-last partner in last 12 mons"
   AGEFSXP3 = "Age of first sex with 3rd-to-last sexual partner in last 12 mos"
   PXAGFRST3 = "DF-2 R's age at 1st sex with 3rd-to-last partner in last 12 mos"
   PXFRLTN6 = "DF-3 Relationship at 1st sex w/3rd-to-last partner in last 12 mos"
   PXFUSE3 = "DF-4 Did R or 3rd-to-last partner use a method at 1st sex"
   PXFMETH27 = "DF-5 Method R or 3rd-to-last partner used at first sex -1st"
   PXFMETH28 = "DF-5 Method R or 3rd-to-last partner used at first sex -2nd"
   PXFMETH29 = "DF-5 Method R or 3rd-to-last partner used at first sex -3rd"
   PXFMETH30 = "DF-5 Method R or 3rd-to-last partner used at first sex -4th"
   PXANYUSE = "DG-1 Last 12 mos: R & partner used any method (most recent partner)"
   PXMETHOD01 = "DG-2 Last 12 mos:  Methods used by R & most recent partner - 1st (most recent partner)"
   PXMETHOD02 = "DG-2 Last 12 mos:  Methods used by R & partner - 2nd (most recent partner)"
   PXMETHOD03 = "DG-2 Last 12 mos:  Methods used by R & partner - 3rd (most recent partner)"
   PXMETHOD04 = "DG-2 Last 12 mos:  Methods used by R & partner - 4th (most recent partner)"
   PXMETHOD05 = "DG-2 Last 12 mos:  Methods used by R & partner - 5th (most recent partner)"
   PXMSTUSE = "DG-3 Last 12 mos: method most often used by R & most recent partner"
   PXCONFRQ = "DG-4 Last 12 mos: percent of time R & P used condom (most recent partner)"
   PXNOFREQ = "DG-5 Last 12 mos: how often R & P used any method (most recent partner)"
   PXANYUSE2 = "DG-1 Last 12 mos: R & 2nd-to-last partner used any method"
   PXMETHOD14 = "DG-2 Last 12 mos:  Methods used by R & 2nd-to-last partner - 1st"
   PXMETHOD15 = "DG-2 Last 12 mos:  Methods used by R & 2nd-to-last partner - 2nd"
   PXMETHOD16 = "DG-2 Last 12 mos:  Methods used by R & 2nd-to-last partner - 3rd"
   PXMETHOD17 = "DG-2 Last 12 mos:  Methods used by R & 2nd-to-last partner - 4th"
   PXMETHOD18 = "DG-2 Last 12 mos:  Methods used by R & 2nd-to-last partner - 5th"
   PXMSTUSE2 = "DG-3 Last 12 mos: method most often used by R & 2nd-to-last P"
   PXCONFRQ2 = "DG-4 Last 12 mos: % of time R & 2nd-to-last P used condom"
   PXNOFREQ2 = "DG-5 Last 12 mos: how often R & 2nd-to-last P used any method"
   PXANYUSE3 = "DG-1 Last 12 mos: R & 3rd-to-last partner used any method"
   PXMETHOD27 = "DG-2 Last 12 mos:  Methods used by R & 3rd-to-last partner - 1st"
   PXMETHOD28 = "DG-2 Last 12 mos:  Methods used by R & 3rd-to-last partner - 2nd"
   PXMETHOD29 = "DG-2 Last 12 mos:  Methods used by R & 3rd-to-last partner - 3rd"
   PXMETHOD30 = "DG-2 Last 12 mos:  Methods used by R & 3rd-to-last partner - 4th"
   PXMETHOD31 = "DG-2 Last 12 mos:  Methods used by R & 3rd-to-last partner - 5th"
   PXMSTUSE3 = "DG-3 Last 12 mos: method most often used by R & 3rd-to-last P"
   PXCONFRQ3 = "DG-4 Last 12 mos: % of time R & 3rd-to-last P used condom"
   PXNOFREQ3 = "DG-5 Last 12 mos: how often R & 3rd-to-last P used any method"
   PXCHILD = "DH-1 R and partner ever have a biological child together (most recent partner)"
   PXCHILDN = "DH-2 # bio children R and partner have had together (most recent partner)"
   PXCXSEX = "DH-5 Sex of bio child with most recent partner - 1st child"
   PXCXBORN_Y = "DH-6 Year of birth of bio child with most recent partner - 1st child"
   MULTBIRT11 = "DH-7 Verifying if multiple birth - 1st child with most recent partner"
   PXCXMARB = "DH-8 Married to most recent partner at time of the birth -1st bio child"
   PXCXRES = "DH-9 Living with most recent partner at time of the birth-1st bio child"
   PXCXKNOW = "DH-10 When R found out most recent partner was pregnant -1st bio child"
   PXCXLIV01 = "DH-11 Where 1st bio child w/most recent partner usually lives now-1st mention"
   PXCXLIV02 = "DH-11 Where 1st bio child w/most recent partner usually lives now-2nd mention"
   PXCXLIV03 = "DH-11 Where 1st bio child w/most recent partner usually lives now-3rd mention"
   PXCXAGE = "DH-12 Current age of 1st bio child with most recent partner"
   PXCXSIG = "DH-13a Established legal paternity by signing application for birth certificate (1st bio child, most recent partner)"
   PXCXCRT = "DH-13b Established legal paternity by going to court (1st bio child, most recent partner)"
   PXCXGEN = "DH-14 Established legal paternity by blood or other genetic test (1st bio child, most recent partner)"
   PXCXEVER = "DH-15 Did R ever live with 1st bio child with most recent partner"
   PXCXFAR = "DH-16 How far away 1st bio child w/most recent partner lives"
   PXWANT = "DH-17 R want child at some time when most recent partner became preg w/1st child"
   PXSOON = "DH-18 Pregnancy timing of 1st child with most recent partner"
   PXSOONN = "DH-18a How much sooner 1st bio child w/most recent partner - Unit"
   PXSOONMY = "DH-18b How much sooner 1st bio child w/(most recent partner - M/Y"
   PXHPYPG = "DH-19 R's happiness about pregnancy w/1st child w/most recent partner"
   PXCXSEX2 = "DH-5 Sex of bio child with most recent partner - 2nd child"
   PXCXBORN_Y2 = "DH-6 Year of birth of bio child with most recent partner - 2nd child"
   MULTBIRT12 = "DH-7 Verifying if multiple birth - 2nd child with most recent partner"
   PXCXMARB2 = "DH-8 Married to most recent partner at time of the birth -2nd bio child"
   PXCXRES2 = "DH-9 Living with most recent partner at time of the birth-2nd bio child"
   PXCXKNOW2 = "DH-10 When R find out most recent partner was pregnant -2nd bio child"
   PXCXLIV11 = "DH-11 Where 2nd bio child w/last P usually lives now-1st mention"
   PXCXLIV12 = "DH-11 Where 2nd bio child w/last P usually lives now-2nd mention"
   PXCXLIV13 = "DH-11 Where 2nd bio child w/last P usually lives now-3rd mention"
   PXCXAGE2 = "DH-12 Current age of 2nd bio child with most recent partner"
   PXCXSIG2 = "DH-13a Established legal paternity by signing application for birth certificate (2nd bio child, most recent partner)"
   PXCXCRT2 = "DH-13b Established legal paternity by going to court (2nd bio child, most recent partner)"
   PXCXGEN2 = "DH-14 Established legal paternity by blood or other genetic test (2nd bio child, most recent partner)"
   PXCXEVER2 = "DH-15 Did R ever live with 2nd bio child with most recent partner"
   PXCXFAR2 = "DH-16 How far away 2nd bio child w/most recent partner lives"
   PXWANT2 = "DH-17 R wanted a child at some time when most recent partner became preg w/2nd child"
   PXSOON2 = "DH-18 Pregnancy timing of 2nd child with most recent partner"
   PXSOONN2 = "DH-18a How much sooner 2nd bio child w/most recent partner - Unit"
   PXSOONMY2 = "DH-18b How much sooner 2nd bio child w/most recent partner - M/Y"
   PXHPYPG2 = "DH-19 R's happiness about pregnancy w/2nd child w/most recent partner"
   PXCXSEX3 = "DH-5 Sex of bio child with most recent partner - 3rd child"
   PXCXBORN_Y3 = "DH-6 Year of birth of bio child with most recent partner - 3rd child"
   MULTBIRT13 = "DH-7 Verifying if multiple birth - 3rd child with most recent partner"
   PXCXMARB3 = "DH-8 Married to most recent partner at time of the birth -3rd bio child"
   PXCXRES3 = "DH-9 Living with most recent partner at time of the birth-3rd bio child"
   PXCXKNOW3 = "DH-10 When R find out most recent partner was pregnant -3rd bio child"
   PXCXLIV21 = "DH-11 Where 3rd bio child w/last P usually lives now-1st mention"
   PXCXLIV22 = "DH-11 Where 3rd bio child w/last P usually lives now-2nd mention"
   PXCXLIV23 = "DH-11 Where 3rd bio child w/last P usually lives now-3rd mention"
   PXCXAGE3 = "DH-12 Current age of 3rd bio child with most recent partner"
   PXCXSIG3 = "DH-13a Established legal paternity by signing application for birth certificate (3rd bio child, most recent partner)"
   PXCXCRT3 = "DH-13b Established legal paternity by going to court (3rd bio child, most recent partner)"
   PXCXGEN3 = "DH-14 Established legal paternity by blood or other genetic test (3rd bio child, most recent partner)"
   PXCXEVER3 = "DH-15 Did R ever live with 3rd bio child with most recent partner"
   PXCXFAR3 = "DH-16 How far away 3rd bio child w/most recent partner lives"
   PXWANT3 = "DH-17 R wanted a child at some time when most recent partner became preg w/3rd child"
   PXSOON3 = "DH-18 Pregnancy timing of 3rd child with most recent partner"
   PXSOONN3 = "DH-18a How much sooner 3rd bio child w/most recent partner - Unit"
   PXSOONMY3 = "DH-18b How much sooner 3rd bio child w/most recent partner - M/Y"
   PXHPYPG3 = "DH-19 R's happiness about pregnancy w/3rd child w/most recent partner"
   PXCXSEX4 = "DH-5 Sex of bio child with most recent partner - 4th child"
   PXCXBORN_Y4 = "DH-6 Year of birth of bio child with most recent partner - 4th child"
   MULTBIRT14 = "DH-7 Verifying if multiple birth - 4th child with most recent partner"
   PXCXMARB4 = "DH-8 Married to most recent partner at time of the birth -4th bio child"
   PXCXRES4 = "DH-9 Living with most recent partner at time of the birth-4th bio child"
   PXCXKNOW4 = "DH-10 When R find out most recent partner was pregnant -4th bio child"
   PXCXLIV31 = "DH-11 Where 4th bio child w/last P usually lives now-1st mention"
   PXCXLIV32 = "DH-11 Where 4th bio child w/last P usually lives now-2nd mention"
   PXCXLIV33 = "DH-11 Where 4th bio child w/last P usually lives now-3rd mention"
   PXCXAGE4 = "DH-12 Current age of 4th bio child with most recent partner"
   PXCXSIG4 = "DH-13a Established legal paternity by signing application for birth certificate (4th bio child, most recent partner)"
   PXCXCRT4 = "DH-13b Established legal paternity by going to court (4th bio child, most recent partner)"
   PXCXGEN4 = "DH-14 Established legal paternity by blood or other genetic test (4th bio child, most recent partner)"
   PXCXEVER4 = "DH-15 Did R ever live with 4th bio child with most recent partner"
   PXCXFAR4 = "DH-16 How far away 4th bio child w/most recent partner lives"
   PXWANT4 = "DH-17 R wanted a child at some time when most recent partner became preg w/4th child"
   PXSOON4 = "DH-18 Pregnancy timing of 4th child with most recent partner"
   PXSOONN4 = "DH-18a How much sooner 4th bio child w/most recent  partner - Unit"
   PXSOONMY4 = "DH-18b How much sooner 4th bio child w/most recent partner - M/Y"
   PXHPYPG4 = "DH-19 R's happiness about pregnancy w/4th child w/most recent partner"
   PXCHILD2 = "DH-1 R and 2nd-to-last partner ever have a biological child together"
   PXCHILDN2 = "DH-2 # bio children R and 2nd-to-last partner have had together"
   PXCXSEX11 = "DH-5 Sex of bio child with 2nd-to-last partner - 1st child"
   PXCXBORN_Y11 = "DH-6 Year of birth of bio child with 2nd-to-last partner - 1st child"
   MULTBIRT21 = "DH-7 Verifying if multiple birth - 1st child with 2nd-to-last partner"
   PXCXMARB11 = "DH-8 Married to 2nd-to-last partner at time of the birth -1st bio child"
   PXCXRES11 = "DH-9 Living with 2nd-to-last partner at time of the birth-1st bio child"
   PXCXKNOW11 = "DH-10 When R find out 2nd-to-last partner was pregnant -1st bio child"
   PXCXLIV101 = "DH-11 Where 1st bio child w/2nd-to-last P usually lives now-1st mention"
   PXCXLIV102 = "DH-11 Where 1st bio child w/2nd-to-last P usually lives now-2nd mention"
   PXCXAGE11 = "DH-12 Current age of 1st bio child with 2nd-to-last partner"
   PXCXSIG11 = "DH-13a Established legal paternity by signing application for birth certificate (1st bio child, 2nd-to-last partner)"
   PXCXCRT11 = "DH-13b Established legal paternity by going to court (1st bio child, 2nd-to-last partner)"
   PXCXGEN11 = "DH-14 Established legal paternity by blood or other genetic test (1st bio child, 2nd-to-last partner)"
   PXCXEVER11 = "DH-15 Did R ever live with 1st bio child with 2nd-to-last partner"
   PXCXFAR11 = "DH-16 How far away 1st bio child w/2nd-to-last partner lives"
   PXWANT11 = "DH-17 R wanted a child at some time when 2nd-to-last partner became preg w/1st child"
   PXSOON11 = "DH-18 Pregnancy timing of 1st child with 2nd-to-last partner"
   PXSOONN11 = "DH-18a How much sooner 1st bio child w/2nd-to-last partner - Unit"
   PXSOONMY11 = "DH-18b How much sooner 1st bio child w/2nd-to-last partner - M/Y"
   PXHPYPG11 = "DH-19 R's happiness about pregnancy w/1st child w/2nd-to-last partner"
   PXCXSEX12 = "DH-5 Sex of bio child with 2nd-to-last partner - 2nd child"
   PXCXBORN_Y12 = "DH-6 Year of birth of bio child with 2nd-to-last partner - 2nd child"
   MULTBIRT22 = "DH-7 Verifying if multiple birth - 2nd child with most recent partner"
   PXCXMARB12 = "DH-8 Married to 2nd-to-last partner at time of the birth -2nd bio child"
   PXCXRES12 = "DH-9 Living with 2nd-to-last partner at time of the birth-2nd bio child"
   PXCXKNOW12 = "DH-10 When R find out 2nd-to-last partner was pregnant -2nd bio child"
   PXCXLIV111 = "DH-11 Where 2nd bio child w/2nd-to-last P usually lives now-1st mention"
   PXCXLIV112 = "DH-11 Where 2nd bio child w/2nd-to-last P usually lives now-2nd mention"
   PXCXAGE12 = "DH-12 Current age of 2nd bio child with 2nd-to-last partner"
   PXCXSIG12 = "DH-13a Established legal paternity by signing application for birth certificate (2nd bio child, 2nd-to-last partner)"
   PXCXCRT12 = "DH-13b Established legal paternity by going to court (2nd bio child, 2nd-to-last partner)"
   PXCXGEN12 = "DH-14 Established legal paternity by blood or other genetic test (2nd bio child, 2nd-to-last partner)"
   PXCXEVER12 = "DH-15 Did R ever live with 2nd bio child with most recent partner"
   PXCXFAR12 = "DH-16 How far away 2nd bio child w/2nd-to-last partner lives"
   PXWANT12 = "DH-17 R wanted a child at some time when 2nd-to-last partner became preg w/2nd child"
   PXSOON12 = "DH-18 Pregnancy timing of 2nd child with 2nd-to-last partner"
   PXSOONN12 = "DH-18a How much sooner 2nd bio child w/2nd-to-last partner - Unit"
   PXSOONMY12 = "DH-18b How much sooner 2nd bio child w/2nd-to-last partner - M/Y"
   PXHPYPG12 = "DH-19 R's happiness about pregnancy w/2nd child w/2nd-to-last partner"
   PXCXSEX13 = "DH-5 Sex of bio child with 2nd-to-last partner - 3rd child"
   PXCXBORN_Y13 = "DH-6 Year of birth of bio child with 2nd-to-last partner - 3rd child"
   MULTBIRT23 = "DH-7 Verifying if multiple birth - 3rd child with most recent partner"
   PXCXMARB13 = "DH-8 Married to 2nd-to-last partner at time of the birth -3rd bio child"
   PXCXRES13 = "DH-9 Living with 2nd-to-last partner at time of the birth-3rd bio child"
   PXCXKNOW13 = "DH-10 When R found out 2nd-to-last partner was pregnant -3rd bio child"
   PXCXLIV121 = "DH-11 Where 3rd bio child w/2nd-to-last P usually lives now-1st mention"
   PXCXLIV122 = "DH-11 Where 3rd bio child w/2nd-to-last P usually lives now-2nd mention"
   PXCXAGE13 = "DH-12 Current age of 3rd bio child with 2nd-to-last partner"
   PXCXSIG13 = "DH-13a Established legal paternity by signing application for birth certificate (3rd bio child, 2nd-to-last partner)"
   PXCXCRT13 = "DH-13b Established legal paternity by going to court (3rd bio child, 2nd-to-last partner)"
   PXCXGEN13 = "DH-14 Established legal paternity by blood or other genetic test (3rd bio child, 2nd-to-last partner)"
   PXCXEVER13 = "DH-15 Did R ever live with 3rd bio child with 2nd-to-last partner"
   PXCXFAR13 = "DH-16 How far away 3rd bio child w/2nd-to-last partner lives"
   PXWANT13 = "DH-17 R wanted a child at some time when 2nd-to-last partner became preg w/3rd child"
   PXSOON13 = "DH-18 Pregnancy timing of 3rd child with 2nd-to-last partner"
   PXSOONN13 = "DH-18a How much sooner 3rd bio child w/2nd-to-last partner - Unit"
   PXSOONMY13 = "DH-18b How much sooner 3rd bio child w/2nd-to-last partner - M/Y"
   PXHPYPG13 = "DH-19 R's happiness about pregnancy w/3rd child w/2nd-to-last partner"
   PXCXSEX14 = "DH-5 Sex of bio child with 2nd-to-last partner - 4th child"
   PXCXBORN_Y14 = "DH-6 Year of birth of bio child with 2nd-to-last partner - 4th child"
   MULTBIRT24 = "DH-7 Verifying if multiple birth - 4th child with 2nd-to-last partner"
   PXCXMARB14 = "DH-8 Married to 2nd-to-last partner at time of the birth -4th bio child"
   PXCXRES14 = "DH-9 Living with 2nd-to-last partner at time of the birth-4th bio child"
   PXCXKNOW14 = "DH-10 When R found out 2nd-to-last partner was pregnant -4th bio child"
   PXCXLIV131 = "DH-11 Where 4th bio child w/2nd-to-last P usually lives now-1st mention"
   PXCXLIV132 = "DH-11 Where 4th bio child w/2nd-to-last P usually lives now-2nd mention"
   PXCXAGE14 = "DH-12 Current age of 4th bio child with 2nd-to-last partner"
   PXCXSIG14 = "DH-13a Established legal paternity by signing application for birth certificate (4th bio child, 2nd-to-last partner)"
   PXCXCRT14 = "DH-13b Established legal paternity by going to court (4th bio child, 2nd-to-last partner)"
   PXCXGEN14 = "DH-14 Established legal paternity by blood or other genetic test (4th bio child, 2nd-to-last partner)"
   PXCXEVER14 = "DH-15 Did R ever live with 4th bio child with 2nd-to-last partner"
   PXCXFAR14 = "DH-16 How far away 4th bio child w/2nd-to-last partner lives"
   PXWANT14 = "DH-17 R wanted a child at some time when 2nd-to-last partner became preg w/4th child"
   PXSOON14 = "DH-18 Pregnancy timing of 4th child with 2nd-to-last partner"
   PXSOONN14 = "DH-18a How much sooner 4th bio child w/2nd-to-last partner - Unit"
   PXSOONMY14 = "DH-18b How much sooner 4th bio child w/2nd-to-last partner - M/Y"
   PXHPYPG14 = "DH-19 R's happiness about pregnancy w/4th child w/2nd-to-last partner"
   PXCHILD3 = "DH-1 R and 3rd-to-last partner ever have a biological child together"
   PXCHILDN3 = "DH-2 # bio children R and 3rd-to-last partner have had together"
   PXCXSEX21 = "DH-5 Sex of bio child with 3rd-to-last partner - 1st child"
   PXCXBORN_Y21 = "DH-6 Year of birth of bio child with 3rd-to-last partner - 1st child"
   MULTBIRT31 = "DH-7 Verifying if multiple birth - 1st child with 3rd-to-last partner"
   PXCXMARB21 = "DH-8 Married to 3rd-to-last partner at time of the birth -1st bio child"
   PXCXRES21 = "DH-9 Living with 3rd-to-last partner at time of the birth-1st bio child"
   PXCXKNOW21 = "DH-10 When R found out 3rd-to-last partner was pregnant -1st bio child"
   PXCXLIV201 = "DH-11 Where 1st bio child w/3rd-to-last P usually lives now-1st mention"
   PXCXLIV202 = "DH-11 Where 1st bio child w/3rd-to-last P usually lives now-2nd mention"
   PXCXAGE21 = "DH-12 Current age of 1st bio child with 3rd-to-last partner"
   PXCXSIG21 = "DH-13a Established legal paternity by signing application for birth certificate (1st bio child, 3rd-to-last partner)"
   PXCXCRT21 = "DH-13b Established legal paternity by going to court (1st bio child, 3rd-to-last partner)"
   PXCXGEN21 = "DH-14 Established legal paternity by blood or other genetic test (1st bio child, 3rd-to-last partner)"
   PXCXEVER21 = "DH-15 Did R ever live with 1st bio child with 3rd-to-last partner"
   PXCXFAR21 = "DH-16 How far away 1st bio child w/3rd-to-last partner lives"
   PXWANT21 = "DH-17 R wanted a child at some time when 3rd-to-last partner became preg w/1st child"
   PXSOON21 = "DH-18 Pregnancy timing of 1st child with 3rd-to-last partner"
   PXSOONN21 = "DH-18a How much sooner 1st bio child w/3rd-to-last partner - Unit"
   PXSOONMY21 = "DH-18b How much sooner 1st bio child w/3rd-to-last partner - M/Y"
   PXHPYPG21 = "DH-19 R's happiness about pregnancy w/1st child w/3rd-to-last partner"
   PXCXSEX22 = "DH-5 Sex of bio child with 3rd-to-last partner - 2nd child"
   PXCXBORN_Y22 = "DH-6 Year of birth of bio child with 3rd-to-last partner - 2nd child"
   MULTBIRT32 = "DH-7 Verifying if multiple birth - 2nd child with 3rd-to-last partner"
   PXCXMARB22 = "DH-8 Married to 3rd-to-last partner at time of the birth -2nd bio child"
   PXCXRES22 = "DH-9 Living with 3rd-to-last partner at time of the birth-2nd bio child"
   PXCXKNOW22 = "DH-10 When R find out 3rd-to-last partner was pregnant -2nd bio child"
   PXCXLIV211 = "DH-11 Where 2nd bio child w/3rd-to-last P usually lives now-1st mention"
   PXCXLIV212 = "DH-11 Where 2nd bio child w/3rd-to-last P usually lives now-2nd mention"
   PXCXAGE22 = "DH-12 Current age of 2nd bio child with 3rd-to-last partner"
   PXCXSIG22 = "DH-13a Established legal paternity by signing application for birth certificate (2nd bio child, 3rd-to-last partner)"
   PXCXCRT22 = "DH-13b Established legal paternity by going to court (2nd bio child, 3rd-to-last partner)"
   PXCXGEN22 = "DH-14 Established legal paternity by blood or other genetic test (2nd bio child, 3rd-to-last partner)"
   PXCXEVER22 = "DH-15 Did R ever live with 2nd bio child with most recent partner"
   PXCXFAR22 = "DH-16 How far away 2nd bio child w/3rd-to-last partner lives"
   PXWANT22 = "DH-17 R wanted a child at some time when 3rd-to-last partner became preg w/2nd child"
   PXSOON22 = "DH-18 Pregnancy timing of 2nd child with 3rd-to-last partner"
   PXSOONN22 = "DH-18a How much sooner ntn bio child w/(last/2nd-to-last recent/3rd-to-last recent) partner - Unit"
   PXSOONMY22 = "DH-18b How much sooner 2nd bio child w/3rd-to-last partner - M/Y"
   PXHPYPG22 = "DH-19 R's happiness about pregnancy w/2nd child w/3rd-to-last partner"
   PXCXSEX23 = "DH-5 Sex of bio child with 3rd-to-last partner - 3rd child"
   PXCXBORN_Y23 = "DH-6 Year of birth of bio child with 3rd-to-last partner - 3rd child"
   MULTBIRT33 = "DH-7 Verifying if multiple birth - 3rd child with most recent partner"
   PXCXMARB23 = "DH-8 Married to 3rd-to-last partner at time of the birth -3rd bio child"
   PXCXRES23 = "DH-9 Living with 3rd-to-last partner at time of the birth-3rd bio child"
   PXCXKNOW23 = "DH-10 When R found out 3rd-to-last partner was pregnant -3rd bio child"
   PXCXLIV221 = "DH-11 Where 3rd bio child w/3rd-to-last P usually lives now-1st mention"
   PXCXLIV222 = "DH-11 Where 3rd bio child w/3rd-to-last P usually lives now-2nd mention"
   PXCXAGE23 = "DH-12 Current age of 3rd bio child with 3rd-to-last partner"
   PXCXSIG23 = "DH-13a Established legal paternity by signing application for birth certificate (3rd bio child, 3rd-to-last partner)"
   PXCXCRT23 = "DH-13b Established legal paternity by going to court (3rd bio child, 3rd-to-last partner)"
   PXCXGEN23 = "DH-14 Established legal paternity by blood or other genetic test (3rd bio child, 3rd-to-last partner)"
   PXCXEVER23 = "DH-15 Did R ever live with 3rd bio child with most recent partner"
   PXCXFAR23 = "DH-16 How far away 3rd bio child w/3rd-to-last partner lives"
   PXWANT23 = "DH-17 R wanted child before 3rd-to-last partner became preg w/3rd child"
   PXSOON23 = "DH-18 Pregnancy timing of 3rd child with 3rd-to-last partner"
   PXSOONN23 = "DH-18a How much sooner 3rd bio child w/3rd-to-last partner - Unit"
   PXSOONMY23 = "DH-18b How much sooner 3rd bio child w/3rd-to-last partner - M/Y"
   PXHPYPG23 = "DH-19 R's happiness about pregnancy w/3rd child w/3rd-to-last partner"
   PXCXSEX24 = "DH-5 Sex of bio child with 3rd-to-last partner - 4th child"
   PXCXBORN_Y24 = "DH-6 Year of birth of bio child with 3rd-to-last partner - 4th child"
   MULTBIRT34 = "DH-7 Verifying if multiple birth - 4th child with most recent partner"
   PXCXMARB24 = "DH-8 Married to 3rd-to-last partner at time of the birth -4th bio child"
   PXCXRES24 = "DH-9 Living with 3rd-to-last partner at time of the birth-4th bio child"
   PXCXKNOW24 = "DH-10 When R find out 3rd-to-last partner was pregnant -4th bio child"
   PXCXLIV231 = "DH-11 Where 4th bio child w/3rd-to-last P usually lives now-1st mention"
   PXCXLIV232 = "DH-11 Where 4th bio child w/3rd-to-last P usually lives now-2nd mention"
   PXCXAGE24 = "DH-12 Current age of 4th bio child with 3rd-to-last partner"
   PXCXSIG24 = "DH-13a Established legal paternity by signing application for birth certificate (4th bio child, 3rd-to-last partner)"
   PXCXCRT24 = "DH-13b Established legal paternity by going to court (4th bio child, 3rd-to-last partner)"
   PXCXGEN24 = "DH-14 Established legal paternity by blood or other genetic test (4th bio child, 3rd-to-last partner)"
   PXCXEVER24 = "DH-15 Did R ever live with 4th bio child with most recent partner"
   PXCXFAR24 = "DH-16 How far away 4th bio child w/3rd-to-last partner lives"
   PXWANT24 = "DH-17 R wanted child before 3rd-to-last partner became preg w/4th child"
   PXSOON24 = "DH-18 Pregnancy timing of 4th child with 3rd-to-last partner"
   PXSOONN24 = "DH-18a How much sooner 4th bio child w/3rd-to-last partner - Unit"
   PXSOONMY24 = "DH-18b How much sooner 4th bio child w/3rd-to-last partner - M/Y"
   PXHPYPG24 = "DH-19 R's happiness about pregnancy w/4th child w/3rd-to-last partner"
   PXCXSEX25 = "DH-5 Sex of bio child with 3rd-to-last partner - 5th child"
   PXCXBORN_Y25 = "DH-6 Year of birth of bio child with 3rd-to-last partner - 5th child"
   MULTBIRT35 = "DH-7 Verifying if multiple birth - 5th child with most recent partner"
   PXCXMARB25 = "DH-8 Married to 3rd-to-last partner at time of the birth -5th bio child"
   PXCXRES25 = "DH-9 Living with 3rd-to-last partner at time of the birth-5th bio child"
   PXCXKNOW25 = "DH-10 When R find out 3rd-to-last partner was pregnant -5th bio child"
   PXCXLIV241 = "DH-11 Where 5th bio child w/3rd-to-last P usually lives now-1st mention"
   PXCXLIV242 = "DH-11 Where 5th bio child w/3rd-to-last P usually lives now-2nd mention"
   PXCXAGE25 = "DH-12 Current age of 5th bio child with 3rd-to-last partner"
   PXCXSIG25 = "DH-13a Established legal paternity by signing application for birth certificate (5th bio child, 3rd-to-last partner)"
   PXCXCRT25 = "DH-13b Established legal paternity by going to court (5th bio child, 3rd-to-last partner)"
   PXCXGEN25 = "DH-14 Established legal paternity by blood or other genetic test (5th bio child, 3rd-to-last partner)"
   PXCXEVER25 = "DH-15 Did R ever live with 5th bio child with most recent partner"
   PXCXFAR25 = "DH-16 How far away 5th bio child w/3rd-to-last partner lives"
   PXWANT25 = "DH-17 R wanted child before 3rd-to-last partner became preg w/5th child"
   PXSOON25 = "DH-18 Pregnancy timing of 5th child with 3rd-to-last partner"
   PXSOONN25 = "DH-18a How much sooner 5th bio child w/3rd-to-last partner - Unit"
   PXSOONMY25 = "DH-18b How much sooner 5th bio child w/3rd-to-last partner - M/Y"
   PXHPYPG25 = "DH-19 R's happiness about pregnancy w/5th child w/3rd-to-last partner"
   PXCPREG = "DI-1 Is R's current partner currently pregnant (last prtnr)"
   PXTRYING = "DI-2 R & curr partner currently trying to get pregnant (last prtnr)"
   PXTRYLONG = "DI-3 How long R & curr prtnr trying to get pregnant (last prtnr)"
   PXRWANT = "DI-4 Bef curr prtnr's curr preg did R want child in future (last prtnr)"
   PXRSOON = "DI-5 Timing of current partner's current pregnancy (last prtnr)"
   PXRSOONN = "DI-5a How much sooner w/current partner - Unit"
   PXRSOONMY = "DI-5b How much sooner w/current partner - M/Y"
   PXCPFEEL = "DI-6 How happy when R found out about curr P's curr preg (last prntr)"
   PXCPREG2 = "DI-1 Is R's current partner currently pregnant (2nd-to-last prtnr)"
   PXTRYING2 = "DI-2 R & curr partner currently trying to get pregnant (2nd-to-last prtnr)"
   PXTRYLONG2 = "DI-3 How long R & curr prtnr trying to get pregnant (2nd-to-last prtnr)"
   PXRWANT2 = "DI-4 Bef curr prtnr's curr preg did R want child in future (2nd-to-last prtnr)"
   PXRSOON2 = "DI-5 Timing of current partner's current pregnancy (2nd-to-last prtnr)"
   PXRSOONN2 = "DI-5a How much sooner w/2nd-to-last partner - Unit"
   PXRSOONMY2 = "DI-5b How much sooner w/(last/2nd-to-last recent/3rd-to-last recent) partner - M/Y"
   PXCPFEEL2 = "DI-6 How happy when R found out about curr P's curr preg (2nd-to-last prntr)"
   PXCPREG3 = "DI-1 Is R's current partner currently pregnant (3rd-to-last prtnr)"
   PXTRYING3 = "DI-2 R & curr partner currently trying to get pregnant (3rd-to-last prtnr)"
   PXTRYLONG3 = "DI-3 How long R & curr prtnr trying to get pregnant (3rd-to-last prtnr)"
   PXRWANT3 = "DI-4 Bef curr prtnr's curr preg did R want child in future (3rd-to-last prtnr)"
   PXRSOON3 = "DI-5 Timing of current partner's current pregnancy (3rd-to-last prtnr)"
   PXRSOONN3 = "DI-5a How much sooner w/3rd-to-last  partner - Unit"
   PXRSOONMY3 = "DI-5b How much sooner w/3rd-to-last recent partner - M/Y"
   PXCPFEEL3 = "DI-6 How happy when R found out about curr P's curr preg (3rd-to-last prntr)"
   CURRPREG = "Whether respondent's current wife or cohabiting partner or other current partner is pregnant (Computed in FC B-11a and updated in Section C or D)"
   D_OKAKIDS = "# of most recent partner's children adopted by R"
   PXOTKID = "DJ-1 Most recent partner had any other children when started living tog"
   PXOKNUM = "DJ-2 # children most recent partner had when started living tog"
   PXOKWTH = "DJ-3 Most recent partner's child/ren lived with R"
   PXOKWTHN = "DJ-4 # of most recent partner's children who lived with R"
   PXOKSEX = "DJ-6 Sex of most recent partner's child R lived with-1st child"
   PXOKAD = "DJ-7 R legally adopted or became legal guardian-most recent partner,1st child"
   PXOKLIV1 = "DJ-8 Where most recent partner's child R lived with usually lives now-1st child,1st ment"
   PXOKFAR = "DJ-9 Distance most recent partner's child R lived with lives now-1st child"
   PXOKAGE = "DJ-10 Age of most recent partner's child R lived with-1st child"
   PXOKSEX2 = "DJ-6 Sex of most recent partner's child R lived with-2nd child"
   PXOKAD2 = "DJ-7 R legally adopted or became legal guardian-most recent partner,2nd child"
   PXOKLIV9 = "DJ-8 Where most recent partner's child R lived with usually lives now-2nd child,1st ment"
   PXOKFAR2 = "DJ-9 Distance most recent partner's child R lived with lives now-2nd child"
   PXOKAGE2 = "DJ-10 Age of most recent partner's child R lived with-2nd child"
   PXOKSEX3 = "DJ-6 Sex of most recent partner's child R lived with-3rd child"
   PXOKAD3 = "DJ-7 R legally adopted or became legal guardian-most recent partner,3rd child"
   PXOKLIV17 = "DJ-8 Where most recent partner's child R lived with usually lives now-3rd child,1st ment"
   PXOKFAR3 = "DJ-9 Distance most recent partner's child R lived with lives now-3rd child"
   PXOKAGE3 = "DJ-10 Age of most recent partner's child R lived with-3rd child"
   PXOKSEX4 = "DJ-6 Sex of most recent partner's child R lived with-4th child"
   PXOKAD4 = "DJ-7 R legally adopted or became legal guardian-most recent partner,4th child"
   PXOKLIV25 = "DJ-8 Where most recent partner's child R lived with usually lives now-4th child,1st ment"
   PXOKFAR4 = "DJ-9 Distance most recent partner's child R lived with lives now-4th child"
   PXOKAGE4 = "DJ-10 Age of most recent partner's child R lived with-4th child"
   D_OKAKIDS2 = "# of 2nd-to-last partner's children adopted by R"
   PXOTKID2 = "DJ-1 2nd-to-last partner had any other children when started liv tog"
   PXOKNUM2 = "DJ-2 # children 2nd-to-last partner had when started living tog"
   PXOKWTH2 = "DJ-3 2nd-to-last partner's child/ren lived with R"
   PXOKWTHN2 = "DJ-4 # of 2nd-to-last partner's children who lived with R"
   PXOKSEX11 = "DJ-6 Sex of 2nd-to-last partner's child R lived with-1st child"
   PXOKAD11 = "DJ-7 R legally adopted or became legal guardian-2nd-to-last partner,1st child"
   PXOKLIV81 = "DJ-8 Where 2nd-to-last partner's child R lived with usually lives now-1st child,1st ment"
   PXOKFAR11 = "DJ-9 Distance 2nd-to-last partner's child R lived with lives now-1st child"
   PXOKAGE11 = "DJ-10 Age of 2nd-to-last partner's child R lived with-1st child"
   PXOKSEX12 = "DJ-6 Sex of 2nd-to-last partner's child R lived with-2nd child"
   PXOKAD12 = "DJ-7 R legally adopted or became legal guardian-2nd-to-last partner,2nd child"
   PXOKLIV89 = "DJ-8 Where 2nd-to-last partner's child R lived with usually lives now-2nd child,1st ment"
   PXOKFAR12 = "DJ-9 Distance 2nd-to-last partner's child R lived with lives now-2nd child"
   PXOKAGE12 = "DJ-10 Age of 2nd-to-last partner's child R lived with-2nd child"
   PXOKSEX13 = "DJ-6 Sex of 2nd-to-last partner's child R lived with-3rd child"
   PXOKAD13 = "DJ-7 R legally adopted or became legal guardian-2nd-to-last partner,3rd child"
   PXOKLIV97 = "DJ-8 Where 2nd-to-last partner's child R lived with usually lives now-3rd child,1st ment"
   PXOKFAR13 = "DJ-9 Distance 2nd-to-last partner's child R lived with lives now-3rd child"
   PXOKAGE13 = "DJ-10 Age of 2nd-to-last partner's child R lived with-3rd child"
   PXOKSEX14 = "DJ-6 Sex of 2nd-to-last partner's child R lived with-4th child"
   PXOKAD14 = "DJ-7 R legally adopted or became legal guardian-2nd-to-last partner,4th child"
   PXOKLIV105 = "DJ-8 Where 2nd-to-last partner's child R lived with usually lives now-4th child,1st ment"
   PXOKFAR14 = "DJ-9 Distance 2nd-to-last partner's child R lived with lives now-4th child"
   PXOKAGE14 = "DJ-10 Age of 2nd-to-last partner's child R lived with-4th child"
   PXOKSEX15 = "DJ-6 Sex of 2nd-to-last partner's child R lived with-5th child"
   PXOKAD15 = "DJ-7 R legally adopted or became legal guardian-2nd-to-last partner,5th child"
   PXOKLIV113 = "DJ-8 Where 2nd-to-last partner's child R lived with usually lives now-5th child,1st ment"
   PXOKFAR15 = "DJ-9 Distance 2nd-to-last partner's child R lived with lives now-5th child"
   PXOKAGE15 = "DJ-10 Age of 2nd-to-last partner's child R lived with-5th child"
   D_OKAKIDS3 = "# of 3rd-to-last partner's children adopted by R"
   PXOTKID3 = "DJ-1 3rd-to-last partner had any other children when started living tog"
   PXOKNUM3 = "DJ-2 # children 3rd-to-last partner had when started living tog"
   PXOKWTH3 = "DJ-3 3rd-to-last partner's child/ren lived with R"
   PXOKWTHN3 = "DJ-4 # of 3rd-to-last partner's children who lived with R"
   PXOKSEX21 = "DJ-6 Sex of 3rd-to-last partner's child R lived with-1st child"
   PXOKAD21 = "DJ-7 R legally adopted or became legal guardian-3rd-to-last partner,1st child"
   PXOKAGE21 = "DJ-10 Age of 3rd-to-last partner's child R lived with-1st child"
   PXOKSEX22 = "DJ-6 Sex of 3rd-to-last partner's child R lived with-2nd child"
   PXOKAD22 = "DJ-7 R legally adopted or became legal guardian-3rd-to-last partner,2nd child"
   PXOKAGE22 = "DJ-10 Age of 3rd-to-last partner's child R lived with-2nd child"
   PXOKSEX23 = "DJ-6 Sex of 3rd-to-last partner's child R lived with-3rd child"
   PXOKAD23 = "DJ-7 R legally adopted or became legal guardian-3rd-to-last partner,3rd child"
   PXOKAGE23 = "DJ-10 Age of 3rd-to-last partner's child R lived with-3rd child"
   PXOKSEX24 = "DJ-6 Sex of 3rd-to-last partner's child R lived with-4th child"
   PXOKAD24 = "DJ-7 R legally adopted or became legal guardian-3rd-to-last partner,4th child"
   PXOKAGE24 = "DJ-10 Age of 3rd-to-last partner's child R lived with-4th child"
   PXOKSEX25 = "DJ-6 Sex of 3rd-to-last partner's child R lived with-5th child"
   PXOKAD25 = "DJ-7 R legally adopted or became legal guardian-3rd-to-last partner,5th child"
   PXOKAGE25 = "DJ-10 Age of 3rd-to-last partner's child R lived with-5th child"
   D_NBAKIDS = "# of other nonbio children adopted by R w/most recent partner"
   PXNBEVR = "DK-1 R & most recent partner have any other nonbio children under care"
   PXNBNUM = "DK-2 # of other nonbio children under R & most recent partner's care"
   PXNBREL = "DK-4 Other nonbio child R cared for related by blood/marr-most recent partner,1st child"
   PXNBFOS = "DK-5 Other nonbio child R cared for foster child/placed by agency-most recent partner,1st child"
   PXNBSEX = "DK-6 Sex of other nonbio child R cared for-most recent partner,1st child"
   PXNBAD = "DK-7 R adopted/became legal guardian of other nonbio child R cared for-most recent partner,1st child"
   PXNBLIV1 = "DK-8 Where oth nonbio child R cared for lives now-most recent partner,1st child,1st ment"
   PXNBLIV2 = "DK-8 Where oth nonbio child R cared for lives-most recent partner,1st child,2nd ment"
   PXNBFAR = "DK-9 How far away oth nonbio child R cared for lives now-most recent partner,1st child"
   PXNBAGE = "DK-10 Age of other nonbio child R cared for-most recent partner,1st child"
   PXNBREL2 = "DK-4 Other nonbio child R cared for related by blood/marr-most recent partner,2nd child"
   PXNBFOS2 = "DK-5 Other nonbio child R cared for foster child/placed by agency-most recent partner,2nd child"
   PXNBSEX2 = "DK-6 Sex of other nonbio child R cared for-most recent partner,2nd child"
   PXNBAD2 = "DK-7 R adopted/became legal guardian of other nonbio child R cared for-most recent partner,2nd child"
   PXNBLIV9 = "DK-8 Where oth nonbio child R cared for lives now-most recent partner,2nd child,1st ment"
   PXNBLIV10 = "DK-8 Where oth nonbio child R cared for lives-most recent partner,2nd child,2nd ment"
   PXNBFAR2 = "DK-9 How far away oth nonbio child R cared for lives now-most recent partner,2nd child"
   PXNBAGE2 = "DK-10 Age of other nonbio child R cared for-most recent partner,2nd child"
   PXNBREL3 = "DK-4 Other nonbio child R cared for related by blood/marr-most recent partner,3rd child"
   PXNBFOS3 = "DK-5 Other nonbio child R cared for foster child/placed by agency-most recent partner,3rd child"
   PXNBSEX3 = "DK-6 Sex of other nonbio child R cared for-most recent partner,3rd child"
   PXNBAD3 = "DK-7 R adopted/became legal guardian of other nonbio child R cared for-most recent partner,3rd child"
   PXNBLIV17 = "DK-8 Where oth nonbio child R cared for lives now-most recent partner,3rd child,1st ment"
   PXNBLIV18 = "DK-8 Where oth nonbio child R cared for lives-most recent partner,3rd child,2nd ment"
   PXNBFAR3 = "DK-9 How far away oth nonbio child R cared for lives now-most recent partner,3rd child"
   PXNBAGE3 = "DK-10 Age of other nonbio child R cared for-most recent partner,3rd child"
   D_NBAKIDS2 = "# of other nonbio children adopted by R w/2nd-to-last partner"
   PXNBEVR2 = "DK-1 R & 2nd-to-last partner have any other nonbio children under care"
   PXNBNUM2 = "DK-2 # of other nonbio children under R & 2nd-to-last partner's care"
   PXNBREL11 = "DK-4 Other nonbio child R cared for related by blood/marr-2nd-to-last partner,1st child"
   PXNBFOS11 = "DK-5 Other nonbio child R cared for foster child/placed by agency-2nd-to-last partner,1st child"
   PXNBSEX11 = "DK-6 Sex of other nonbio child R cared for-2nd-to-last partner,1st child"
   PXNBAD11 = "DK-7 R adopted/became legal guardian of other nonbio child R cared for-2nd-to-last partner,1st child"
   PXNBAGE11 = "DK-10 Age of other nonbio child R cared for-2nd-to-last partner,1st child"
   PXNBREL12 = "DK-4 Other nonbio child R cared for related by blood/marr-2nd-to-last partner,2nd child"
   PXNBFOS12 = "DK-5 Other nonbio child R cared for foster child/placed by agency-2nd-to-last partner,2nd child"
   PXNBSEX12 = "DK-6 Sex of other nonbio child R cared for-2nd-to-last partner,2nd child"
   PXNBAD12 = "DK-7 R adopted/became legal guardian of other nonbio child R cared for-2nd-to-last partner,2nd child"
   PXNBAGE12 = "DK-10 Age of other nonbio child R cared for-2nd-to-last partner,2nd child"
   PXNBREL13 = "DK-4 Other nonbio child R cared for related by blood/marr-2nd-to-last partner,3rd child"
   PXNBFOS13 = "DK-5 Other nonbio child R cared for foster child/placed by agency-2nd-to-last partner,3rd child"
   PXNBSEX13 = "DK-6 Sex of other nonbio child R cared for-2nd-to-last partner,3rd child"
   PXNBAD13 = "DK-7 R adopted/became legal guardian of other nonbio child R cared for-2nd-to-last partner,3rd child"
   PXNBAGE13 = "DK-10 Age of other nonbio child R cared for-2nd-to-last partner,3rd child"
   PXNBREL14 = "DK-4 Other nonbio child R cared for related by blood/marr-2nd-to-last partner,4th child"
   PXNBFOS14 = "DK-5 Other nonbio child R cared for foster child/placed by agency-2nd-to-last partner,4th child"
   PXNBSEX14 = "DK-6 Sex of other nonbio child R cared for-2nd-to-last partner,4th child"
   PXNBAD14 = "DK-7 R adopted/became legal guardian of other nonbio child R cared for-2nd-to-last partner,4th child"
   PXNBAGE14 = "DK-10 Age of other nonbio child R cared for-2nd-to-last partner,4th child"
   PXNBREL15 = "DK-4 Other nonbio child R cared for related by blood/marr-2nd-to-last partner,5th child"
   PXNBFOS15 = "DK-5 Other nonbio child R cared for foster child/placed by agency-2nd-to-last partner,5th child"
   PXNBSEX15 = "DK-6 Sex of other nonbio child R cared for-2nd-to-last partner,5th child"
   PXNBAD15 = "DK-7 R adopted/became legal guardian of other nonbio child R cared for-2nd-to-last partner,5th child"
   PXNBAGE15 = "DK-10 Age of other nonbio child R cared for-2nd-to-last partner,5th child"
   PXNBREL16 = "DK-4 Other nonbio child R cared for related by blood/marr-2nd-to-last partner,6th child"
   PXNBFOS16 = "DK-5 Other nonbio child R cared for foster child/placed by agency-2nd-to-last partner,6th child"
   PXNBSEX16 = "DK-6 Sex of other nonbio child R cared for-2nd-to-last partner,6th child"
   PXNBAD16 = "DK-7 R adopted/became legal guardian of other nonbio child R cared for-2nd-to-last partner,6th child"
   PXNBAGE16 = "DK-10 Age of other nonbio child R cared for-2nd-to-last partner,6th child"
   PXNBREL17 = "DK-4 Other nonbio child R cared for related by blood/marr-2nd-to-last partner,7th child"
   PXNBFOS17 = "DK-5 Other nonbio child R cared for foster child/placed by agency-2nd-to-last partner,7th child"
   PXNBSEX17 = "DK-6 Sex of other nonbio child R cared for-2nd-to-last partner,7th child"
   PXNBAD17 = "DK-7 R adopted/became legal guardian of other nonbio child R cared for-2nd-to-last partner,7th child"
   PXNBAGE17 = "DK-10 Age of other nonbio child R cared for-2nd-to-last partner,7th child"
   PXNBREL18 = "DK-4 Other nonbio child R cared for related by blood/marr-2nd-to-last partner,8th child"
   PXNBFOS18 = "DK-5 Other nonbio child R cared for foster child/placed by agency-2nd-to-last partner,8th child"
   PXNBSEX18 = "DK-6 Sex of other nonbio child R cared for-2nd-to-last partner,8th child"
   PXNBAD18 = "DK-7 R adopted/became legal guardian of other nonbio child R cared for-2nd-to-last partner,8th child"
   PXNBAGE18 = "DK-10 Age of other nonbio child R cared for-2nd-to-last partner,8th child"
   D_NBAKIDS3 = "# of other nonbio children adopted by R w/3rd-to-last partner"
   PXNBEVR3 = "DK-1 R & 3rd-to-last partner have any other nonbio children under care"
   FPFIRST_M = "DL-1 Month of first sexual intercourse"
   FPFIRST_Y = "DL-1 Year of first sexual intercourse"
   CMFSTSEX = "CM for date of first sexual intercourse"
   FSTSEXAGE = "Age at first sexual intercourse (based on CMFSTSEX)"
   FPAGE = "DL-2 R's age at first sexual intercourse"
   FPAGE18 = "DL-3 Was R less than 18 years old at 1st sex (specif.age DK)"
   FPAGE15 = "DL-4 Was R less than 15 years old at 1st sex (specif.age DK)"
   FPAGE20 = "DL-5 Was R less than 20 years old at 1st sex (specif.age DK)"
   RFSXAGEGP = "Respondent's estimated age at first sexual intercourse (if age unknown, otherwise this is 0) (computed in Flow Check D-61)"
   FPRLTN = "DL-10 Relationship with 1st partner at 1st sex"
   FPUSE = "DL-11 R & 1st partner use a method at 1st sex"
   FPMETH01 = "DL-12 Method(s) R & 1st partner used at 1st sex-1st mention"
   FPMETH02 = "DL-12 Method(s) R & 1st partner used at 1st sex-2nd mention"
   FPMETH03 = "DL-12 Method(s) R & 1st partner used at 1st sex-3rd mention"
   FPMETH04 = "DL-12 Method(s) R & 1st partner used at 1st sex-4th mention"
   FPPROBE = "DL-13 1st ptnr, 1st sex: could P have used method & R not know"
   NFORMWIFE = "Number of Former Wives"
   NFORMCOHAB = "Number of Former Cohabiting Partners"
   FWVERIFY = "EA-2 Already Talked About 1st Former Wife as Last/Recent Partner?"
   FWVER = "1st Former Wife Not Previously Discussed in Interview"
   FWVERIFY2 = "EA-2 Already Talked About 2nd Former Wife as Last/Recent Partner?"
   FWVER2 = "2nd Former Wife Not Previously Discussed in Interview"
   FWVERIFY3 = "EA-2 Already Talked About 3rd Former Wife as Last/Recent Partner?"
   FWVER3 = "3rd Former Wife Not Previously Discussed in Interview"
   FWVERIFY4 = "EA-2 Already Talked About 4th Former Wife as Last/Recent Partner?"
   FWVER4 = "4th Former Wife Not Previously Discussed in Interview"
   FCVER = "1st Former Cohab Partner Not Previously Discussed in Interview"
   FCVERIFY = "EA-4 Already Talked About 1st Former Cohab as Recent Partner?"
   EXRELATION = "Relationship with Each Woman in E - Former Cohab or Wife-1st"
   FWMAREND_Y = "EB-2 Year of Marriage - 1st Former Wife"
   AGEMARRN = "EB-3 R's Age at Marriage - 1st Former Wife"
   LIVTOGN4 = "EB-4 Ever Cohabited Premaritally -1st Former Wife"
   STRTLIVE_Y4 = "EB-5 Year Began Cohabiting (Premar) - 1st Former Wife"
   AGELIV4 = "EB-6 R's Age When Began Cohabiting (Premar) - 1st Former Wife"
   CMUNIONW = "Year When Coresidence (Union) Began - 1st Former Wife"
   ENGAGTHN4 = "EB-7 Engaged When Began Living Together - 1st former wife"
   MARREND4 = "EB-8 How Marriage Ended-1st Former Wife"
   WIFEDIED_Y4 = "EB-9 Year Wife Died-1st Former Wife"
   DIVORFIN_Y4 = "EB-10 Year Divorce Final-1st Former Wife"
   ANNULLED_Y4 = "EB-11 Year Marriage Annulled - 1st Former Wife"
   STOPLIVE_Y4 = "EB-12 Year Last Lived Together - 1st former wife"
   EXRELATION2 = "Relationship with Each Woman in E - Former Cohab or Wife-2nd"
   FWMAREND_Y2 = "EB-2 Year of Marriage - 2nd Former Wife"
   AGEMARRN2 = "EB-3 R's Age at Marriage - 2nd Former Wife"
   LIVTOGN5 = "EB-4 Ever Cohabited Premaritally -2nd Former Wife"
   STRTLIVE_Y5 = "EB-5 Year Began Cohabiting (Premar) - 2nd Former Wife"
   AGELIV5 = "EB-6 R's Age When Began Cohabiting (Premar) - 2nd former wife"
   ENGAGTHN5 = "EB-7 Engaged When Began Living Together - 2nd former wife"
   MARREND5 = "EB-8 How Marriage Ended-2nd Former Wife"
   WIFEDIED_Y5 = "EB-9 Year Wife Died-2nd Former Wife"
   DIVORFIN_Y5 = "EB-10 Year Divorce Final-2nd Former Wife"
   ANNULLED_Y5 = "EB-11 Year Marriage Annulled - 2nd Former Wife"
   STOPLIVE_Y5 = "EB-12 Year Last Lived Together - 2nd former wife"
   EXRELATION3 = "Relationship with Each Woman in E - Former Cohab or Wife-3rd"
   FWMAREND_Y3 = "EB-2 Year of Marriage - 3rd former Wife"
   AGEMARRN3 = "EB-3 R's Age at Marriage - 3rd former Wife"
   LIVTOGN6 = "EB-4 Ever Cohabited Premaritally -3rd former Wife"
   STRTLIVE_Y6 = "EB-5 Year Began Cohabiting (Premar) - 3rd Former Wife"
   AGELIV6 = "EB-6 R's Age When Began Cohabiting (Premar) - 3rd former wife"
   ENGAGTHN6 = "EB-7 Engaged When Began Living Together - 3rd former wife"
   MARREND6 = "EB-8 How Marriage Ended-3rd former Wife"
   WIFEDIED_Y6 = "EB-9 Year Wife Died-3rd former Wife"
   DIVORFIN_Y6 = "EB-10 Year Divorce Final-3rd former Wife"
   ANNULLED_Y6 = "EB-11 Year Marriage Annulled - 3rd former Wife"
   STOPLIVE_Y6 = "EB-12 Year Last Lived Together - 3rd former wife"
   EXRELATION4 = "Relationship with Each Woman in E - Former Cohab or Wife-4th"
   FWMAREND_Y4 = "EB-2 Year of Marriage - 4th former Wife"
   AGEMARRN4 = "EB-3 R's Age at Marriage - 4th former Wife"
   LIVTOGN7 = "EB-4 Ever Cohabited Premaritally -4th former Wife"
   STRTLIVE_Y7 = "EB-5 Year Began Cohabiting (Premar) - 4th Former Wife"
   AGELIV7 = "EB-6 R's Age When Began Cohabiting (Premar) - 4th former wife"
   ENGAGTHN7 = "EB-7 Engaged When Began Living Together - 4th former wife"
   MARREND7 = "EB-8 How Marriage Ended-4th former Wife"
   WIFEDIED_Y7 = "EB-9 Year Wife Died-4th former Wife"
   DIVORFIN_Y7 = "EB-10 Year Divorce Final-4th former Wife"
   ANNULLED_Y7 = "EB-11 Year Marriage Annulled - 4th former Wife"
   STOPLIVE_Y7 = "EB-12 Year Last Lived Together - 4th former wife"
   EXRELATION11 = "Relationship with Each Woman in E - 1st Cohab Partner"
   STRTLIVE_Y14 = "EB-5 Year Began Cohabiting - 1st Cohab Partner"
   CMCOHFC11 = "Year Began Living with 1st Former Cohabiting Partner"
   AGELIV14 = "EB-6 R s Age When Began Cohabiting (Premar) - 1st Cohab Partner"
   ENGAGTHN14 = "EB-7 Engaged When Began Living Together - 1st cohab partner"
   STOPLIVE_Y14 = "EB-12 Year Last Lived Together - 1st cohab partner"
   FWPDOB_Y = "EC-1 Year Former Wife Was Born - 1st"
   FWPAGE = "EC-2 Former wife's Age at End of Union - 1st"
   WIF1RACE = "RACE-recode-like variable for 1st former wife"
   WIF1NRACE = "HISPRACE2-recode-like variable for 1st former wife"
   FWPMARBF = "EC-6 Ever Married (Before) - 1st Former Wife"
   FWPDOB_Y2 = "EC-1 Year former wife Was Born - 2nd"
   FWPAGE2 = "EC-2 former wife's Age at End of Union - 2nd"
   FWPMARBF2 = "EC-6 Ever Married (Before) - 2nd former wife"
   FWPDOB_Y3 = "EC-1 Year former wife Was Born - 3rd"
   FWPAGE3 = "EC-2 former wife's Age at End of Union - 3rd"
   FWPMARBF3 = "EC-6 Ever Married (Before) - 3rd former wife"
   FWPDOB_Y4 = "EC-1 Year former wife Was Born - 4th"
   FWPAGE4 = "EC-2 former wife's Age at End of Union - 4th"
   FWPMARBF4 = "EC-6 Ever Married (Before) - 4th former wife"
   FWPDOB_Y11 = "EC-1 Year 1st Cohab Partner was Born"
   FWPAGE11 = "EC-2 1st Cohab Partner's Age at End of Union"
   COH1RACE = "RACE-recode-like variable for 1st former cohabiting partner"
   COH1NRACE = "HISPRACE2-recode-like variable for 1st former cohabiting partner"
   FWPMARBF11 = "EC-6 Ever Married (Before) - 1st Cohab Partner"
   FWPBIOKD = "ED-1 Ever Had Biological Children with Her - 1st Former Wife"
   FWPNUMKD = "ED-2 # of Biological Children R had with 1st Former Wife"
   FWPCHSEX = "ED-5 Sex of bio child w/1st former wife - 1st child"
   FWPCHDOB_Y = "ED-6 Year of birth of bio child w/1st former wife - 1st child"
   FWCHMARB = "ED-8 Married to 1st former wife at time of the birth - 1st child"
   FWPCHRES = "ED-9 Living with 1st former wife at time of the birth-1st child"
   FWPCHLRN = "ED-10 When R found out 1st former wife was pregnant - 1st child"
   FWPCHLIV01 = "ED-11 Where 1st bio child w/1st FW usually lives now - 1st"
   FWPCHLIV02 = "ED-11 Where 1st bio child w/1st FW usually lives now - 2nd"
   FWPCHLIV03 = "ED-11 Where 1st bio child w/1st FW usually lives now - 3rd"
   FWPCHAGE = "ED-12 Current age of 1st bio child w/1st former wife"
   FWPCHSIG = "ED-13a Established legal paternity by signing application for birth certificate (1st bio child, 1st FW)"
   FWPCHCRT = "ED-13b Established legal paternity by going to court (1st bio child, 1st FW)"
   FWPCHGEN = "ED-14 Established legal paternity by blood or other genetic test (1st bio child, 1st FW)"
   FWPCHEVR = "ED-15 Did R ever live with 1st bio child w/1st former wife"
   FWPCHFAR = "ED-16 How far away 1st bio child w/1st former wife lives now"
   FWPRWANT = "ED-17 R wanted child before 1st FW got pregnant w/1st child"
   FWPSOON = "ED-18 Pregnancy timing of 1st child w/1st former wife"
   FWPSOONN = "ED-18a How much sooner 1st child w/ 1st former wife - Unit"
   FWPSOONMY = "ED-18b How much sooner 1st child w/1st former - M/Y"
   FWPHPYPG = "ED-19 R's happiness about pregnancy w/1st child w/1st FW"
   FWPCHSEX2 = "ED-5 Sex of bio child w/1st former wife - 2nd child"
   FWPCHDOB_Y2 = "ED-6 Year of birth of bio child w/1st former wife - 2nd child"
   MULTBIRT42 = "ED-7 Verifying if multiple births - 2nd child w/1st former wife"
   FWCHMARB2 = "ED-8 Married to 1st former wife at time of the birth - 2nd child"
   FWPCHRES2 = "ED-9 Living with 1st former wife at time of the birth-2nd child"
   FWPCHLRN2 = "ED-10 When R found out 1st former wife was pregnant - 2nd child"
   FWPCHLIV11 = "ED-11 Where 2nd bio child w/1st FW usually lives now - 1st"
   FWPCHLIV12 = "ED-11 Where 2nd bio child w/1st FW usually lives now - 2nd"
   FWPCHLIV13 = "ED-11 Where 2nd bio child w/1st FW usually lives now - 3rd"
   FWPCHAGE2 = "ED-12 Current age of 2nd bio child w/1st former wife"
   FWPCHSIG2 = "ED-13a Established legal paternity by signing application for birth certificate (2nd bio child, 1st FW)"
   FWPCHCRT2 = "ED-13b Established legal paternity by going to court (2nd bio child, 1st FW)"
   FWPCHGEN2 = "ED-14 Established legal paternity by blood or other genetic test (2nd bio child, 1st FW)"
   FWPCHEVR2 = "ED-15 Did R ever live with 2nd bio child w/1st former wife"
   FWPCHFAR2 = "ED-16 How far away 2nd bio child w/1st former wife lives now"
   FWPRWANT2 = "ED-17 R wanted child before 1st FW got pregnant w/2nd child"
   FWPSOON2 = "ED-18 Pregnancy timing of 2nd child w/1st former wife"
   FWPSOONN2 = "ED-18a How much sooner 2nd child w/ 1st former wife - Unit"
   FWPSOONMY2 = "ED-18b How much sooner 2nd child w/ 1st former wife  - M/Y"
   FWPHPYPG2 = "ED-19 R's happiness about pregnancy w/2nd child w/1st FW"
   FWPCHSEX3 = "ED-5 Sex of bio child w/1st former wife - 3rd child"
   FWPCHDOB_Y3 = "ED-6 Year of birth of bio child w/1st former wife - 3rd child"
   MULTBIRT43 = "ED-7 Verifying if multiple births - 3rd child w/1st former wife"
   FWCHMARB3 = "ED-8 Married to 1st former wife at time of the birth - 3rd child"
   FWPCHRES3 = "ED-9 Living with 1st former wife at time of the birth-3rd child"
   FWPCHLRN3 = "ED-10 When R found out 1st former wife was pregnant - 3rd child"
   FWPCHLIV21 = "ED-11 Where 3rd bio child w/1st FW usually lives now - 1st"
   FWPCHLIV22 = "ED-11 Where 3rd bio child w/1st FW usually lives now - 2nd"
   FWPCHLIV23 = "ED-11 Where 3rd bio child w/1st FW usually lives now - 3rd"
   FWPCHAGE3 = "ED-12 Current age of 3rd bio child w/1st former wife"
   FWPCHSIG3 = "ED-13a Established legal paternity by signing application for birth certificate (3rd bio child, 1st FW)"
   FWPCHCRT3 = "ED-13b Established legal paternity by going to court (3rd bio child, 1st FW)"
   FWPCHGEN3 = "ED-14 Established legal paternity by blood or other genetic test (3rd bio child, 1st FW)"
   FWPCHEVR3 = "ED-15 Did R ever live with 3rd bio child w/1st former wife"
   FWPCHFAR3 = "ED-16 How far away 3rd bio child w/1st former wife lives now"
   FWPRWANT3 = "ED-17 R wanted child before 1st FW got pregnant w/3rd child"
   FWPSOON3 = "ED-18 Pregnancy timing of 3rd child w/1st former wife"
   FWPSOONN3 = "ED-18a How much sooner 3rd child w/ 1st former wife - Unit"
   FWPSOONMY3 = "ED-18b How much sooner 3rd child w/ 1st former wife - M/Y"
   FWPHPYPG3 = "ED-19 R's happiness about pregnancy w/3rd child w/1st FW"
   FWPCHSEX4 = "ED-5 Sex of bio child w/1st former wife - 4th child"
   FWPCHDOB_Y4 = "ED-6 Year of birth of bio child w/1st former wife - 4th child"
   MULTBIRT44 = "ED-7 Verifying if multiple births - 4th child w/1st former wife"
   FWCHMARB4 = "ED-8 Married to 1st former wife at time of the birth - 4th child"
   FWPCHRES4 = "ED-9 Living with 1st former wife at time of the birth-4th child"
   FWPCHLRN4 = "ED-10 When R found out 1st former wife was pregnant - 4th child"
   FWPCHLIV31 = "ED-11 Where 4th bio child w/1st FW usually lives now - 1st"
   FWPCHLIV32 = "ED-11 Where 4th bio child w/1st FW usually lives now - 2nd"
   FWPCHLIV33 = "ED-11 Where 4th bio child w/1st FW usually lives now - 3rd"
   FWPCHAGE4 = "ED-12 Current age of 4th bio child w/1st former wife"
   FWPCHSIG4 = "ED-13a Established legal paternity by signing application for birth certificate (4th bio child, 1st FW)"
   FWPCHCRT4 = "ED-13b Established legal paternity by going to court (4th bio child, 1st FW)"
   FWPCHGEN4 = "ED-14 Established legal paternity by blood or other genetic test (4th bio child, 1st FW)"
   FWPCHEVR4 = "ED-15 Did R ever live with 4th bio child w/1st former wife"
   FWPCHFAR4 = "ED-16 How far away 4th bio child w/1st former wife lives now"
   FWPRWANT4 = "ED-17 R wanted child before 1st FW got pregnant w/4th child"
   FWPSOON4 = "ED-18 Pregnancy timing of 4th child w/1st former wife"
   FWPSOONN4 = "ED-18a How much sooner 4th child w/ 1st former wife - Unit"
   FWPSOONMY4 = "ED-18b How much sooner 4th child w/ 1st former wife  - M/Y"
   FWPHPYPG4 = "ED-19 R's happiness about pregnancy w/4th child w/1st FW"
   FWPCHSEX5 = "ED-5 Sex of bio child w/1st former wife - 5th child"
   FWPCHDOB_Y5 = "ED-6 Year of birth of bio child w/1st former wife - 5th child"
   MULTBIRT45 = "ED-7 Verifying if multiple births - 5th child w/1st former wife"
   FWCHMARB5 = "ED-8 Married to 1st former wife at time of the birth - 5th child"
   FWPCHRES5 = "ED-9 Living with 1st former wife at time of the birth-5th child"
   FWPCHLRN5 = "ED-10 When R found out 1st former wife was pregnant - 5th child"
   FWPCHLIV41 = "ED-11 Where 5th bio child w/1st FW usually lives now - 1st"
   FWPCHLIV42 = "ED-11 Where 5th bio child w/1st FW usually lives now - 2nd"
   FWPCHLIV43 = "ED-11 Where 5th bio child w/1st FW usually lives now - 3rd"
   FWPCHAGE5 = "ED-12 Current age of 5th bio child w/1st former wife"
   FWPCHSIG5 = "ED-13a Established legal paternity by signing application for birth certificate (5th bio child, 1st FW)"
   FWPCHCRT5 = "ED-13b Established legal paternity by going to court (5th bio child, 1st FW)"
   FWPCHGEN5 = "ED-14 Established legal paternity by blood or other genetic test (5th bio child, 1st FW)"
   FWPCHEVR5 = "ED-15 Did R ever live with 5th bio child w/1st former wife"
   FWPCHFAR5 = "ED-16 How far away 5th bio child w/1st former wife lives now"
   FWPRWANT5 = "ED-17 R wanted child before 1st FW got pregnant w/5th child"
   FWPSOON5 = "ED-18 Pregnancy timing of 5th child w/1st former wife"
   FWPSOONN5 = "ED-18a How much sooner 5th child w/ 1st former wife - Unit"
   FWPSOONMY5 = "ED-18b How much sooner 5th child w/ 1st former wife - M/Y"
   FWPHPYPG5 = "ED-19 R's happiness about pregnancy w/5th child w/1st FW"
   FWPBIOKD2 = "ED-1 Ever Had Biological Children with Her - 2nd former wife"
   FWPNUMKD2 = "ED-2 # of Biological Children R had with 2nd former wife"
   FWPCHSEX11 = "ED-5 Sex of bio child w/2nd former wife - 1st child"
   FWPCHDOB_Y11 = "ED-6 Year of birth of bio child w/2nd former wife - 1st child"
   FWCHMARB11 = "ED-8 Married to 2nd former wife at time of the birth - 1st child"
   FWPCHRES11 = "ED-9 Living with 2nd former wife at time of the birth-1st child"
   FWPCHLRN11 = "ED-10 When R found out 2nd former wife was pregnant - 1st child"
   FWPCHLIV101 = "ED-11 Where 1st bio child w/2nd FW usually lives now - 1st"
   FWPCHLIV102 = "ED-11 Where 1st bio child w/2nd FW usually lives now - 2nd"
   FWPCHAGE11 = "ED-12 Current age of 1st bio child w/2nd former wife"
   FWPCHSIG11 = "ED-13a Established legal paternity by signing application for birth certificate (1st bio child, 2nd FW)"
   FWPCHCRT11 = "ED-13b Established legal paternity by going to court (1st bio child, 2nd FW)"
   FWPCHGEN11 = "ED-14 Established legal paternity by blood or other genetic test (1st bio child, 2nd FW)"
   FWPCHEVR11 = "ED-15 Did R ever live with 1st bio child w/2nd former wife"
   FWPCHFAR11 = "ED-16 How far away 1st bio child w/2nd former wife lives now"
   FWPRWANT11 = "ED-17 R wanted child before 2nd FW got pregnant w/1st child"
   FWPSOON11 = "ED-18 Pregnancy timing of 1st child w/2nd former wife"
   FWPSOONN11 = "ED-18a How much sooner 1st child w/ 2nd former wife - Unit"
   FWPSOONMY11 = "ED-18b How much sooner 1st  child w/ 2nd former wife - M/Y"
   FWPHPYPG11 = "ED-19 R's happiness about pregnancy w/1st child w/2nd FW"
   FWPCHSEX12 = "ED-5 Sex of bio child w/2nd former wife - 2nd child"
   FWPCHDOB_Y12 = "ED-6 Year of birth of bio child w/2nd former wife - 2nd child"
   MULTBIRT52 = "ED-7 Verifying if multiple births - 2nd child w/2nd former wife"
   FWCHMARB12 = "ED-8 Married to 2nd former wife at time of the birth - 2nd child"
   FWPCHRES12 = "ED-9 Living with 2nd former wife at time of the birth-2nd child"
   FWPCHLRN12 = "ED-10 When R found out 2nd former wife was pregnant - 2nd child"
   FWPCHLIV111 = "ED-11 Where 2nd bio child w/2nd FW usually lives now - 1st"
   FWPCHLIV112 = "ED-11 Where 2nd bio child w/2nd FW usually lives now - 2nd"
   FWPCHAGE12 = "ED-12 Current age of 2nd bio child w/2nd former wife"
   FWPCHSIG12 = "ED-13a Established legal paternity by signing application for birth certificate (2nd bio child, 2nd FW)"
   FWPCHCRT12 = "ED-13b Established legal paternity by going to court (2nd bio child, 2nd FW)"
   FWPCHGEN12 = "ED-14 Established legal paternity by blood or other genetic test (2nd bio child, 2nd FW)"
   FWPCHEVR12 = "ED-15 Did R ever live with 2nd bio child w/2nd former wife"
   FWPCHFAR12 = "ED-16 How far away 2nd bio child w/2nd former wife lives now"
   FWPRWANT12 = "ED-17 R wanted child before 2nd FW got pregnant w/2nd child"
   FWPSOON12 = "ED-18 Pregnancy timing of 2nd child w/2nd former wife"
   FWPSOONN12 = "ED-18a How much sooner 2nd child w/ 2nd former wife - Unit"
   FWPSOONMY12 = "ED-18b How much sooner 2nd child w/ 2nd former wife - M/Y"
   FWPHPYPG12 = "ED-19 R's happiness about pregnancy w/2nd child w/2nd FW"
   FWPCHSEX13 = "ED-5 Sex of bio child w/2nd former wife - 3rd child"
   FWPCHDOB_Y13 = "ED-6 Year of birth of bio child w/2nd former wife - 3rd child"
   MULTBIRT53 = "ED-7 Verifying if multiple births - 3rd child w/2nd former wife"
   FWCHMARB13 = "ED-8 Married to 2nd former wife at time of the birth - 3rd child"
   FWPCHRES13 = "ED-9 Living with 2nd former wife at time of the birth-3rd child"
   FWPCHLRN13 = "ED-10 When R found out 2nd former wife was pregnant - 3rd child"
   FWPCHLIV121 = "ED-11 Where 3rd bio child w/2nd FW usually lives now - 1st"
   FWPCHLIV122 = "ED-11 Where 3rd bio child w/2nd FW usually lives now - 2nd"
   FWPCHAGE13 = "ED-12 Current age of 3rd bio child w/2nd former wife"
   FWPCHSIG13 = "ED-13a Established legal paternity by signing application for birth certificate (3rd bio child, 2nd FW)"
   FWPCHCRT13 = "ED-13b Established legal paternity by going to court (3rd bio child, 2nd FW)"
   FWPCHGEN13 = "ED-14 Established legal paternity by blood or other genetic test (3rd bio child, 2nd FW)"
   FWPCHEVR13 = "ED-15 Did R ever live with 3rd bio child w/2nd former wife"
   FWPCHFAR13 = "ED-16 How far away 3rd bio child w/2nd former wife lives now"
   FWPRWANT13 = "ED-17 R wanted child before 2nd FW got pregnant w/3rd child"
   FWPSOON13 = "ED-18 Pregnancy timing of 3rd child w/2nd former wife"
   FWPSOONN13 = "ED-18a How much sooner 3rd child w/ 2nd former wife - Unit"
   FWPSOONMY13 = "ED-18b How much sooner 3rd child w/ 2nd former wife - M/Y"
   FWPHPYPG13 = "ED-19 R's happiness about pregnancy w/3rd child w/2nd FW"
   FWPCHSEX14 = "ED-5 Sex of bio child w/2nd former wife - 4th child"
   FWPCHDOB_Y14 = "ED-6 Year of birth of bio child w/2nd former wife - 4th child"
   MULTBIRT54 = "ED-7 Verifying if multiple births - 4th child w/2nd former wife"
   FWCHMARB14 = "ED-8 Married to 2nd former wife at time of the birth - 4th child"
   FWPCHRES14 = "ED-9 Living with 2nd former wife at time of the birth-4th child"
   FWPCHLRN14 = "ED-10 When R found out 2nd former wife was pregnant - 4th child"
   FWPCHLIV131 = "ED-11 Where 4th bio child w/2nd FW usually lives now - 1st"
   FWPCHLIV132 = "ED-11 Where 4th bio child w/2nd FW usually lives now - 2nd"
   FWPCHAGE14 = "ED-12 Current age of 4th bio child w/2nd former wife"
   FWPCHSIG14 = "ED-13a Established legal paternity by signing application for birth certificate"
   FWPCHCRT14 = "ED-13b Established legal paternity by going to court"
   FWPCHGEN14 = "ED-14 Established legal paternity by blood or other genetic test"
   FWPCHEVR14 = "ED-15 Did R ever live with 4th bio child w/2nd former wife"
   FWPCHFAR14 = "ED-16 How far away 4th bio child w/2nd former wife lives now"
   FWPRWANT14 = "ED-17 R wanted child before 2nd FW got pregnant w/4th child"
   FWPSOON14 = "ED-18 Pregnancy timing of 4th child w/2nd former wife"
   FWPSOONN14 = "ED-18a How much sooner 4th child w/ 2nd former wife - Unit"
   FWPSOONMY14 = "ED-18b How much sooner 4th child w/2nd former wife - M/Y"
   FWPHPYPG14 = "ED-19 R's happiness about pregnancy w/4th child w/2nd FW"
   FWPBIOKD3 = "ED-1 Ever Had Biological Children with Her - 3rd former wife"
   FWPNUMKD3 = "ED-2 # of Biological Children R had with 3rd former wife"
   FWPCHSEX21 = "ED-5 Sex of bio child w/3rd former wife - 1st child"
   FWPCHDOB_Y21 = "ED-6 Year of birth of bio child w/3rd former wife - 1st child"
   FWCHMARB21 = "ED-8 Married to 3rd former wife at time of the birth - 1st child"
   FWPCHRES21 = "ED-9 Living with 3rd former wife at time of the birth-1st child"
   FWPCHLRN21 = "ED-10 When R found out 3rd former wife was pregnant - 1st child"
   FWPCHLIV201 = "ED-11 Where 1st bio child w/3rd FW usually lives now - 1st"
   FWPCHLIV202 = "ED-11 Where 1st bio child w/3rd FW usually lives now - 2nd"
   FWPCHAGE21 = "ED-12 Current age of 1st bio child w/3rd former wife"
   FWPCHSIG21 = "ED-13a Established legal paternity by signing application for birth certificate (1st bio child, 3rd FW)"
   FWPCHCRT21 = "ED-13b Established legal paternity by going to court (1st bio child, 3rd FW)"
   FWPCHGEN21 = "ED-14 Established legal paternity by blood or other genetic test (1st bio child, 3rd FW)"
   FWPCHEVR21 = "ED-15 Did R ever live with 1st bio child w/3rd former wife"
   FWPCHFAR21 = "ED-16 How far away 1st bio child w/3rd former wife lives now"
   FWPRWANT21 = "ED-17 R wanted child before 3rd FW got pregnant w/1st child"
   FWPSOON21 = "ED-18 Pregnancy timing of 1st child w/3rd former wife"
   FWPSOONN21 = "ED-18a How much sooner 1st child w/ 3rd former partner - Unit"
   FWPSOONMY21 = "ED-18b How much sooner 1st child w/ 3rd former partner - M/Y"
   FWPHPYPG21 = "ED-19 R's happiness about pregnancy w/1st child w/3rd FW"
   FWPCHSEX22 = "ED-5 Sex of bio child w/3rd former wife - 2nd child"
   FWPCHDOB_Y22 = "ED-6 Year of birth of bio child w/3rd former wife - 2nd child"
   MULTBIRT62 = "ED-7 Verifying if multiple births - 2nd child w/3rd former wife"
   FWCHMARB22 = "ED-8 Married to 3rd former wife at time of the birth - 2nd child"
   FWPCHRES22 = "ED-9 Living with 3rd former wife at time of the birth-2nd child"
   FWPCHLRN22 = "ED-10 When R found out 3rd former wife was pregnant - 2nd child"
   FWPCHLIV211 = "ED-11 Where 2nd bio child w/3rd FW usually lives now - 1st"
   FWPCHLIV212 = "ED-11 Where 2nd bio child w/3rd FW usually lives now - 2nd"
   FWPCHAGE22 = "ED-12 Current age of 2nd bio child w/3rd former wife"
   FWPCHSIG22 = "ED-13a Established legal paternity by signing application for birth certificate (2nd bio child, 3rd FW)"
   FWPCHCRT22 = "ED-13b Established legal paternity by going to court (2nd bio child, 3rd FW)"
   FWPCHGEN22 = "ED-14 Established legal paternity by blood or other genetic test (2nd bio child, 3rd FW)"
   FWPCHEVR22 = "ED-15 Did R ever live with 2nd bio child w/3rd former wife"
   FWPCHFAR22 = "ED-16 How far away 2nd bio child w/3rd former wife lives now"
   FWPRWANT22 = "ED-17 R wanted child before 3rd FW got pregnant w/2nd child"
   FWPSOON22 = "ED-18 Pregnancy timing of 2nd child w/3rd former wife"
   FWPSOONN22 = "ED-18a How much sooner 2nd child w/ 3rd former partner - Unit"
   FWPSOONMY22 = "ED-18b How much sooner 2nd child w/ 3rd former partner - M/Y"
   FWPHPYPG22 = "ED-19 R's happiness about pregnancy w/2nd child w/3rd FW"
   FWPCHSEX23 = "ED-5 Sex of bio child w/3rd former wife - 3rd child"
   FWPCHDOB_Y23 = "ED-6 Year of birth of bio child w/3rd former wife - 3rd child"
   MULTBIRT63 = "ED-7 Verifying if multiple births - 3rd child w/3rd former wife"
   FWCHMARB23 = "ED-8 Married to 3rd former wife at time of the birth - 3rd child"
   FWPCHRES23 = "ED-9 Living with 3rd former wife at time of the birth-3rd child"
   FWPCHLRN23 = "ED-10 When R found out 3rd former wife was pregnant - 3rd child"
   FWPCHLIV221 = "ED-11 Where 3rd bio child w/3rd FW usually lives now - 1st"
   FWPCHLIV222 = "ED-11 Where 3rd bio child w/3rd FW usually lives now - 2nd"
   FWPCHAGE23 = "ED-12 Current age of 3rd bio child w/3rd former wife"
   FWPCHSIG23 = "ED-13a Established legal paternity by signing application for birth certificate (3rd bio child, 3rd FW)"
   FWPCHCRT23 = "ED-13b Established legal paternity by going to court (3rd bio child, 3rd FW)"
   FWPCHGEN23 = "ED-14 Established legal paternity by blood or other genetic test (3rd bio child, 3rd FW)"
   FWPCHEVR23 = "ED-15 Did R ever live with 3rd bio child w/3rd former wife"
   FWPCHFAR23 = "ED-16 How far away 3rd bio child w/3rd former wife lives now"
   FWPRWANT23 = "ED-17 R wanted child before 3rd FW got pregnant w/3rd child"
   FWPSOON23 = "ED-18 Pregnancy timing of 3rd child w/3rd former wife"
   FWPSOONN23 = "ED-18a How much sooner 3rd child w/ 3rd former partner - Unit"
   FWPSOONMY23 = "ED-18b How much sooner 3rd  child/ 3rd former partner - M/Y"
   FWPHPYPG23 = "ED-19 R's happiness about pregnancy w/3rd child w/3rd FW"
   FWPBIOKD4 = "ED-1 Ever Had Biological Children with Her - 4th former wife"
   FWPNUMKD4 = "ED-2 # of Biological Children R had with 4th former wife"
   FWPBIOKD11 = "ED-1 Ever Had Biological Children with 1st Cohab Partner"
   FWPNUMKD11 = "ED-2 # of Biological Children R had with 1st Cohab Partner"
   FWPCHSEX101 = "ED-5 Sex of bio child w/1st CP - 1st child"
   FWPCHDOB_Y101 = "ED-6 Year of birth of bio child w/1st CP - 1st child"
   FWPCHRES101 = "ED-9 Living with 1st CP at time of the birth-1st child"
   FWPCHLRN101 = "ED-10 When R found out 1st CP was pregnant - 1st child"
   FWPCHLIV1001 = "ED-11 Where 1st bio child w/1st CP usually lives now - 1st"
   FWPCHLIV1002 = "ED-11 Where 1st bio child w/1st CP usually lives now - 2nd"
   FWPCHLIV1003 = "ED-11 Where 1st bio child w/1st CP usually lives now - 3rd"
   FWPCHAGE101 = "ED-12 Current age of 1st bio child w/1st CP"
   FWPCHSIG101 = "ED-13a Established legal paternity by signing application for birth certificate (1st bio child, 1st CP)"
   FWPCHCRT101 = "ED-13b Established legal paternity by going to court (1st bio child, 1st CP)"
   FWPCHGEN101 = "ED-14 Established legal paternity by blood or other genetic test (1st bio child, 1st CP)"
   FWPCHEVR101 = "ED-15 Did R ever live with 1st bio child w/1st CP"
   FWPCHFAR101 = "ED-16 How far away 1st bio child w/1st CP lives now"
   FWPRWANT101 = "ED-17 R wanted child before 1st CP got pregnant w/1st child"
   FWPSOON101 = "ED-18 Pregnancy timing of 1st child w/1st CP"
   FWPSOONN101 = "ED-18a How much sooner 1st child w/ 1st CP - Unit"
   FWPSOONMY101 = "ED-18b How much sooner 1st child w/ 1st CP - M/Y"
   FWPHPYPG101 = "ED-19 R's happiness about pregnancy w/1st child w/1st CP"
   FWPCHSEX102 = "ED-5 Sex of bio child w/1st Cohab Partner - 2nd child"
   FWPCHDOB_Y102 = "ED-6 Year of birth of bio child w/1st Cohab Partner - 2nd child"
   MULTBIRT142 = "ED-7 Verifying if multiple births - 2nd child w/1st Cohab Partner"
   FWPCHRES102 = "ED-9 Living with 1st Cohab Partner at time of the birth-2nd child"
   FWPCHLRN102 = "ED-10 When R found out 1st Cohab Partner was pregnant - 2nd child"
   FWPCHLIV1011 = "ED-11 Where 2nd bio child w/1st CP usually lives now - 1st"
   FWPCHLIV1012 = "ED-11 Where 2nd bio child w/1st CP usually lives now - 2nd"
   FWPCHLIV1013 = "ED-11 Where 2nd bio child w/1st CP usually lives now - 3rd"
   FWPCHAGE102 = "ED-13a Established legal paternity by signing application for birth certificate (2nd bio child, 1st CP)"
   FWPCHSIG102 = "ED-13b Established legal paternity by going to court (2nd bio child, 1st CP)"
   FWPCHCRT102 = "ED-14 Established legal paternity by blood or other genetic test (2nd bio child, 1st CP)"
   FWPCHGEN102 = "ED-14 Established legal paternity by blood or other genetic test"
   FWPCHEVR102 = "ED-15 Did R ever live with 2nd bio child w/1st Cohab Partner"
   FWPCHFAR102 = "ED-16 How far away 2nd bio child w/1st Cohab Partner lives now"
   FWPRWANT102 = "ED-17 R wanted child before 1st CP got pregnant w/2nd child"
   FWPSOON102 = "ED-18 Pregnancy timing of 2nd child w/1st Cohab Partner"
   FWPSOONN102 = "ED-18a How much sooner 2nd child w/ 1st CP- Unit"
   FWPSOONMY102 = "ED-18b How much sooner 2nd  child w/ 1st CP- M/Y"
   FWPHPYPG102 = "ED-19 R's happiness about pregnancy w/2nd child w/1st CP"
   FWPCHSEX103 = "ED-5 Sex of bio child w/1st Cohab Partner - 3rd child"
   FWPCHDOB_Y103 = "ED-6 Year of birth of bio child w/1st Cohab Partner - 3rd child"
   MULTBIRT143 = "ED-7 Verifying if multiple births - 3rd child w/1st Cohab Partner"
   FWPCHRES103 = "ED-9 Living with 1st Cohab Partner at time of the birth-3rd child"
   FWPCHLRN103 = "ED-10 When R found out 1st Cohab Partner was pregnant - 3rd child"
   FWPCHLIV1021 = "ED-11 Where 3rd bio child w/1st CP usually lives now - 1st"
   FWPCHLIV1022 = "ED-11 Where 3rd bio child w/1st CP usually lives now - 2nd"
   FWPCHLIV1023 = "ED-11 Where 3rd bio child w/1st CP usually lives now - 3rd"
   FWPCHAGE103 = "ED-12 Current age of 3rd bio child w/1st Cohab Partner"
   FWPCHSIG103 = "ED-13a Established legal paternity by signing application for birth certificate (3rd bio child, 1st CP)"
   FWPCHCRT103 = "ED-13b Established legal paternity by going to court (3rd bio child, 1st CP)"
   FWPCHGEN103 = "ED-14 Established legal paternity by blood or other genetic test (3rd bio child, 1st CP)"
   FWPCHEVR103 = "ED-15 Did R ever live with 3rd bio child w/1st Cohab Partner"
   FWPCHFAR103 = "ED-16 How far away 3rd bio child w/1st Cohab Partner lives now"
   FWPRWANT103 = "ED-17 R wanted child before 1st CP got pregnant w/3rd child"
   FWPSOON103 = "ED-18 Pregnancy timing of 3rd child w/1st Cohab Partner"
   FWPSOONN103 = "ED-18a How much sooner 3rd child w/ 1st CP - Unit"
   FWPSOONMY103 = "ED-18b How much sooner 3rd child w/ 1st CP - M/Y"
   FWPHPYPG103 = "ED-19 R's happiness about pregnancy w/3rd child w/1st CP"
   FWPCHSEX104 = "ED-5 Sex of bio child w/1st Cohab Partner - 4th child"
   FWPCHDOB_Y104 = "ED-6 Year of birth of bio child w/1st Cohab Partner - 4th child"
   MULTBIRT144 = "ED-7 Verifying if multiple births - 4th child w/1st Cohab Partner"
   FWPCHRES104 = "ED-9 Living with 1st Cohab Partner at time of the birth-4th child"
   FWPCHLRN104 = "ED-10 When R found out 1st Cohab Partner was pregnant - 4th child"
   FWPCHLIV1031 = "ED-11 Where 4th bio child w/1st CP usually lives now - 1st"
   FWPCHLIV1032 = "ED-11 Where 4th bio child w/1st CP usually lives now - 2nd"
   FWPCHLIV1033 = "ED-11 Where 4th bio child w/1st CP usually lives now - 3rd"
   FWPCHAGE104 = "ED-12 Current age of 4th bio child w/1st Cohab Partner"
   FWPCHSIG104 = "ED-13a Established legal paternity by signing application for birth certificate (4th bio child, 1st CP)"
   FWPCHCRT104 = "ED-13b Established legal paternity by going to court (4th bio child, 1st CP)"
   FWPCHGEN104 = "ED-14 Established legal paternity by blood or other genetic test (4th bio child, 1st CP)"
   FWPCHEVR104 = "ED-15 Did R ever live with 4th bio child w/1st Cohab Partner"
   FWPCHFAR104 = "ED-16 How far away 4th bio child w/1st Cohab Partner lives now"
   FWPRWANT104 = "ED-17 R wanted child before 1st CP got pregnant w/4th child"
   FWPSOON104 = "ED-18 Pregnancy timing of 4th child w/1st Cohab Partner"
   FWPSOONN104 = "ED-18a How much sooner 4th child w/ 1st CP - Unit"
   FWPSOONMY104 = "ED-18b How much sooner 4th child w/ 1st CP - M/Y"
   FWPHPYPG104 = "ED-19 R's happiness about pregnancy w/4th child w/1st CP"
   E_OKAKIDS = "# of 1st former wife's children R adopted"
   FWPOTKID = "EE-1 1st FW had any other children when R began living w/her"
   FWPOKNUM = "EE-2 # other children 1st FW had when R began living w/her"
   FWPOKWTH = "EE-3 Did 1st FW's child/ren ever live with R"
   FWPOKWTHN = "EE-4 # of 1st FW's children who lived with R"
   FWPOKSEX = "EE-6 Sex of 1st FW's child R lived with-1st child"
   FWPOKAD = "EE-7 R legally adopted or became legal guardian-1st FW,1st child"
   FWPOKLIV1 = "EE-8 Where 1st FW's child R lived with usually lives now-1st child,1st ment"
   FWPOKLIV2 = "EE-8 Where 1st FW's child R lived with usually lives now-1st child,2nd ment"
   FWPOKFAR = "EE-9 How far away 1st FW's child R lived with lives now-1st child"
   FWPOKAGE = "EE-10 Age of 1st FW's child R lived with-1st child"
   FWPOKSEX2 = "EE-6 Sex of 1st FW's child R lived with-2nd child"
   FWPOKAD2 = "EE-7 R legally adopted or became legal guardian-1st FW,2nd child"
   FWPOKLIV9 = "EE-8 Where 1st FW's child R lived with usually lives now-2nd child,1st ment"
   FWPOKLIV10 = "EE-8 Where 1st FW's child R lived with usually lives now-2nd child,2nd ment"
   FWPOKFAR2 = "EE-9 How far away 1st FW's child R lived with lives now-2nd child"
   FWPOKAGE2 = "EE-10 Age of 1st FW's child R lived with-2nd child"
   FWPOKSEX3 = "EE-6 Sex of 1st FW's child R lived with-3rd child"
   FWPOKAD3 = "EE-7 R legally adopted or became legal guardian-1st FW,3rd child"
   FWPOKLIV17 = "EE-8 Where 1st FW's child R lived with usually lives now-3rd child,1st ment"
   FWPOKLIV18 = "EE-8 Where 1st FW's child R lived with usually lives now-3rd child,2nd ment"
   FWPOKFAR3 = "EE-9 How far away 1st FW's child R lived with lives now-3rd child"
   FWPOKAGE3 = "EE-10 Age of 1st FW's child R lived with-3rd child"
   FWPOKSEX4 = "EE-6 Sex of 1st FW's child R lived with-4th child"
   FWPOKAD4 = "EE-7 R legally adopted or became legal guardian-1st FW,4th child"
   FWPOKLIV25 = "EE-8 Where 1st FW's child R lived with usually lives now-4th child,1st ment"
   FWPOKLIV26 = "EE-8 Where 1st FW's child R lived with usually lives now-4th child,2nd ment"
   FWPOKFAR4 = "EE-9 How far away 1st FW's child R lived with lives now-4th child"
   FWPOKAGE4 = "EE-10 Age of 1st FW's child R lived with-4th child"
   FWPOKSEX5 = "EE-6 Sex of 1st FW's child R lived with-5th child"
   FWPOKAD5 = "EE-7 R legally adopted or became legal guardian-1st FW,5th child"
   FWPOKLIV33 = "EE-8 Where 1st FW's child R lived with usually lives-5th child,1st ment"
   FWPOKLIV34 = "EE-8 Where 1st FW's child R lived with usually lives-5th child,2nd ment"
   FWPOKFAR5 = "EE-9 How far away 1st FW's child R lived with lives now-5th child"
   FWPOKAGE5 = "EE-10 Age of 1st FW's child R lived with-5th child"
   E_OKAKIDS2 = "# of 2nd former wife's children R adopted"
   FWPOTKID2 = "EE-1 2nd FW had any other children when R began living w/her"
   FWPOKNUM2 = "EE-2 # other children 2nd FW had when R began living w/her"
   FWPOKWTH2 = "EE-3 Did 2nd FW's child/ren ever live with R"
   FWPOKWTHN2 = "EE-4 # of 2nd FW's children who lived with R"
   FWPOKSEX11 = "EE-6 Sex of 2nd FW's child R lived with-1st child"
   FWPOKAD11 = "EE-7 R legally adopted or became legal guardian-2nd FW,1st child"
   FWPOKLIV81 = "EE-8 Where 2nd FW's child R lived with lives now-1st child,1st ment"
   FWPOKFAR11 = "EE-9 How far away 2nd FW's child R lived with lives now-1st child"
   FWPOKAGE11 = "EE-10 Age of 2nd FW's child R lived with-1st child"
   FWPOKSEX12 = "EE-6 Sex of 2nd FW's child R lived with-2nd child"
   FWPOKAD12 = "EE-7 R legally adopted or became legal guardian-2nd FW,2nd child"
   FWPOKLIV89 = "EE-8 Where 2nd FW's child R lived with lives now-2nd child,1st ment"
   FWPOKFAR12 = "EE-9 How far away 2nd FW's child R lived with lives now-2nd child"
   FWPOKAGE12 = "EE-10 Age of 2nd FW's child R lived with-2nd child"
   FWPOKSEX13 = "EE-6 Sex of 2nd FW's child R lived with-3rd child"
   FWPOKAD13 = "EE-7 R legally adopted or became legal guardian-2nd FW,3rd child"
   FWPOKLIV97 = "EE-8 Where 2nd FW's child R lived with lives now-3rd child,1st ment"
   FWPOKFAR13 = "EE-9 How far away 2nd FW's child R lived with lives now-3rd child"
   FWPOKAGE13 = "EE-10 Age of 2nd FW's child R lived with-3rd child"
   FWPOKSEX14 = "EE-6 Sex of 2nd FW's child R lived with-4th child"
   FWPOKAD14 = "EE-7 R legally adopted or became legal guardian-2nd FW,4th child"
   FWPOKLIV105 = "EE-8 Where 2nd FW's child R lived with lives-4th child,1st ment"
   FWPOKFAR14 = "EE-9 How far away 2nd FW's child R lived with lives-4th child"
   FWPOKAGE14 = "EE-10 Age of 2nd FW's child R lived with-4th child"
   FWPOKSEX15 = "EE-6 Sex of 2nd FW's child R lived with-5th child"
   FWPOKAD15 = "EE-7 R legally adopted or became legal guardian-2nd FW,5th child"
   FWPOKLIV113 = "EE-8 Where 2nd FW's child R lived with lives-5th child,1st ment"
   FWPOKFAR15 = "EE-9 How far away 2nd FW's child R lived with lives-5th child"
   FWPOKAGE15 = "EE-10 Age of 2nd FW's child R lived with-5th child"
   E_OKAKIDS3 = "# of 3rd Former Wife's Children R Adopted"
   FWPOTKID3 = "EE-1 3rd FW had any other children when R began living w/her"
   FWPOKNUM3 = "EE-2 # other children 3rd FW had when R began living w/her"
   FWPOKWTH3 = "EE-3 Did 3rd FW's child/ren ever live with R"
   FWPOKWTHN3 = "EE-4 # of 3rd FW's children who lived with R"
   FWPOKSEX21 = "EE-6 Sex of 3rd FW's child R lived with-1st child"
   FWPOKAD21 = "EE-7 R legally adopted or became legal guardian-3rd FW,1st child"
   FWPOKLIV161 = "EE-8 Where 3rd FW's child R lived with lives now-1st child,1st ment"
   FWPOKFAR21 = "EE-9 How far away 3rd FW's child R lived with lives now-1st child"
   FWPOKAGE21 = "EE-10 Age of 3rd FW's child R lived with-1st child"
   FWPOKSEX22 = "EE-6 Sex of 3rd FW's child R lived with-2nd child"
   FWPOKAD22 = "EE-7 R legally adopted or became legal guardian-3rd FW,2nd child"
   FWPOKLIV169 = "EE-8 Where 3rd FW's child R lived with lives now-2nd child,1st ment"
   FWPOKFAR22 = "EE-9 How far away 3rd FW's child R lived with lives now-2nd child"
   FWPOKAGE22 = "EE-10 Age of 3rd FW's child R lived with-2nd child"
   E_OKAKIDS4 = "# of 4th Former Wife's Children R Adopted"
   FWPOTKID4 = "EE-1 4th FW had any other children when R began living w/her"
   FWPOKNUM4 = "EE-2 # other children 4th FW had when R began living w/her"
   FWPOKWTH4 = "EE-3 Did 4th FW's child/ren ever live with R"
   FWPOKWTHN4 = "EE-4 # of 4th FW's children who lived with R"
   FWPOKSEX31 = "EE-6 Sex of 4th FW's child who lived with R - 1st child"
   FWPOKAD31 = "EE-7 R legally adopted or became legal guardian -4th FW, 1st child"
   FWPOKAGE31 = "EE-10 Current age of 4th FW's child R lived with - 1st child"
   E_OKAKIDS11 = "# of 1st Cohab Partner s Children R Adopted"
   FWPOTKID11 = "EE-1 1st CP had any other children when R began living w/her"
   FWPOKNUM11 = "EE-2 # other children 1st CP had when R began living w/her"
   FWPOKWTH11 = "EE-3 Did 1st CP's child/ren ever live with R"
   FWPOKWTHN11 = "EE-4 # of 1st CP's children who lived with R"
   FWPOKSEX101 = "EE-6 Sex of 1st CP's child R lived with-1st child"
   FWPOKAD101 = "EE-7 R legally adopted or became legal guardian-1st CP, 1st child"
   FWPOKLIV801 = "EE-8 Where 1st CP's child R lived with lives now-1st child,1st ment"
   FWPOKFAR101 = "EE-9 How far away 1st CP's child R lived with lives now-1st child"
   FWPOKAGE101 = "EE-10 Age of 1st CP's child R lived with-1st child"
   FWPOKSEX102 = "EE-6 Sex of 1st CP's child R lived with-2nd child"
   FWPOKAD102 = "EE-7 R legally adopted or became legal guardian-1st CP, 2nd child"
   FWPOKLIV809 = "EE-8 Where 1st CP's child R lived with lives now-2nd child,1st ment"
   FWPOKFAR102 = "EE-9 How far away 1st CP's child R lived with lives now-2nd child"
   FWPOKAGE102 = "EE-10 Age of 1st CP's child R lived with-2nd child"
   FWPOKSEX103 = "EE-6 Sex of 1st CP's child R lived with-3rd child"
   FWPOKAD103 = "EE-7 R legally adopted or became legal guardian-1st CP, 3rd child"
   FWPOKLIV817 = "EE-8 Where 1st CP's child R lived with lives now-3rd child,1st ment"
   FWPOKFAR103 = "EE-9 How far away 1st CP's child R lived with lives now-3rd child"
   FWPOKAGE103 = "EE-10 Age of 1st CP's child R lived with-3rd child"
   FWPOKSEX104 = "EE-6 Sex of 1st CP's child R lived with-4th child"
   FWPOKAD104 = "EE-7 R legally adopted or became legal guardian-1st CP, 4th child"
   FWPOKLIV825 = "EE-8 Where 1st CP's child R lived with lives now-4th child,1st ment"
   FWPOKFAR104 = "EE-9 How far away 1st CP's child R lived with lives now-4th child"
   FWPOKAGE104 = "EE-10 Age of 1st CP's child R lived with-4th child"
   FWPOKSEX105 = "EE-6 Sex of 1st CP's child R lived with-5th child"
   FWPOKAD105 = "EE-7 R legally adopted or became legal guardian-1st CP, 5th child"
   FWPOKLIV833 = "EE-8 Where 1st CP's child R lived with lives-5th child,1st ment"
   FWPOKFAR105 = "EE-9 How far away 1st CP's child R lived with lives now-5th child"
   FWPOKAGE105 = "EE-10 Age of 1st CP's child R lived with-5th child"
   FWPOKSEX106 = "EE-6 Sex of 1st CP's child R lived with-6th child"
   FWPOKAD106 = "EE-7 R legally adopted or became legal guardian-1st CP, 6th child"
   FWPOKLIV841 = "EE-8 Where 1st CP's child R lived with lives-6th child,1st ment"
   FWPOKFAR106 = "EE-9 How far away 1st CP's child R lived with lives now-6th child"
   FWPOKAGE106 = "EE-10 Age of 1st CP's child R lived with-6th child"
   E_NBAKIDS = "# of other nonbiological children adopted by R w/1st FW"
   FWPNBEVR = "EF-1 R & 1st FW ever had any other nonbio children under care"
   FWPNBNUM = "EF-2 # other nonbio children under R & 1st FW's care"
   FWPNBREL = "EF-4 Other nonbio child R cared for related by blood/marr-1st FW,1st child"
   FWPNBFOS = "EF-5 Other nonbio child R cared for foster child/placed by agency-1st FW,1st child"
   FWPNBSEX = "EF-6 Sex of other nonbio child R cared for-1st FW,1st child"
   FWPNBAD = "EF-7 R adopted/became legal guardian of other nonbio child R cared for-1st FW,1st child"
   FWPNBLIV1 = "EF-8 Where oth child R cared for lives now-1st FW,1st child,1st ment"
   FWPNBLIV2 = "EF-8 Where oth child R cared for lives now-1st FW,1st child,2nd ment"
   FWPNBFAR = "EF-9 How far away oth child R cared for lives now-1st FW,1st child"
   FWPNBAGE = "EF-10 Age of oth child R cared for-1st FW,1st child"
   FWPNBREL2 = "EF-4 Other nonbio child R cared for related by blood/marr-1st FW,2nd child"
   FWPNBFOS2 = "EF-5 Other nonbio child R cared for foster child/placed by agency-1st FW,2nd child"
   FWPNBSEX2 = "EF-6 Sex of other nonbio child R cared for-1st FW,2nd child"
   FWPNBAD2 = "EF-7 R adopted/became legal guardian of other nonbio child R cared for-1st FW,2nd child"
   FWPNBLIV9 = "EF-8 Where oth child R cared for lives now-1st FW,2nd child,1st ment"
   FWPNBLIV10 = "EF-8 Where oth child R cared for lives now-1st FW,2nd child,2nd ment"
   FWPNBFAR2 = "EF-9 How far away oth child R cared for lives now-1st FW,2nd child"
   FWPNBAGE2 = "EF-10 Age of oth child R cared for-1st FW,2nd child"
   FWPNBREL3 = "EF-4 Other nonbio child R cared for related by blood/marr-1st FW,3rd child"
   FWPNBFOS3 = "EF-5 Other nonbio child R cared for foster child/placed by agency-1st FW,3rd child"
   FWPNBSEX3 = "EF-6 Sex of other nonbio child R cared for-1st FW,3rd child"
   FWPNBAD3 = "EF-7 R adopted/became legal guardian of other nonbio child R cared for-1st FW,3rd child"
   FWPNBLIV17 = "EF-8 Where oth child R cared for lives now-1st FW,3rd child,1st ment"
   FWPNBLIV18 = "EF-8 Where oth child R cared for lives now-1st FW,3rd child,2nd ment"
   FWPNBFAR3 = "EF-9 How far away oth child R cared for lives now-1st FW,3rd child"
   FWPNBAGE3 = "EF-10 Age of oth child R cared for-1st FW,3rd child"
   FWPNBREL4 = "EF-4 Other nonbio child R cared for related by blood/marr-1st FW,4th child"
   FWPNBFOS4 = "EF-5 Other nonbio child R cared for foster child/placed by agency-1st FW,4th child"
   FWPNBSEX4 = "EF-6 Sex of other nonbio child R cared for-1st FW,4th child"
   FWPNBAD4 = "EF-7 R adopted/became legal guardian of other nonbio child R cared for-1st FW,4th child"
   FWPNBLIV25 = "EF-8 Where oth child R cared for lives now-1st FW,4th child,1st ment"
   FWPNBLIV26 = "EF-8 Where oth child R cared for lives now-1st FW,4th child,2nd ment"
   FWPNBFAR4 = "EF-9 How far away oth child R cared for lives now- 1st FW,4th child"
   FWPNBAGE4 = "EF-10 Current age of oth child R cared for-1st FW,4th child"
   E_NBAKIDS2 = "# of other nonbiological children adopted w/2nd FW"
   FWPNBEVR2 = "EF-1 R & 2nd FW ever had any other nonbio children under care"
   FWPNBNUM2 = "EF-2 # other nonbio children under R & 2nd FW's care"
   FWPNBREL11 = "EF-4 Other nonbio child R cared for related by blood/marr-2nd FW,1st child"
   FWPNBFOS11 = "EF-5 Other nonbio child R cared for foster child/placed by agency-2nd FW,1st child"
   FWPNBSEX11 = "EF-6 Sex of other nonbio child R cared for-2nd FW,1st child"
   FWPNBAD11 = "EF-7 R adopted/became legal guardian of other nonbio child R cared for-2nd FW,1st child"
   FWPNBLIV81 = "EF-8 Where oth child R cared for lives now-2nd FW,1st child,1st ment"
   FWPNBFAR11 = "EF-9 How far away oth child R cared for lives now-2nd FW,1st child"
   FWPNBAGE11 = "EF-10 Age of oth child R cared for-2nd FW,1st child"
   FWPNBREL12 = "EF-4 Other nonbio child R cared for related by blood/marr-2nd FW,2nd child"
   FWPNBFOS12 = "EF-5 Other nonbio child R cared for foster child/placed by agency-2nd FW,2nd child"
   FWPNBSEX12 = "EF-6 Sex of other nonbio child R cared for-2nd FW,2nd child"
   FWPNBAD12 = "EF-7 R adopted/became legal guardian of other nonbio child R cared for-2nd FW,2nd child"
   FWPNBLIV89 = "EF-8 Where oth child R cared for lives-2nd FW,2nd child,1st ment"
   FWPNBFAR12 = "EF-9 How far away oth child R cared for lives-2nd FW,2nd child"
   FWPNBAGE12 = "EF-10 Age of oth child R cared for-2nd FW,2nd child"
   E_NBAKIDS3 = "# of other nonbiological children adopted w/3rd FW"
   FWPNBEVR3 = "EF-1 R & 3rd FW ever had any other nonbio children under care"
   FWPNBNUM3 = "EF-2 # other nonbio children under R & 3rd FW's care"
   FWPNBREL21 = "EF-4 Other nonbio child R cared for related by blood/marr-3rd FW,1st child"
   FWPNBFOS21 = "EF-5 Other nonbio child R cared for foster child/placed by agency-3rd FW,1st child"
   FWPNBSEX21 = "EF-6 Sex of other nonbio child R adopted-3rd FW,1st child"
   FWPNBAD21 = "EF-7 R adopted any of these other nonbio children"
   FWPNBLIV161 = "EF-8 Where oth child R adopted-3rd FW/1st child/1st men"
   FWPNBFAR21 = "EF-9 How far away lives oth child R adopted-3rd FW,1st child"
   FWPNBAGE21 = "EF-10 Current age of oth child R adopted-3rd FW,1st child"
   FWPNBREL22 = "EF-4 Other nonbio child R cared for related by blood/marr-3rd FW,2nd child"
   FWPNBFOS22 = "EF-5 Other nonbio child R cared for foster child/placed by agency-3rd FW,2nd child"
   FWPNBSEX22 = "EF-6 Sex of other nonbio child R adopted-3rd FW,2nd child"
   FWPNBAD22 = "EF-7 R adopted any of these other nonbio children"
   FWPNBLIV169 = "EF-8 Where oth child R adopted-3rd FW/2nd child/1st men"
   FWPNBFAR22 = "EF-9 How far away lives oth child R adopted-3rd FW,2nd child"
   FWPNBAGE22 = "EF-10 Current age of oth child R adopted-3rd FW,2nd child"
   E_NBAKIDS4 = "# of other nonbiological children adopted w/4th FW"
   FWPNBEVR4 = "EF-1 R & 4th FW ever had any other nonbio children under care"
   E_NBAKIDS11 = "# of other nonbio children adopted w/1st CP"
   FWPNBEVR11 = "EF-1 R & 1st CP ever had any other nonbio children under care"
   FWPNBNUM11 = "EF-2 # other nonbio children under R & 1st CP's care"
   FWPNBREL101 = "EF-4 Other nonbio child R cared for related by blood/marr-1st CP,1st child"
   FWPNBFOS101 = "EF-5 Other nonbio child R cared for foster child/placed by agency-1st CP,1st child"
   FWPNBSEX101 = "EF-6 Sex of other nonbio child R cared for-1st CP,1st child"
   FWPNBAD101 = "EF-7 R adopted/became legal guardian of other nonbio child R cared for-1st CP,1st child"
   FWPNBLIV801 = "EF-8 Where oth child R cared for lives now-1st CP,1st child,1st ment"
   FWPNBFAR101 = "EF-9 How far away oth child R cared for lives now-1st CP,1st child"
   FWPNBAGE101 = "EF-10 Age of oth child R cared for-1st CP,1st child"
   FWPNBREL102 = "EF-4 Other nonbio child R cared for related by blood/marr-1st CP,2nd child"
   FWPNBFOS102 = "EF-5 Other nonbio child R cared for foster child/placed by agency-1st CP,2nd child"
   FWPNBSEX102 = "EF-6 Sex of other nonbio child R cared for-1st CP,2nd child"
   FWPNBAD102 = "EF-7 R adopted/became legal guardian of other nonbio child R cared for-1st CP,2nd child"
   FWPNBLIV809 = "EF-8 Where oth child R cared for lives now-1st CP,2nd child,1st ment"
   FWPNBFAR102 = "EF-9 How far away oth child R cared for lives now-1st CP,2nd child"
   FWPNBAGE102 = "EF-10 Age of oth child R cared for-1st CP,2nd child"
   FWPNBREL103 = "EF-4 Other nonbio child R cared for related by blood/marr-1st CP,3rd child"
   FWPNBFOS103 = "EF-5 Other nonbio child R cared for foster child/placed by agency-1st CP,3rd child"
   FWPNBSEX103 = "EF-6 Sex of other nonbio child R cared for-1st CP,3rd child"
   FWPNBAD103 = "EF-7 R adopted/became legal guardian of other nonbio child R cared for-1st CP,3rd child"
   FWPNBLIV817 = "EF-8 Where oth child R cared for lives now-1st CP, 3rd child,1st ment"
   FWPNBFAR103 = "EF-9 How far away lives oth child R cared for lives now-1st CP,3rd child"
   FWPNBAGE103 = "EF-10 Current age of oth child R cared for-1st CP,3rd child"
   FWPNBREL104 = "EF-4 Other nonbio child R cared for related by blood/marr-1st CP,4th child"
   FWPNBFOS104 = "EF-5 Other nonbio child R cared for foster child/placed by agency-1st CP,4th child"
   FWPNBSEX104 = "EF-6 Sex of other nonbio child R cared for-1st CP,4th child"
   FWPNBAD104 = "EF-7 R adopted/became legal guardian of other nonbio child R cared for-1st CP,4th child"
   FWPNBLIV825 = "EF-8 Where oth child R cared for lives now-1st CP, 4th child,1st ment"
   FWPNBFAR104 = "EF-9 How far away lives oth child R cared for lives now-1st CP,4th child"
   FWPNBAGE104 = "EF-10 Current age of oth child R cared for-1st CP,4th child"
   OTBCHIL = "FA-1 R had other biological children w/nonmarital partners"
   OTBPROBE = "FA-2 Could R have fathered a child he didn't know about"
   OTBCHILN = "FA-3 # of other biological children R has w/nonmarital partners"
   OTBSAME = "FA-5 Do these children have same biological mother"
   OBCSEXX = "FA-8 Sex of bio child w/nonmarital partner-1st child"
   OBCDOB_Y = "FA-9y Year of birth of bio child-1st child"
   OBCMAGEX = "FA-11 Age of mother when child born-1st child"
   OBCMLIV = "FA-12 R living with mother when child was born-1st child"
   OBCKNOWX = "FA-13 When R found out nonmar partner was pregnant-1st child"
   OBCLIVEX01 = "FA-14 Where bio child usually lives now-1st child, 1st mention"
   OBCLIVEX02 = "FA-14 Where bio child usually lives now-1st child, 2nd mention"
   OBCLIVEX03 = "FA-14 Where bio child usually lives now-1st child, 3rd mention"
   OBCAGE = "FA-15 Current age of bio child w/nonmar partner-1st child"
   OBCCHSIG = "FA-16a Established legal paternity by signing application for birth certificate-1st child"
   OBCCHCRT = "FA-16b Established legal paternity by going to court-1st child"
   OBCCHGEN = "FA-17 Established legal paternity by blood or other genetic test-1st child"
   OBCEVER = "FA-18 Did R ever live with 1st bio child w/nonmarital partner"
   OBCFAR = "FA-19 How far away 1st bio child w/nonmar partner lives now"
   OBCRWANX = "FA-20 R wanted child before mother got preg-1st child"
   OBCSOONX = "FA-21 Pregnancy timing of 1st child w/nonmarital partner"
   OBCSOONN = "FA-21a How much sooner 1st child w/nonmarital partner - Unit"
   OBCSOONMY = "FA-21b How much sooner 1st child w/nonmarital partner - Month/Year"
   OBCHPYX = "FA-22 R's happiness about pregnancy w/1st child w/nonmar partner"
   OBCSEXX2 = "FA-8 Sex of bio child w/nonmarital partner-2nd child"
   OBCDOB_Y2 = "FA-9y Year of birth of bio child-2nd child"
   MULTBIRT152 = "FA-10 Verifying if multiple birth - 2nd bio child"
   OBCMAGEX2 = "FA-11 Age of mother when child born-2nd child"
   OBCMLIV2 = "FA-12 R living with mother when child was born-2nd child"
   OBCKNOWX2 = "FA-13 When R found out nonmar partner was pregnant-2nd child"
   OBCLIVEX11 = "FA-14 Where bio child usually lives now-2nd child, 1st mention"
   OBCLIVEX12 = "FA-14 Where bio child usually lives now-2nd child, 2nd mention"
   OBCLIVEX13 = "FA-14 Where bio child usually lives now-2nd child, 3rd mention"
   OBCAGE2 = "FA-15 Current age of bio child w/nonmar partner-2nd child"
   OBCCHSIG2 = "FA-16a Established legal paternity by signing application for birth certificate-2nd child"
   OBCCHCRT2 = "FA-16b Established legal paternity by going to court-2nd child"
   OBCCHGEN2 = "FA-17 Established legal paternity by blood or other genetic test-2nd child"
   OBCEVER2 = "FA-18 Did R ever live with 2nd bio child w/nonmarital partner"
   OBCFAR2 = "FA-19 How far away 2nd bio child w/nonmar partner lives now"
   OBCRWANX2 = "FA-20 R wanted child before mother got preg-2nd child"
   OBCSOONX2 = "FA-21 Pregnancy timing of 2nd child w/nonmarital partner"
   OBCSOONN2 = "FA-21a How much sooner 2nd child w/nonmarital partner - Unit"
   OBCSOONMY2 = "FA-21b How much sooner 2nd child w/nonmarital partner - Month/Year"
   OBCHPYX2 = "FA-22 R's happiness about pregnancy w/2nd child w/nonmar partner"
   OBCSEXX3 = "FA-8 Sex of bio child w/nonmarital partner-3rd child"
   OBCDOB_Y3 = "FA-9y Year of birth of bio child-3rd child"
   MULTBIRT153 = "FA-10 Verifying if multiple birth - 3rd bio child"
   OBCMAGEX3 = "FA-11 Age of mother when child born-3rd child"
   OBCMLIV3 = "FA-12 R living with mother when child was born-3rd child"
   OBCKNOWX3 = "FA-13 When R found out nonmar partner was pregnant-3rd child"
   OBCLIVEX21 = "FA-14 Where bio child usually lives now-3rd child, 1st mention"
   OBCLIVEX22 = "FA-14 Where bio child usually lives now-3rd child, 2nd mention"
   OBCLIVEX23 = "FA-14 Where bio child usually lives now-3rd child, 3rd mention"
   OBCAGE3 = "FA-15 Current age of bio child w/nonmar partner-3rd child"
   OBCCHSIG3 = "FA-16a Established legal paternity by signing application for birth certificate-3rd child"
   OBCCHCRT3 = "FA-16b Established legal paternity by going to court-3rd child"
   OBCCHGEN3 = "FA-17 Established legal paternity by blood or other genetic test-3rd child"
   OBCEVER3 = "FA-18 Did R ever live with 3rd bio child w/nonmarital partner"
   OBCFAR3 = "FA-19 How far away 3rd bio child w/nonmar partner lives now"
   OBCRWANX3 = "FA-20 R wanted child before mother got preg-3rd child"
   OBCSOONX3 = "FA-21 Pregnancy timing of 3rd child w/nonmarital partner"
   OBCSOONN3 = "FA-21a How much sooner 3rd child w/nonmarital partner - Unit"
   OBCSOONMY3 = "FA-21b How much sooner 3rd child w/nonmarital partner - Month/Year"
   OBCHPYX3 = "FA-22 R's happiness about pregnancy w/3rd child w/nonmar partner"
   OBCSEXX4 = "FA-8 Sex of bio child w/nonmarital partner-4th child"
   OBCDOB_Y4 = "FA-9y Year of birth of bio child-4th child"
   MULTBIRT154 = "FA-10 Verifying if multiple birth - 4th bio child"
   OBCMAGEX4 = "FA-11 Age of mother when child born-4th child"
   OBCMLIV4 = "FA-12 R living with mother when child was born-4th child"
   OBCKNOWX4 = "FA-13 When R found out nonmar partner was pregnant-4th child"
   OBCLIVEX31 = "FA-14 Where bio child usually lives now-4th child, 1st mention"
   OBCLIVEX32 = "FA-14 Where bio child usually lives now-4th child, 2nd mention"
   OBCLIVEX33 = "FA-14 Where bio child usually lives now-4th child, 3rd mention"
   OBCAGE4 = "FA-15 Current age of bio child w/nonmar partner-4th child"
   OBCCHSIG4 = "FA-16a Established legal paternity by signing application for birth certificate-4th child"
   OBCCHCRT4 = "FA-16b Established legal paternity by going to court-4th child"
   OBCCHGEN4 = "FA-17 Established legal paternity by blood or other genetic test-4th child"
   OBCEVER4 = "FA-18 Did R ever live with 4th bio child w/nonmarital partner"
   OBCFAR4 = "FA-19 How far away 4th bio child w/nonmar partner lives now"
   OBCRWANX4 = "FA-20 R wanted child before mother got preg-4th child"
   OBCSOONX4 = "FA-21 Pregnancy timing of 4th child w/nonmarital partner"
   OBCSOONN4 = "FA-21a How much sooner 4th child w/nonmarital partner - Unit"
   OBCSOONMY4 = "FA-21b How much sooner 4th child w/nonmarital partner - Month/Year"
   OBCHPYX4 = "FA-22 R's happiness about pregnancy w/4th child w/nonmar partner"
   OBCSEXX5 = "FA-8 Sex of bio child w/nonmarital partner-5th child"
   OBCDOB_Y5 = "FA-9y Year of birth of bio child-5th child"
   MULTBIRT155 = "FA-10 Verifying if multiple birth - 5th bio child"
   OBCMAGEX5 = "FA-11 Age of mother when child born-5th child"
   OBCMLIV5 = "FA-12 R living with mother when child was born-5th child"
   OBCKNOWX5 = "FA-13 When R found out nonmar partner was pregnant-5th child"
   OBCLIVEX41 = "FA-14 Where bio child usually lives now-5th child, 1st mention"
   OBCLIVEX42 = "FA-14 Where bio child usually lives now-5th child, 2nd mention"
   OBCLIVEX43 = "FA-14 Where bio child usually lives now-5th child, 3rd mention"
   OBCAGE5 = "FA-15 Current age of bio child w/nonmar partner-5th child"
   OBCCHSIG5 = "FA-16a Established legal paternity by signing application for birth certificate-5th child"
   OBCCHCRT5 = "FA-16b Established legal paternity by going to court-5th child"
   OBCCHGEN5 = "FA-17 Established legal paternity by blood or other genetic test-5th child"
   OBCEVER5 = "FA-18 Did R ever live with 5th bio child w/nonmarital partner"
   OBCFAR5 = "FA-19 How far away 5th bio child w/nonmar partner lives now"
   OBCRWANX5 = "FA-20 R wanted child before mother got preg-5th child"
   OBCSOONX5 = "FA-21 Pregnancy timing of 5th child w/nonmarital partner"
   OBCSOONN5 = "FA-21a How much sooner 5th child w/nonmarital partner - Unit"
   OBCSOONMY5 = "FA-21b How much sooner 5th child w/nonmarital partner - Month/Year"
   OBCHPYX5 = "FA-22 R's happiness about pregnancy w/5th child w/nonmar partner"
   OBCSEXX6 = "FA-8 Sex of bio child w/nonmarital partner-6th child"
   OBCDOB_Y6 = "FA-9y Year of birth of bio child-6th child"
   MULTBIRT156 = "FA-10 Verifying if multiple birth - 6th bio child"
   OBCMAGEX6 = "FA-11 Age of mother when child born-6th child"
   OBCMLIV6 = "FA-12 R living with mother when child was born-6th child"
   OBCKNOWX6 = "FA-13 When R found out nonmar partner was pregnant-6th child"
   OBCLIVEX51 = "FA-14 Where bio child usually lives now-6th child, 1st mention"
   OBCLIVEX52 = "FA-14 Where bio child usually lives now-6th child, 2nd mention"
   OBCLIVEX53 = "FA-14 Where bio child usually lives now-6th child, 3rd mention"
   OBCAGE6 = "FA-15 Current age of bio child w/nonmar partner-6th child"
   OBCCHSIG6 = "FA-16a Established legal paternity by signing application for birth certificate-6th child"
   OBCCHCRT6 = "FA-16b Established legal paternity by going to court-6th child"
   OBCCHGEN6 = "FA-17 Established legal paternity by blood or other genetic test-6th child"
   OBCEVER6 = "FA-18 Did R ever live with 6th bio child w/nonmarital partner"
   OBCFAR6 = "FA-19 How far away 6th bio child w/nonmar partner lives now"
   OBCRWANX6 = "FA-20 R wanted child before mother got preg-6th child"
   OBCSOONX6 = "FA-21 Pregnancy timing of 6th child w/nonmarital partner"
   OBCSOONN6 = "FA-21a How much sooner 6th child w/nonmarital partner - Unit"
   OBCSOONMY6 = "FA-21b How much sooner 6th child w/nonmarital partner - Month/Year"
   OBCHPYX6 = "FA-22 R's happiness about pregnancy w/6th child w/nonmar partner"
   OBCSEXX7 = "FA-8 Sex of bio child w/nonmarital partner-7th child"
   OBCDOB_Y7 = "FA-9y Year of birth of bio child-7th child"
   MULTBIRT157 = "FA-10 Verifying if multiple birth - 7th bio child"
   OBCMAGEX7 = "FA-11 Age of mother when child born-7th child"
   OBCMLIV7 = "FA-12 R living with mother when child was born-7th child"
   OBCKNOWX7 = "FA-13 When R found out nonmar partner was pregnant-7th child"
   OBCLIVEX61 = "FA-14 Where bio child usually lives now-7th child, 1st mention"
   OBCLIVEX62 = "FA-14 Where bio child usually lives now-7th child, 2nd mention"
   OBCLIVEX63 = "FA-14 Where bio child usually lives now-7th child, 3rd mention"
   OBCAGE7 = "FA-15 Current age of bio child w/nonmar partner-7th child"
   OBCCHSIG7 = "FA-16a Established legal paternity by signing application for birth certificate-7th child"
   OBCCHCRT7 = "FA-16b Established legal paternity by going to court-7th child"
   OBCCHGEN7 = "FA-17 Established legal paternity by blood or other genetic test-7th child"
   OBCEVER7 = "FA-18 Did R ever live with 7th bio child w/nonmarital partner"
   OBCFAR7 = "FA-19 How far away 7th bio child w/nonmar partner lives now"
   OBCRWANX7 = "FA-20 R wanted child before mother got preg- 7th child"
   OBCSOONX7 = "FA-21 Pregnancy timing of 7th child w/nonmarital partner"
   OBCSOONN7 = "FA-21a How much sooner 7th child w/nonmarital partner - Unit"
   OBCSOONMY7 = "FA-21b How much sooner 7th child w/nonmarital partner - Month/Year"
   OBCHPYX7 = "FA-22 R's happiness about pregnancy w/ 7th child w/nonmar partner"
   OBCSEXX8 = "FA-8 Sex of bio child w/nonmarital partner-8th child"
   OBCDOB_Y8 = "FA-9y Year of birth of bio child-8th child"
   MULTBIRT158 = "FA-10 Verifying if multiple birth - 8th bio child"
   OBCMAGEX8 = "FA-11 Age of mother when child born-8th child"
   OBCMLIV8 = "FA-12 R living with mother when child was born-8th child"
   OBCKNOWX8 = "FA-13 When R found out nonmar partner was pregnant-8th child"
   OBCLIVEX71 = "FA-14 Where bio child usually lives now-8th child, 1st mention"
   OBCLIVEX72 = "FA-14 Where bio child usually lives now-8th child, 2nd mention"
   OBCLIVEX73 = "FA-14 Where bio child usually lives now-8th child, 3rd mention"
   OBCAGE8 = "FA-15 Current age of bio child w/nonmar partner-8th child"
   OBCCHSIG8 = "FA-16a Established legal paternity by signing application for birth certificate-8th child"
   OBCCHCRT8 = "FA-16b Established legal paternity by going to court-8th child"
   OBCCHGEN8 = "FA-17 Established legal paternity by blood or other genetic test-8th child"
   OBCEVER8 = "FA-18 Did R ever live with 8th bio child w/nonmarital partner"
   OBCFAR8 = "FA-19 How far away 8th bio child w/nonmar partner lives now"
   OBCRWANX8 = "FA-20 R wanted child before mother got preg-8th child"
   OBCSOONX8 = "FA-21 Pregnancy timing of 8th child w/nonmarital partner"
   OBCSOONN8 = "FA-21a How much sooner 8th child w/nonmarital partner - Unit"
   OBCSOONMY8 = "FA-21b How much sooner 8th child w/nonmarital partner - Month/Year"
   OBCHPYX8 = "FA-22 R's happiness about pregnancy w/8th child w/nonmar partner"
   OBCSEXX9 = "FA-8 Sex of bio child w/nonmarital partner-9th child"
   OBCDOB_Y9 = "FA-9y Year of birth of bio child-9th child"
   MULTBIRT159 = "FA-10 Verifying if multiple birth - 9th bio child"
   OBCMAGEX9 = "FA-11 Age of mother when child born-9th child"
   OBCMLIV9 = "FA-12 R living with mother when child was born-9th child"
   OBCKNOWX9 = "FA-13 When R found out nonmar partner was pregnant-9th child"
   OBCLIVEX81 = "FA-14 Where bio child usually lives now-9th child, 1st mention"
   OBCLIVEX82 = "FA-14 Where bio child usually lives now-9th child, 2nd mention"
   OBCLIVEX83 = "FA-14 Where bio child usually lives now-9th child, 3rd mention"
   OBCAGE9 = "FA-15 Current age of bio child w/nonmar partner-9th child"
   OBCCHSIG9 = "FA-16a Established legal paternity by signing application for birth certificate -9th child"
   OBCCHCRT9 = "FA-16b Established legal paternity by going to court -9th child"
   OBCCHGEN9 = "FA-17 Established legal paternity by blood or other genetic test -9th child"
   OBCEVER9 = "FA-18 Did R ever live with 9th bio child w/nonmarital partner"
   OBCFAR9 = "FA-19 How far away 9th bio child w/nonmar partner lives now"
   OBCRWANX9 = "FA-20 R wanted child before mother got preg-9th child"
   OBCSOONX9 = "FA-21 Pregnancy timing of 9th child w/nonmarital partner"
   OBCSOONN9 = "FA-21a How much sooner 9th child w/nonmarital partner - Unit"
   OBCSOONMY9 = "FA-21b How much sooner 9th child w/nonmarital partner - Month/Year"
   OBCHPYX9 = "FA-22 R's happiness about pregnancy w/9th child w/nonmar partner"
   OBCSEXX10 = "FA-8 Sex of bio child w/nonmarital partner-10th child"
   OBCDOB_Y10 = "FA-9y Year of birth of bio child-10th child"
   MULTBIRT160 = "FA-10 Verifying if multiple birth - 10th bio child"
   OBCMAGEX10 = "FA-11 Age of mother when child born-10th child"
   OBCMLIV10 = "FA-12 R living with mother when child was born-10th child"
   OBCKNOWX10 = "FA-13 When R found out nonmar partner was pregnant-10th child"
   OBCLIVEX91 = "FA-14 Where bio child usually lives now-10th child, 1st mention"
   OBCLIVEX92 = "FA-14 Where bio child usually lives now-10th child, 2nd mention"
   OBCLIVEX93 = "FA-14 Where bio child usually lives now-10th child, 3rd mention"
   OBCAGE10 = "FA-15 Current age of bio child w/nonmar partner-10th child"
   OBCCHSIG10 = "FA-16a Established legal paternity by signing application for birth certificate (10th child)"
   OBCCHCRT10 = "FA-16b Established legal paternity by going to court (10th child)"
   OBCCHGEN10 = "FA-17 Established legal paternity by blood or other genetic test (10th child)"
   OBCEVER10 = "FA-18 Did R ever live with 10th bio child w/nonmarital partner"
   OBCFAR10 = "FA-19 How far away 10th bio child w/nonmar partner lives now"
   OBCRWANX10 = "FA-20 R wanted child before mother got preg-10th child"
   OBCSOONX10 = "FA-21 Pregnancy timing of 10th child w/nonmarital partner"
   OBCSOONN10 = "FA-21a How much sooner 10th child w/nonmarital partner - Unit"
   OBCSOONMY10 = "FA-21b How much sooner 10th child w/nonmarital partner - Month/Year"
   OBCHPYX10 = "FA-22 R's happiness about pregnancy w/10th child w/nonmar partner"
   F_AKIDS = "Number of other children adopted (from section F)"
   OTACHIL = "FB-1 Other non-biological children under R's care"
   OTACHILN = "FB-2 # of other non-biological children under R's care"
   OTNBREL = "FB-4 Other nonbio child R cared for related by blood/marr-1st child"
   OTNBFOS = "FB-5 Other nonbio child R cared for foster child/placed by agency-1st child"
   OTNBSEX = "FB-6 Sex of other nonbio child R cared for-1st child"
   OTNBAD = "FB-7 R adopted/became legal guardian of other nonbio child R cared for-1st child"
   OTNBLIV1 = "FB-8 Where other child R cared for lives-1st child,1st ment"
   OTNBLIV2 = "FB-8 Where other child R cared for lives-1st child,2nd ment"
   OTNBFAR = "FB-9 How far away other child cared for by R lives now-1st child"
   OTNBAGE = "FB-10 Age of other child cared for by R-1st child"
   OTNBREL2 = "FB-4 Other nonbio child R cared for related by blood/marr-2nd child"
   OTNBFOS2 = "FB-5 Other nonbio child R cared for foster child/placed by agency-2nd child"
   OTNBSEX2 = "FB-6 Sex of other nonbio child R cared for-2nd child"
   OTNBAD2 = "FB-7 R adopted/became legal guardian of other nonbio child R cared for-2nd child"
   OTNBLIV9 = "FB-8 Where other child R cared for lives-2nd child,1st ment"
   OTNBLIV10 = "FB-8 Where other child R cared for lives-2nd child,2nd ment"
   OTNBFAR2 = "FB-9 How far away other child cared for by R lives now-2nd child"
   OTNBAGE2 = "FB-10 Age of other child cared for by R-2nd child"
   OTNBREL3 = "FB-4 Other nonbio child R cared for related by blood/marr-3rd child"
   OTNBFOS3 = "FB-5 Other nonbio child R cared for foster child/placed by agency-3rd child"
   OTNBSEX3 = "FB-6 Sex of other nonbio child R cared for-3rd child"
   OTNBAD3 = "FB-7 R adopted/became legal guardian of other nonbio child R cared for-3rd child"
   OTNBLIV17 = "FB-8 Where other child R cared for lives-3rd child,1st ment"
   OTNBLIV18 = "FB-8 Where other child R cared for lives-3rd child,2nd ment"
   OTNBFAR3 = "FB-9 How far away other child cared for by R lives now-3rd child"
   OTNBAGE3 = "FB-10 Age of other child cared for by R-3rd child"
   OTNBREL4 = "FB-4 Other nonbio child R cared for related by blood/marr-4th child"
   OTNBFOS4 = "FB-5 Other nonbio child R cared for foster child/placed by agency-4th child"
   OTNBSEX4 = "FB-6 Sex of other nonbio child R cared for-4th child"
   OTNBAD4 = "FB-7 R adopted/became legal guardian of other nonbio child R cared for-4th child"
   OTNBLIV25 = "FB-8 Where other child R cared for lives-4th child,1st ment"
   OTNBLIV26 = "FB-8 Where other child R cared for lives-4th child,2nd ment"
   OTNBFAR4 = "FB-9 How far away other child cared for by R lives now-4th child"
   OTNBAGE4 = "FB-10 Age of other child cared for by R-4th child"
   OTNBREL5 = "FB-4 Other nonbio child R cared for related by blood/marr-5th child"
   OTNBFOS5 = "FB-5 Other nonbio child R cared for foster child/placed by agency-5th child"
   OTNBSEX5 = "FB-6 Sex of other nonbio child R cared for-5th child"
   OTNBAD5 = "FB-7 R adopted/became legal guardian of other nonbio child R cared for-5th child"
   OTNBLIV33 = "FB-8 Where other child R cared for lives-5th child,1st ment"
   OTNBLIV34 = "FB-8 Where other child R cared for lives-5th child,2nd ment"
   OTNBFAR5 = "FB-9 How far away other child adopted by R lives now-5th child"
   OTNBAGE5 = "FB-10 Age of other child cared for by R-5th child"
   OTNBREL6 = "FB-4 Other nonbio child R cared for related by blood/marr-6th child"
   OTNBFOS6 = "FB-5 Other nonbio child R cared for foster child/placed by agency-6th child"
   OTNBSEX6 = "FB-6 Sex of other nonbio child R cared for-6th child"
   OTNBAD6 = "FB-7 R adopted/became legal guardian of other nonbio child R cared for-6th child"
   OTNBLIV41 = "FB-8 Where other child R cared for lives-6th child,1st ment"
   OTNBLIV42 = "FB-8 Where other child R cared for lives-6th child,2nd ment"
   OTNBFAR6 = "FB-9 How far away other child cared for by R lives now-6th child"
   OTNBAGE6 = "FB-10 Age of other child cared for by R-6th child"
   OTNBREL7 = "FB-4 Other nonbio child R cared for related by blood/marr-7th child"
   OTNBFOS7 = "FB-5 Other nonbio child R cared for foster child/placed by agency-7th child"
   OTNBSEX7 = "FB-6 Sex of other nonbio child R cared for-7th child"
   OTNBAD7 = "FB-7 R adopted/became legal guardian of other nonbio child R cared for-7th child"
   OTNBLIV49 = "FB-8 Where other child R cared for lives-7th child,1st ment"
   OTNBLIV50 = "FB-8 Where other child R cared for lives-7th child,2nd ment"
   OTNBFAR7 = "FB-9 How far away other child cared for by R lives now-7th child"
   OTNBAGE7 = "FB-10 Age of other child cared for by R-7th child"
   OTNBREL8 = "FB-4 Other nonbio child R cared for related by blood/marr-8th child"
   OTNBFOS8 = "FB-5 Other nonbio child R cared for foster child/placed by agency-8th child"
   OTNBSEX8 = "FB-6 Sex of other nonbio child R cared for-8th child"
   OTNBAD8 = "FB-7 R adopted/became legal guardian of other nonbio child R cared for-8th child"
   OTNBLIV57 = "FB-8 Where other child R cared for lives-8th child,1st ment"
   OTNBLIV58 = "FB-8 Where other child R cared for lives-8th child,2nd ment"
   OTNBFAR8 = "FB-9 How far away other child cared for by R lives now-8th child"
   OTNBAGE8 = "FB-10 Age of other child cared for by R-8th"
   OTPREG = "FC-1 Ever had pregnancy ending in miscarriage/stillbirth/abortion"
   OTPRGPRB = "FC-2 Could R have had a pregnancy like this but not know"
   OTPRGN = "FC-3 Number of Pregnancies Ending in Miscarriage/Stillbirth/Abortion"
   OTPRGEND = "FC-4 How Did Pregnancy End (if 1 preg reported)"
   OTMSN = "FC-5 Number of Pregnancies Ending in Miscarriage"
   OTSTN = "FC-6 Number of Pregnancies Ending in Stillbirth"
   OTABN = "FC-7 Number of Pregnancies Ending in Abortion"
   TOTPRG = "FC-8 Total Number of Times R Made Someone Pregnant"
   OTPREGS = "Number of Pregnancies Ending in Miscarriage/Stillbirth/Abortion"
   TOTPREGS_C = "Total Number of Pregnancies Collected Throughout Interview"
   TOTPREGS_R = "Total Number of Times R Reports He Made Someone Pregnant"
   BIOKIDS = "Number of Biological Children R Has Ever Fathered"
   ADOPKIDS = "Number of Children R Has Ever Adopted"
   ANYKIDS = "Whether R Ever Had Biological or Adopted Children"
   BIOADOPT = "Total # of biological and adopted children reported by R"
   PREGSNOW = "# of women currently pregnant with R's child"
   NUMLIFE = "FE-1 # Female Sexual Partners in Lifetime (if 7+ reported)"
   BIODOB1 = "Year when R's 1st biological child was born (in chronol order)"
   BIODOB2 = "Year when R's 2nd biological child was born (in chronol order)"
   BIODOB3 = "Year when R's 3rd biological child was born (in chronol order)"
   BIODOB4 = "Year when R's 4th biological child was born (in chronol order)"
   BIODOB5 = "Year when R's 5th biological child was born (in chronol order)"
   BIODOB6 = "Year when R's 6th biological child was born (in chronol order)"
   BIODOB7 = "Year when R's 7th biological child was born (in chronol order)"
   BIODOB8 = "Year when R's 8th biological child was born (in chronol order)"
   BIODOB9 = "Year when R's 9th biological child was born (in chronol order)"
   BIODOB10 = "Year when R's 10th biological child was born (in chronol order)"
   BIOSEX1 = "Sex of R's 1st biological child"
   BIOSEX2 = "Sex of R's 2nd biological child"
   BIOSEX3 = "Sex of R's 3rd biological child"
   BIOSEX4 = "Sex of R's 4th biological child"
   BIOSEX5 = "Sex of R's 5th biological child"
   BIOSEX6 = "Sex of R's 6th biological child"
   BIOSEX7 = "Sex of R's 7th biological child"
   BIOSEX8 = "Sex of R's 8th biological child"
   BIOSEX9 = "Sex of R's 9th biological child"
   BIOSEX10 = "Sex of R's 10th biological child"
   BIOAGE1 = "Age of R's 1st biological child"
   BIOAGE2 = "Age of R's 2nd biological child"
   BIOAGE3 = "Age of R's 3rd biological child"
   BIOAGE4 = "Age of R's 4th biological child"
   BIOAGE5 = "Age of R's 5th biological child"
   BIOAGE6 = "Age of R's 6th biological child"
   BIOAGE7 = "Age of R's 7th biological child"
   BIOAGE8 = "Age of R's 8th biological child"
   BIOAGE9 = "Age of R's 9th biological child"
   BIOAGE10 = "Age of R's 10th biological child"
   BIOAGEGP1 = "Age group of R's 1st biological child"
   BIOAGEGP2 = "Age group of R's 2nd biological child"
   BIOAGEGP3 = "Age group of R's 3rd biological child"
   BIOAGEGP4 = "Age group of R's 4th biological child"
   BIOAGEGP5 = "Age group of R's 5th biological child"
   BIOAGEGP6 = "Age group of R's 6th biological child"
   BIOAGEGP7 = "Age group of R's 7th biological child"
   BIOAGEGP8 = "Age group of R's 8th biological child"
   BIOAGEGP9 = "Age group of R's 9th biological child"
   BIOAGEGP10 = "Age group of R's 10th biological child"
   BIOHH1 = "Is child in R's household - R's 1st biological child"
   BIOHH2 = "Is child in R's household - R's 2nd biological child"
   BIOHH3 = "Is child in R's household - R's 3rd biological child"
   BIOHH4 = "Is child in R's household - R's 4th biological child"
   BIOHH5 = "Is child in R's household - R's 5th biological child"
   BIOHH6 = "Is child in R's household - R's 6th biological child"
   BIOHH7 = "Is child in R's household - R's 7th biological child"
   BIOHH8 = "Is child in R's household - R's 8th biological child"
   BIOHH9 = "Is child in R's household - R's 9th biological child"
   BIOHH10 = "Is child in R's household - R's 10th biological child"
   BIOMOM1 = "Relationship of child's biological mother to R - R's 1st biological child"
   BIOMOM2 = "Relationship of child's biological mother to R - R's 2nd biological child"
   BIOMOM3 = "Relationship of child's biological mother to R - R's 3rd biological child"
   BIOMOM4 = "Relationship of child's biological mother to R - R's 4th biological child"
   BIOMOM5 = "Relationship of child's biological mother to R - R's 5th biological child"
   BIOMOM6 = "Relationship of child's biological mother to R - R's 6th biological child"
   BIOMOM7 = "Relationship of child's biological mother to R - R's 7th biological child"
   BIOMOM8 = "Relationship of child's biological mother to R - R's 8th biological child"
   BIOMOM9 = "Relationship of child's biological mother to R - R's 9th biological child"
   BIOMOM10 = "Relationship of child's biological mother to R - R's 10th biological child"
   BIOMAR1 = "Was R married to child's mother at time of child's birth - R's 1st biological child"
   BIOMAR2 = "Was R married to child's mother at time of child's birth - R's 2nd biological child"
   BIOMAR3 = "Was R married to child's mother at time of child's birth - R's 3rd biological child"
   BIOMAR4 = "Was R married to child's mother at time of child's birth - R's 4th biological child"
   BIOMAR5 = "Was R married to child's mother at time of child's birth - R's 5th biological child"
   BIOMAR6 = "Was R married to child's mother at time of child's birth - R's 6th biological child"
   BIOMAR7 = "Was R married to child's mother at time of child's birth - R's 7th biological child"
   BIOMAR8 = "Was R married to child's mother at time of child's birth - R's 8th biological child"
   BIOMAR9 = "Was R married to child's mother at time of child's birth - R's 9th biological child"
   BIOMAR10 = "Was R married to child's mother at time of child's birth - R's 10th biological child"
   BIOCOHB1 = "Was R married to or living with child's mother at time of child's birth - R's 1st biological child"
   BIOCOHB2 = "Was R married to or living with child's mother at time of child's birth - R's 2nd biological child"
   BIOCOHB3 = "Was R married to or living with child's mother at time of child's birth - R's 3rd biological child"
   BIOCOHB4 = "Was R married to or living with child's mother at time of child's birth - R's 4th biological child"
   BIOCOHB5 = "Was R married to or living with child's mother at time of child's birth - R's 5th biological child"
   BIOCOHB6 = "Was R married to or living with child's mother at time of child's birth - R's 6th biological child"
   BIOCOHB7 = "Was R married to or living with child's mother at time of child's birth - R's 7th biological child"
   BIOCOHB8 = "Was R married to or living with child's mother at time of child's birth - R's 8th biological child"
   BIOCOHB9 = "Was R married to or living with child's mother at time of child's birth - R's 9th biological child"
   BIOCOHB10 = "Was R married to or living with child's mother at time of child's birth - R's 10th biological child"
   BIOLRNPG1 = "When did R learn about the pregnancy - R's 1st biological child"
   BIOLRNPG2 = "When did R learn about the pregnancy - R's 2nd biological child"
   BIOLRNPG3 = "When did R learn about the pregnancy - R's 3rd biological child"
   BIOLRNPG4 = "When did R learn about the pregnancy - R's 4th biological child"
   BIOLRNPG5 = "When did R learn about the pregnancy - R's 5th biological child"
   BIOLRNPG6 = "When did R learn about the pregnancy - R's 6th biological child"
   BIOLRNPG7 = "When did R learn about the pregnancy - R's 7th biological child"
   BIOLRNPG8 = "When did R learn about the pregnancy - R's 8th biological child"
   BIOLRNPG9 = "When did R learn about the pregnancy - R's 9th biological child"
   BIOLRNPG10 = "When did R learn about the pregnancy - R's 10th biological child"
   BIOLIVNG11 = "Where is child living now - R's 1st biological child, 1st mention"
   BIOLIVNG12 = "Where is child living now - R's 1st biological child, 2nd mention"
   BIOLIVNG13 = "Where is child living now - R's 1st biological child, 3rd mention"
   BIOLIVNG21 = "Where is child living now - R's 2nd biological child, 1st mention"
   BIOLIVNG22 = "Where is child living now - R's 2nd biological child, 2nd mention"
   BIOLIVNG23 = "Where is child living now - R's 2nd biological child, 3rd mention"
   BIOLIVNG31 = "Where is child living now - R's 3rd biological child, 1st mention"
   BIOLIVNG32 = "Where is child living now - R's 3rd biological child, 2nd mention"
   BIOLIVNG33 = "Where is child living now - R's 3rd biological child, 3rd mention"
   BIOLIVNG41 = "Where is child living now - R's 4th biological child, 1st mention"
   BIOLIVNG42 = "Where is child living now - R's 4th biological child, 2nd mention"
   BIOLIVNG43 = "Where is child living now - R's 4th biological child, 3rd mention"
   BIOLIVNG51 = "Where is child living now - R's 5th biological child, 1st mention"
   BIOLIVNG52 = "Where is child living now - R's 5th biological child, 2nd mention"
   BIOLIVNG53 = "Where is child living now - R's 5th biological child, 3rd mention"
   BIOLIVNG61 = "Where is child living now - R's 6th biological child, 1st mention"
   BIOLIVNG62 = "Where is child living now - R's 6th biological child, 2nd mention"
   BIOLIVNG63 = "Where is child living now - R's 6th biological child, 3rd mention"
   BIOLIVNG71 = "Where is child living now - R's 7th biological child, 1st mention"
   BIOLIVNG72 = "Where is child living now - R's 7th biological child, 2nd mention"
   BIOLIVNG73 = "Where is child living now - R's 7th biological child, 3rd mention"
   BIOLIVNG81 = "Where is child living now - R's 8th biological child, 1st mention"
   BIOLIVNG82 = "Where is child living now - R's 8th biological child, 2nd mention"
   BIOLIVNG83 = "Where is child living now - R's 8th biological child, 3rd mention"
   BIOLIVNG91 = "Where is child living now - R's 9th biological child, 1st mention"
   BIOLIVNG92 = "Where is child living now - R's 9th biological child, 2nd mention"
   BIOLIVNG93 = "Where is child living now - R's 9th biological child, 3rd mention"
   BIOLIVNG101 = "Where is child living now - R's 10th biological child, 1st mention"
   BIOLIVNG102 = "Where is child living now - R's 10th biological child, 2nd mention"
   BIOLIVNG103 = "Where is child living now - R's 10th biological child, 3rd mention"
   BIOCHSIG1 = "Did R sign application for birth certificate or other paternity doc - R's 1st bio child"
   BIOCHSIG2 = "Did R sign application for birth certificate or other paternity doc - R's 2nd bio child"
   BIOCHSIG3 = "Did R sign application for birth certificate or other paternity doc - R's 3rd bio child"
   BIOCHSIG4 = "Did R sign application for birth certificate or other paternity doc - R's 4th bio child"
   BIOCHSIG5 = "Did R sign application for birth certificate or other paternity doc - R's 5th bio child"
   BIOCHSIG6 = "Did R sign application for birth certificate or other paternity doc - R's 6th bio child"
   BIOCHSIG7 = "Did R sign application for birth certificate or other paternity doc - R's 7th bio child"
   BIOCHSIG8 = "Did R sign application for birth certificate or other paternity doc - R's 8th bio child"
   BIOCHSIG9 = "Did R sign application for birth certificate or other paternity doc - R's 9th bio child"
   BIOCHSIG10 = "Did R sign application for birth certificate or other paternity doc - R's 10th bio child"
   BIOCHCRT1 = "Did R go to court to establish paternity for this child - R's 1st biological child"
   BIOCHCRT2 = "Did R go to court to establish paternity for this child - R's 2nd biological child"
   BIOCHCRT3 = "Did R go to court to establish paternity for this child - R's 3rd biological child"
   BIOCHCRT4 = "Did R go to court to establish paternity for this child - R's 4th biological child"
   BIOCHCRT5 = "Did R go to court to establish paternity for this child - R's 5th biological child"
   BIOCHCRT6 = "Did R go to court to establish paternity for this child - R's 6th biological child"
   BIOCHCRT7 = "Did R go to court to establish paternity for this child - R's 7th biological child"
   BIOCHCRT8 = "Did R go to court to establish paternity for this child - R's 8th biological child"
   BIOCHCRT9 = "Did R go to court to establish paternity for this child - R's 9th biological child"
   BIOCHCRT10 = "Did R go to court to establish paternity for this child - R's 10th biological child"
   BIOCHGEN1 = "Was R's paternity established by blood or genetic test for this child - R's 1st biological child"
   BIOCHGEN2 = "Was R's paternity established by blood or genetic test for this child - R's 2nd biological child"
   BIOCHGEN3 = "Was R's paternity established by blood or genetic test for this child - R's 3rd biological child"
   BIOCHGEN4 = "Was R's paternity established by blood or genetic test for this child - R's 4th biological child"
   BIOCHGEN5 = "Was R's paternity established by blood or genetic test for this child - R's 5th biological child"
   BIOCHGEN6 = "Was R's paternity established by blood or genetic test for this child - R's 6th biological child"
   BIOCHGEN7 = "Was R's paternity established by blood or genetic test for this child - R's 7th biological child"
   BIOCHGEN8 = "Was R's paternity established by blood or genetic test for this child - R's 8th biological child"
   BIOCHGEN9 = "Was R's paternity established by blood or genetic test for this child - R's 9th biological child"
   BIOCHGEN10 = "Was R's paternity established by blood or genetic test for this child - R's 10th biological child"
   BIOLVEVR1 = "Has R ever lived with this child (if not living w/child now) - R's 1st biological child"
   BIOLVEVR2 = "Has R ever lived with this child (if not living w/child now) - R's 2nd biological child"
   BIOLVEVR3 = "Has R ever lived with this child (if not living w/child now) - R's 3rd biological child"
   BIOLVEVR4 = "Has R ever lived with this child (if not living w/child now) - R's 4th biological child"
   BIOLVEVR5 = "Has R ever lived with this child (if not living w/child now) - R's 5th biological child"
   BIOLVEVR6 = "Has R ever lived with this child (if not living w/child now) - R's 6th biological child"
   BIOLVEVR7 = "Has R ever lived with this child (if not living w/child now) - R's 7th biological child"
   BIOLVEVR8 = "Has R ever lived with this child (if not living w/child now) - R's 8th biological child"
   BIOLVEVR9 = "Has R ever lived with this child (if not living w/child now) - R's 9th biological child"
   BIOLVEVR10 = "Has R ever lived with this child (if not living w/child now) - R's 10th biological child"
   BIOHWFAR1 = "How far away does child live (miles) - R's 1st biological child"
   BIOHWFAR2 = "How far away does child live (miles) - R's 2nd biological child"
   BIOHWFAR3 = "How far away does child live (miles) - R's 3rd biological child"
   BIOHWFAR4 = "How far away does child live (miles) - R's 4th biological child"
   BIOHWFAR5 = "How far away does child live (miles) - R's 5th biological child"
   BIOHWFAR6 = "How far away does child live (miles) - R's 6th biological child"
   BIOHWFAR7 = "How far away does child live (miles) - R's 7th biological child"
   BIOHWFAR8 = "How far away does child live (miles) - R's 8th biological child"
   BIOHWFAR9 = "How far away does child live (miles) - R's 9th biological child"
   BIOHWFAR10 = "How far away does child live (miles) - R's 10th biological child"
   BIOWANT1 = "Wantedness of pregnancy right before it began - R's 1st biological child"
   BIOWANT2 = "Wantedness of pregnancy right before it began - R's 2nd biological child"
   BIOWANT3 = "Wantedness of pregnancy right before it began - R's 3rd biological child"
   BIOWANT4 = "Wantedness of pregnancy right before it began - R's 4th biological child"
   BIOWANT5 = "Wantedness of pregnancy right before it began - R's 5th biological child"
   BIOWANT6 = "Wantedness of pregnancy right before it began - R's 6th biological child"
   BIOWANT7 = "Wantedness of pregnancy right before it began - R's 7th biological child"
   BIOWANT8 = "Wantedness of pregnancy right before it began - R's 8th biological child"
   BIOWANT9 = "Wantedness of pregnancy right before it began - R's 9th biological child"
   BIOWANT10 = "Wantedness of pregnancy right before it began - R's 10th biological child"
   BIOHSOON1 = "Pregnancy came too soon, right time, or later than R wanted - R's 1st biological child"
   BIOHSOON2 = "Pregnancy came too soon, right time, or later than R wanted - R's 2nd biological child"
   BIOHSOON3 = "Pregnancy came too soon, right time, or later than R wanted - R's 3rd biological child"
   BIOHSOON4 = "Pregnancy came too soon, right time, or later than R wanted - R's 4th biological child"
   BIOHSOON5 = "Pregnancy came too soon, right time, or later than R wanted - R's 5th biological child"
   BIOHSOON6 = "Pregnancy came too soon, right time, or later than R wanted - R's 6th biological child"
   BIOHSOON7 = "Pregnancy came too soon, right time, or later than R wanted - R's 7th biological child"
   BIOHSOON8 = "Pregnancy came too soon, right time, or later than R wanted - R's 8th biological child"
   BIOHSOON9 = "Pregnancy came too soon, right time, or later than R wanted - R's 9th biological child"
   BIOHSOON10 = "Pregnancy came too soon, right time, or later than R wanted - R's 10th biological child"
   BIOHOWSN1 = "How much too soon did this pregnancy occur - R's 1st biological child"
   BIOHOWSN2 = "How much too soon did this pregnancy occur - R's 2nd biological child"
   BIOHOWSN3 = "How much too soon did this pregnancy occur - R's 3rd biological child"
   BIOHOWSN4 = "How much too soon did this pregnancy occur - R's 4th biological child"
   BIOHOWSN5 = "How much too soon did this pregnancy occur - R's 5th biological child"
   BIOHOWSN6 = "How much too soon did this pregnancy occur - R's 6th biological child"
   BIOHOWSN7 = "How much too soon did this pregnancy occur - R's 7th biological child"
   BIOHOWSN8 = "How much too soon did this pregnancy occur - R's 8th biological child"
   BIOHOWSN9 = "How much too soon did this pregnancy occur - R's 9th biological child"
   BIOHOWSN10 = "How much too soon did this pregnancy occur - R's 10th biological child"
   BIOHPYPG1 = "Happiness about pregnancy when R learned about pregnancy - R's 1st biological child"
   BIOHPYPG2 = "Happiness about pregnancy when R learned about pregnancy - R's 2nd biological child"
   BIOHPYPG3 = "Happiness about pregnancy when R learned about pregnancy - R's 3rd biological child"
   BIOHPYPG4 = "Happiness about pregnancy when R learned about pregnancy - R's 4th biological child"
   BIOHPYPG5 = "Happiness about pregnancy when R learned about pregnancy - R's 5th biological child"
   BIOHPYPG6 = "Happiness about pregnancy when R learned about pregnancy - R's 6th biological child"
   BIOHPYPG7 = "Happiness about pregnancy when R learned about pregnancy - R's 7th biological child"
   BIOHPYPG8 = "Happiness about pregnancy when R learned about pregnancy - R's 8th biological child"
   BIOHPYPG9 = "Happiness about pregnancy when R learned about pregnancy - R's 9th biological child"
   BIOHPYPG10 = "Happiness about pregnancy when R learned about pregnancy - R's 10th biological child"
   CRALL = "# of eligible coresidential children 0-18 (FC G-0)"
   CRALLU5 = "# of eligible coresidential children < age 5 (FC G-0)"
   CRALL518 = "# of eligible coresidential children ages 5-18 (FC G-0)"
   CRMALU5 = "# of eligible male coresidential children < age 5 (FC G-0)"
   CRMAL518 = "# of eligible male coresidential children ages 5-18 (FC G-0)"
   CRFEMU5 = "# of eligible female coresidential children < age 5 (FC G-0)"
   CRFEM518 = "# of eligible female coresidential children ages 5-18(FC G-0)"
   NCALL = "# of noncoresidential children (FC G-0)"
   NCALLU5 = "# of noncoresidential children < age 5 (FC G-0)"
   NCALL518 = "# of noncoresidential children ages 5-18 (FC G-0)"
   NCMALU5 = "# of male noncoresidential children < age 5 (FC G-0)"
   NCMAL518 = "# of male noncoresidential children ages 5-18 (FC G-0)"
   NCFEMU5 = "# of female noncoresidential children < age 5 (FC G-0)"
   NCFEM518 = "# of female noncoresidential children ages 5-18 (FC G-0)"
   RFAGE = "Age of focal residential child"
   RFSEX = "Gender of focal residential child"
   ROUTG04 = "GA-1 How often R spent time with child on outing in last 4 weeks (CR 0-4)"
   RMEAL04 = "GA-2 How often R ate evening meals with child in last 4 weeks (CR 0-4)"
   RERRAND04 = "GA-3 How often R took child along on errands in last 4 weeks (CR 0-4)"
   RPLAY04 = "GA-4 How often R played/played games with child in last 4 weeks (CR 0-4)"
   RREAD04 = "GA-5 How often R read to him/her in last 4 weeks (CR 0-4)"
   RAFFECT04 = "GA-6 How often R showed physical affection to child in last 4 weeks (CR 0-4)"
   RPRAISE04 = "GA-7 How often R praised child in last 4 weeks (CR 0-4)"
   RFEED04 = "GA-8 How often R fed child in last 4 weeks (CR 0-4)"
   RBATH04 = "GA-9 How often R gave bath to child in last 4 weeks (CR 0-4)"
   RDIAPER04 = "GA-10 How often R diapered/helped use toilet in last 4 weeks (CR 0-4)"
   RBED04 = "GA-11 How often R put child to bed in last 4 weeks (CR 0-4)"
   RAPPT04 = "GA-12 How often R took child to/from appointments in last 4 weeks (CR 0-4)"
   RDISC04 = "GA-13 How often R disciplined child in last 4 weeks (CR 0-4)"
   ROUTG518 = "GA-14 How often R spent time with child on outing in last 4 weeks (CR 5-18)"
   RMEAL518 = "GA-15 how often R ate evening meals with child in last 4 weeks (CR 5-18)"
   RERRAND518 = "GA-16 How often R took child along on errands in last 4 weeks (CR 5-18)"
   RAFFECT518 = "GA-17 How often R showed child physical affection in last 4 weeks (CR 5-18)"
   RPRAISE518 = "GA-18 How often R praised child in last 4 weeks (CR 5-18)"
   RTAKE518 = "GA-19 How often R took child to/from activities in last 4 weeks (CR 5-18)"
   RAPPT518 = "GA-20 How often R took child to/from appointments in last 4 weeks (CR 5-18)"
   RHELP518 = "GA-21 How often R helped child with/checked homework in last 4 weeks (CR 5-18)"
   RDISC518 = "GA-22 How often R disciplined child in last 4 weeks (CR 5-18)"
   RCLFR518 = "GA-23 How much R knows about child's close friends (CR 5-18)"
   RDO518 = "GA-24 How much R knows about what child is doing when not home (CR 5-18)"
   NRFAGE = "Age of focal nonresidential child"
   NRFSEX = "Gender of focal nonresidential child"
   NRVISIT04 = "GB-1 How often visit child in last 4 weeks (NCR 0-4)"
   NRSATVIS04 = "GB-2 How satisfied with how often visit child in last 4 weeks (NCR 0-4)"
   NROUTG04 = "GB-3 How often R went on outing with child in last 4 weeks (NCR 0-4)"
   NRMEAL04 = "GB-4 How often R ate evening meals together with child in last 4 weeks (NCR 0-4)"
   NRERRAND04 = "GB-5 How often R took child along on errands in last 4 weeks (NCR 0-4)"
   NROVRNT04 = "GB-6 How often child stayed overnight with you in last 4 weeks (NCR 0-4)"
   NRPLAY04 = "GB-7 How often R played games with child  in last 4 weeks (NCR 0-4)"
   NRREAD04 = "GB-8 How often R read to him/her in last 4 weeks (NCR 0-4)"
   NRAFFECT04 = "GB-9 How often R showed child physical affection in last 4 weeks (NCR 0-4)"
   NRPRAISE04 = "GB-10 How often R praised child in last 4 weeks (NCR 0-4)"
   NRFEED04 = "GB-11 How often R fed child in last 4 weeks (NCR 0-4)"
   NRBATH04 = "GB-12 How often R gave bath to child in last 4 weeks (NCR 0-4)"
   NRDIAPER04 = "GB-13 How often R diaper/help use toilet in last 4 weeks (NCR 0-4)"
   NRBED04 = "GB-14 How often R put child to bed in last 4 weeks (NCR 0-4)"
   NRAPPT04 = "GB-15 How often R took child to/from appointments- last 4 weeks (NCR 0-4)"
   NRDISC04 = "GB-16 How often R disciplined child in last 4 weeks (NCR 0-4)"
   NRVISIT518 = "GB-17 How often visit child in last 4 weeks (NCR 5-18)"
   NRSATVIS518 = "GB-18 How satisfied with how often visit child in last 4 weeks (NCR 5-18)"
   NROUTG518 = "GB-19 How often R went on outing with child in last 4 weeks (NCR 5-18)"
   NRMEAL518 = "GB-20 How often R ate evening meals together with child in last 4 weeks (NCR 5-18)"
   NRERRAND518 = "GB-21 How often R took child along on errands in last 4 weeks (NCR 5-18)"
   NROVRNT518 = "GB-22 How often child stayed overnight with you in last 4 weeks (NCR 5-18)"
   NRAFFECT518 = "GB-23 How often R showed child physical affection in last 4 weeks (NCR 5-18)"
   NRPRAISE518 = "GB-24 How often R praised child in last 4 weeks (NCR 5-18)"
   NRTAKE518 = "GB-25 How often R took child to/from activities in last 4 weeks (NCR 5-18)"
   NRAPPT518 = "GB-26 How often R took child to/from appointments in last 4 weeks (NCR 5-18)"
   NRHELP518 = "GB-27 How often R helped child with homework in last 4 weeks (NCR 5-18)"
   NRDISC518 = "GB-28 How often R disciplined child in last 4 weeks (NCR 5-18)"
   NRCLFR518 = "GB-29 How much know child's close friends in last 4 weeks (NCR 5-18)"
   NRDO518 = "GB-30 How much know about what doing when not home in last 4 weeks (CR 5-18)"
   NRMONEY = "GC-1 Contributed money or child support in last 12 months (NCR)"
   NREG = "GC-2 Contributed on regular basis (NCR)"
   NRAGREE = "GC-4 Was contribution part of child support order (NCR)"
   NRCHSUPPYR = "Yearly child support paid for the youngest NCR child LE 18"
   COPARENT = "GD-1 Whether R and child s mother are a good parenting team"
   RWANT = "HA-2 R Wants A(another) Child Some Time"
   PROBWANT = "HA-3 R probably wants/does not want A(nother) Child some time"
   JINTEND = "HB-2 R & W/P Intend to Have A(another) Child Some Time"
   JSUREINT = "HB-3 How sure R & W/P will/will not have A(another) Child"
   JINTENDN = "HB-4 How Many (More) Children R and W/P Intend?"
   JEXPECTL = "HB-5 Largest Number of (additional) Children R & W/P Intend"
   JEXPECTS = "HB-6 Smallest Number of (additional) Children R & W/P Intend"
   JINTNEXT = "HB-7 When R and W/P expect 1st/next child to be born"
   INTEND = "HC-2 R Intends To Have A(another) Child some time?"
   INTENDN = "HC-3 Number of (more) Children R Intends"
   EXPECTL = "HC-4 Largest number of (additional) Children R Intends"
   EXPECTS = "HC-5 Smallest number of (additional) Children R Intends"
   INTNEXT = "HC-6 When R expects 1st/next child to be born"
   USUALCAR = "IA-1 Is there place R usually goes when sick"
   USLPLACE = "IA-2 Place R usually goes when sick"
   USL12MOS = "IA-2a R has gone to his usual source of health care in last 12 months"
   CURRCOV = "IA-3 Currently covered by health insurance"
   COVERHOW01 = "IA-4 Type of health care coverage last 12 months - 1st mention"
   COVERHOW02 = "IA-4 Type of health care coverage last 12 months - 2nd mention"
   COVERHOW03 = "IA-4 Type of health care coverage last 12 months - 3rd mention"
   COVERHOW04 = "IA-4 Type of health care coverage last 12 months - 4th mention"
   PARINSUR = "IA-5 Covered by parents' plan"
   INS_EXCH = "IA-6 Obtained health insurance through Healthcare.gov or the State Marketplace"
   INS_PREM = "IA-7 Paid a premium for state-sponsored health plan"
   COVER12 = "IA-8 Whether R lacked health coverage any time in last 12 months"
   NUMNOCOV = "IA-9 How many of the last 12 months was R without coverage"
   YOUGOFPC = "IB-1 R ever received service at a Family Planning clinic"
   WHENGOFP = "IB-2 Last time R received service at a Family Planning clinic"
   YOUFPSVC1 = "IB-3 Services R received at last visit- 1st mention"
   YOUFPSVC2 = "IB-3 Services R received at last visit- 2nd mention"
   YOUFPSVC3 = "IB-3 Services R received at last visit- 3rd mention"
   YOUFPSVC4 = "IB-3 Services R received at last visit- 4th mention"
   YOUFPSVC5 = "IB-3 Services R received at last visit- 5th mention"
   YOUFPSVC6 = "IB-3 Services R received at last visit- 6th mention"
   DEAF = "IC-1 R has any serious difficulty hearing"
   BLIND = "IC-2 R has any serious difficulty seeing"
   DIFDECIDE = "IC-3 R has any serious difficulty w/memory or decision-making"
   DIFWALK = "IC-4 R has any serious difficulty walking or climbing stairs"
   DIFDRESS = "IC-5 R has any serious difficulty dressing or bathing"
   DIFOUT = "IC-6 R has any difficulty doing errands alone due to phys/mental/emotional conditions"
   EVRCANCER = "IC-7 R has ever had cancer"
   AGECANCER = "IC-7a Age when R was first told he had cancer"
   CANCTYPE = "IC-7b Type of cancer R (first) had"
   ALCORISK = "IC-8 Drinking increases chances of cancer"
   VISIT12MO1 = "ID-1 Had any of these visits to a doctor in last 12 months - 1st mention"
   VISIT12MO2 = "ID-1 Had any of these visits to a doctor in last 12 months - 2nd mention"
   VISIT12MO3 = "ID-1 Had any of these visits to a doctor in last 12 months - 3rd mention"
   SVC12MO1 = "ID-2 Received any of these services at visit in last 12 months - 1st mention"
   SVC12MO2 = "ID-2 Received any of these services at visit in last 12 months - 2nd mention"
   SVC12MO3 = "ID-2 Received any of these services at visit in last 12 months - 3rd mention"
   SVC12MO4 = "ID-2 Received any of these services at visit in last 12 months - 4th mention"
   SVC12MO5 = "ID-2 Received any of these services at visit in last 12 months - 5th mention"
   SVC12MO6 = "ID-2 Received any of these services at visit in last 12 months - 6th mention"
   SVC12MO7 = "ID-2 Received any of these services at visit in last 12 months - 7th mention"
   SVC12MO8 = "ID-2 Received any of these services at visit in last 12 months - 8th mention"
   NUMVISIT = "ID-3 Last 12 mos: # of visits for these services"
   PLACEVIS01 = "ID-4 Type of place R received service(s) - 1st mention"
   PLACEVIS02 = "ID-4 Type of place R received service(s) - 2nd mention"
   PLACEVIS03 = "ID-4 Type of place R received service(s) - 3rd mention"
   PLACEVIS04 = "ID-4 Type of place R received service(s) - 4th mention"
   PLACEVIS05 = "ID-4 Type of place R received service(s) - 5th mention"
   PLACEVIS06 = "ID-4 Type of place R received service(s) - 6th mention"
   SVCPAY1 = "ID-5 Payment for health/FP service(s) - 1st mention"
   SVCPAY2 = "ID-5 Payment for health/FP service(s) - 2nd mention"
   SVCPAY3 = "ID-5 Payment for health/FP service(s) - 3rd mention"
   SVCPAY4 = "ID-5 Payment for health/FP service(s) - 4th mention"
   SVCPAY5 = "ID-5 Payment for health/FP service(s) - 5th mention"
   TALKSA = "ID-6 At visits last 12 months doctor asked if sexually active"
   TALKEC = "ID-7 At visits last 12 months doctor talked about EC"
   TALKDM = "ID-8 At visits last 12 months doctor talked using condom & female method"
   WHYPSTD = "ID-8a Main reason chose place for  STD test"
   BARRIER1 = "ID-9 Reason did not see a doctor in past 12 months - 1st mention"
   BARRIER2 = "ID-9 Reason did not see a doctor in past 12 months - 2nd mention"
   BARRIER3 = "ID-9 Reason did not see a doctor in past 12 months - 3rd  mention"
   BARRIER4 = "ID-9 Reason did not see a doctor in past 12 months - 4th mention"
   BLDPRESS = "ID-10 R had blood pressure checked in past year"
   HIGHBP = "ID-11 R told she had high blood pressure in the past year"
   BPMEDS = "ID-12 R currently taking medicine for high blood pressure"
   ASKSMOKE = "ID-13 R asked if smoked in the past year"
   INFHELP = "IE-1 R & partner ever received medical help to have a baby"
   INFSVCS1 = "IE-2 Infertility services received - 1st mention"
   INFSVCS2 = "IE-2 Infertility services received - 2nd mention"
   INFSVCS3 = "IE-2 Infertility services received - 3rd mention"
   INFSVCS4 = "IE-2 Infertility services received - 4th mention"
   INFSVCS5 = "IE-2 Infertility services received - 5th mention"
   INFSVCS6 = "IE-2 Infertility services received - 6th mention"
   INFTEST = "IE-3 Who had Infertility Testing - R or Partner or Both"
   WHOINSEM = "IE-4 R's partner received R's sperm or someone else's"
   INFHLPNW = "IE-5 R & partner currently pursuing medical help to have baby"
   LASTVIS_M = "IE-6 Month of last visit for medical help to have a baby"
   LASTVIS_Y = "IE-6 Year of last visit for medical help to have a baby"
   CMINFVIS = "CM of last/most recent infertility service visit"
   INFRTHIS_1 = "IE-7 Male infertility problem diagnosed - 1st mention"
   INFRTHIS_2 = "IE-7 Male infertility problem diagnosed - 2nd mention"
   DONBLOOD = "IF-1 Has R Ever Donated Blood or Blood Products"
   HIVTEST = "IF-2 Any HIV test outside of blood donation"
   NOHIVTST = "IF-2b R's main reason for never having had an HIV test"
   WHENHIV_M = "IF-3 Month of most recent HIV test"
   WHENHIV_Y = "IF-3 Year of most recent HIV test"
   CMHIVTST = "CM of last/most recent HIV test"
   HIVTSTYR = "IF-3b Has R had HIV test in last 12 months"
   HIVRESULT = "IF-3d Whether R found out results of last HIV test"
   WHYNOGET = "IF-3e Main reason why R did not get results of last HIV test"
   PLCHIV = "IF-4 Place where R had last HIV test"
   RHHIVT1 = "IF-4a Used a rapid home HIV test"
   RHHIVT21 = "IF-4b Reasons for rapid home HIV test - 1st mentioned"
   HIVTST = "IF-5 Reason for this (last) HIV test- 1st mention"
   WHOSUGG = "IF-5b Who suggested R be tested for HIV"
   TALKDOCT = "IF-6 Has a doctor ever talked with R about HIV/AIDS"
   AIDSTALK01 = "IF-7 HIV Topics Covered in Discussion with Doctor/Provider - 1st mention"
   AIDSTALK02 = "IF-7 HIV Topics Covered in Discussion with Doctor/Provider - 2nd mention"
   AIDSTALK03 = "IF-7 HIV Topics Covered in Discussion with Doctor/Provider - 3rd mention"
   AIDSTALK04 = "IF-7 HIV Topics Covered in Discussion with Doctor/Provider - 4th mention"
   AIDSTALK05 = "IF-7 HIV Topics Covered in Discussion with Doctor/Provider - 5th mention"
   AIDSTALK06 = "IF-7 HIV Topics Covered in Discussion with Doctor/Provider - 6th mention"
   AIDSTALK07 = "IF-7 HIV Topics Covered in Discussion with Doctor/Provider - 7th mention"
   AIDSTALK08 = "IF-7 HIV Topics Covered in Discussion with Doctor/Provider - 8th mention"
   AIDSTALK09 = "IF-7 HIV Topics Covered in Discussion with Doctor/Provider - 9th mention"
   AIDSTALK10 = "IF-7 HIV Topics Covered in Discussion with Doctor/Provider - 10th mention"
   AIDSTALK11 = "IF-7 HIV topics covered in post-test counseling - 11th mention"
   SAMEADD = "JA-0 R living at curr address on April 1, 2010"
   CNTRY10 = "JA-1 R living in US on April 1, 2010"
   BRNOUT = "JA-7 Whether R was born outside of US"
   YRSTRUS = "Year R came to the United States to stay"
   RELRAISD = "JB-1/JB-2 Religion in which R was raised"
   ATTND14 = "JB-4 How often R attended religious services at age 14"
   RELCURR = "JB-5/JB-6 Religion R is now"
   RELTRAD = "Current religious affiliation by Protestant categories"
   FUNDAM1 = "JB-8 Fundamental affiliation, if any - 1st mention"
   FUNDAM2 = "JB-8 Fundamental affiliation, if any - 2nd mention"
   FUNDAM3 = "JB-8 Fundamental affiliation, if any - 3rd mention"
   FUNDAM4 = "JB-8 Fundamental affiliation, if any - 4th mention"
   RELDLIFE = "JB-9 How important is religion in R's daily life"
   ATTNDNOW = "JB-10 How often R currently attends religious services"
   MILSVC = "JC-1 R ever on active duty in Armed Forces for 6+ months"
   WRK12MOS = "JD-4 How many of the last 12 months did R work"
   FPT12MOS = "JD-5 Did R work full-time, part-time or both in last 12 months"
   DOLASTWK1 = "JE-1 What R was doing last week (employment status) - 1st mention"
   DOLASTWK2 = "JE-1 What R was doing last week (employment status) - 2nd mention"
   DOLASTWK3 = "JE-1 What R was doing last week (employment status) - 3rd mention"
   DOLASTWK4 = "JE-1 What R was doing last week (employment status) - 4th mention"
   DOLASTWK5 = "JE-1 What R was doing last week (employment status) - 5th mention"
   DOLASTWK6 = "JE-1 What R was doing last week (employment status) - 6th mention"
   RWRKST = "Whether R is currently employed (working or temporarily on leave from a job)"
   WORKP12 = "Whether R worked in the previous 12 months"
   RPAYJOB = "JE-2 Did R ever work at a job for pay on regular basis"
   RNUMJOB = "JE-3 Number of jobs R worked last week or last week R worked"
   RFTPTX = "JE-4 R worked full- or part-time at curr/last/primary job"
   REARNTY = "Whether R ever worked at all"
   SPLSTWK1 = "JF-1 W/P doing what last week (employment status) - 1st mention"
   SPLSTWK2 = "JF-1 W/P doing what last week (employment status) - 2nd mention"
   SPLSTWK3 = "JF-1 W/P doing what last week (employment status) - 3rd mention"
   SPLSTWK4 = "JF-1 W/P doing what last week (employment status) - 4th mention"
   SPLSTWK5 = "JF-1 W/P doing what last week (employment status) - 5th mention"
   SPWRKST = "Whether R's wife/partner is currently employed (working or temporarily on leave from a job)"
   SPPAYJOB = "JF-2 Did W/P ever work at a job for pay on regular basis"
   SPNUMJOB = "JF-3 Number of jobs W/P worked last week or last week worked"
   SPFTPTX = "JF-4 W/P worked full- or part-time at curr/last/primary job"
   SPEARNTY = "Whether R's wife/partner ever worked for pay"
   SAMESEX = "JG-1 Sexual relations between two same-sex adults are all right"
   CHSUPPOR = "JG-2 Okay for unmarried woman to have and raise a child"
   REACTSLF = "JG-3 How R would feel if he got (wife/partner/a female) pregnant now"
   CHBOTHER = "JG-4 How bothered would R be if he did not have children"
   SEXNEEDS = "JG-5 Men have greater sexual needs than women"
   WHENSICK = "JG-6 Men only need to see a doctor when they are hurt or sick"
   SHOWPAIN = "JG-7 A man should not show pain"
   PMARCOHB = "Whether R ever cohabited premaritally"
   COHCHANCE = "JG-8 R thinks he will cohabit in the future"
   MARRCHANCE = "JG-9 R thinks he will (re)marry in the future"
   PMARCOH = "JG-10 R thinks he will live with a future wife before marriage"
   ACASILANG = "JG-11 Language to be used in ACASI"
   GENHEALT = "KA-4 R's General Health"
   INCHES = "Height (converted to inches) (original version)"
   RWEIGHT = "KA-6 R's Weight (Pounds)"
   BMI = "Body Mass Index (computed in post-processing)"
   DRWEIGH = "KA-6a Was R weighed in past year by a medical care provider"
   TELLWGHT = "KA-6b Was R told about his weight status"
   WGHTSCRN = "KA-6c Was R provided with diet or exercise counseling by medical care provider"
   ENGSPEAK = "KA-7 How well does R speak English"
   NOBEDYR = "KB-1a R had to stay overnight in a shelter, car or someplace outdoors"
   STAYREL = "KB-1b R had to stay overnight with a friend or relative"
   JAILED = "KB-2 R Last 12 months: Spent time in jail"
   JAILED2 = "KB-3 R Ever in jail, prison, juvenile detention"
   FRQJAIL = "KB-4 Number of times R has ever been in jail, prison, or juvenile detention facility"
   FRQJAIL2 = "KB-5 Length of time R was jailed or detained the last time"
   EVSUSPEN = "KB-6 Ever been suspended or expelled from school?"
   GRADSUSP = "KB-7 Grade when suspended or expelled from school"
   SMK100 = "KC-0a1 Has R Smoked at Least 100 Cigarettes in Life"
   AGESMK = "KC-0a2 Age That R First Began Smoking Regularly"
   SMOKE12 = "KC-0a3 Last 12 mos: How Often R Smoked Cigarettes"
   SMKSTOP = "KC-0a4 Was R provided with counseling to stop smoking"
   DRINK12 = "KC-1 Last 12 months: How Often Drank Alcoholic Beverages"
   UNIT30D = "KC-1a-U Drinking in past 30 days - unit of response"
   DRINK30D = "KC-1a-N Drinking in past 30 days - amount"
   DRINKDAY = "KC-1b Number of drinks R had on average on days R drank"
   BINGE30 = "KC-1c Number of times R binge-drank in past 30 days"
   DRNKMOST = "KC-1d Largest number of drinks R had on any occasion in past 30 days"
   BINGE12 = "KC-2 Last 12 months: How Often Binge Drank (5+ in Couple of Hours)"
   POT12 = "KC-3 Last 12 months: How Often Smoked Marijuana"
   COC12 = "KC-4 Last 12 months: How Often Used Cocaine"
   CRACK12 = "KC-5 Last 12 months: How Often Used Crack"
   CRYSTMTH12 = "KC-5a Last 12 months: How Often Used Crystal/Meth"
   INJECT12 = "KC-6 Last 12 months: How Often Injected Non-Prescription Drugs"
   MADEPREG = "KD-1 Has R ever made someone pregnant"
   PREGTOT2 = "KD-2 Number of times R made someone pregnant"
   PREGACASI = "Number of pregnancies R reported fathering (in ACASI)"
   NUMABORT = "KD-3 # pregnancies fathered by R that ended in abortion"
   NUMLIVEB = "KD-4 # pregnancies fathered by R that ended in live birth"
   TOLDPREG = "KD-5 R ever been told may have made someone pregnant"
   WHATHAPP = "KD-6 what outcome last time R was told he made someone pregnant"
   FEMTOUCH = "KE-1 R ever touched by female and ejaculated"
   VAGSEX = "KE-2 Ever had vaginal intercourse"
   AGEVAGR = "KE-2b R's age at first vaginal intercourse"
   CONDVAG = "KE-3 Was condom used at last vaginal sex"
   COND1BRK = "KE-3a Did condom break"
   COND1OFF = "KE-3b When was the condom used"
   WHYCONDL = "KE-4 Reason for condom use at last vaginal sex"
   GETORALF = "KE-5 R has ever received oral sex from a female"
   CONDFELL = "KE-6 Was condom used when last received oral sex from a female"
   GIVORALF = "KE-7 R has ever performed oral sex on a female"
   ANYORAL = "Whether R has ever had oral sex (either type) with an opposite-sex (female) partner"
   TIMING = "KE-7b oral sex before or after vaginal sex"
   ANALSEX = "KE-8 R ever had anal sex with a female"
   CONDANAL = "KE-9 Was condom used when last had anal sex with a female"
   OPPSEXANY = "Ever had vaginal, oral or anal sex with a female"
   OPPSEXGEN = "Ever had male-genital-involving sex with a female"
   CONDSEXL = "KE-10 Was condom used at last sex of any kind with a female"
   WANTSEX1 = "KF-1 How much R wanted his first vaginal intercourse to happen"
   HOWOLD = "KF-1b R's Age at First Vaginal Intercourse"
   EVRFORCD = "KF-2 R Ever Forced by Female to Have Vaginal Intercourse"
   AGEFORC1 = "KF-3 R's age at first forced vaginal sex by female"
   GIVNDRG2 = "KF-4a How forced by female: R was given alcohol/drugs"
   SHEBIGOL = "KF-4b How forced by female: she was bigger than R or grown up, R young"
   ENDRELA2 = "KF-4c How forced by female: she said relationship would end if no sex"
   WRDPRES2 = "KF-4d How forced by female: R was pressured by her words but no threats of harm"
   THRTPHY2 = "KF-4e How forced by female: R was threatened with physical hurt/injury"
   PHYSHRT2 = "KF-4f How forced by female: R was physically hurt/injured"
   HELDDWN2 = "KF-4g How forced by female: R was physically held down"
   PARTSLIF_1 = "KG-1 # of Female Sex Partners in Entire Life (any type of sex)"
   PARTSLFV = "KG-1v Verify # Female Sex Partners in Entire Life (any type of sex)"
   PARTSLIF_2 = "KG-1 # of Female Sex Partners in Entire Life (any type of sex)"
   OPPLIFENUM = "Number of opposite-sex partners in lifetime for all types of sex. (computed in FC K-9d)"
   PARTS12_1 = "KG-2 # Female Sex Partners in Last 12 Months (any type of sex)"
   PARTS12V = "KG-2v Verify # Fem Sex Partners in Last 12 Months (any type of sex)"
   PARTS12_2 = "KG-2 # Female Sex Partners in Last 12 Months (any type of sex)"
   OPPYEARNUM = "Number of opposite-sex partners in last 12 months for all types of sex (computed in FC K-9d)"
   NEWYEAR = "KG-2YR re-ask # female partners in last 12 months"
   NEWLIFE = "KG-2LF re-ask # female partners in lifetime"
   VAGNUM12 = "KG-2YRa # female partners past 12 months had vaginal sex with"
   ORALNUM12 = "KG-2YRb # female partners past 12 months had oral sex with, either giving or receiving"
   ANALNUM12 = "KG-2YRc # female partners past 12 months had anal sex with"
   NONMONOG = "KG-4 Last 12 months: R Had Sex w/Female Having Sex With Others"
   NNONMONOG1 = "KG-5a Number of female partners in last year who were having sex with others"
   NNONMONOG2 = "KG-5b Number of other partners R's 1 partner in last year had at same time"
   NNONMONOG3 = "KG-5c Number of other partners R's most recent partner in last year had at same time"
   FEMSHT12 = "KG-6 Last 12 months: R had sex w/female intravenous drug user"
   JOHNFREQ = "KG-7 Last 12 months: R gave money or drugs to female for sex"
   PROSTFRQ = "KG-8 Last 12 months: R took money or drugs from female for sex"
   HIVFEM12 = "KG-9 Last 12 months: R had sex with an HIV-positive female"
   GIVORALM = "KH-1 R has ever performed oral sex on a male"
   GETORALM = "KH-2 R has ever received oral sex from a male"
   ORALCONDM = "KH-2b R used a condom at last oral sex with a male partner"
   ANALSEX2 = "KH-3 Has a male partner ever put his penis in R's anus"
   ANALCONDM1 = "KH-3b R used a condom at last receptive anal sex with a male partner"
   ANALSEX3 = "KH-4 Has R ever put his penis in a male partner's anus"
   ANALCONDM2 = "KH-4b R used a condom at last insertive anal sex with a male partner"
   MALESEX = "KH-4C R had any other sexual experience with another male"
   SAMESEXANY = "Ever had oral or anal sex with a male"
   MALPRTAGE = "KH-5 Relative age of R s most recent male sexual partner"
   MALPRTHRACE = "HISPRACE2-recode-like variable for current/most recent same-sex partner (computed in post-processing)"
   EVRFORC2 = "KI-1 R Ever Been Forced by a Male to Have Sex"
   AGEFORC2 = "KI-2 R's Age at First Forced Sex by a Male"
   GIVNDRG3 = "KI-3a How forced by male: given alcohol/drugs"
   HEBIGOLD = "KI-3b How forced by male: He was bigger than R or grown up, R young"
   ENDRELA3 = "KI-3c How forced by male: he said relationship would end if no sex"
   WRDPRES3 = "KI-3d How forced by male: R was pressured by his words, but no threat of harm"
   THRTPHY3 = "KI-3e How forced by male: R was threatened with physical hurt/injury"
   PHYSHRT3 = "KI-3f How forced by male: R was physically hurt/injured"
   HELDDWN3 = "KI-3g How forced by male: R was physically held down"
   MALEPRTS_1 = "KJ-1 Number of Male Sex Partners in Entire Life"
   MALEPRTSV = "KJ-1v Verify number of Male Sex Partners in Entire Life"
   MALEPRTS_2 = "KJ-1 Number of Male Sex Partners in Entire Life"
   SAMLIFENUM = "Number of same-sex partners in lifetime. (computed in FC K-14d)"
   MALPRT12_1 = "KJ-2 Number of Male Sex Partners in Last 12 Months"
   MALPRT12V = "KJ-2v Verify Number of Sex Partners in Last 12 Months"
   MALPRT12_2 = "KJ-2 Number of Male Sex Partners in Last 12 Months"
   SAMYEARNUM = "Number of same-sex partners in last 12 months. (computed in FC K-14f)"
   SAMORAL12 = "KJ-2YRa Number of male (same-sex) partners in last year for oral sex"
   RECEPANAL12 = "KJ-2YRb Number of male (same-sex) partners in last year for receptive anal sex"
   INSERANAL12 = "KJ-2YRc Number of male (same-sex) partners in last year for insertive anal sex"
   SAMESEX1 = "KJ-3 R s age at first sexual experience with same-sex (male) partner"
   MSAMEREL = "KJ-3a R s relationship with 1st male partner at time of 1st sex with him"
   MSMNONMON = "KJ-4 Number of non-monogamous male (same-sex) partners in last 12 months"
   MALSHT12 = "KJ-5 Last 12 months: R had sex w/male intravenous drug user"
   JOHN2FRQ = "KJ-6 Last 12 months: R gave money or drugs to male for sex"
   PROS2FRQ = "KJ-7 Last 12 months: R took money or drugs from male for sex"
   HIVMAL12 = "KJ-8 Last 12 months: R had sex with an HIV-positive male"
   MSMWEB12 = "KJ-9 Male partners in last 12 months: First met any using internet"
   MSMSORT12 = "KJ-10 Male partners in last 12 months: Chosen based on HIV status"
   CNDLSMAL = "KJ-11 Was condom used at last sex with a male partner"
   CONDALLS = "KK-1 Was condom used at last sex w/male or female partner"
   MFLASTP = "KK-2 Was Last Sex Partner Male or Female?"
   WHYCOND = "KK-3 Reason For Condom Use at Last Sex (if vaginal)"
   ATTRACT = "KK-4 R's Sexual Attraction to Males vs Females"
   ORIENT_A = "KK-5a R s Sexual Orientation random 1/2 sample"
   ORIENT_B = "KK-5b R s Sexual Orientation random 1/2 sample"
   CONFCONC = "KK-6a Not go for sexual or reproductive health care because your parents might find out"
   TIMALON = "KK-6b Dr ever spend any time alone with you without a parent"
   RISKCHEK1 = "KK-6c Dr ever asked R about sexual orientation or sex of his partners"
   RISKCHEK2 = "KK-6d Dr ever asked R about his number of sexual partners"
   RISKCHEK3 = "KK-6e Dr ever asked R about his use of condoms"
   RISKCHEK4 = "KK-6f Dr ever asked R about the types of sex he has"
   RECTDOUCH = "KK-6g Rectal douching in last 12 months"
   STDTST12 = "KK-7 Last 12 months: R Tested for STD"
   STDSITE12 = "KK-7b R had pharyngeal or rectal STD testing in the past year"
   STDTRT12 = "KK-8 Last 12 months: R Treated for STD"
   GON = "KK-9 Last 12 months: R told he had gonorrhea"
   CHLAM = "KK-10 Last 12 months: R told he had chlamydia"
   HERPES = "KK-11 R ever told he had genital herpes"
   GENWARTS = "KK-12 R ever told he had genital warts"
   SYPHILIS = "KK-13 R ever told he had syphilis"
   EVRINJECT = "KK-14 Ever Injected Non-Prescription Drugs in Lifetime"
   EVRSHARE = "KK-15 Ever (in life) Shared an IV Needle"
   EARNTYPE = "KL-0a Report total earnings before taxes by week/month/year"
   EARN = "KL-0b Total earnings reported by category"
   EARNDK1 = "KL-0c If DK/RF total earnings, was it $20,000 or more"
   EARNDK2 = "KL-0d If more than $20,000, was it $50,000 or more"
   EARNDK3 = "KL-0e If more than $50,000, was it $75,000 or more"
   EARNDK4 = "KL-0f If more than $75,000, was it $100,000 or more"
   TOINCWMY = "KL-2 Prefer to report total income per week/month/year"
   TOTINC = "KL-3 Total combined family income in prior calendar year"
   FMINCDK1 = "KL-3a If DK/RF combined family income, was it less than $50,000 or $50,000 or more?"
   FMINCDK2 = "KL-3b If income less than $50,000, was it less than $35,000?"
   FMINCDK3 = "KL-3c If income less than $35,000, was it less than the poverty threshold?"
   FMINCDK4 = "KL-3d If income more than $50,000, was it $75,000 or more?"
   FMINCDK5 = "KL-3e If income more than $75,000, was it $100,000 or more?"
   PUBASST = "KL-4 Received cash assistance from state/county welfare program in prior calendar year"
   PUBASTYP1 = "KL-5 Type of cash assistance program in prior calendar year-1st mention"
   FOODSTMP = "KL-6 In prior calendar year, received food stamps"
   WIC = "KL-7 In prior calendar year, received WIC"
   HLPTRANS = "KL-8a In prior calendar year, received transportation assistance"
   HLPCHLDC = "KL-8b In prior calendar year, received child care services or assistance"
   HLPJOB = "KL-8c In prior calendar year, received job training/search help from social svcs"
   FREEFOOD = "KL-9 In last 12 months, received free or reduced-cost food because couldn't afford to buy food"
   HUNGRY = "KL-10 In last 12 months, a family member was hungry because could not afford more food"
   MED_COST = "KL-11 In last 12 months, a family member didn't see doctor because of cost"
   AGER = "R's age at interview (RECODE)"
   FMARITAL = "Formal (legal) marital status  (RECODE)"
   RMARITAL = "Informal marital status (RECODE)"
   EDUCAT = "Education (number of years of schooling)  (RECODE)"
   HIEDUC = "Highest completed year of school or highest degree received  (RECODE)"
   HISPANIC = "Hispanic origin of respondent  (RECODE)"
   RACE = "Race of respondent  (RECODE)"
   HISPRACE = "Race & Hispanic origin of respondent - 1977 OMB standards  (RECODE)"
   HISPRACE2 = "Race & Hispanic origin of respondent - 1997 OMB standards  (RECODE)"
   NUMKDHH = "Number of bio/adopt/related/legal children under age 18 in household  (RECODE)"
   NUMFMHH = "Number of family members in household  (RECODE)"
   HHFAMTYP = "Type of household/family structure  (RECODE)"
   HHPARTYP = "Type of parental situation in household  (RECODE)"
   NCHILDHH = "Number of R's children (18 or younger) living in household  (RECODE)"
   HHKIDTYP = "Whether R has children (18 or younger), and whether bio or non-bio, living in the household  (RECODE)"
   CSPBBHH = "Number of R's biological children (18 or younger) with current spouse/ cohabiting partner who live in household  (RECODE)"
   CSPSBHH = "Number of R's nonbiological children (18 or younger) in household who are the biological chldren of his current wife/cohabiting partner  (RECODE)"
   CSPOKDHH = "Number of all other children (18 or younger) living in household with R and current spouse/cohabiting partner  (RECODE)"
   INTCTFAM = "Intact status of childhood family  (RECODE)"
   PARAGE14 = "Parental living situation at age 14  (RECODE)"
   EDUCMOM = "Mother's (or mother-figure's) education  (RECODE)"
   AGEMOMB1 = "Age of mother (or mother-figure) at first birth  (RECODE)"
   FMARNO = "Number of times R has been married  (RECODE)"
   AGER_I = "AGER  Imputation Flag"
   FMARITAL_I = "FMARITAL  Imputation Flag"
   RMARITAL_I = "RMARITAL Imputation Flag"
   EDUCAT_I = "EDUCAT  Imputation Flag"
   HIEDUC_I = "HIEDUC  Imputation Flag"
   HISPANIC_I = "HISPANIC  Imputation Flag"
   RACE_I = "RACE  Imputation Flag"
   HISPRACE_I = "HISPRACE  Imputation Flag"
   HISPRACE2_I = "HISPRACE2  Imputation Flag"
   NUMKDHH_I = "NUMKDHH  Imputation Flag"
   NUMFMHH_I = "NUMFMHH  Imputation Flag"
   HHFAMTYP_I = "HHFAMTYP Imputation Flag"
   HHPARTYP_I = "HHPARTYP Imputation Flag"
   NCHILDHH_I = "NCHILDHH Imputation Flag"
   HHKIDTYP_I = "HHKIDTYP Imputation Flag"
   CSPBBHH_I = "CSPBBHH Imputation Flag"
   CSPSBHH_I = "CSPSBHH Imputation Flag"
   CSPOKDHH_I = "CSPOKHH Imputation Flag"
   INTCTFAM_I = "INTCTFAM Imputation Flag"
   PARAGE14_I = "PARAGE14 Imputation Flag"
   EDUCMOM_I = "EDUCMOM Imputation Flag"
   AGEMOMB1_I = "AGEMOMB1 Imputation Flag"
   FMARNO_I = "FMARNO Imputation Flag"
   HADSEX = "Whether R ever had sexual intercourse (RECODE)"
   SEXONCE = "Whether R has had sex only once (RECODE)"
   VRY1STSX = "CM date of 1st sex (RECODE)"
   FIRSTPFLAG = "Flag indicating which section and which series reflects R's first sexual partner"
   VRY1STAG = "R's age at 1st sex (RECODE)"
   ELAPSED = "# of mos btwn 1st sex and last sex:R's 1st partner (DF series only)"
   SEXMAR = "Months between 1st intercourse and 1st marriage (RECODE)"
   SEXUNION = "Months between 1st intercourse and 1st coresidential union or interview (marriage or cohabitation) (RECODE)"
   FSEXRLTN = "Relationship with 1st sexual partner at time of 1st sex (RECODE)"
   SEX1MTHD1 = "Contraceptive method use at 1st sexual intercourse: 1st mentioned (RECODE)"
   SEX1MTHD2 = "Contraceptive method use at 1st sexual intercourse: 2nd mentioned (RECODE)"
   SEX1MTHD3 = "Contraceptive method use at 1st sexual intercourse: 3rd mentioned (RECODE)"
   SEX1MTHD4 = "Contraceptive method use at 1st sexual intercourse: 4th mentioned (RECODE)"
   LSEXDATE = "Date of last (or most recent) sexual intercourse (RECODE)"
   SEX3MO = "Whether R had sexual intercourse in past 3 months (based on LSEXDATE) (RECODE)"
   SEX12MO = "Whether R had sexual intercourse in past 12 months (based on LSEXDATE) (RECODE)"
   LSEXRAGE = "R's age at last (or most recent) sexual intercourse (RECODE)"
   LSEXPRAC = "Race/ethnicity of last (or most recent) sexual partner"
   LSEXRLTN = "Relationship with last sexual partner at last sex ever (RECODE)"
   LSEXUSE1 = "Contraceptive method used at last sexual intercourse ever - 1st mentioned (RECODE)"
   LSEXUSE2 = "Contraceptive method used at last sexual intercourse ever - 2nd mentioned (RECODE)"
   LSEXUSE3 = "Contraceptive method used at last sexual intercourse ever - 3rd mentioned (RECODE)"
   LSEXUSE4 = "Contraceptive method used at last sexual intercourse ever - 4th mentioned (RECODE)"
   METH12M1 = "Contraceptive method used at last sexual intercourse in past 12 mos: 1st  mentioned (RECODE)"
   METH12M2 = "Contraceptive method used at last sexual intercourse in past 12 mos: 2nd  mentioned (RECODE)"
   METH12M3 = "Contraceptive method used at last sexual intercourse in past 12 mos: 3rd  mentioned  (RECODE)"
   METH12M4 = "Contraceptive method used at last sexual intercourse in past 12 mos: 4th  mentioned (RECODE)"
   METH3M1 = "Contraceptive method used last sexual intercourse in past 3 mos: 1st mentioned (RECODE)"
   METH3M2 = "Contraceptive method used last sexual intercourse in past 3 mos: 2nd mentioned (RECODE)"
   METH3M3 = "Contraceptive method used last sexual intercourse in past 3 mos: 3rd mentioned (RECODE)"
   METH3M4 = "Contraceptive method used last sexual intercourse in past 3 mos: 4th mentioned (RECODE)"
   NUMP3MOS = "Number of female sexual partners in last 3 mos (RECODE)"
   LIFPRTNR = "Number of opposite-sex partners in lifetime (RECODE)"
   PARTS1YR = "Number of opposite-sex partners in past 12 mos (RECODE)"
   PARTDUR1 = "Months between 1st and most recent intercourse with last/most recent partner (RECODE)"
   PARTDUR2 = "Months between 1st and most recent intercourse with 2nd-to-last partner within past 12 mos (RECODE)"
   PARTDUR3 = "Months between 1st and most recent intercourse with 3rd-to-last partner within past 12 mos (RECODE)"
   COHEVER = "Whether R ever cohabited (including premarital cohabitation) (RECODE)"
   EVMARCOH = "Whether R ever married or cohabited (RECODE)"
   PMARRNO = "Number of premarital cohabitations (RECODE)"
   NONMARR = "Number of nonmarital cohabitations (RECODE)"
   TIMESCOH = "Total number of cohabitations  (RECODE)"
   MARDAT01 = "Year of R's 1st marriage (RECODE)"
   MARDAT02 = "Year of R's 2nd marriage (RECODE)"
   MARDAT03 = "Year of R's 3rd marriage (RECODE)"
   MARDAT04 = "Year of R's 4th marriage (RECODE)"
   MAREND01 = "How R's 1st marriage ended (RECODE)"
   MAREND02 = "How R's 2nd marriage ended (RECODE)"
   MAREND03 = "How R's 3rd marriage ended (RECODE)"
   MAREND04 = "How R's 4th marriage ended (RECODE)"
   MARDIS01 = "Year when R's 1st marriage ended (RECODE)"
   MARDIS02 = "Year when R's 2nd marriage ended (RECODE)"
   MARDIS03 = "Year when R's 3rd marriage ended (RECODE)"
   MARDIS04 = "Year when R's 4th marriage ended (RECODE)"
   MAR1DISS = "Months between 1st marriage and dissolution of 1st marriage (or interview) (RECODE)"
   PREMARW1 = "Whether R lived premaritally with his first wife (RECODE)"
   COHAB1 = "Year of 1st cohabitation (incl. premarital cohabitation) (RECODE)"
   COHSTAT = "Cohabitation status experience relative to 1st marriage (RECODE)"
   COHOUT = "Outcome of R's first (if premarital) cohabitation (RECODE)"
   COH1DUR = "Duration (in months) of R's first (if premarital) cohabitation (RECODE)"
   CSPBIOKD = "Total number of biological children R has had with current spouse or cohabiting partner (RECODE)"
   DATBABY1 = "Year when R fathered his first biological child (RECODE)"
   AGEBABY1 = "Age when R fathered his first biological child (RECODE)"
   B1PREMAR = "Whether R's first biological child was born before his first marriage (premaritally) (RECODE)"
   MARBABY1 = "Formal marital status at time of R's first biological child's birth (RECODE)"
   CEBOW = "Number of biological children born out of wedlock (RECODE)"
   CEBOWC = "Number of biological children born out of wedlock, in cohabiting unions (RECODE)"
   CEBOWP = "Number of biological children born out of wedlock, but paternity established (RECODE)"
   EVRNOPAT = "Whether R has never established paternity for his biological children born out of wedlock (RECODE)"
   NONLIVEB = "Number of non-live birth pregnancies R has fathered (RECODE)"
   COMPREG = "Number of completed pregnancies R has fathered (RECODE)"
   ABORTION = "Number of abortions R has fathered (RECODE)"
   LOSSNUM = "Number of spontaneous pregnancy losses R has fathered (RECODE)"
   PARENT01 = "Other biological parent of R's 1st biological child (RECODE)"
   PARENT02 = "Other biological parent of R's 2nd biological child (RECODE)"
   PARENT03 = "Other biological parent of R's 3rd biological child (RECODE)"
   PARENT04 = "Other biological parent of R's 4th biological child (RECODE)"
   PARENT05 = "Other biological parent of R's 5th biological child (RECODE)"
   PARENT06 = "Other biological parent of R's 6th biological child (RECODE)"
   PARENT07 = "Other biological parent of R's 7th biological child (RECODE)"
   PARENT08 = "Other biological parent of R's 8th biological child (RECODE)"
   PARENT09 = "Other biological parent of R's 9th biological child (RECODE)"
   PARENT10 = "Other biological parent of R's 10th biological child (RECODE)"
   WANTB01 = "Wantedness of pregnancy: R's 1st bio child aged 18 or younger (RECODE)"
   WANTB02 = "Wantedness of pregnancy: R's 2nd bio child aged 18 or younger (RECODE)"
   WANTB03 = "Wantedness of pregnancy: R's 3rd bio child aged 18 or younger (RECODE)"
   WANTB04 = "Wantedness of pregnancy: R's 4th bio child aged 18 or younger (RECODE)"
   WANTB05 = "Wantedness of pregnancy: R's 5th bio child aged 18 or younger (RECODE)"
   WANTB06 = "Wantedness of pregnancy: R's 6th bio child aged 18 or younger (RECODE)"
   WANTB07 = "Wantedness of pregnancy: R's 7th bio child aged 18 or younger (RECODE)"
   WANTB08 = "Wantedness of pregnancy: R's 8th bio child aged 18 or younger (RECODE)"
   WANTB09 = "Wantedness of pregnancy: R's 9th bio child aged 18 or younger (RECODE)"
   WANTB10 = "Wantedness of pregnancy: R's 10th bio child aged 18 or younger (RECODE)"
   HADSEX_I = "HADSEX Imputation Flag"
   SEXONCE_I = "SEXONCE Imputation Flag"
   VRY1STSX_I = "VRY1STSX Imputation Flag"
   VRY1STAG_I = "VRY1STAG Imputation Flag"
   SEXMAR_I = "SEXMAR Imputation Flag"
   SEXUNION_I = "SEXUNION Imputation Flag"
   FSEXRLTN_I = "FSEXRLTN Imputation Flag"
   SEX1MTHD1_I = "SEX1MTHD1 Imputation Flag"
   SEX1MTHD2_I = "SEX1MTHD2 Imputation Flag"
   SEX1MTHD3_I = "SEX1MTHD3 Imputation Flag"
   SEX1MTHD4_I = "SEX1MTHD4 Imputation Flag"
   LSEXDATE_I = "LSEXDATE Imputation Flag"
   SEX3MO_I = "SEX3MO Imputation Flag"
   SEX12MO_I = "SEX12MO Imputation Flag"
   LSEXRAGE_I = "LSEXRAGE Imputation Flag"
   LSEXRLTN_I = "LSEXRLTN Imputation Flag"
   LSEXUSE1_I = "LSEXUSE1 Imputation Flag"
   LSEXUSE2_I = "LSEXUSE2 Imputation Flag"
   LSEXUSE3_I = "LSEXUSE3 Imputation Flag"
   LSEXUSE4_I = "LSEXUSE4 Imputation Flag"
   METH12M1_I = "METH12M1 Imputation Flag"
   METH12M2_I = "METH12M2 Imputation Flag"
   METH12M3_I = "METH12M3 Imputation Flag"
   METH12M4_I = "METH12M4 Imputation Flag"
   METH3M1_I = "METH3M1 Imputation Flag"
   METH3M2_I = "METH3M2 Imputation Flag"
   METH3M3_I = "METH3M3 Imputation Flag"
   METH3M4_I = "METH3M4 Imputation Flag"
   NUMP3MOS_I = "NUMP3MOS Imputation Flag"
   LIFPRTNR_I = "LIFPRTNR Imputation Flag"
   PARTS1YR_I = "PARTS1YR Imputation Flag"
   PARTDUR1_I = "PARTDUR1 Imputation Flag"
   PARTDUR2_I = "PARTDUR2 Imputation Flag"
   PARTDUR3_I = "PARTDUR3 Imputation Flag"
   COHEVER_I = "COHEVER Imputation Flag"
   EVMARCOH_I = "EVMARCOH Imputation Flag"
   PMARRNO_I = "PMARRNO Imputation Flag"
   NONMARR_I = "NONMARR Imputation Flag"
   TIMESCOH_I = "TIMESCOH Imputation Flag"
   MARDAT01_I = "MARDAT01 Imputation Flag"
   MARDAT02_I = "MARDAT02 Imputation Flag"
   MARDAT03_I = "MARDAT03 Imputation Flag"
   MARDAT04_I = "MARDAT04 Imputation Flag"
   MAREND01_I = "MAREND01 Imputation Flag"
   MAREND02_I = "MAREND02 Imputation Flag"
   MAREND03_I = "MAREND03 Imputation Flag"
   MAREND04_I = "MAREND04 Imputation Flag"
   MARDIS01_I = "MARDIS01 Imputation Flag"
   MARDIS02_I = "MARDIS02 Imputation Flag"
   MARDIS03_I = "MARDIS03 Imputation Flag"
   MARDIS04_I = "MARDIS04 Imputation Flag"
   MAR1DISS_I = "MAR1DISS Imputation Flag"
   PREMARW1_I = "PREMARW1 Imputation Flag"
   COHAB1_I = "COHAB1 Imputation Flag"
   COHSTAT_I = "COHSTAT Imputation Flag"
   COHOUT_I = "COHOUT Imputation Flag"
   COH1DUR_I = "COH1DUR Imputation Flag"
   CSPBIOKD_I = "CSPBIOKD Imputation Flag"
   DATBABY1_I = "DATBABY1 Imputation Flag"
   AGEBABY1_I = "AGEBABY1 Imputation Flag"
   B1PREMAR_I = "B1PREMAR Imputation Flag"
   MARBABY1_I = "MARBABY1 Imputation Flag"
   CEBOW_I = "CEBOW Imputation Flag"
   CEBOWC_I = "CEBOWC Imputation Flag"
   CEBOWP_I = "CEBOWP Imputation Flag"
   EVRNOPAT_I = "EVRNOPAT Imputation Flag"
   NONLIVEB_I = "NONLIVEB Imputation Flag"
   COMPREG_I = "COMPREG Imputation Flag"
   ABORTION_I = "ABORTION Imputation Flag"
   LOSSNUM_I = "LOSSNUM Imputation Flag"
   PARENT01_I = "PARENT01 Imputation Flag"
   PARENT02_I = "PARENT02 Imputation Flag"
   PARENT03_I = "PARENT03 Imputation Flag"
   PARENT04_I = "PARENT04 Imputation Flag"
   PARENT05_I = "PARENT05 Imputation Flag"
   PARENT06_I = "PARENT06 Imputation Flag"
   PARENT07_I = "PARENT07 Imputation Flag"
   PARENT08_I = "PARENT08 Imputation Flag"
   PARENT09_I = "PARENT09 Imputation Flag"
   PARENT10_I = "PARENT10 Imputation Flag"
   WANTB01_I = "WANTB01 Imputation Flag"
   WANTB02_I = "WANTB02 Imputation Flag"
   WANTB03_I = "WANTB03 Imputation Flag"
   WANTB04_I = "WANTB04 Imputation Flag"
   WANTB05_I = "WANTB05 Imputation Flag"
   WANTB06_I = "WANTB06 Imputation Flag"
   WANTB07_I = "WANTB07 Imputation Flag"
   WANTB08_I = "WANTB08 Imputation Flag"
   WANTB09_I = "WANTB09 Imputation Flag"
   WANTB10_I = "WANTB10 Imputation Flag"
   DADTYPE = "Type of children aged 18 or younger that R has (RECODE)"
   DADTYPU5 = "Type of children under 5 that R has (RECODE)"
   DADTYP518 = "Type of children 5-18 that R has (RECODE)"
   NUMCRU18 = "Number of coresidential children (<= 18 yrs old) (RECODE)"
   NUMNCU18 = "Number of noncoresidential children (< = 18 yrs old) (RECODE)"
   SUPP12MO = "Contribution of child support in last 12 months (RECODE)"
   DADTYPE_I = "DADTYPE Imputation Flag"
   DADTYPU5_I = "DADTYPU5 Imputation Flag"
   DADTYP518_I = "DADTYP518 Imputation Flag"
   NUMCRU18_I = "NUMCRU18 Imputation Flag"
   NUMNCU18_I = "NUMNCU18 Imputation Flag"
   SUPP12MO_I = "SUPP12MO Imputation Flag"
   INTENT = "Intentions for additional births (RECODE)"
   ADDEXP = "Central number of additional births expected (RECODE)"
   INTENT_I = "INTENT Imputation Flag"
   ADDEXP_I = "ADDEXP Imputation Flag"
   CURR_INS = "Current health insurance status (RECODE)"
   INFEVER = "Ever used infertility services (RECODE)"
   EVHIVTST = "Ever had an HIV test (RECODE)"
   CURR_INS_I = "CURR_INS Imputation Flag"
   INFEVER_I = "INFEVER Imputation Flag"
   EVHIVTST_I = "EVHIVTST Imputation Flag"
   METRO = "Place of residence (metropolitan-non-metropolitan) (RECODE)"
   RELIGION = "Current religious affiliation (RECODE)"
   LABORFOR = "Labor force status (RECODE)"
   METRO_I = "METRO Imputation Flag"
   RELIGION_I = "RELIGION Imputation Flag"
   LABORFOR_I = "LABORFOR Imputation Flag"
   POVERTY = "Poverty level income (RECODE)"
   TOTINCR = "Total income of R's family (RECODE)"
   PUBASSIS = "Whether R received public assistance in last calendar year (RECODE)"
   POVERTY_I = "POVERTY Imputation Flag"
   TOTINCR_I = "TOTINCR Imputation Flag"
   PUBASSIS_I = "PUBASSIS Imputation Flag"
   WGT2015_2017 = "Final weight for the 2015-2017 NSFG"
   SECU = "Randomized version of the sampling error computational unit"
   SEST = "Randomized version of the stratum"
   CMINTVW = "Century month for date of interview (Computed in Flow Check A-1)"
   CMLSTYR = "Century month for month/year of interview minus one year (Computed in Flow Check A-1)"
   CMFIVYR = "Century month for month/year of interview minus 5 years (Computed in Flow Check A-1)"
   QUARTER = "Quarter when case was sampled"
   PHASE = "Regular- or double-sample portion of the quarter"
   INTVWYEAR = "Calendar year when interview occurred"
   INTVLNGTH = "Interview Length in Minutes" ;


* SAS FORMAT STATEMENT;

/*
FORMAT
   CASEID         CASEID.
   RSCRNINF       Y1N5C.
   RSCRAGE        AGESCRN.
   RSCRHISP       Y1N5RDF.
   RSCRRACE       RSCRRACE.
   AGE_A          AGEFMT.
   AGE_R          AGEFMT.
   AGESCRN        AGESCRN.
   HISP           Y1N5RDF.
   HISPGRP        HISPGRPF.
   PRIMLANG1      PRIMLANGF.
   PRIMLANG2      PRIMLANGF.
   PRIMLANG3      PRIMLANGF.
   ROSCNT         ROSCNT.
   MARSTAT        MARSTAT.
   FMARSTAT       FMARSTAT.
   FMARIT         FMARITF.
   EVRMARRY       EVRMARRY.
   WPLOCALE       WPLOCALE.
   WOMREL         WOMREL.
   GOSCHOL        Y1N5RDF.
   VACA           Y1N5RDF.
   HIGRADE        HIGRADE.
   COMPGRD        Y1N5RDF.
   DIPGED         DIPGED.
   EARNHS_Y       YEARFMT.
   HISCHGRD       HISCHGRD.
   LSTGRADE       LSTGRADE.
   MYSCHOL_Y      YEARFMT.
   HAVEDEG        Y1N5RDF.
   DEGREES        DEGREES.
   EARNBA_Y       YEARFMT.
   EXPSCHL        Y1N5RDF.
   EXPGRADE       EXPGRADE2F.
   WTHPARNW       WTHPARNW.
   ONOWN          Y1N5RDF.
   ONOWN18        Y1N5RDF.
   INTACT         Y1N5RDF.
   PARMARR        Y1N5RDF.
   INTACT18       Y1N2RDF.
   LVSIT14F       LVSIT14F.
   LVSIT14M       MALFIGF.
   WOMRASDU       WOMRASDU.
   MOMDEGRE       MDDEGRE.
   MOMWORKD       MOMWORKD.
   MOMFSTCH       MOMFSTCH.
   MOM18          MOM18F.
   MANRASDU       MALFIGF.
   R_FOSTER       R_FOSTER.
   EVRFSTER       Y1N5RDF.
   MNYFSTER       MNYFSTER.
   DURFSTER       DURFSTER.
   TIMESMAR       TIMESMAR.
   EVCOHAB1       Y1N5RDF.
   NUMCOH1        NUMCOH2F.
   EVCOHAB2       Y1N5RDF.
   NUMCOH2        NUMCOH2F.
   EVRCOHAB       N0Y1RDF.
   NUMWIFE        NUMWIFE.
   NUMCOHAB       NUMCOHAB.
   EVERSEX        Y1N5RDF.
   RHADSEX        RHADSEX.
   SXMTONCE       Y1N5RDF.
   YNOSEX         YNOSEX.
   TALKPAR1       TALKPARF.
   TALKPAR2       TALKPARF.
   TALKPAR3       TALKPARF.
   TALKPAR4       TALKPARF.
   TALKPAR5       TALKPARF.
   TALKPAR6       TALKPARF.
   TALKPAR7       TALKPARF.
   SEDNO          Y1N5RDF.
   SEDNOG         SEDGRDF.
   SEDNOSX        SEDBFAFF.
   SEDBC          Y1N5RDF.
   SEDBCLC1       SXEDWHRINS.
   SEDBCLC2       SXEDWHRINS.
   SEDBCLC3       SXEDWHRINS.
   SEDBCLC4       SXEDWHRINS.
   SEDBCG         SEDGRDF.
   SEDBCSX        SEDBFAFF.
   SEDWHBC        Y1N5RDF.
   SEDWHBCG       SEDGRDF.
   SEDWBCSX       SEDBFAFF.
   SEDCOND        Y1N5RDF.
   SEDCONDG       SEDGRDF.
   SEDCONSX       SEDBFAFF.
   SEDSTD         Y1N5RDF.
   SEDSTDG        SEDGRDF.
   SEDSTDSX       SEDBFAFF.
   SEDHIV         Y1N5RDF.
   SEDHIVG        SEDGRDF.
   SEDHIVSX       SEDBFAFF.
   SEDABST        Y1N5RDF.
   SEDABLC1       SXEDWHRINS.
   SEDABLC2       SXEDWHRINS.
   SEDABLC3       SXEDWHRINS.
   SEDABLC4       SXEDWHRINS.
   SEDABSTG       SEDGRDF.
   SEDABSSX       SEDBFAFF.
   EVEROPER       Y1N5RDF.
   TYPEOPER       TYPEOPER.
   STEROPER       Y1N5RDF.
   VASEC_Y        YEARFMT.
   PLCSTROP       PLCSTROP.
   RVRSVAS        Y1N5RDF.
   VASREV_Y       YEARFMT.
   RSURGSTR       N0Y1RDF.
   FATHPOSS       Y1N5RDF.
   FATHDIFF       Y1N5RDF.
   RSTRSTAT       RSTRSTAT.
   LIFEPRT        LIFEPRT.
   LIFEPRTS       NUMPARTF.
   SXMON12        Y1N5RDF.
   MON12PRT       MON12PRT.
   MON12PRTS      NUMPARTF.
   SEXSTAT        SEXSTAT.
   P12MOCONO      Y1N5RDF.
   P12MOCON       OFTIMEF.
   SEXFREQ        SEXFREQ.
   CONFREQ        SEXFREQ.
   P1RLTN1        Y1N5RDF.
   P1CURRWIFE     Y1N5RDF.
   P1CURRSEP      Y1N5RDF.
   P1RLTN2        Y1N5RDF.
   P1COHABIT      Y1N5RDF.
   P1SXLAST_M     MNTHFMT.
   P1SXLAST_Y     YEARFMT.
   CMLSXP1        CMFMT.
   P2RLTN1        Y1N5RDF.
   P2CURRWIFE     Y1N5RDF.
   P2CURRSEP      Y1N5RDF.
   P2RLTN2        Y1N5RDF.
   P2COHABIT      Y1N5RDF.
   P2SXLAST_M     MNTHFMT.
   P2SXLAST_Y     YEARFMT.
   CMLSXP2        CMFMT.
   P3RLTN1        Y1N5RDF.
   P3CURRWIFE     Y1N5RDF.
   P3CURRSEP      Y1N5RDF.
   P3RLTN2        Y1N5RDF.
   P3COHABIT      Y1N5RDF.
   P3SXLAST_M     MNTHFMT.
   P3SXLAST_Y     YEARFMT.
   CMLSXP3        CMFMT.
   P1RELATION     PXRELATF.
   P2RELATION     PXRELATF.
   P3RELATION     PXRELATF.
   FIRST          FIRST.
   MARRDATE_Y     YEARFMT.
   HISAGEM        AGESELF.
   LIVTOGWF       Y1N5RDF.
   STRTWFCP_Y     YEARFMT.
   HISAGEC        AGESELF.
   CMSTRTWP       YEARFMT.
   ENGATHEN       ENGAGF.
   WILLMARR       DEFPROBF.
   CWPDOB_Y       YEARFMT.
   CWPAGE         AGEWFPRT2F.
   CWPRACE        RACEFMT.
   CWPNRACE       NRACE.
   CWPEDUCN       EDUCNF.
   CWPBORN        Y1N5RDF.
   CWPMARBF       Y1N5RDF.
   CWPSX1WN_M     MNTHFMT.
   CWPSX1WN_Y     YEARFMT.
   CWPSX1AG       AGEWFPRT2F.
   CMFSXCWP       CMFMT.
   AGEFSXCWP      AGEWFPRT2F.
   CWPSX1RL       RLTNFMT.
   CWPFUSE        Y1N5RDF.
   CWPFMET01      MTHDSV1F.
   CWPFMET02      MTHDSV1F.
   CWPFMET03      MTHDSV1F.
   CWPFMET04      MTHDSV1F.
   CWPFMET05      MTHDSV1F.
   CWPOPSTR       Y1N5RDF.
   CWPTYPOP1      CWPTYPOPF.
   CWPTYPOP2      CWPTYPOPF.
   CWPTOTST       Y1N5RDF.
   CWPREVST       Y1N5RDF.
   PSURGSTR       N0Y1CF.
   CWPPOSS        Y1N5RDF.
   CWPDIFF        Y1N5RDF.
   PSTRSTAT       PSTRSTAT.
   CWPLSXWN_M     MNTHFMT.
   CWPLSXWN_Y     YEARFMT.
   CMLSXCWP       CMFMT.
   CWPLUSE1       Y1N5RDF.
   CWPLMET11      MTHDSV2F.
   CWPLMET12      MTHDSV2F.
   CWPLMET13      MTHDSV2F.
   CWPLUSE2       Y1N5RDF.
   DKCWPLUSE      RECNEVKNF.
   CWPLMET201     MTHDSV3F.
   CWPLMET202     MTHDSV3F.
   DKCWPLMET      RECNEVKNF.
   CWPLSXUSE      LSXUSEPF.
   CWPRECBC       Y1N5RDF.
   CWPALLBC01     MTHDSV1F.
   CWPALLBC02     MTHDSV1F.
   CWPALLBC03     MTHDSV1F.
   CWPALLBC04     MTHDSV1F.
   CWPALLBC05     MTHDSV1F.
   CWPBCMST       MTHDSV1F.
   CONDFREQ       CONDPCT.
   CWPNOFRQ       OFTIMEF.
   CWPBIOKD       Y1N5RDF.
   CWPNUMKD       EVRCHL3F.
   PARTFATH       N0Y1CF.
   CWPCHSEX       MALFEMF.
   CWPCHDOB_Y     YEARFMT.
   CWPCHMAR       Y1N5RDF.
   CWPCHRES       Y1N5RDF.
   CWPCHLRN       LRNPRGF.
   CWPCHLIV1      CWPCHLIVF.
   CWPCHLIV2      CWPCHLIVF.
   CWPCHAGE       AGEGPRW.
   CWPCHSIG       Y1N5RDF.
   CWPCHCRT       Y1N5RDF.
   CWPCHGEN       Y1N5RDF.
   CWPCHEVR       Y1N5RDF.
   CWPCHFAR       MILESFMT.
   CWPCHWNT       DEFPROBF.
   CWPCHSON       TIMINGF.
   CWPSOONN       SOONNVF.
   CWPSOONMY      MONYRF.
   CWPCHHPY       SCALEF.
   CWPCHSEX2      MALFEMF.
   CWPCHDOB_Y2    YEARFMT.
   MULTBIRT2      Y1N5RDF.
   CWPCHMAR2      Y1N5RDF.
   CWPCHRES2      Y1N5RDF.
   CWPCHLRN2      LRNPRGF.
   CWPCHLIV10     CWPCHLIVF.
   CWPCHLIV11     CWPCHLIVF.
   CWPCHAGE2      AGEGPRW.
   CWPCHSIG2      Y1N5RDF.
   CWPCHCRT2      Y1N5RDF.
   CWPCHGEN2      Y1N5RDF.
   CWPCHEVR2      Y1N5RDF.
   CWPCHFAR2      MILESFMT.
   CWPCHWNT2      DEFPROBF.
   CWPCHSON2      TIMINGF.
   CWPSOONN2      SOONNVF.
   CWPSOONMY2     MONYRF.
   CWPCHHPY2      SCALEF.
   CWPCHSEX3      MALFEMF.
   CWPCHDOB_Y3    YEARFMT.
   MULTBIRT3      Y1N5RDF.
   CWPCHMAR3      Y1N5RDF.
   CWPCHRES3      Y1N5RDF.
   CWPCHLRN3      LRNPRGF.
   CWPCHLIV19     CWPCHLIVF.
   CWPCHLIV20     CWPCHLIVF.
   CWPCHAGE3      AGEGPRW.
   CWPCHSIG3      Y1N5RDF.
   CWPCHCRT3      Y1N5RDF.
   CWPCHGEN3      Y1N5RDF.
   CWPCHEVR3      Y1N5RDF.
   CWPCHFAR3      MILESFMT.
   CWPCHWNT3      DEFPROBF.
   CWPCHSON3      TIMINGF.
   CWPSOONN3      SOONNVF.
   CWPSOONMY3     MONYRF.
   CWPCHHPY3      SCALEF.
   CWPCHSEX4      MALFEMF.
   CWPCHDOB_Y4    YEARFMT.
   MULTBIRT4      Y1N5RDF.
   CWPCHMAR4      Y1N5RDF.
   CWPCHRES4      Y1N5RDF.
   CWPCHLRN4      LRNPRGF.
   CWPCHLIV28     CWPCHLIVF.
   CWPCHLIV29     CWPCHLIVF.
   CWPCHAGE4      AGEGPRW.
   CWPCHSIG4      Y1N5RDF.
   CWPCHCRT4      Y1N5RDF.
   CWPCHGEN4      Y1N5RDF.
   CWPCHEVR4      Y1N5RDF.
   CWPCHFAR4      MILESFMT.
   CWPCHWNT4      DEFPROBF.
   CWPCHSON4      TIMINGF.
   CWPSOONN4      SOONNVF.
   CWPSOONMY4     MONYRF.
   CWPCHHPY4      SCALEF.
   CWPCHSEX5      MALFEMF.
   CWPCHDOB_Y5    YEARFMT.
   MULTBIRT5      Y1N5RDF.
   CWPCHMAR5      Y1N5RDF.
   CWPCHRES5      Y1N5RDF.
   CWPCHLRN5      LRNPRGF.
   CWPCHLIV37     CWPCHLIVF.
   CWPCHLIV38     CWPCHLIVF.
   CWPCHAGE5      AGEGPRW.
   CWPCHSIG5      Y1N5RDF.
   CWPCHCRT5      Y1N5RDF.
   CWPCHGEN5      Y1N5RDF.
   CWPCHEVR5      Y1N5RDF.
   CWPCHFAR5      MILESFMT.
   CWPCHWNT5      DEFPROBF.
   CWPCHSON5      TIMINGF.
   CWPSOONN5      SOONNVF.
   CWPSOONMY5     MONYRF.
   CWPCHHPY5      SCALEF.
   CWPCHSEX6      MALFEMF.
   CWPCHDOB_Y6    YEARFMT.
   MULTBIRT6      Y1N5RDF.
   CWPCHMAR6      Y1N5RDF.
   CWPCHRES6      Y1N5RDF.
   CWPCHLRN6      LRNPRGF.
   CWPCHLIV46     CWPCHLIVF.
   CWPCHLIV47     CWPCHLIVF.
   CWPCHAGE6      AGEGPRW.
   CWPCHSIG6      Y1N5RDF.
   CWPCHCRT6      Y1N5RDF.
   CWPCHGEN6      Y1N5RDF.
   CWPCHEVR6      Y1N5RDF.
   CWPCHFAR6      MILESFMT.
   CWPCHWNT6      DEFPROBF.
   CWPCHSON6      TIMINGF.
   CWPSOONN6      SOONNVF.
   CWPSOONMY6     MONYRF.
   CWPCHHPY6      SCALEF.
   CWPCHSEX7      MALFEMF.
   CWPCHDOB_Y7    YEARFMT.
   MULTBIRT7      Y1N5RDF.
   CWPCHMAR7      Y1N5RDF.
   CWPCHRES7      Y1N5RDF.
   CWPCHLRN7      LRNPRGF.
   CWPCHLIV55     CWPCHLIVF.
   CWPCHLIV56     CWPCHLIVF.
   CWPCHAGE7      AGEGPRW.
   CWPCHSIG7      Y1N5RDF.
   CWPCHCRT7      Y1N5RDF.
   CWPCHGEN7      Y1N5RDF.
   CWPCHEVR7      Y1N5RDF.
   CWPCHFAR7      MILESFMT.
   CWPCHWNT7      DEFPROBF.
   CWPCHSON7      TIMINGF.
   CWPSOONN7      SOONNVF.
   CWPSOONMY7     MONYRF.
   CWPCHHPY7      SCALEF.
   CWPPRGNW       Y1N5RDF.
   CWPTRYPG       Y1N5RDF.
   CWPTRYLG       CWPTRYLG.
   CWPCPWNT       DEFPROBF.
   CWPCPSON       TIMINGF.
   CWPCPSNN       SOONNVF.
   CWPCPSNMY      MONYRF.
   CWPCPHPY       SCALEF.
   C_OKAKIDS      AKIDNONE.
   CWPOTKID       Y1N5RDF.
   CWPOKNUM       AKIDNONE.
   CWPOKWTH       Y1N5RDF.
   CWPOKWTHN      AKIDNONE.
   CWPOKSEX       MALFEMF.
   CWPOKAD        ADPTGUARF.
   CWPOKTRY       Y1N5RDF.
   CWPOKTHR       ADPTGUAR2F.
   CWPOKLIV1      CWPNBLIVF.
   CWPOKLIV2      CWPNBLIVF.
   CWPOKFAR       MILESFMT.
   CWPOKAGE       KIDAGEF2D.
   CWPOKSEX2      MALFEMF.
   CWPOKAD2       ADPTGUARF.
   CWPOKTRY2      Y1N5RDF.
   CWPOKTHR2      ADPTGUAR2F.
   CWPOKLIV8      CWPNBLIVF.
   CWPOKLIV9      CWPNBLIVF.
   CWPOKFAR2      MILESFMT.
   CWPOKAGE2      KIDAGEF2D.
   CWPOKSEX3      MALFEMF.
   CWPOKAD3       ADPTGUARF.
   CWPOKTRY3      Y1N5RDF.
   CWPOKTHR3      ADPTGUAR2F.
   CWPOKLIV15     CWPNBLIVF.
   CWPOKLIV16     CWPNBLIVF.
   CWPOKFAR3      MILESFMT.
   CWPOKAGE3      KIDAGEF2D.
   CWPOKSEX4      MALFEMF.
   CWPOKAD4       ADPTGUARF.
   CWPOKTRY4      Y1N5RDF.
   CWPOKTHR4      ADPTGUAR2F.
   CWPOKLIV22     CWPNBLIVF.
   CWPOKLIV23     CWPNBLIVF.
   CWPOKFAR4      MILESFMT.
   CWPOKAGE4      KIDAGEF2D.
   CWPOKSEX5      MALFEMF.
   CWPOKAD5       ADPTGUARF.
   CWPOKTRY5      Y1N5RDF.
   CWPOKTHR5      ADPTGUAR2F.
   CWPOKLIV29     CWPNBLIVF.
   CWPOKLIV30     CWPNBLIVF.
   CWPOKFAR5      MILESFMT.
   CWPOKAGE5      KIDAGEF2D.
   C_NBAKIDS      AKIDNONE.
   CWPNBEVR       Y1N5RDF.
   CWPNBNUM       AKIDNONE.
   CWPNBREL       CWPNBRELF.
   CWPNBFOS       Y1N5RDF.
   CWPNBSEX       MALFEMF.
   CWPNBAD        ADPTGUARF.
   CWPNBTRY       Y1N5RDF.
   CWPNBTHR       ADPTGUAR2F.
   CWPNBLIV1      CWPNBLIVF.
   CWPNBLIV2      CWPNBLIVF.
   CWPNBLIV3      CWPNBLIVF.
   CWPNBFAR       MILESFMT.
   CWPNBAGE       KIDAGEF2D.
   CWPNBREL2      CWPNBRELF.
   CWPNBFOS2      Y1N5RDF.
   CWPNBSEX2      MALFEMF.
   CWPNBAD2       ADPTGUARF.
   CWPNBTRY2      Y1N5RDF.
   CWPNBTHR2      ADPTGUAR2F.
   CWPNBLIV8      CWPNBLIVF.
   CWPNBLIV9      CWPNBLIVF.
   CWPNBLIV10     CWPNBLIVF.
   CWPNBFAR2      MILESFMT.
   CWPNBAGE2      KIDAGEF2D.
   CWPNBREL3      CWPNBRELF.
   CWPNBFOS3      Y1N5RDF.
   CWPNBSEX3      MALFEMF.
   CWPNBAD3       ADPTGUARF.
   CWPNBTRY3      Y1N5RDF.
   CWPNBTHR3      ADPTGUAR2F.
   CWPNBLIV15     CWPNBLIVF.
   CWPNBLIV16     CWPNBLIVF.
   CWPNBLIV17     CWPNBLIVF.
   CWPNBFAR3      MILESFMT.
   CWPNBAGE3      KIDAGEF2D.
   CWPNBREL4      CWPNBRELF.
   CWPNBFOS4      Y1N5RDF.
   CWPNBSEX4      MALFEMF.
   CWPNBAD4       ADPTGUARF.
   CWPNBTRY4      Y1N5RDF.
   CWPNBTHR4      ADPTGUAR2F.
   CWPNBLIV22     CWPNBLIVF.
   CWPNBLIV23     CWPNBLIVF.
   CWPNBLIV24     CWPNBLIVF.
   CWPNBFAR4      MILESFMT.
   CWPNBAGE4      KIDAGEF2D.
   CWPNBREL5      CWPNBRELF.
   CWPNBFOS5      Y1N5RDF.
   CWPNBSEX5      MALFEMF.
   CWPNBAD5       ADPTGUARF.
   CWPNBTRY5      Y1N5RDF.
   CWPNBTHR5      ADPTGUAR2F.
   CWPNBLIV29     CWPNBLIVF.
   CWPNBLIV30     CWPNBLIVF.
   CWPNBLIV31     CWPNBLIVF.
   CWPNBFAR5      MILESFMT.
   CWPNBAGE5      KIDAGEF2D.
   CWPNBREL6      CWPNBRELF.
   CWPNBFOS6      Y1N5RDF.
   CWPNBSEX6      MALFEMF.
   CWPNBAD6       ADPTGUARF.
   CWPNBTRY6      Y1N5RDF.
   CWPNBTHR6      ADPTGUAR2F.
   CWPNBLIV36     CWPNBLIVF.
   CWPNBLIV37     CWPNBLIVF.
   CWPNBLIV38     CWPNBLIVF.
   CWPNBFAR6      MILESFMT.
   CWPNBAGE6      KIDAGEF2D.
   CWPNBREL7      CWPNBRELF.
   CWPNBFOS7      Y1N5RDF.
   CWPNBSEX7      MALFEMF.
   CWPNBAD7       ADPTGUARF.
   CWPNBTRY7      Y1N5RDF.
   CWPNBTHR7      ADPTGUAR2F.
   CWPNBLIV43     CWPNBLIVF.
   CWPNBLIV44     CWPNBLIVF.
   CWPNBLIV45     CWPNBLIVF.
   CWPNBFAR7      MILESFMT.
   CWPNBAGE7      KIDAGEF2D.
   CWPNBREL8      CWPNBRELF.
   CWPNBFOS8      Y1N5RDF.
   CWPNBSEX8      MALFEMF.
   CWPNBAD8       ADPTGUARF.
   CWPNBTRY8      Y1N5RDF.
   CWPNBTHR8      ADPTGUAR2F.
   CWPNBLIV50     CWPNBLIVF.
   CWPNBLIV51     CWPNBLIVF.
   CWPNBLIV52     CWPNBLIVF.
   CWPNBFAR8      MILESFMT.
   CWPNBAGE8      KIDAGEF2D.
   CWPNBREL9      CWPNBRELF.
   CWPNBFOS9      Y1N5RDF.
   CWPNBSEX9      MALFEMF.
   CWPNBAD9       ADPTGUARF.
   CWPNBTRY9      Y1N5RDF.
   CWPNBTHR9      ADPTGUAR2F.
   CWPNBLIV57     CWPNBLIVF.
   CWPNBLIV58     CWPNBLIVF.
   CWPNBLIV59     CWPNBLIVF.
   CWPNBFAR9      MILESFMT.
   CWPNBAGE9      KIDAGEF2D.
   CWPNBREL10     CWPNBRELF.
   CWPNBFOS10     Y1N5RDF.
   CWPNBSEX10     MALFEMF.
   CWPNBAD10      ADPTGUARF.
   CWPNBTRY10     Y1N5RDF.
   CWPNBTHR10     ADPTGUAR2F.
   CWPNBLIV64     CWPNBLIVF.
   CWPNBLIV65     CWPNBLIVF.
   CWPNBLIV66     CWPNBLIVF.
   CWPNBFAR10     MILESFMT.
   CWPNBAGE10     KIDAGEF2D.
   MARDATEN_Y     YEARFMT.
   AGEMARR        AGESELF.
   LIVTOGN        Y1N5RDF.
   STRTLIVE_Y     YEARFMT.
   AGELIV         AGESELF.
   CMUNIONP       YEARFMT.
   ENGAGTHN       ENGAGF.
   MARREND        MARRENDF.
   WIFEDIED_Y     YEARFMT.
   DIVORFIN_Y     YEARFMT.
   ANNULLED_Y     YEARFMT.
   STOPLIVE_Y     YEARFMT.
   MARDATEN_Y2    YEARFMT.
   AGEMARR2       AGESELF.
   LIVTOGN2       Y1N5RDF.
   STRTLIVE_Y2    YEARFMT.
   AGELIV2        AGESELF.
   ENGAGTHN2      ENGAGF.
   MARREND2       MARRENDF.
   WIFEDIED_Y2    YEARFMT.
   DIVORFIN_Y2    YEARFMT.
   ANNULLED_Y2    YEARFMT.
   STOPLIVE_Y2    YEARFMT.
   MARDATEN_Y3    YEARFMT.
   AGEMARR3       AGESELF.
   LIVTOGN3       Y1N5RDF.
   STRTLIVE_Y3    YEARFMT.
   AGELIV3        AGESELF.
   ENGAGTHN3      ENGAGF.
   MARREND3       MARRENDF.
   WIFEDIED_Y3    YEARFMT.
   DIVORFIN_Y3    YEARFMT.
   ANNULLED_Y3    YEARFMT.
   STOPLIVE_Y3    YEARFMT.
   CURRPRTS       CURRPRTF.
   PXCURR         Y1N5RDF.
   PXCURRPRT      N0Y1RDF.
   PXMARRY        DEFPROBF.
   PXCURR2        Y1N5RDF.
   PXCURRPRT2     N0Y1RDF.
   PXMARRY2       DEFPROBF.
   PXCURR3        Y1N5RDF.
   PXCURRPRT3     N0Y1RDF.
   PXMARRY3       DEFPROBF.
   PXLRUSE        Y1N5RDF.
   PXLRMETH1      MTHDSV2F.
   PXLRMETH2      MTHDSV2F.
   PXLRMETH3      MTHDSV2F.
   PXLPUSE        Y1N5RDF.
   DKPXLPUSE      RECNEVKNF.
   PXLPMETH01     MTHDSV3F.
   PXLPMETH02     MTHDSV3F.
   PXLPMETH03     MTHDSV3F.
   DKPXLPMETH     RECNEVKNF.
   LSXUSEP        LSXUSEPF.
   MTONCEP        MTONCEPF.
   PXLSXPRB       Y1N5RDF.
   PXMTONCE       Y1N5RDF.
   PXFRLTN1       RLTNFMT.
   P1YRACE1       RACEFMT.
   P1YNRACE1      NRACE.
   PXLRUSE2       Y1N5RDF.
   PXLRMETH5      MTHDSV2F.
   PXLRMETH6      MTHDSV2F.
   PXLRMETH7      MTHDSV2F.
   PXLPUSE2       Y1N5RDF.
   DKPXLPUSE2     RECNEVKNF.
   PXLPMETH11     MTHDSV3F.
   PXLPMETH12     MTHDSV3F.
   PXLPMETH13     MTHDSV3F.
   DKPXLPMETH2    RECNEVKNF.
   LSXUSEP2       LSXUSEPF.
   MTONCEP2       MTONCEPF.
   PXLSXPRB2      Y1N5RDF.
   PXMTONCE2      Y1N5RDF.
   PXFRLTN3       RLTNFMT.
   P1YRACE2       RACEFMT.
   P1YNRACE2      NRACE.
   PXLRUSE3       Y1N5RDF.
   PXLRMETH9      MTHDSV2F.
   PXLRMETH10     MTHDSV2F.
   PXLRMETH11     MTHDSV2F.
   PXLPUSE3       Y1N5RDF.
   DKPXLPUSE3     RECNEVKNF.
   PXLPMETH21     MTHDSV3F.
   PXLPMETH22     MTHDSV3F.
   PXLPMETH23     MTHDSV3F.
   DKPXLPMETH3    RECNEVKNF.
   LSXUSEP3       LSXUSEPF.
   MTONCEP3       MTONCEPF.
   PXLSXPRB3      Y1N5RDF.
   PXMTONCE3      Y1N5RDF.
   PXFRLTN5       RLTNFMT.
   P1YRACE3       RACEFMT.
   P1YNRACE3      NRACE.
   PXDOB_Y        YEARFMT.
   PXEDUC         EDUCNF.
   PXMARBF        Y1N5RDF.
   PXANYCH        Y1N5RDF.
   PXANYCHN       PXANYCHN.
   PXABLECH       Y1N5RDF.
   PXDOB_Y2       YEARFMT.
   PXEDUC2        EDUCNF.
   PXMARBF2       Y1N5RDF.
   PXANYCH2       Y1N5RDF.
   PXANYCHN2      PXANYCHN.
   PXABLECH2      Y1N5RDF.
   PXDOB_Y3       YEARFMT.
   PXEDUC3        EDUCNF.
   PXMARBF3       Y1N5RDF.
   PXANYCH3       Y1N5RDF.
   PXANYCHN3      PXANYCHN.
   PXABLECH3      Y1N5RDF.
   PXSXFRST_M     MNTHFMT.
   PXSXFRST_Y     YEARFMT.
   CMFSXP         CMFMT.
   AGEFSXP        AGE2SELF.
   PXAGFRST       SEX1AGEF.
   PXFRLTN2       RLTNFMT.
   PXFUSE         Y1N5RDF.
   PXFMETH01      MTHDSV1F.
   PXFMETH02      MTHDSV1F.
   PXFMETH03      MTHDSV1F.
   PXFMETH04      MTHDSV1F.
   PXSXFRST_M2    MNTHFMT.
   PXSXFRST_Y2    YEARFMT.
   CMFSXP2        CMFMT.
   AGEFSXP2       AGE2SELF.
   PXAGFRST2      SEX1AGEF.
   PXFRLTN4       RLTNFMT.
   PXFUSE2        Y1N5RDF.
   PXFMETH14      MTHDSV1F.
   PXFMETH15      MTHDSV1F.
   PXFMETH16      MTHDSV1F.
   PXFMETH17      MTHDSV1F.
   PXSXFRST_M3    MNTHFMT.
   PXSXFRST_Y3    YEARFMT.
   CMFSXP3        CMFMT.
   AGEFSXP3       AGE2SELF.
   PXAGFRST3      SEX1AGEF.
   PXFRLTN6       RLTNFMT.
   PXFUSE3        Y1N5RDF.
   PXFMETH27      MTHDSV1F.
   PXFMETH28      MTHDSV1F.
   PXFMETH29      MTHDSV1F.
   PXFMETH30      MTHDSV1F.
   PXANYUSE       Y1N5RDF.
   PXMETHOD01     MTHDSV1F.
   PXMETHOD02     MTHDSV1F.
   PXMETHOD03     MTHDSV1F.
   PXMETHOD04     MTHDSV1F.
   PXMETHOD05     MTHDSV1F.
   PXMSTUSE       MTHDSV1F.
   PXCONFRQ       CONDPCT.
   PXNOFREQ       OFTIMEF.
   PXANYUSE2      Y1N5RDF.
   PXMETHOD14     MTHDSV1F.
   PXMETHOD15     MTHDSV1F.
   PXMETHOD16     MTHDSV1F.
   PXMETHOD17     MTHDSV1F.
   PXMETHOD18     MTHDSV1F.
   PXMSTUSE2      MTHDSV1F.
   PXCONFRQ2      CONDPCT.
   PXNOFREQ2      OFTIMEF.
   PXANYUSE3      Y1N5RDF.
   PXMETHOD27     MTHDSV1F.
   PXMETHOD28     MTHDSV1F.
   PXMETHOD29     MTHDSV1F.
   PXMETHOD30     MTHDSV1F.
   PXMETHOD31     MTHDSV1F.
   PXMSTUSE3      MTHDSV1F.
   PXCONFRQ3      CONDPCT.
   PXNOFREQ3      OFTIMEF.
   PXCHILD        Y1N5RDF.
   PXCHILDN       PXCHILDN.
   PXCXSEX        MALFEMF.
   PXCXBORN_Y     YEARFMT.
   MULTBIRT11     Y1N5RDF.
   PXCXMARB       Y1N5RDF.
   PXCXRES        Y1N5RDF.
   PXCXKNOW       LRNPRGF.
   PXCXLIV01      BIOCHLIV1F.
   PXCXLIV02      BIOCHLIV1F.
   PXCXLIV03      BIOCHLIV1F.
   PXCXAGE        AGEGPRW.
   PXCXSIG        Y1N5RDF.
   PXCXCRT        Y1N5RDF.
   PXCXGEN        Y1N5RDF.
   PXCXEVER       Y1N5RDF.
   PXCXFAR        MILESFMT.
   PXWANT         DEFPROBF.
   PXSOON         TIMINGF.
   PXSOONN        SOONNVF.
   PXSOONMY       MONYRF.
   PXHPYPG        SCALEF.
   PXCXSEX2       MALFEMF.
   PXCXBORN_Y2    YEARFMT.
   MULTBIRT12     Y1N5RDF.
   PXCXMARB2      Y1N5RDF.
   PXCXRES2       Y1N5RDF.
   PXCXKNOW2      LRNPRGF.
   PXCXLIV11      BIOCHLIV1F.
   PXCXLIV12      BIOCHLIV1F.
   PXCXLIV13      BIOCHLIV1F.
   PXCXAGE2       AGEGPRW.
   PXCXSIG2       Y1N5RDF.
   PXCXCRT2       Y1N5RDF.
   PXCXGEN2       Y1N5RDF.
   PXCXEVER2      Y1N5RDF.
   PXCXFAR2       MILESFMT.
   PXWANT2        DEFPROBF.
   PXSOON2        TIMINGF.
   PXSOONN2       SOONNVF.
   PXSOONMY2      MONYRF.
   PXHPYPG2       SCALEF.
   PXCXSEX3       MALFEMF.
   PXCXBORN_Y3    YEARFMT.
   MULTBIRT13     Y1N5RDF.
   PXCXMARB3      Y1N5RDF.
   PXCXRES3       Y1N5RDF.
   PXCXKNOW3      LRNPRGF.
   PXCXLIV21      BIOCHLIV1F.
   PXCXLIV22      BIOCHLIV1F.
   PXCXLIV23      BIOCHLIV1F.
   PXCXAGE3       AGEGPRW.
   PXCXSIG3       Y1N5RDF.
   PXCXCRT3       Y1N5RDF.
   PXCXGEN3       Y1N5RDF.
   PXCXEVER3      Y1N5RDF.
   PXCXFAR3       MILESFMT.
   PXWANT3        DEFPROBF.
   PXSOON3        TIMINGF.
   PXSOONN3       SOONNVF.
   PXSOONMY3      MONYRF.
   PXHPYPG3       SCALEF.
   PXCXSEX4       MALFEMF.
   PXCXBORN_Y4    YEARFMT.
   MULTBIRT14     Y1N5RDF.
   PXCXMARB4      Y1N5RDF.
   PXCXRES4       Y1N5RDF.
   PXCXKNOW4      LRNPRGF.
   PXCXLIV31      BIOCHLIV1F.
   PXCXLIV32      BIOCHLIV1F.
   PXCXLIV33      BIOCHLIV1F.
   PXCXAGE4       AGEGPRW.
   PXCXSIG4       Y1N5RDF.
   PXCXCRT4       Y1N5RDF.
   PXCXGEN4       Y1N5RDF.
   PXCXEVER4      Y1N5RDF.
   PXCXFAR4       MILESFMT.
   PXWANT4        DEFPROBF.
   PXSOON4        TIMINGF.
   PXSOONN4       SOONNVF.
   PXSOONMY4      MONYRF.
   PXHPYPG4       SCALEF.
   PXCHILD2       Y1N5RDF.
   PXCHILDN2      PXCHILDN.
   PXCXSEX11      MALFEMF.
   PXCXBORN_Y11   YEARFMT.
   MULTBIRT21     Y1N5RDF.
   PXCXMARB11     Y1N5RDF.
   PXCXRES11      Y1N5RDF.
   PXCXKNOW11     LRNPRGF.
   PXCXLIV101     BIOCHLIV1F.
   PXCXLIV102     BIOCHLIV1F.
   PXCXAGE11      AGEGPRW.
   PXCXSIG11      Y1N5RDF.
   PXCXCRT11      Y1N5RDF.
   PXCXGEN11      Y1N5RDF.
   PXCXEVER11     Y1N5RDF.
   PXCXFAR11      MILESFMT.
   PXWANT11       DEFPROBF.
   PXSOON11       TIMINGF.
   PXSOONN11      SOONNVF.
   PXSOONMY11     MONYRF.
   PXHPYPG11      SCALEF.
   PXCXSEX12      MALFEMF.
   PXCXBORN_Y12   YEARFMT.
   MULTBIRT22     Y1N5RDF.
   PXCXMARB12     Y1N5RDF.
   PXCXRES12      Y1N5RDF.
   PXCXKNOW12     LRNPRGF.
   PXCXLIV111     BIOCHLIV1F.
   PXCXLIV112     BIOCHLIV1F.
   PXCXAGE12      AGEGPRW.
   PXCXSIG12      Y1N5RDF.
   PXCXCRT12      Y1N5RDF.
   PXCXGEN12      Y1N5RDF.
   PXCXEVER12     Y1N5RDF.
   PXCXFAR12      MILESFMT.
   PXWANT12       DEFPROBF.
   PXSOON12       TIMINGF.
   PXSOONN12      SOONNVF.
   PXSOONMY12     MONYRF.
   PXHPYPG12      SCALEF.
   PXCXSEX13      MALFEMF.
   PXCXBORN_Y13   YEARFMT.
   MULTBIRT23     Y1N5RDF.
   PXCXMARB13     Y1N5RDF.
   PXCXRES13      Y1N5RDF.
   PXCXKNOW13     LRNPRGF.
   PXCXLIV121     BIOCHLIV1F.
   PXCXLIV122     BIOCHLIV1F.
   PXCXAGE13      AGEGPRW.
   PXCXSIG13      Y1N5RDF.
   PXCXCRT13      Y1N5RDF.
   PXCXGEN13      Y1N5RDF.
   PXCXEVER13     Y1N5RDF.
   PXCXFAR13      MILESFMT.
   PXWANT13       DEFPROBF.
   PXSOON13       TIMINGF.
   PXSOONN13      SOONNVF.
   PXSOONMY13     MONYRF.
   PXHPYPG13      SCALEF.
   PXCXSEX14      MALFEMF.
   PXCXBORN_Y14   YEARFMT.
   MULTBIRT24     Y1N5RDF.
   PXCXMARB14     Y1N5RDF.
   PXCXRES14      Y1N5RDF.
   PXCXKNOW14     LRNPRGF.
   PXCXLIV131     BIOCHLIV1F.
   PXCXLIV132     BIOCHLIV1F.
   PXCXAGE14      AGEGPRW.
   PXCXSIG14      Y1N5RDF.
   PXCXCRT14      Y1N5RDF.
   PXCXGEN14      Y1N5RDF.
   PXCXEVER14     Y1N5RDF.
   PXCXFAR14      MILESFMT.
   PXWANT14       DEFPROBF.
   PXSOON14       TIMINGF.
   PXSOONN14      SOONNVF.
   PXSOONMY14     MONYRF.
   PXHPYPG14      SCALEF.
   PXCHILD3       Y1N5RDF.
   PXCHILDN3      PXCHILDN.
   PXCXSEX21      MALFEMF.
   PXCXBORN_Y21   YEARFMT.
   MULTBIRT31     Y1N5RDF.
   PXCXMARB21     Y1N5RDF.
   PXCXRES21      Y1N5RDF.
   PXCXKNOW21     LRNPRGF.
   PXCXLIV201     BIOCHLIV1F.
   PXCXLIV202     BIOCHLIV1F.
   PXCXAGE21      AGEGPRW.
   PXCXSIG21      Y1N5RDF.
   PXCXCRT21      Y1N5RDF.
   PXCXGEN21      Y1N5RDF.
   PXCXEVER21     Y1N5RDF.
   PXCXFAR21      MILESFMT.
   PXWANT21       DEFPROBF.
   PXSOON21       TIMINGF.
   PXSOONN21      SOONNVF.
   PXSOONMY21     MONYRF.
   PXHPYPG21      SCALEF.
   PXCXSEX22      MALFEMF.
   PXCXBORN_Y22   YEARFMT.
   MULTBIRT32     Y1N5RDF.
   PXCXMARB22     Y1N5RDF.
   PXCXRES22      Y1N5RDF.
   PXCXKNOW22     LRNPRGF.
   PXCXLIV211     BIOCHLIV1F.
   PXCXLIV212     BIOCHLIV1F.
   PXCXAGE22      AGEGPRW.
   PXCXSIG22      Y1N5RDF.
   PXCXCRT22      Y1N5RDF.
   PXCXGEN22      Y1N5RDF.
   PXCXEVER22     Y1N5RDF.
   PXCXFAR22      MILESFMT.
   PXWANT22       DEFPROBF.
   PXSOON22       TIMINGF.
   PXSOONN22      SOONNVF.
   PXSOONMY22     MONYRF.
   PXHPYPG22      SCALEF.
   PXCXSEX23      MALFEMF.
   PXCXBORN_Y23   YEARFMT.
   MULTBIRT33     Y1N5RDF.
   PXCXMARB23     Y1N5RDF.
   PXCXRES23      Y1N5RDF.
   PXCXKNOW23     LRNPRGF.
   PXCXLIV221     BIOCHLIV1F.
   PXCXLIV222     BIOCHLIV1F.
   PXCXAGE23      AGEGPRW.
   PXCXSIG23      Y1N5RDF.
   PXCXCRT23      Y1N5RDF.
   PXCXGEN23      Y1N5RDF.
   PXCXEVER23     Y1N5RDF.
   PXCXFAR23      MILESFMT.
   PXWANT23       DEFPROBF.
   PXSOON23       TIMINGF.
   PXSOONN23      SOONNVF.
   PXSOONMY23     MONYRF.
   PXHPYPG23      SCALEF.
   PXCXSEX24      MALFEMF.
   PXCXBORN_Y24   YEARFMT.
   MULTBIRT34     Y1N5RDF.
   PXCXMARB24     Y1N5RDF.
   PXCXRES24      Y1N5RDF.
   PXCXKNOW24     LRNPRGF.
   PXCXLIV231     BIOCHLIV1F.
   PXCXLIV232     BIOCHLIV1F.
   PXCXAGE24      AGEGPRW.
   PXCXSIG24      Y1N5RDF.
   PXCXCRT24      Y1N5RDF.
   PXCXGEN24      Y1N5RDF.
   PXCXEVER24     Y1N5RDF.
   PXCXFAR24      MILESFMT.
   PXWANT24       DEFPROBF.
   PXSOON24       TIMINGF.
   PXSOONN24      SOONNVF.
   PXSOONMY24     MONYRF.
   PXHPYPG24      SCALEF.
   PXCXSEX25      MALFEMF.
   PXCXBORN_Y25   YEARFMT.
   MULTBIRT35     Y1N5RDF.
   PXCXMARB25     Y1N5RDF.
   PXCXRES25      Y1N5RDF.
   PXCXKNOW25     LRNPRGF.
   PXCXLIV241     BIOCHLIV1F.
   PXCXLIV242     BIOCHLIV1F.
   PXCXAGE25      AGEGPRW.
   PXCXSIG25      Y1N5RDF.
   PXCXCRT25      Y1N5RDF.
   PXCXGEN25      Y1N5RDF.
   PXCXEVER25     Y1N5RDF.
   PXCXFAR25      MILESFMT.
   PXWANT25       DEFPROBF.
   PXSOON25       TIMINGF.
   PXSOONN25      SOONNVF.
   PXSOONMY25     MONYRF.
   PXHPYPG25      SCALEF.
   PXCPREG        Y1N5RDF.
   PXTRYING       Y1N5RDF.
   PXTRYLONG      TRYLONG.
   PXRWANT        DEFPROBF.
   PXRSOON        TIMINGF.
   PXRSOONN       SOONNVF.
   PXRSOONMY      MONYRF.
   PXCPFEEL       SCALEF.
   PXCPREG2       Y1N5RDF.
   PXTRYING2      Y1N5RDF.
   PXTRYLONG2     TRYLONG.
   PXRWANT2       DEFPROBF.
   PXRSOON2       TIMINGF.
   PXRSOONN2      SOONNVF.
   PXRSOONMY2     MONYRF.
   PXCPFEEL2      SCALEF.
   PXCPREG3       Y1N5RDF.
   PXTRYING3      Y1N5RDF.
   PXTRYLONG3     TRYLONG.
   PXRWANT3       DEFPROBF.
   PXRSOON3       TIMINGF.
   PXRSOONN3      SOONNVF.
   PXRSOONMY3     MONYRF.
   PXCPFEEL3      SCALEF.
   CURRPREG       CURRPREGF.
   D_OKAKIDS      AKIDNUM.
   PXOTKID        Y1N5RDF.
   PXOKNUM        AKIDNUM.
   PXOKWTH        Y1N5RDF.
   PXOKWTHN       EVRCHL3F.
   PXOKSEX        MALFEMF.
   PXOKAD         ADPTGUARF.
   PXOKLIV1       ADPTCHLIVF.
   PXOKFAR        MILESFMT.
   PXOKAGE        KIDAGEF2D.
   PXOKSEX2       MALFEMF.
   PXOKAD2        ADPTGUARF.
   PXOKLIV9       ADPTCHLIVF.
   PXOKFAR2       MILESFMT.
   PXOKAGE2       KIDAGEF2D.
   PXOKSEX3       MALFEMF.
   PXOKAD3        ADPTGUARF.
   PXOKLIV17      ADPTCHLIVF.
   PXOKFAR3       MILESFMT.
   PXOKAGE3       KIDAGEF2D.
   PXOKSEX4       MALFEMF.
   PXOKAD4        ADPTGUARF.
   PXOKLIV25      ADPTCHLIVF.
   PXOKFAR4       MILESFMT.
   PXOKAGE4       KIDAGEF2D.
   D_OKAKIDS2     AKIDNUM.
   PXOTKID2       Y1N5RDF.
   PXOKNUM2       AKIDNUM.
   PXOKWTH2       Y1N5RDF.
   PXOKWTHN2      EVRCHL3F.
   PXOKSEX11      MALFEMF.
   PXOKAD11       ADPTGUARF.
   PXOKLIV81      ADPTCHLIVF.
   PXOKFAR11      MILESFMT.
   PXOKAGE11      KIDAGEF2D.
   PXOKSEX12      MALFEMF.
   PXOKAD12       ADPTGUARF.
   PXOKLIV89      ADPTCHLIVF.
   PXOKFAR12      MILESFMT.
   PXOKAGE12      KIDAGEF2D.
   PXOKSEX13      MALFEMF.
   PXOKAD13       ADPTGUARF.
   PXOKLIV97      ADPTCHLIVF.
   PXOKFAR13      MILESFMT.
   PXOKAGE13      KIDAGEF2D.
   PXOKSEX14      MALFEMF.
   PXOKAD14       ADPTGUARF.
   PXOKLIV105     ADPTCHLIVF.
   PXOKFAR14      MILESFMT.
   PXOKAGE14      KIDAGEF2D.
   PXOKSEX15      MALFEMF.
   PXOKAD15       ADPTGUARF.
   PXOKLIV113     ADPTCHLIVF.
   PXOKFAR15      MILESFMT.
   PXOKAGE15      KIDAGEF2D.
   D_OKAKIDS3     AKIDNUM.
   PXOTKID3       Y1N5RDF.
   PXOKNUM3       AKIDNUM.
   PXOKWTH3       Y1N5RDF.
   PXOKWTHN3      EVRCHL3F.
   PXOKSEX21      MALFEMF.
   PXOKAD21       ADPTGUARF.
   PXOKAGE21      KIDAGEF2D.
   PXOKSEX22      MALFEMF.
   PXOKAD22       ADPTGUARF.
   PXOKAGE22      KIDAGEF2D.
   PXOKSEX23      MALFEMF.
   PXOKAD23       ADPTGUARF.
   PXOKAGE23      KIDAGEF2D.
   PXOKSEX24      MALFEMF.
   PXOKAD24       ADPTGUARF.
   PXOKAGE24      KIDAGEF2D.
   PXOKSEX25      MALFEMF.
   PXOKAD25       ADPTGUARF.
   PXOKAGE25      KIDAGEF2D.
   D_NBAKIDS      AKIDNUM.
   PXNBEVR        Y1N5RDF.
   PXNBNUM        AKIDNUM.
   PXNBREL        Y1N5RDF.
   PXNBFOS        Y1N5RDF.
   PXNBSEX        MALFEMF.
   PXNBAD         ADPTGUARF.
   PXNBLIV1       ADPTCHLIVF.
   PXNBLIV2       ADPTCHLIVF.
   PXNBFAR        MILESFMT.
   PXNBAGE        KIDAGEF2D.
   PXNBREL2       Y1N5RDF.
   PXNBFOS2       Y1N5RDF.
   PXNBSEX2       MALFEMF.
   PXNBAD2        ADPTGUARF.
   PXNBLIV9       ADPTCHLIVF.
   PXNBLIV10      ADPTCHLIVF.
   PXNBFAR2       MILESFMT.
   PXNBAGE2       KIDAGEF2D.
   PXNBREL3       Y1N5RDF.
   PXNBFOS3       Y1N5RDF.
   PXNBSEX3       MALFEMF.
   PXNBAD3        ADPTGUARF.
   PXNBLIV17      ADPTCHLIVF.
   PXNBLIV18      ADPTCHLIVF.
   PXNBFAR3       MILESFMT.
   PXNBAGE3       KIDAGEF2D.
   D_NBAKIDS2     AKIDNUM.
   PXNBEVR2       Y1N5RDF.
   PXNBNUM2       AKIDNUM.
   PXNBREL11      Y1N5RDF.
   PXNBFOS11      Y1N5RDF.
   PXNBSEX11      MALFEMF.
   PXNBAD11       ADPTGUARF.
   PXNBAGE11      KIDAGEF2D.
   PXNBREL12      Y1N5RDF.
   PXNBFOS12      Y1N5RDF.
   PXNBSEX12      MALFEMF.
   PXNBAD12       ADPTGUARF.
   PXNBAGE12      KIDAGEF2D.
   PXNBREL13      Y1N5RDF.
   PXNBFOS13      Y1N5RDF.
   PXNBSEX13      MALFEMF.
   PXNBAD13       ADPTGUARF.
   PXNBAGE13      KIDAGEF2D.
   PXNBREL14      Y1N5RDF.
   PXNBFOS14      Y1N5RDF.
   PXNBSEX14      MALFEMF.
   PXNBAD14       ADPTGUARF.
   PXNBAGE14      KIDAGEF2D.
   PXNBREL15      Y1N5RDF.
   PXNBFOS15      Y1N5RDF.
   PXNBSEX15      MALFEMF.
   PXNBAD15       ADPTGUARF.
   PXNBAGE15      KIDAGEF2D.
   PXNBREL16      Y1N5RDF.
   PXNBFOS16      Y1N5RDF.
   PXNBSEX16      MALFEMF.
   PXNBAD16       ADPTGUARF.
   PXNBAGE16      KIDAGEF2D.
   PXNBREL17      Y1N5RDF.
   PXNBFOS17      Y1N5RDF.
   PXNBSEX17      MALFEMF.
   PXNBAD17       ADPTGUARF.
   PXNBAGE17      KIDAGEF2D.
   PXNBREL18      Y1N5RDF.
   PXNBFOS18      Y1N5RDF.
   PXNBSEX18      MALFEMF.
   PXNBAD18       ADPTGUARF.
   PXNBAGE18      KIDAGEF2D.
   D_NBAKIDS3     AKIDNUM.
   PXNBEVR3       Y1N5RDF.
   FPFIRST_M      MNTHFMT.
   FPFIRST_Y      YEARFMT.
   CMFSTSEX       CMFMT.
   FSTSEXAGE      AGESELF.
   FPAGE          FPAGE.
   FPAGE18        FPAGE18F.
   FPAGE15        FPAGE15F.
   FPAGE20        FPAGE20F.
   RFSXAGEGP      RFSXAGEGP.
   FPRLTN         RLTNFMT.
   FPUSE          Y1N5RDF.
   FPMETH01       MTHDSV1F.
   FPMETH02       MTHDSV1F.
   FPMETH03       MTHDSV1F.
   FPMETH04       MTHDSV1F.
   FPPROBE        Y1N5RDF.
   NFORMWIFE      NFORMWIFE.
   NFORMCOHAB     NFORMCOH.
   FWVERIFY       Y1N5RDF.
   FWVER          N0Y1CF.
   FWVERIFY2      Y1N5RDF.
   FWVER2         N0Y1CF.
   FWVERIFY3      Y1N5RDF.
   FWVER3         N0Y1CF.
   FWVERIFY4      Y1N5RDF.
   FWVER4         N0Y1CF.
   FCVER          N0Y1CF.
   FCVERIFY       Y1N5RDF.
   EXRELATION     EXRELAT.
   FWMAREND_Y     YEARFMT.
   AGEMARRN       AGESELF.
   LIVTOGN4       Y1N5RDF.
   STRTLIVE_Y4    YEARFMT.
   AGELIV4        AGESELF.
   CMUNIONW       YEARFMT.
   ENGAGTHN4      ENGAGF.
   MARREND4       MARRENDF.
   WIFEDIED_Y4    YEARFMT.
   DIVORFIN_Y4    YEARFMT.
   ANNULLED_Y4    YEARFMT.
   STOPLIVE_Y4    YEARFMT.
   EXRELATION2    EXRELAT.
   FWMAREND_Y2    YEARFMT.
   AGEMARRN2      AGESELF.
   LIVTOGN5       Y1N5RDF.
   STRTLIVE_Y5    YEARFMT.
   AGELIV5        AGESELF.
   ENGAGTHN5      ENGAGF.
   MARREND5       MARRENDF.
   WIFEDIED_Y5    YEARFMT.
   DIVORFIN_Y5    YEARFMT.
   ANNULLED_Y5    YEARFMT.
   STOPLIVE_Y5    YEARFMT.
   EXRELATION3    EXRELAT.
   FWMAREND_Y3    YEARFMT.
   AGEMARRN3      AGESELF.
   LIVTOGN6       Y1N5RDF.
   STRTLIVE_Y6    YEARFMT.
   AGELIV6        AGESELF.
   ENGAGTHN6      ENGAGF.
   MARREND6       MARRENDF.
   WIFEDIED_Y6    YEARFMT.
   DIVORFIN_Y6    YEARFMT.
   ANNULLED_Y6    YEARFMT.
   STOPLIVE_Y6    YEARFMT.
   EXRELATION4    EXRELAT.
   FWMAREND_Y4    YEARFMT.
   AGEMARRN4      AGESELF.
   LIVTOGN7       Y1N5RDF.
   STRTLIVE_Y7    YEARFMT.
   AGELIV7        AGESELF.
   ENGAGTHN7      ENGAGF.
   MARREND7       MARRENDF.
   WIFEDIED_Y7    YEARFMT.
   DIVORFIN_Y7    YEARFMT.
   ANNULLED_Y7    YEARFMT.
   STOPLIVE_Y7    YEARFMT.
   EXRELATION11   EXRELAT.
   STRTLIVE_Y14   YEARFMT.
   CMCOHFC11      YEARFMT.
   AGELIV14       AGESELF.
   ENGAGTHN14     ENGAGF.
   STOPLIVE_Y14   YEARFMT.
   FWPDOB_Y       YEARFMT.
   FWPAGE         AGEWFPRT2F.
   WIF1RACE       RACEFMT.
   WIF1NRACE      NRACE.
   FWPMARBF       Y1N5RDF.
   FWPDOB_Y2      YEARFMT.
   FWPAGE2        AGEWFPRT2F.
   FWPMARBF2      Y1N5RDF.
   FWPDOB_Y3      YEARFMT.
   FWPAGE3        AGEWFPRT2F.
   FWPMARBF3      Y1N5RDF.
   FWPDOB_Y4      YEARFMT.
   FWPAGE4        AGEWFPRT2F.
   FWPMARBF4      Y1N5RDF.
   FWPDOB_Y11     YEARFMT.
   FWPAGE11       AGEWFPRT2F.
   COH1RACE       RACEFMT.
   COH1NRACE      NRACE.
   FWPMARBF11     Y1N5RDF.
   FWPBIOKD       Y1N5RDF.
   FWPNUMKD       AKIDNONE.
   FWPCHSEX       MALFEMF.
   FWPCHDOB_Y     YEARFMT.
   FWCHMARB       Y1N5RDF.
   FWPCHRES       Y1N5RDF.
   FWPCHLRN       LRNPRGF.
   FWPCHLIV01     BIOCHLIV1F.
   FWPCHLIV02     BIOCHLIV1F.
   FWPCHLIV03     BIOCHLIV1F.
   FWPCHAGE       AGEGPRW.
   FWPCHSIG       Y1N5RDF.
   FWPCHCRT       Y1N5RDF.
   FWPCHGEN       Y1N5RDF.
   FWPCHEVR       Y1N5RDF.
   FWPCHFAR       MILESFMT.
   FWPRWANT       DEFPROBF.
   FWPSOON        TIMINGF.
   FWPSOONN       SOONNVF.
   FWPSOONMY      MONYRF.
   FWPHPYPG       SCALEF.
   FWPCHSEX2      MALFEMF.
   FWPCHDOB_Y2    YEARFMT.
   MULTBIRT42     Y1N5RDF.
   FWCHMARB2      Y1N5RDF.
   FWPCHRES2      Y1N5RDF.
   FWPCHLRN2      LRNPRGF.
   FWPCHLIV11     BIOCHLIV1F.
   FWPCHLIV12     BIOCHLIV1F.
   FWPCHLIV13     BIOCHLIV1F.
   FWPCHAGE2      AGEGPRW.
   FWPCHSIG2      Y1N5RDF.
   FWPCHCRT2      Y1N5RDF.
   FWPCHGEN2      Y1N5RDF.
   FWPCHEVR2      Y1N5RDF.
   FWPCHFAR2      MILESFMT.
   FWPRWANT2      DEFPROBF.
   FWPSOON2       TIMINGF.
   FWPSOONN2      SOONNVF.
   FWPSOONMY2     MONYRF.
   FWPHPYPG2      SCALEF.
   FWPCHSEX3      MALFEMF.
   FWPCHDOB_Y3    YEARFMT.
   MULTBIRT43     Y1N5RDF.
   FWCHMARB3      Y1N5RDF.
   FWPCHRES3      Y1N5RDF.
   FWPCHLRN3      LRNPRGF.
   FWPCHLIV21     BIOCHLIV1F.
   FWPCHLIV22     BIOCHLIV1F.
   FWPCHLIV23     BIOCHLIV1F.
   FWPCHAGE3      AGEGPRW.
   FWPCHSIG3      Y1N5RDF.
   FWPCHCRT3      Y1N5RDF.
   FWPCHGEN3      Y1N5RDF.
   FWPCHEVR3      Y1N5RDF.
   FWPCHFAR3      MILESFMT.
   FWPRWANT3      DEFPROBF.
   FWPSOON3       TIMINGF.
   FWPSOONN3      SOONNVF.
   FWPSOONMY3     MONYRF.
   FWPHPYPG3      SCALEF.
   FWPCHSEX4      MALFEMF.
   FWPCHDOB_Y4    YEARFMT.
   MULTBIRT44     Y1N5RDF.
   FWCHMARB4      Y1N5RDF.
   FWPCHRES4      Y1N5RDF.
   FWPCHLRN4      LRNPRGF.
   FWPCHLIV31     BIOCHLIV1F.
   FWPCHLIV32     BIOCHLIV1F.
   FWPCHLIV33     BIOCHLIV1F.
   FWPCHAGE4      AGEGPRW.
   FWPCHSIG4      Y1N5RDF.
   FWPCHCRT4      Y1N5RDF.
   FWPCHGEN4      Y1N5RDF.
   FWPCHEVR4      Y1N5RDF.
   FWPCHFAR4      MILESFMT.
   FWPRWANT4      DEFPROBF.
   FWPSOON4       TIMINGF.
   FWPSOONN4      SOONNVF.
   FWPSOONMY4     MONYRF.
   FWPHPYPG4      SCALEF.
   FWPCHSEX5      MALFEMF.
   FWPCHDOB_Y5    YEARFMT.
   MULTBIRT45     Y1N5RDF.
   FWCHMARB5      Y1N5RDF.
   FWPCHRES5      Y1N5RDF.
   FWPCHLRN5      LRNPRGF.
   FWPCHLIV41     BIOCHLIV1F.
   FWPCHLIV42     BIOCHLIV1F.
   FWPCHLIV43     BIOCHLIV1F.
   FWPCHAGE5      AGEGPRW.
   FWPCHSIG5      Y1N5RDF.
   FWPCHCRT5      Y1N5RDF.
   FWPCHGEN5      Y1N5RDF.
   FWPCHEVR5      Y1N5RDF.
   FWPCHFAR5      MILESFMT.
   FWPRWANT5      DEFPROBF.
   FWPSOON5       TIMINGF.
   FWPSOONN5      SOONNVF.
   FWPSOONMY5     MONYRF.
   FWPHPYPG5      SCALEF.
   FWPBIOKD2      Y1N5RDF.
   FWPNUMKD2      AKIDNONE.
   FWPCHSEX11     MALFEMF.
   FWPCHDOB_Y11   YEARFMT.
   FWCHMARB11     Y1N5RDF.
   FWPCHRES11     Y1N5RDF.
   FWPCHLRN11     LRNPRGF.
   FWPCHLIV101    BIOCHLIV1F.
   FWPCHLIV102    BIOCHLIV1F.
   FWPCHAGE11     AGEGPRW.
   FWPCHSIG11     Y1N5RDF.
   FWPCHCRT11     Y1N5RDF.
   FWPCHGEN11     Y1N5RDF.
   FWPCHEVR11     Y1N5RDF.
   FWPCHFAR11     MILESFMT.
   FWPRWANT11     DEFPROBF.
   FWPSOON11      TIMINGF.
   FWPSOONN11     SOONNVF.
   FWPSOONMY11    MONYRF.
   FWPHPYPG11     SCALEF.
   FWPCHSEX12     MALFEMF.
   FWPCHDOB_Y12   YEARFMT.
   MULTBIRT52     Y1N5RDF.
   FWCHMARB12     Y1N5RDF.
   FWPCHRES12     Y1N5RDF.
   FWPCHLRN12     LRNPRGF.
   FWPCHLIV111    BIOCHLIV1F.
   FWPCHLIV112    BIOCHLIV1F.
   FWPCHAGE12     AGEGPRW.
   FWPCHSIG12     Y1N5RDF.
   FWPCHCRT12     Y1N5RDF.
   FWPCHGEN12     Y1N5RDF.
   FWPCHEVR12     Y1N5RDF.
   FWPCHFAR12     MILESFMT.
   FWPRWANT12     DEFPROBF.
   FWPSOON12      TIMINGF.
   FWPSOONN12     SOONNVF.
   FWPSOONMY12    MONYRF.
   FWPHPYPG12     SCALEF.
   FWPCHSEX13     MALFEMF.
   FWPCHDOB_Y13   YEARFMT.
   MULTBIRT53     Y1N5RDF.
   FWCHMARB13     Y1N5RDF.
   FWPCHRES13     Y1N5RDF.
   FWPCHLRN13     LRNPRGF.
   FWPCHLIV121    BIOCHLIV1F.
   FWPCHLIV122    BIOCHLIV1F.
   FWPCHAGE13     AGEGPRW.
   FWPCHSIG13     Y1N5RDF.
   FWPCHCRT13     Y1N5RDF.
   FWPCHGEN13     Y1N5RDF.
   FWPCHEVR13     Y1N5RDF.
   FWPCHFAR13     MILESFMT.
   FWPRWANT13     DEFPROBF.
   FWPSOON13      TIMINGF.
   FWPSOONN13     SOONNVF.
   FWPSOONMY13    MONYRF.
   FWPHPYPG13     SCALEF.
   FWPCHSEX14     MALFEMF.
   FWPCHDOB_Y14   YEARFMT.
   MULTBIRT54     Y1N5RDF.
   FWCHMARB14     Y1N5RDF.
   FWPCHRES14     Y1N5RDF.
   FWPCHLRN14     LRNPRGF.
   FWPCHLIV131    BIOCHLIV1F.
   FWPCHLIV132    BIOCHLIV1F.
   FWPCHAGE14     AGEGPRW.
   FWPCHSIG14     Y1N5RDF.
   FWPCHCRT14     Y1N5RDF.
   FWPCHGEN14     Y1N5RDF.
   FWPCHEVR14     Y1N5RDF.
   FWPCHFAR14     MILESFMT.
   FWPRWANT14     DEFPROBF.
   FWPSOON14      TIMINGF.
   FWPSOONN14     SOONNVF.
   FWPSOONMY14    MONYRF.
   FWPHPYPG14     SCALEF.
   FWPBIOKD3      Y1N5RDF.
   FWPNUMKD3      AKIDNONE.
   FWPCHSEX21     MALFEMF.
   FWPCHDOB_Y21   YEARFMT.
   FWCHMARB21     Y1N5RDF.
   FWPCHRES21     Y1N5RDF.
   FWPCHLRN21     LRNPRGF.
   FWPCHLIV201    BIOCHLIV1F.
   FWPCHLIV202    BIOCHLIV1F.
   FWPCHAGE21     AGEGPRW.
   FWPCHSIG21     Y1N5RDF.
   FWPCHCRT21     Y1N5RDF.
   FWPCHGEN21     Y1N5RDF.
   FWPCHEVR21     Y1N5RDF.
   FWPCHFAR21     MILESFMT.
   FWPRWANT21     DEFPROBF.
   FWPSOON21      TIMINGF.
   FWPSOONN21     SOONNVF.
   FWPSOONMY21    MONYRF.
   FWPHPYPG21     SCALEF.
   FWPCHSEX22     MALFEMF.
   FWPCHDOB_Y22   YEARFMT.
   MULTBIRT62     Y1N5RDF.
   FWCHMARB22     Y1N5RDF.
   FWPCHRES22     Y1N5RDF.
   FWPCHLRN22     LRNPRGF.
   FWPCHLIV211    BIOCHLIV1F.
   FWPCHLIV212    BIOCHLIV1F.
   FWPCHAGE22     AGEGPRW.
   FWPCHSIG22     Y1N5RDF.
   FWPCHCRT22     Y1N5RDF.
   FWPCHGEN22     Y1N5RDF.
   FWPCHEVR22     Y1N5RDF.
   FWPCHFAR22     MILESFMT.
   FWPRWANT22     DEFPROBF.
   FWPSOON22      TIMINGF.
   FWPSOONN22     SOONNVF.
   FWPSOONMY22    MONYRF.
   FWPHPYPG22     SCALEF.
   FWPCHSEX23     MALFEMF.
   FWPCHDOB_Y23   YEARFMT.
   MULTBIRT63     Y1N5RDF.
   FWCHMARB23     Y1N5RDF.
   FWPCHRES23     Y1N5RDF.
   FWPCHLRN23     LRNPRGF.
   FWPCHLIV221    BIOCHLIV1F.
   FWPCHLIV222    BIOCHLIV1F.
   FWPCHAGE23     AGEGPRW.
   FWPCHSIG23     Y1N5RDF.
   FWPCHCRT23     Y1N5RDF.
   FWPCHGEN23     Y1N5RDF.
   FWPCHEVR23     Y1N5RDF.
   FWPCHFAR23     MILESFMT.
   FWPRWANT23     DEFPROBF.
   FWPSOON23      TIMINGF.
   FWPSOONN23     SOONNVF.
   FWPSOONMY23    MONYRF.
   FWPHPYPG23     SCALEF.
   FWPBIOKD4      Y1N5RDF.
   FWPNUMKD4      AKIDNONE.
   FWPBIOKD11     Y1N5RDF.
   FWPNUMKD11     AKIDNONE.
   FWPCHSEX101    MALFEMF.
   FWPCHDOB_Y101  YEARFMT.
   FWPCHRES101    Y1N5RDF.
   FWPCHLRN101    LRNPRGF.
   FWPCHLIV1001   BIOCHLIV1F.
   FWPCHLIV1002   BIOCHLIV1F.
   FWPCHLIV1003   BIOCHLIV1F.
   FWPCHAGE101    AGEGPRW.
   FWPCHSIG101    Y1N5RDF.
   FWPCHCRT101    Y1N5RDF.
   FWPCHGEN101    Y1N5RDF.
   FWPCHEVR101    Y1N5RDF.
   FWPCHFAR101    MILESFMT.
   FWPRWANT101    DEFPROBF.
   FWPSOON101     TIMINGF.
   FWPSOONN101    SOONNVF.
   FWPSOONMY101   MONYRF.
   FWPHPYPG101    SCALEF.
   FWPCHSEX102    MALFEMF.
   FWPCHDOB_Y102  YEARFMT.
   MULTBIRT142    Y1N5RDF.
   FWPCHRES102    Y1N5RDF.
   FWPCHLRN102    LRNPRGF.
   FWPCHLIV1011   BIOCHLIV1F.
   FWPCHLIV1012   BIOCHLIV1F.
   FWPCHLIV1013   BIOCHLIV1F.
   FWPCHAGE102    AGEGPRW.
   FWPCHSIG102    Y1N5RDF.
   FWPCHCRT102    Y1N5RDF.
   FWPCHGEN102    Y1N5RDF.
   FWPCHEVR102    Y1N5RDF.
   FWPCHFAR102    MILESFMT.
   FWPRWANT102    DEFPROBF.
   FWPSOON102     TIMINGF.
   FWPSOONN102    SOONNVF.
   FWPSOONMY102   MONYRF.
   FWPHPYPG102    SCALEF.
   FWPCHSEX103    MALFEMF.
   FWPCHDOB_Y103  YEARFMT.
   MULTBIRT143    Y1N5RDF.
   FWPCHRES103    Y1N5RDF.
   FWPCHLRN103    LRNPRGF.
   FWPCHLIV1021   BIOCHLIV1F.
   FWPCHLIV1022   BIOCHLIV1F.
   FWPCHLIV1023   BIOCHLIV1F.
   FWPCHAGE103    AGEGPRW.
   FWPCHSIG103    Y1N5RDF.
   FWPCHCRT103    Y1N5RDF.
   FWPCHGEN103    Y1N5RDF.
   FWPCHEVR103    Y1N5RDF.
   FWPCHFAR103    MILESFMT.
   FWPRWANT103    DEFPROBF.
   FWPSOON103     TIMINGF.
   FWPSOONN103    SOONNVF.
   FWPSOONMY103   MONYRF.
   FWPHPYPG103    SCALEF.
   FWPCHSEX104    MALFEMF.
   FWPCHDOB_Y104  YEARFMT.
   MULTBIRT144    Y1N5RDF.
   FWPCHRES104    Y1N5RDF.
   FWPCHLRN104    LRNPRGF.
   FWPCHLIV1031   BIOCHLIV1F.
   FWPCHLIV1032   BIOCHLIV1F.
   FWPCHLIV1033   BIOCHLIV1F.
   FWPCHAGE104    AGEGPRW.
   FWPCHSIG104    Y1N5RDF.
   FWPCHCRT104    Y1N5RDF.
   FWPCHGEN104    Y1N5RDF.
   FWPCHEVR104    Y1N5RDF.
   FWPCHFAR104    MILESFMT.
   FWPRWANT104    DEFPROBF.
   FWPSOON104     TIMINGF.
   FWPSOONN104    SOONNVF.
   FWPSOONMY104   MONYRF.
   FWPHPYPG104    SCALEF.
   E_OKAKIDS      AKIDNUM.
   FWPOTKID       Y1N5RDF.
   FWPOKNUM       OTN2BNUM.
   FWPOKWTH       Y1N5RDF.
   FWPOKWTHN      OTN2BNUM.
   FWPOKSEX       MALFEMF.
   FWPOKAD        ADPTGUARF.
   FWPOKLIV1      ADPTCHLIVF.
   FWPOKLIV2      ADPTCHLIVF.
   FWPOKFAR       MILESFMT.
   FWPOKAGE       KIDAGEF2D.
   FWPOKSEX2      MALFEMF.
   FWPOKAD2       ADPTGUARF.
   FWPOKLIV9      ADPTCHLIVF.
   FWPOKLIV10     ADPTCHLIVF.
   FWPOKFAR2      MILESFMT.
   FWPOKAGE2      KIDAGEF2D.
   FWPOKSEX3      MALFEMF.
   FWPOKAD3       ADPTGUARF.
   FWPOKLIV17     ADPTCHLIVF.
   FWPOKLIV18     ADPTCHLIVF.
   FWPOKFAR3      MILESFMT.
   FWPOKAGE3      KIDAGEF2D.
   FWPOKSEX4      MALFEMF.
   FWPOKAD4       ADPTGUARF.
   FWPOKLIV25     ADPTCHLIVF.
   FWPOKLIV26     ADPTCHLIVF.
   FWPOKFAR4      MILESFMT.
   FWPOKAGE4      KIDAGEF2D.
   FWPOKSEX5      MALFEMF.
   FWPOKAD5       ADPTGUARF.
   FWPOKLIV33     ADPTCHLIVF.
   FWPOKLIV34     ADPTCHLIVF.
   FWPOKFAR5      MILESFMT.
   FWPOKAGE5      KIDAGEF2D.
   E_OKAKIDS2     AKIDNUM.
   FWPOTKID2      Y1N5RDF.
   FWPOKNUM2      OTN2BNUM.
   FWPOKWTH2      Y1N5RDF.
   FWPOKWTHN2     OTN2BNUM.
   FWPOKSEX11     MALFEMF.
   FWPOKAD11      ADPTGUARF.
   FWPOKLIV81     ADPTCHLIVF.
   FWPOKFAR11     MILESFMT.
   FWPOKAGE11     KIDAGEF2D.
   FWPOKSEX12     MALFEMF.
   FWPOKAD12      ADPTGUARF.
   FWPOKLIV89     ADPTCHLIVF.
   FWPOKFAR12     MILESFMT.
   FWPOKAGE12     KIDAGEF2D.
   FWPOKSEX13     MALFEMF.
   FWPOKAD13      ADPTGUARF.
   FWPOKLIV97     ADPTCHLIVF.
   FWPOKFAR13     MILESFMT.
   FWPOKAGE13     KIDAGEF2D.
   FWPOKSEX14     MALFEMF.
   FWPOKAD14      ADPTGUARF.
   FWPOKLIV105    ADPTCHLIVF.
   FWPOKFAR14     MILESFMT.
   FWPOKAGE14     KIDAGEF2D.
   FWPOKSEX15     MALFEMF.
   FWPOKAD15      ADPTGUARF.
   FWPOKLIV113    ADPTCHLIVF.
   FWPOKFAR15     MILESFMT.
   FWPOKAGE15     KIDAGEF2D.
   E_OKAKIDS3     AKIDNUM.
   FWPOTKID3      Y1N5RDF.
   FWPOKNUM3      OTN2BNUM.
   FWPOKWTH3      Y1N5RDF.
   FWPOKWTHN3     OTN2BNUM.
   FWPOKSEX21     MALFEMF.
   FWPOKAD21      ADPTGUARF.
   FWPOKLIV161    ADPTCHLIVF.
   FWPOKFAR21     MILESFMT.
   FWPOKAGE21     KIDAGEF2D.
   FWPOKSEX22     MALFEMF.
   FWPOKAD22      ADPTGUARF.
   FWPOKLIV169    ADPTCHLIVF.
   FWPOKFAR22     MILESFMT.
   FWPOKAGE22     KIDAGEF2D.
   E_OKAKIDS4     AKIDNUM.
   FWPOTKID4      Y1N5RDF.
   FWPOKNUM4      OTN2BNUM.
   FWPOKWTH4      Y1N5RDF.
   FWPOKWTHN4     OTN2BNUM.
   FWPOKSEX31     MALFEMF.
   FWPOKAD31      ADPTGUARF.
   FWPOKAGE31     KIDAGEF2D.
   E_OKAKIDS11    AKIDNUM.
   FWPOTKID11     Y1N5RDF.
   FWPOKNUM11     OTN2BNUM.
   FWPOKWTH11     Y1N5RDF.
   FWPOKWTHN11    OTN2BNUM.
   FWPOKSEX101    MALFEMF.
   FWPOKAD101     ADPTGUARF.
   FWPOKLIV801    ADPTCHLIVF.
   FWPOKFAR101    MILESFMT.
   FWPOKAGE101    KIDAGEF2D.
   FWPOKSEX102    MALFEMF.
   FWPOKAD102     ADPTGUARF.
   FWPOKLIV809    ADPTCHLIVF.
   FWPOKFAR102    MILESFMT.
   FWPOKAGE102    KIDAGEF2D.
   FWPOKSEX103    MALFEMF.
   FWPOKAD103     ADPTGUARF.
   FWPOKLIV817    ADPTCHLIVF.
   FWPOKFAR103    MILESFMT.
   FWPOKAGE103    KIDAGEF2D.
   FWPOKSEX104    MALFEMF.
   FWPOKAD104     ADPTGUARF.
   FWPOKLIV825    ADPTCHLIVF.
   FWPOKFAR104    MILESFMT.
   FWPOKAGE104    KIDAGEF2D.
   FWPOKSEX105    MALFEMF.
   FWPOKAD105     ADPTGUARF.
   FWPOKLIV833    ADPTCHLIVF.
   FWPOKFAR105    MILESFMT.
   FWPOKAGE105    KIDAGEF2D.
   FWPOKSEX106    MALFEMF.
   FWPOKAD106     ADPTGUARF.
   FWPOKLIV841    ADPTCHLIVF.
   FWPOKFAR106    MILESFMT.
   FWPOKAGE106    KIDAGEF2D.
   E_NBAKIDS      AKIDNUM.
   FWPNBEVR       Y1N5RDF.
   FWPNBNUM       OTN2BNUM.
   FWPNBREL       Y1N5RDF.
   FWPNBFOS       Y1N5RDF.
   FWPNBSEX       MALFEMF.
   FWPNBAD        ADPTGUARF.
   FWPNBLIV1      ADPTCHLIVF.
   FWPNBLIV2      ADPTCHLIVF.
   FWPNBFAR       MILESFMT.
   FWPNBAGE       KIDAGEF2D.
   FWPNBREL2      Y1N5RDF.
   FWPNBFOS2      Y1N5RDF.
   FWPNBSEX2      MALFEMF.
   FWPNBAD2       ADPTGUARF.
   FWPNBLIV9      ADPTCHLIVF.
   FWPNBLIV10     ADPTCHLIVF.
   FWPNBFAR2      MILESFMT.
   FWPNBAGE2      KIDAGEF2D.
   FWPNBREL3      Y1N5RDF.
   FWPNBFOS3      Y1N5RDF.
   FWPNBSEX3      MALFEMF.
   FWPNBAD3       ADPTGUARF.
   FWPNBLIV17     ADPTCHLIVF.
   FWPNBLIV18     ADPTCHLIVF.
   FWPNBFAR3      MILESFMT.
   FWPNBAGE3      KIDAGEF2D.
   FWPNBREL4      Y1N5RDF.
   FWPNBFOS4      Y1N5RDF.
   FWPNBSEX4      MALFEMF.
   FWPNBAD4       ADPTGUARF.
   FWPNBLIV25     ADPTCHLIVF.
   FWPNBLIV26     ADPTCHLIVF.
   FWPNBFAR4      MILESFMT.
   FWPNBAGE4      KIDAGEF2D.
   E_NBAKIDS2     AKIDNUM.
   FWPNBEVR2      Y1N5RDF.
   FWPNBNUM2      OTN2BNUM.
   FWPNBREL11     Y1N5RDF.
   FWPNBFOS11     Y1N5RDF.
   FWPNBSEX11     MALFEMF.
   FWPNBAD11      ADPTGUARF.
   FWPNBLIV81     ADPTCHLIVF.
   FWPNBFAR11     MILESFMT.
   FWPNBAGE11     KIDAGEF2D.
   FWPNBREL12     Y1N5RDF.
   FWPNBFOS12     Y1N5RDF.
   FWPNBSEX12     MALFEMF.
   FWPNBAD12      ADPTGUARF.
   FWPNBLIV89     ADPTCHLIVF.
   FWPNBFAR12     MILESFMT.
   FWPNBAGE12     KIDAGEF2D.
   E_NBAKIDS3     AKIDNUM.
   FWPNBEVR3      Y1N5RDF.
   FWPNBNUM3      OTN2BNUM.
   FWPNBREL21     Y1N5RDF.
   FWPNBFOS21     Y1N5RDF.
   FWPNBSEX21     MALFEMF.
   FWPNBAD21      ADPTGUARF.
   FWPNBLIV161    ADPTCHLIVF.
   FWPNBFAR21     MILESFMT.
   FWPNBAGE21     KIDAGEF2D.
   FWPNBREL22     Y1N5RDF.
   FWPNBFOS22     Y1N5RDF.
   FWPNBSEX22     MALFEMF.
   FWPNBAD22      ADPTGUARF.
   FWPNBLIV169    ADPTCHLIVF.
   FWPNBFAR22     MILESFMT.
   FWPNBAGE22     KIDAGEF2D.
   E_NBAKIDS4     AKIDNUM.
   FWPNBEVR4      Y1N5RDF.
   E_NBAKIDS11    AKIDNUM.
   FWPNBEVR11     Y1N5RDF.
   FWPNBNUM11     OTN2BNUM.
   FWPNBREL101    Y1N5RDF.
   FWPNBFOS101    Y1N5RDF.
   FWPNBSEX101    MALFEMF.
   FWPNBAD101     ADPTGUARF.
   FWPNBLIV801    ADPTCHLIVF.
   FWPNBFAR101    MILESFMT.
   FWPNBAGE101    KIDAGEF2D.
   FWPNBREL102    Y1N5RDF.
   FWPNBFOS102    Y1N5RDF.
   FWPNBSEX102    MALFEMF.
   FWPNBAD102     ADPTGUARF.
   FWPNBLIV809    ADPTCHLIVF.
   FWPNBFAR102    MILESFMT.
   FWPNBAGE102    KIDAGEF2D.
   FWPNBREL103    Y1N5RDF.
   FWPNBFOS103    Y1N5RDF.
   FWPNBSEX103    MALFEMF.
   FWPNBAD103     ADPTGUARF.
   FWPNBLIV817    ADPTCHLIVF.
   FWPNBFAR103    MILESFMT.
   FWPNBAGE103    KIDAGEF2D.
   FWPNBREL104    Y1N5RDF.
   FWPNBFOS104    Y1N5RDF.
   FWPNBSEX104    MALFEMF.
   FWPNBAD104     ADPTGUARF.
   FWPNBLIV825    ADPTCHLIVF.
   FWPNBFAR104    MILESFMT.
   FWPNBAGE104    KIDAGEF2D.
   OTBCHIL        Y1N5RDF.
   OTBPROBE       Y1N5RDF.
   OTBCHILN       EVRCHL3F.
   OTBSAME        Y1N5RDF.
   OBCSEXX        MALFEMF.
   OBCDOB_Y       YEARFMT.
   OBCMAGEX       AGEWFPRT2F.
   OBCMLIV        Y1N5RDF.
   OBCKNOWX       LRNPRGF.
   OBCLIVEX01     BIOCHLIV1F.
   OBCLIVEX02     BIOCHLIV1F.
   OBCLIVEX03     BIOCHLIV1F.
   OBCAGE         AGEGPRW.
   OBCCHSIG       Y1N5RDF.
   OBCCHCRT       Y1N5RDF.
   OBCCHGEN       Y1N5RDF.
   OBCEVER        Y1N5RDF.
   OBCFAR         MILESFMT.
   OBCRWANX       DEFPROBF.
   OBCSOONX       TIMINGF.
   OBCSOONN       SOONNVF.
   OBCSOONMY      MONYRF.
   OBCHPYX        SCALEF.
   OBCSEXX2       MALFEMF.
   OBCDOB_Y2      YEARFMT.
   MULTBIRT152    Y1N5RDF.
   OBCMAGEX2      AGEWFPRT2F.
   OBCMLIV2       Y1N5RDF.
   OBCKNOWX2      LRNPRGF.
   OBCLIVEX11     BIOCHLIV1F.
   OBCLIVEX12     BIOCHLIV1F.
   OBCLIVEX13     BIOCHLIV1F.
   OBCAGE2        AGEGPRW.
   OBCCHSIG2      Y1N5RDF.
   OBCCHCRT2      Y1N5RDF.
   OBCCHGEN2      Y1N5RDF.
   OBCEVER2       Y1N5RDF.
   OBCFAR2        MILESFMT.
   OBCRWANX2      DEFPROBF.
   OBCSOONX2      TIMINGF.
   OBCSOONN2      SOONNVF.
   OBCSOONMY2     MONYRF.
   OBCHPYX2       SCALEF.
   OBCSEXX3       MALFEMF.
   OBCDOB_Y3      YEARFMT.
   MULTBIRT153    Y1N5RDF.
   OBCMAGEX3      AGEWFPRT2F.
   OBCMLIV3       Y1N5RDF.
   OBCKNOWX3      LRNPRGF.
   OBCLIVEX21     BIOCHLIV1F.
   OBCLIVEX22     BIOCHLIV1F.
   OBCLIVEX23     BIOCHLIV1F.
   OBCAGE3        AGEGPRW.
   OBCCHSIG3      Y1N5RDF.
   OBCCHCRT3      Y1N5RDF.
   OBCCHGEN3      Y1N5RDF.
   OBCEVER3       Y1N5RDF.
   OBCFAR3        MILESFMT.
   OBCRWANX3      DEFPROBF.
   OBCSOONX3      TIMINGF.
   OBCSOONN3      SOONNVF.
   OBCSOONMY3     MONYRF.
   OBCHPYX3       SCALEF.
   OBCSEXX4       MALFEMF.
   OBCDOB_Y4      YEARFMT.
   MULTBIRT154    Y1N5RDF.
   OBCMAGEX4      AGEWFPRT2F.
   OBCMLIV4       Y1N5RDF.
   OBCKNOWX4      LRNPRGF.
   OBCLIVEX31     BIOCHLIV1F.
   OBCLIVEX32     BIOCHLIV1F.
   OBCLIVEX33     BIOCHLIV1F.
   OBCAGE4        AGEGPRW.
   OBCCHSIG4      Y1N5RDF.
   OBCCHCRT4      Y1N5RDF.
   OBCCHGEN4      Y1N5RDF.
   OBCEVER4       Y1N5RDF.
   OBCFAR4        MILESFMT.
   OBCRWANX4      DEFPROBF.
   OBCSOONX4      TIMINGF.
   OBCSOONN4      SOONNVF.
   OBCSOONMY4     MONYRF.
   OBCHPYX4       SCALEF.
   OBCSEXX5       MALFEMF.
   OBCDOB_Y5      YEARFMT.
   MULTBIRT155    Y1N5RDF.
   OBCMAGEX5      AGEWFPRT2F.
   OBCMLIV5       Y1N5RDF.
   OBCKNOWX5      LRNPRGF.
   OBCLIVEX41     BIOCHLIV1F.
   OBCLIVEX42     BIOCHLIV1F.
   OBCLIVEX43     BIOCHLIV1F.
   OBCAGE5        AGEGPRW.
   OBCCHSIG5      Y1N5RDF.
   OBCCHCRT5      Y1N5RDF.
   OBCCHGEN5      Y1N5RDF.
   OBCEVER5       Y1N5RDF.
   OBCFAR5        MILESFMT.
   OBCRWANX5      DEFPROBF.
   OBCSOONX5      TIMINGF.
   OBCSOONN5      SOONNVF.
   OBCSOONMY5     MONYRF.
   OBCHPYX5       SCALEF.
   OBCSEXX6       MALFEMF.
   OBCDOB_Y6      YEARFMT.
   MULTBIRT156    Y1N5RDF.
   OBCMAGEX6      AGEWFPRT2F.
   OBCMLIV6       Y1N5RDF.
   OBCKNOWX6      LRNPRGF.
   OBCLIVEX51     BIOCHLIV1F.
   OBCLIVEX52     BIOCHLIV1F.
   OBCLIVEX53     BIOCHLIV1F.
   OBCAGE6        AGEGPRW.
   OBCCHSIG6      Y1N5RDF.
   OBCCHCRT6      Y1N5RDF.
   OBCCHGEN6      Y1N5RDF.
   OBCEVER6       Y1N5RDF.
   OBCFAR6        MILESFMT.
   OBCRWANX6      DEFPROBF.
   OBCSOONX6      TIMINGF.
   OBCSOONN6      SOONNVF.
   OBCSOONMY6     MONYRF.
   OBCHPYX6       SCALEF.
   OBCSEXX7       MALFEMF.
   OBCDOB_Y7      YEARFMT.
   MULTBIRT157    Y1N5RDF.
   OBCMAGEX7      AGEWFPRT2F.
   OBCMLIV7       Y1N5RDF.
   OBCKNOWX7      LRNPRGF.
   OBCLIVEX61     BIOCHLIV1F.
   OBCLIVEX62     BIOCHLIV1F.
   OBCLIVEX63     BIOCHLIV1F.
   OBCAGE7        AGEGPRW.
   OBCCHSIG7      Y1N5RDF.
   OBCCHCRT7      Y1N5RDF.
   OBCCHGEN7      Y1N5RDF.
   OBCEVER7       Y1N5RDF.
   OBCFAR7        MILESFMT.
   OBCRWANX7      DEFPROBF.
   OBCSOONX7      TIMINGF.
   OBCSOONN7      SOONNVF.
   OBCSOONMY7     MONYRF.
   OBCHPYX7       SCALEF.
   OBCSEXX8       MALFEMF.
   OBCDOB_Y8      YEARFMT.
   MULTBIRT158    Y1N5RDF.
   OBCMAGEX8      AGEWFPRT2F.
   OBCMLIV8       Y1N5RDF.
   OBCKNOWX8      LRNPRGF.
   OBCLIVEX71     BIOCHLIV1F.
   OBCLIVEX72     BIOCHLIV1F.
   OBCLIVEX73     BIOCHLIV1F.
   OBCAGE8        AGEGPRW.
   OBCCHSIG8      Y1N5RDF.
   OBCCHCRT8      Y1N5RDF.
   OBCCHGEN8      Y1N5RDF.
   OBCEVER8       Y1N5RDF.
   OBCFAR8        MILESFMT.
   OBCRWANX8      DEFPROBF.
   OBCSOONX8      TIMINGF.
   OBCSOONN8      SOONNVF.
   OBCSOONMY8     MONYRF.
   OBCHPYX8       SCALEF.
   OBCSEXX9       MALFEMF.
   OBCDOB_Y9      YEARFMT.
   MULTBIRT159    Y1N5RDF.
   OBCMAGEX9      AGEWFPRT2F.
   OBCMLIV9       Y1N5RDF.
   OBCKNOWX9      LRNPRGF.
   OBCLIVEX81     BIOCHLIV1F.
   OBCLIVEX82     BIOCHLIV1F.
   OBCLIVEX83     BIOCHLIV1F.
   OBCAGE9        AGEGPRW.
   OBCCHSIG9      Y1N5RDF.
   OBCCHCRT9      Y1N5RDF.
   OBCCHGEN9      Y1N5RDF.
   OBCEVER9       Y1N5RDF.
   OBCFAR9        MILESFMT.
   OBCRWANX9      DEFPROBF.
   OBCSOONX9      TIMINGF.
   OBCSOONN9      SOONNVF.
   OBCSOONMY9     MONYRF.
   OBCHPYX9       SCALEF.
   OBCSEXX10      MALFEMF.
   OBCDOB_Y10     YEARFMT.
   MULTBIRT160    Y1N5RDF.
   OBCMAGEX10     AGEWFPRT2F.
   OBCMLIV10      Y1N5RDF.
   OBCKNOWX10     LRNPRGF.
   OBCLIVEX91     BIOCHLIV1F.
   OBCLIVEX92     BIOCHLIV1F.
   OBCLIVEX93     BIOCHLIV1F.
   OBCAGE10       AGEGPRW.
   OBCCHSIG10     Y1N5RDF.
   OBCCHCRT10     Y1N5RDF.
   OBCCHGEN10     Y1N5RDF.
   OBCEVER10      Y1N5RDF.
   OBCFAR10       MILESFMT.
   OBCRWANX10     DEFPROBF.
   OBCSOONX10     TIMINGF.
   OBCSOONN10     SOONNVF.
   OBCSOONMY10    MONYRF.
   OBCHPYX10      SCALEF.
   F_AKIDS        AKIDNUM.
   OTACHIL        Y1N5RDF.
   OTACHILN       OTACHILN.
   OTNBREL        Y1N5RDF.
   OTNBFOS        Y1N5RDF.
   OTNBSEX        MALFEMF.
   OTNBAD         ADPTGUARF.
   OTNBLIV1       ADPTCHLIVF.
   OTNBLIV2       ADPTCHLIVF.
   OTNBFAR        MILESFMT.
   OTNBAGE        KIDAGEF2D.
   OTNBREL2       Y1N5RDF.
   OTNBFOS2       Y1N5RDF.
   OTNBSEX2       MALFEMF.
   OTNBAD2        ADPTGUARF.
   OTNBLIV9       ADPTCHLIVF.
   OTNBLIV10      ADPTCHLIVF.
   OTNBFAR2       MILESFMT.
   OTNBAGE2       KIDAGEF2D.
   OTNBREL3       Y1N5RDF.
   OTNBFOS3       Y1N5RDF.
   OTNBSEX3       MALFEMF.
   OTNBAD3        ADPTGUARF.
   OTNBLIV17      ADPTCHLIVF.
   OTNBLIV18      ADPTCHLIVF.
   OTNBFAR3       MILESFMT.
   OTNBAGE3       KIDAGEF2D.
   OTNBREL4       Y1N5RDF.
   OTNBFOS4       Y1N5RDF.
   OTNBSEX4       MALFEMF.
   OTNBAD4        ADPTGUARF.
   OTNBLIV25      ADPTCHLIVF.
   OTNBLIV26      ADPTCHLIVF.
   OTNBFAR4       MILESFMT.
   OTNBAGE4       KIDAGEF2D.
   OTNBREL5       Y1N5RDF.
   OTNBFOS5       Y1N5RDF.
   OTNBSEX5       MALFEMF.
   OTNBAD5        ADPTGUARF.
   OTNBLIV33      ADPTCHLIVF.
   OTNBLIV34      ADPTCHLIVF.
   OTNBFAR5       MILESFMT.
   OTNBAGE5       KIDAGEF2D.
   OTNBREL6       Y1N5RDF.
   OTNBFOS6       Y1N5RDF.
   OTNBSEX6       MALFEMF.
   OTNBAD6        ADPTGUARF.
   OTNBLIV41      ADPTCHLIVF.
   OTNBLIV42      ADPTCHLIVF.
   OTNBFAR6       MILESFMT.
   OTNBAGE6       KIDAGEF2D.
   OTNBREL7       Y1N5RDF.
   OTNBFOS7       Y1N5RDF.
   OTNBSEX7       MALFEMF.
   OTNBAD7        ADPTGUARF.
   OTNBLIV49      ADPTCHLIVF.
   OTNBLIV50      ADPTCHLIVF.
   OTNBFAR7       MILESFMT.
   OTNBAGE7       KIDAGEF2D.
   OTNBREL8       Y1N5RDF.
   OTNBFOS8       Y1N5RDF.
   OTNBSEX8       MALFEMF.
   OTNBAD8        ADPTGUARF.
   OTNBLIV57      ADPTCHLIVF.
   OTNBLIV58      ADPTCHLIVF.
   OTNBFAR8       MILESFMT.
   OTNBAGE8       KIDAGEF2D.
   OTPREG         Y1N5RDF.
   OTPRGPRB       Y1N5RDF.
   OTPRGN         OTPRGN.
   OTPRGEND       OTPRGEND.
   OTMSN          OTLOSSN.
   OTSTN          OTLOSSN.
   OTABN          OTLOSSN.
   TOTPRG         TOTPRG.
   OTPREGS        OTPRGS.
   TOTPREGS_C     TOTPRG2F.
   TOTPREGS_R     TOTPRG2F.
   BIOKIDS        BIOADPTKD.
   ADOPKIDS       ADOPKIDS.
   ANYKIDS        N0Y1RDF.
   BIOADOPT       BIOADPTKD.
   PREGSNOW       PREGSNOW.
   NUMLIFE        NUMLIFE.
   BIODOB1        YEARFMT.
   BIODOB2        YEARFMT.
   BIODOB3        YEARFMT.
   BIODOB4        YEARFMT.
   BIODOB5        YEARFMT.
   BIODOB6        YEARFMT.
   BIODOB7        YEARFMT.
   BIODOB8        YEARFMT.
   BIODOB9        YEARFMT.
   BIODOB10       YEARFMT.
   BIOSEX1        MALFEMF.
   BIOSEX2        MALFEMF.
   BIOSEX3        MALFEMF.
   BIOSEX4        MALFEMF.
   BIOSEX5        MALFEMF.
   BIOSEX6        MALFEMF.
   BIOSEX7        MALFEMF.
   BIOSEX8        MALFEMF.
   BIOSEX9        MALFEMF.
   BIOSEX10       MALFEMF.
   BIOAGE1        KIDAGEF2F.
   BIOAGE2        KIDAGEF2F.
   BIOAGE3        KIDAGEF2F.
   BIOAGE4        KIDAGEF2F.
   BIOAGE5        KIDAGEF2F.
   BIOAGE6        KIDAGEF2F.
   BIOAGE7        KIDAGEF2F.
   BIOAGE8        KIDAGEF2F.
   BIOAGE9        KIDAGEF2F.
   BIOAGE10       KIDAGEF2F.
   BIOAGEGP1      KAGEGRPNAF.
   BIOAGEGP2      KAGEGRPNAF.
   BIOAGEGP3      KAGEGRPNAF.
   BIOAGEGP4      KAGEGRPNAF.
   BIOAGEGP5      KAGEGRPNAF.
   BIOAGEGP6      KAGEGRPNAF.
   BIOAGEGP7      KAGEGRPNAF.
   BIOAGEGP8      KAGEGRPNAF.
   BIOAGEGP9      KAGEGRPNAF.
   BIOAGEGP10     KAGEGRPNAF.
   BIOHH1         KIDHHF.
   BIOHH2         KIDHHF.
   BIOHH3         KIDHHF.
   BIOHH4         KIDHHF.
   BIOHH5         KIDHHF.
   BIOHH6         KIDHHF.
   BIOHH7         KIDHHF.
   BIOHH8         KIDHHF.
   BIOHH9         KIDHHF.
   BIOHH10        KIDHHF.
   BIOMOM1        RELATBIOF.
   BIOMOM2        RELATBIOF.
   BIOMOM3        RELATBIOF.
   BIOMOM4        RELATBIOF.
   BIOMOM5        RELATBIOF.
   BIOMOM6        RELATBIOF.
   BIOMOM7        RELATBIOF.
   BIOMOM8        RELATBIOF.
   BIOMOM9        RELATBIOF.
   BIOMOM10       RELATBIOF.
   BIOMAR1        N0Y1RDF.
   BIOMAR2        N0Y1RDF.
   BIOMAR3        N0Y1RDF.
   BIOMAR4        N0Y1RDF.
   BIOMAR5        N0Y1RDF.
   BIOMAR6        N0Y1RDF.
   BIOMAR7        N0Y1RDF.
   BIOMAR8        N0Y1RDF.
   BIOMAR9        N0Y1RDF.
   BIOMAR10       N0Y1RDF.
   BIOCOHB1       N0Y1RDF.
   BIOCOHB2       N0Y1RDF.
   BIOCOHB3       N0Y1RDF.
   BIOCOHB4       N0Y1RDF.
   BIOCOHB5       N0Y1RDF.
   BIOCOHB6       N0Y1RDF.
   BIOCOHB7       N0Y1RDF.
   BIOCOHB8       N0Y1RDF.
   BIOCOHB9       N0Y1RDF.
   BIOCOHB10      N0Y1RDF.
   BIOLRNPG1      LRNPRGF.
   BIOLRNPG2      LRNPRGF.
   BIOLRNPG3      LRNPRGF.
   BIOLRNPG4      LRNPRGF.
   BIOLRNPG5      LRNPRGF.
   BIOLRNPG6      LRNPRGF.
   BIOLRNPG7      LRNPRGF.
   BIOLRNPG8      LRNPRGF.
   BIOLRNPG9      LRNPRGF.
   BIOLRNPG10     LRNPRGF.
   BIOLIVNG11     BIOLIVNGF.
   BIOLIVNG12     BIOLIVNGF.
   BIOLIVNG13     BIOLIVNGF.
   BIOLIVNG21     BIOLIVNGF.
   BIOLIVNG22     BIOLIVNGF.
   BIOLIVNG23     BIOLIVNGF.
   BIOLIVNG31     BIOLIVNGF.
   BIOLIVNG32     BIOLIVNGF.
   BIOLIVNG33     BIOLIVNGF.
   BIOLIVNG41     BIOLIVNGF.
   BIOLIVNG42     BIOLIVNGF.
   BIOLIVNG43     BIOLIVNGF.
   BIOLIVNG51     BIOLIVNGF.
   BIOLIVNG52     BIOLIVNGF.
   BIOLIVNG53     BIOLIVNGF.
   BIOLIVNG61     BIOLIVNGF.
   BIOLIVNG62     BIOLIVNGF.
   BIOLIVNG63     BIOLIVNGF.
   BIOLIVNG71     BIOLIVNGF.
   BIOLIVNG72     BIOLIVNGF.
   BIOLIVNG73     BIOLIVNGF.
   BIOLIVNG81     BIOLIVNGF.
   BIOLIVNG82     BIOLIVNGF.
   BIOLIVNG83     BIOLIVNGF.
   BIOLIVNG91     BIOLIVNGF.
   BIOLIVNG92     BIOLIVNGF.
   BIOLIVNG93     BIOLIVNGF.
   BIOLIVNG101    BIOLIVNGF.
   BIOLIVNG102    BIOLIVNGF.
   BIOLIVNG103    BIOLIVNGF.
   BIOCHSIG1      Y1N5RDF.
   BIOCHSIG2      Y1N5RDF.
   BIOCHSIG3      Y1N5RDF.
   BIOCHSIG4      Y1N5RDF.
   BIOCHSIG5      Y1N5RDF.
   BIOCHSIG6      Y1N5RDF.
   BIOCHSIG7      Y1N5RDF.
   BIOCHSIG8      Y1N5RDF.
   BIOCHSIG9      Y1N5RDF.
   BIOCHSIG10     Y1N5RDF.
   BIOCHCRT1      Y1N5RDF.
   BIOCHCRT2      Y1N5RDF.
   BIOCHCRT3      Y1N5RDF.
   BIOCHCRT4      Y1N5RDF.
   BIOCHCRT5      Y1N5RDF.
   BIOCHCRT6      Y1N5RDF.
   BIOCHCRT7      Y1N5RDF.
   BIOCHCRT8      Y1N5RDF.
   BIOCHCRT9      Y1N5RDF.
   BIOCHCRT10     Y1N5RDF.
   BIOCHGEN1      Y1N5RDF.
   BIOCHGEN2      Y1N5RDF.
   BIOCHGEN3      Y1N5RDF.
   BIOCHGEN4      Y1N5RDF.
   BIOCHGEN5      Y1N5RDF.
   BIOCHGEN6      Y1N5RDF.
   BIOCHGEN7      Y1N5RDF.
   BIOCHGEN8      Y1N5RDF.
   BIOCHGEN9      Y1N5RDF.
   BIOCHGEN10     Y1N5RDF.
   BIOLVEVR1      Y1N5RDF.
   BIOLVEVR2      Y1N5RDF.
   BIOLVEVR3      Y1N5RDF.
   BIOLVEVR4      Y1N5RDF.
   BIOLVEVR5      Y1N5RDF.
   BIOLVEVR6      Y1N5RDF.
   BIOLVEVR7      Y1N5RDF.
   BIOLVEVR8      Y1N5RDF.
   BIOLVEVR9      Y1N5RDF.
   BIOLVEVR10     Y1N5RDF.
   BIOHWFAR1      MILESFMT.
   BIOHWFAR2      MILESFMT.
   BIOHWFAR3      MILESFMT.
   BIOHWFAR4      MILESFMT.
   BIOHWFAR5      MILESFMT.
   BIOHWFAR6      MILESFMT.
   BIOHWFAR7      MILESFMT.
   BIOHWFAR8      MILESFMT.
   BIOHWFAR9      MILESFMT.
   BIOHWFAR10     MILESFMT.
   BIOWANT1       DEFPROBF.
   BIOWANT2       DEFPROBF.
   BIOWANT3       DEFPROBF.
   BIOWANT4       DEFPROBF.
   BIOWANT5       DEFPROBF.
   BIOWANT6       DEFPROBF.
   BIOWANT7       DEFPROBF.
   BIOWANT8       DEFPROBF.
   BIOWANT9       DEFPROBF.
   BIOWANT10      DEFPROBF.
   BIOHSOON1      TIMINGF.
   BIOHSOON2      TIMINGF.
   BIOHSOON3      TIMINGF.
   BIOHSOON4      TIMINGF.
   BIOHSOON5      TIMINGF.
   BIOHSOON6      TIMINGF.
   BIOHSOON7      TIMINGF.
   BIOHSOON8      TIMINGF.
   BIOHSOON9      TIMINGF.
   BIOHSOON10     TIMINGF.
   BIOHOWSN1      BIOHWSNF.
   BIOHOWSN2      BIOHWSNF.
   BIOHOWSN3      BIOHWSNF.
   BIOHOWSN4      BIOHWSNF.
   BIOHOWSN5      BIOHWSNF.
   BIOHOWSN6      BIOHWSNF.
   BIOHOWSN7      BIOHWSNF.
   BIOHOWSN8      BIOHWSNF.
   BIOHOWSN9      BIOHWSNF.
   BIOHOWSN10     BIOHWSNF.
   BIOHPYPG1      SCALEF.
   BIOHPYPG2      SCALEF.
   BIOHPYPG3      SCALEF.
   BIOHPYPG4      SCALEF.
   BIOHPYPG5      SCALEF.
   BIOHPYPG6      SCALEF.
   BIOHPYPG7      SCALEF.
   BIOHPYPG8      SCALEF.
   BIOHPYPG9      SCALEF.
   BIOHPYPG10     SCALEF.
   CRALL          CRALL4F.
   CRALLU5        CNCALLU5FF.
   CRALL518       CRALL3F.
   CRMALU5        CNCALLU5FF.
   CRMAL518       CNCALLU5FF.
   CRFEMU5        CNCALLU5FF.
   CRFEM518       CNCALLU5FF.
   NCALL          NCINEXCHF.
   NCALLU5        NCALLU5F.
   NCALL518       NCINEXCHF.
   NCMALU5        NCALLU5F.
   NCMAL518       NCALLU5F.
   NCFEMU5        NCALLU5F.
   NCFEM518       NCALLU5F.
   RFAGE          KDAGFNAF.
   RFSEX          MLFMNAF2D.
   ROUTG04        OFTCHLDF.
   RMEAL04        OFTCHLDF.
   RERRAND04      OFTCHLDF.
   RPLAY04        OFTCHLDF.
   RREAD04        OFTCHLDF.
   RAFFECT04      OFTCHLDF.
   RPRAISE04      OFTCHLDF.
   RFEED04        OFTCHLDF.
   RBATH04        OFTCHLDF.
   RDIAPER04      OFTCHLDF.
   RBED04         OFTCHLDF.
   RAPPT04        OFTCHLDF.
   RDISC04        OFTCHLDF.
   ROUTG518       OFTCHLDF.
   RMEAL518       OFTCHLDF.
   RERRAND518     OFTCHLDF.
   RAFFECT518     OFTCHLDF.
   RPRAISE518     OFTCHLDF.
   RTAKE518       OFTCHLDF.
   RAPPT518       OFTCHLDF.
   RHELP518       OFTCHLDF.
   RDISC518       OFTCHLDF.
   RCLFR518       KNOWSFMT.
   RDO518         KNOWSFMT.
   NRFAGE         KDAGFNAF.
   NRFSEX         MLFMNAF2D.
   NRVISIT04      OFTCHLDF.
   NRSATVIS04     NRSATVISF.
   NROUTG04       OFTCHLDF.
   NRMEAL04       OFTCHLDF.
   NRERRAND04     OFTCHLDF.
   NROVRNT04      OFTCHLDF.
   NRPLAY04       OFTCHLDF.
   NRREAD04       OFTCHLDF.
   NRAFFECT04     OFTCHLDF.
   NRPRAISE04     OFTCHLDF.
   NRFEED04       OFTCHLDF.
   NRBATH04       OFTCHLDF.
   NRDIAPER04     OFTCHLDF.
   NRBED04        OFTCHLDF.
   NRAPPT04       OFTCHLDF.
   NRDISC04       OFTCHLDF.
   NRVISIT518     OFTCHLDF.
   NRSATVIS518    NRSATVISF.
   NROUTG518      OFTCHLDF.
   NRMEAL518      OFTCHLDF.
   NRERRAND518    OFTCHLDF.
   NROVRNT518     OFTCHLDF.
   NRAFFECT518    OFTCHLDF.
   NRPRAISE518    OFTCHLDF.
   NRTAKE518      OFTCHLDF.
   NRAPPT518      OFTCHLDF.
   NRHELP518      OFTCHLDF.
   NRDISC518      OFTCHLDF.
   NRCLFR518      KNOWSFMT.
   NRDO518        KNOWSFMT.
   NRMONEY        Y1N5RDF.
   NREG           NREG.
   NRAGREE        Y1N5RDF.
   NRCHSUPPYR     NRCHSUPPYR.
   COPARENT       AGDGFMT.
   RWANT          Y1N5RDF.
   PROBWANT       PROBWANT.
   JINTEND        Y1N5RDF.
   JSUREINT       JSUREINT.
   JINTENDN       NCINEXCHF.
   JEXPECTL       NCINEXCHF.
   JEXPECTS       EXPECTSF.
   JINTNEXT       INTNEXT.
   INTEND         DEFPROBF.
   INTENDN        NCINEXCHF.
   EXPECTL        NCINEXCHF.
   EXPECTS        EXPECTSF.
   INTNEXT        INTNEXT.
   USUALCAR       Y1N5RDF.
   USLPLACE       PLCVISF.
   USL12MOS       Y1N5RDF.
   CURRCOV        Y1N5RDF.
   COVERHOW01     COVERHOWF.
   COVERHOW02     COVERHOWF.
   COVERHOW03     COVERHOWF.
   COVERHOW04     COVERHOWF.
   PARINSUR       Y1N5RDF.
   INS_EXCH       Y1N5RDF.
   INS_PREM       Y1N5RDF.
   COVER12        Y1N5RDF.
   NUMNOCOV       NUMNOCOV.
   YOUGOFPC       Y1N5RDF.
   WHENGOFP       WHENGOFP.
   YOUFPSVC1      YOUFPSVCF.
   YOUFPSVC2      YOUFPSVCF.
   YOUFPSVC3      YOUFPSVCF.
   YOUFPSVC4      YOUFPSVCF.
   YOUFPSVC5      YOUFPSVCF.
   YOUFPSVC6      YOUFPSVCF.
   DEAF           Y1N5RDF.
   BLIND          Y1N5RDF.
   DIFDECIDE      Y1N5RDF.
   DIFWALK        Y1N5RDF.
   DIFDRESS       Y1N5RDF.
   DIFOUT         Y1N5RDF.
   EVRCANCER      Y1N5RDF.
   AGECANCER      AGECANCER.
   CANCTYPE       CANCTYPEF.
   ALCORISK       ALCORISK.
   VISIT12MO1     VISIT12MOF.
   VISIT12MO2     VISIT12MOF.
   VISIT12MO3     VISIT12MOF.
   SVC12MO1       SVC12MOF.
   SVC12MO2       SVC12MOF.
   SVC12MO3       SVC12MOF.
   SVC12MO4       SVC12MOF.
   SVC12MO5       SVC12MOF.
   SVC12MO6       SVC12MOF.
   SVC12MO7       SVC12MOF.
   SVC12MO8       SVC12MOF.
   NUMVISIT       NUMVISIT.
   PLACEVIS01     PLCVISF.
   PLACEVIS02     PLCVISF.
   PLACEVIS03     PLCVISF.
   PLACEVIS04     PLCVISF.
   PLACEVIS05     PLCVISF.
   PLACEVIS06     PLCVISF.
   SVCPAY1        SVCPAYF.
   SVCPAY2        SVCPAYF.
   SVCPAY3        SVCPAYF.
   SVCPAY4        SVCPAYF.
   SVCPAY5        SVCPAYF.
   TALKSA         TALKSA.
   TALKEC         Y1N5RDF.
   TALKDM         Y1N5RDF.
   WHYPSTD        WHYPSTD.
   BARRIER1       BARRIERF.
   BARRIER2       BARRIERF.
   BARRIER3       BARRIERF.
   BARRIER4       BARRIERF.
   BLDPRESS       Y1N5RDF.
   HIGHBP         HIGHBP.
   BPMEDS         Y1N5RDF.
   ASKSMOKE       Y1N5RDF.
   INFHELP        Y1N5RDF.
   INFSVCS1       INFSVCSF.
   INFSVCS2       INFSVCSF.
   INFSVCS3       INFSVCSF.
   INFSVCS4       INFSVCSF.
   INFSVCS5       INFSVCSF.
   INFSVCS6       INFSVCSF.
   INFTEST        INFTEST.
   WHOINSEM       WHOINSEM.
   INFHLPNW       Y1N5RDF.
   LASTVIS_M      MNTHFMT.
   LASTVIS_Y      YEARFMT.
   CMINFVIS       CMFMT.
   INFRTHIS_1     INFRTHISNF.
   INFRTHIS_2     INFRTHISNF.
   DONBLOOD       Y1N5RDF.
   HIVTEST        Y1N5RDF.
   NOHIVTST       NOHIVTST.
   WHENHIV_M      WHNHIVMF.
   WHENHIV_Y      YEARFMT.
   CMHIVTST       CMFMT.
   HIVTSTYR       Y1N5RDF.
   HIVRESULT      YESNONAF.
   WHYNOGET       WHYNOGET.
   PLCHIV         PLCHIV.
   RHHIVT1        YESNONAF.
   RHHIVT21       RHHIVT2F.
   HIVTST         HIVTST.
   WHOSUGG        WHOSUGG.
   TALKDOCT       Y1N5RDF.
   AIDSTALK01     AIDSTALKF.
   AIDSTALK02     AIDSTALKF.
   AIDSTALK03     AIDSTALKF.
   AIDSTALK04     AIDSTALKF.
   AIDSTALK05     AIDSTALKF.
   AIDSTALK06     AIDSTALKF.
   AIDSTALK07     AIDSTALKF.
   AIDSTALK08     AIDSTALKF.
   AIDSTALK09     AIDSTALKF.
   AIDSTALK10     AIDSTALKF.
   AIDSTALK11     AIDSTALKF.
   SAMEADD        Y1N5RDF.
   CNTRY10        Y1N5RDF.
   BRNOUT         Y1N5RDF.
   YRSTRUS        YEARFMT.
   RELRAISD       CURR_RSD.
   ATTND14        ATTNDFMT.
   RELCURR        CURR_RSD.
   RELTRAD        RELTRAD.
   FUNDAM1        FUNDAMF.
   FUNDAM2        FUNDAMF.
   FUNDAM3        FUNDAMF.
   FUNDAM4        FUNDAMF.
   RELDLIFE       RELDLIFE.
   ATTNDNOW       ATTNDFMT.
   MILSVC         Y1N5RDF.
   WRK12MOS       WRK12MOS.
   FPT12MOS       FTPTSEF.
   DOLASTWK1      DOLASTWKF.
   DOLASTWK2      DOLASTWKF.
   DOLASTWK3      DOLASTWKF.
   DOLASTWK4      DOLASTWKF.
   DOLASTWK5      DOLASTWKF.
   DOLASTWK6      DOLASTWKF.
   RWRKST         Y1N5C.
   WORKP12        Y1N5C.
   RPAYJOB        Y1N5RDF.
   RNUMJOB        RNUMJOB.
   RFTPTX         FTPTSEF.
   REARNTY        Y1N5C.
   SPLSTWK1       SPLSTWKF.
   SPLSTWK2       SPLSTWKF.
   SPLSTWK3       SPLSTWKF.
   SPLSTWK4       SPLSTWKF.
   SPLSTWK5       SPLSTWKF.
   SPWRKST        Y1N5C.
   SPPAYJOB       Y1N5RDF.
   SPNUMJOB       SPNUMJOB.
   SPFTPTX        FTPTSEF.
   SPEARNTY       Y1N2C.
   SAMESEX        AGDGFMT.
   CHSUPPOR       AGDGFMT.
   REACTSLF       REACTSLF.
   CHBOTHER       CHBOTHER.
   SEXNEEDS       AGDGFMT.
   WHENSICK       AGDGFMT.
   SHOWPAIN       AGDGFMT.
   PMARCOHB       Y1N5C.
   COHCHANCE      DEFPROBF.
   MARRCHANCE     DEFPROBF.
   PMARCOH        DEFPROBF.
   ACASILANG      ACASILANG.
   GENHEALT       GENHEALT.
   INCHES         INCHES.
   RWEIGHT        RWEIGHT.
   BMI            BMI.
   DRWEIGH        YESNONAF.
   TELLWGHT       TELLWGHT.
   WGHTSCRN       YESNONAF.
   ENGSPEAK       ENGSPEAK.
   NOBEDYR        YESNONAF.
   STAYREL        YESNONAF.
   JAILED         YESNONAF.
   JAILED2        YESNONAF.
   FRQJAIL        FRQJAIL.
   FRQJAIL2       FRQJAIL2F.
   EVSUSPEN       YESNONAF.
   GRADSUSP       GRADSUSP.
   SMK100         YESNONAF.
   AGESMK         CASISMK.
   SMOKE12        SMOKE12F.
   SMKSTOP        YESNONAF.
   DRINK12        DRINKF.
   UNIT30D        UNIT30D.
   DRINK30D       DRNK30D.
   DRINKDAY       DRNKNUM.
   BINGE30        BNGE30T.
   DRNKMOST       DRNKNUM.
   BINGE12        DRINKF.
   POT12          FREQ2VF.
   COC12          DRUGFRF.
   CRACK12        DRUGFRF.
   CRYSTMTH12     DRUGFRF.
   INJECT12       DRUGFRF.
   MADEPREG       YESNONAF.
   PREGTOT2       NUMF.
   PREGACASI      NUMNEVF.
   NUMABORT       ACASINUM.
   NUMLIVEB       ACASINUM.
   TOLDPREG       YESNONAF.
   WHATHAPP       WHATHAPP.
   FEMTOUCH       YESNONAF.
   VAGSEX         YESNONAF.
   AGEVAGR        SEX1AGENAF.
   CONDVAG        YESNONAF.
   COND1BRK       YESNONAF.
   COND1OFF       YESNONAF.
   WHYCONDL       WHYCONDF.
   GETORALF       YESNONAF.
   CONDFELL       YESNONAF.
   GIVORALF       YESNONAF.
   ANYORAL        Y1N5NAC.
   TIMING         TIMING.
   ANALSEX        YESNONAF.
   CONDANAL       YESNONAF.
   OPPSEXANY      Y1N5NAC.
   OPPSEXGEN      Y1N5NAC.
   CONDSEXL       YESNONAF.
   WANTSEX1       WANTSEX1F.
   HOWOLD         SEX1AGENAF.
   EVRFORCD       YESNONAF.
   AGEFORC1       SEX1AGENAF.
   GIVNDRG2       YESNONAF.
   SHEBIGOL       YESNONAF.
   ENDRELA2       YESNONAF.
   WRDPRES2       YESNONAF.
   THRTPHY2       YESNONAF.
   PHYSHRT2       YESNONAF.
   HELDDWN2       YESNONAF.
   PARTSLIF_1     PARTLIF.
   PARTSLFV       YESNONAF.
   PARTSLIF_2     PARTLIF.
   OPPLIFENUM     PARTLIF.
   PARTS12_1      PARTS12MB.
   PARTS12V       YESNONAF.
   PARTS12_2      PARTS12MB.
   OPPYEARNUM     PARTS12MB.
   NEWYEAR        PRT12F2F.
   NEWLIFE        PARTLIF2F.
   VAGNUM12       PRT12F2F.
   ORALNUM12      PRT12F2F.
   ANALNUM12      PRT12F2F.
   NONMONOG       YESNONAF.
   NNONMONOG1     NNONMONOG1F.
   NNONMONOG2     NNONMONOG2F.
   NNONMONOG3     NNONMONOG2F.
   FEMSHT12       YESNONAF.
   JOHNFREQ       YESNONAF.
   PROSTFRQ       YESNONAF.
   HIVFEM12       YESNONAF.
   GIVORALM       YESNONAF.
   GETORALM       YESNONAF.
   ORALCONDM      YESNONAF.
   ANALSEX2       YESNONAF.
   ANALCONDM1     YESNONAF.
   ANALSEX3       YESNONAF.
   ANALCONDM2     YESNONAF.
   MALESEX        YESNONAF.
   SAMESEXANY     Y1N5NAC.
   MALPRTAGE      PRTAGE2NAF.
   MALPRTHRACE    NRACE.
   EVRFORC2       YESNONAF.
   AGEFORC2       SEX1AGENAF.
   GIVNDRG3       YESNONAF.
   HEBIGOLD       YESNONAF.
   ENDRELA3       YESNONAF.
   WRDPRES3       YESNONAF.
   THRTPHY3       YESNONAF.
   PHYSHRT3       YESNONAF.
   HELDDWN3       YESNONAF.
   MALEPRTS_1     MALPRT.
   MALEPRTSV      YESNONAF.
   MALEPRTS_2     MALPRT.
   SAMLIFENUM     MALPRT.
   MALPRT12_1     PRT12F.
   MALPRT12V      YESNONAF.
   MALPRT12_2     PRT12F.
   SAMYEARNUM     PRT12F.
   SAMORAL12      TYPSEXF.
   RECEPANAL12    TYPSEXF.
   INSERANAL12    TYPSEXF.
   SAMESEX1       SMSX1F.
   MSAMEREL       MSAMEREL.
   MSMNONMON      TYPSEXF.
   MALSHT12       YESNONAF.
   JOHN2FRQ       YESNONAF.
   PROS2FRQ       YESNONAF.
   HIVMAL12       YESNONAF.
   MSMWEB12       YESNONAF.
   MSMSORT12      MSMSORT12F.
   CNDLSMAL       YESNONAF.
   CONDALLS       YESNONAF.
   MFLASTP        MALFEMNAF.
   WHYCOND        WHYCONDF.
   ATTRACT        ATTRACT.
   ORIENT_A       ORIENT_A.
   ORIENT_B       ORIENT_B.
   CONFCONC       YESNONAF.
   TIMALON        TIMALON.
   RISKCHEK1      YESNONAF.
   RISKCHEK2      YESNONAF.
   RISKCHEK3      YESNONAF.
   RISKCHEK4      YESNONAF.
   RECTDOUCH      FREQ2VF.
   STDTST12       YESNONAF.
   STDSITE12      YESNONAF.
   STDTRT12       YESNONAF.
   GON            YESNONAF.
   CHLAM          YESNONAF.
   HERPES         YESNONAF.
   GENWARTS       YESNONAF.
   SYPHILIS       YESNONAF.
   EVRINJECT      YESNONAF.
   EVRSHARE       YESNONAF.
   EARNTYPE       INCWMYF.
   EARN           EARN.
   EARNDK1        YESNONAF.
   EARNDK2        YESNONAF.
   EARNDK3        YESNONAF.
   EARNDK4        YESNONAF.
   TOINCWMY       INCWMYF.
   TOTINC         EARN.
   FMINCDK1       FMINCDK1F.
   FMINCDK2       YESNONAF.
   FMINCDK3       YESNONAF.
   FMINCDK4       YESNONAF.
   FMINCDK5       YESNONAF.
   PUBASST        YESNONAF.
   PUBASTYP1      PUBASTYPF.
   FOODSTMP       YESNONAF.
   WIC            YESNONAF.
   HLPTRANS       YESNONAF.
   HLPCHLDC       YESNONAF.
   HLPJOB         YESNONAF.
   FREEFOOD       YESNONAF.
   HUNGRY         YESNONAF.
   MED_COST       YESNONAF.
   AGER           AGER.
   FMARITAL       FMARITAL.
   RMARITAL       RMARITAL.
   EDUCAT         EDUCAT2F.
   HIEDUC         HIEDUC.
   HISPANIC       HISPANIC.
   RACE           RACE.
   HISPRACE       HSPLSXPRC.
   HISPRACE2      HISPRACE2F.
   NUMKDHH        NUMKDHH.
   NUMFMHH        NUMFMHH.
   HHFAMTYP       HHFAMTYP.
   HHPARTYP       HHPARTYP.
   NCHILDHH       NCHILDHH.
   HHKIDTYP       HHKIDTYP.
   CSPBBHH        CSPBIO.
   CSPSBHH        CSPNOT.
   CSPOKDHH       CSPNOT.
   INTCTFAM       INTCTFAM.
   PARAGE14       PARAGE14F.
   EDUCMOM        EDUCMOMF.
   AGEMOMB1       AGEMOMBF.
   FMARNO         FMARNO.
   AGER_I         IMPFLG.
   FMARITAL_I     IMPFLG.
   RMARITAL_I     IMPFLG.
   EDUCAT_I       IMPFLG.
   HIEDUC_I       IMPFLG.
   HISPANIC_I     IMPFLG.
   RACE_I         IMPFLG.
   HISPRACE_I     IMPFLG.
   HISPRACE2_I    IMPFLG.
   NUMKDHH_I      IMPFLG.
   NUMFMHH_I      IMPFLG.
   HHFAMTYP_I     IMPFLG.
   HHPARTYP_I     IMPFLG.
   NCHILDHH_I     IMPFLG.
   HHKIDTYP_I     IMPFLG.
   CSPBBHH_I      IMPFLG.
   CSPSBHH_I      IMPFLG.
   CSPOKDHH_I     IMPFLG.
   INTCTFAM_I     IMPFLG.
   PARAGE14_I     IMPFLG.
   EDUCMOM_I      IMPFLG.
   AGEMOMB1_I     IMPFLG.
   FMARNO_I       IMPFLG.
   HADSEX         HADSEX.
   SEXONCE        SEXONCE.
   VRY1STSX       CMFMT.
   FIRSTPFLAG     FPFLAG.
   VRY1STAG       VRY1STAG.
   ELAPSED        ELAPSED.
   SEXMAR         MARCATF.
   SEXUNION       MARCATG.
   FSEXRLTN       FSEXRLTN.
   SEX1MTHD1      SEX1MTHDF.
   SEX1MTHD2      SEX1MTHDF.
   SEX1MTHD3      SEX1MTHDF.
   SEX1MTHD4      SEX1MTHDF.
   LSEXDATE       CMFMT.
   SEX3MO         SEX3MO.
   SEX12MO        SEX3MO.
   LSEXRAGE       LSEXRAGE.
   LSEXPRAC       HSPLSXPRC.
   LSEXRLTN       LSEXRLTN.
   LSEXUSE1       LSEXUSEF.
   LSEXUSE2       LSEXUSEF.
   LSEXUSE3       LSEXUSEF.
   LSEXUSE4       LSEXUSEF.
   METH12M1       METH12MF.
   METH12M2       METH12MF.
   METH12M3       METH12MF.
   METH12M4       METH12MF.
   METH3M1        METH3MF.
   METH3M2        METH3MF.
   METH3M3        METH3MF.
   METH3M4        METH3MF.
   NUMP3MOS       NUMP3MOS.
   LIFPRTNR       LIFPRTNR.
   PARTS1YR       PARTS1YR.
   PARTDUR1       PARTDUR.
   PARTDUR2       PARTDUR.
   PARTDUR3       PARTDUR.
   COHEVER        COHEVER.
   EVMARCOH       EVMARCOH.
   PMARRNO        COHNUM.
   NONMARR        COHNUM.
   TIMESCOH       COHNUM.
   MARDAT01       YEARFMT.
   MARDAT02       YEARFMT.
   MARDAT03       YEARFMT.
   MARDAT04       YEARFMT.
   MAREND01       MAREND.
   MAREND02       MAREND.
   MAREND03       MAREND.
   MAREND04       MAREND.
   MARDIS01       YEARFMT.
   MARDIS02       YEARFMT.
   MARDIS03       YEARFMT.
   MARDIS04       YEARFMT.
   MAR1DISS       MARCATA.
   PREMARW1       Y1N2RECF.
   COHAB1         YEARFMT.
   COHSTAT        COHSTAT.
   COHOUT         COHOUT.
   COH1DUR        MARCATA.
   CSPBIOKD       BIOCHRFMT.
   DATBABY1       YEARFMT.
   AGEBABY1       AGEBABY.
   B1PREMAR       B1PREMAR.
   MARBABY1       MARBABYF.
   CEBOW          CEBOW.
   CEBOWC         BIOCHRFMT.
   CEBOWP         BIOCHRFMT.
   EVRNOPAT       EVRNOPAT.
   NONLIVEB       NONLIVEB.
   COMPREG        COMPREG.
   ABORTION       PREGFMT.
   LOSSNUM        PREGFMT.
   PARENT01       RELATBIOF.
   PARENT02       RELATBIOF.
   PARENT03       RELATBIOF.
   PARENT04       RELATBIOF.
   PARENT05       RELATBIOF.
   PARENT06       RELATBIOF.
   PARENT07       RELATBIOF.
   PARENT08       RELATBIOF.
   PARENT09       RELATBIOF.
   PARENT10       RELATBIOF.
   WANTB01        WANTBF.
   WANTB02        WANTBF.
   WANTB03        WANTBF.
   WANTB04        WANTBF.
   WANTB05        WANTBF.
   WANTB06        WANTBF.
   WANTB07        WANTBF.
   WANTB08        WANTBF.
   WANTB09        WANTBF.
   WANTB10        WANTBF.
   HADSEX_I       IMPFLG.
   SEXONCE_I      IMPFLG.
   VRY1STSX_I     IMPFLG.
   VRY1STAG_I     IMPFLG.
   SEXMAR_I       IMPFLG.
   SEXUNION_I     IMPFLG.
   FSEXRLTN_I     IMPFLG.
   SEX1MTHD1_I    IMPFLG.
   SEX1MTHD2_I    IMPFLG.
   SEX1MTHD3_I    IMPFLG.
   SEX1MTHD4_I    IMPFLG.
   LSEXDATE_I     IMPFLG.
   SEX3MO_I       IMPFLG.
   SEX12MO_I      IMPFLG.
   LSEXRAGE_I     IMPFLG.
   LSEXRLTN_I     IMPFLG.
   LSEXUSE1_I     IMPFLG.
   LSEXUSE2_I     IMPFLG.
   LSEXUSE3_I     IMPFLG.
   LSEXUSE4_I     IMPFLG.
   METH12M1_I     IMPFLG.
   METH12M2_I     IMPFLG.
   METH12M3_I     IMPFLG.
   METH12M4_I     IMPFLG.
   METH3M1_I      IMPFLG.
   METH3M2_I      IMPFLG.
   METH3M3_I      IMPFLG.
   METH3M4_I      IMPFLG.
   NUMP3MOS_I     IMPFLG.
   LIFPRTNR_I     IMPFLG.
   PARTS1YR_I     IMPFLG.
   PARTDUR1_I     IMPFLG.
   PARTDUR2_I     IMPFLG.
   PARTDUR3_I     IMPFLG.
   COHEVER_I      IMPFLG.
   EVMARCOH_I     IMPFLG.
   PMARRNO_I      IMPFLG.
   NONMARR_I      IMPFLG.
   TIMESCOH_I     IMPFLG.
   MARDAT01_I     IMPFLG.
   MARDAT02_I     IMPFLG.
   MARDAT03_I     IMPFLG.
   MARDAT04_I     IMPFLG.
   MAREND01_I     IMPFLG.
   MAREND02_I     IMPFLG.
   MAREND03_I     IMPFLG.
   MAREND04_I     IMPFLG.
   MARDIS01_I     IMPFLG.
   MARDIS02_I     IMPFLG.
   MARDIS03_I     IMPFLG.
   MARDIS04_I     IMPFLG.
   MAR1DISS_I     IMPFLG.
   PREMARW1_I     IMPFLG.
   COHAB1_I       IMPFLG.
   COHSTAT_I      IMPFLG.
   COHOUT_I       IMPFLG.
   COH1DUR_I      IMPFLG.
   CSPBIOKD_I     IMPFLG.
   DATBABY1_I     IMPFLG.
   AGEBABY1_I     IMPFLG.
   B1PREMAR_I     IMPFLG.
   MARBABY1_I     IMPFLG.
   CEBOW_I        IMPFLG.
   CEBOWC_I       IMPFLG.
   CEBOWP_I       IMPFLG.
   EVRNOPAT_I     IMPFLG.
   NONLIVEB_I     IMPFLG.
   COMPREG_I      IMPFLG.
   ABORTION_I     IMPFLG.
   LOSSNUM_I      IMPFLG.
   PARENT01_I     IMPFLG.
   PARENT02_I     IMPFLG.
   PARENT03_I     IMPFLG.
   PARENT04_I     IMPFLG.
   PARENT05_I     IMPFLG.
   PARENT06_I     IMPFLG.
   PARENT07_I     IMPFLG.
   PARENT08_I     IMPFLG.
   PARENT09_I     IMPFLG.
   PARENT10_I     IMPFLG.
   WANTB01_I      IMPFLG.
   WANTB02_I      IMPFLG.
   WANTB03_I      IMPFLG.
   WANTB04_I      IMPFLG.
   WANTB05_I      IMPFLG.
   WANTB06_I      IMPFLG.
   WANTB07_I      IMPFLG.
   WANTB08_I      IMPFLG.
   WANTB09_I      IMPFLG.
   WANTB10_I      IMPFLG.
   DADTYPE        DADTYPE.
   DADTYPU5       DADTYPU5F.
   DADTYP518      DADTYP518F.
   NUMCRU18       BIOCHRFMT2F.
   NUMNCU18       BIOCHRFMT.
   SUPP12MO       SUPP12MO.
   DADTYPE_I      IMPFLG.
   DADTYPU5_I     IMPFLG.
   DADTYP518_I    IMPFLG.
   NUMCRU18_I     IMPFLG.
   NUMNCU18_I     IMPFLG.
   SUPP12MO_I     IMPFLG.
   INTENT         INTENT.
   ADDEXP         ADDEXP.
   INTENT_I       IMPFLG.
   ADDEXP_I       IMPFLG.
   CURR_INS       CURR_INS.
   INFEVER        INFEVER.
   EVHIVTST       EVHIVTST.
   CURR_INS_I     IMPFLG.
   INFEVER_I      IMPFLG.
   EVHIVTST_I     IMPFLG.
   METRO          METRO.
   RELIGION       RELIGION.
   LABORFOR       LABORFOR.
   METRO_I        IMPFLG.
   RELIGION_I     IMPFLG.
   LABORFOR_I     IMPFLG.
   POVERTY        POVERTY.
   TOTINCR        TOTINCR.
   PUBASSIS       PUBASSIS.
   POVERTY_I      IMPFLG.
   TOTINCR_I      IMPFLG.
   PUBASSIS_I     IMPFLG.
   WGT2015_2017   WGTFMT.
   CMINTVW        CMFMT.
   CMLSTYR        CMFMT.
   CMFIVYR        CMFMT.
   QUARTER        $QUARTER.
   PHASE          $PHASE.
   INTVWYEAR      $YEARF.
   INTVLNGTH      INTVLNGTH. ;
*/


* SAS LENGTH STATEMENT;

LENGTH
   CASEID 6                 RSCRNINF 3               RSCRAGE 3             
   RSCRHISP 3               RSCRRACE 3               AGE_A 3               
   AGE_R 3                  AGESCRN 3                HISP 3                
   HISPGRP 3                PRIMLANG1 3              PRIMLANG2 3           
   PRIMLANG3 3              ROSCNT 3                 MARSTAT 3             
   FMARSTAT 3               FMARIT 3                 EVRMARRY 3            
   WPLOCALE 3               WOMREL 3                 GOSCHOL 3             
   VACA 3                   HIGRADE 3                COMPGRD 3             
   DIPGED 3                 EARNHS_Y 4               HISCHGRD 3            
   LSTGRADE 3               MYSCHOL_Y 4              HAVEDEG 3             
   DEGREES 3                EARNBA_Y 4               EXPSCHL 3             
   EXPGRADE 3               WTHPARNW 3               ONOWN 3               
   ONOWN18 3                INTACT 3                 PARMARR 3             
   INTACT18 3               LVSIT14F 3               LVSIT14M 3            
   WOMRASDU 3               MOMDEGRE 3               MOMWORKD 3            
   MOMFSTCH 3               MOM18 3                  MANRASDU 3            
   R_FOSTER 3               EVRFSTER 3               MNYFSTER 3            
   DURFSTER 3               TIMESMAR 3               EVCOHAB1 3            
   NUMCOH1 3                EVCOHAB2 3               NUMCOH2 3             
   EVRCOHAB 3               NUMWIFE 4                NUMCOHAB 4            
   EVERSEX 3                RHADSEX 3                SXMTONCE 3            
   YNOSEX 3                 TALKPAR1 3               TALKPAR2 3            
   TALKPAR3 3               TALKPAR4 3               TALKPAR5 3            
   TALKPAR6 3               TALKPAR7 3               SEDNO 3               
   SEDNOG 3                 SEDNOSX 3                SEDBC 3               
   SEDBCLC1 3               SEDBCLC2 3               SEDBCLC3 3            
   SEDBCLC4 3               SEDBCG 3                 SEDBCSX 3             
   SEDWHBC 3                SEDWHBCG 3               SEDWBCSX 3            
   SEDCOND 3                SEDCONDG 3               SEDCONSX 3            
   SEDSTD 3                 SEDSTDG 3                SEDSTDSX 3            
   SEDHIV 3                 SEDHIVG 3                SEDHIVSX 3            
   SEDABST 3                SEDABLC1 3               SEDABLC2 3            
   SEDABLC3 3               SEDABLC4 3               SEDABSTG 3            
   SEDABSSX 3               EVEROPER 3               TYPEOPER 3            
   STEROPER 3               VASEC_Y 4                PLCSTROP 3            
   RVRSVAS 3                VASREV_Y 3               RSURGSTR 3            
   FATHPOSS 3               FATHDIFF 3               RSTRSTAT 3            
   LIFEPRT 3                LIFEPRTS 3               SXMON12 3             
   MON12PRT 3               MON12PRTS 3              SEXSTAT 3             
   P12MOCONO 3              P12MOCON 3               SEXFREQ 4             
   CONFREQ 4                P1RLTN1 3                P1CURRWIFE 3          
   P1CURRSEP 3              P1RLTN2 3                P1COHABIT 3           
   P1SXLAST_M 3             P1SXLAST_Y 4             CMLSXP1 4             
   P2RLTN1 3                P2CURRWIFE 3             P2CURRSEP 3           
   P2RLTN2 3                P2COHABIT 3              P2SXLAST_M 3          
   P2SXLAST_Y 4             CMLSXP2 4                P3RLTN1 3             
   P3CURRWIFE 3             P3CURRSEP 3              P3RLTN2 3             
   P3COHABIT 3              P3SXLAST_M 3             P3SXLAST_Y 4          
   CMLSXP3 4                P1RELATION 3             P2RELATION 3          
   P3RELATION 3             FIRST 3                  MARRDATE_Y 4          
   HISAGEM 3                LIVTOGWF 3               STRTWFCP_Y 4          
   HISAGEC 3                CMSTRTWP 4               ENGATHEN 3            
   WILLMARR 3               CWPDOB_Y 4               CWPAGE 3              
   CWPRACE 3                CWPNRACE 3               CWPEDUCN 3            
   CWPBORN 3                CWPMARBF 3               CWPSX1WN_M 3          
   CWPSX1WN_Y 4             CWPSX1AG 3               CMFSXCWP 4            
   AGEFSXCWP 3              CWPSX1RL 3               CWPFUSE 3             
   CWPFMET01 3              CWPFMET02 3              CWPFMET03 3           
   CWPFMET04 3              CWPFMET05 3              CWPOPSTR 3            
   CWPTYPOP1 3              CWPTYPOP2 3              CWPTOTST 3            
   CWPREVST 3               PSURGSTR 3               CWPPOSS 3             
   CWPDIFF 3                PSTRSTAT 3               CWPLSXWN_M 3          
   CWPLSXWN_Y 4             CMLSXCWP 4               CWPLUSE1 3            
   CWPLMET11 3              CWPLMET12 3              CWPLMET13 3           
   CWPLUSE2 3               DKCWPLUSE 3              CWPLMET201 3          
   CWPLMET202 3             DKCWPLMET 3              CWPLSXUSE 3           
   CWPRECBC 3               CWPALLBC01 3             CWPALLBC02 3          
   CWPALLBC03 3             CWPALLBC04 3             CWPALLBC05 3          
   CWPBCMST 3               CONDFREQ 4               CWPNOFRQ 3            
   CWPBIOKD 3               CWPNUMKD 3               PARTFATH 3            
   CWPCHSEX 3               CWPCHDOB_Y 4             CWPCHMAR 3            
   CWPCHRES 3               CWPCHLRN 3               CWPCHLIV1 3           
   CWPCHLIV2 3              CWPCHAGE 3               CWPCHSIG 3            
   CWPCHCRT 3               CWPCHGEN 3               CWPCHEVR 3            
   CWPCHFAR 6               CWPCHWNT 3               CWPCHSON 3            
   CWPSOONN 4               CWPSOONMY 3              CWPCHHPY 3            
   CWPCHSEX2 3              CWPCHDOB_Y2 4            MULTBIRT2 3           
   CWPCHMAR2 3              CWPCHRES2 3              CWPCHLRN2 3           
   CWPCHLIV10 3             CWPCHLIV11 3             CWPCHAGE2 3           
   CWPCHSIG2 3              CWPCHCRT2 3              CWPCHGEN2 3           
   CWPCHEVR2 3              CWPCHFAR2 6              CWPCHWNT2 3           
   CWPCHSON2 3              CWPSOONN2 3              CWPSOONMY2 3          
   CWPCHHPY2 3              CWPCHSEX3 3              CWPCHDOB_Y3 4         
   MULTBIRT3 3              CWPCHMAR3 3              CWPCHRES3 3           
   CWPCHLRN3 3              CWPCHLIV19 3             CWPCHLIV20 3          
   CWPCHAGE3 3              CWPCHSIG3 3              CWPCHCRT3 3           
   CWPCHGEN3 3              CWPCHEVR3 3              CWPCHFAR3 4           
   CWPCHWNT3 3              CWPCHSON3 3              CWPSOONN3 4           
   CWPSOONMY3 3             CWPCHHPY3 3              CWPCHSEX4 3           
   CWPCHDOB_Y4 4            MULTBIRT4 3              CWPCHMAR4 3           
   CWPCHRES4 3              CWPCHLRN4 3              CWPCHLIV28 3          
   CWPCHLIV29 3             CWPCHAGE4 3              CWPCHSIG4 3           
   CWPCHCRT4 3              CWPCHGEN4 3              CWPCHEVR4 3           
   CWPCHFAR4 4              CWPCHWNT4 3              CWPCHSON4 3           
   CWPSOONN4 3              CWPSOONMY4 3             CWPCHHPY4 3           
   CWPCHSEX5 3              CWPCHDOB_Y5 4            MULTBIRT5 3           
   CWPCHMAR5 3              CWPCHRES5 3              CWPCHLRN5 3           
   CWPCHLIV37 3             CWPCHLIV38 3             CWPCHAGE5 3           
   CWPCHSIG5 3              CWPCHCRT5 3              CWPCHGEN5 3           
   CWPCHEVR5 3              CWPCHFAR5 4              CWPCHWNT5 3           
   CWPCHSON5 3              CWPSOONN5 3              CWPSOONMY5 3          
   CWPCHHPY5 3              CWPCHSEX6 3              CWPCHDOB_Y6 4         
   MULTBIRT6 3              CWPCHMAR6 3              CWPCHRES6 3           
   CWPCHLRN6 3              CWPCHLIV46 3             CWPCHLIV47 3          
   CWPCHAGE6 3              CWPCHSIG6 3              CWPCHCRT6 3           
   CWPCHGEN6 3              CWPCHEVR6 3              CWPCHFAR6 3           
   CWPCHWNT6 3              CWPCHSON6 3              CWPSOONN6 3           
   CWPSOONMY6 3             CWPCHHPY6 3              CWPCHSEX7 3           
   CWPCHDOB_Y7 4            MULTBIRT7 3              CWPCHMAR7 3           
   CWPCHRES7 3              CWPCHLRN7 3              CWPCHLIV55 3          
   CWPCHLIV56 3             CWPCHAGE7 3              CWPCHSIG7 3           
   CWPCHCRT7 3              CWPCHGEN7 3              CWPCHEVR7 3           
   CWPCHFAR7 3              CWPCHWNT7 3              CWPCHSON7 3           
   CWPSOONN7 3              CWPSOONMY7 3             CWPCHHPY7 3           
   CWPPRGNW 3               CWPTRYPG 3               CWPTRYLG 4            
   CWPCPWNT 3               CWPCPSON 3               CWPCPSNN 4            
   CWPCPSNMY 3              CWPCPHPY 3               C_OKAKIDS 3           
   CWPOTKID 3               CWPOKNUM 3               CWPOKWTH 3            
   CWPOKWTHN 3              CWPOKSEX 3               CWPOKAD 3             
   CWPOKTRY 3               CWPOKTHR 3               CWPOKLIV1 3           
   CWPOKLIV2 3              CWPOKFAR 6               CWPOKAGE 3            
   CWPOKSEX2 3              CWPOKAD2 3               CWPOKTRY2 3           
   CWPOKTHR2 3              CWPOKLIV8 3              CWPOKLIV9 3           
   CWPOKFAR2 4              CWPOKAGE2 3              CWPOKSEX3 3           
   CWPOKAD3 3               CWPOKTRY3 3              CWPOKTHR3 3           
   CWPOKLIV15 3             CWPOKLIV16 3             CWPOKFAR3 4           
   CWPOKAGE3 3              CWPOKSEX4 3              CWPOKAD4 3            
   CWPOKTRY4 3              CWPOKTHR4 3              CWPOKLIV22 3          
   CWPOKLIV23 3             CWPOKFAR4 4              CWPOKAGE4 3           
   CWPOKSEX5 3              CWPOKAD5 3               CWPOKTRY5 3           
   CWPOKTHR5 3              CWPOKLIV29 3             CWPOKLIV30 3          
   CWPOKFAR5 4              CWPOKAGE5 3              C_NBAKIDS 3           
   CWPNBEVR 3               CWPNBNUM 3               CWPNBREL 3            
   CWPNBFOS 3               CWPNBSEX 3               CWPNBAD 3             
   CWPNBTRY 3               CWPNBTHR 3               CWPNBLIV1 3           
   CWPNBLIV2 3              CWPNBLIV3 3              CWPNBFAR 6            
   CWPNBAGE 3               CWPNBREL2 3              CWPNBFOS2 3           
   CWPNBSEX2 3              CWPNBAD2 3               CWPNBTRY2 3           
   CWPNBTHR2 3              CWPNBLIV8 3              CWPNBLIV9 3           
   CWPNBLIV10 3             CWPNBFAR2 6              CWPNBAGE2 3           
   CWPNBREL3 3              CWPNBFOS3 3              CWPNBSEX3 3           
   CWPNBAD3 3               CWPNBTRY3 3              CWPNBTHR3 3           
   CWPNBLIV15 3             CWPNBLIV16 3             CWPNBLIV17 3          
   CWPNBFAR3 4              CWPNBAGE3 3              CWPNBREL4 3           
   CWPNBFOS4 3              CWPNBSEX4 3              CWPNBAD4 3            
   CWPNBTRY4 3              CWPNBTHR4 3              CWPNBLIV22 3          
   CWPNBLIV23 3             CWPNBLIV24 3             CWPNBFAR4 4           
   CWPNBAGE4 3              CWPNBREL5 3              CWPNBFOS5 3           
   CWPNBSEX5 3              CWPNBAD5 3               CWPNBTRY5 3           
   CWPNBTHR5 3              CWPNBLIV29 3             CWPNBLIV30 3          
   CWPNBLIV31 3             CWPNBFAR5 3              CWPNBAGE5 3           
   CWPNBREL6 3              CWPNBFOS6 3              CWPNBSEX6 3           
   CWPNBAD6 3               CWPNBTRY6 3              CWPNBTHR6 3           
   CWPNBLIV36 3             CWPNBLIV37 3             CWPNBLIV38 3          
   CWPNBFAR6 3              CWPNBAGE6 3              CWPNBREL7 3           
   CWPNBFOS7 3              CWPNBSEX7 3              CWPNBAD7 3            
   CWPNBTRY7 3              CWPNBTHR7 3              CWPNBLIV43 3          
   CWPNBLIV44 3             CWPNBLIV45 3             CWPNBFAR7 3           
   CWPNBAGE7 3              CWPNBREL8 3              CWPNBFOS8 3           
   CWPNBSEX8 3              CWPNBAD8 3               CWPNBTRY8 3           
   CWPNBTHR8 3              CWPNBLIV50 3             CWPNBLIV51 3          
   CWPNBLIV52 3             CWPNBFAR8 6              CWPNBAGE8 3           
   CWPNBREL9 3              CWPNBFOS9 3              CWPNBSEX9 3           
   CWPNBAD9 3               CWPNBTRY9 3              CWPNBTHR9 3           
   CWPNBLIV57 3             CWPNBLIV58 3             CWPNBLIV59 3          
   CWPNBFAR9 6              CWPNBAGE9 3              CWPNBREL10 3          
   CWPNBFOS10 3             CWPNBSEX10 3             CWPNBAD10 3           
   CWPNBTRY10 3             CWPNBTHR10 3             CWPNBLIV64 3          
   CWPNBLIV65 3             CWPNBLIV66 3             CWPNBFAR10 6          
   CWPNBAGE10 3             MARDATEN_Y 4             AGEMARR 3             
   LIVTOGN 3                STRTLIVE_Y 4             AGELIV 3              
   CMUNIONP 4               ENGAGTHN 3               MARREND 3             
   WIFEDIED_Y 4             DIVORFIN_Y 4             ANNULLED_Y 3          
   STOPLIVE_Y 4             MARDATEN_Y2 4            AGEMARR2 3            
   LIVTOGN2 3               STRTLIVE_Y2 4            AGELIV2 3             
   ENGAGTHN2 3              MARREND2 3               WIFEDIED_Y2 4         
   DIVORFIN_Y2 4            ANNULLED_Y2 3            STOPLIVE_Y2 4         
   MARDATEN_Y3 4            AGEMARR3 3               LIVTOGN3 3            
   STRTLIVE_Y3 4            AGELIV3 3                ENGAGTHN3 3           
   MARREND3 3               WIFEDIED_Y3 3            DIVORFIN_Y3 4         
   ANNULLED_Y3 3            STOPLIVE_Y3 4            CURRPRTS 3            
   PXCURR 3                 PXCURRPRT 3              PXMARRY 3             
   PXCURR2 3                PXCURRPRT2 3             PXMARRY2 3            
   PXCURR3 3                PXCURRPRT3 3             PXMARRY3 3            
   PXLRUSE 3                PXLRMETH1 3              PXLRMETH2 3           
   PXLRMETH3 3              PXLPUSE 3                DKPXLPUSE 3           
   PXLPMETH01 3             PXLPMETH02 3             PXLPMETH03 3          
   DKPXLPMETH 3             LSXUSEP 3                MTONCEP 3             
   PXLSXPRB 3               PXMTONCE 3               PXFRLTN1 3            
   P1YRACE1 3               P1YNRACE1 3              PXLRUSE2 3            
   PXLRMETH5 3              PXLRMETH6 3              PXLRMETH7 3           
   PXLPUSE2 3               DKPXLPUSE2 3             PXLPMETH11 3          
   PXLPMETH12 3             PXLPMETH13 3             DKPXLPMETH2 3         
   LSXUSEP2 3               MTONCEP2 3               PXLSXPRB2 3           
   PXMTONCE2 3              PXFRLTN3 3               P1YRACE2 3            
   P1YNRACE2 3              PXLRUSE3 3               PXLRMETH9 3           
   PXLRMETH10 3             PXLRMETH11 3             PXLPUSE3 3            
   DKPXLPUSE3 3             PXLPMETH21 3             PXLPMETH22 3          
   PXLPMETH23 3             DKPXLPMETH3 3            LSXUSEP3 3            
   MTONCEP3 3               PXLSXPRB3 3              PXMTONCE3 3           
   PXFRLTN5 3               P1YRACE3 3               P1YNRACE3 3           
   PXDOB_Y 4                PXEDUC 3                 PXMARBF 3             
   PXANYCH 3                PXANYCHN 3               PXABLECH 3            
   PXDOB_Y2 4               PXEDUC2 3                PXMARBF2 3            
   PXANYCH2 3               PXANYCHN2 3              PXABLECH2 3           
   PXDOB_Y3 4               PXEDUC3 3                PXMARBF3 3            
   PXANYCH3 3               PXANYCHN3 3              PXABLECH3 3           
   PXSXFRST_M 3             PXSXFRST_Y 4             CMFSXP 4              
   AGEFSXP 4                PXAGFRST 3               PXFRLTN2 3            
   PXFUSE 3                 PXFMETH01 3              PXFMETH02 3           
   PXFMETH03 3              PXFMETH04 3              PXSXFRST_M2 3         
   PXSXFRST_Y2 4            CMFSXP2 4                AGEFSXP2 4            
   PXAGFRST2 3              PXFRLTN4 3               PXFUSE2 3             
   PXFMETH14 3              PXFMETH15 3              PXFMETH16 3           
   PXFMETH17 3              PXSXFRST_M3 3            PXSXFRST_Y3 4         
   CMFSXP3 4                AGEFSXP3 4               PXAGFRST3 3           
   PXFRLTN6 3               PXFUSE3 3                PXFMETH27 3           
   PXFMETH28 3              PXFMETH29 3              PXFMETH30 3           
   PXANYUSE 3               PXMETHOD01 3             PXMETHOD02 3          
   PXMETHOD03 3             PXMETHOD04 3             PXMETHOD05 3          
   PXMSTUSE 3               PXCONFRQ 3               PXNOFREQ 3            
   PXANYUSE2 3              PXMETHOD14 3             PXMETHOD15 3          
   PXMETHOD16 3             PXMETHOD17 3             PXMETHOD18 3          
   PXMSTUSE2 3              PXCONFRQ2 4              PXNOFREQ2 3           
   PXANYUSE3 3              PXMETHOD27 3             PXMETHOD28 3          
   PXMETHOD29 3             PXMETHOD30 3             PXMETHOD31 3          
   PXMSTUSE3 3              PXCONFRQ3 4              PXNOFREQ3 3           
   PXCHILD 3                PXCHILDN 3               PXCXSEX 3             
   PXCXBORN_Y 4             MULTBIRT11 3             PXCXMARB 3            
   PXCXRES 3                PXCXKNOW 3               PXCXLIV01 3           
   PXCXLIV02 3              PXCXLIV03 3              PXCXAGE 3             
   PXCXSIG 3                PXCXCRT 3                PXCXGEN 3             
   PXCXEVER 3               PXCXFAR 6                PXWANT 3              
   PXSOON 3                 PXSOONN 3                PXSOONMY 3            
   PXHPYPG 3                PXCXSEX2 3               PXCXBORN_Y2 4         
   MULTBIRT12 3             PXCXMARB2 3              PXCXRES2 3            
   PXCXKNOW2 3              PXCXLIV11 3              PXCXLIV12 3           
   PXCXLIV13 3              PXCXAGE2 3               PXCXSIG2 3            
   PXCXCRT2 3               PXCXGEN2 3               PXCXEVER2 3           
   PXCXFAR2 6               PXWANT2 3                PXSOON2 3             
   PXSOONN2 3               PXSOONMY2 3              PXHPYPG2 3            
   PXCXSEX3 3               PXCXBORN_Y3 4            MULTBIRT13 3          
   PXCXMARB3 3              PXCXRES3 3               PXCXKNOW3 3           
   PXCXLIV21 3              PXCXLIV22 3              PXCXLIV23 3           
   PXCXAGE3 3               PXCXSIG3 3               PXCXCRT3 3            
   PXCXGEN3 3               PXCXEVER3 3              PXCXFAR3 4            
   PXWANT3 3                PXSOON3 3                PXSOONN3 3            
   PXSOONMY3 3              PXHPYPG3 3               PXCXSEX4 3            
   PXCXBORN_Y4 4            MULTBIRT14 3             PXCXMARB4 3           
   PXCXRES4 3               PXCXKNOW4 3              PXCXLIV31 3           
   PXCXLIV32 3              PXCXLIV33 3              PXCXAGE4 3            
   PXCXSIG4 3               PXCXCRT4 3               PXCXGEN4 3            
   PXCXEVER4 3              PXCXFAR4 3               PXWANT4 3             
   PXSOON4 3                PXSOONN4 3               PXSOONMY4 3           
   PXHPYPG4 3               PXCHILD2 3               PXCHILDN2 3           
   PXCXSEX11 3              PXCXBORN_Y11 4           MULTBIRT21 3          
   PXCXMARB11 3             PXCXRES11 3              PXCXKNOW11 3          
   PXCXLIV101 3             PXCXLIV102 3             PXCXAGE11 3           
   PXCXSIG11 3              PXCXCRT11 3              PXCXGEN11 3           
   PXCXEVER11 3             PXCXFAR11 3              PXWANT11 3            
   PXSOON11 3               PXSOONN11 3              PXSOONMY11 3          
   PXHPYPG11 3              PXCXSEX12 3              PXCXBORN_Y12 4        
   MULTBIRT22 3             PXCXMARB12 3             PXCXRES12 3           
   PXCXKNOW12 3             PXCXLIV111 3             PXCXLIV112 3          
   PXCXAGE12 3              PXCXSIG12 3              PXCXCRT12 3           
   PXCXGEN12 3              PXCXEVER12 3             PXCXFAR12 4           
   PXWANT12 3               PXSOON12 3               PXSOONN12 3           
   PXSOONMY12 3             PXHPYPG12 3              PXCXSEX13 3           
   PXCXBORN_Y13 4           MULTBIRT23 3             PXCXMARB13 3          
   PXCXRES13 3              PXCXKNOW13 3             PXCXLIV121 3          
   PXCXLIV122 3             PXCXAGE13 3              PXCXSIG13 3           
   PXCXCRT13 3              PXCXGEN13 3              PXCXEVER13 3          
   PXCXFAR13 3              PXWANT13 3               PXSOON13 3            
   PXSOONN13 3              PXSOONMY13 3             PXHPYPG13 3           
   PXCXSEX14 3              PXCXBORN_Y14 4           MULTBIRT24 3          
   PXCXMARB14 3             PXCXRES14 3              PXCXKNOW14 3          
   PXCXLIV131 3             PXCXLIV132 3             PXCXAGE14 3           
   PXCXSIG14 3              PXCXCRT14 3              PXCXGEN14 3           
   PXCXEVER14 3             PXCXFAR14 3              PXWANT14 3            
   PXSOON14 3               PXSOONN14 3              PXSOONMY14 3          
   PXHPYPG14 3              PXCHILD3 3               PXCHILDN3 3           
   PXCXSEX21 3              PXCXBORN_Y21 4           MULTBIRT31 3          
   PXCXMARB21 3             PXCXRES21 3              PXCXKNOW21 3          
   PXCXLIV201 3             PXCXLIV202 3             PXCXAGE21 3           
   PXCXSIG21 3              PXCXCRT21 3              PXCXGEN21 3           
   PXCXEVER21 3             PXCXFAR21 3              PXWANT21 3            
   PXSOON21 3               PXSOONN21 3              PXSOONMY21 3          
   PXHPYPG21 3              PXCXSEX22 3              PXCXBORN_Y22 4        
   MULTBIRT32 3             PXCXMARB22 3             PXCXRES22 3           
   PXCXKNOW22 3             PXCXLIV211 3             PXCXLIV212 3          
   PXCXAGE22 3              PXCXSIG22 3              PXCXCRT22 3           
   PXCXGEN22 3              PXCXEVER22 3             PXCXFAR22 3           
   PXWANT22 3               PXSOON22 3               PXSOONN22 3           
   PXSOONMY22 3             PXHPYPG22 3              PXCXSEX23 3           
   PXCXBORN_Y23 4           MULTBIRT33 3             PXCXMARB23 3          
   PXCXRES23 3              PXCXKNOW23 3             PXCXLIV221 3          
   PXCXLIV222 3             PXCXAGE23 3              PXCXSIG23 3           
   PXCXCRT23 3              PXCXGEN23 3              PXCXEVER23 3          
   PXCXFAR23 3              PXWANT23 3               PXSOON23 3            
   PXSOONN23 3              PXSOONMY23 3             PXHPYPG23 3           
   PXCXSEX24 3              PXCXBORN_Y24 4           MULTBIRT34 3          
   PXCXMARB24 3             PXCXRES24 3              PXCXKNOW24 3          
   PXCXLIV231 3             PXCXLIV232 3             PXCXAGE24 3           
   PXCXSIG24 3              PXCXCRT24 3              PXCXGEN24 3           
   PXCXEVER24 3             PXCXFAR24 3              PXWANT24 3            
   PXSOON24 3               PXSOONN24 3              PXSOONMY24 3          
   PXHPYPG24 3              PXCXSEX25 3              PXCXBORN_Y25 4        
   MULTBIRT35 3             PXCXMARB25 3             PXCXRES25 3           
   PXCXKNOW25 3             PXCXLIV241 3             PXCXLIV242 3          
   PXCXAGE25 3              PXCXSIG25 3              PXCXCRT25 3           
   PXCXGEN25 3              PXCXEVER25 3             PXCXFAR25 3           
   PXWANT25 3               PXSOON25 3               PXSOONN25 3           
   PXSOONMY25 3             PXHPYPG25 3              PXCPREG 3             
   PXTRYING 3               PXTRYLONG 3              PXRWANT 3             
   PXRSOON 3                PXRSOONN 3               PXRSOONMY 3           
   PXCPFEEL 3               PXCPREG2 3               PXTRYING2 3           
   PXTRYLONG2 3             PXRWANT2 3               PXRSOON2 3            
   PXRSOONN2 3              PXRSOONMY2 3             PXCPFEEL2 3           
   PXCPREG3 3               PXTRYING3 3              PXTRYLONG3 3          
   PXRWANT3 3               PXRSOON3 3               PXRSOONN3 3           
   PXRSOONMY3 3             PXCPFEEL3 3              CURRPREG 3            
   D_OKAKIDS 3              PXOTKID 3                PXOKNUM 3             
   PXOKWTH 3                PXOKWTHN 3               PXOKSEX 3             
   PXOKAD 3                 PXOKLIV1 3               PXOKFAR 3             
   PXOKAGE 3                PXOKSEX2 3               PXOKAD2 3             
   PXOKLIV9 3               PXOKFAR2 3               PXOKAGE2 3            
   PXOKSEX3 3               PXOKAD3 3                PXOKLIV17 3           
   PXOKFAR3 3               PXOKAGE3 3               PXOKSEX4 3            
   PXOKAD4 3                PXOKLIV25 3              PXOKFAR4 3            
   PXOKAGE4 3               D_OKAKIDS2 3             PXOTKID2 3            
   PXOKNUM2 3               PXOKWTH2 3               PXOKWTHN2 3           
   PXOKSEX11 3              PXOKAD11 3               PXOKLIV81 3           
   PXOKFAR11 4              PXOKAGE11 3              PXOKSEX12 3           
   PXOKAD12 3               PXOKLIV89 3              PXOKFAR12 3           
   PXOKAGE12 3              PXOKSEX13 3              PXOKAD13 3            
   PXOKLIV97 3              PXOKFAR13 3              PXOKAGE13 3           
   PXOKSEX14 3              PXOKAD14 3               PXOKLIV105 3          
   PXOKFAR14 3              PXOKAGE14 3              PXOKSEX15 3           
   PXOKAD15 3               PXOKLIV113 3             PXOKFAR15 3           
   PXOKAGE15 3              D_OKAKIDS3 3             PXOTKID3 3            
   PXOKNUM3 3               PXOKWTH3 3               PXOKWTHN3 3           
   PXOKSEX21 3              PXOKAD21 3               PXOKAGE21 3           
   PXOKSEX22 3              PXOKAD22 3               PXOKAGE22 3           
   PXOKSEX23 3              PXOKAD23 3               PXOKAGE23 3           
   PXOKSEX24 3              PXOKAD24 3               PXOKAGE24 3           
   PXOKSEX25 3              PXOKAD25 3               PXOKAGE25 3           
   D_NBAKIDS 3              PXNBEVR 3                PXNBNUM 3             
   PXNBREL 3                PXNBFOS 3                PXNBSEX 3             
   PXNBAD 3                 PXNBLIV1 3               PXNBLIV2 3            
   PXNBFAR 4                PXNBAGE 3                PXNBREL2 3            
   PXNBFOS2 3               PXNBSEX2 3               PXNBAD2 3             
   PXNBLIV9 3               PXNBLIV10 3              PXNBFAR2 3            
   PXNBAGE2 3               PXNBREL3 3               PXNBFOS3 3            
   PXNBSEX3 3               PXNBAD3 3                PXNBLIV17 3           
   PXNBLIV18 3              PXNBFAR3 3               PXNBAGE3 3            
   D_NBAKIDS2 3             PXNBEVR2 3               PXNBNUM2 3            
   PXNBREL11 3              PXNBFOS11 3              PXNBSEX11 3           
   PXNBAD11 3               PXNBAGE11 3              PXNBREL12 3           
   PXNBFOS12 3              PXNBSEX12 3              PXNBAD12 3            
   PXNBAGE12 3              PXNBREL13 3              PXNBFOS13 3           
   PXNBSEX13 3              PXNBAD13 3               PXNBAGE13 3           
   PXNBREL14 3              PXNBFOS14 3              PXNBSEX14 3           
   PXNBAD14 3               PXNBAGE14 3              PXNBREL15 3           
   PXNBFOS15 3              PXNBSEX15 3              PXNBAD15 3            
   PXNBAGE15 3              PXNBREL16 3              PXNBFOS16 3           
   PXNBSEX16 3              PXNBAD16 3               PXNBAGE16 3           
   PXNBREL17 3              PXNBFOS17 3              PXNBSEX17 3           
   PXNBAD17 3               PXNBAGE17 3              PXNBREL18 3           
   PXNBFOS18 3              PXNBSEX18 3              PXNBAD18 3            
   PXNBAGE18 3              D_NBAKIDS3 3             PXNBEVR3 3            
   FPFIRST_M 3              FPFIRST_Y 4              CMFSTSEX 4            
   FSTSEXAGE 3              FPAGE 3                  FPAGE18 3             
   FPAGE15 3                FPAGE20 3                RFSXAGEGP 3           
   FPRLTN 3                 FPUSE 3                  FPMETH01 3            
   FPMETH02 3               FPMETH03 3               FPMETH04 3            
   FPPROBE 3                NFORMWIFE 4              NFORMCOHAB 4          
   FWVERIFY 3               FWVER 3                  FWVERIFY2 3           
   FWVER2 3                 FWVERIFY3 3              FWVER3 3              
   FWVERIFY4 3              FWVER4 3                 FCVER 3               
   FCVERIFY 3               EXRELATION 3             FWMAREND_Y 4          
   AGEMARRN 3               LIVTOGN4 3               STRTLIVE_Y4 4         
   AGELIV4 3                CMUNIONW 4               ENGAGTHN4 3           
   MARREND4 3               WIFEDIED_Y4 4            DIVORFIN_Y4 4         
   ANNULLED_Y4 4            STOPLIVE_Y4 4            EXRELATION2 3         
   FWMAREND_Y2 4            AGEMARRN2 3              LIVTOGN5 3            
   STRTLIVE_Y5 4            AGELIV5 3                ENGAGTHN5 3           
   MARREND5 3               WIFEDIED_Y5 4            DIVORFIN_Y5 4         
   ANNULLED_Y5 3            STOPLIVE_Y5 4            EXRELATION3 3         
   FWMAREND_Y3 4            AGEMARRN3 3              LIVTOGN6 3            
   STRTLIVE_Y6 4            AGELIV6 3                ENGAGTHN6 3           
   MARREND6 3               WIFEDIED_Y6 4            DIVORFIN_Y6 4         
   ANNULLED_Y6 3            STOPLIVE_Y6 4            EXRELATION4 3         
   FWMAREND_Y4 4            AGEMARRN4 3              LIVTOGN7 3            
   STRTLIVE_Y7 4            AGELIV7 3                ENGAGTHN7 3           
   MARREND7 3               WIFEDIED_Y7 3            DIVORFIN_Y7 4         
   ANNULLED_Y7 3            STOPLIVE_Y7 4            EXRELATION11 3        
   STRTLIVE_Y14 4           CMCOHFC11 4              AGELIV14 3            
   ENGAGTHN14 3             STOPLIVE_Y14 4           FWPDOB_Y 4            
   FWPAGE 3                 WIF1RACE 3               WIF1NRACE 3           
   FWPMARBF 3               FWPDOB_Y2 4              FWPAGE2 3             
   FWPMARBF2 3              FWPDOB_Y3 4              FWPAGE3 3             
   FWPMARBF3 3              FWPDOB_Y4 4              FWPAGE4 3             
   FWPMARBF4 3              FWPDOB_Y11 4             FWPAGE11 3            
   COH1RACE 3               COH1NRACE 3              FWPMARBF11 3          
   FWPBIOKD 3               FWPNUMKD 3               FWPCHSEX 3            
   FWPCHDOB_Y 4             FWCHMARB 3               FWPCHRES 3            
   FWPCHLRN 3               FWPCHLIV01 3             FWPCHLIV02 3          
   FWPCHLIV03 3             FWPCHAGE 3               FWPCHSIG 3            
   FWPCHCRT 3               FWPCHGEN 3               FWPCHEVR 3            
   FWPCHFAR 6               FWPRWANT 3               FWPSOON 3             
   FWPSOONN 4               FWPSOONMY 3              FWPHPYPG 3            
   FWPCHSEX2 3              FWPCHDOB_Y2 4            MULTBIRT42 3          
   FWCHMARB2 3              FWPCHRES2 3              FWPCHLRN2 3           
   FWPCHLIV11 3             FWPCHLIV12 3             FWPCHLIV13 3          
   FWPCHAGE2 3              FWPCHSIG2 3              FWPCHCRT2 3           
   FWPCHGEN2 3              FWPCHEVR2 3              FWPCHFAR2 6           
   FWPRWANT2 3              FWPSOON2 3               FWPSOONN2 4           
   FWPSOONMY2 3             FWPHPYPG2 3              FWPCHSEX3 3           
   FWPCHDOB_Y3 4            MULTBIRT43 3             FWCHMARB3 3           
   FWPCHRES3 3              FWPCHLRN3 3              FWPCHLIV21 3          
   FWPCHLIV22 3             FWPCHLIV23 3             FWPCHAGE3 3           
   FWPCHSIG3 3              FWPCHCRT3 3              FWPCHGEN3 3           
   FWPCHEVR3 3              FWPCHFAR3 4              FWPRWANT3 3           
   FWPSOON3 3               FWPSOONN3 3              FWPSOONMY3 3          
   FWPHPYPG3 3              FWPCHSEX4 3              FWPCHDOB_Y4 4         
   MULTBIRT44 3             FWCHMARB4 3              FWPCHRES4 3           
   FWPCHLRN4 3              FWPCHLIV31 3             FWPCHLIV32 3          
   FWPCHLIV33 3             FWPCHAGE4 3              FWPCHSIG4 3           
   FWPCHCRT4 3              FWPCHGEN4 3              FWPCHEVR4 3           
   FWPCHFAR4 4              FWPRWANT4 3              FWPSOON4 3            
   FWPSOONN4 3              FWPSOONMY4 3             FWPHPYPG4 3           
   FWPCHSEX5 3              FWPCHDOB_Y5 4            MULTBIRT45 3          
   FWCHMARB5 3              FWPCHRES5 3              FWPCHLRN5 3           
   FWPCHLIV41 3             FWPCHLIV42 3             FWPCHLIV43 3          
   FWPCHAGE5 3              FWPCHSIG5 3              FWPCHCRT5 3           
   FWPCHGEN5 3              FWPCHEVR5 3              FWPCHFAR5 4           
   FWPRWANT5 3              FWPSOON5 3               FWPSOONN5 3           
   FWPSOONMY5 3             FWPHPYPG5 3              FWPBIOKD2 3           
   FWPNUMKD2 3              FWPCHSEX11 3             FWPCHDOB_Y11 4        
   FWCHMARB11 3             FWPCHRES11 3             FWPCHLRN11 3          
   FWPCHLIV101 3            FWPCHLIV102 3            FWPCHAGE11 3          
   FWPCHSIG11 3             FWPCHCRT11 3             FWPCHGEN11 3          
   FWPCHEVR11 3             FWPCHFAR11 6             FWPRWANT11 3          
   FWPSOON11 3              FWPSOONN11 3             FWPSOONMY11 3         
   FWPHPYPG11 3             FWPCHSEX12 3             FWPCHDOB_Y12 4        
   MULTBIRT52 3             FWCHMARB12 3             FWPCHRES12 3          
   FWPCHLRN12 3             FWPCHLIV111 3            FWPCHLIV112 3         
   FWPCHAGE12 3             FWPCHSIG12 3             FWPCHCRT12 3          
   FWPCHGEN12 3             FWPCHEVR12 3             FWPCHFAR12 3          
   FWPRWANT12 3             FWPSOON12 3              FWPSOONN12 4          
   FWPSOONMY12 3            FWPHPYPG12 3             FWPCHSEX13 3          
   FWPCHDOB_Y13 4           MULTBIRT53 3             FWCHMARB13 3          
   FWPCHRES13 3             FWPCHLRN13 3             FWPCHLIV121 3         
   FWPCHLIV122 3            FWPCHAGE13 3             FWPCHSIG13 3          
   FWPCHCRT13 3             FWPCHGEN13 3             FWPCHEVR13 3          
   FWPCHFAR13 3             FWPRWANT13 3             FWPSOON13 3           
   FWPSOONN13 3             FWPSOONMY13 3            FWPHPYPG13 3          
   FWPCHSEX14 3             FWPCHDOB_Y14 4           MULTBIRT54 3          
   FWCHMARB14 3             FWPCHRES14 3             FWPCHLRN14 3          
   FWPCHLIV131 3            FWPCHLIV132 3            FWPCHAGE14 3          
   FWPCHSIG14 3             FWPCHCRT14 3             FWPCHGEN14 3          
   FWPCHEVR14 3             FWPCHFAR14 3             FWPRWANT14 3          
   FWPSOON14 3              FWPSOONN14 3             FWPSOONMY14 3         
   FWPHPYPG14 3             FWPBIOKD3 3              FWPNUMKD3 3           
   FWPCHSEX21 3             FWPCHDOB_Y21 4           FWCHMARB21 3          
   FWPCHRES21 3             FWPCHLRN21 3             FWPCHLIV201 3         
   FWPCHLIV202 3            FWPCHAGE21 3             FWPCHSIG21 3          
   FWPCHCRT21 3             FWPCHGEN21 3             FWPCHEVR21 3          
   FWPCHFAR21 4             FWPRWANT21 3             FWPSOON21 3           
   FWPSOONN21 3             FWPSOONMY21 3            FWPHPYPG21 3          
   FWPCHSEX22 3             FWPCHDOB_Y22 4           MULTBIRT62 3          
   FWCHMARB22 3             FWPCHRES22 3             FWPCHLRN22 3          
   FWPCHLIV211 3            FWPCHLIV212 3            FWPCHAGE22 3          
   FWPCHSIG22 3             FWPCHCRT22 3             FWPCHGEN22 3          
   FWPCHEVR22 3             FWPCHFAR22 4             FWPRWANT22 3          
   FWPSOON22 3              FWPSOONN22 3             FWPSOONMY22 3         
   FWPHPYPG22 3             FWPCHSEX23 3             FWPCHDOB_Y23 4        
   MULTBIRT63 3             FWCHMARB23 3             FWPCHRES23 3          
   FWPCHLRN23 3             FWPCHLIV221 3            FWPCHLIV222 3         
   FWPCHAGE23 3             FWPCHSIG23 3             FWPCHCRT23 3          
   FWPCHGEN23 3             FWPCHEVR23 3             FWPCHFAR23 3          
   FWPRWANT23 3             FWPSOON23 3              FWPSOONN23 3          
   FWPSOONMY23 3            FWPHPYPG23 3             FWPBIOKD4 3           
   FWPNUMKD4 3              FWPBIOKD11 3             FWPNUMKD11 3          
   FWPCHSEX101 3            FWPCHDOB_Y101 4          FWPCHRES101 3         
   FWPCHLRN101 3            FWPCHLIV1001 3           FWPCHLIV1002 3        
   FWPCHLIV1003 3           FWPCHAGE101 3            FWPCHSIG101 3         
   FWPCHCRT101 3            FWPCHGEN101 3            FWPCHEVR101 3         
   FWPCHFAR101 6            FWPRWANT101 3            FWPSOON101 3          
   FWPSOONN101 4            FWPSOONMY101 3           FWPHPYPG101 3         
   FWPCHSEX102 3            FWPCHDOB_Y102 4          MULTBIRT142 3         
   FWPCHRES102 3            FWPCHLRN102 3            FWPCHLIV1011 3        
   FWPCHLIV1012 3           FWPCHLIV1013 3           FWPCHAGE102 3         
   FWPCHSIG102 3            FWPCHCRT102 3            FWPCHGEN102 3         
   FWPCHEVR102 3            FWPCHFAR102 6            FWPRWANT102 3         
   FWPSOON102 3             FWPSOONN102 3            FWPSOONMY102 3        
   FWPHPYPG102 3            FWPCHSEX103 3            FWPCHDOB_Y103 4       
   MULTBIRT143 3            FWPCHRES103 3            FWPCHLRN103 3         
   FWPCHLIV1021 3           FWPCHLIV1022 3           FWPCHLIV1023 3        
   FWPCHAGE103 3            FWPCHSIG103 3            FWPCHCRT103 3         
   FWPCHGEN103 3            FWPCHEVR103 3            FWPCHFAR103 4         
   FWPRWANT103 3            FWPSOON103 3             FWPSOONN103 3         
   FWPSOONMY103 3           FWPHPYPG103 3            FWPCHSEX104 3         
   FWPCHDOB_Y104 4          MULTBIRT144 3            FWPCHRES104 3         
   FWPCHLRN104 3            FWPCHLIV1031 3           FWPCHLIV1032 3        
   FWPCHLIV1033 3           FWPCHAGE104 3            FWPCHSIG104 3         
   FWPCHCRT104 3            FWPCHGEN104 3            FWPCHEVR104 3         
   FWPCHFAR104 4            FWPRWANT104 3            FWPSOON104 3          
   FWPSOONN104 3            FWPSOONMY104 3           FWPHPYPG104 3         
   E_OKAKIDS 3              FWPOTKID 3               FWPOKNUM 3            
   FWPOKWTH 3               FWPOKWTHN 3              FWPOKSEX 3            
   FWPOKAD 3                FWPOKLIV1 3              FWPOKLIV2 3           
   FWPOKFAR 4               FWPOKAGE 3               FWPOKSEX2 3           
   FWPOKAD2 3               FWPOKLIV9 3              FWPOKLIV10 3          
   FWPOKFAR2 3              FWPOKAGE2 3              FWPOKSEX3 3           
   FWPOKAD3 3               FWPOKLIV17 3             FWPOKLIV18 3          
   FWPOKFAR3 3              FWPOKAGE3 3              FWPOKSEX4 3           
   FWPOKAD4 3               FWPOKLIV25 3             FWPOKLIV26 3          
   FWPOKFAR4 3              FWPOKAGE4 3              FWPOKSEX5 3           
   FWPOKAD5 3               FWPOKLIV33 3             FWPOKLIV34 3          
   FWPOKFAR5 3              FWPOKAGE5 3              E_OKAKIDS2 3          
   FWPOTKID2 3              FWPOKNUM2 3              FWPOKWTH2 3           
   FWPOKWTHN2 3             FWPOKSEX11 3             FWPOKAD11 3           
   FWPOKLIV81 3             FWPOKFAR11 4             FWPOKAGE11 3          
   FWPOKSEX12 3             FWPOKAD12 3              FWPOKLIV89 3          
   FWPOKFAR12 3             FWPOKAGE12 3             FWPOKSEX13 3          
   FWPOKAD13 3              FWPOKLIV97 3             FWPOKFAR13 3          
   FWPOKAGE13 3             FWPOKSEX14 3             FWPOKAD14 3           
   FWPOKLIV105 3            FWPOKFAR14 3             FWPOKAGE14 3          
   FWPOKSEX15 3             FWPOKAD15 3              FWPOKLIV113 3         
   FWPOKFAR15 3             FWPOKAGE15 3             E_OKAKIDS3 3          
   FWPOTKID3 3              FWPOKNUM3 3              FWPOKWTH3 3           
   FWPOKWTHN3 3             FWPOKSEX21 3             FWPOKAD21 3           
   FWPOKLIV161 3            FWPOKFAR21 3             FWPOKAGE21 3          
   FWPOKSEX22 3             FWPOKAD22 3              FWPOKLIV169 3         
   FWPOKFAR22 3             FWPOKAGE22 3             E_OKAKIDS4 3          
   FWPOTKID4 3              FWPOKNUM4 3              FWPOKWTH4 3           
   FWPOKWTHN4 3             FWPOKSEX31 3             FWPOKAD31 3           
   FWPOKAGE31 3             E_OKAKIDS11 3            FWPOTKID11 3          
   FWPOKNUM11 3             FWPOKWTH11 3             FWPOKWTHN11 3         
   FWPOKSEX101 3            FWPOKAD101 3             FWPOKLIV801 3         
   FWPOKFAR101 6            FWPOKAGE101 3            FWPOKSEX102 3         
   FWPOKAD102 3             FWPOKLIV809 3            FWPOKFAR102 4         
   FWPOKAGE102 3            FWPOKSEX103 3            FWPOKAD103 3          
   FWPOKLIV817 3            FWPOKFAR103 4            FWPOKAGE103 3         
   FWPOKSEX104 3            FWPOKAD104 3             FWPOKLIV825 3         
   FWPOKFAR104 3            FWPOKAGE104 3            FWPOKSEX105 3         
   FWPOKAD105 3             FWPOKLIV833 3            FWPOKFAR105 3         
   FWPOKAGE105 3            FWPOKSEX106 3            FWPOKAD106 3          
   FWPOKLIV841 3            FWPOKFAR106 3            FWPOKAGE106 3         
   E_NBAKIDS 3              FWPNBEVR 3               FWPNBNUM 3            
   FWPNBREL 3               FWPNBFOS 3               FWPNBSEX 3            
   FWPNBAD 3                FWPNBLIV1 3              FWPNBLIV2 3           
   FWPNBFAR 4               FWPNBAGE 3               FWPNBREL2 3           
   FWPNBFOS2 3              FWPNBSEX2 3              FWPNBAD2 3            
   FWPNBLIV9 3              FWPNBLIV10 3             FWPNBFAR2 4           
   FWPNBAGE2 3              FWPNBREL3 3              FWPNBFOS3 3           
   FWPNBSEX3 3              FWPNBAD3 3               FWPNBLIV17 3          
   FWPNBLIV18 3             FWPNBFAR3 3              FWPNBAGE3 3           
   FWPNBREL4 3              FWPNBFOS4 3              FWPNBSEX4 3           
   FWPNBAD4 3               FWPNBLIV25 3             FWPNBLIV26 3          
   FWPNBFAR4 3              FWPNBAGE4 3              E_NBAKIDS2 3          
   FWPNBEVR2 3              FWPNBNUM2 3              FWPNBREL11 3          
   FWPNBFOS11 3             FWPNBSEX11 3             FWPNBAD11 3           
   FWPNBLIV81 3             FWPNBFAR11 3             FWPNBAGE11 3          
   FWPNBREL12 3             FWPNBFOS12 3             FWPNBSEX12 3          
   FWPNBAD12 3              FWPNBLIV89 3             FWPNBFAR12 3          
   FWPNBAGE12 3             E_NBAKIDS3 3             FWPNBEVR3 3           
   FWPNBNUM3 3              FWPNBREL21 3             FWPNBFOS21 3          
   FWPNBSEX21 3             FWPNBAD21 3              FWPNBLIV161 3         
   FWPNBFAR21 3             FWPNBAGE21 3             FWPNBREL22 3          
   FWPNBFOS22 3             FWPNBSEX22 3             FWPNBAD22 3           
   FWPNBLIV169 3            FWPNBFAR22 3             FWPNBAGE22 3          
   E_NBAKIDS4 3             FWPNBEVR4 3              E_NBAKIDS11 3         
   FWPNBEVR11 3             FWPNBNUM11 3             FWPNBREL101 3         
   FWPNBFOS101 3            FWPNBSEX101 3            FWPNBAD101 3          
   FWPNBLIV801 3            FWPNBFAR101 4            FWPNBAGE101 3         
   FWPNBREL102 3            FWPNBFOS102 3            FWPNBSEX102 3         
   FWPNBAD102 3             FWPNBLIV809 3            FWPNBFAR102 4         
   FWPNBAGE102 3            FWPNBREL103 3            FWPNBFOS103 3         
   FWPNBSEX103 3            FWPNBAD103 3             FWPNBLIV817 3         
   FWPNBFAR103 3            FWPNBAGE103 3            FWPNBREL104 3         
   FWPNBFOS104 3            FWPNBSEX104 3            FWPNBAD104 3          
   FWPNBLIV825 3            FWPNBFAR104 3            FWPNBAGE104 3         
   OTBCHIL 3                OTBPROBE 3               OTBCHILN 3            
   OTBSAME 3                OBCSEXX 3                OBCDOB_Y 4            
   OBCMAGEX 3               OBCMLIV 3                OBCKNOWX 3            
   OBCLIVEX01 3             OBCLIVEX02 3             OBCLIVEX03 3          
   OBCAGE 3                 OBCCHSIG 3               OBCCHCRT 3            
   OBCCHGEN 3               OBCEVER 3                OBCFAR 6              
   OBCRWANX 3               OBCSOONX 3               OBCSOONN 4            
   OBCSOONMY 3              OBCHPYX 3                OBCSEXX2 3            
   OBCDOB_Y2 4              MULTBIRT152 3            OBCMAGEX2 3           
   OBCMLIV2 3               OBCKNOWX2 3              OBCLIVEX11 3          
   OBCLIVEX12 3             OBCLIVEX13 3             OBCAGE2 3             
   OBCCHSIG2 3              OBCCHCRT2 3              OBCCHGEN2 3           
   OBCEVER2 3               OBCFAR2 6                OBCRWANX2 3           
   OBCSOONX2 3              OBCSOONN2 4              OBCSOONMY2 3          
   OBCHPYX2 3               OBCSEXX3 3               OBCDOB_Y3 4           
   MULTBIRT153 3            OBCMAGEX3 3              OBCMLIV3 3            
   OBCKNOWX3 3              OBCLIVEX21 3             OBCLIVEX22 3          
   OBCLIVEX23 3             OBCAGE3 3                OBCCHSIG3 3           
   OBCCHCRT3 3              OBCCHGEN3 3              OBCEVER3 3            
   OBCFAR3 6                OBCRWANX3 3              OBCSOONX3 3           
   OBCSOONN3 3              OBCSOONMY3 3             OBCHPYX3 3            
   OBCSEXX4 3               OBCDOB_Y4 4              MULTBIRT154 3         
   OBCMAGEX4 3              OBCMLIV4 3               OBCKNOWX4 3           
   OBCLIVEX31 3             OBCLIVEX32 3             OBCLIVEX33 3          
   OBCAGE4 3                OBCCHSIG4 3              OBCCHCRT4 3           
   OBCCHGEN4 3              OBCEVER4 3               OBCFAR4 6             
   OBCRWANX4 3              OBCSOONX4 3              OBCSOONN4 3           
   OBCSOONMY4 3             OBCHPYX4 3               OBCSEXX5 3            
   OBCDOB_Y5 4              MULTBIRT155 3            OBCMAGEX5 3           
   OBCMLIV5 3               OBCKNOWX5 3              OBCLIVEX41 3          
   OBCLIVEX42 3             OBCLIVEX43 3             OBCAGE5 3             
   OBCCHSIG5 3              OBCCHCRT5 3              OBCCHGEN5 3           
   OBCEVER5 3               OBCFAR5 4                OBCRWANX5 3           
   OBCSOONX5 3              OBCSOONN5 3              OBCSOONMY5 3          
   OBCHPYX5 3               OBCSEXX6 3               OBCDOB_Y6 4           
   MULTBIRT156 3            OBCMAGEX6 3              OBCMLIV6 3            
   OBCKNOWX6 3              OBCLIVEX51 3             OBCLIVEX52 3          
   OBCLIVEX53 3             OBCAGE6 3                OBCCHSIG6 3           
   OBCCHCRT6 3              OBCCHGEN6 3              OBCEVER6 3            
   OBCFAR6 3                OBCRWANX6 3              OBCSOONX6 3           
   OBCSOONN6 3              OBCSOONMY6 3             OBCHPYX6 3            
   OBCSEXX7 3               OBCDOB_Y7 4              MULTBIRT157 3         
   OBCMAGEX7 3              OBCMLIV7 3               OBCKNOWX7 3           
   OBCLIVEX61 3             OBCLIVEX62 3             OBCLIVEX63 3          
   OBCAGE7 3                OBCCHSIG7 3              OBCCHCRT7 3           
   OBCCHGEN7 3              OBCEVER7 3               OBCFAR7 3             
   OBCRWANX7 3              OBCSOONX7 3              OBCSOONN7 3           
   OBCSOONMY7 3             OBCHPYX7 3               OBCSEXX8 3            
   OBCDOB_Y8 4              MULTBIRT158 3            OBCMAGEX8 3           
   OBCMLIV8 3               OBCKNOWX8 3              OBCLIVEX71 3          
   OBCLIVEX72 3             OBCLIVEX73 3             OBCAGE8 3             
   OBCCHSIG8 3              OBCCHCRT8 3              OBCCHGEN8 3           
   OBCEVER8 3               OBCFAR8 3                OBCRWANX8 3           
   OBCSOONX8 3              OBCSOONN8 3              OBCSOONMY8 3          
   OBCHPYX8 3               OBCSEXX9 3               OBCDOB_Y9 4           
   MULTBIRT159 3            OBCMAGEX9 3              OBCMLIV9 3            
   OBCKNOWX9 3              OBCLIVEX81 3             OBCLIVEX82 3          
   OBCLIVEX83 3             OBCAGE9 3                OBCCHSIG9 3           
   OBCCHCRT9 3              OBCCHGEN9 3              OBCEVER9 3            
   OBCFAR9 4                OBCRWANX9 3              OBCSOONX9 3           
   OBCSOONN9 3              OBCSOONMY9 3             OBCHPYX9 3            
   OBCSEXX10 3              OBCDOB_Y10 4             MULTBIRT160 3         
   OBCMAGEX10 3             OBCMLIV10 3              OBCKNOWX10 3          
   OBCLIVEX91 3             OBCLIVEX92 3             OBCLIVEX93 3          
   OBCAGE10 3               OBCCHSIG10 3             OBCCHCRT10 3          
   OBCCHGEN10 3             OBCEVER10 3              OBCFAR10 4            
   OBCRWANX10 3             OBCSOONX10 3             OBCSOONN10 3          
   OBCSOONMY10 3            OBCHPYX10 3              F_AKIDS 3             
   OTACHIL 3                OTACHILN 3               OTNBREL 3             
   OTNBFOS 3                OTNBSEX 3                OTNBAD 3              
   OTNBLIV1 3               OTNBLIV2 3               OTNBFAR 4             
   OTNBAGE 3                OTNBREL2 3               OTNBFOS2 3            
   OTNBSEX2 3               OTNBAD2 3                OTNBLIV9 3            
   OTNBLIV10 3              OTNBFAR2 3               OTNBAGE2 3            
   OTNBREL3 3               OTNBFOS3 3               OTNBSEX3 3            
   OTNBAD3 3                OTNBLIV17 3              OTNBLIV18 3           
   OTNBFAR3 3               OTNBAGE3 3               OTNBREL4 3            
   OTNBFOS4 3               OTNBSEX4 3               OTNBAD4 3             
   OTNBLIV25 3              OTNBLIV26 3              OTNBFAR4 3            
   OTNBAGE4 3               OTNBREL5 3               OTNBFOS5 3            
   OTNBSEX5 3               OTNBAD5 3                OTNBLIV33 3           
   OTNBLIV34 3              OTNBFAR5 3               OTNBAGE5 3            
   OTNBREL6 3               OTNBFOS6 3               OTNBSEX6 3            
   OTNBAD6 3                OTNBLIV41 3              OTNBLIV42 3           
   OTNBFAR6 3               OTNBAGE6 3               OTNBREL7 3            
   OTNBFOS7 3               OTNBSEX7 3               OTNBAD7 3             
   OTNBLIV49 3              OTNBLIV50 3              OTNBFAR7 3            
   OTNBAGE7 3               OTNBREL8 3               OTNBFOS8 3            
   OTNBSEX8 3               OTNBAD8 3                OTNBLIV57 3           
   OTNBLIV58 3              OTNBFAR8 3               OTNBAGE8 3            
   OTPREG 3                 OTPRGPRB 3               OTPRGN 3              
   OTPRGEND 3               OTMSN 3                  OTSTN 3               
   OTABN 3                  TOTPRG 3                 OTPREGS 3             
   TOTPREGS_C 3             TOTPREGS_R 4             BIOKIDS 3             
   ADOPKIDS 3               ANYKIDS 3                BIOADOPT 3            
   PREGSNOW 3               NUMLIFE 4                BIODOB1 4             
   BIODOB2 4                BIODOB3 4                BIODOB4 4             
   BIODOB5 4                BIODOB6 4                BIODOB7 4             
   BIODOB8 4                BIODOB9 4                BIODOB10 4            
   BIOSEX1 3                BIOSEX2 3                BIOSEX3 3             
   BIOSEX4 3                BIOSEX5 3                BIOSEX6 3             
   BIOSEX7 3                BIOSEX8 3                BIOSEX9 3             
   BIOSEX10 3               BIOAGE1 4                BIOAGE2 4             
   BIOAGE3 4                BIOAGE4 4                BIOAGE5 4             
   BIOAGE6 4                BIOAGE7 4                BIOAGE8 4             
   BIOAGE9 3                BIOAGE10 3               BIOAGEGP1 3           
   BIOAGEGP2 3              BIOAGEGP3 3              BIOAGEGP4 3           
   BIOAGEGP5 3              BIOAGEGP6 3              BIOAGEGP7 3           
   BIOAGEGP8 3              BIOAGEGP9 3              BIOAGEGP10 3          
   BIOHH1 3                 BIOHH2 3                 BIOHH3 3              
   BIOHH4 3                 BIOHH5 3                 BIOHH6 3              
   BIOHH7 3                 BIOHH8 3                 BIOHH9 3              
   BIOHH10 3                BIOMOM1 3                BIOMOM2 3             
   BIOMOM3 3                BIOMOM4 3                BIOMOM5 3             
   BIOMOM6 3                BIOMOM7 3                BIOMOM8 3             
   BIOMOM9 3                BIOMOM10 3               BIOMAR1 3             
   BIOMAR2 3                BIOMAR3 3                BIOMAR4 3             
   BIOMAR5 3                BIOMAR6 3                BIOMAR7 3             
   BIOMAR8 3                BIOMAR9 3                BIOMAR10 3            
   BIOCOHB1 3               BIOCOHB2 3               BIOCOHB3 3            
   BIOCOHB4 3               BIOCOHB5 3               BIOCOHB6 3            
   BIOCOHB7 3               BIOCOHB8 3               BIOCOHB9 3            
   BIOCOHB10 3              BIOLRNPG1 3              BIOLRNPG2 3           
   BIOLRNPG3 3              BIOLRNPG4 3              BIOLRNPG5 3           
   BIOLRNPG6 3              BIOLRNPG7 3              BIOLRNPG8 3           
   BIOLRNPG9 3              BIOLRNPG10 3             BIOLIVNG11 3          
   BIOLIVNG12 3             BIOLIVNG13 3             BIOLIVNG21 3          
   BIOLIVNG22 3             BIOLIVNG23 3             BIOLIVNG31 3          
   BIOLIVNG32 3             BIOLIVNG33 3             BIOLIVNG41 3          
   BIOLIVNG42 3             BIOLIVNG43 3             BIOLIVNG51 3          
   BIOLIVNG52 3             BIOLIVNG53 3             BIOLIVNG61 3          
   BIOLIVNG62 3             BIOLIVNG63 3             BIOLIVNG71 3          
   BIOLIVNG72 3             BIOLIVNG73 3             BIOLIVNG81 3          
   BIOLIVNG82 3             BIOLIVNG83 3             BIOLIVNG91 3          
   BIOLIVNG92 3             BIOLIVNG93 3             BIOLIVNG101 3         
   BIOLIVNG102 3            BIOLIVNG103 3            BIOCHSIG1 3           
   BIOCHSIG2 3              BIOCHSIG3 3              BIOCHSIG4 3           
   BIOCHSIG5 3              BIOCHSIG6 3              BIOCHSIG7 3           
   BIOCHSIG8 3              BIOCHSIG9 3              BIOCHSIG10 3          
   BIOCHCRT1 3              BIOCHCRT2 3              BIOCHCRT3 3           
   BIOCHCRT4 3              BIOCHCRT5 3              BIOCHCRT6 3           
   BIOCHCRT7 3              BIOCHCRT8 3              BIOCHCRT9 3           
   BIOCHCRT10 3             BIOCHGEN1 3              BIOCHGEN2 3           
   BIOCHGEN3 3              BIOCHGEN4 3              BIOCHGEN5 3           
   BIOCHGEN6 3              BIOCHGEN7 3              BIOCHGEN8 3           
   BIOCHGEN9 3              BIOCHGEN10 3             BIOLVEVR1 3           
   BIOLVEVR2 3              BIOLVEVR3 3              BIOLVEVR4 3           
   BIOLVEVR5 3              BIOLVEVR6 3              BIOLVEVR7 3           
   BIOLVEVR8 3              BIOLVEVR9 3              BIOLVEVR10 3          
   BIOHWFAR1 6              BIOHWFAR2 6              BIOHWFAR3 6           
   BIOHWFAR4 6              BIOHWFAR5 4              BIOHWFAR6 6           
   BIOHWFAR7 4              BIOHWFAR8 3              BIOHWFAR9 4           
   BIOHWFAR10 4             BIOWANT1 3               BIOWANT2 3            
   BIOWANT3 3               BIOWANT4 3               BIOWANT5 3            
   BIOWANT6 3               BIOWANT7 3               BIOWANT8 3            
   BIOWANT9 3               BIOWANT10 3              BIOHSOON1 3           
   BIOHSOON2 3              BIOHSOON3 3              BIOHSOON4 3           
   BIOHSOON5 3              BIOHSOON6 3              BIOHSOON7 3           
   BIOHSOON8 3              BIOHSOON9 3              BIOHSOON10 3          
   BIOHOWSN1 4              BIOHOWSN2 4              BIOHOWSN3 4           
   BIOHOWSN4 4              BIOHOWSN5 4              BIOHOWSN6 4           
   BIOHOWSN7 3              BIOHOWSN8 3              BIOHOWSN9 3           
   BIOHOWSN10 3             BIOHPYPG1 3              BIOHPYPG2 3           
   BIOHPYPG3 3              BIOHPYPG4 3              BIOHPYPG5 3           
   BIOHPYPG6 3              BIOHPYPG7 3              BIOHPYPG8 3           
   BIOHPYPG9 3              BIOHPYPG10 3             CRALL 3               
   CRALLU5 3                CRALL518 3               CRMALU5 3             
   CRMAL518 3               CRFEMU5 3                CRFEM518 3            
   NCALL 3                  NCALLU5 3                NCALL518 3            
   NCMALU5 3                NCMAL518 3               NCFEMU5 3             
   NCFEM518 3               RFAGE 4                  RFSEX 3               
   ROUTG04 3                RMEAL04 3                RERRAND04 3           
   RPLAY04 3                RREAD04 3                RAFFECT04 3           
   RPRAISE04 3              RFEED04 3                RBATH04 3             
   RDIAPER04 3              RBED04 3                 RAPPT04 3             
   RDISC04 3                ROUTG518 3               RMEAL518 3            
   RERRAND518 3             RAFFECT518 3             RPRAISE518 3          
   RTAKE518 3               RAPPT518 3               RHELP518 3            
   RDISC518 3               RCLFR518 3               RDO518 3              
   NRFAGE 4                 NRFSEX 3                 NRVISIT04 3           
   NRSATVIS04 3             NROUTG04 3               NRMEAL04 3            
   NRERRAND04 3             NROVRNT04 3              NRPLAY04 3            
   NRREAD04 3               NRAFFECT04 3             NRPRAISE04 3          
   NRFEED04 3               NRBATH04 3               NRDIAPER04 3          
   NRBED04 3                NRAPPT04 3               NRDISC04 3            
   NRVISIT518 3             NRSATVIS518 3            NROUTG518 3           
   NRMEAL518 3              NRERRAND518 3            NROVRNT518 3          
   NRAFFECT518 3            NRPRAISE518 3            NRTAKE518 3           
   NRAPPT518 3              NRHELP518 3              NRDISC518 3           
   NRCLFR518 3              NRDO518 3                NRMONEY 3             
   NREG 3                   NRAGREE 3                NRCHSUPPYR 3          
   COPARENT 3               RWANT 3                  PROBWANT 3            
   JINTEND 3                JSUREINT 3               JINTENDN 3            
   JEXPECTL 3               JEXPECTS 3               JINTNEXT 3            
   INTEND 3                 INTENDN 3                EXPECTL 3             
   EXPECTS 3                INTNEXT 3                USUALCAR 3            
   USLPLACE 3               USL12MOS 3               CURRCOV 3             
   COVERHOW01 3             COVERHOW02 3             COVERHOW03 3          
   COVERHOW04 3             PARINSUR 3               INS_EXCH 3            
   INS_PREM 3               COVER12 3                NUMNOCOV 3            
   YOUGOFPC 3               WHENGOFP 3               YOUFPSVC1 3           
   YOUFPSVC2 3              YOUFPSVC3 3              YOUFPSVC4 3           
   YOUFPSVC5 3              YOUFPSVC6 3              DEAF 3                
   BLIND 3                  DIFDECIDE 3              DIFWALK 3             
   DIFDRESS 3               DIFOUT 3                 EVRCANCER 3           
   AGECANCER 3              CANCTYPE 3               ALCORISK 3            
   VISIT12MO1 3             VISIT12MO2 3             VISIT12MO3 3          
   SVC12MO1 3               SVC12MO2 3               SVC12MO3 3            
   SVC12MO4 3               SVC12MO5 3               SVC12MO6 3            
   SVC12MO7 3               SVC12MO8 3               NUMVISIT 3            
   PLACEVIS01 3             PLACEVIS02 3             PLACEVIS03 3          
   PLACEVIS04 3             PLACEVIS05 3             PLACEVIS06 3          
   SVCPAY1 3                SVCPAY2 3                SVCPAY3 3             
   SVCPAY4 3                SVCPAY5 3                TALKSA 3              
   TALKEC 3                 TALKDM 3                 WHYPSTD 3             
   BARRIER1 3               BARRIER2 3               BARRIER3 3            
   BARRIER4 3               BLDPRESS 3               HIGHBP 3              
   BPMEDS 3                 ASKSMOKE 3               INFHELP 3             
   INFSVCS1 3               INFSVCS2 3               INFSVCS3 3            
   INFSVCS4 3               INFSVCS5 3               INFSVCS6 3            
   INFTEST 3                WHOINSEM 3               INFHLPNW 3            
   LASTVIS_M 3              LASTVIS_Y 4              CMINFVIS 4            
   INFRTHIS_1 3             INFRTHIS_2 3             DONBLOOD 3            
   HIVTEST 3                NOHIVTST 3               WHENHIV_M 3           
   WHENHIV_Y 4              CMHIVTST 4               HIVTSTYR 3            
   HIVRESULT 3              WHYNOGET 3               PLCHIV 3              
   RHHIVT1 3                RHHIVT21 3               HIVTST 3              
   WHOSUGG 3                TALKDOCT 3               AIDSTALK01 3          
   AIDSTALK02 3             AIDSTALK03 3             AIDSTALK04 3          
   AIDSTALK05 3             AIDSTALK06 3             AIDSTALK07 3          
   AIDSTALK08 3             AIDSTALK09 3             AIDSTALK10 3          
   AIDSTALK11 3             SAMEADD 3                CNTRY10 3             
   BRNOUT 3                 YRSTRUS 4                RELRAISD 3            
   ATTND14 3                RELCURR 3                RELTRAD 3             
   FUNDAM1 3                FUNDAM2 3                FUNDAM3 3             
   FUNDAM4 3                RELDLIFE 3               ATTNDNOW 3            
   MILSVC 3                 WRK12MOS 3               FPT12MOS 3            
   DOLASTWK1 3              DOLASTWK2 3              DOLASTWK3 3           
   DOLASTWK4 3              DOLASTWK5 3              DOLASTWK6 3           
   RWRKST 3                 WORKP12 3                RPAYJOB 3             
   RNUMJOB 3                RFTPTX 3                 REARNTY 3             
   SPLSTWK1 3               SPLSTWK2 3               SPLSTWK3 3            
   SPLSTWK4 3               SPLSTWK5 3               SPWRKST 3             
   SPPAYJOB 3               SPNUMJOB 3               SPFTPTX 3             
   SPEARNTY 3               SAMESEX 3                CHSUPPOR 3            
   REACTSLF 3               CHBOTHER 3               SEXNEEDS 3            
   WHENSICK 3               SHOWPAIN 3               PMARCOHB 3            
   COHCHANCE 3              MARRCHANCE 3             PMARCOH 3             
   ACASILANG 3              GENHEALT 3               INCHES 3              
   RWEIGHT 4                BMI 3                    DRWEIGH 3             
   TELLWGHT 3               WGHTSCRN 3               ENGSPEAK 3            
   NOBEDYR 3                STAYREL 3                JAILED 3              
   JAILED2 3                FRQJAIL 3                FRQJAIL2 3            
   EVSUSPEN 3               GRADSUSP 3               SMK100 3              
   AGESMK 3                 SMOKE12 3                SMKSTOP 3             
   DRINK12 3                UNIT30D 3                DRINK30D 3            
   DRINKDAY 3               BINGE30 3                DRNKMOST 3            
   BINGE12 3                POT12 3                  COC12 3               
   CRACK12 3                CRYSTMTH12 3             INJECT12 3            
   MADEPREG 3               PREGTOT2 3               PREGACASI 3           
   NUMABORT 3               NUMLIVEB 3               TOLDPREG 3            
   WHATHAPP 3               FEMTOUCH 3               VAGSEX 3              
   AGEVAGR 3                CONDVAG 3                COND1BRK 3            
   COND1OFF 3               WHYCONDL 3               GETORALF 3            
   CONDFELL 3               GIVORALF 3               ANYORAL 3             
   TIMING 3                 ANALSEX 3                CONDANAL 3            
   OPPSEXANY 3              OPPSEXGEN 3              CONDSEXL 3            
   WANTSEX1 3               HOWOLD 3                 EVRFORCD 3            
   AGEFORC1 3               GIVNDRG2 3               SHEBIGOL 3            
   ENDRELA2 3               WRDPRES2 3               THRTPHY2 3            
   PHYSHRT2 3               HELDDWN2 3               PARTSLIF_1 4          
   PARTSLFV 3               PARTSLIF_2 4             OPPLIFENUM 4          
   PARTS12_1 4              PARTS12V 3               PARTS12_2 4           
   OPPYEARNUM 4             NEWYEAR 4                NEWLIFE 4             
   VAGNUM12 4               ORALNUM12 4              ANALNUM12 4           
   NONMONOG 3               NNONMONOG1 3             NNONMONOG2 3          
   NNONMONOG3 3             FEMSHT12 3               JOHNFREQ 3            
   PROSTFRQ 3               HIVFEM12 3               GIVORALM 3            
   GETORALM 3               ORALCONDM 3              ANALSEX2 3            
   ANALCONDM1 3             ANALSEX3 3               ANALCONDM2 3          
   MALESEX 3                SAMESEXANY 3             MALPRTAGE 3           
   MALPRTHRACE 3            EVRFORC2 3               AGEFORC2 3            
   GIVNDRG3 3               HEBIGOLD 3               ENDRELA3 3            
   WRDPRES3 3               THRTPHY3 3               PHYSHRT3 3            
   HELDDWN3 3               MALEPRTS_1 4             MALEPRTSV 3           
   MALEPRTS_2 4             SAMLIFENUM 4             MALPRT12_1 4          
   MALPRT12V 3              MALPRT12_2 4             SAMYEARNUM 4          
   SAMORAL12 4              RECEPANAL12 4            INSERANAL12 4         
   SAMESEX1 3               MSAMEREL 3               MSMNONMON 4           
   MALSHT12 3               JOHN2FRQ 3               PROS2FRQ 3            
   HIVMAL12 3               MSMWEB12 3               MSMSORT12 3           
   CNDLSMAL 3               CONDALLS 3               MFLASTP 3             
   WHYCOND 3                ATTRACT 3                ORIENT_A 3            
   ORIENT_B 3               CONFCONC 3               TIMALON 3             
   RISKCHEK1 3              RISKCHEK2 3              RISKCHEK3 3           
   RISKCHEK4 3              RECTDOUCH 3              STDTST12 3            
   STDSITE12 3              STDTRT12 3               GON 3                 
   CHLAM 3                  HERPES 3                 GENWARTS 3            
   SYPHILIS 3               EVRINJECT 3              EVRSHARE 3            
   EARNTYPE 3               EARN 3                   EARNDK1 3             
   EARNDK2 3                EARNDK3 3                EARNDK4 3             
   TOINCWMY 3               TOTINC 3                 FMINCDK1 3            
   FMINCDK2 3               FMINCDK3 3               FMINCDK4 3            
   FMINCDK5 3               PUBASST 3                PUBASTYP1 3           
   FOODSTMP 3               WIC 3                    HLPTRANS 3            
   HLPCHLDC 3               HLPJOB 3                 FREEFOOD 3            
   HUNGRY 3                 MED_COST 3               AGER 3                
   FMARITAL 3               RMARITAL 3               EDUCAT 3              
   HIEDUC 3                 HISPANIC 3               RACE 3                
   HISPRACE 3               HISPRACE2 3              NUMKDHH 3             
   NUMFMHH 3                HHFAMTYP 3               HHPARTYP 3            
   NCHILDHH 3               HHKIDTYP 3               CSPBBHH 3             
   CSPSBHH 3                CSPOKDHH 3               INTCTFAM 3            
   PARAGE14 3               EDUCMOM 3                AGEMOMB1 3            
   FMARNO 3                 AGER_I 3                 FMARITAL_I 3          
   RMARITAL_I 3             EDUCAT_I 3               HIEDUC_I 3            
   HISPANIC_I 3             RACE_I 3                 HISPRACE_I 3          
   HISPRACE2_I 3            NUMKDHH_I 3              NUMFMHH_I 3           
   HHFAMTYP_I 3             HHPARTYP_I 3             NCHILDHH_I 3          
   HHKIDTYP_I 3             CSPBBHH_I 3              CSPSBHH_I 3           
   CSPOKDHH_I 3             INTCTFAM_I 3             PARAGE14_I 3          
   EDUCMOM_I 3              AGEMOMB1_I 3             FMARNO_I 3            
   HADSEX 3                 SEXONCE 3                VRY1STSX 4            
   FIRSTPFLAG 3             VRY1STAG 3               ELAPSED 4             
   SEXMAR 3                 SEXUNION 3               FSEXRLTN 3            
   SEX1MTHD1 3              SEX1MTHD2 3              SEX1MTHD3 3           
   SEX1MTHD4 3              LSEXDATE 4               SEX3MO 3              
   SEX12MO 3                LSEXRAGE 3               LSEXPRAC 3            
   LSEXRLTN 3               LSEXUSE1 3               LSEXUSE2 3            
   LSEXUSE3 3               LSEXUSE4 3               METH12M1 3            
   METH12M2 3               METH12M3 3               METH12M4 3            
   METH3M1 3                METH3M2 3                METH3M3 3             
   METH3M4 3                NUMP3MOS 3               LIFPRTNR 3            
   PARTS1YR 3               PARTDUR1 4               PARTDUR2 4            
   PARTDUR3 4               COHEVER 3                EVMARCOH 3            
   PMARRNO 3                NONMARR 3                TIMESCOH 3            
   MARDAT01 4               MARDAT02 4               MARDAT03 4            
   MARDAT04 4               MAREND01 3               MAREND02 3            
   MAREND03 3               MAREND04 3               MARDIS01 4            
   MARDIS02 4               MARDIS03 4               MARDIS04 4            
   MAR1DISS 3               PREMARW1 3               COHAB1 4              
   COHSTAT 3                COHOUT 3                 COH1DUR 3             
   CSPBIOKD 3               DATBABY1 4               AGEBABY1 3            
   B1PREMAR 3               MARBABY1 3               CEBOW 3               
   CEBOWC 3                 CEBOWP 3                 EVRNOPAT 3            
   NONLIVEB 3               COMPREG 3                ABORTION 3            
   LOSSNUM 3                PARENT01 3               PARENT02 3            
   PARENT03 3               PARENT04 3               PARENT05 3            
   PARENT06 3               PARENT07 3               PARENT08 3            
   PARENT09 3               PARENT10 3               WANTB01 3             
   WANTB02 3                WANTB03 3                WANTB04 3             
   WANTB05 3                WANTB06 3                WANTB07 3             
   WANTB08 3                WANTB09 3                WANTB10 3             
   HADSEX_I 3               SEXONCE_I 3              VRY1STSX_I 3          
   VRY1STAG_I 3             SEXMAR_I 3               SEXUNION_I 3          
   FSEXRLTN_I 3             SEX1MTHD1_I 3            SEX1MTHD2_I 3         
   SEX1MTHD3_I 3            SEX1MTHD4_I 3            LSEXDATE_I 3          
   SEX3MO_I 3               SEX12MO_I 3              LSEXRAGE_I 3          
   LSEXRLTN_I 3             LSEXUSE1_I 3             LSEXUSE2_I 3          
   LSEXUSE3_I 3             LSEXUSE4_I 3             METH12M1_I 3          
   METH12M2_I 3             METH12M3_I 3             METH12M4_I 3          
   METH3M1_I 3              METH3M2_I 3              METH3M3_I 3           
   METH3M4_I 3              NUMP3MOS_I 3             LIFPRTNR_I 3          
   PARTS1YR_I 3             PARTDUR1_I 3             PARTDUR2_I 3          
   PARTDUR3_I 3             COHEVER_I 3              EVMARCOH_I 3          
   PMARRNO_I 3              NONMARR_I 3              TIMESCOH_I 3          
   MARDAT01_I 3             MARDAT02_I 3             MARDAT03_I 3          
   MARDAT04_I 3             MAREND01_I 3             MAREND02_I 3          
   MAREND03_I 3             MAREND04_I 3             MARDIS01_I 3          
   MARDIS02_I 3             MARDIS03_I 3             MARDIS04_I 3          
   MAR1DISS_I 3             PREMARW1_I 3             COHAB1_I 3            
   COHSTAT_I 3              COHOUT_I 3               COH1DUR_I 3           
   CSPBIOKD_I 3             DATBABY1_I 3             AGEBABY1_I 3          
   B1PREMAR_I 3             MARBABY1_I 3             CEBOW_I 3             
   CEBOWC_I 3               CEBOWP_I 3               EVRNOPAT_I 3          
   NONLIVEB_I 3             COMPREG_I 3              ABORTION_I 3          
   LOSSNUM_I 3              PARENT01_I 3             PARENT02_I 3          
   PARENT03_I 3             PARENT04_I 3             PARENT05_I 3          
   PARENT06_I 3             PARENT07_I 3             PARENT08_I 3          
   PARENT09_I 3             PARENT10_I 3             WANTB01_I 3           
   WANTB02_I 3              WANTB03_I 3              WANTB04_I 3           
   WANTB05_I 3              WANTB06_I 3              WANTB07_I 3           
   WANTB08_I 3              WANTB09_I 3              WANTB10_I 3           
   DADTYPE 3                DADTYPU5 3               DADTYP518 3           
   NUMCRU18 3               NUMNCU18 3               SUPP12MO 3            
   DADTYPE_I 3              DADTYPU5_I 3             DADTYP518_I 3         
   NUMCRU18_I 3             NUMNCU18_I 3             SUPP12MO_I 3          
   INTENT 3                 ADDEXP 4                 INTENT_I 3            
   ADDEXP_I 3               CURR_INS 3               INFEVER 3             
   EVHIVTST 3               CURR_INS_I 3             INFEVER_I 3           
   EVHIVTST_I 3             METRO 3                  RELIGION 3            
   LABORFOR 3               METRO_I 3                RELIGION_I 3          
   LABORFOR_I 3             POVERTY 4                TOTINCR 3             
   PUBASSIS 3               POVERTY_I 3              TOTINCR_I 3           
   PUBASSIS_I 3             SECU 3                   SEST 4                
   CMINTVW 4                CMLSTYR 4                CMFIVYR 4 ;

RUN ;
