
source('code/source_code.R')
source('code/process_results/make_systematic_table.R')
source('code/targets/target_msas.R')
source('code/interventions/melissa_croi_interventions.R')

#### New set of interventions - 11/18/20 ####

location = LA.MSA
if (1==2)
{
  location = LA.MSA
  load(file.path('mcmc_runs/quick_simsets', location, get.full.filename(location)))
  
  run.systematic.interventions(simset,
                               dst.dir = 'mcmc_runs/quick_simsets',
                               interventions = MELISSA.INTERVENTIONS.2,
                               save.baseline.and.seed = F)
}

if (1==2)
{
  location = LA.MSA
  intervention.code.table = matrix(c('baseline', 'ybhm.p25', 'ybhm.p50', 'ybhm.s80', 'm.p25.s80', 'm.p50.s80', 'ybhm.s90', 'm.p25.s90', 'm.p50.s90'),
                                   ncol=9)
  
  tab = get.estimates.for.interventions(intervention.codes=intervention.code.table,
                                        location=location,
                                        dir='mcmc_runs/quick_simsets')
  
}

write.csv(tab, file = "Melissa_CROI_v2.csv")




#### Original analysis - 11/9/2020 ####

location = LA.MSA
#This code will run your list of interventions
if (1==2)
{
    location = LA.MSA
    load(file.path('mcmc_runs/quick_simsets', location, get.full.filename(location)))
    
    run.systematic.interventions(simset,
                                 dst.dir = 'mcmc_runs/quick_simsets',
                                 interventions = MELISSA.INTERVENTIONS,
                                 save.baseline.and.seed = F)
}

#This code is going to make you a nice table,
# then write it to a pretty excel file
if (1==2)
{
    location = LA.MSA
    # MS: I updated this based on the order I listed in the previous code 
    # MELISSA - you need to change the 'INT.TTT' to 'm.ttt' (or analagous) for each of these, like I did below
    intervention.code.table = matrix(c('m.ttt', 'm.stt', 'm.ptt', 'm.tts', 'm.sts',  'm.pts', 'm.ttp',  'm.stp', 'm.ptp', 
                                        'm.tst', 'm.sst', 'm.pst', 'm.tss', 'm.sss', 'm.pss', 'm.tsp', 'm.ssp', 'm.psp', 
                                        'm.tpt', 'm.spt', 'm.ppt', 'm.tps', 'm.sps', 'm.pps', 'm.tpp', 'm.spp', 'm.ppp'),
                                     ncol=3)
    
    #Melissa, this should be a table with the intervention codes listed in the
    # order (row/column) in which you want the results to appear
    #I think this matches what you had, but I could be wrong
    # intervention.code.table = matrix(c('INT.TTT', 'INT.TTP', 'INT.TTS', 'INT.PTT', 'INT.PTP', 'INT.PTS', 'INT.STT', 'INT.STP', 'INT.STS',
    #                                    'INT.TPT', 'INT.TPP', 'INT.TPS', 'INT.PPT', 'INT.PPP', 'INT.PPS', 'INT.SPT', 'INT.SPP', 'INT.SPS',
    #                                    'INT.TST', 'INT.TSP', 'INT.TSS', 'INT.PST', 'INT.PSP', 'INT.PSS', 'INT.SST', 'INT.SSP', 'INT.SSS'),
    #                                  ncol=3)
    

    tab = get.estimates.for.interventions(intervention.codes=intervention.code.table,
                                          location=location,
                                          dir='mcmc_runs/quick_simsets')
    
    write.shaded.table(tab, file='whatever file you want to save to.xlsx')
    
}

#Todd's test - melissa, you can ignore this
# (just making sure the code I gave you was correct)
if (1==2)
{
    int.code.table = matrix(INTERVENTION.MANAGER.1.0$code[1:4], nrow=2)
    test.tab = get.estimates.for.interventions(intervention.codes = int.code.table,
                                               location = NYC.MSA,
                                               dir = 'mcmc_runs/visualization_simsets/')
    write.shaded.table(test.tab, file='../temp/test.xlsx')
    
    intervention.code.table = matrix(c('m.ttt', 'm.tst'),
                                     ncol=2)
  
    tab = get.estimates.for.interventions(intervention.codes=intervention.code.table,
                                          location=location,
                                          dir='mcmc_runs/quick_simsets')
}