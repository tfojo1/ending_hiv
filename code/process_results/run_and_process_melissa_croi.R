
source('code/source_code.R')
source('code/process_results/make_systematic_table.R')
source('code/targets/target_msas.R')
source('code/interventions/melissa_croi_interventions.R')

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
    intervention.code.table = matrix(c('m.ttt', 'm.tst', 'm.tpt', 'm.stt', 'm.sst', 'm.spt', 'm.ptt', 'm.pst', 'm.ppt', 
                                        'm.tts', 'm.tss', 'm.tps', 'm.sts', 'm.sss', 'm.sps', 'm.pts', 'm.pss', 'm.pps', 
                                        'm.ttp', 'm.tsp', 'm.tpp', 'm.stp', 'm.ssp', 'm.spp', 'm.ptp', 'm.psp', 'm.ppp'),
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