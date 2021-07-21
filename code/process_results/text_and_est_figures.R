

source('code/source_code.R')
source('code/targets/target_msas.R')
source('code/process_results/make_systematic_table.R')
source('code/process_results/distributed_process_results.R')
source('code/process_results/make_pretty_table.R')


FIGURE.DIR = '../Manuscripts/manuscript_1/Annals Submission/revision 1/tables/'

TOTAL.ROW = 33
NOINT.COL = 1
YBHM.COLS = 6:9
MI.COLS = 10:13
MAIN.SINGLE.COLS = 3:5
ALL.POP.COLS = 14:17
BROAD.MOD.COL = 2

if (1==2)
{
    load('results/full/estimates/ests.main.Rdata')
    load('results/full/estimates/ests.main.2025.Rdata')
    load('results/full/estimates/ests.main.new.Rdata')
    load('results/full/estimates/ests.main.new.2025.Rdata')
    load('results/full/estimates/ests.rollout.3y.Rdata')
    load('results/full/estimates/ests.rollout.3y.2025.Rdata')
    load('results/full/estimates/ests.single.mod.2025.Rdata')
    load('results/full/estimates/ests.single.mod.Rdata')
    load('results/full/estimates/ests.idu.Rdata')
    
    
    write.systematic.table(ests.main, 
                           interventions=MAIN.INTERVENTIONS.23.27,
                           file=file.path(FIGURE.DIR, 'main_table.xlsx'))
    
    msa.names(names(ests.main$estimates[,NOINT.COL])[!is.na(ests.main$estimates[,NOINT.COL]) & ests.main$estimates[,NOINT.COL]==max(ests.main$estimates[,NOINT.COL], na.rm=T)])
    msa.names(names(ests.main$estimates[,NOINT.COL])[!is.na(ests.main$estimates[,NOINT.COL]) & ests.main$estimates[,NOINT.COL]==min(ests.main$estimates[,NOINT.COL], na.rm=T)])

    
    
    write.systematic.table(ests.main.new, 
                           interventions=MAIN.INTERVENTIONS.23.27,
                           file=file.path(FIGURE.DIR, 'main_table_new.xlsx'))
    
    
    #-- How many scenarios achieve target --#
    length(ests.main$estimates[-NOINT.COL,-TOTAL.ROW])
    
    # 5y rollout
    meets.2025.main = ests.main.2025$estimates[-NOINT.COL,-TOTAL.ROW]>=0.75
    mean(meets.2025.main, na.rm=T)
    sum(meets.2025.main, na.rm=T)
    
    meets.2030.main = ests.main$estimates[-NOINT.COL,-TOTAL.ROW]>=0.9
    mean(meets.2030.main, na.rm=T)
    sum(meets.2030.main, na.rm=T)
    
    #any city
    sum(apply(meets.2030.main, 1, any), na.rm=T)
    
    # 3y rollout
    meets.2025.rollout.3y = ests.rollout.3y.2025$estimates[-NOINT.COL,-TOTAL.ROW]>=0.75
    mean(meets.2025.rollout.3y, na.rm=T)
    sum(meets.2025.rollout.3y, na.rm=T)
    
    meets.2030.rollout.3y = ests.rollout.3y$estimates[-NOINT.COL,-TOTAL.ROW]>=0.9
    mean(meets.2030.rollout.3y, na.rm=T)
    sum(meets.2030.rollout.3y, na.rm=T)
    
    sum(!meets.2025.rollout.3y & meets.2030.rollout.3y, na.rm=T)
    
    # 3 vs 5y rollout
    sum(meets.2030.main & !meets.2030.rollout.3y, na.rm=T)
    sum(!meets.2030.main & meets.2030.rollout.3y, na.rm=T)
    sum(meets.2030.main & meets.2030.rollout.3y, na.rm=T)
    
    # Effects of single-modality
    range(ests.main$estimates[-TOTAL.ROW, MAIN.SINGLE.COLS], na.rm=T)
    
    # Effects of yhbm
    range(ests.main$estimates[-TOTAL.ROW, YBHM.COLS], na.rm=T)

    # Effects of mi
    range(ests.main$estimates[-TOTAL.ROW, MI.COLS], na.rm=T)

    # Effects of all pop
    range(ests.main$estimates[-TOTAL.ROW, ALL.POP.COLS], na.rm=T)
    
    # Broad moderate improvement
    range(ests.main$estimates[-TOTAL.ROW, BROAD.MOD.COL], na.rm=T)
    
    
    # single-modality interventions
    
    supp.80.to.90 = ests.single.mod$estimates[-TOTAL.ROW, 'mi.s90_23.27'] - ests.single.mod$estimates[-TOTAL.ROW, 'mi.s80_23.27']
    prep.10.to.25 = ests.single.mod$estimates[-TOTAL.ROW, 'mi.p25_23.27'] - ests.single.mod$estimates[-TOTAL.ROW, 'mi.p10_23.27']
    
    sum(supp.80.to.90 > prep.10.to.25, na.rm=T)
    mean(supp.80.to.90 > prep.10.to.25, na.rm=T)
    
    # New dx - the approximate 95-95-95 for Bradley comparison
    floor(100 * ests.approx959595.3y.new$estimates[TOTAL.ROW,])
    floor(100 * apply(ests.approx959595.3y.new$estimates,2,range, na.rm=T))
    
    msa.names(names(ests.approx959595.3y.new$estimates[,NOINT.COL])[!is.na(ests.approx959595.3y.new$estimates[,NOINT.COL]) & ests.approx959595.3y.new$estimates[,NOINT.COL]==max(ests.approx959595.3y.new$estimates[,NOINT.COL], na.rm=T)])
    msa.names(names(ests.approx959595.3y.new$estimates[,NOINT.COL])[!is.na(ests.approx959595.3y.new$estimates[,NOINT.COL]) & ests.approx959595.3y.new$estimates[,NOINT.COL]==min(ests.approx959595.3y.new$estimates[,NOINT.COL], na.rm=T)])
    
    
    # IDU estimates
    cbind(floor(100*(ests.idu$estimates[TOTAL.ROW,] - ests.idu$estimates[TOTAL.ROW, NOINT.COL])),
            t(floor(100*apply(ests.idu$estimates[-TOTAL.ROW,]-ests.idu$estimates[-TOTAL.ROW,NOINT.COL], 2, range, na.rm=T))))
    
    cbind(floor(100*(ests.idu$estimates[TOTAL.ROW,-NOINT.COL] - ests.idu$estimates[TOTAL.ROW, 9])),
         t(floor(100*apply(ests.idu$estimates[-TOTAL.ROW,-NOINT.COL]-ests.idu$estimates[-TOTAL.ROW,9], 2, range, na.rm=T))))
    
}


# BASELINE LEVELS
if (1==2)
{
    source('code/process_results/make_pretty_table.R')
    
    
    #-- PrEP --#
    load('results/full/baseline.prep.Rdata')
    
    write.shaded.table(x=baseline.prep$estimates,
                       file=file.path(FIGURE.DIR, 'baseline.prep.xlsx'),
                       lowers = baseline.prep$ci.lower,
                       uppers = baseline.prep$ci.upper,
                       max.value = .2,
                       threshold = 0.1,
                       label.lower = 0.01,
                       as.pct = T,
                       digits = 0,
                       use.floor.not.round = F)
    
    round(100*apply(baseline.prep$estimates, 2, range, na.rm=T),0)[,5:6]
    
    
    #-- Testing --#
    load('results/full/baseline.testing.Rdata')
    
    write.shaded.table(x=baseline.testing$estimates,
                       file=file.path(FIGURE.DIR, 'baseline.testing.xlsx'),
                       lowers = baseline.testing$ci.lower,
                       uppers = baseline.testing$ci.upper,
                       max.value = 2,
                       threshold = 1,
                       label.lower = 0.01,
                       as.pct = F,
                       digits = 1,
                       use.floor.not.round = F)
    
    round(apply(baseline.testing$estimates, 2, range, na.rm=T),1)[,5:6]
    
    
    #-- Suppression --#
    load('results/full/baseline.suppression.Rdata')
    
    write.shaded.table(x=baseline.suppression$estimates,
                       file=file.path(FIGURE.DIR, 'baseline.suppression.xlsx'),
                       lowers = baseline.suppression$ci.lower,
                       uppers = baseline.suppression$ci.upper,
                       max.value = 1,
                       threshold = 0.8,
                       label.lower = 0.01,
                       as.pct = T,
                       digits = 0,
                       use.floor.not.round = F)
    
    round(100*apply(baseline.suppression$estimates, 2, range, na.rm=T),0)[,5:6]
    
}
