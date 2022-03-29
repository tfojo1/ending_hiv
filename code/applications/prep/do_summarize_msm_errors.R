

source('code/source_code.R')

is.desktop = dir.exists('Q:Ending_HIV')
# to run on big desktop
if (is.desktop)
{
    print("Running on DESKTOPs")
    source('code/processing/outcome_formatting.R')
    source('code/processing/generalized_extract_results.R')
    source('code/processing/error_summaries.R')
    
    
    error.summaries = make.error.summary.table(risks=c('msm','msm_idu'),
                                               locations=TARGET.MSAS,
                                               by.risk=T,
                                               by.age=F,
                                               by.sex=F)
    
    save(error.summaries, file='results/prep/msm_error_summaries.Rdata')

}

#to run on local machine
if (!is.desktop)
{
    print('Running on LAPTOP')
    source('code/processing/pretty_table/make_pretty_table.R')
    source('code/processing/pretty_table/pretty_table_colors.R')
    
    load('results/prep/msm_error_summaries.Rdata')
    
    error.summaries$pretty$new[is.infinite(error.summaries$mape$new)] = "NA"
    error.summaries$mape$new[is.infinite(error.summaries$mape$new)] = NA
    
    write.shaded.table(tab=error.summaries$pretty$new,
                       color.by=error.summaries$mape$new,
                       file='results/prep/msm_new_errors.xlsx',
                       thresholds=c(0,1),
                       lower.threshold.colors = PRETTY.ERROR.COLORS[1],
                       upper.threshold.colors = PRETTY.ERROR.COLORS[2],
                       na.color='grey')
    
    write.shaded.table(tab=error.summaries$pretty$prevalence,
                       color.by=error.summaries$mape$prevalence,
                       file='results/prep/msm_prevalence_errors.xlsx',
                       thresholds=c(0,1),
                       lower.threshold.colors = PRETTY.ERROR.COLORS[1],
                       upper.threshold.colors = PRETTY.ERROR.COLORS[2])
}