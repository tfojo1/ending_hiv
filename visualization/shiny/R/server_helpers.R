
##--------------------------------------##
##          server_helpers.R            ##
##                                      ##
##   Contains helper functions for the  ##
##   main server.R file                 ##
##--------------------------------------##

##---------------------------------------------##
##-- THE MAIN PLOT/TABLE GENERATING FUNCTION --##
##---------------------------------------------##

generate.plot.and.table <- function(input, cache) 
{
    # Pre-processing, data fetching & caching ####
    # For now there is only one version
    version = get.version()
    
    #-- Pull Intervention Names from Input --#
    if (input[['no_intervention_checkbox']])
        intervention.codes = get.intervention.code(NO.INTERVENTION)
    else
        intervention.codes = character()
    
    intervention.codes = c(
        intervention.codes,
        get.intervention.selection(1, input))
    
    intervention.codes = intervention.codes[intervention.codes != 'none']
    
    #-- Get the filenames to pre-cache --#
    filenames = get.sim.filenames.to.load(
        version,
        location=input[['geographic_location']],
        intervention.codes=intervention.codes)
    filenames = filenames[!is.sim.cached(filenames, cache=cache)]
    
    #-- Pre-fetch the simsets --#
    if (length(filenames)>0)
    {
      if (length(filenames)==1)
        msg = "Fetching 1 Simulation File from Remote Server:"
      else
        msg = paste0("Fetching ", length(filenames), 
                     " Simulation Files from Remote Server:")
        withProgress(
          message=msg, min=0, max=1, value=0.2/length(filenames),
          detail=paste("Fetching file 1 of ", length(filenames)),
            {
                for (i in 1:length(filenames))
                {
                    if (i>1)
                        setProgress(
                            (i-1)/length(filenames),
                            detail=paste("Fetching file ", i, " of ", 
                                         length(filenames)))
                    filename = filenames[i]
                    cache = update.sims.cache(
                      filenames=filename,
                      cache=cache)
                    
                    setProgress(1, detail='Done')
                }
            })
    }
    
    #-- Make the plot --# ####
    plot.results = make.simulations.plot.and.table(
        cache=cache,
        version=version,
        location=input[['geographic_location']],
        intervention.codes=intervention.codes,
        # years=input[['years']][1]:input[['years']][2],
        years=get.year.options(
            version,
            get.location.options(version)[1]),
        data.types=input[['epidemiological-indicators']],
        facet.by=input[['facet']],
        split.by=input[['split']],
        dimension.subsets=list(  # TODO
            'age'=input[['age-groups']],
            'race'=input[['racial-groups']],
            'sex'=input[['sex']],  # aka gender
            'risk'=input[['risk-groups']]),
        plot.format=input[['aggregation-of-simulations-ran']] )
    
    #-- Return --#
    plot.results
}

##----------------------------------------------##
##-- GENERATE MESSAGE for PLOT PANEL if EMPTY --##
##----------------------------------------------##

BLANK.MESSAGE = "Select intervention(s) and click 'Generate Projections'"
make.plotly.message <- function(message=BLANK.MESSAGE)
{
    plot = plotly_empty()
    plot = add_text(plot, text=message, x=0, y=0)
    #plot = layout(plot, xaxis=list(range = c(-0.5,0.5)))
    #plot = layout(plot, uniformtext=list(minsize=8, mode='hide'))
    
    plot
}