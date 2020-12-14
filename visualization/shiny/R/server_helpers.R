
##--------------------------------------##
##          server_helpers.R            ##
##                                      ##
##   Contains helper functions for the  ##
##   main server.R file                 ##
##--------------------------------------##

# Plot functionality ####
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
        intervention.codes = c('No Intervention' = get.intervention.code(NO.INTERVENTION))
    else
        intervention.codes = character()
    
    intervention.codes = c(
        intervention.codes,
        'Intervention 1' = get.intervention.selection(1, input)
        )
    
    if (input$use_intervention_2)
        intervention.codes = c(
            intervention.codes,
            'Intervention 2' = get.intervention.selection(2, input)
        )
    
    intervention.codes = intervention.codes[intervention.codes != 'none']
    
    #-- Get the filenames to pre-cache --#
    filenames = get.sim.filenames.to.load(
        version,
        location=input$geographic_location,
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
        location=input$geographic_location,
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
        plot.format=input[['plot_format']],
        plot.interval.coverage = input[['interval_coverage']]/100,
        label.change = input[['label_change']],
        change.years = input[['change_years']],
        change.decrease.is.positive = F)
    
    #-- Make the mode bar always visible --#
    
    plot.results$plot = format.plotly.toolbar(plot.results$plot,
                                              input)
    
    
    #-- Return --#
    plot.results
}

format.plotly.toolbar <- function(plot,
                                  input)
{
  # https://plotly.com/r/reference/#layout-updatemenus
  plot = layout(plot,
                modebar=list(
                  orientation='v',
                  borderwidth=1,
                  bordercolor='black',
                  bgcolor='#3c8dbc',#'#f6d8ac',
                  font=list(size=30),
                  position='left',
                  height='30px',
                  color='#f1f7e7',##3c8dbc',
                  activecolor='#b8d585',#'#255876',
                  x=0,
                y=0,
                xanchor='left',
                yanchor='bottom'
                ))
  
  plot = config(plot,
                displayModeBar=T,
                displaylogo=F,
                scrollZoom=F,
                
                toImageButtonOptions=list(filename=get.default.download.filename(input)),
                
                modeBarButtons=list(
            #      list('toImage'),
                  list('zoom2d'),
                  list('pan2d'),
                  list('zoomIn2d'),
                  list('zoomOut2d'),
                  list('autoScale2d')
                )
  )
   #modebar button options at 
  # https://github.com/plotly/plotly.js/blob/master/src/components/modebar/buttons.js
  
  
  plot
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
    
    plot = plot = config(plot, displayModeBar=F)
    
    plot
}

# Location names ####
##--------------------##
##-- LOCATION NAMES --##
##--------------------##
get.location.long.name <- function(location)
{
    unlist(msa.names(location))
}


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

MSA.SHORT.NAMES = c(
  '35620' = 'NYC',
  '33100' = 'Miami',
  '31080' = 'LA',
  '12060' = 'Atlanta',
  '26420' = 'Houston',
  '19100' = 'Dallas',
  '16980' = 'Chicago',
  '47900' = 'DC',
  '37980' = 'Philadelphia',
  '36740' = 'Orlando',
  '41860' = 'SF',
  '38060' = 'Phoenix',
  '45300' = 'Tampa',
  '40140' = 'Riverside',
  '19820' = 'Detroit',
  '12580' = 'Baltimore',
  '29820' = 'Vegas',
  '14460' = 'Boston',
  '41740' = 'San_Diego',
  '16740' = 'Charlotte',
  '41700' = 'San_Antonio',
  '27260' = 'Jacksonville',
  '35380' = 'New_Orleans',
  '32820' = 'Memphis',
  '42660' = 'Seattle',
  '12420' = 'Austin',
  '26900' = 'Indianapolis',
  '17140' = 'Cincinatti',
  '18140' = 'Columbus',
  '12940' = 'Baton_Rouge',
  '40900' = 'Sacramento',
  '17460' = 'Cleveland'
)

get.location.short.name <- function(location)
{
    MSA.SHORT.NAMES[location]
}

get.default.download.filename <- function(input,
                                          ext='')
{
    if (ext != '' && !grepl("^\\.", ext))
        ext = paste0(".", ext)
    
    location = NULL
    data.types = input[['epidemiological-indicators']]
    
    facet.by = input[['facet']]
    split.by = input[['split']]
    
    if (length(facet.by)==0 && length(split.by)==0)
        by.suffix = ''
    else
        by.suffix = paste0(" by ",
                           paste0(unique(c(facet.by, split.by)), collapse=', ')
                           )
    
    paste0(get.location.short.name(location), " - ",
           paste0(DATA.TYPE.NAMES[data.types], collapse=", "),
           by.suffix,
           ext)
}

# Modal dialogue boxes ####
showMessageModal <- function(message) {
  showModal(
    modalDialog(
      title=NULL,
      footer=modalButton("Dismiss"),
      size=c("m", "s", "l"),
      easyClose=FALSE,
      fade=TRUE,
      
      verticalSpacer(40),
      HTML(paste0('<br/><br/>', message, '<br/><br/>'))))
}
