
NO.CUSTOM.INTERVENTIONS.MESSAGE = 'No custom interventions have been run. Go to the "Custom Interventions" panel on the left to design and run custom interventions'

TAB1.TITLE.COLOR = '#3c8dbc' # '#737CA1' # '#98AFC7'
TAB1.TITLE.FONT.SIZE = '0.85em'

TAB2.TITLE.COLOR = '#3c8dbc' # '#737CA1' # '#98AFC7'
TAB2.TITLE.FONT.SIZE = '0.85em'

TPOP.SELECTOR.WIDTH = 3#4
UNIT.SELECTOR.WIDTH = 3#6

create.intervention.selector.panel <- function(
  num, input, state, lump.idu=T, title=paste0("Intervention ", num)
) {
  #-- Set up the Pre-Run Panel --#
  # to-do?: I tried changing state() to input for geographic location below, but
  # when i went to plot, got the following error. - jef
  # Warning: Error in :: NA/NaN argument
  # 76: [.data.frame
  #      74: make.pretty.change.data.frame [R/model_code/plot_simulations.R#1441]
  #                                         73: reset.handler [/Users/joeflack4/projects/ending-hiv/visualization/shiny/server.R#175]
  #                                                            72: observeEventHandler [/Users/joeflack4/projects/ending-hiv/visualization/shiny/server.R#188]
  #                                                                                     1: shiny::runApp  
  interventions = get.intervention.options(
      version=get.version(input),
      # location=input$geographic_location,
      location=state()[['geographic_location']],
      return.intervention.objects = T)
    
    interventions = interventions[!sapply(interventions, is.null.intervention)]
    
    if (length(interventions)==0)
    {
        return(flowLayout(textOutput(outputId = paste0('temp_selector_',num))))
    }
    
    prerun.panel = make.interventions.by.tpop.panel(num=num,
                                                    interventions=interventions,
                                                    state=state,
                                                    lump.idu=lump.idu)
   
    #-- Make the Custom Intervention Panel --#
    
    custom.panel = div(
             verbatimTextOutput(
               placeholder=FALSE,
               paste0('custom_int_msg_',num)
             )
      )
#    custom.panel = div(NO.CUSTOM.INTERVENTIONS.MESSAGE)
    #-- Put them together into a panel --#
    
    prerun.wrapper = tabPanel(title='Pre-Specified Interventions',
                              value='prerun',
                              prerun.panel)
    
    custom.wrapper = tabPanel(title='Custom Interventions',
                              value='custom',
                              custom.panel)
    
    fluidRow(
        column(width=12,
               tipBox('First, choose from either Pre-Specified or Custom Interventions
                      <ul><li><b>Pre-Specified Interventions</b> are interventions that have already been designed and run across simulations. You can choose from the list below.</li>
                      <li><b>Custom Interventions</b> are interventions that you can design using the "Custom Interventions" tab at the left. This allows you to choose any target subgroups and levels of testing, PrEP, and suppression, but will take 5-10 minutes to run.</li></ul>',
                      left.arrow = T, left.arrow.align='middle', left.arrow.direction='down'),
               tabsetPanel(
                 id=paste0("intervention_",num,"_selector"),
                 # selected='prerun',
                 selected=state()[[paste0("intervention_",num,"_selector")]],
                 #color=TAB.TITLE.COLOR,
                 prerun.wrapper,
                 custom.wrapper)
        ))
}

get.intervention.selection <- function(num, input)
{
    top.level.selector.id = paste0("intervention_",num,"_selector")
    top.level.selection = input[[top.level.selector.id]]
    
    if (top.level.selection=='custom')
    {
        #for now
        'none'
    }
    else
    {
        tpop.selector.id = paste0("preset_tpop_",num)
        tpop.selection = input[[tpop.selector.id]]
        
        if (tpop.selection=='none')
            'none'
        else
        {
            unit.selector.id = tpop.selection
            unit.selection = input[[unit.selector.id]]
            
            intervention.selector.id = unit.selection
            input[[intervention.selector.id]]
        }
    }
}

make.interventions.by.tpop.panel <- function(
  num, interventions, state, lump.idu=T)
{
    #-- Create a mapping for interventions to target populations and target populations to interventions --#
    
    if (lump.idu)
        interventions.lumped.idu = lapply(interventions, lump.idu.for.intervention)
    else
        interventions.lumped.idu = interventions
    
    target.population.codes.for.intervention = lapply(
      interventions.lumped.idu, function(int) {
        sapply(
          get.target.populations.for.intervention(int), 
          target.population.to.code)
    })
    
    unique.tpop.codes = unique(target.population.codes.for.intervention)
    
    interventions.for.unique.tpop.codes = lapply(
      unique.tpop.codes, function(tpop.codes){
        mask = sapply(target.population.codes.for.intervention, setequal, tpop.codes)
        interventions[mask]
    })
    
    lumped.idu.interventions.for.unique.tpop.codes = lapply(
      unique.tpop.codes, function(tpop.codes){
        mask = sapply(target.population.codes.for.intervention, setequal, tpop.codes)
        interventions.lumped.idu[mask]
    })
    
    
    
    
    #-- Create the Components for the Input --#
    
    # The T-Pop Selector
    tpop.choice.names = lapply(c(list('none'),unique.tpop.codes), function(name){
        target.population.codes.to.pretty.name(name, font.size = TAB1.TITLE.FONT.SIZE)
    })
    tpop.choice.values = c(list('none'), lapply(1:length(unique.tpop.codes), function(i){
        paste0("int", num, "_tpop", i)
    }))
    names(tpop.choice.names) = names(tpop.choice.values) = NULL
    
    #add arrows
    tpop.selector.id = paste0("preset_tpop_",num)
    tpop.selector = radioGroupButtons(tpop.selector.id,
                                      choiceNames=tpop.choice.names,
                                      choiceValues=tpop.choice.values,
                                      # selected=tpop.choice.values[1],
                                      selected=state()[[paste0("preset_tpop_",num)]],
                                      direction = 'vertical',
                                      justified=T,
                                      individual=F,
                                   #   checkIcon = list(yes = the.icon),
                                      size='lg',
                                      status='primary')
    
    tpop.selector = make.radioButtonGroup.wrappable(tpop.selector)

    instruction.panel = tipBox("Second, select the demographic subgroups to which interventions may be applied",
                               left.arrow = T)
    
    # The radio buttons for each group
    conditional.panels = lapply(1:length(tpop.choice.values), function(i){
        tab.id = tpop.choice.values[i]
        
        
        if (tpop.choice.values[i]=='none')
            inner.tabset = instruction.panel
        else
        {
            interventions.for.tpop = interventions.for.unique.tpop.codes[[i-1]]
            lumped.interventions.for.tpop = lumped.idu.interventions.for.unique.tpop.codes[[i-1]]
            
            inner.tabset = make.interventions.by.unit.panel(
              num=num,
              interventions = interventions.for.tpop,
              state=state,
              lumped.interventions = lumped.interventions.for.tpop,
              selector.id = paste0(tab.id))
        }
        
        conditionalPanel(
            style="height:100%",
            condition=paste0("input.", tpop.selector.id, "=='", tab.id,"'"),
            inner.tabset
        )
    })
    
    #-- Make the Panel --#
    nestedWellPanel(fluidRow(
        column(width=TPOP.SELECTOR.WIDTH,
               tpop.selector),
        column(width=12-TPOP.SELECTOR.WIDTH,
               style="height: 100%;",
               conditional.panels)
    ))
}

make.interventions.by.unit.panel <- function(
  num, interventions, state, lumped.interventions, selector.id)
{
    #-- Sort interventions (and lumped interventions) by unit type --#
    unit.types.for.interventions = lapply(interventions, function(int){
        sort(get.intervention.unit.types(int))
    })   
    
    unique.unit.types.for.interventions = unique(unit.types.for.interventions)
    
    interventions.for.unique.unit.types = lapply(unique.unit.types.for.interventions, function(unit.types){
        mask = sapply(unit.types.for.interventions, function(ut){
            setequal(ut, unit.types)
        })
        
        interventions[mask]
    })
    
    lumped.interventions.for.unique.unit.types = lapply(unique.unit.types.for.interventions, function(unit.types){
        mask = sapply(unit.types.for.interventions, function(ut){
            setequal(ut, unit.types)
        })
        
        lumped.interventions[mask]
    })
    
    
    #-- Create the Components for the Input --#
    
    # The tip box
    unit.tip.box = tipBox("Third, select the potential factors on which to intervene",
                          width=12,
                          left.arrow=T, left.arrow.direction = 'up', left.arrow.align = 'top')
                   #  up.arrow = T, up.arrow.align='center')
    
    # The Unit Selector
    unit.choice.values = paste0(selector.id, "_unit", 
                                1:length(unique.unit.types.for.interventions))
    unit.choice.names = lapply(unique.unit.types.for.interventions, function(unit.types){
        unit.types.to.pretty.name(unit.types, font.size = TAB2.TITLE.FONT.SIZE)
    })
    names(unit.choice.values) = names(unit.choice.names) = NULL
    
    unit.selector = radioGroupButtons(selector.id,
                                      choiceNames=unit.choice.names,
                                      choiceValues=unit.choice.values,
                                      # selected=unit.choice.values[1],
                                      selected=state()[[selector.id]],
                                      direction = 'vertical',
                                      justified=T,
                                      individual=F,
                                      size='lg',
                                      status='primary')
    
    unit.selector = make.radioButtonGroup.wrappable(unit.selector)
    
    # The radio buttons for each group
    conditional.panels = lapply(1:length(unique.unit.types.for.interventions), function(i){
        tab.id = unit.choice.values[i]
        
        interventions.for.unit.type = interventions.for.unique.unit.types[[i]]
        lumped.interventions.for.unit.type = lumped.interventions.for.unique.unit.types[[i]]
        
        choice.values = sapply(interventions.for.unit.type, get.intervention.code)
        choice.names = lapply(lumped.interventions.for.unit.type, intervention.brief.description)
        
        choice.names = lapply(choice.names, function(name){div(lump.idu.in.name(name))})
        names(choice.names) = names(choice.values) = NULL
        
        buttons = radioButtons(
          inputId=tab.id,
          label=NULL,
          choiceValues=choice.values,
          choiceNames=choice.names,
          selected=state()[[tab.id]])
        
        conditionalPanel(
            condition=paste0("input.", selector.id, "=='", tab.id,"'"),
            tipBox("Fourth, select an intervention from the list below", 
                   left.arrow=T, left.arrow.direction = 'down', left.arrow.align='bottom'),
            buttons
    )
    })
    
    #-- Make the Panel --#
    nestedWellPanel(level=2,
                    fluidRow(
        style='height: 100%',
        column(width=UNIT.SELECTOR.WIDTH,
               unit.selector,
               unit.tip.box),
        column(width=12-UNIT.SELECTOR.WIDTH,
               conditional.panels)
    ))
    
}

##-------------##
##-- HELPERS --##
##-------------##

intervention.brief.description <- function(int, include.start.text=F)
{
    HTML(get.intervention.description.by.target(int, 
                                                include.start.text=include.start.text,
                                                pre="<table>",
                                                post="</table>",
                                                bullet.pre = "<tr><td style='vertical-align: text-top; word-wrap:break-word;'>&#149;&nbsp;</td><td>",
                                                bullet.post = "</td></tr>",
                                                tpop.delimeter = '',
                                                unit.delimiter = ', ',
                                                pre.header = "<u>",
                                                post.header = ":</u> "
                                                ))
}

unit.types.to.pretty.name <- function(unit.types, font.size='200%')
{
    unit.types = get.pretty.unit.type.names(unit.types)
    if (length(unit.types)==1)
        unit.types = paste0(unit.types, ' only')
    #paste0("", paste0("&#149; ", sapply(tpops, target.population.name), collapse='<BR>'), "")
    HTML(paste0("<table>", paste0("<tr><td style='vertical-align: text-top; font-size: ", font.size, 
                                  "'>&#149;&nbsp;&nbsp;</td>",
                                  "<td style='text-align: left; font-style: italic; font-size: ", font.size, "'>",
                                  unit.types, 
                                  "</td></tr>", collapse=''), 
                "</table>"))
}

target.population.codes.to.pretty.name <- function(tpop.codes, font.size='1em')
{
    if (length(tpop.codes)==1 && tpop.codes=='none')
    {
        bullet = ''
        content = 'None'
    }
    else
    {
        bullet = paste0("<td style='vertical-align: text-top; font-size: ", font.size, 
                       "'>&#149;&nbsp;&nbsp;</td>")
        tpops = lapply(tpop.codes, target.population.from.code)
        content = sapply(tpops, function(tpop){
            lump.idu.in.name(target.population.name(tpop))})
    }
    #paste0("", paste0("&#149; ", sapply(tpops, target.population.name), collapse='<BR>'), "")
    HTML(paste0("<table>",
                paste0("<tr>",
                       bullet,
                       "<td style='text-align: left; font-style: italic; font-size: ", font.size, "'>",
                       content,
                       "</td></tr>", collapse=''), 
           "</table>"))
}

wrap.radioGroupButtons <- function(buttons)
{
    sub = buttons$children[!sapply(buttons$children, is.null)]
}

lump.idu.for.intervention <- function(int)
{
    tpops = get.target.populations.for.intervention(int)
    mapped = lapply(tpops, function(tpop){
        lumped = tpop
        lumped[,,,'IDU_in_remission'] = lumped[,,,'active_IDU']
        if (target.populations.equal(tpop, lumped))
            tpop
        else
        {
            lumped.already.present = any(sapply(int$raw, function(sub){
                any(sapply(sub$target.populations, target.populations.equal, tpop))
            }))
            
            if (lumped.already.present)
                lumped
            else
                tpop
        }
    })
    names(mapped) = sapply(tpops, target.population.hash)
    
    for (i in 1:length(int$raw))
    {
        for (j in 1:length(int$raw[[i]]$target.populations))
            int$raw[[i]]$target.populations[[j]] = 
                mapped[[target.population.hash(int$raw[[i]]$target.populations[[j]]) ]]
    }
    
    int
}

lump.idu.in.name <- function(name)
{
    gsub('active and prior IDU', 'IDU', name, ignore.case = T)
}
