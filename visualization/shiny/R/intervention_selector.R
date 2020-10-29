
NO.CUSTOM.INTERVENTIONS.MESSAGE = 'No custom interventions have been run. Go to the "Custom Interventions" panel on the left to design and run custom interventions'

TAB1.TITLE.COLOR = '#3c8dbc' # '#737CA1' # '#98AFC7'
TAB1.TITLE.FONT.SIZE = '0.85em'

TAB2.TITLE.COLOR = '#3c8dbc' # '#737CA1' # '#98AFC7'
TAB2.TITLE.FONT.SIZE = '0.85em'

create.intervention.selector.panel <- function(num, input,
                                               lump.idu=T,
                                               title=paste0("Intervention ", num))
{
    #-- Set up the Pre-Run Panel -_#
    
    interventions = get.intervention.options(version=get.version(input),
                                             location=get.location(input),
                                             return.intervention.objects = T)
    
    interventions = interventions[!sapply(interventions, is.null.intervention)]
    
    if (length(interventions)==0)
        return(flowLayout(textOutput('No Intervention Options')))
    
    
    prerun.panel = make.interventions.by.tpop.panel(num=num,
                                                    interventions=interventions,
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
               tipBox('First, choose from either Pre-Specified or Custom Interventions you have defined',
                      left.arrow = T, left.arrow.align='middle', left.arrow.direction='down'),
               tabsetPanel(id=paste0("intervention_",num,"_selector"),
                           selected='prerun',
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
        tpop.selector.id = paste0("preset_tpop_",num,"_selector")
        tpop.selection = input[[tpop.selector.id]]
        
        if (tpop.selection=='none')
            'none'
        else
        {
            unit.selector.id = tpop.selection
            unit.selection = input[[unit.selector.id]]
            
            intervention.selector.id = paste0(unit.selection, '_selector')
            input[[intervention.selector.id]]
        }
    }
}

make.interventions.by.tpop.panel <- function(num,
                                            interventions,
                                            lump.idu=T)
{
    #-- Create a mapping for interventions to target populations and target populations to interventions --#
    
    if (lump.idu)
        interventions.lumped.idu = lapply(interventions, lump.idu.for.intervention)
    else
        interventions.lumped.idu = interventions
    
    target.population.codes.for.intervention = lapply(interventions.lumped.idu, function(int){
        sapply(get.target.populations.for.intervention(int), target.population.to.code)
    })
    
    
    unique.tpop.codes = unique(target.population.codes.for.intervention)
    
    interventions.for.unique.tpop.codes = lapply(unique.tpop.codes, function(tpop.codes){
        mask = sapply(target.population.codes.for.intervention, setequal, tpop.codes)
        interventions[mask]
    })
    
    lumped.idu.interventions.for.unique.tpop.codes = lapply(unique.tpop.codes, function(tpop.codes){
        mask = sapply(target.population.codes.for.intervention, setequal, tpop.codes)
        interventions.lumped.idu[mask]
    })
    
    
    
    
    #-- Create the Components for the Input --#
    
    # The T-Pop Selector
    tpop.choice.names = lapply(c(list('none'),unique.tpop.codes), function(name){
        div(target.population.codes.to.pretty.name(name, font.size = TAB1.TITLE.FONT.SIZE))
    })
    tpop.choice.values = c(list('none'), lapply(1:length(unique.tpop.codes), function(i){
        paste0("int",num, "_tpop",i)
    }))
    names(tpop.choice.names) = names(tpop.choice.values) = NULL
    
    #add arrows
    tpop.selector.id = paste0("preset_tpop_",num,"_selector")
    
    if (1==2)
    {
        #204e73
    tpop.choice.names = lapply(1:length(tpop.choice.names), function(i){
        one.name = tpop.choice.names[[i]]
        tab.id = tpop.choice.values[i]
        tags$table(
            style='border-style: solid; border-color: #FF0000; width: 100%',
            tags$tr(
                tags$td(one.name),
                tags$td(
                    style='text-align: right; width=1px; border-style: solid; border-color: #FF0000;',
                    conditionalPanel(
                        condition=paste0("input.", tpop.selector.id, "=='", tab.id,"'"),
                        HTML("<div style='color: #FF0000; font-size: 300%;
                       #      padding-top: -100px;
                        #     padding-bottom: -100px;
                             line-height: 33%;
                             margin-right: -30px;'>&#129078;</div>")
                        )
                    )
                )
            )
    })
    }
    
    the.icon = HTML(paste0("<div style='vertical-align: middle; color='#FF00000'>",
                           "&#129078;",
                           "</div>"))

    tpop.selector.id = paste0("preset_tpop_",num,"_selector")
    tpop.selector = radioGroupButtons(tpop.selector.id,
                                      choiceNames=tpop.choice.names,
                                      choiceValues=tpop.choice.values,
                                      selected=tpop.choice.values[1],
                                      direction = 'vertical',
                                      justified=T,
                                      individual=F,
                                   #   checkIcon = list(yes = the.icon),
                                      size='lg',
                                      status='primary')
    
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
            
            inner.tabset = make.interventions.by.unit.panel(num=num,
                                                            interventions = interventions.for.tpop,
                                                            lumped.interventions = lumped.interventions.for.tpop,
                                                            id.prefix = paste0(tab.id, "_"))
        }
        
        conditionalPanel(
            style="height:100%",
            condition=paste0("input.", tpop.selector.id, "=='", tab.id,"'"),
            inner.tabset
        )
    })
    
    #-- Make the Panel --#
    nestedWellPanel(fluidRow(
        column(width=3,
               tpop.selector),
        column(width=9,
               style="height: 100%;",
               conditional.panels)
    ))
}

make.interventions.by.unit.panel <- function(num,
                                            interventions,
                                            lumped.interventions,
                                            id.prefix)
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
    unit.choice.values = paste0(id.prefix, "unit", 1:length(unique.unit.types.for.interventions))
    unit.choice.names = lapply(unique.unit.types.for.interventions, function(unit.types){
        unit.types.choice.names = div(unit.types.to.pretty.name(unit.types, font.size = TAB2.TITLE.FONT.SIZE))
    })
    names(unit.choice.values) = names(unit.choice.names) = NULL
    
    unit.selector.id = paste0(id.prefix, 'selector')
    unit.selector = radioGroupButtons(unit.selector.id,
                                      choiceNames=unit.choice.names,
                                      choiceValues=unit.choice.values,
                                      selected=unit.choice.values[1],
                                      direction = 'vertical',
                                      justified=T,
                                      individual=F,
                                      size='lg',
                                      status='primary')
    
    # The radio buttons for each group
    conditional.panels = lapply(1:length(unique.unit.types.for.interventions), function(i){
        tab.id = unit.choice.values[i]
        
        interventions.for.unit.type = interventions.for.unique.unit.types[[i]]
        lumped.interventions.for.unit.type = lumped.interventions.for.unique.unit.types[[i]]
        
        choice.values = sapply(interventions.for.unit.type, get.intervention.code)
        choice.names = lapply(lumped.interventions.for.unit.type, intervention.brief.description)
        
        choice.names = lapply(choice.names, function(name){div(lump.idu.in.name(name))})
        names(choice.names) = names(choice.values) = NULL
        
        buttons = radioButtons(inputId = paste0(tab.id, "_selector"),
                     label=NULL,
                     choiceValues = choice.values,
                     choiceNames = choice.names)
        
        conditionalPanel(
            condition=paste0("input.", unit.selector.id, "=='", tab.id,"'"),
            tipBox("Fourth, select an intervention from the list below", 
                   left.arrow=T, left.arrow.direction = 'down', left.arrow.align='bottom'),
            buttons
    )
    })
    
    #-- Make the Panel --#
    nestedWellPanel(level=2,
                    fluidRow(
        style='height: 100%',
        column(width=3,
               unit.selector,
               unit.tip.box),
        column(width=9,
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
                                                bullet.pre = "<tr><td style='vertical-align: text-top;'>&#149;&nbsp;</td><td>",
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