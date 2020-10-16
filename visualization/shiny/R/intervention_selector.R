
create.intervention.selector.panel <- function(num, input,
                                               lump.idu=T,
                                               title=paste0("Intervention ", num))
{
    # Create a mapping for interventions to target populations and target populations to interventions
    interventions = get.intervention.options(version=get.version(input),
                                             location=get.location(input),
                                             return.intervention.objects = T)
    
    interventions = interventions[!sapply(interventions, is.null.intervention)]
    
    if (length(interventions)==0)
        return(flowLayout(textOutput('No Intervention Options')))
    
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
    
    
    TAB.TITLE.FONT.SIZE = '0.8em'
    tpop.choice.names = lapply(unique.tpop.codes, function(name){
        div(target.population.codes.to.pretty.name(name, font.size = TAB.TITLE.FONT.SIZE))
        })
    
    tab.panels = lapply(1:length(unique.tpop.codes), function(i){
        interventions.for.tpop = interventions.for.unique.tpop.codes[[i]]
        lumped.interventions.for.tpop = lumped.idu.interventions.for.unique.tpop.codes[[i]]
        
        choice.values = sapply(interventions.for.tpop, get.intervention.code)
        choice.names = lapply(lumped.interventions.for.tpop, intervention.brief.description)
        
        choice.names = lapply(choice.names, function(name){div(name)})
        names(choice.names) = names(choice.values) = NULL
      
        verticalTabPanel(tpop.choice.names[[i]],
                         value=i,
                         box_height = NULL,
                 radioButtons(inputId = paste0('intervention_', num, "_", i),
                                        label=NULL,
                                        choiceValues = choice.values,
                                        choiceNames = choice.names))
    })

    instruction.panel = div(HTML("Select a Target Population to the Left"))
    tab.panels = c(list(verticalTabPanel(HTML(paste0("<div style='text-align: left; font-size: ",
                                                     TAB.TITLE.FONT.SIZE,"'>None</div>")),
                                         value=0,
                                         box_height = NULL,
                                         instruction.panel)),
                   tab.panels)
    
    #-- Make the Panel --#
    
    args = c(list(id=paste0("tpop_intervention_",num),
                  selected=0
                  ),
             tab.panels)
    do.call('verticalTabsetPanel', args)
}

get.intervention.selection <- function(num, input)
{
    tpop_intervention_id = paste0("tpop_intervention_",num)
    tpop_selection = input[[tpop_intervention_id]]
    
    if (tpop_selection==0)
        'none'
    else
    {
        intervention.selector.id = paste0('intervention_', num, "_", tpop_selection)
        input[[intervention.selector.id]]
    }
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
                                                pre.header = "<b>",
                                                post.header = ": </b>"
                                                ))
}

target.population.codes.to.pretty.name <- function(tpop.codes, font.size=12)
{
    tpops = lapply(tpop.codes, target.population.from.code)
    #paste0("", paste0("&#149; ", sapply(tpops, target.population.name), collapse='<BR>'), "")
    HTML(paste0("<table>", paste0("<tr><td style='vertical-align: text-top; font-size: ", font.size, 
                                  "'>&#149;&nbsp;</td>",
                            "<td style='text-align: left; font-size: ", font.size, "'>",
                             sapply(tpops, function(tpop){
                                 lump.idu.in.name(target.population.name(tpop))}), 
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