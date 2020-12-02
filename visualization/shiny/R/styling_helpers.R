tipBox <- function(message,
                   width=NULL,
                   text.color='black',
                   bg.color='#FFF3CD',
                   border.color='black',
                   padding=5,
                   up.arrow=F,
                   down.arrow=F,
                   left.arrow=F,
                   right.arrow=F,
                   up.arrow.align=c('left','center','right')[2],
                   down.arrow.align=c('left','center','right')[2],
                   left.arrow.direction=c('left','up','down')[1],
                   right.arrow.direction=c('right','up','down')[1],
                   left.arrow.align=c('top','middle','bottom')[1],
                   right.arrow.align=c('top','middle','bottom')[1]
)
{
    #-- SET UP ARROWS --#
    arrows = c(up='&uarr;', down='&darr;', right='&rarr;', left='&larr;')
    arrow.names = names(arrows)
    arrows = paste0("<b>", arrows, "</b>")
    names(arrows) = arrow.names
    
    # https://www.htmlsymbols.xyz/arrow-symbols
    up = down = right = left = '<td/>'
    if (up.arrow)
        up = paste0("<td style='padding-bottom: 2px; font-size: 150%; text-align: ", 
                    up.arrow.align, "'>",arrows['up'],"</td>")
    if (down.arrow)
        down = paste0("<td style='padding-top: 2px; font-size: 150%; text-align: ", 
                      down.arrow.align, "'>",arrows['down'],"</td>")
    if (right.arrow)
        right = paste0("<td style='padding-left: 5px; font-size: 150%; vertical-align: ",
                       right.arrow.align, "'>",
                       arrows[right.arrow.direction], "</td>")
    if (left.arrow)
        left = paste0("<td style='padding-right: 5px; font-size: 150%; vertical-align: ",
                      left.arrow.align, "'>",
                      arrows[left.arrow.direction], "</td>")
    
    #-- SET UP THE PANEL --#
    box = tags$div(
        background=bg.color, 
        class="yellow-box", 
        style=paste0('color: ', text.color, ';
                     padding: ', padding, 'px;
                     border-radius: 10px; 
                     border-style: dashed;
                     border-width: thin;
                     border-color: ', border.color, ';'),
        { 
            HTML(paste0("<table>",
                        "<tr><td/>",up,"</td></tr>",
                        "<tr>",left,
                        "<td>",message,"</td>",
                        right,"</tr>",
                        "<tr><td/>",down,"<td/></tr>",
                        "</table>"))
        }
    )
    
    
    
    if (is.null(width))
    {
        box = tags$table(
            tags$tr(
                tags$td(box),
                tags$td('')
            )
        )
        
        fluidRow(column(width=12, box))
    }
    else
        fluidRow(
            column(
                width=width, 
                box
            ))
    
}


BOOTSTRAP.BLUE = '#3c8dbc'
prettyActionButton <- function(inputId, label, icon=NULL, width=NULL, ...)
{
    actionButton(inputId = inputId,
                 label = label,
                 icon = icon,
                 width = width,
                 style = paste0("background: ", BOOTSTRAP.BLUE, "; font-size: 150%; color: white; "),
                 ...)
}

WELL.COLORS = c('#F7F7F7',
                '#F0F0F0',
                '#E0E0E0',
                '#D3D3D3',
                '#C4C4C4',
                '#B3B3B3')


nestedWellPanel <- function(...,
                            style=NULL,
                            title=NULL,
                            level=(1:5)[1],
                            title.size='120%',
                            title.decoration=c('bold','italic','normal')[1])
{
    
    bg.color = WELL.COLORS[level]
    border.color = WELL.COLORS[level+1]
    
    
    z = is.null(style)    
    
    style = paste0(paste0("box-shadow: inset 2px 2px 2px ", border.color, ";",
                          "border-style: solid;
               border-color: ", border.color, ";
                border-width: 1px;
               background: ", bg.color, ";
               width: 100%;
               height: 100%;
               padding: 5px;"),
                   style)
    
    if (!z)
        print(style)
    
    panel = tags$div(
        style=style,
        ...
    )
    
    if (!is.null(title))
    {
        title.style = paste0('font-size: ', title.size, ';')
        if (any(title.decoration=='bold'))
            title.style = paste0('font-weight: bold;')
        if (any(title.decoration=='italic'))
            title.style = paste0('font-style: italic;')
            
        tags$table(
            tags$tr(tags$div(title, style=title.style)),
            tags$tr(tags$td(panel))
        )
    }
    else
        panel
}

add.style.to.tag <- function(elem, style,
                             recursive=F)
{
    if (is.null(elem))
        return (NULL)
    
    style = paste0(gsub(';$', '', style), ';')
    if (any(names(elem$attribs)=='style'))
        elem$attribs$style = paste0(style, elem$attribs$style)
    elem$attribs = c(elem$attribs, list(style=style))
    
    if (recursive)
        elem$children = lapply(elem$children, add.style.to.tag, style=style, recursive=T)
    
    elem
}

verticalSpacer <- function(height, unit='px')
{
    div(style=paste0("height: ", height, unit))
}

horizontalSpacer <- function(width, unit='px')
{
    div(style=paste0("width: ", width, unit))
}

make.radioButtonGroup.wrappable <- function(button.group)
{
    button.group$children[[3]]$children[[1]]$children[[1]] = lapply(button.group$children[[3]]$children[[1]]$children[[1]], function(elem){
        elem$children[[1]] = add.style.to.tag(elem$children[[1]], "white-space: normal; word-wrap: break-word;")
    })
    
    button.group
}

tableRow <- function(...,
                     fill.width=T,
                     vertical.align='top',
                     inner.padding='25px')
{
    args = list(...)
    tds = lapply(1:length(args), function(i){
        elem = args[[i]]
        style = paste0("vertical-align: ", vertical.align)
        if (i>1)
            style = paste0(style, "; ",
                           "padding-left: ", inner.padding)
        
        tags$td(elem,
                style=style)
    })
    
    if (fill.width)
        tags$table(
            style='width: 100%',
            do.call(tags$tr, tds)
        )
    else
        tags$table(
            do.call(tags$tr, tds)
        )
}