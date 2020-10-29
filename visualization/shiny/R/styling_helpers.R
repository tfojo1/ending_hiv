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
                   up.arrow.align=c('left','center','right')[1],
                   down.arrow.align=c('left','center','right')[1],
                   left.arrow.direction=c('left','up','down')[1],
                   right.arrow.direction=c('right','up','down')[1],
                   left.arrow.align=c('top','middle','bottom')[1],
                   right.arrow.align=c('top','middle','bottom')[1]
)
{
    #-- SET UP ARROWS --#
    # https://www.htmlsymbols.xyz/arrow-symbols
    if (left.arrow.direction=='up')
        l.code = '&#129093'
    else if (left.arrow.direction=='down')
        l.code = '&#129095'
    else
        l.code = '&#129092'
    
    if (right.arrow.direction=='up')
        r.code = '&#129093'
    else if (right.arrow.direction=='down')
        r.code = '&#129095'
    else
        r.code = '&#129094'
    
    up = down = right = left = '<td/>'
    if (up.arrow)
        up = paste0("<td style='padding-bottom: 2px; font-size: 150%; text-align: ", 
                    up.arrow.align, "'>&#129093</td>")
    if (down.arrow)
        down = paste0("<td style='padding-top: 2px; font-size: 150%; text-align: ", 
                      down.arrow.align, "'>&#129095</td>")
    if (right.arrow)
        right = paste0("<td style='padding-left: 5px; font-size: 150%; vertical-align: ",
                       right.arrow.align, "'>",
                       r.code, "</td>")
    if (left.arrow)
        left = paste0("<td style='padding-right: 5px; font-size: 150%; vertical-align: ",
                      left.arrow.align, "'>",
                      l.code, "</td>")
    
    #-- SET UP THE PANEL --#
    box = tags$div(
        background=bg.color, 
        class="yellow-box", 
        style=paste0('color: ', text.color, ';
                     padding: ', padding, 'px;
                     border-radius: 10px; 
                     border-style: dotted;
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

nestedWellPanel <- function(...,
                            style=NULL,
                            level=(1:2)[1])
{
    WELL.COLORS = c('#F7F7F7',
                    '#F0F0F0',
                    '#E0E0E0',
                    '#D3D3D3',
                    '#C4C4C4',
                    '#B3B3B3')
    
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
    
    tags$div(
        style=style,
        ...
    )
}


add.style.to.top.level.tag <- function(elem, style)
{
    elem$attribs = c(elem$attribs, list(style=style))
    elem
}

verticalSpacer <- function(height, unit='px')
{
    div(style=paste0("height: ", height, unit))
}