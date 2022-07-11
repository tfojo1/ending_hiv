
union.named.lists <- function(l1, l2)
{
    if (!is.list(l1) || !is.list(l2))
        stop("l1 and l2 must both be lists")
    if (is.null(names(l1)) || is.null(names(l2)))
        stop("l1 and l2 must be named lists")
    
    l.names = union(names(l1), names(l2))
    rv = lapply(l.names, function(elem.name){
        union(l1[[elem.name]], l2[[elem.name]])
    })
    names(rv) = l.names
    
    rv
}

is.named.list.subset <- function(sub, super)
{
    if (!is.list(sub) || !is.list(super))
        stop("sub and super must both be lists")
    if (is.null(names(sub)) || is.null(names(super)))
        stop("sub and super must be named lists")
    
    length(setdiff(names(sub), names(super)))==0 && 
        all(sapply(names(sub), function(elem.name){
            sub.elem = sub[[elem.name]]
            super.elem = super[[elem.name]]
            
            length(setdiff(class(sub.elem), class(super.elem)))==0 && 
                length(setdiff(sub.elem, super.elem))==0
        }))
}

named.lists.equal <- function(dn1, dn2)
{
    if (is.null(dn1) || is.null(dn2))
    {
        if (is.null(dn1) && is.null(dn2))
            T
        else
            F
    }
    else if (!is.list(dn1) || is.null(names(dn1)) || 
             !is.list(dn2) || is.null(names(dn2)))
        stop('dn1 and dn2 must be named lists')
    else
        length(dn1) == length(dn2) &&
        all(names(dn1) == names(dn2)) &&
        all(sapply(1:length(dn1), function(i){
            all(dn1[[i]]==dn2[[i]])
        }))
}
