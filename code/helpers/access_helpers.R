

# subsets the array for the single value 'dim.value' of the dimension 'dim'
single.dim.access <- function(arr,
                              dim,
                              dim.value)
{
    dim.names = dimnames(arr)
    
    if (is.character(dim.value))
        dim.value = (1:length(dim.names[[dim]]))[dim.names[[dim]]==dim.value]
    else if (is.logical(dim.value))
        dim.value = (1:length(dim.names[[dim]]))[dim.value][1]
    
    dim.index = (1:length(dim.names))[names(dim.names)==dim]
    n.dim = length(dim.names[[dim]])
    n.before = prod(sapply(dim.names[1:dim.index], length)) / n.dim
    n = prod(sapply(dim.names, length))
    
    mask = (floor((1:n-1)/n.before) %% n.dim + 1) == dim.value
    
    rv = arr[mask]
    dim.names = dim.names[names(dim.names) != dim]
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}


get.two.dim.access.indices <- function(dim.names,
                                       dim1,
                                       dim.value1,
                                       dim2,
                                       dim.value2)
{
    # get the values as indices
    
    if (is.character(dim.value1))
        dim.value1 = (1:length(dim.names[[dim1]]))[dim.names[[dim1]]==dim.value1]
    else if (is.logical(dim.value1))
        dim.value1 = (1:length(dim.names[[dim1]]))[dim.value1][1]
    
    if (is.character(dim.value2))
        dim.value2 = (1:length(dim.names[[dim2]]))[dim.names[[dim2]]==dim.value2]
    else if (is.logical(dim.value2))
        dim.value2 = (1:length(dim.names[[dim2]]))[dim.value2][1]
    
    # set up pre-calculation stuff
    
    n = prod(sapply(dim.names, length))
    
    dim.index1 = (1:length(dim.names))[names(dim.names)==dim1]
    n.dim1 = length(dim.names[[dim1]])
    n.before1 = prod(sapply(dim.names[1:dim.index1], length)) / n.dim1
    
    dim.index2 = (1:length(dim.names))[names(dim.names)==dim2]
    n.dim2 = length(dim.names[[dim2]])
    n.before2 = prod(sapply(dim.names[1:dim.index2], length)) / n.dim2
    
    # make and return the mask
    (floor((1:n-1)/n.before1) %% n.dim1 + 1) == dim.value1 &
        (floor((1:n-1)/n.before2) %% n.dim2 + 1) == dim.value2
}

# testing speed

if (1==2)
{
    N.TEST = 1000
    
    settings = get.settings.for.version('expanded_1.0')
    arr = array(1:prod(sapply(settings$DIMENSION.NAMES, length)),
                dim=sapply(settings$DIMENSION.NAMES, length), dimnames=settings$DIMENSION.NAMES)
    
    mask = get.two.dim.access.indices(dimnames(arr), dim1='sex', dim.value1='msm', dim2='risk', dim.value2='active_IDU')
    
    all(arr[mask] == access(arr, sex='msm', risk='active_IDU'))
    
    start.time.custom = Sys.time()
    for (i in 1:N.TEST)
    {
        mask = get.two.dim.access.indices(dimnames(arr), dim1='sex', dim.value1='msm', dim2='risk', dim.value2='active_IDU')
        arr[mask] = arr[mask] + 1
        mask = get.two.dim.access.indices(dimnames(arr), dim1='sex', dim.value1='msm', dim2='risk', dim.value2='IDU_in_remission')
        arr[mask] = arr[mask] + 1
        
    }
    end.time.custom = Sys.time()
    
    start.time.access = Sys.time()
    for (i in 1:N.TEST)
    {
        access(arr, sex='msm', risk=c('active_IDU','IDU_in_remission')) = 
            access(arr, sex='msm', risk=c('active_IDU','IDU_in_remission')) + 1
    }
    end.time.access = Sys.time()
    
    print(paste0("Ratio of custom to access = ",
                 round(as.numeric(end.time.custom-start.time.custom) /
                           as.numeric(end.time.access-start.time.access), 3)))
}
