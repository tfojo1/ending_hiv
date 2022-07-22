

#-- ACCESS HELPERS --#
# A collection of functions to subset arrays without having to know the specific dimensions of the array
# These are in general optimized to a specific use case

# subsets the array for the single value 'dim.value' of the dimension 'dim'
single.dim.access <- function(arr,
                              dim,
                              dim.value,
                              allow.unoptimized = T)
{
    dim.names = dimnames(arr)
    
    dim.index = (1:length(dim.names))[names(dim.names)==dim]
    num.dims = length(dim.names)
    
    if (num.dims==8)
    {
        arr = switch(dim.index,
                     arr[dim.value,,,,,,,], #1
                     arr[,dim.value,,,,,,], #2
                     arr[,,dim.value,,,,,], #3
                     arr[,,,dim.value,,,,], #4
                     arr[,,,,dim.value,,,], #5
                     arr[,,,,,dim.value,,], #6
                     arr[,,,,,,dim.value,], #7
                     arr[,,,,,,,dim.value] #8
        )
    }
    else if (num.dims==6)
    {
        arr = switch(dim.index,
                     arr[dim.value,,,,,], #1
                     arr[,dim.value,,,,], #2
                     arr[,,dim.value,,,], #3
                     arr[,,,dim.value,,], #4
                     arr[,,,,dim.value,], #5
                     arr[,,,,,dim.value] #6
        )
    }
    else if (num.dims==5)
    {
        arr = switch(dim.index,
                     arr[dim.value,,,,], #1
                     arr[,dim.value,,,], #2
                     arr[,,dim.value,,], #3
                     arr[,,,dim.value,], #4
                     arr[,,,,dim.value] #5
        )
    }
    else if (allow.unoptimized)
    {
        if (is.character(dim.value))
            dim.value = (1:length(dim.names[[dim]]))[dim.names[[dim]]==dim.value]
        else if (is.logical(dim.value))
            dim.value = (1:length(dim.names[[dim]]))[dim.value][1]
        
        n.dim = length(dim.names[[dim]])
        
        n.before = prod(sapply(dim.names[1:dim.index], length)) / n.dim
        n = prod(sapply(dim.names, length))
        
        mask = (floor((1:n-1)/n.before) %% n.dim + 1) == dim.value
        
        arr = arr[mask]
    }
    else
    {
        stop("No optimized implementation exists for the given array configuration ")
    }
    
    dim.names = dim.names[names(dim.names) != dim]
    dim(arr) = sapply(dim.names, length)
    dimnames(arr) = dim.names
    
    arr
}

single.dim.multi.value.access <- function(arr,
                                          dim,
                                          dim.values)
{
    dim.names = dimnames(arr)
    
    dim.index = (1:length(dim.names))[names(dim.names)==dim]
    n.dim = length(dim.names[[dim]])
    
    if (n.dim==8)
    {
        arr = switch(dim.index,
                     arr[dim.values,,,,,,,], #1
                     arr[,dim.values,,,,,,], #2
                     arr[,,dim.values,,,,,], #3
                     arr[,,,dim.values,,,,], #4
                     arr[,,,,dim.values,,,], #5
                     arr[,,,,,dim.values,,], #6
                     arr[,,,,,,dim.values,], #7
                     arr[,,,,,,,dim.values] #8
        )
    }
    else if (n.dim==6)
    {
        arr = switch(dim.index,
                     arr[dim.values,,,,,], #1
                     arr[,dim.values,,,,], #2
                     arr[,,dim.values,,,], #3
                     arr[,,,dim.values,,], #4
                     arr[,,,,dim.values,], #5
                     arr[,,,,,dim.values] #6
        )
    }
    else if (n.dim==5)
    {
        arr = switch(dim.index,
                     arr[dim.values,,,,], #1
                     arr[,dim.values,,,], #2
                     arr[,,dim.values,,], #3
                     arr[,,,dim.values,], #4
                     arr[,,,,dim.values] #5
        )
    }
    else if (length(dim.values)==1)
        return(single.dim.access(arr=arr, dim=dim, dim.value=dim.values, allow.unoptimized = allow.unoptimized))
    else if (allow.unoptimized)
    {
        if (is.character(dim.values))
            dim.values = sapply(dim.values, function(dim.value){
                (1:length(dim.names[[dim]]))[dim.names[[dim]]==dim.value]})
        else if (is.logical(dim.value))
            dim.value = (1:length(dim.names[[dim]]))[dim.value]
        
        n.before = prod(sapply(dim.names[1:dim.index], length)) / n.dim
        n = prod(sapply(dim.names, length))
        
        matrix.mask = sapply(dim.values, function(dim.value){
            (floor((1:n-1)/n.before) %% n.dim + 1) == dim.value
        })
        mask = apply(matrix.mask, 1, any)
        
        arr = arr[mask]
    }
    
    dim.names[[dim]] = dim.values
    dim(arr) = sapply(dim.names, length)
    dimnames(arr) = dim.names
    
    arr
}

access.two.dim <- function(arr,
                           dim1,
                           dim.value1,
                           dim2,
                           dim.value2)
{
    dim.names = dimnames(arr)
    
    dim.index1 = (1:length(dim.names))[names(dim.names)==dim1]
    dim.index2 = (1:length(dim.names))[names(dim.names)==dim2]
    
    
    rv = arr[get.two.dim.access.indices(dim.names, 
                                        dim1=dim1, dim.value1=dim.value1,
                                        dim2=dim2, dim.value2=dim.value2)]
    
    
    dim.names = dim.names[-c(dim.index1,dim.index2)]
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

access.single.transition <- function(arr,
                                     dimension,
                                     from.value,
                                     to.value,
                                     allow.unoptimized=F)
{
    n.dim = length(dim(arr))
    from.dim = (1:n.dim)[names(dim(arr))==paste0(dimension, '.from')]
    
    dim.names = dimnames(arr)

    if (n.dim==9)
    {
        arr = switch(from.dim,
               arr[from.value,,,,,,,,to.value], #1
               arr[,from.value,,,,,,,to.value], #2
               arr[,,from.value,,,,,,to.value], #3
               arr[,,,from.value,,,,,to.value], #4
               arr[,,,,from.value,,,,to.value], #5
               arr[,,,,,from.value,,,to.value], #6
               arr[,,,,,,from.value,,to.value], #7
               arr[,,,,,,,from.value,to.value] #8
               )
    }
    else if (n.dim==7)
    {
        arr = switch(from.dim,
               arr[from.value,,,,,,to.value], #1
               arr[,from.value,,,,,to.value], #2
               arr[,,from.value,,,,to.value], #3
               arr[,,,from.value,,,to.value], #4
               arr[,,,,from.value,,to.value], #5
               arr[,,,,from.value,,to.value] #6
        )
    }
    else if (n.dim==6)
    {
        arr = switch(from.dim,
               arr[from.value,,,,,to.value], #1
               arr[,from.value,,,,to.value], #2
               arr[,,from.value,,,to.value], #3
               arr[,,,from.value,,to.value], #4
               arr[,,,,from.value,to.value] #5
        )
    }
    else if (allow.unoptimized)
        return(access.two.dim(arr,
                       dim1 = paste0(dimension, '.from'),
                       dim.value1 = from.value,
                       dim2 = n.dim,
                       dim.value2 = to.value))
    else
        stop("No optimized implementation exists for the given transition array configuration")
    
    dim.names = dim.names[-c(from.dim, n.dim)]
    dim(arr) = sapply(dim.names, length)
    dimnames(arr) = dim.names
    
    arr
}

pairwise.access.transitions <- function(arr,
                                        dimension,
                                        from.values,
                                        to.values,
                                        allow.unoptimized=T,
                                        group.by=c('from','to')[1])
{
    dim.names = dimnames(arr)
    
    if (length(from.values)==1)
    {
        arr = access.single.transition(arr,
                                       dimension=dimension,
                                       from.value=from.values,
                                       to.value=to.values,
                                       allow.unoptimized = allow.unoptimized)
        
        # Add back in the dimension that was marginalized out (of length 1)
        dim.names = dim.names[-length(dim.names)]
        names(dim.names)[names(dim.names)==paste0(dimension, '.from')] = dimension
        if (group.by=='to')
            dim.names[[dimension]] = to.values
        else
            dim.names[[dimension]] = from.values
        
        dim(arr) = sapply(dim.names, length)
        dimnames(arr) = dim.names
        
        arr
    }
    else
    {
        # The order we are going to generate from our sapply statement
        generated.dim.names = dim.names[setdiff(names(dim.names), 
                                                paste0(dimension, '.', c('from','to')))]
        
        # Set up the to/from values
        if (group.by=='to')
        {
            sorted.to.values = intersect(dim.names[[paste0(dimension, '.to')]],
                                         to.values)
            generated.dim.names[[dimension]] = sorted.to.values
            
            sorted.from.values = lapply(sorted.to.values, function(val){
                from.values[to.values==val]
            })
            sorted.to.values = lapply(1:length(sorted.to.values), function(i){
                rep(sorted.to.values[i], length(sorted.from.values[[i]]))
            })
        }
        else
        {
            sorted.from.values = intersect(dim.names[[paste0(dimension, '.from')]],
                                           from.values)
            generated.dim.names[[dimension]] = sorted.from.values
            
            sorted.to.values = lapply(sorted.from.values, function(val){
                to.values[from.values==val]
            })
            sorted.from.values = lapply(1:length(sorted.from.values), function(i){
                rep(sorted.from.values[i], length(sorted.to.values[[i]]))
            })
        }
              
        # The order we want the dimensions in
        desired.dimension.order = names(dim.names)[-length(dim.names)]
        desired.dimension.order[desired.dimension.order==paste0(dimension, '.from')] = dimension
        
        # Execute the sapply
        arr = sapply(1:length(sorted.from.values), function(i){
            if (length(sorted.from.values[[i]])==1)
                access.single.transition(arr,
                                         dimension=dimension,
                                         from.value=sorted.from.values[[i]],
                                         to.value=sorted.to.values[[i]],
                                         allow.unoptimized = allow.unoptimized)
            else
                rowSums(sapply(1:length(sorted.from.values), function(j){
                    access.single.transition(arr,
                                             dimension=dimension,
                                             from.value=sorted.from.values[[i]][j],
                                             to.value=sorted.to.values[[i]][j],
                                             allow.unoptimized = allow.unoptimized)
                }))
        })
        
        # Hydrate up the dimensions
        dim(arr) = sapply(generated.dim.names, length)
        dimnames(arr) = generated.dim.names
        
        # Reorder the dimensions
        arr = apply(arr, desired.dimension.order, function(x){x})
        
        arr
    }
}


get.two.dim.access.indices <- function(dim.names,
                                       dim1,
                                       dim.value1,
                                       dim2,
                                       dim.value2)
{
    # get the values as indices
    
    if (is.character(dim1))
        dim1 = (1:length(dim.names))[names(dim.names)==dim1]
    
    if (is.character(dim2))
        dim2 = (1:length(dim.names))[names(dim.names)==dim2]
    
    if (is.character(dim.value1))
        dim.value1 = (1:length(dim.names[[dim1]]))[dim.names[[dim1]]==dim.value1]
    else if (is.logical(dim.value1))
        dim.value1 = (1:length(dim.names[[dim1]]))[dim.value1][1]
    
    if (is.character(dim.value2))
        dim.value2 = (1:length(dim.names[[dim2]]))[dim.names[[dim2]]==dim.value2]
    else if (is.logical(dim.value2))
        dim.value2 = (1:length(dim.names[[dim2]]))[dim.value2][1]
    
    1 + do_get_two_dim_access_indices(dims = as.integer(sapply(dim.names, length)),
                                  dim1 = as.integer(dim1),
                                  dim1_value = as.integer(dim.value1),
                                  dim2 = as.integer(dim2),
                                  dim2_value = as.integer(dim.value2))
}

OLD.get.two.dim.access.indices <- function(dim.names,
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
    
    
    
    start.time.direct = Sys.time()
    for (i in 1:N.TEST)
    {
        arr[,,,'msm',c('active_IDU','IDU_in_remission'),,,] =arr[,,,'msm',c('active_IDU','IDU_in_remission'),,,] + 1
    }
    end.time.direct = Sys.time()
    
    fn.acc = function(arr, v1, v2){
        arr[,,,v1,v2,,,] = arr[,,,v1,v2,,,] + 1
        arr
    }
    
    start.time.direct.wrapped = Sys.time()
    for (i in 1:N.TEST)
    {
        arr = fn.acc(arr, v1, v2)
    }
    end.time.direct.wrapped = Sys.time()
    
    
    print(paste0("Ratio of custom to access = ",
                 round(as.numeric(end.time.custom-start.time.custom) /
                           as.numeric(end.time.access-start.time.access), 3)))
    
    print(paste0("Ratio of custom to direct = ",
                 round(as.numeric(end.time.custom-start.time.custom) /
                           as.numeric(end.time.direct-start.time.direct), 3)))
    
    print(paste0("Ratio of wrapped.direct to direct = ",
                 round(as.numeric(end.time.direct.wrapped-start.time.direct.wrapped) /
                           as.numeric(end.time.direct-start.time.direct), 3)))
}
