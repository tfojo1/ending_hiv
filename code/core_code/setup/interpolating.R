interpolate.parameters <- function(values, 
                                   value.times,
                                   desired.times,
                                   return.list=T)
{
    fn = function(time){
        
        n.times = length(value.times)
        if (n.times==1)
            return (values[[1]])
        
        index.before = (1:n.times)[value.times <= time]
        index.before = index.before[length(index.before)]
        index.after = (1:n.times)[value.times > time][1]
        
        if (length(index.before)==0)
            values[[index.after]]
        else if (is.na(index.after) || is.infinite(value.times[index.before]))
            values[[index.before]]
        else if (is.infinite(value.times[index.after]))
            values[[index.after]]
        else
            (values[[index.before]] * (value.times[index.after] - time) +
                 values[[index.after]] * (time - value.times[index.before])) /
            (value.times[index.after] - value.times[index.before])
    }
    
    if (return.list)
        lapply(desired.times, fn)
    else
        sapply(desired.times, fn)
}
