
n.times = 3
n.strata = 3

mu = matrix(0.8, nrow=n.times, ncol=n.strata)
lambda = t(matrix(100 + 1:(n.times*n.strata), ncol=n.times, nrow=n.strata))

phi = .8
theta = .8

sigma = matrix(.02, nrow=n.times, ncol=n.strata)
tau = .05 * lambda

tau = matrix(.05 * mean(lambda), nrow=n.times, ncol=n.strata) 

var.names = c(
    paste0('t', rep(1:n.times, each=n.strata), '-l', rep(1:n.strata, n.times)),
    paste0('tot-', 1:n.times)
)

mean.arr = numeric(length(var.names))
names(mean.arr) = var.names
cov.mat = matrix(0, nrow=length(var.names), ncol=length(var.names))
rownames(cov.mat) = var.names
colnames(cov.mat) = var.names

for (t in 1:n.times)
{
    total.index1 = n.times*n.strata + t
    indices1 = (t-1) * n.times + 1:n.strata
    for (i in 1:n.strata)
    {
        # the means
        index1 = (t-1) * n.times + i
        
        mean.arr[index1] = mu[t,i] * lambda[t,i]
        
        # the covariances
        cov.mat[index1,index1] = lambda[t,i] * mu[t,i] * (1-mu[t,i]) +
            sigma[t,i]^2 * lambda[t,i] * (1-lambda[t,i]) +
            tau[t,i]^2 * mu[t,i]^2 +
            sigma[t,i]^2 * tau[t,i]^2
        
        cov.mat[total.index1,index1] = cov.mat[index1,total.index1] =
            cov.mat[index1,index1]
        
        
        if (t < n.times)
        {
            for (s in (t+1):n.times)
            {
                total.index2 = n.times*n.strata + s
                indices2 = (s-1) * n.times + 1:n.strata
                
                index2 = (s-1) * n.times + i
                
                cov.mat[index1,index2] = cov.mat[index2,index1] = 
                    mu[t,i] * mu[s,i] * theta * tau[t,i] * tau[s,i] +
                    lambda[t,i] * lambda[s, i] * phi * sigma[t,i] * sigma[s,i] +
                    phi * sigma[t,i] * sigma[s,i] * theta * tau[t,i] * tau[s,i]
                
                
                cov.mat[total.index2,index1] = cov.mat[index1,total.index2] =
                    cov.mat[total.index1,index2] = cov.mat[index2,total.index1] =
                    cov.mat[index1,index2]
                
                cov.mat[total.index1, total.index2] = cov.mat[total.index2, total.index1] = 
                    sum(diag(cov.mat[indices1,indices2]))
            }
        }
        
    }
    
    mean.arr[total.index1] = sum(mean.arr[indices1])
    cov.mat[total.index1,total.index1] = sum(diag(cov.mat[indices1,indices1]))
}
        

is.symmetric.matrix(cov.mat)  
is.positive.semi.definite(cov.mat)   
is.positive.definite(cov.mat)   

time.order = unlist(sapply(1:n.times, function(t){
    c((t-1)*n.times + 1:n.strata,
      n.times*n.strata + t)
}))

cov.mat[time.order,time.order]

loc.order = c(unlist(sapply(1:n.strata, function(i){
        (1:n.times-1)*n.times + i
    })),
    n.times*n.strata + 1:n.times
)

cov.mat[loc.order,loc.order]
cov2cor(cov.mat[loc.order,loc.order])




indices1 = n.times * n.strata + 1:n.times
indices2 = setdiff(1:length(var.names), indices1)
cond.cov.mat = cov.mat[indices2,indices2] -
    cov.mat[indices2,indices1] %*% solve(cov.mat[indices1,indices1]) %*% cov.mat[indices1,indices2]

round(cov2cor(cond.cov.mat),2)
round(sqrt(diag(cond.cov.mat)),1)
round(sqrt(diag(cov.mat))[indices2],1)

round(sqrt(diag(cond.cov.mat)),1) /
round(sqrt(diag(cov.mat))[indices2],1)
