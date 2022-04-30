

n.times = 2
n.strata = 3

phi = rep(0.5, n.strata)

# indexed[time, stratum]
sigma = t(matrix(1+1:(n.strata*n.times), nrow=n.strata, ncol=n.times))

# indexed[time, stratum
mu = matrix(.8, nrow=n.times, ncol=n.strata)

# sums
z.var = rowSums(sigma^2)
z.mean = rowSums(mu)

# make the big matrix
E = numeric(n.times * (n.strata+1))
names(E) = c(paste0(rep(1:n.times, each=n.strata), '-', rep(1:n.strata, n.times)),
             paste0(1:n.times, '-T'))
V = matrix(0, nrow=n.times*(n.strata+1), ncol=n.times*(n.strata+1))
rownames(V) = names(E)
colnames(V) = names(E)

for (t in 1:n.times)
{
    total.index = n.times * n.strata + t
    E[total.index] = z.mean[t]
    V[total.index,total.index] = z.var[t]
    
    for (s in 1:n.strata)
    {
        stratum.index = (t-1) * n.strata + s
        
        E[stratum.index] = mu[t,s]
        V[stratum.index, stratum.index] = sigma[t,s]^2
        V[total.index, stratum.index] = V[stratum.index, total.index] = sigma[t,s]^2
        
        for (t2 in setdiff(1:n.times, t))
        {
            total.index2 = n.times * n.strata + t2
            V[total.index, total.index2] = sum(phi * sigma[1,] * sigma[2,])
            V[total.index2, stratum.index] = V[stratum.index, total.index2] = phi[s] * sigma[t,s] * sigma[t2,s]
            
            stratum.index2 = (t2-1) * n.strata + s
            V[stratum.index, stratum.index2] = V[stratum.index2, stratum.index] = phi[s] * sigma[t,s] * sigma[t2,s]
        }
    }
    
    
}


#tests
#
# test 1 - same time, conditioned on sum

i1 = 1:2
i2 = n.strata * n.times + 1
V[c(i1,i2),c(i1,i2)]

z=V[i1,i1] - V[i1,i2] %*% solve(V[i2,i2]) %*% V[i2,i1];z
cov2cor(z)

#1,1 should be
sigma[1,1]^2 * (1-sigma[1,1]^2/z.var[1])
#1,2 and 2,1 should be
-sigma[1,1]^2*sigma[1,2]^2/z.var[1]


# test 2, obs 1,1 and total 2 given total 1

i1 = c(1, n.strata*n.times + 2)
i2 = n.strata*n.times + 1
V[c(i1,i2),c(i1,i2)]

z=V[i1,i1] - V[i1,i2] %*% solve(V[i2,i2]) %*% V[i2,i1];z
cov2cor(z)


# test 3 - conditioned on both totals
i1 = 1:(n.strata*n.times)
i2 = n.strata*n.times + 1:n.times
V[c(i1,i2),c(i1,i2)]

z=V[i1,i1] - V[i1,i2] %*% solve(V[i2,i2]) %*% V[i2,i1];z
cov2cor(z)