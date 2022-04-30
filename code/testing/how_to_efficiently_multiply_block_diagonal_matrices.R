
# Multiply something with a block-diagonal matrix

M1 = matrix(1:6, nrow=2)
M2 = matrix(10+1:6, nrow=2)
M3 = matrix(100+1:6, nrow=2)

T1 = matrix(1:9, nrow=3)
T2 = matrix(10+1:9, nrow=3)
T3 = matrix(100+1:9, nrow=3)

M = cbind(M1, M2, M3)

T = matrix(0, nrow=9, ncol=9)
T[1:3,1:3] = T1
T[4:6,4:6] = T2
T[7:9,7:9] = T3

R = M%*%T

cbind(M1%*%T1, M2%*%T2, M3%*%T3)


Tc = cbind(T1,T2,T3)
dim(Tc) = c(3,3,3)

Mc = cbind(M1,M2,M3)
dim(Mc) = c(2,3,3)

Rc = sapply(1:3, function(i){
    Mc[,,i] %*% Tc[,,i]
})
dim(Rc) = c(2,9)


#efficiently multiply a diagonal matrix

n = 4
m = 3

M = matrix(1:(n*m), nrow=n, ncol=m)
T = diag(10*(1:m))

M %*% T

sapply(1:m, function(i){
    diag(T)[i] * M[,i]
})

# prune zero row

B = matrix(c(1,0,1,0,0,0,1,1,0), nrow=3)
S = matrix(2:10, nrow=3)

B%*%S
mask = apply(B!=0, 2, any)
B[,mask] %*% S[mask,]



##-- SPEED TESTS --##

# blockwise matrix

# to see what is faster

print("STARTING BLOCK MATRIX MULTIPLICATION TEST...")

n = 3*10 # *10 years
m = 10*10
d = 135

M.list = lapply(1:d, function(i){
    matrix(round(runif(n*m,0,200)), nrow=n)
})

T.list = lapply(1:d, function(i){
    matrix(round(runif(m*m,0,200)), nrow=m)
})

M.arr = array(unlist(M.list), dim=c(n,m,d))
T.arr = array(unlist(T.list), dim=c(m,m,d))


M.full = matrix(unlist(M.list), nrow=n)
T.full = matrix(0, nrow=m*d, ncol=m*d)
for (i in 1:d)
    T.full[(i-1)*m+1:m,(i-1)*m+1:m] = T.list[[i]]

N.ITER = 200

start.list = Sys.time()
z = sapply(1:N.ITER, function(j){
    R = sapply(1:d, function(j){
        M.list[[j]] %*% T.list[[j]]
    })
    dim(R) = c(n,m*d)
})
end.list = Sys.time()

start.list.list = Sys.time()
z = sapply(1:N.ITER, function(j){
    R = lapply(1:d, function(j){
        M.list[[j]] %*% T.list[[j]]
    })
    1
})
end.list.list = Sys.time()


start.arr = Sys.time()
z = sapply(1:N.ITER, function(i){
    R = sapply(1:d, function(j){
        M.arr[,,j] %*% T.arr[,,j]
    })
    dim(R) = c(n,m*d)
})
end.arr = Sys.time()

start.full = Sys.time()
z = sapply(1:N.ITER, function(j){
    R = M.full %*% T.full
    1
})
end.full= Sys.time()


print("list / array for block matrix mutiplication:")
print(as.numeric(end.list-start.list) / as.numeric(end.arr-start.arr))
print("list / full for block matrix mutiplication:")
print(as.numeric(end.list-start.list) / as.numeric(end.full-start.full))
print("array / full for block matrix mutiplication:")
print(as.numeric(end.arr-start.arr) / as.numeric(end.full-start.full))


print("list->list / list->array for block matrix mutiplication:")
print(as.numeric(end.list.list-start.list.list) / as.numeric(end.list-start.list))
print("list->list / array->array for block matrix mutiplication:")
print(as.numeric(end.list.list-start.list.list) / as.numeric(end.arr-start.arr))

print("secs per full:")
print(as.numeric(end.full-start.full) / N.ITER)

print('full multiplications per second:')
print(N.ITER / as.numeric(end.full-start.full))


print("DONE BLOCK MATRIX MULTIPLICATION TEST")

# For masking zero rows

print("STARTING MASKED MATRIX MULTIPLICATION TEST...")

n = 10
d = 135*10*10
n.per = ceiling(.25*135)

B = matrix(0, nrow=n, ncol=d)
non.zero.indices = sample(1:d, n.per, replace = F)
B[,non.zero.indices] = round(runif(n*n.per, 0, 200))

mask = apply(B!=0,2,any)

S = matrix(round(runif(d*d, 0, 200)), nrow=d, ncol=d)

N.ITER = 200
start.regular = Sys.time()
z = sapply(1:N.ITER, function(i){
    B %*% S
    1
})
end.regular = Sys.time()


start.masked = Sys.time()
z = sapply(1:N.ITER, function(i){
    B[,mask] %*% S[mask,]
    1
})
end.masked = Sys.time()


print("masked vs full matrix multiplication:")
print(as.numeric(end.masked-start.masked) / as.numeric(end.regular-start.regular))

print("DONE MASKED MATRIX MULTIPLICATION TEST")



# More real world for conditioning
# (we are not actually concerned with the numbers being right here, just the number of matrix mults)

print("STARTING CONDITIONING TEST...")

N.ITER = 100

n.strata = 135
n.obs.loc = 3
n.metalocation = 10
n.years = 10

Sigma.list = lapply(1:n.strata, function(d){
    sds = round(runif(n.metalocation*n.years, 0, 200))
    cor.mat = matrix(0.5,
           nrow = n.metalocation*n.years, ncol = n.metalocation*n.years)
    diag(cor.mat) = 1
    sds %*% t(sds) * cor.mat
})

Mat.list = lapply(1:n.strata, function(d){
    matrix(round(runif(n.obs.loc*n.years^2*n.metalocation,0,200)),
           nrow=n.obs.loc*n.years, ncol=n.metalocation*n.years)
})

Sigma.full = matrix(0, nrow=n.strata*n.metalocation*n.years,
                    ncol=n.strata*n.metalocation*n.years)
Mat.full = matrix(0, nrow=n.strata*n.obs.loc*n.years,
                  ncol=n.strata*n.metalocation*n.years)
for (d in 1:n.strata)
{
    Sigma.full[(d-1)*n.metalocation*n.years + 1:(n.metalocation*n.years),
               (d-1)*n.metalocation*n.years + 1:(n.metalocation*n.years)] = Sigma.list[[d]]
    
    Mat.full[(d-1)*n.obs.loc*n.years + 1:(n.obs.loc*n.years),
             (d-1)*n.metalocation*n.years + 1:(n.metalocation*n.years)] = Mat.list[[d]]
}

t.Mat.full = t(Mat.full)
t.Mat.list = lapply(Mat.list, function(Mat){t(Mat)})

start.cfull = Sys.time()
z.full = sapply(1:N.ITER, function(i){
    Mat.Sigma = Mat.full %*% Sigma.full
    cond.Sigma = Sigma.full - 
        Sigma.full %*% t.Mat.full %*% 
        solve(Mat.Sigma %*% t.Mat.full) %*%
        Mat.Sigma
      
    cond.Sigma[2]
})
end.cfull = Sys.time()


start.clist = Sys.time()
z.list = sapply(1:N.ITER, function(i){
    Mat.Sigma = lapply(1:n.strata, function(d){
        Mat.list[[d]] %*% Sigma.list[[d]]
    })
    cond.Sigma = lapply(1:n.strata, function(d){
        Sigma.list[[d]] -
            Sigma.list[[d]] %*% t.Mat.list[[d]] %*%
            solve(Mat.Sigma[[d]] %*% t.Mat.list[[d]]) %*%
            Mat.Sigma[[d]]
    })
    
    cond.Sigma[[1]][d]
})
end.clist = Sys.time()


print("list vs full conditioning:")
print(as.numeric(end.clist-start.clist) / as.numeric(end.cfull-start.cfull))

print("secs per full conditioning:")
print(as.numeric(end.cfull-start.cfull) / N.ITER)

print('full conditionings per second:')
print(N.ITER / as.numeric(end.cfull-start.cfull))


print("secs per list conditioning:")
print(as.numeric(end.clist-start.clist) / N.ITER)

print("DONE CONDITIONING TEST")
