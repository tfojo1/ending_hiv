calc.lambda = function(persistence){-log(persistence)}

ff = expression(calc.lambda(persistence))
#ff = expression(uptake * (1/lambda - exp(-lambda)/lambda))

params = c(persistence = .6)
e = list2env(as.list(params))

eval(ff, envir = e)


bquote(ff, where=e)
substitute(ff, env=e)

names(ff)

multiple = c(ff,ff)
length(multiple)

all.names(ff, functions=F, unique=T)
