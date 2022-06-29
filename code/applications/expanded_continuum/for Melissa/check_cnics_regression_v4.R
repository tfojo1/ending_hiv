round(exp(cbind(RECENT=c(model.recent.to.failing.noslopes$coefficients,NA), 
          DURABLE=c(model.durable.to.failing.noslopes$coefficients,NA),
          ALL=model.supp.to.failing.noslopes$coefficients)),2)

round(exp(cbind(RECENT=c(model.recent.to.lost.noslopes$coefficients,NA), 
                DURABLE=c(model.durable.to.lost.noslopes$coefficients,NA),
                ALL=model.supp.to.lost.noslopes$coefficients)),2)


r.to.f.1y = model.recent.to.failing.noslopes
r.to.l.1y = model.recent.to.lost.noslopes

d.to.f.1y = model.durable.to.failing.noslopes
d.to.l.1y = model.durable.to.lost.noslopes

s.to.f.1y = model.supp.to.failing.noslopes
s.to.l.1y = model.supp.to.lost.noslopes

round(exp(cbind(RECENT.1y=c(r.to.f.1y$coefficients,NA), 
                RECENT.2y=c(model.recent.to.failing.noslopes$coefficients,NA), 
                DURABLE.1y=c(d.to.f.1y$coefficients,NA),
                DURABLE.2y=c(model.durable.to.failing.noslopes$coefficients,NA),
                ALL.1y=s.to.f.1y$coefficients,
                ALL.2y=model.supp.to.failing.noslopes$coefficients
                ))
      ,2)

round(exp(cbind(RECENT.1y=c(r.to.l.1y$coefficients,NA), 
                RECENT.2y=c(model.recent.to.lost.noslopes$coefficients,NA), 
                DURABLE.1y=c(d.to.l.1y$coefficients,NA),
                DURABLE.2y=c(model.durable.to.lost.noslopes$coefficients,NA),
                ALL.1y=s.to.l.1y$coefficients,
                ALL.2y=model.supp.to.lost.noslopes$coefficients
))
,2)


round(exp(cbind(RECENT=c(model.recent.to.failing.noslopes$coefficients,NA,NA), 
                DURABLE=c(model.durable.to.failing.noslopes$coefficients,NA,NA),
                old.ALL=c(bk.f$coefficients, NA),
                ALL=model.supp.to.failing.noslopes$coefficients)),2)

round(exp(cbind(RECENT=c(model.recent.to.lost.noslopes$coefficients,NA,NA), 
                DURABLE=c(model.durable.to.lost.noslopes$coefficients,NA,NA),
                old.ALL=c(bk.l$coefficients, NA),
                ALL=model.supp.to.lost.noslopes$coefficients)),2)



z=names(output)[grepl('noslopes', names(output)) & grepl('coefficients', names(output))]
sapply(output[z], length)

print.nicely = function(col.names)
{
    rv = NULL
    for (col.name in col.names)
        rv = cbind(rv,
                   c(expit(output[[col.name]][1]),
                     exp(output[[col.name]][-1])))
    
    dimnames(rv)[[2]] = gsub("\\.noslopes\\.coefficients", '', col.names)
    
    round(rv,2)
}

print.nicely(c('naive.to.suppressed.noslopes.coefficients', 
             'failing.to.suppressed.noslopes.coefficients'))

print.nicely(c('recent.to.failing.noslopes.coefficients', 
             'durable.to.failing.noslopes.coefficients'))

print.nicely(c('naive.to.lost.noslopes.coefficients', 
             'failing.to.lost.noslopes.coefficients',
             'recent.to.lost.noslopes.coefficients',
             'durable.to.lost.noslopes.coefficients'))

print.nicely('disengaged.noslopes.coefficients')
