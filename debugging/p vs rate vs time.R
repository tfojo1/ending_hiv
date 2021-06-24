
p = c(.7,.75,.8,.85,.9)
p = pmax(.4, pmin(.95, rnorm(1000, .8, .05)))
print(mean(p))

r = -log(1-p)
print(1-exp(-mean(r)))

t = 1/r
print(1-exp(-1/(mean(t))))

print(qplot(p)) +
    geom_vline(xintercept = mean(p), size=2, color='red') +
    geom_vline(xintercept = 1-exp(-mean(r)), size=2, color='blue') +
    geom_vline(xintercept = 1-exp(-1/(mean(t))), size=2, color='green')
