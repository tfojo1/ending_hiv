
library(plotly)
head(midwest)

#for saving plots
# https://github.com/plotly/orca#windows



png('test plot.png', pointsize=10, width=3, height=2, res=300, units='in')
plot_ly(data=midwest, x=~percollege, color=~state, type='box')
dev.off() 


df1 = data.frame(x=1:10,
                sim=2+rnorm(10,1:10),
                type='sim1')

df2 = data.frame(x=1:10,
                 sim=4+rnorm(10,1:10),
                 type='sim2')

df.sim = rbind(df1, df2)

df.truth = data.frame(x=1:10,
                      obs=5 + rnorm(10))

plot = plot_ly()
plot = add_trace(plot, data=df.sim, x=~x, y=~sim, color=~type, mode='lines')
plot = add_trace(plot, data=df.truth, x=~x, y=~obs, mode='markers', name='Truth')
plot

#test save
orca(plot, 'test plot.png')
