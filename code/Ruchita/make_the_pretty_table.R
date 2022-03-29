
source('code/processing/pretty_table/make_pretty_table.R')

df = read.csv('results/prep/Incidence Reduction Table.csv')
#df = df[1:32,1:11]
dimnames(df)[[1]] = df[,1]
df = df[,-1]

for (col in 1:dim(df)[2])
    df[,col] = as.numeric(gsub('%','',df[,col]))/100

write.shaded.table(as.matrix(df),
                   file='results/prep/Incidence Reduction Table.xlsx', 
                   write.row.names = T, write.col.names = T)
