

x=load("results/full/estimates/approx959595.v2.Rdata")
source('code/process_results/extract_absolute_arrays.R')

dim(abs.arr)
abs.arr = add.abs.total.row(abs.arr)

x= abs.arr['2025','total','prevalence.diagnosed','Total',1,] / abs.arr['2025','total','prevalence.all','Total',1,]
mean(x)

load('results/full/estimates/ests.approx959595.3y.new.Rdata')
floor(100*ests.approx959595.3y.new$estimates['Total',2])
floor(100*range(ests.approx959595.3y.new$estimates[,2]))
floor(100*ests.approx959595.3y.new$estimates[,2])[order(ests.approx959595.3y.new$estimates[,2], decreasing=T)]
