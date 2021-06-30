
setwd("Ending_HIV")
source('code/source_code.R')
source('code/targets/target_msas.R')

print("ASSEMBLING QUICK MCMCs FROM INITIAL RUNS")
print(Sys.Date())
print(Sys.time())

to.do = TARGET.MSAS
done.filenames = list.files(file.path(SYSTEMATIC.ROOT.DIR, 'quick_simsets'))
already.done.mask = sapply(to.do, function(msa){
    any(grepl(msa, done.filenames))
})

mcmc.filenames = list.files(file.path(SYSTEMATIC.ROOT.DIR, 'systematic_initial'))
mcmc.done.mask = sapply(to.do, function(msa){
    any(grepl(msa, mcmc.filenames))
})

to.do = to.do[mcmc.done.mask & !already.done.mask]
if (length(to.do)==0)
    print(paste0("Have already made quick simsets for all done MCMCs (",
                 sum(!mcmc.done.mask), " MSA",
                 ifelse(sum(!mcmc.done.mask)==1," is","s are"),
                 " still pending)"))
if (length(to.do)>1)
{
    print(paste0("Extracting simset for ",
                 length(to.do), " MSA",
                 ifelse(length(to.do)==1, "", "s"),
                 " (",
                 sum(!mcmc.done.mask), " MSA",
                 ifelse(sum(!mcmc.done.mask)==1," is","s are"),
                 " still pending)"))
    
    for (msa in to.do)
    {
        print(paste0("- Doing ", msa, " (", msa.names(msa), ")..."))
        load(file.path(SYSTEMATIC.ROOT.DIR, "systematic_initial", paste0(msa, ".Rdata")))
        simset = extract.simset(mcmc, additional.burn=10000, additional.thin=200)
        save(simset, file=file.path(SYSTEMATIC.ROOT.DIR, "quick_simsets", paste0("1.0_", msa, "_quick.Rdata")))
        print("   ...DONE")
    }
}