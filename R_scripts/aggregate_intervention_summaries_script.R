
setwd('Ending_HIV')

print("Sourcing Code Files")
source('code/source_code.R')
source('code/targets/target_msas.R')
source('code/process_results/distributed_process_results.R')
source('code/process_results/make_systematic_table.R')

ROOT.DIR = file.path(SYSTEMATIC.ROOT.DIR, "..", "results", "full", "estimates")

#-- MAIN --#
print("Assembling Main Estimates")
ests.main = assemble.estimates.and.intervals(dir.name='full',
                                             suffix='main',
                                             throw.error.if.all.missing = F)
if (is.null(ests.main))
    print("- No estimates have been done\n")
if (!is.null(ests.main))
{
    save(ests.main, file=file.path(ROOT.DIR, "ests.main.Rdata"))
    print("- Done\n")
}


#-- MAIN to 2025 --#
print("Assembling Main Estimates to 2025")
ests.main.2025 = assemble.estimates.and.intervals(dir.name='full',
                                             suffix='main.2025',
                                             throw.error.if.all.missing = F)

if (is.null(ests.main.2025))
    print("- No estimates have been done\n")
if (!is.null(ests.main))
{
    save(ests.main.2025, file=file.path(ROOT.DIR, "ests.main.2025.Rdata"))
    print("- Done\n")
}


#-- 3y ROLLOUT --#
print("Assembling Estimates, 3y Roll-Out")
ests.rollout.3y = assemble.estimates.and.intervals(dir.name='full',
                                             suffix='rollout.3y',
                                             throw.error.if.all.missing = F)

if (is.null(ests.rollout.3y))
    print("- No estimates have been done\n")
if (!is.null(ests.rollout.3y))
{
    save(ests.rollout.3y, file=file.path(ROOT.DIR, "ests.rollout.3y.Rdata"))
    print("- Done\n")
}


#-- 3y ROLLOUT, to 2025 --#
print("Assembling Estimates, 3y Roll-Out, to 2027")
ests.rollout.3y.2025 = assemble.estimates.and.intervals(dir.name='full',
                                                   suffix='rollout.3y.2025',
                                                   throw.error.if.all.missing = F)

if (is.null(ests.rollout.3y.2025))
    print("- No estimates have been done\n")
if (!is.null(ests.rollout.3y.2025))
{
    save(ests.rollout.3y.2025, file=file.path(ROOT.DIR, "ests.rollout.3y.2025.Rdata"))
    print("- Done\n")
}


#-- IDU --#
print("Assembling IDU Estimates")
ests.idu = assemble.estimates.and.intervals(dir.name='full',
                                             suffix='idu',
                                            throw.error.if.all.missing = F)

if (is.null(ests.idu))
    print("- No estimates have been done\n")
if (!is.null(ests.idu))
{
    save(ests.idu, file=file.path(ROOT.DIR, "ests.idu.Rdata"))
    print("- Done\n")
}
