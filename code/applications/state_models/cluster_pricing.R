

dollars.per.gb.hr = 0.02
gb.per.simulation = 16
seconds.per.simulation = 12
n.sim.per.location = 200000*5
n.locations = 3
n.calibrations.per.year = 4

total = n.calibrations.per.year * n.locations * n.sim.per.location * seconds.per.simulation / 60 / 60 * gb.per.simulation *dollars.per.gb.hr
print(total)