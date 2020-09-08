

IMAGE.DIR = 'results/figures'

if (1==2)
{
    ALPHA = 0.2
    
    png(file.path(IMAGE.DIR, 'Figure 2a.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='msm', data.type='new', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2b.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='msm', data.type='prevalence', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2c.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='idu', data.type='new', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2d.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='idu', data.type='prevalence', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2e.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='msm_idu', data.type='new', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2f.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='msm_idu', data.type='prevalence', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2g.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='heterosexual', data.type='new', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    png(file.path(IMAGE.DIR, 'Figure 2h.png'), pointsize=10, width=3, height=1.75, res=300, units='in')
    plot.simulations.race.risk(simset, risk='heterosexual', data.type='prevalence', simulation.alpha=ALPHA, truth.point.size=2.5, truth.color='gray40', baseline.color='gray60') +
        theme(legend.position = 'none', axis.title.x = element_blank())
    dev.off()
    
    
    plot.simulations.total(simset)
    plot.simulations.flex(simset, split.by='race')
}
