
get.surveillance.data(location.codes=attr(simset@simulations[[1]], 'location'), data.type='diagnosed')

d.age = extract.simset.distribution(simset,
                                    extract.diagnosed.hiv,
                                    year=2018,
                                    keep.dimensions='age')
p.age = get.means(d.age); o.age=p.age/(1-p.age); ors.age = o.age/o.age[3];p.age;ors.age


d.sex = extract.simset.distribution(simset,
                                    extract.diagnosed.hiv,
                                    year=2018,
                                    keep.dimensions='sex',
                                    use.cdc.categorizations=T)
get.means(d.sex)
