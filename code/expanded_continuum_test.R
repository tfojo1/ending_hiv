source('code/source_code.R')
SETTINGS = SETTINGS.EXPANDED.CONTINUUM

msa = '35620'
base.components = setup.initial.components(msa=msa)
components = base.components

jheem = setup.jheem.from.components(base.components)
