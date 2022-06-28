# Run once when first setting up someone to run JHEEM

# 1) Install Rtools
# this is a manual install - NOT through install.packages()
# For Windows:
#  https://cran.r-project.org/bin/windows/Rtools/
#  (restart computer - and Rstudio - after installing)


# 2) Install devtools (to be able to pull from github)
install.packages("devtools")
library(devtools)


# 3) Install Todd's three packages from GitHub
install_github(repo='tfojo1/jheem')

install_github(repo='tfojo1/distributions')

install_github(repo='tfojo1/bayesian.simulations')
