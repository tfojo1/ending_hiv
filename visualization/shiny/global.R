install.packages(
  "resources/distributions_0.1.0.tar.gz", 
  repos=NULL, 
  type="source")
install.packages(
  "resources/bayesian.simulations_0.2.2.tar.gz", 
  repos=NULL, 
  type="source")
install.packages(
  "resources/jheem_0.1.0.tar.gz", 
  repos=NULL, 
  type="source")

# Custom installations
# By default, the directories in .libPaths() aren't writable on shinyapps.io, so
# create a subdir where we'll install our package.
if (!file.exists("R-lib")) {
  dir.create("R-lib")
}
# Unfortunately, there's no way to get deployapp() to ignore this directory, so
# make sure to remove it locally before you call deployapp(). This can be done
# with:
#   unlink("pkgInst/R-lib", recursive = TRUE)

# You may also need to restart R before calling deployapp(), because calling
# runApp() will modify your libpath (below), which can confuse deployapp().

# Add ./R-lib/ to the libPaths
.libPaths( c(normalizePath("R-lib/"), .libPaths()) )

# Install the package if needed.
if (!do.call(require, list("distributions"))) {
  install.packages(
    "resources/distributions_0.1.0.tar.gz", 
    repos=NULL, type = "source")
}

# Instead of `library(myPackage)`, we'll use do.call, to evade deployapp's
# checks for packages installed locally from source.
do.call(library, list("distributions"))
library('distributions')

if (!do.call(require, list("bayesian.simulations")))
  install.packages(
    "resources/bayesian.simulations_0.2.2.tar.gz", 
    repos=NULL, 
    type="source")
# do.call(library, list("bayesian.simulations"))
library('bayesian.simulations')

if (!do.call(require, list("jheem")))
  install.packages(
    "resources/jheem_0.1.0.tar.gz", 
    repos=NULL, 
    type="source")
do.call(library, list("jheem"))
library('jheem')