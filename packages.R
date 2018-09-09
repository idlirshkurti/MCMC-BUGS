# Call this from all the other scripts in order to load libraries
packages <- c("ggplot2", "gridExtra", "reshape", "MCMCpack", "lattice", "rjags", "mcmcplots")
lapply(packages, require, character.only = TRUE)
