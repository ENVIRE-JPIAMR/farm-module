## Script for loading libraries

## Set install_libraries to TRUE, or source manually the following
## block, in order to install all the libraries needed.
install_libraries = FALSE

## Install libraries
if (install_libraries)
{
  install.packages(c(
    "tidyverse",
    "mc2d",
    "future",
    "furrr",
    "scales",
    "ggplot2",
    "doSNOW",
    "foreach",
    "parallel")
  )
}

####################
## LOAD LIBRARIES ##
####################

library(tidyverse)    # for data cleaning and plotting
library(mc2d)         # for pert distribution
library(future)
library(furrr)
library(scales)
library(ggplot2)      # for visualization
library(doSNOW)       # for parallelization
library(foreach)      # for parallelization
library(parallel)     # for parallelization
library(abind)        # for parallelization bind function

## Cleanup
rm("install_libraries")



