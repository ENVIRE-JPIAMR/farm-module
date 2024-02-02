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
    "scales")
  )
}

####################
## LOAD LIBRARIES ##
####################

library(tidyverse) # for data cleaning and plotting
library(mc2d)      # for pert distribution
library(future)
library(furrr)
library(scales)

## Cleanup
rm("install_libraries")



