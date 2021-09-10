# Install dev version of COVOID, if necessary (restart session after this)
# devtools::install_github('CBDRH/covoid', ref = 'responsive-interventions')

# Libraries
library(covoid)
library(tidyverse)
library(kableExtra)
library(ggpubr)
library(RColorBrewer)

# Fix random seed
set.seed(1290)

# create output directory
dir.create("Outputs")

# Source the simulation functions
source('R/simulation-functions.R')

# Source the baseline parameterisation
source('R/seir-cv-baseline.R')

# Run the 2x2x2 simulations without NPIs
source('R/seir-cv-scenarios-noNPI.R')

# Run the 2x2x2 simulations with NPIs
source('R/seir-cv-scenarios-NPI.R')

# Visualise the simulation results
source('R/seir-cv-visualisations.R')
