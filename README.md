# The health system response to COVID-19 will continue long after we vaccinate 80% of eligible adults

## Overview 
This repo contains the R statistical code to reproduce the analysis in the paper The health system response to COVID-19 will continue long after we vaccinate 80% of eligible adults (_add link to final publication_).

## Methods in brief

This analysis uses an age-structured deterministic compartmental model implemented in the R package [`COVOID` (COVID-19 Opensource Infectious Dynamics)](https://cbdrh.github.io/covoidance/). In this extension of the widely used SIR epidemic model, individual disease status is classified as one of five states: Susceptible, Exposed, Infectious, Hospitalised or Recovered (SEIHR). The model also distinguishes between vaccinated and unvaccinated individuals, resulting in a total of 10 compartments, as depicted below. 

![](https://raw.githubusercontent.com/CBDRH/vaccineBorders/93d959072a5c20b0ecde7727ba8922902c25242d/Images/seihr-model.png "Well head")

Technical details, installation instructions and worked examples for the `COVOID` R package are available from the companion website https://cbdrh.github.io/covoidance/. A complete description of the model specifcation and assumptions used in this analysis is provided in the Technical Appendix to the main paper. 

## Software 

The analysis was undertaken using R v4.0.3 running on unix (Darwin 17.0) and the following R packages:  

* `covoid` (0.2.0) 

* `tidyverse` (1.3.1)

* `kableExtra` (1.3.4) 

* `ggpubr` (0.4.0)

* `RColorBrewer` (1.1-2)

## Suggested citation 
Hanly MJ, Churches, T, Fitzgerald O, Post JJ, MacIntyre CR, Jorm L. The health system response to COVID-19 will continue long after we vaccinate 80% of eligible adults, _The Medical Journal of Australia_ 2021. _(TBC following publication)_ 

## Files to reproduce the analysis

All files to replicate, adapt or extend this analysis are saved in the `R/` subfolder. There are six files in total:  

1. `simulation-control.R`
1. `simulation-functions.R`
1. `seir-cv-baseline.R`
1. `seir-cv-scenarios-noNPI.R`
1. `seir-cv-scenarios-NPI.R`
1. `seir-cv-visualisations.R`

The entire analysis can be reproduced by running the file `simulation-control.R`. This script will call the other files in the order listed below to run the simulations from the published analysis and recreate the corresponding figures. Adapting or extending the analysis requires editing the other files as desired. The purpose of each script is brielfy summarised below. 

**`simulation-control.R`** 

* Loads required libraries. 
* Sets a random seed. 
* Creates an `Outputs` subfolder, if it does not exist. 
* Sources the remaining files in order to replicate the published analysis.

**`simulation-functions.R`** 

* Defines useful functions used in the analysis and data summary.
 
**`seir-cv-baseline.R`** 

* Defines a baseline parameterisation of the SEIHR epidemic model. 

**`seir-cv-scenarios-noNPI.R`** 

* Parameterises and runs the simulations for scenarios _without_ non-pharmaceutical interventions (NPIs) on social contacts.

* Saves the following files in the `Outputs` folder: 

    * `simulation_results_noNPI.Rda` (A list with the raw results of each simulation run)
    * `incidence_results_noNPI.Rda` (A list with a condensed summary of each simulation run)
    * `incidence_results_full_noNPI.Rda` (The condensed summary of all simulations compiled into a dataframe)

**`seir-cv-scenarios-NPI.R`** 

* Parameterises and runs the simulations for scenarios _with_ nNPIs on social contacts. 

* Saves the following files in the `Outputs` folder: 

    * `simulation_results_NPI.Rda` (A list with the raw results of each simulation run)
    * `incidence_results_NPI.Rda` (A list with a condensed summary of each simulation run)
    * `incidence_results_full_NPI.Rda` (The condensed summary of all simulations compiled into a dataframe)

**`seir-cv-visualisations.R`**  

* Creates plots of the vaccine rollout projections, and estimates hospitalisation & infection over time.

* Specifically, saves the following files in the output folder: 
    * `yyyy-mm-dd-vaccine_projections.png` (projected vaccination coverage over time)
    * `yyyy-mm-dd-hospitalisation_projections.png` (projected number of hospitalised patients over time)
    * `yyyy-mm-dd-hospitalisation_projections-hi-res.png`  (projected number of hospitalised patients over time ,higher resolution)
    * `yyyy-mm-dd-infection_projections.png` (projected number of infected patients over time)
    * `yyyy-mm-dd-infection_projections-hi-res.png` (projected number of infected patients over time, higher resolution)

## Preprint
An early version of this analysis was published as an MJA preprint at the following link: https://www.mja.com.au/journal/2021/health-system-response-covid-19-will-continue-long-after-we-reach-80-vaccination

***


