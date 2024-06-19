
# PatchSize
> Giacomuzzo et al. (2024) Ecosystem size mediates the effects of resource flows on biodiversity and ecosystem function at different scales

This is the repository with data and code for the publication Giacomuzzo et al. (2024) Ecosystem size mediates the effects of resource flows on biodiversity and ecosystem function at different scales. 

## Content of the repository

This repository is structured with the following folders:

- 0_bin: deleted files.
- 1_experiment: files related to planning and executing the microcosm experiment.
- 2_data: data files obtained from analysing the culture videos with the R package BEMOVI.
- 3_r_files: r files to analyse the data.
- 4_environments: RStudio environments with the objects created during the analysis. 
- 5_images: images.
- 6_results: results saved during the analysis.
- 7_paper: files to publish the paper.

Furthermore, it contains the following files:

- library.bib: bibliography database file.
- master.Rmd: master file to produce the R markdown with the analysis of the data.
- PatchSize.Rproj: 
- results_high_disturbance.html: results of the analysis for the high disturbance treatment
- results_low_disturbance.html: results of the analysis for the low disturbance treatment

## Code execution

To execute the code in this repository:

1. Open the PatchSize project by opening the PatchSize.Rproj file using RStudio
2. Open the master r markdown file master.Rmd
3. Define which disturbance level you want to analyse by assigning disturbance_global_input = "low" or disturbance_global_input = "high"
4. Press the knit button to create the html file with the data analysed. This require you to have the r package knitr installed on your machine.
