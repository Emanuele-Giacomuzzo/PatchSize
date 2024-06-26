
# PatchSize
> *_Giacomuzzo E., Peller T., Gounand I., Altermatt F. (2024). Ecosystem size mediates the effects of resource flows on biodiversity and ecosystem function at different scales._*

## Abstract

Ecosystem size and resource flows are key factors driving biodiversity and ecosystem function. However, the question of whether and how these drivers interact has been largely overlooked. Here, we investigated how ecosystem size asymmetry affects biodiversity and function of two-patch meta-ecosystems connected through flows of non-living resources. We conducted a microcosm experiment, mimicking spatial resource flows between ecosystems of different sizes yet otherwise identical properties or between ecosystems of the same size. Meta-ecosystems with asymmetric ecosystem sizes displayed higher α- diversity but lower β-diversity and ecosystem function (total biomass) than their unconnected counterparts. At the same time, such an effect was not found for meta-ecosystems of identical patch sizes. Our work demonstrates how the size of ecosystems, interconnected via resource flows, can modulate cross-ecosystem dynamics, having implications for biodiversity and function across scales.  

## Content of the repository

This repository contains data and code from a microcosm experiment designed to investigate whether ecosystem size can mediate the effects of resource flow on biodiversity and ecosystem function. To gather this data, cultures were filmed for 28 days. The videos were analyzed using the R package BEMOVI, which is based on ImageJ, by tracking particles and excluding those that couldn't have been protists. The repository includes data files obtained from the video analysis and code used for data analysis. The flow for data analysis can be found in the master r markdown file and its associated code chunks (referred to as "children" in r markdown). This data will be used for the upcoming publication: _Giacomuzzo E., Peller T., Gounand I., Altermatt F. (2024). Ecosystem size mediates the effects of resource flows on biodiversity and ecosystem function at different scales._ This repository is structured with the following folders and files:

- 0_bin: Deleted files
- 1_experiment: Files related to planning and executing the microcosm experiment
- 2_data: Data files obtained from analysing the culture videos with the R package BEMOVI
- 3_r_files: R files to analyse the data
- 4_environments: RStudio environments with the objects created during the analysis
- 5_images: Images
- 6_results: Results saved during the analysis
- 7_paper: Files to publish the paper
- library.bib: Bibliography database file
- master.Rmd: Master file to produce the R markdown with the analysis of the data
- PatchSize.Rproj: PatchSize project on RStudio
- results_high_disturbance.html: Results of the analysis for the high disturbance treatment
- results_low_disturbance.html: Results of the analysis for the low disturbance treatment

## Installation and Code Execution 

1. Clone the repository: git clone https://github.com/Emanuele-Giacomuzzo/PatchSize.git
2. Open the PatchSize project by opening the PatchSize.Rproj file using RStudio.
3. Ensure you have the necessary R packages installed, particularly knitr. You will find the packages needed for analysis in 3_r_files > set_packages.Rmd.
4. Open the master r markdown file master.Rmd
5. Define which disturbance level you want to analyse by assigning disturbance_global_input = "low" or disturbance_global_input = "high"
6. Press the knit button to create the html file with the analysed data. This requires the knitr package installed on your machine.

## R packages

To run the code you need a variety of base and additional packages. The base packages are stats, graphics, grDevices, utils, datasets, methods, and base. The other attached packages and their respective versions are: conflicted 1.2.0, combinat 0.0-8, Rmisc 1.5.1, betapart 1.6, vegan 2.6-4, lattice 0.21-9, permute 0.9-7, plotly 4.10.2, ggpubr 0.6.0, lubridate 1.9.2, forcats 1.0.0, stringr 1.5.1, dplyr 1.1.4, purrr 1.0.2, readr 2.1.4, tidyr 1.3.1, tibble 3.2.1, ggplot2 3.5.1, tidyverse 2.0.0, plyr 1.8.8, renv 1.0.2, testthat 3.2.1.1, here 1.0.1, lmerTest 3.1-3, lme4 1.1-35.3, and Matrix 1.6-5. Additionally, the following packages are loaded via a namespace but not attached: gridExtra 2.3, rlang 1.1.3, magrittr 2.0.3, compiler 4.3.2, mgcv 1.9-0, vctrs 0.6.5, pkgconfig 2.0.3, fastmap 1.1.1, backports 1.4.1, magic 1.6-1, utf8 1.2.4, rmarkdown 2.24, tzdb 0.4.0, nloptr 2.0.3, itertools 0.1-3, xfun 0.40, cachem 1.0.8, jsonlite 1.8.8, broom 1.0.5, parallel 4.3.2, cluster 2.1.4, R6 2.5.1, stringi 1.8.3, car 3.1-2, boot 1.3-28.1, brio 1.1.5, rcdd 1.5-2, numDeriv 2016.8-1.1, Rcpp 1.0.12, iterators 1.0.14, knitr 1.43, snow 0.4-4, picante 1.8.2, splines 4.3.2, timechange 0.2.0, tidyselect 1.2.1, rstudioapi 0.15.0, abind 1.4-5, yaml 2.3.7, codetools 0.2-19, minpack.lm 1.2-3, withr 3.0.0, evaluate 0.23, pillar 1.9.0, carData 3.0-5, foreach 1.5.2, geometry 0.4.7, generics 0.1.3, rprojroot 2.0.4, hms 1.1.3, munsell 0.5.1, scales 1.3.0, minqa 1.2.5, glue 1.7.0, lazyeval 0.2.2, tools 4.3.2, data.table 1.15.0, ggsignif 0.6.4, fastmatch 1.1-3, cowplot 1.1.1, grid 4.3.2, ape 5.7-1, colorspace 2.1-0, nlme 3.1-163, cli 3.6.2, fansi 1.0.6, viridisLite 0.4.2, doSNOW 1.0.20, gtable 0.3.5, rstatix 0.7.2, digest 0.6.35, htmlwidgets 1.6.2, memoise 2.0.1, htmltools 0.5.6, lifecycle 1.0.4, httr 1.4.7, and MASS 7.3-60.

## Contact Information

For questions or support, please contact me at EmanueleGiacomuzzo@gmail.com
