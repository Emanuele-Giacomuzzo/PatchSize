# PatchSize

*Ecosystem size mediates the effects of resource flows on biodiversity and ecosystem function at different scales.*

## Abstract

Ecosystem size and resource flows are key factors driving biodiversity and ecosystem function. However, the question of whether and how these drivers interact has been largely overlooked. Here, we investigated how ecosystem size asymmetry affects biodiversity and function of two-patch meta-ecosystems connected through flows of non-living resources. We conducted a microcosm experiment, mimicking spatial resource flows between ecosystems of different sizes yet otherwise identical properties or between ecosystems of the same size. Meta-ecosystems with asymmetric ecosystem sizes displayed higher α- diversity but lower β-diversity and ecosystem function (total biomass) than their unconnected counterparts. At the same time, such an effect was not found for meta-ecosystems of identical patch sizes. Our work demonstrates how the size of ecosystems, interconnected via resource flows, can modulate cross-ecosystem dynamics, having implications for biodiversity and function across scales.

## Content of the repository

This repository contains data and code from a microcosm experiment designed to investigate whether ecosystem size can mediate the effects of resource flow on biodiversity and ecosystem function. To gather this data, cultures were filmed for 28 days. The videos were analyzed using the R package BEMOVI, which is based on ImageJ, by tracking particles and excluding those that couldn't have been protists. The repository includes data files obtained from the video analysis and code used for data analysis. The flow for data analysis can be found in the master r markdown file and its associated code chunks (referred to as "children" in r markdown). This data will be used for the upcoming publication: *Ecosystem size mediates the effects of resource flows on biodiversity and ecosystem function at different scales.* This repository is structured with the following folders and files:

-   1_data: Data files obtained from analysing the culture videos with the R package BEMOVI
-   2_analysis: R files to analyse the data 
-   3_results: Results saved during the analysis
-   4_other: Other files

-   1_experiment: Files related to planning and executing the microcosm experiment
-   2_data: 
-   3_r_files: 
-   4_environments: RStudio environments with the objects created during the analysis
-   5_images: Images
-   6_results: 
-   renv: Files related to the r package `renv`. This package enables you to restore the exact package versions used when working on this project.
-   library.bib: Bibliography database file
-   master.Rmd: Master file to produce the R markdown with the analysis of the data
-   PatchSize.Rproj: PatchSize project on RStudio
-   [renv.lock](https://github.com/Emanuele-Giacomuzzo/PatchSize/blob/master/renv.lock "renv.lock"): File from the package `renv` which contains information on which packages (and versions) were used when working on this project.
-   results_high_disturbance.html: Results of the analysis for the high disturbance treatment (resulting from knitting the master file with disturbance high)
-   results_low_disturbance.html: Results of the analysis for the low disturbance treatment (resulting from knitting the master file with disturbance low)

## R and R studio versions

This project was developed using the following versions:

-   R: 4.3.2

-   RStudio: 2023.12.1.402

## Installation and Code Execution

1.  Open the PatchSize project by opening the PatchSize.Rproj file using RStudio.
2.  Install the `knitr` package if you do not have installed installed on your machine. Do not worry about installing other packages (see step 5).
3.  Open the master r markdown file master.Rmd
4.  Define which disturbance level you want to analyse by assigning disturbance_global_input = "low" or disturbance_global_input = "high"
5.  Press the 'knit' button to generate the HTML file containing the analysed data. By knitting the R markdown file, the R package `renv` installs all the necessary packages with the correct versions in the project, so that you don't need to manually manage package versions.

## R packages

Although the `renv` package should enable you to reinstall all packages with the correct versions, I'm providing you with the necessary packages in case there are any issues with `renv`.

-   **Base Packages:**
    -   stats
    -   graphics
    -   grDevices
    -   datasets
    -   utils
    -   methods
    -   base
-   **Other Attached Packages:**
    -   conflicted_1.2.0
    -   combinat_0.0-8
    -   Rmisc_1.5.1
    -   betapart_1.6
    -   vegan_2.6-6.1
    -   lattice_0.22-6
    -   permute_0.9-7
    -   lmerTest_3.1-3
    -   lme4_1.1-35.4
    -   Matrix_1.6-5
    -   plotly_4.10.4
    -   ggpubr_0.6.0
    -   lubridate_1.9.3
    -   forcats_1.0.0
    -   stringr_1.5.1
    -   dplyr_1.1.4
    -   purrr_1.0.2
    -   readr_2.1.5
    -   tidyr_1.3.1
    -   tibble_3.2.1
    -   ggplot2_3.5.1
    -   tidyverse_2.0.0
    -   plyr_1.8.9
    -   renv_1.0.7.9000
    -   testthat_3.2.1.1
    -   here_1.0.1
-   **Loaded via a Namespace (and not attached):**
    -   gridExtra_2.3
    -   rlang_1.1.4
    -   magrittr_2.0.3
    -   compiler_4.3.2
    -   mgcv_1.9-0
    -   vctrs_0.6.5
    -   pkgconfig_2.0.3
    -   fastmap_1.2.0
    -   backports_1.5.0
    -   labeling_0.4.3
    -   magic_1.6-1
    -   utf8_1.2.4
    -   rmarkdown_2.27
    -   pracma_2.4.4
    -   tzdb_0.4.0
    -   nloptr_2.1.1
    -   itertools_0.1-3
    -   waldo_0.5.2
    -   xfun_0.45
    -   cachem_1.1.0
    -   jsonlite_1.8.8
    -   highr_0.11
    -   broom_1.0.6
    -   parallel_4.3.2
    -   cluster_2.1.4
    -   R6_2.5.1
    -   bslib_0.7.0
    -   stringi_1.8.4
    -   pkgload_1.3.4
    -   car_3.1-2
    -   boot_1.3-30
    -   brio_1.1.5
    -   jquerylib_0.1.4
    -   rcdd_1.6
    -   numDeriv_2016.8-1.1
    -   Rcpp_1.0.12
    -   iterators_1.0.14
    -   knitr_1.47
    -   snow_0.4-4
    -   picante_1.8.2
    -   splines_4.3.2
    -   timechange_0.3.0
    -   tidyselect_1.2.1
    -   rstudioapi_0.16.0
    -   abind_1.4-5
    -   yaml_2.3.8
    -   codetools_0.2-20
    -   minpack.lm_1.2-4
    -   withr_3.0.0
    -   evaluate_0.24.0
    -   desc_1.4.3
    -   pillar_1.9.0
    -   carData_3.0-5
    -   foreach_1.5.2
    -   geometry_0.4.7
    -   generics_0.1.3
    -   rprojroot_2.0.4
    -   hms_1.1.3
    -   munsell_0.5.1
    -   scales_1.3.0
    -   minqa_1.2.7
    -   glue_1.7.0
    -   lazyeval_0.2.2
    -   tools_4.3.2
    -   data.table_1.15.4
    -   ggsignif_0.6.4
    -   cowplot_1.1.3
    -   fastmatch_1.1-4
    -   grid_4.3.2
    -   ape_5.8
    -   crosstalk_1.2.1
    -   colorspace_2.1-0
    -   nlme_3.1-163
    -   cli_3.6.3
    -   optimx_2023-10.21
    -   fansi_1.0.6
    -   viridisLite_0.4.2
    -   doSNOW_1.0.20
    -   gtable_0.3.5
    -   rstatix_0.7.2
    -   sass_0.4.9
    -   digest_0.6.36
    -   farver_2.1.2
    -   htmlwidgets_1.6.4
    -   memoise_2.0.1
    -   htmltools_0.5.8.1
    -   lifecycle_1.0.4
    -   httr_1.4.7
    -   MASS_7.3-60
