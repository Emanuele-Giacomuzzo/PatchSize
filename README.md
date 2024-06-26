
# PatchSize
> *_Giacomuzzo E., Peller T., Gounand I., Altermatt F. (2024). Ecosystem size mediates the effects of resource flows on biodiversity and ecosystem function at different scales._*

This repository contains data and code from a microcosm experiment designed to investigate whether ecosystem size can mediate the effects of resource flow on biodiversity and ecosystem function. To gather this data, cultures were filmed for 28 days. The videos were analyzed using the R package BEMOVI, which is based on ImageJ, by tracking particles and excluding those that couldn't have been protists. The repository includes data files obtained from the video analysis and code used for data analysis. The flow for data analysis can be found in the master r markdown file and its associated code chunks (referred to as "children" in r markdown). This data will be used for the upcoming publication: _Giacomuzzo E., Peller T., Gounand I., Altermatt F. (2024). Ecosystem size mediates the effects of resource flows on biodiversity and ecosystem function at different scales._

## Content of the repository

This repository is structured with the following folders and files:

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

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] conflicted_1.2.0 combinat_0.0-8   Rmisc_1.5.1      betapart_1.6     vegan_2.6-4      lattice_0.21-9   permute_0.9-7    plotly_4.10.2    ggpubr_0.6.0    
[10] lubridate_1.9.2  forcats_1.0.0    stringr_1.5.1    dplyr_1.1.4      purrr_1.0.2      readr_2.1.4      tidyr_1.3.1      tibble_3.2.1     ggplot2_3.5.1   
[19] tidyverse_2.0.0  plyr_1.8.8       renv_1.0.2       testthat_3.2.1.1 here_1.0.1       lmerTest_3.1-3   lme4_1.1-35.3    Matrix_1.6-5    

loaded via a namespace (and not attached):
 [1] gridExtra_2.3       rlang_1.1.3         magrittr_2.0.3      compiler_4.3.2      mgcv_1.9-0          vctrs_0.6.5         pkgconfig_2.0.3    
 [8] fastmap_1.1.1       backports_1.4.1     magic_1.6-1         utf8_1.2.4          rmarkdown_2.24      tzdb_0.4.0          nloptr_2.0.3       
[15] itertools_0.1-3     xfun_0.40           cachem_1.0.8        jsonlite_1.8.8      broom_1.0.5         parallel_4.3.2      cluster_2.1.4      
[22] R6_2.5.1            stringi_1.8.3       car_3.1-2           boot_1.3-28.1       brio_1.1.5          rcdd_1.5-2          numDeriv_2016.8-1.1
[29] Rcpp_1.0.12         iterators_1.0.14    knitr_1.43          snow_0.4-4          picante_1.8.2       splines_4.3.2       timechange_0.2.0   
[36] tidyselect_1.2.1    rstudioapi_0.15.0   abind_1.4-5         yaml_2.3.7          codetools_0.2-19    minpack.lm_1.2-3    withr_3.0.0        
[43] evaluate_0.23       pillar_1.9.0        carData_3.0-5       foreach_1.5.2       geometry_0.4.7      generics_0.1.3      rprojroot_2.0.4    
[50] hms_1.1.3           munsell_0.5.1       scales_1.3.0        minqa_1.2.5         glue_1.7.0          lazyeval_0.2.2      tools_4.3.2        
[57] data.table_1.15.0   ggsignif_0.6.4      fastmatch_1.1-3     cowplot_1.1.1       grid_4.3.2          ape_5.7-1           colorspace_2.1-0   
[64] nlme_3.1-163        cli_3.6.2           fansi_1.0.6         viridisLite_0.4.2   doSNOW_1.0.20       gtable_0.3.5        rstatix_0.7.2      
[71] digest_0.6.35       htmlwidgets_1.6.2   memoise_2.0.1       htmltools_0.5.6     lifecycle_1.0.4     httr_1.4.7          MASS_7.3-60 

## Contact Information

For questions or support, please contact me at EmanueleGiacomuzzo@gmail.com
