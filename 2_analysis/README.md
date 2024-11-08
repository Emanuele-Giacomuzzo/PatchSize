## Content of the folder

-   functions: folder with R functions
-   r_markdown_files: folder with R markdown files
-   renv: Files related to the r package `renv`. This package enables you to restore the exact package versions used when working on this project.
-   .Rprofile: file to setup the startup behavior of R
-   PatchSize.Rproj: PatchSize project on RStudio
-   [renv.lock](https://github.com/Emanuele-Giacomuzzo/PatchSize/blob/master/renv.lock "renv.lock"): File from the package `renv` which contains information on which packages (and versions) were used when working on this project.

## Installation and Code Execution

1.  Open the PatchSize project by opening the PatchSize.Rproj file using RStudio.
2.  Install the `knitr` package if you do not have installed installed on your machine. Do not worry about installing other packages (see step 5).
3.  Open the master R markdown file "r_markdown_files/00_master.Rmd"
4.  Press the 'knit' button to generate the HTML file containing the analysed data. By knitting the R markdown file, the R package `renv` installs all the necessary packages with the correct versions in the project, so that you don't need to manually manage package versions.

## R and R studio versions

This project was developed using the following versions:

-   R: 4.4.1

-   RStudio: 2023.12.1.402

## R packages

Although the `renv` package should enable you to reinstall all packages with the correct versions, I'm providing you with the necessary packages in case there are any issues with `renv`.

### Base Packages
stats
graphics
grDevices
utils
datasets
methods
base
Other Attached Packages
conflicted_1.2.0
optimx_2023-10.21
broom.mixed_0.2.9.6
emmeans_1.10.5
combinat_0.0-8
Rmisc_1.5.1
betapart_1.6
vegan_2.6-8
lattice_0.22-6
permute_0.9-7
glmmTMB_1.1.10
lmerTest_3.1-3
lme4_1.1-35.5
Matrix_1.7-0
DHARMa_0.4.7
GGally_2.2.1
gridExtra_2.3
plotly_4.10.4
ggpubr_0.6.0
lubridate_1.9.3
forcats_1.0.0
stringr_1.5.1
dplyr_1.1.4
purrr_1.0.2
readr_2.1.5
tidyr_1.3.1
tibble_3.2.1
ggplot2_3.5.1
tidyverse_2.0.0
plyr_1.8.9
renv_1.0.11
testthat_3.2.1.1
here_1.0.1

### Loaded via Namespace (and Not Attached)
RColorBrewer_1.1-3
rstudioapi_0.17.1
jsonlite_1.8.9
magrittr_2.0.3
estimability_1.5.1
farver_2.1.2
nloptr_2.1.1
rmarkdown_2.28
vctrs_0.6.5
memoise_2.0.1
minqa_1.2.8
rstatix_0.7.2
htmltools_0.5.8.1
itertools_0.1-3
broom_1.0.7
Formula_1.2-5
pracma_2.4.4
sass_0.4.9
parallelly_1.38.0
bslib_0.8.0
desc_1.4.3
htmlwidgets_1.6.4
cachem_1.1.0
TMB_1.9.15
mime_0.12
lifecycle_1.0.4
minpack.lm_1.2-4
iterators_1.0.14
pkgconfig_2.0.3
gap_1.6
R6_2.5.1
fastmap_1.2.0
shiny_1.9.1
rbibutils_2.3
future_1.34.0
magic_1.6-1
digest_0.6.37
numDeriv_2016.8-1.1
colorspace_2.1-1
furrr_0.3.1
rprojroot_2.0.4
pkgload_1.4.0
qgam_1.3.4
labeling_0.4.3
fansi_1.0.6
timechange_0.3.0
httr_1.4.7
abind_1.4-8
mgcv_1.9-1
compiler_4.4.1
doParallel_1.0.17
withr_3.0.2
backports_1.5.0
carData_3.0-5
ggstats_0.7.0
highr_0.11
ggsignif_0.6.4
MASS_7.3-60.2
tools_4.4.1
ape_5.8
httpuv_1.6.15
glue_1.8.0
rcdd_1.6
promises_1.3.0
nlme_3.1-164
grid_4.4.1
cluster_2.1.6
generics_0.1.3
snow_0.4-4
gtable_0.3.6
tzdb_0.4.0
data.table_1.16.2
hms_1.1.3
car_3.1-3
utf8_1.2.4
foreach_1.5.2
pillar_1.9.0
later_1.3.2
splines_4.4.1
tidyselect_1.2.1
knitr_1.48
reformulas_0.3.0
xfun_0.49
brio_1.1.5
stringi_1.8.4
lazyeval_0.2.2
yaml_2.3.10
boot_1.3-30
evaluate_1.0.1
codetools_0.2-20
cli_3.6.3
xtable_1.8-4
geometry_0.5.0
Rdpack_2.6.1
munsell_0.5.1
jquerylib_0.1.4
Rcpp_1.0.13
doSNOW_1.0.20
globals_0.16.3
coda_0.19-4.1
parallel_4.4.1
picante_1.8.2
gap.datasets_0.0.6
listenv_0.9.1
viridisLite_0.4.2
mvtnorm_1.3-1
scales_1.3.0
rlang_1.1.4
cowplot_1.1.3
fastmatch_1.1-4