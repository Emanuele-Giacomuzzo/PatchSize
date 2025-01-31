## Content of the folder

-   functions: folder with R functions
-   r_markdown_files: folder with R markdown files
-   renv: Files related to the r package `renv`. This package enables you to restore the exact package versions used when working on this project.
-   .Rprofile: file to setup the startup behavior of R
-   PatchSize.Rproj: PatchSize project on RStudio
-   [renv.lock](https://github.com/Emanuele-Giacomuzzo/PatchSize/blob/master/renv.lock "renv.lock"): File from the package `renv` which contains information on which packages (and versions) were used when working on this project.

## Installation and Code Execution

1.  Open the PatchSize project by opening the PatchSize.Rproj file using RStudio.
2.  Install the `knitr` package if you do not have installed installed on your machine. Do not worry about installing other packages (see step 4).
3.  Open the master R markdown file "r_markdown_files/00_master.Rmd"
4.  Press the 'knit' button to generate the HTML file containing the analysed data. By knitting the R markdown file, the R package `renv` installs all the necessary packages with the correct versions in the project, so that you don't need to manually manage package versions.

## R and R studio versions

This project was developed using the following versions:

-   R: 4.4.1

-   RStudio: 2023.12.1.402

## R packages

Although the `renv` package should enable you to reinstall all packages with the correct versions, I'm providing you with the necessary packages in case there are any issues with `renv`.

### Base Packages
- `stats` - Statistical functions and models.
- `graphics` - Basic plotting functions.
- `grDevices` - Graphics devices for output.
- `utils` - Utility functions for data manipulation.
- `datasets` - Sample datasets for R.
- `methods` - Object-oriented programming.
- `base` - Core R functions.

### Other Attached Packages
- `conflicted` (1.2.0) - Manages namespace conflicts.
- `optimx` (2023-10.21) - Advanced optimization tools.
- `broom.mixed` (0.2.9.6) - Tidies mixed models.
- `emmeans` (1.10.5) - Estimated marginal means.
- `combinat` (0.0-8) - Combinatorial functions.
- `Rmisc` (1.5.1) - Summary statistics and plot utilities.
- `betapart` (1.6) - Beta diversity calculations.
- `vegan` (2.6-8) - Ecological diversity analysis.
- `lattice` (0.22-6) - Advanced, grid-based plotting.
- `permute` (0.9-7) - Permutation tests.
- `glmmTMB` (1.1.10) - Generalized linear mixed models.
- `lmerTest` (3.1-3) - Linear mixed models with tests.
- `lme4` (1.1-35.5) - Linear and nonlinear mixed models.
- `Matrix` (1.7-0) - Matrix data structures.
- `DHARMa` (0.4.7) - Diagnostics for hierarchical models.
- `GGally` (2.2.1) - Additional ggplot2 functions.
- `gridExtra` (2.3) - Extends grid graphics layout.
- `plotly` (4.10.4) - Interactive plotting.
- `ggpubr` (0.6.0) - Simplifies ggplot2-based publication plots.
- `lubridate` (1.9.3) - Date and time manipulation.
- `forcats` (1.0.0) - Factor variable management.
- `stringr` (1.5.1) - String manipulation.
- `dplyr` (1.1.4) - Data manipulation grammar.
- `purrr` (1.0.2) - Functional programming tools.
- `readr` (2.1.5) - Data import.
- `tidyr` (1.3.1) - Data tidying.
- `tibble` (3.2.1) - Enhanced data frames.
- `ggplot2` (3.5.1) - Data visualization.
- `tidyverse` (2.0.0) - Collection of data science packages.
- `plyr` (1.8.9) - Data manipulation tools.
- `renv` (1.0.11) - Dependency management.
- `testthat` (3.2.1.1) - Unit testing framework.
- `here` (1.0.1) - Simplifies file path management.

### Loaded via Namespace (and Not Attached)
- `RColorBrewer` (1.1-3) - Color palettes.
- `rstudioapi` (0.17.1) - RStudio interface tools.
- `jsonlite` (1.8.9) - JSON handling.
- `magrittr` (2.0.3) - Pipe operators.
- `estimability` (1.5.1) - Matrix calculations for estimable functions.
- `farver` (2.1.2) - Color space conversion.
- `nloptr` (2.1.1) - Nonlinear optimization.
- `rmarkdown` (2.28) - Document conversion.
- `vctrs` (0.6.5) - Vector types and coercion.
- `memoise` (2.0.1) - Caching for functions.
- `minqa` (1.2.8) - Optimization algorithms.
- `rstatix` (0.7.2) - Statistical functions.
- `htmltools` (0.5.8.1) - HTML tools for web output.
- `itertools` (0.1-3) - Iterator functions.
- `broom` (1.0.7) - Tidies statistical outputs.
- `Formula` (1.2-5) - Formula notation.
- `pracma` (2.4.4) - Practical math functions.
- `sass` (0.4.9) - CSS with superpowers.
- `parallelly` (1.38.0) - Tools for parallel processing.
- `bslib` (0.8.0) - Bootstrap themes.
- `desc` (1.4.3) - Manipulate DESCRIPTION files.
- `htmlwidgets` (1.6.4) - Interactive web visualizations.
- `cachem` (1.1.0) - Cache management.
- `TMB` (1.9.15) - Template Model Builder for complex models.
- `mime` (0.12) - MIME type utilities.
- `lifecycle` (1.0.4) - Lifecycle management.
- `minpack.lm` (1.2-4) - Nonlinear regression.
- `iterators` (1.0.14) - Create iterators.
- `pkgconfig` (2.0.3) - Package options.
- `gap` (1.6) - Genetic analysis programs.
- `R6` (2.5.1) - R6 class system.
- `fastmap` (1.2.0) - Fast key-value mapping.
- `shiny` (1.9.1) - Web applications.
- `rbibutils` (2.3) - Bibliography utilities.
- `future` (1.34.0) - Asynchronous programming.
- `magic` (1.6-1) - Magic square and other functions.
- `digest` (0.6.37) - Hashing functions.
- `numDeriv` (2016.8-1.1) - Numerical derivatives.
- `colorspace` (2.1-1) - Color space utilities.
- `furrr` (0.3.1) - Parallel mapping functions.
- `rprojroot` (2.0.4) - Find root directories.
- `pkgload` (1.4.0) - Load packages.
- `qgam` (1.3.4) - Quantile additive models.
- `labeling` (0.4.3) - Enhanced label handling.
- `fansi` (1.0.6) - Advanced string handling.
- `timechange` (0.3.0) - Time zone manipulation.
- `httr` (1.4.7) - HTTP request handling.
- `abind` (1.4-8) - Array binding.
- `mgcv` (1.9-1) - Generalized additive models.
- `compiler` (4.4.1) - Compile R code.
- `doParallel` (1.0.17) - Parallel computation.
- `withr` (3.0.2) - Temporary changes to global state.
- `backports` (1.5.0) - Backward compatibility.
- `carData` (3.0-5) - Companion to Applied Regression datasets.
- `ggstats` (0.7.0) - Statistical extensions for ggplot2.
- `highr` (0.11) - Syntax highlighting.
- `ggsignif` (0.6.4) - Add significance bars to ggplots.
- `MASS` (7.3-60.2) - Statistical functions and datasets.
- `tools` (4.4.1) - Low-level utilities.
- `ape` (5.8) - Phylogenetic analysis.
- `httpuv` (1.6.15) - HTTP and WebSocket server.
- `glue` (1.8.0) - String interpolation.
- `rcdd` (1.6) - Computational geometry.
- `promises` (1.3.0) - Promises for asynchronous programming.
- `nlme` (3.1-164) - Mixed-effects models.
- `grid` (4.4.1) - Low-level graphics functions.
- `cluster` (2.1.6) - Clustering functions.
- `generics` (0.1.3) - S3 method generics.
- `snow` (0.4-4) - Simple network of workstations.
- `gtable` (0.3.6) - Table layouts for grid graphics.
- `tzdb` (0.4.0) - Time zone database.
- `data.table` (1.16.2) - Fast data manipulation.
- `hms` (1.1.3) - Time-of-day values.
- `car` (3.1-3) - Companion to Applied Regression.
- `utf8` (1.2.4) - UTF-8 text handling.
- `foreach` (1.5.2) - Looping construct for parallel.
- `pillar` (1.9.0) - Format columns.
- `later` (1.3.2) - Deferred execution.
- `splines` (4.4.1) - Functions for spline interpolation.
- `tidyselect` (1.2.1) - Helpers for selecting columns.
- `knitr` (1.48) - Dynamic report generation.
- `reformulas` (0.3.0) - Formula manipulation.
- `xfun` (0.49) - Utility functions for R packages.
- `brio` (1.1.5) - Read/write binary data.
- `stringi` (1.8.4) - Advanced string manipulation.
- `lazyeval` (0.2.2) - Lazy evaluation functions.
- `yaml` (2.3.10) - YAML file handling.
- `boot` (1.3-30) - Bootstrap resampling.
- `evaluate` (1.0.1) - Expression evaluation utilities.
- `codetools` (0.2-20) - Tools for analyzing R code.
- `cli` (3.6.3) - Command-line interface utilities.
- `xtable` (1.8-4) - LaTeX and HTML table conversion.
- `geometry` (0.5.0) - Computational geometry functions.
- `Rdpack` (2.6.1) - R documentation utilities.
- `munsell` (0.5.1) - Color handling.
- `jquerylib` (0.1.4) - jQuery dependency for web apps.
- `Rcpp` (1.0.13) - R and C++ integration.
- `doSNOW` (1.0.20) - Parallel computing using snow.
- `globals` (0.16.3) - Global variable management.
- `coda` (0.19-4.1) - Markov Chain Monte Carlo diagnostics.
- `parallel` (4.4.1) - Parallel computing functions.
- `picante` (1.8.2) - Phylogenetic analysis of communities.
- `gap.datasets` (0.0.6) - Datasets for genetic analysis.
- `listenv` (0.9.1) - Environment for lists.
- `viridisLite` (0.4.2) - Color scales.
- `mvtnorm` (1.3-1) - Multivariate normal distributions.
- `scales` (1.3.0) - Scales for plotting.
- `rlang` (1.1.4) - Functions for working with R expressions.
- `cowplot` (1.1.3) - Publication-ready plots with ggplot2.
- `fastmatch` (1.1-4) - Fast matching for R objects.
