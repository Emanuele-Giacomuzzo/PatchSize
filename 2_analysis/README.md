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
- `stats` - Statistical functions and models.
- `graphics` - Basic plotting functions.
- `grDevices` - Graphics devices for output.
- `utils` - Utility functions for data manipulation.
- `datasets` - Sample datasets for R.
- `methods` - Object-oriented programming.
- `base` - Core R functions.

### Other Attached Packages
- `conflicted` - Manages namespace conflicts.
- `optimx` - Advanced optimization tools.
- `broom.mixed` - Tidies mixed models.
- `emmeans` - Estimated marginal means.
- `combinat` - Combinatorial functions.
- `Rmisc` - Summary statistics and plot utilities.
- `betapart` - Beta diversity calculations.
- `vegan` - Ecological diversity analysis.
- `lattice` - Advanced, grid-based plotting.
- `permute` - Permutation tests.
- `glmmTMB` - Generalized linear mixed models.
- `lmerTest` - Linear mixed models with tests.
- `lme4` - Linear and nonlinear mixed models.
- `Matrix` - Matrix data structures.
- `DHARMa` - Diagnostics for hierarchical models.
- `GGally` - Additional ggplot2 functions.
- `gridExtra` - Extends grid graphics layout.
- `plotly` - Interactive plotting.
- `ggpubr` - Simplifies ggplot2-based publication plots.
- `lubridate` - Date and time manipulation.
- `forcats` - Factor variable management.
- `stringr` - String manipulation.
- `dplyr` - Data manipulation grammar.
- `purrr` - Functional programming tools.
- `readr` - Data import.
- `tidyr` - Data tidying.
- `tibble` - Enhanced data frames.
- `ggplot2` - Data visualization.
- `tidyverse` - Collection of data science packages.
- `plyr` - Data manipulation tools.
- `renv` - Dependency management.
- `testthat` - Unit testing framework.
- `here` - Simplifies file path management.

### Loaded via Namespace (and Not Attached)
- `RColorBrewer` - Color palettes.
- `rstudioapi` - RStudio interface tools.
- `jsonlite` - JSON handling.
- `magrittr` - Pipe operators.
- `estimability` - Matrix calculations for estimable functions.
- `farver` - Color space conversion.
- `nloptr` - Nonlinear optimization.
- `rmarkdown` - Document conversion.
- `vctrs` - Vector types and coercion.
- `memoise` - Caching for functions.
- `minqa` - Optimization algorithms.
- `rstatix` - Statistical functions.
- `htmltools` - HTML tools for web output.
- `itertools` - Iterator functions.
- `broom` - Tidies statistical outputs.
- `Formula` - Formula notation.
- `pracma` - Practical math functions.
- `sass` - CSS with superpowers.
- `parallelly` - Tools for parallel processing.
- `bslib` - Bootstrap themes.
- `desc` - Manipulate DESCRIPTION files.
- `htmlwidgets` - Interactive web visualizations.
- `cachem` - Cache management.
- `TMB` - Template Model Builder for complex models.
- `mime` - MIME type utilities.
- `lifecycle` - Lifecycle management.
- `minpack.lm` - Nonlinear regression.
- `iterators` - Create iterators.
- `pkgconfig` - Package options.
- `gap` - Genetic analysis programs.
- `R6` - R6 class system.
- `fastmap` - Fast key-value mapping.
- `shiny` - Web applications.
- `rbibutils` - Bibliography utilities.
- `future` - Asynchronous programming.
- `magic` - Magic square and other functions.
- `digest` - Hashing functions.
- `numDeriv` - Numerical derivatives.
- `colorspace` - Color space utilities.
- `furrr` - Parallel mapping functions.
- `rprojroot` - Find root directories.
- `pkgload` - Load packages.
- `qgam` - Quantile additive models.
- `labeling` - Enhanced label handling.
- `fansi` - Advanced string handling.
- `timechange` - Time zone manipulation.
- `httr` - HTTP request handling.
- `abind` - Array binding.
- `mgcv` - Generalized additive models.
- `compiler` - Compile R code.
- `doParallel` - Parallel computation.
- `withr` - Temporary changes to global state.
- `backports` - Backward compatibility.
- `carData` - Companion to Applied Regression datasets.
- `ggstats` - Statistical extensions for ggplot2.
- `highr` - Syntax highlighting.
- `ggsignif` - Add significance bars to ggplots.
- `MASS` - Statistical functions and datasets.
- `tools` - Low-level utilities.
- `ape` - Phylogenetic analysis.
- `httpuv` - HTTP and WebSocket server.
- `glue` - String interpolation.
- `rcdd` - Computational geometry.
- `promises` - Promises for asynchronous programming.
- `nlme` - Mixed-effects models.
- `grid` - Low-level graphics functions.
- `cluster` - Clustering functions.
- `generics` - S3 method generics.
- `snow` - Simple network of workstations.
- `gtable` - Table layouts for grid graphics.
- `tzdb` - Time zone database.
- `data.table` - Fast data manipulation.
- `hms` - Time-of-day values.
- `car` - Companion to Applied Regression.
- `utf8` - UTF-8 text handling.
- `foreach` - Looping construct for parallel.
- `pillar` - Format columns.
- `later` - Deferred execution.
- `splines` - Spline functions.
- `tidyselect` - Helper for selecting variables.
- `knitr` - Dynamic report generation.
- `reformulas` - Reparameterizes models.
- `xfun` - Miscellaneous functions.
- `brio` - Basic R I/O.
- `stringi` - String processing.
- `lazyeval` - Lazy evaluation.
- `yaml` - YAML parser.
- `boot` - Bootstrap functions.
- `evaluate` - Evaluate R code with output capture.
- `codetools` - Code analysis.
- `cli` - Command-line tools.
- `xtable` - Export tables to LaTeX or HTML.
- `geometry` - Computational geometry.
- `Rdpack` - Utilities for Rd documentation.
- `munsell` - Munsell color system.
- `jquerylib` - jQuery library.
- `Rcpp` - R and C++ integration.
- `doSNOW` - Parallel computing with snow.
- `globals` - Global variable handling.
- `coda` - MCMC diagnostics.
- `parallel` - Parallel processing.
- `picante` - Phylogenetic community analysis.
- `gap.datasets` - Genetic analysis datasets.
- `listenv` - List environments.
- `viridisLite` - Color scales.
- `mvtnorm` - Multivariate normal and t distributions.
- `scales` - Graphical scales for visualizations.
- `rlang` - Tools for programming in R.
- `cowplot` - Plot grids and annotations.
- `fastmatch` - Fast matching.