
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

## Contact Information

For questions or support, please contact me at EmanueleGiacomuzzo@gmail.com
