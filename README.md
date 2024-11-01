# PatchSize

*Ecosystem size mediates the effects of resource flows on biodiversity and ecosystem function at different scales.*

## Abstract

Ecosystem size and resource flows are key factors driving biodiversity and ecosystem function. However, the question of whether and how these drivers interact has been largely overlooked. Here, we investigated how ecosystem size asymmetry affects biodiversity and function of two-patch meta-ecosystems connected through flows of non-living resources. We conducted a microcosm experiment, mimicking spatial resource flows between ecosystems of different sizes yet otherwise identical properties or between ecosystems of the same size. Meta-ecosystems with asymmetric ecosystem sizes displayed higher α- diversity but lower β-diversity and ecosystem function (total biomass) than their unconnected counterparts. At the same time, such an effect was not found for meta-ecosystems of identical patch sizes. Our work demonstrates how the size of ecosystems, interconnected via resource flows, can modulate cross-ecosystem dynamics, having implications for biodiversity and function across scales.

## Content of the repository

This repository contains data and code from a microcosm experiment designed to investigate whether ecosystem size can mediate the effects of resource flow on biodiversity and ecosystem function. To gather this data, cultures were filmed for 28 days. The videos were analyzed using the R package BEMOVI, which is based on ImageJ, by tracking particles and excluding those that couldn't have been protists. The repository includes data files obtained from the video analysis and code used for data analysis. The flow for data analysis can be found in the master r markdown file and its associated code chunks (referred to as "children" in r markdown). This data will be used for the upcoming publication: *Ecosystem size mediates the effects of resource flows on species diversity and ecosystem function at different scales.* This repository is structured with the following folders:

-   1_data: Data files obtained from analysing the culture videos with the R package BEMOVI
-   2_analysis: R files to analyse the data 
-   3_results: Results saved during the analysis
-   4_other: Other files