# How this data was produced

This folder contains the data obtained from the microcosm experiment. Two lowest threshold levels of pixel intensities were used to differentiate between individuals and the background using ImageJ because different protist species were better identified at different thresholds. Specifically, _Blepharisma sp._, _Cephalodella sp._, and _Spirostomum sp._ were identified with a lower threshold of 40, while _Colpidium sp._, _Euglena gracilis_, _Euplotes aediculatus_, _Loxocephalus sp._, _Paramecium aurelia_, _Paramecium caudatum_, _Spirostomum teres_, and _Tetrahymena cf. pyriformis_ were identified with a lower threshold of 13. Data was obtained from video files through the following steps:

1. __Video Analysis at Threshold 13__: Use the script in 3_r_files > BEMOVI_script_video_analysis to analyze videos at all time points and training data with thresholds set to c(13, 255). Save the results in 2_data > individuals and 2_data > populations. This analysis captures all protist individuals for a comprehensive final analysis of individuals and biomass.
2. __Video Analysis at Threshold 40__: Use the same script, adjusting the threshold to c(40, 255), to analyze videos at all time points and training data. This data is only saved locally on the hard disk and is used to get a better understanding of _Blepharisma sp._, _Cephalodella sp._, and _Spirostomum sp._
3. __Species Identification at Threshold 13__: Use the script in 3_r_files > BEMOVI_script_species_id to identify species for all time points analyzed with the threshold of 13, using training data also analyzed at threshold 13. This step helps determine the density of species like _Colpidium sp._, _Euglena gracilis_, _Euplotes aediculatus_, _Loxocephalus sp._, _Paramecium aurelia_, _Paramecium caudatum_, _Spirostomum teres_, and _Tetrahymena cf. pyriformis_ in each ecosystem at each time point.
4. __Species Identification at Threshold 40__: Use the same species identification script for all time points analyzed with the threshold of 40, using training data from threshold 13. This step helps determine the density of _Blepharisma sp._, _Cephalodella sp_., and _Spirostomum sp._ in each ecosystem at each time point.

During data analysis we keep the individual data with threshold 13 to represent individuals. Then, we bind the data from the folder "populations"" with the species densities of _Blepharisma sp._, _Cephalodella sp._, and _Spirostomum sp._ in the folder "species_ID_13_threshold" and _Colpidium sp._, _Euglena gracilis_, _Euplotes aediculatus_, _Loxocephalus sp._, _Paramecium aurelia_, _Paramecium caudatum_, _Spirostomum teres_, and _Tetrahymena cf. pyriformis_ in the folder "species_ID_40_threshold".

# Content of the folder

This directory is organised into the following folders and files:

- individuals: Contains data for each individual detected in the videos at each time point, as well as monoculture data used to train a species identification model (training). Videos were analysed with a lowest threshold in ImageJ of 13.
- populations: Contains ecosystem data for each time point and monoculture data used to train a species identification model (training). Videos were analyzed with a lowest threshold in ImageJ of 13.
- species_ID_13_threshold: Contains data for each individual detected in the videos at each time point, with species identified. Videos were analysed with a lowest threshold in ImageJ of 13.
- species_ID_40_threshold: Contains data for each individual detected in the videos at each time point, with species identified. Videos were analysed with a lowest threshold in ImageJ of 40.
- culture_info.csv: Provides information about each ecosystem.
- water_addition: Details the amount of water added to ecosystems to compensate for evaporation.
