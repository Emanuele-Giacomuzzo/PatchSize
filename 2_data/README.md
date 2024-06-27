# How this data was produced

This folder contains the data obtained from the microcosm experiment. Two lowest threshold levels of pixel intensities were used to differentiate between individuals and the background using ImageJ (herein referred to simply as "thresholds") because different protist species were better identified at different thresholds. Specifically, _Blepharisma sp._, _Cephalodella sp._, and _Spirostomum sp._ were identified with a lower threshold of 40, while _Colpidium sp._, _Euglena gracilis_, _Euplotes aediculatus_, _Loxocephalus sp._, _Paramecium aurelia_, _Paramecium caudatum_, _Spirostomum teres_, and _Tetrahymena cf. pyriformis_ were identified with a lower threshold of 13. Data was obtained from video files through the following steps:

1. __Video Analysis at Threshold 13__: Use the script in 3_r_files > BEMOVI_script_video_analysis to analyze videos at all time points and training data with threshold set to 13 (thresholds <- c(13, 255)) and save the results in 2_data.
2. __Video Analysis at Threshold 40__: Repeat step 1 adjusting the threshold to 40 (thresholds <- c(40, 255)).
3. __Species Identification at Threshold 13__: Use the script in 3_r_files > BEMOVI_script_species_id to identify species for all time points analysed with the threshold set to 13. 
4. __Species Identification at Threshold 40__: Repeat step 3 but with time points analysed with the threshold set to 40.

# Summary of how to assemble the data

During data analysis, we retain the individual and culture data with a threshold of 13 to represent individuals and ecosystem biomass. This threshold does not exclude any protist species, providing a more comprehensive view of these metrics compared to a threshold of 40. Next, we combine the culture data with the species densities. Specifically, data for _Blepharisma sp._, _Cephalodella sp._, and _Spirostomum sp._ from the "species_ID_13_threshold" folder and data for _Colpidium sp._, _Euglena gracilis_, _Euplotes aediculatus_, _Loxocephalus sp._, _Paramecium aurelia_, _Paramecium caudatum_, _Spirostomum teres_, and _Tetrahymena cf. pyriformis_ from the "species_ID_40_threshold" folder.

# Content of the folder

This directory is organised into the following folders and files:

- individuals_13_threshold: Contains data for each individual detected in the videos at each time point, as well as monoculture data used to train a species identification model (training). Videos were analysed with a lowest threshold in ImageJ of 13.
- patches_13_threshold: Contains ecosystem data for each time point and monoculture data used to train a species identification model (training). Videos were analyzed with a lowest threshold in ImageJ of 13.
- species_ID_13_threshold: Contains data for each individual detected in the videos at each time point, with species identified. Videos were analysed with a lowest threshold in ImageJ of 13.
- species_ID_40_threshold: Contains data for each individual detected in the videos at each time point, with species identified. Videos were analysed with a lowest threshold in ImageJ of 40.
- culture_info.csv: Provides information about each ecosystem.
- water_addition: Details the amount of water added to ecosystems to compensate for evaporation.