# How this data was produced

This folder contains the data obtained from the microcosm experiment. Two lowest threshold levels of pixel intensities were used to differentiate between individuals and the background using ImageJ (herein referred to simply as "thresholds") because different protist species were better identified at different thresholds. Specifically, *Colpidium sp.*, *Euglena gracilis*, *Euplotes aediculatus*, *Loxocephalus sp.*, *Paramecium aurelia*, *Paramecium caudatum*, *Spirostomum teres*, and *Tetrahymena cf. pyriformis* were identified with a lower threshold of 13 while *Blepharisma sp.*, *Cephalodella sp.*, and *Spirostomum sp.* were identified with a lower threshold of 40. Data was obtained from video files through the following steps:

1.  **Video Analysis at Threshold 13**: Use the script in 3_r_files \> BEMOVI_script_video_analysis to analyze videos at all time points and training data with threshold set to 13 (thresholds \<- c(13, 255)) and save the results in 2_data.
2.  **Video Analysis at Threshold 40**: Repeat step 1 adjusting the threshold to 40 (thresholds \<- c(40, 255)).
3.  **Species Identification at Threshold 13**: Use the script in 3_r_files \> BEMOVI_script_species_id to identify species for all time points analysed with the threshold set to 13.
4.  **Species Identification at Threshold 40**: Repeat step 3 but with time points analysed with the threshold set to 40.

# Summary of how to assemble the data

1.  **Assemble a dataset with the individuals.** Keep the individuals from the output with a threshold 13 (individuals_13_threshold). This threshold does not exclude any protist species, providing a more comprehensive view of all the individuals. Bind this data with ecosystem_info.
2.  **Assemble a dataset with the ecosystems.** Keep the biomass and total individuals of each ecosystem from the output with a threshold 13 (ecosystems_13_threshold). This threshold does not exclude any protist species, providing a more comprehensive view of ecosystem biomass and the total number of individuals. Bind it then with the species densities of the species densities of *Colpidium sp.*, *Euglena gracilis*, *Euplotes aediculatus*, *Loxocephalus sp.*, *Paramecium aurelia*, *Paramecium caudatum*, *Spirostomum teres*, and *Tetrahymena cf. pyriformis* obtained from data analysis with threshold set to 13 (species_ID_13_threshold) and *Blepharisma sp.*, *Cephalodella sp.*, and *Spirostomum sp.* obtained from data analysis with threshold set to 40 (species_ID_40_threshold). Bind this data with ecosystem_info.

# Content of the folder

This directory is organised into the following folders and files:

-   individuals_13_threshold: Contains data for each individual detected in the videos at each time point, as well as monoculture data used to train a species identification model (training). Videos were analysed with the threshold set to 13.
-   individuals_40_threshold: Contains data for each individual detected in the videos at each time point, as well as monoculture data used to train a species identification model (training). Videos were analysed with the threshold set to 40.
-   ecosystems_13_threshold: Contains ecosystem data for each time point and monoculture data used to train a species identification model (training). Videos were analysed with the threshold set to 13.
-   ecosystems_40_threshold: Contains ecosystem data for each time point and monoculture data used to train a species identification model (training). Videos were analysed with the threshold set to 40.
-   species_ID_13_threshold: Contains data for each individual detected in the videos at each time point, with species identified. Videos were analysed with the threshold set to 13.
-   species_ID_40_threshold: Contains data for each individual detected in the videos at each time point, with species identified. Videos were analysed with the threshold set to 40.
-   culture_info.csv: Provides information about each ecosystem.
-   water_addition: Details the amount of water (ml) added to ecosystems to compensate for evaporation. Culture ID represents the ID of the ecosystems.

# Ecosystem info

The columns in the ecosystems_info file represent the following (this file uses two synonyms for ecosystem-patch and culture):

culture_ID: ID of the ecosystem\
system_nr: ID of the system (a system is either an unconnected ecosystem or a connected meta-ecosystem; in other words, an unconnected ecosystem has its own system number, while two connected ecosystems share the same system number)\
treatment_replicate: treatment replicate\
disturbance: level of disturbance\
disturbance_volume: volume of the ecosystem that was disturbed (ml)\
patch_size: size of the ecosystem (S = small, M = medium, L = large)\
patch_size_volume: size of the ecosystem (ml) \
metaecosystem: indicates whether the ecosystem is part of a connected meta-ecosystem\
metaecosystem_type: type of connected meta-ecosystem the ecosystem is part of (S_S = small-small, M_M = medium-medium, L_L = large-large, S_L = small-large)\
eco_metaeco_type: ecosystem type followed by the connected meta-ecosystem it is part of in parentheses
