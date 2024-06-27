This folder contains the data with the aggregated metrics of all the individuals found in each ecosystem at a certain time point with the density of the species, which was inferred using the species identification model. Each row is an ecosystem at a time point. All metrics (e.g., mean, standard deviation) refer to a metric across individuals. Each data file represents a time point (t0 = time point 0, t1 = time point 1, t2 = time point 2, etc.), except for confusion_matrix. The columns in these datasets represent the following:

- file: name of the video file 
- date: date on which the video was taken
- time_point: time point filmed
- comment: comment on the culture filmed
- indiv_per_frame: individuals per frame (indiv/frame)
- indiv_per_volume: individuals per volume (indiv/µL)
- bioarea_per_frame: bioarea per frame (µm²/frame)
- bioarea_per_volume: bioarea per frame (µm²/µL)
- major_mean: mean length of the major body axis of the filmed individuals (µm)
- major_sd: standard deviation of the length of the major body axis of the filmed individuals (µm)
- minor_mean: mean length of the minor body axis of the filmed individuals (µm)
- minor_sd: standard deviation of the length of the minor body axis of the filmed individuals (µm)
- gross_speed_mean: mean gross speed, which is the speed of an individual calculated based on the total distance traveled along its actual path (µm/s)
- gross_speed_sd: standard deviation of the gross speed, which is the speed of an individual calculated based on the total distance traveled along its actual path (µm/s)
- net_speed_mean: mean net speed, which is how quickly an individual moves from its starting point to its ending point (µm/s)
- net_speed_sd: standard deviation of the net speed (µm/s)
- sd_turning_mean: mean standard deviation of the turning angle of the individual
- species (present only in training): species filmed to gather data for training the species ID model
- Ble: density of _Blepharisma sp._, averaged across frames (indiv/µL)
- Cep: density of _Cephalodella sp._, averaged across frames (indiv/µL)
- Col: density of _Colpidium sp._, averaged across frames (indiv/µL)
- Eug: density of _Euglena gracilis_, averaged across frames (indiv/µL)
- Eup: density of _Euplotes aediculatus_, averaged across frames (indiv/µL)
- Lox: density of _Loxocephalus sp._, averaged across frames (indiv/µL)
- Pau: density of _Paramecium aurelia_, averaged across frames (indiv/µL)
- Pca: density of _Paramecium caudatum_, averaged across frames (indiv/µL)
- Spi: density of _Spirostomum sp._, averaged across frames (indiv/µL)
- Spi_te: density of _Spirostomum teres_, averaged across frames (indiv/µL)
- Tet: density of _Tetrahymena cf. pyriformis_, averaged across frames (indiv/µL)

The confusion matrix represents how often each species is misidentified in the species identification model. The columns in this matrix represent the following:

- Ble:  _Blepharisma sp._ 
- Cep:  _Cephalodella sp._ 
- Col:  _Colpidium sp._ 
- Eug:  _Euglena gracilis_ 
- Eup:  _Euplotes aediculatus_ 
- Lox:  _Loxocephalus sp._ 
- Pau:  _Paramecium aurelia_ 
- Pca:  _Paramecium caudatum_ 
- Spi:  _Spirostomum sp._ 
- Spi_te:  _Spirostomum teres_ 
- Tet:  _Tetrahymena cf. pyriformis_ 
- indiv_misclass_percent: percentage of times in which the individual was misclassified