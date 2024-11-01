Files with the individuals that were filmed during the experiment. All the columns have been created during movement analysis using BEMOVI, which is based on ImageJ movement analysis. These columns can be found in the output of imageJ. Each row refers to an individual filmed at a certain time point. Each data file represents a time point (t0 = time point 0, t1 = time point 1, t2 = time point 2, etc.), except for training which refers to the monocultures we filmed before the experiment to train the species identification model. All metrics (e.g., mean, standard deviation) refer to an individual across frames.   

- file: name of the video file 
- mean_grey: mean brightness of the individual
- sd_grey: standard deviation of the brightness of the individual
- mean_area: mean area of the individual (µm²)
- sd_area: standard deviation of the area of the individual (µm²)
- mean_perimeter: mean perimeter length of the individual (µm)
- sd_perimeter: standard deviation of the perimeter length of the individual (µm)
- mean_major: mean length of the major body axis of the individual (µm)
- sd_major: standard deviation of the length of the major body axis of the individual (µm)
- mean_minor: mean length of the minor body axis of the individual (µm)
- sd_minor: standard deviation of the length of the minor body axis of the individual (µm)
- mean_ar: mean aspect ratio of the individual, which is the ratio of the major to minor axis 
- sd_ar: standard deviation of the aspect ratio of the individual
- mean_turning: mean turning angle of the individual
- sd_turning: standard deviation of the turning angle of the individual
- duration: time for which the individual was filmed (s)
- N_frames: number of frames in which the individual was filmed
- max_net: maximum net displacement, which is the furthest distance achieved from the starting point (µm)
- net_disp: net displacement, which is the distance between the initial and final positions (µm)
- net_speed: net speed, which is how quickly an object moves from its starting point to its ending point (µm/s)
- gross_disp: gross displacement, which is the total distance traveled by an object along its actual path (µm)
- gross_speed: gross speed, which is the speed of an object calculated based on the total distance traveled along its actual path (µm/s)
- max_gross_speed: maximum gross speed (µm/s)
- min_gross_speed: minimum gross speed (µm/s)
- max_step: largest single movement an object between two frames (µm)
- min_step: smallest single movement an object between two frames (µm)
- sd_step: standard deviation of the step lengths (distances traveled between consecutive time points or frames) of an object throughout its trajectory (µm)
- sd_gross_speed: standard deviation of the gross speed of an object over a series of time intervals or frames during its movement analysis (µm/s)
- id: identification number for the individual
- date: date on which the video was taken
- time_point: time point filmed
- comment: comment on the culture filmed
- species (present only in training): species filmed to gather data for training the species ID model