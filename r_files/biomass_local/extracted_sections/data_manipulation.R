## ----experimental-cultures--------------------------------------------------------------------
culture_info = read.csv(here("data", "PatchSizePilot_culture_info.csv"), header = TRUE)


## ---------------------------------------------------------------------------------------------
datatable(culture_info[,1:10],
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))


## ----biomass-import, message = FALSE, echo = TRUE---------------------------------------------
load(here("data", "population", "t0.RData")); t0 = pop_output
load(here("data", "population", "t1.RData")); t1 = pop_output
load(here("data", "population", "t2.RData")); t2 = pop_output
load(here("data", "population", "t3.RData")); t3 = pop_output
load(here("data", "population", "t4.RData")); t4 = pop_output
load(here("data", "population", "t5.RData")); t5 = pop_output
load(here("data", "population", "t6.RData")); t6 = pop_output
load(here("data", "population", "t7.RData")); t7 = pop_output
rm(pop_output)


## ----biomass-tidy-time-points, message = FALSE, echo = TRUE-----------------------------------
#Column: time
t0$time = NA
t1$time = NA

#Column: replicate_video
t0$replicate_video = 1:12 #In t1 I took 12 videos of a single 
t1$replicate_video = 1 #In t1 I took only 1 video/culture
t2$replicate_video = 1 #In t2 I took only 1 video/culture
t3$replicate_video = 1 #In t3 I took only 1 video/culture
t4$replicate_video = 1 #In t4 I took only 1 video/culture
t5$replicate_video = 1 #In t5 I took only 1 video/culture
t6 = t6 %>%
  rename(replicate_video = replicate)
t7 = t7 %>%
  rename(replicate_video = replicate)


## ----biomass-bind-time-points, message = FALSE, echo = TRUE-----------------------------------
#Elongate t0 (so that it can be merged wiht culture_info)
number_of_columns_t0 = ncol(t0)
nr_of_cultures = nrow(culture_info)
nr_of_videos = nrow(t0)

t0 = t0[rep(row.names(t0), nr_of_cultures),] %>%
   arrange(file) %>%
   mutate(culture_ID = rep(1:nr_of_cultures, times = nr_of_videos))

#Merge time points
t0 = merge(culture_info,t0, by="culture_ID")
t1 = merge(culture_info,t1, by = "culture_ID")
t2 = merge(culture_info,t2, by = "culture_ID")
t3 = merge(culture_info,t3, by = "culture_ID")
t4 = merge(culture_info,t4, by = "culture_ID")
t5 = merge(culture_info,t5, by = "culture_ID")
t6 = merge(culture_info,t6, by = "culture_ID")
t7 = merge(culture_info,t7, by = "culture_ID")
ds_biomass = rbind(t0, t1, t2, t3, t4, t5, t6, t7)
rm(t0, t1, t2, t3, t4, t5, t6, t7)


## ----biomass-tidy-columns, message = FALSE, echo = TRUE---------------------------------------
#Take off spilled cultures
ds_biomass = ds_biomass %>%
  filter(! culture_ID %in% ecosystems_to_take_off)

#Column: time_point
ds_biomass$time_point[ds_biomass$time_point=="t0"] = 0
ds_biomass$time_point[ds_biomass$time_point=="t1"] = 1
ds_biomass$time_point[ds_biomass$time_point=="t2"] = 2
ds_biomass$time_point[ds_biomass$time_point=="t3"] = 3
ds_biomass$time_point[ds_biomass$time_point=="t4"] = 4
ds_biomass$time_point[ds_biomass$time_point=="t5"] = 5
ds_biomass$time_point[ds_biomass$time_point=="t6"] = 6
ds_biomass$time_point[ds_biomass$time_point=="t7"] = 7
ds_biomass$time_point = as.character(ds_biomass$time_point)

#Column: day
ds_biomass$day = NA
ds_biomass$day[ds_biomass$time_point== 0] = 0
ds_biomass$day[ds_biomass$time_point== 1] = 4
ds_biomass$day[ds_biomass$time_point== 2] = 8
ds_biomass$day[ds_biomass$time_point== 3] = 12
ds_biomass$day[ds_biomass$time_point== 4] = 16
ds_biomass$day[ds_biomass$time_point== 5] = 20
ds_biomass$day[ds_biomass$time_point== 6] = 24
ds_biomass$day[ds_biomass$time_point== 7] = 28

#Column: size_of_connected_patch
ds_biomass$size_of_connected_patch[ds_biomass$eco_metaeco_type == "S"] = "S"
ds_biomass$size_of_connected_patch[ds_biomass$eco_metaeco_type == "S (S_S)"] = "S"
ds_biomass$size_of_connected_patch[ds_biomass$eco_metaeco_type == "S (S_L)"] = "L"
ds_biomass$size_of_connected_patch[ds_biomass$eco_metaeco_type == "M (M_M)"] = "M"
ds_biomass$size_of_connected_patch[ds_biomass$eco_metaeco_type == "L"] = "L"
ds_biomass$size_of_connected_patch[ds_biomass$eco_metaeco_type == "L (L_L)"] = "L"
ds_biomass$size_of_connected_patch[ds_biomass$eco_metaeco_type == "L (S_L)"] = "S"

#Keep this dataset for the evaporation effects 
ds_for_evaporation = ds_biomass

ds_biomass = ds_biomass %>% 
  select(culture_ID, 
         patch_size,
         patch_size_volume,
         disturbance, 
         metaecosystem_type, 
         bioarea_per_volume, 
         replicate_video, 
         time_point,
         day,
         metaecosystem, 
         system_nr, 
         eco_metaeco_type,
         size_of_connected_patch) %>%
  relocate(culture_ID,
           system_nr,
           disturbance,
           time_point,
           day,
           patch_size,
           patch_size_volume,
           metaecosystem,
           metaecosystem_type,
           eco_metaeco_type,
           size_of_connected_patch,
           replicate_video,
           bioarea_per_volume)


## ---------------------------------------------------------------------------------------------
datatable(ds_biomass,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))


## ----regional-biomass-------------------------------------------------------------------------
ds_regional_biomass = ds_biomass %>%
  filter(metaecosystem == "yes") %>%
  filter(! system_nr %in% metaecosystems_to_take_off) %>%
  group_by(culture_ID, 
           system_nr, 
           disturbance, 
           time_point,
           day, 
           patch_size,
           patch_size_volume,
           metaecosystem_type) %>%
  summarise(bioarea_per_volume_video_averaged = mean(bioarea_per_volume)) %>%
  mutate(total_patch_bioarea = bioarea_per_volume_video_averaged * patch_size_volume) %>%
  group_by(system_nr, 
           disturbance, 
           time_point,
           day,
           metaecosystem_type) %>%
  summarise(total_regional_bioarea = sum(total_patch_bioarea))


## ----create-SL_SL_from_isolated, message=FALSE, results='hide'--------------------------------
isolated_S_and_L = ds_biomass %>%
  filter(eco_metaeco_type == "S" | eco_metaeco_type == "L") %>%
  group_by(system_nr, disturbance, time_point, day, eco_metaeco_type) %>%
  summarise(bioarea_per_volume_across_videos = mean(bioarea_per_volume))

isolated_S_low = isolated_S_and_L %>%
  filter(eco_metaeco_type == "S") %>%
  filter(disturbance == "low")
isolated_L_low = isolated_S_and_L %>%
  filter(eco_metaeco_type == "L") %>%
  filter(disturbance == "low")
isolated_S_high = isolated_S_and_L %>%
  filter(eco_metaeco_type == "S") %>%
  filter(disturbance == "high")
isolated_L_high = isolated_S_and_L %>%
  filter(eco_metaeco_type == "L") %>%
  filter(disturbance == "high")

S_low_system_nrs = unique(isolated_S_low$system_nr)
S_high_system_nrs = unique(isolated_S_high$system_nr)
L_low_system_nrs = unique(isolated_L_low$system_nr)
L_high_system_nrs = unique(isolated_L_high$system_nr)

low_system_nrs_combination = expand.grid(S_low_system_nrs, L_low_system_nrs) %>%
  mutate(disturbance = "low")
high_system_nrs_combination = expand.grid(S_high_system_nrs, L_high_system_nrs) %>%
  mutate(disturbance = "high")
system_nr_combinations = rbind(low_system_nrs_combination, high_system_nrs_combination) %>%
  rename(S_system_nr = Var1) %>%
  rename(L_system_nr = Var2)

number_of_combinations = nrow(system_nr_combinations)
SL_from_isolated_all_combinations = NULL
for (pair in 1:number_of_combinations){
  
  SL_from_isolated_one_combination = 
    ds_biomass %>%
    filter(system_nr %in% system_nr_combinations[pair,]) %>%
    group_by(disturbance, day, time_point, system_nr) %>%
    summarise(regional_bioarea_across_videos = mean(bioarea_per_volume)) %>%
    group_by(disturbance, day, time_point) %>%
    summarise(total_regional_bioarea = sum(regional_bioarea_across_videos)) %>%
    mutate(system_nr = 1000 + pair) %>%
    mutate(metaecosystem_type = "S_L_from_isolated")
  
  SL_from_isolated_all_combinations[[pair]] = SL_from_isolated_one_combination
  
}

SL_from_isolated_all_combinations_together = NULL
for (combination in 1:number_of_combinations){
 
  SL_from_isolated_all_combinations_together = 
    rbind(SL_from_isolated_all_combinations_together,
          SL_from_isolated_all_combinations[[pair]])
  
}

ds_regional_biomass = rbind(ds_regional_biomass, SL_from_isolated_all_combinations_together)


## ---------------------------------------------------------------------------------------------
datatable(ds_regional_biomass,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))


## ----created-averaged-lnRR-ds-----------------------------------------------------------------
ds_biomass_averaged_treatments = data.frame(eco_metaeco_type = character(),
                                         disturbance = character(),
                                         time_point = integer(),
                                         mean_bioarea_per_volume = double())

eco_metaeco_types = unique(ds_biomass$eco_metaeco_type)

for (disturbance_input in c("low", "high")){
  for (eco_metaeco_input in eco_metaeco_types){
    for (time_point_input in 0:7){
      
      temporary = ds_biomass %>%
        filter(eco_metaeco_type == eco_metaeco_input) %>%
        filter(disturbance == disturbance_input) %>%
        filter(time_point == time_point_input) %>% 
        group_by(culture_ID, eco_metaeco_type, patch_size, disturbance, time_point, day) %>%
        summarise(bioarea_per_volume_across_videos = mean(bioarea_per_volume)) %>%
        group_by(eco_metaeco_type, patch_size, disturbance, time_point, day) %>%
        summarise(mean_bioarea_per_volume = mean(bioarea_per_volume_across_videos))
      
      ds_biomass_averaged_treatments = rbind(ds_biomass_averaged_treatments, temporary)
      
    }
  }
}

ds_biomass_averaged_treatments$isolated_control = NA
for (patch_size_input in c("S", "L")){
  for (disturbance_input in c("low", "high")){
    for (time_point_input in 0:7){
      
      averaged_value_isolated_control = ds_biomass_averaged_treatments %>%
        filter(eco_metaeco_type == patch_size_input) %>%
        filter(disturbance == disturbance_input) %>%
        filter(time_point == time_point_input) %>%
        ungroup() %>%
        select(mean_bioarea_per_volume)
      
      ds_biomass_averaged_treatments$isolated_control[
        ds_biomass_averaged_treatments$patch_size == patch_size_input & 
        ds_biomass_averaged_treatments$disturbance == disturbance_input &
        ds_biomass_averaged_treatments$time_point == time_point_input] = 
        averaged_value_isolated_control
    
    }
  }
}

ds_biomass_averaged_treatments = ds_biomass_averaged_treatments %>%
  filter(!patch_size == "M") %>%
  mutate(isolated_control = as.numeric(isolated_control)) %>%
  mutate(lnRR_biomass = ln(mean_bioarea_per_volume / isolated_control))


## ----eval = FALSE-----------------------------------------------------------------------------
## #Takes about 3h to run.
## ds_lnRR = data.frame()
## iterations_n = 1000
## upper_bound = iterations_n * 0.025
## lower_bound = iterations_n * 0.975
## rows_to_subsample = 5
## 
## for (eco_metaeco_type_input in c("S (S_S)", "S (S_L)", "L (L_L)", "L (S_L)")){
##   for (disturbance_input in c("low", "high")){
##     for (time_point_input in 0:7){
## 
##       mean_bioarea_isolated = ds_biomass_averaged_treatments %>%
##                                filter(eco_metaeco_type == eco_metaeco_type_input) %>%
##                                filter(time_point == time_point_input) %>%
##                                filter(disturbance == disturbance_input) %>%
##                                ungroup() %>%
##                                select(isolated_control)
##       mean_bioarea_isolated = unlist(mean_bioarea_isolated)
## 
##       mean_bioarea_all_iterations = NULL
##       for (iteration in 1:iterations_n){
## 
##         mean_bioarea_iteration = ds_biomass %>%
##           filter(eco_metaeco_type == eco_metaeco_type_input) %>%
##           filter(time_point == time_point_input) %>%
##           filter(disturbance == disturbance_input) %>%
##           group_by(culture_ID, system_nr, eco_metaeco_type, disturbance,time_point,day) %>%
##           summarise(mean_bioarea_per_volume_video_averaged = mean(bioarea_per_volume)) %>%
##           group_by(system_nr) %>%
##           slice_sample(n = 1) %>%
##           ungroup() %>%
##           slice_sample(n = rows_to_subsample,
##                        replace = TRUE) %>%
##           summarise(mean_bioarea_per_volume = mean(mean_bioarea_per_volume_video_averaged))
##         mean_bioarea_all_iterations[iteration] = as.numeric(unlist(mean_bioarea_iteration))
## 
##         }
## 
##       mean_bioarea_all_iterations = sort(mean_bioarea_all_iterations, decreasing = TRUE)
## 
##       mean_bioarea_lower_CI =  mean_bioarea_all_iterations[lower_bound]
##       mean_bioarea_density = mean_bioarea_all_iterations[iterations_n/2]
##       mean_bioarea_upper_CI =  mean_bioarea_all_iterations[upper_bound]
## 
##       lnRR_bioarea_lower_CI = ln(mean_bioarea_lower_CI/mean_bioarea_isolated)
##       lnRR_bioarea_density = ln(mean_bioarea_density/mean_bioarea_isolated)
##       lnRR_bioarea_upper_CI = ln(mean_bioarea_upper_CI/mean_bioarea_isolated)
## 
##       new_row = nrow(ds_lnRR) + 1
##       ds_lnRR[new_row,] = NA
##       ds_lnRR$disturbance[new_row] = disturbance_input
##       ds_lnRR$eco_metaeco_type[new_row] = eco_metaeco_type_input
##       ds_lnRR$time_point[new_row] = time_point_input
##       ds_lnRR$day[new_row] = time_point_input*4
##       ds_lnRR$lnRR_lower[new_row] = lnRR_bioarea_lower_CI
##       ds_lnRR$lnRR[new_row] = lnRR_bioarea_density
##       ds_lnRR$lnRR_upper[new_row] = lnRR_bioarea_upper_CI
## 
##     }
##   }
## }
## 
## write.csv(ds_lnRR,
##           file = here("results", "biomass", "bootstrapped_lnRR_patches.csv"),
##           sep = ",",
##           col.names = TRUE)


## ---------------------------------------------------------------------------------------------
ds_lnRR = read.csv(here("results", "biomass", "bootstrapped_lnRR_patches.csv"), header = TRUE)
ds_lnRR = ds_lnRR %>%
  rename(lnRR_bioarea_density = lnRR)


## ---------------------------------------------------------------------------------------------
datatable(ds_lnRR,
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))

