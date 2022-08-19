## ----message=FALSE, results='hide'--------------------------------------------------------------------------------------------------------------------------
system_nr_to_take_off = 50
n_time_points = 8

isolated_S_and_L = ds_biomass %>%
  filter(!system_nr == system_nr_to_take_off) %>%
  filter(eco_metaeco_type == "S" | eco_metaeco_type == "L") %>%
  group_by(system_nr, disturbance, time_point, day, eco_metaeco_type) %>%
  summarise(bioarea_per_volume_across_videos = mean(bioarea_per_volume))

### --- Low disturbance --- ###

isolated_S_low = isolated_S_and_L %>%
  filter(eco_metaeco_type == "S") %>%
  filter(disturbance == "low")
isolated_L_low = isolated_S_and_L %>%
  filter(eco_metaeco_type == "L") %>%
  filter(disturbance == "low")
  
n_isolated_patches = (nrow(isolated_S_low)) / n_time_points

number_for_pairing_v = rep(1:n_isolated_patches, each = n_time_points)
isolated_S_low$number_for_pairing = number_for_pairing_v  
isolated_L_low$number_for_pairing = number_for_pairing_v 

### --- High disturbance --- ###

isolated_S_high = isolated_S_and_L %>%
  filter(eco_metaeco_type == "S") %>%
  filter(disturbance == "high")
isolated_L_high = isolated_S_and_L %>%
  filter(eco_metaeco_type == "L") %>%
  filter(disturbance == "high")
  
n_isolated_patches = (nrow(isolated_S_high)) / n_time_points
number_for_pairing_v = rep(1:n_isolated_patches, each = n_time_points)
isolated_S_high$number_for_pairing = number_for_pairing_v + 100
isolated_L_high$number_for_pairing = number_for_pairing_v + 100

SL_from_isolated = rbind(isolated_S_low,
                         isolated_L_low,
                         isolated_S_high,
                         isolated_L_high) %>%
  mutate(metaecosystem_type = "S_L_from_isolated") %>%
  rename(regional_mean_bioarea = bioarea_per_volume_across_videos)

ds_regional_with_SL_from_isolated = rbind(SL_from_isolated, ds_regional)


## ----eval = FALSE-------------------------------------------------------------------------------------------------------------------------------------------
## #This would be another way of doing it
## system_nr_S_low = unique(isolated_S$system_nr)[1:5]
## system_nr_L_low = unique(isolated_L$system_nr)[1:5]
## system_nr_S_high = unique(isolated_S$system_nr)[6:9]
## system_nr_L_high = unique(isolated_L$system_nr)[6:9]
## 
## low_pairs = expand.grid(system_nr_S_low,system_nr_L_low)
## high_pairs = expand.grid(system_nr_S_high, system_nr_L_high)
## pairs = rbind(low_pairs, high_pairs)
## number_of_pairs = nrow(pairs)
## 
## 
## SL_from_isolated_all_combinations = NULL
## for (pair in 1:number_of_pairs){
## 
##  SL_from_isolated_one_combination = ds_biomass %>%
##   filter(system_nr %in% pairs[pair,]) %>%
##   group_by(disturbance, day, time_point, system_nr) %>%
##   summarise(regional_bioarea_across_videos = mean(bioarea_per_volume)) %>%
##   group_by(disturbance, day, time_point) %>%
##   summarise(regional_mean_bioarea = mean(regional_bioarea_across_videos)) %>%
##   mutate(system_nr = 1000 + pair) %>%
##    mutate(metaecosystem_type = "S_L_from_isolated")
## 
##  SL_from_isolated_all_combinations = rbind(SL_from_isolated_one_combination,
##                                           SL_from_isolated_all_combinations)
## 
## 
## }
## 
## ds_regional_with_SL_from_isolated = rbind(SL_from_isolated_all_combinations, ds_regional)


## ----message=FALSE, eval = FALSE----------------------------------------------------------------------------------------------------------------------------
## ds_regional_with_SL_from_isolated %>%
##     filter ( disturbance == "low") %>%
##     filter (metaecosystem_type == "S_L" | metaecosystem_type == "S_L_from_isolated") %>%
##     ggplot (aes(x = day,
##                 y = regional_mean_bioarea,
##                 group = system_nr,
##                 fill = system_nr,
##                 color = system_nr,
##                 linetype = metaecosystem_type)) +
##     geom_line () +
##     labs(x = "Day",
##          y = "Regional bioarea (something/µl)",
##          title = "Disturbance = low",
##          fill = "System nr",
##          color = "System nr",
##          linetype = "") +
##     scale_y_continuous(limits = c(0, 6250)) +
##     scale_x_continuous(limits = c(-2, 30)) +
##     scale_linetype_discrete(labels = c("small-large",
##                                      "small-large \n from isolated")) +
##     theme_bw() +
##     theme(panel.grid.major = element_blank(),
##           panel.grid.minor = element_blank(),
##           legend.position = c(.95, .95),
##           legend.justification = c("right", "top"),
##           legend.box.just = "right",
##           legend.margin = margin(6, 6, 6, 6)) +
##   geom_vline(xintercept = first_perturbation_day,
##              linetype="dotdash",
##              color = "grey",
##              size=0.7) +
##   labs(caption = "Vertical grey line: first perturbation")
## 
## ds_regional_with_SL_from_isolated %>%
##     filter ( disturbance == "high") %>%
##     filter (metaecosystem_type == "S_L" | metaecosystem_type == "S_L_from_isolated") %>%
##     ggplot (aes(x = day,
##                 y = regional_mean_bioarea,
##                 group = system_nr,
##                 fill = system_nr,
##                 color = system_nr,
##                 linetype = metaecosystem_type)) +
##     geom_line () +
##     labs(title = "Disturbance = high",
##          x = "Day",
##          y = "Regional bioarea (something/µl)",
##          fill = "System nr",
##          color = "System nr",
##          linetype = "") +
##     scale_y_continuous(limits = c(0, 6250)) +
##     scale_x_continuous(limits = c(-2, 30)) +
##     scale_linetype_discrete(labels = c("small-large",
##                                      "small-large \n from isolated")) +
##     theme_bw() +
##     theme(panel.grid.major = element_blank(),
##           panel.grid.minor = element_blank(),
##           legend.position = c(.95, .95),
##           legend.justification = c("right", "top"),
##           legend.box.just = "right",
##           legend.margin = margin(6, 6, 6, 6)) +
##   geom_vline(xintercept = first_perturbation_day,
##              linetype="dotdash",
##              color = "grey",
##              size=0.7) +
##   labs(caption = "Vertical grey line: first perturbation")
## 
## ds_regional_with_SL_from_isolated %>%
##   filter(disturbance == "low") %>%
##   filter(metaecosystem_type == "S_L" | metaecosystem_type == "S_L_from_isolated") %>%
##   ggplot(aes(x = day,
##              y = regional_mean_bioarea,
##              group = interaction(day, metaecosystem_type),
##              fill = metaecosystem_type)) +
##   geom_boxplot() +
##   labs(title = "Disturbance = low",
##        x = "Day",
##        y = "Regional bioarea (something/microlitre)",
##        fill = "") +
##   scale_fill_discrete(labels = c("small-large", "isolated small & \n isolated large")) +
##   theme_bw() +
##   theme(panel.grid.major = element_blank(),
##         panel.grid.minor = element_blank(),
##         legend.position = c(.95, .95),
##         legend.justification = c("right", "top"),
##         legend.box.just = "right",
##         legend.margin = margin(6, 6, 6, 6)) +
##   geom_vline(xintercept = first_perturbation_day + 0.7,
##              linetype="dotdash",
##              color = "grey",
##              size=0.7) +
##   labs(caption = "Vertical grey line: first perturbation")
## 
## ds_regional_with_SL_from_isolated %>%
##   filter(disturbance == "high") %>%
##   filter(metaecosystem_type == "S_L" | metaecosystem_type == "S_L_from_isolated") %>%
##   ggplot(aes(x = day,
##              y = regional_mean_bioarea,
##              group = interaction(day, metaecosystem_type),
##              fill = metaecosystem_type)) +
##   geom_boxplot() +
##   labs(title = "Disturbance = high",
##        x = "Day",
##        y = "Regional bioarea (something/microlitre)",
##        fill = "") +
##   scale_fill_discrete(labels = c("small-large", "isolated small & \n isolated large")) +
##   theme_bw() +
##   theme(panel.grid.major = element_blank(),
##         panel.grid.minor = element_blank(),
##         legend.position = c(.95, .95),
##         legend.justification = c("right", "top"),
##         legend.box.just = "right",
##         legend.margin = margin(6, 6, 6, 6)) +
##   geom_vline(xintercept = first_perturbation_day + 0.7,
##              linetype="dotdash",
##              color = "grey",
##              size=0.7) +
##   labs(caption = "Vertical grey line: first perturbation")


## ----message = FALSE, eval = FALSE--------------------------------------------------------------------------------------------------------------------------
## mixed_model = lmer(log10(regional_mean_bioarea +1) ~
##                      day * metaecosystem_type * disturbance +
##                      (day | system_nr),
##                    data = ds_regional_with_SL_from_isolated %>%
##                      filter(metaecosystem_type == "S_L" |
##                               metaecosystem_type == "S_L_from_isolated") %>%
##                      filter(time_point >= 2),
##                    REML = FALSE,
##                    control = lmerControl (optimizer = "Nelder_Mead"))
## 
## null_model = lmer(log10(regional_mean_bioarea +1) ~
##                      day * disturbance +
##                      (day | system_nr),
##                    data = ds_regional_with_SL_from_isolated %>%
##                      filter(metaecosystem_type == "S_L" |
##                               metaecosystem_type == "S_L_from_isolated") %>%
##                      filter(time_point >= 2),
##                    REML = FALSE,
##                    control = lmerControl (optimizer = "Nelder_Mead"))
## anova(mixed_model, null_model)

