## ----large-single-patches-plots----------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")) {

  print(ds_biomass_abund %>%
          filter(disturbance == disturbance_input) %>%
          filter(patch_size == "L") %>%
          ggplot(aes(x = day,
                     y = bioarea_per_volume,
                     group = system_nr,
                     fill = system_nr,
                     color = system_nr,
                     linetype = eco_metaeco_type)) +
          geom_line(stat = "summary", fun = "mean") + 
          labs(x = "Day",
               y = "Local bioarea (µm²/μl)",
               title = paste("Disturbance =", disturbance_input),
               linetype = "") +
          theme_bw() +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.position = c(.95, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          scale_linetype_discrete(labels = c("large isolated",
                                             "large connected to large",
                                             "large connected to small")) +
          geom_vline(xintercept = first_perturbation_day, 
                     linetype="dotdash", 
                     color = "grey", 
                     size = 0.7) +
          labs(caption = "Vertical grey line: first perturbation"))}


## ----large-patches-boxplots--------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")){
  
  print(ds_biomass_abund %>%
          filter(disturbance == disturbance_input) %>%
          filter(patch_size == "L") %>%
          ggplot(aes(x = day,
                     y = bioarea_per_volume,
                     group = interaction(day,eco_metaeco_type),
                     fill = eco_metaeco_type)) +
          geom_boxplot() +
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day",
               y = "Local bioarea (µm²/μl)",
               fill = "") +
          theme_bw() +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.position = c(.95, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          scale_fill_discrete(labels = c("large isolated", 
                                         "large connected to large",
                                         "large connected to small")) +
          geom_vline(xintercept = first_perturbation_day + 0.7, 
                     linetype="dotdash", 
                     color = "grey", 
                     size=0.7) +
          labs(caption = "Vertical grey line: first perturbation"))}


## ----large-patches-effect-size-plots-----------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")){

  print(ds_effect_size_bioarea_density %>%
          filter(!time_point == 0) %>% #At time point 0 all cultures were the same 
          filter(disturbance == disturbance_input) %>%
          filter(eco_metaeco_type == "L (L_L)" | eco_metaeco_type == "L (S_L)") %>%
          ggplot(aes(x = day,
                     y = bioarea_density_hedges_d,
                     color = eco_metaeco_type)) +
          geom_point(position = position_dodge(0.5)) +
          geom_line(position = position_dodge(0.5)) + 
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day",
               y = "hedge's d local bioarea",
               color = "") +
          #geom_errorbar(aes(ymin = lnRR_lower, 
          #                  ymax = lnRR_upper), 
          #              width = .2,
          #              position = position_dodge(0.5)) + 
          scale_color_discrete(labels = c("large connected to large", 
                                          "large connnected to small")) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.position = c(.90, .97),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          #  geom_vline(xintercept = first_perturbation_day + 0.7,
          #             linetype="dotdash", 
          #             color = "grey", 
          #             size=0.7) +
          geom_hline(yintercept = 0, 
                     linetype = "dotted", 
                     color = "black", 
                     size = 0.7))}


## ----choose-time-points-large------------------------------------------------------------------------------------------
first_time_point = 2
last_time_point = 7


## ----large-patches-full-model------------------------------------------------------------------------------------------
full_model = lm(bioarea_density_hedges_d ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * eco_metaeco_type +
                  day * disturbance + 
                  eco_metaeco_type * disturbance,
                  data = ds_effect_size_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))


## ----large-patches-no_P------------------------------------------------------------------------------------------------
no_P = lm(bioarea_density_hedges_d ~
                  day + 
                  disturbance +
                  day * eco_metaeco_type +
                  day * disturbance + 
                  eco_metaeco_type * disturbance,
                  data = ds_effect_size_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))

AIC(full_model, no_P)


## ----large-patches-no_D------------------------------------------------------------------------------------------------
no_D = lm(bioarea_density_hedges_d ~
                  day + 
                  eco_metaeco_type + 
                  day * eco_metaeco_type +
                  day * disturbance + 
                  eco_metaeco_type * disturbance,
                  data = ds_effect_size_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))

AIC(full_model, no_D)


## ----large-patches-no-TM-----------------------------------------------------------------------------------------------
no_TP = lm(bioarea_density_hedges_d ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * disturbance + 
                  eco_metaeco_type * disturbance,
                  data = ds_effect_size_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))

AIC(full_model, no_TP)


## ----large-patches-no-TD-----------------------------------------------------------------------------------------------
no_TD = lm(bioarea_density_hedges_d ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  eco_metaeco_type * disturbance,
                  data = ds_effect_size_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))

AIC(no_TP, no_TD)


## ----large-patches-no-PD-----------------------------------------------------------------------------------------------
no_PD = lm(bioarea_density_hedges_d ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * disturbance,
                  data = ds_effect_size_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))

AIC(no_TP, no_PD)


## ----large-t2-t7-best-model--------------------------------------------------------------------------------------------
best_model = no_PD
par(mfrow = c(2,3))
plot(best_model, which = 1:5)


## ----------------------------------------------------------------------------------------------------------------------
R2_full = glance(best_model)$r.squared

no_patch_type = lm(bioarea_density_hedges_d ~
                  day + 
                  disturbance +
                  day * disturbance,
                  data = ds_effect_size_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))

R2_no_P = glance(no_patch_type)$r.squared
R2_P = R2_full - R2_no_P

R2_full = round(R2_full, digits = 2)
R2_P = round(R2_P, digits = 2)

