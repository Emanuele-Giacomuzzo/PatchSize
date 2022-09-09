## ----small-patches-single-ecosystems-plots------------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")) {

  print(ds_biomass_abund %>%
          filter(disturbance == disturbance_input) %>%
          filter(patch_size == "S") %>%
          ggplot(aes(x = day,
                     y = bioarea_per_volume,
                     group = culture_ID,
                     fill = culture_ID,
                     color = culture_ID,
                     linetype = eco_metaeco_type)) +
          geom_line(stat = "summary", fun = "mean") +
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day",
               y = "Local bioarea (µm²/μl)",
               linetype = "") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = c(.95, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          scale_linetype_discrete(labels = c("small isolated",
                                             "small connected to small",
                                             "small connected to large"))  +
          geom_vline(xintercept = first_perturbation_day,
                     linetype = "dotdash",
                     color = "grey",
                     size = 0.7) +
          labs(caption = "Vertical grey line: first perturbation"))}


## ----small-patches-boxplots---------------------------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")) {
  
  print(ds_biomass_abund %>%
          filter(disturbance == disturbance_input) %>%
          filter(patch_size == "S") %>%
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
          scale_fill_discrete(labels = c("small isolated",
                                         "small connected to small",
                                         "small connected to large")) +
          geom_vline(xintercept = first_perturbation_day + 0.7,
                     linetype = "dotdash",
                     color = "grey",
                     size = 0.7) +
          labs(caption = "Vertical grey line: first perturbation"))}


## ----small-patches-lnRR-plots-------------------------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")) {
  
  print(ds_lnRR_bioarea_density %>%
          filter(disturbance == disturbance_input) %>%
          filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
          ggplot(aes(x = day,
                     y = lnRR_bioarea_density,
                     color = eco_metaeco_type)) +
          geom_point(position = position_dodge(0.5)) +
          geom_line(position = position_dodge(0.5)) + 
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day",
               y = "lnRR local bioarea (µm²/µl)",
               color = "") +
          #geom_errorbar(aes(ymin = lnRR_lower, 
          #                  ymax = lnRR_upper), 
          #              width = .2,
          #              position = position_dodge(0.5)) + 
          scale_color_discrete(labels = c("small connected to large", 
                                          "small connnected to small")) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.position = c(.40, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          #geom_vline(xintercept = first_perturbation_day + 0.7, 
          #           linetype="dotdash", 
          #           color = "grey", 
          #           size=0.7) +
          geom_hline(yintercept = 0, 
                     linetype = "dotted", 
                     color = "black", 
                     size = 0.7))}


## ----choose-time-points-------------------------------------------------------------------------------------------------------------------------------------------
first_time_point = 2
last_time_point = 7


## ----small-patches-full-model-------------------------------------------------------------------------------------------------------------------------------------
full_model = lm(lnRR_bioarea_density ~                  
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * eco_metaeco_type +
                  day * disturbance + 
                  eco_metaeco_type * disturbance,
                  data = ds_lnRR_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"))


## ----small-patches-no-TM------------------------------------------------------------------------------------------------------------------------------------------
no_TP = lm(lnRR_bioarea_density ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * disturbance + 
                  eco_metaeco_type * disturbance,
                  data = ds_lnRR_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"))

AIC(full_model, no_TP)


## ----small-patches-no-TD------------------------------------------------------------------------------------------------------------------------------------------
no_TD = lm(lnRR_bioarea_density ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * eco_metaeco_type +
                  eco_metaeco_type * disturbance,
                  data = ds_lnRR_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"))

AIC(full_model, no_TD)


## ----small-patches-no-MD------------------------------------------------------------------------------------------------------------------------------------------
no_PD = lm(lnRR_bioarea_density ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * eco_metaeco_type,
                  data = ds_lnRR_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"))

AIC(no_TD, no_PD)


## ----small-t2-t7-best-model---------------------------------------------------------------------------------------------------------------------------------------
best_model = no_PD
par(mfrow = c(2,3))
plot(best_model, which = 1:5)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
R2_full = glance(best_model)$r.squared
no_patch_type = lm(lnRR_bioarea_density ~
                  day + 
                  disturbance,
                  data = ds_lnRR_bioarea_density %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | eco_metaeco_type == "S (S_L)"))

R2_no_P = glance(no_patch_type)$r.squared
R2_P = R2_full - R2_no_P

R2_full = round(R2_full, digits = 2)
R2_P = round(R2_P, digits = 2)

