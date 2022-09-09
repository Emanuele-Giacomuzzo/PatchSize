## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")){
  
  print(ds_median_body_size %>%
          filter(disturbance == disturbance_input) %>%
          filter(patch_size == "S") %>%
          group_by(disturbance, 
                   patch_size, 
                   eco_metaeco_type, 
                   culture_ID, 
                   time_point,
                   day) %>%
          summarise(median_body_size = mean(median_body_size)) %>%
          ggplot(aes(x = day,
                     y = median_body_size,
                     group = interaction(day, eco_metaeco_type),
                     fill = eco_metaeco_type)) +
          geom_boxplot()+
          labs(title = paste("Disturbance = ", disturbance_input),
               x = "Day",
               y = "Median body size (µm²)",
               fill = "") +
          scale_fill_discrete(labels = c("small isolated", 
                                         "small connected to large",
                                         "small connected to small")) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.position = c(.40, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          geom_vline(xintercept = first_perturbation_day + 0.7, 
                     linetype = "dotdash", 
                     color = "grey", 
                     size = 0.7) +
        labs(caption = "Vertical grey line: first perturbation"))}


## ----choose-time-points-------------------------------------------------------------------------------------------------------------------------------------------
first_time_point = 2
last_time_point = 7


## ----small-patches-full-model-------------------------------------------------------------------------------------------------------------------------------------
full_model = lm(lnRR_median_body_size ~                  
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * eco_metaeco_type +
                  day * disturbance + 
                  eco_metaeco_type * disturbance,
                  data = ds_lnRR_median_body_size %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"))


## ----small-patches-no-TM------------------------------------------------------------------------------------------------------------------------------------------
no_TP = lm(lnRR_median_body_size ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * disturbance + 
                  eco_metaeco_type * disturbance,
                  data = ds_lnRR_median_body_size %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"))

AIC(full_model, no_TP)


## ----small-patches-no-TD------------------------------------------------------------------------------------------------------------------------------------------
no_TD = lm(lnRR_median_body_size ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  eco_metaeco_type * disturbance,
                  data = ds_lnRR_median_body_size %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"))

AIC(no_TP, no_TD)


## ----small-patches-no-MD------------------------------------------------------------------------------------------------------------------------------------------
no_PD = lm(lnRR_median_body_size ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * disturbance,
                  data = ds_lnRR_median_body_size %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"))

AIC(no_TD, no_PD)


## ----small-t2-t7-best-model---------------------------------------------------------------------------------------------------------------------------------------
best_model = no_PD
par(mfrow = c(2,3))
plot(best_model, which = 1:5)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
R2_full = glance(best_model)$r.squared
no_patch_type = lm(lnRR_median_body_size ~
                  day + 
                  disturbance +
                  day * disturbance,
                  data = ds_lnRR_median_body_size %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "S (S_S)" | 
                                eco_metaeco_type == "S (S_L)"))

R2_no_P = glance(no_patch_type)$r.squared
R2_P = R2_full - R2_no_P

R2_full = round(R2_full, digits = 2)
R2_P = round(R2_P, digits = 2)

