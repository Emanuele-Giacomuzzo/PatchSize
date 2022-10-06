## ----median-body-size-small-patches-single-ecosystem-plots---------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")) {

  print(ds_median_body_size %>%
          filter(disturbance == disturbance_input) %>%
          filter(patch_size == "S") %>%
          ggplot(aes(x = day,
                     y = median_body_size,
                     group = culture_ID,
                     fill = culture_ID,
                     color = culture_ID,
                     linetype = eco_metaeco_type)) +
          geom_line(stat = "summary", fun = "mean") +
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day",
               y = "Median body size (µm²)",
               linetype = "") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = c(.35, .95),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
          scale_linetype_discrete(labels = c("small isolated",
                                             "small connected to large",
                                             "small connected to small"))  +
          geom_vline(xintercept = first_perturbation_day,
                     linetype = "dotdash",
                     color = "grey",
                     size = 0.7) +
          labs(caption = "Vertical grey line: first perturbation"))}


## ----median-body-size-small-patches-boxplots-----------------------------------------------------------------------------------------------------------------------
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


## ----median-body-size-small-patches-lnRR-plots---------------------------------------------------------------------------------------------------------------------
for (disturbance_input in c("low", "high")) {
  
  print(ds_lnRR_median_body_size %>%
          filter(!time_point == 0) %>% #At time point 0 all cultures were the same 
          filter(disturbance == disturbance_input) %>%
          filter(eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
          ggplot(aes(x = day,
                     y = lnRR_median_body_size,
                     color = eco_metaeco_type)) +
          geom_point(position = position_dodge(0.5)) +
          geom_line(position = position_dodge(0.5)) + 
          labs(title = paste("Disturbance =", disturbance_input),
               x = "Day",
               y = "lnRR medain body size (µm²)",
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
          geom_vline(xintercept = first_perturbation_day + 0.7, 
                     linetype="dotdash", 
                     color = "grey", 
                     size=0.7) +
          geom_hline(yintercept = 0, 
                     linetype = "dotted", 
                     color = "black", 
                     size = 0.7) +
          labs(caption = "Vertical grey line: first perturbation"))}


## ----body-size-median-choose-time-points---------------------------------------------------------------------------------------------------------------------------
first_time_point = 2
last_time_point = 7


## ----body-size-median-small-patches-full-model---------------------------------------------------------------------------------------------------------------------
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


## ----body-size-median-small-patches-no-TM--------------------------------------------------------------------------------------------------------------------------
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


## ----body-size-median-small-patches-no-TD--------------------------------------------------------------------------------------------------------------------------
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


## ----body-size-median-small-patches-no-MD--------------------------------------------------------------------------------------------------------------------------
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


## ----body-size-median-small-t2-t7-best-model-----------------------------------------------------------------------------------------------------------------------
best_model = no_PD
par(mfrow = c(2,3))
plot(best_model, which = 1:5)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

