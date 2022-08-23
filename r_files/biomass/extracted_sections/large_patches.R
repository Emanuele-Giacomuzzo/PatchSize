## ----large-single-patches-plots------------------------------------------------------------------------------------
ds_biomass %>%
  filter(disturbance == "low") %>%
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
       title = "Disturbance = low",
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
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")

ds_biomass %>%
  filter(disturbance == "high") %>%
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
       title = "Disturbance = high",
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
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")


## ----large-boxplots------------------------------------------------------------------------------------------------
ds_biomass %>%
  filter(disturbance == "low") %>%
  filter(patch_size == "L") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day,eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(x = "Day",
       y = "Local bioarea (µm²/μl)",
       title = "Disturbance = low",
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
  geom_vline(xintercept = first_perturbation_day, 
             linetype="dotdash", 
             color = "grey", 
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")

local_large_high_plot = ds_biomass %>%
  filter(disturbance == "high") %>%
  filter(patch_size == "L") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day,eco_metaeco_type),
             fill = eco_metaeco_type)) +
  geom_boxplot() +
  labs(x = "Day",
       y = "Local bioarea (µm²/μl)",
       title = "Disturbance = high",
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
  labs(caption = "Vertical grey line: first perturbation")
local_large_high_plot


## ----large-patches-lnRR-plots--------------------------------------------------------------------------------------
ds_biomass_averaged_treatments %>%
  filter(disturbance == "low") %>%
  filter(eco_metaeco_type == "L (S_L)" | eco_metaeco_type == "L (L_L)") %>%
  ggplot(aes(x = day,
             y = lnRR_biomass,
             color = eco_metaeco_type)) +
  geom_point() +
  geom_line(aes(color = eco_metaeco_type)) +
  labs(title = "Disturbance = low",
       x = "Day",
       y = "LnRR Local Bioarea (µm²/µl)",
       color = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_color_discrete(labels = c("large connected to large",
                                 "large connected to small")) +
  geom_vline(xintercept = first_perturbation_day + 0.7,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")

ds_biomass_averaged_treatments %>%
  filter(disturbance == "high") %>%
  filter(eco_metaeco_type == "L (S_L)" | eco_metaeco_type == "L (L_L)") %>%
  ggplot(aes(x = day,
             y = lnRR_biomass,
             color = eco_metaeco_type)) +
  geom_point() +
  geom_line(aes(color = eco_metaeco_type)) +
  labs(title = "Disturbance = high",
       x = "Day",
       y = "LnRR Local Bioarea (µm²/µl)",
       color = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_color_discrete(labels = c("large connected to large",
                                 "large connected to small")) +
  geom_vline(xintercept = first_perturbation_day + 0.7,
             linetype="dotdash",
             color = "grey",
             size=0.7) +
  labs(caption = "Vertical grey line: first perturbation")


## ----choose-time-points-large--------------------------------------------------------------------------------------
first_time_point = 2
last_time_point = 7


## ----large-patches-full-model--------------------------------------------------------------------------------------
full_model = lm(lnRR_biomass ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * eco_metaeco_type +
                  day * disturbance + 
                  eco_metaeco_type * disturbance,
                  data = ds_biomass_averaged_treatments %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))


## ----large-patches-no-TM-------------------------------------------------------------------------------------------
no_TM = lm(lnRR_biomass ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * disturbance + 
                  eco_metaeco_type * disturbance,
                  data = ds_biomass_averaged_treatments %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))

AIC(full_model, no_TM)


## ----large-patches-no-TD-------------------------------------------------------------------------------------------
no_TD = lm(lnRR_biomass ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * eco_metaeco_type +
                  eco_metaeco_type * disturbance,
                  data = ds_biomass_averaged_treatments %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))

AIC(full_model, no_TD)


## ----large-patches-no-PD-------------------------------------------------------------------------------------------
no_PD = lm(lnRR_biomass ~
                  day + 
                  eco_metaeco_type + 
                  disturbance +
                  day * eco_metaeco_type,
                  data = ds_biomass_averaged_treatments %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))

AIC(no_TD, no_PD)


## ----large-t2-t7-best-model----------------------------------------------------------------------------------------
best_model = no_TD
par(mfrow = c(2,3))
plot(best_model, which = 1:5)


## ------------------------------------------------------------------------------------------------------------------
R2_full = glance(best_model)$r.squared

no_patch_type = lm(lnRR_biomass ~
                  day + 
                  disturbance +
                  day * disturbance,
                  data = ds_biomass_averaged_treatments %>%
                         filter(time_point >= first_time_point) %>%
                         filter(time_point <= last_time_point) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))

R2_no_P = glance(no_patch_type)$r.squared
R2_P = R2_full - R2_no_P

R2_full = round(R2_full, digits = 2)
R2_P = round(R2_P, digits = 2)


## ------------------------------------------------------------------------------------------------------------------
time_point_input = 2


## ------------------------------------------------------------------------------------------------------------------
full_model = lm(lnRR_biomass ~
                  eco_metaeco_type + 
                  disturbance +
                  eco_metaeco_type * disturbance,
                  data = ds_biomass_averaged_treatments %>%
                         filter(time_point == time_point_input) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))


## ------------------------------------------------------------------------------------------------------------------
no_MD = lm(lnRR_biomass ~
                  eco_metaeco_type + 
                  disturbance,
                  data = ds_biomass_averaged_treatments %>%
                         filter(time_point == time_point_input) %>%
                         filter(eco_metaeco_type== "L (L_L)" | 
                                eco_metaeco_type == "L (S_L)"))

AIC(full_model, no_MD)

