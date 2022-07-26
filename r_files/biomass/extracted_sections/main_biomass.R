## ----set-up-environment, echo = FALSE, results = FALSE, message = FALSE--------------------------------------------------------------------------------------------------
start_time = Sys.time()

rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("grid")
library("gridExtra") 
library("lme4")
library("here")
library("MuMIn")
library("equatiomatic")
library("knitr")
library("optimx")
library("GenSA")
library("purrr")

knitr::opts_chunk$set(message = FALSE)

source(here("r_files", "biomass", "functions", "biomass_plots.R"))

p = NULL #To run the plotting function


## ----extract-sections, include = FALSE-----------------------------------------------------------------------------------------------------------------------------------

sections.path = here("r_files", "biomass")
r.files = list.files(sections.path)
rmd.files = r.files[grepl(".Rmd", r.files)]
extracted.path = here("r_files", "biomass", "extracted_sections")

purrr::map(rmd.files, function(file.i) {
  
  file.name = gsub(".Rmd", "", file.i)
  extracted.file <- paste0(file.name, ".R")
  knitr::purl(
    file.path(sections.path, file.i),
    file.path(extracted.path, extracted.file)
    )
  
})


## ----import, message = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------------------------------------
culture_info = read.csv(here("data", "PatchSizePilot_culture_info.csv"), header = TRUE)
load(here("data", "population", "t0.RData")); t0 = pop_output
load(here("data", "population", "t1.RData")); t1 = pop_output
load(here("data", "population", "t2.RData")); t2 = pop_output
load(here("data", "population", "t3.RData")); t3 = pop_output
load(here("data", "population", "t4.RData")); t4 = pop_output
load(here("data", "population", "t5.RData")); t5 = pop_output
load(here("data", "population", "t6.RData")); t6 = pop_output
load(here("data", "population", "t7.RData")); t7 = pop_output
rm(pop_output)


## ----child = "tidy.Rmd"--------------------------------------------------------------------------------------------------------------------------------------------------

## ----tidy-up-------------------------------------------------------------------------------------------------------------------------------------------------------------
t0$time = NA
t1$time = NA

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

#Create an elongated version of t0 so that each of the 110 cultures can have 12 video replicates at t0.
elongating_t0 = NULL
for (video in 1:nrow(t0)){
  for (ID in 1:nrow(culture_info)) {
    elongating_t0 = rbind(elongating_t0, t0[video,])
  }
}

ID_vector = rep(1:nrow(culture_info), 
                times = nrow(t0))

elongating_t0$culture_ID = ID_vector

t0 = merge(culture_info,elongating_t0, by="culture_ID")
t1 = merge(culture_info,t1, by = "culture_ID")
t2 = merge(culture_info,t2, by = "culture_ID")
t3 = merge(culture_info,t3, by = "culture_ID")
t4 = merge(culture_info,t4, by = "culture_ID")
t5 = merge(culture_info,t5, by = "culture_ID")
t6 = merge(culture_info,t6, by = "culture_ID")
t7 = merge(culture_info,t7, by = "culture_ID")
ds = rbind(t0, t1, t2, t3, t4, t5, t6, t7)
rm(elongating_t0, t0, t1, t2, t3, t4, t5, t6, t7)

ds$time_point[ds$time_point=="t0"] = 0
ds$time_point[ds$time_point=="t1"] = 1
ds$time_point[ds$time_point=="t2"] = 2
ds$time_point[ds$time_point=="t3"] = 3
ds$time_point[ds$time_point=="t4"] = 4
ds$time_point[ds$time_point=="t5"] = 5
ds$time_point[ds$time_point=="t6"] = 6
ds$time_point[ds$time_point=="t7"] = 7
ds$time_point = as.character(ds$time_point)

ds$day = NA
ds$day[ds$time_point== 0] = 0
ds$day[ds$time_point== 1] = 4
ds$day[ds$time_point== 2] = 8
ds$day[ds$time_point== 3] = 12
ds$day[ds$time_point== 4] = 16
ds$day[ds$time_point== 5] = 20
ds$day[ds$time_point== 6] = 24
ds$day[ds$time_point== 7] = 28

ds = ds %>% 
  select(culture_ID, 
         patch_size, 
         disturbance, 
         metaecosystem_type, 
         bioarea_per_volume, 
         replicate_video, 
         time_point,
         day,
         metaecosystem, 
         system_nr, 
         eco_metaeco_type)

ds = ds[, c("culture_ID", 
            "system_nr", 
            "disturbance", 
            "time_point",
            "day", 
            "patch_size", 
            "metaecosystem", 
            "metaecosystem_type", 
            "eco_metaeco_type", 
            "replicate_video",
            "bioarea_per_volume")]

ds$eco_metaeco_type = factor(ds$eco_metaeco_type, 
                             levels = c('S', 
                                      'S (S_S)', 
                                      'S (S_L)', 
                                      'M', 
                                      'M (M_M)', 
                                      'L', 
                                      'L (L_L)', 
                                      'L (S_L)'))

#ds$patch_size = factor(ds$patch_size,
#                        levels = c ('S', 'M', 'L'))

#ds$metaecosystem_type = factor(ds$metaecosystem_type, 
#levels = c ('S_S', 'M_M', 'L_L', 'S_L'))

ecosystems_to_take_off = 60 #Culture number 60 because it was spilled
ds = ds %>%
  filter(! culture_ID %in% ecosystems_to_take_off)



## ----regional-biomass----------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional = ds %>%
  filter(metaecosystem == "yes") %>%
  group_by(culture_ID, system_nr, disturbance, day, time_point, patch_size, metaecosystem_type) %>%
  summarise(patch_mean_bioarea_across_videos = mean(bioarea_per_volume)) %>%
  group_by(system_nr, disturbance, day, time_point, metaecosystem_type) %>%
  summarise(regional_mean_bioarea = mean(patch_mean_bioarea_across_videos))

metaecosystems_to_take_off = 40 #System 40 was the system of culture 60 that I spilled
ds_regional = ds_regional %>%
  filter(! system_nr %in% metaecosystems_to_take_off)


## ----child = "plots.Rmd", eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------
## #source(here("r_files", "biomass", "extracted_sections", "plots.R"))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional_shrunk_type_n_day = ds_regional %>%
    filter (metaecosystem_type == "M_M" | metaecosystem_type == "S_L", 
            time_point >= 2)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#calculate the standard deviation
ds_regional %>%
  group_by(metaecosystem_type, disturbance, day) %>%
  summarise(mean_bioarea = mean(regional_mean_bioarea), 
            sd_bioarea = sd(regional_mean_bioarea)) %>%
  filter(disturbance == "low") %>%
  filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
  ggplot (aes(x = day,
                y = mean_bioarea,
                fill = metaecosystem_type,
                color = metaecosystem_type)) +
  #geom_point(stat = "summary", fun = "mean") +
  geom_line (stat = "summary", 
             fun = "mean", 
             position=position_dodge(width=0.5)) +
  geom_pointrange(
    aes(ymin = mean_bioarea - sd_bioarea, 
        ymax = mean_bioarea + sd_bioarea),
    position=position_dodge(width=0.5)) +
  labs(x = "Day", 
       y = "Regional biomass (average bioarea between 2 patches/µl)", 
       color='', 
       fill='') +
  scale_fill_discrete(labels = c("Meta-ecosystem with patches of same size", 
                                 "Meta-ecosystem with patches of different size")) +
  scale_color_discrete(labels = c("Meta-ecosystem with patches of same size", 
                                  "Meta-ecosystem with patches of different size")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


## ----child = "no_time_fitting_t2-t7.Rmd", echo = TRUE, eval = TRUE-------------------------------------------------------------------------------------------------------

## ----full-models---------------------------------------------------------------------------------------------------------------------------------------------------------
#Uncorrelated intercepts and slopes
full_model = lmer(regional_mean_bioarea ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day),
                  data = ds_regional_shrunk_type_n_day, 
                  REML = FALSE)

#Correlated intercepts and slopes
full_model_correlated = lmer(regional_mean_bioarea ~ 
                               metaecosystem_type  + 
                               disturbance + 
                               metaecosystem_type * disturbance + 
                               (metaecosystem_type | day) + 
                               (disturbance | day) + 
                               (metaecosystem_type*disturbance  | day), 
                             data = ds_regional_shrunk_type_n_day, 
                             REML = FALSE)

anova(full_model, full_model_correlated)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = full_model


## ----fixed_effects_model-------------------------------------------------------------------------------------------------------------------------------------------------
fixed_effects_model = lm(regional_mean_bioarea ~ 
                           metaecosystem_type  + 
                           disturbance +
                           metaecosystem_type * disturbance, 
                         data = ds_regional_shrunk_type_n_day)

anova(best_model, fixed_effects_model)


## ----no_interaction_model, message=FALSE---------------------------------------------------------------------------------------------------------------------------------
no_interaction_model = lmer(regional_mean_bioarea ~ 
                              metaecosystem_type + 
                              disturbance  + 
                              (metaecosystem_type || day) + 
                              (disturbance || day), 
                            data = ds_regional_shrunk_type_n_day, 
                            REML = FALSE)

anova(best_model, no_interaction_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_interaction_model


## ----no_slopes_model, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------
no_slopes_model = lmer(regional_mean_bioarea ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (1 | day), 
                       data = ds_regional_shrunk_type_n_day, 
                       REML = FALSE)

anova(best_model, no_slopes_model)


## ----best-model----------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_slopes_model


## ----r2-p-best-model-----------------------------------------------------------------------------------------------------------------------------------------------------

model.null = lm(regional_mean_bioarea ~ 1 , 
                  data = ds_regional_shrunk_type_n_day)

r2_best = r.squaredGLMM(best_model)
r2_best = round(r2_best, digits = 3)

anova = anova(best_model, model.null)
p_best = anova$`Pr(>Chisq)`[2]
p_best = round(p_best, digits = 5)

if (p_best < 0.00001) {
  p_best = "< 0.00001"
}



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
metaecosystem_type_null = lmer(regional_mean_bioarea ~  
                                 disturbance  + 
                                 (1 | day),
                               data = ds_regional_shrunk_type_n_day, 
                               REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead"))

r2_no_metaecosystem_type = r.squaredGLMM(metaecosystem_type_null)
r2_no_metaecosystem_type = round(r2_no_metaecosystem_type, digits = 3)

anova = anova(best_model, metaecosystem_type_null)
p_no_metaecosystem_type = anova$`Pr(>Chisq)`[2]
p_no_metaecosystem_type = round(p_no_metaecosystem_type, digits = 5)

if (p_no_metaecosystem_type < 0.00001) {
  p_no_metaecosystem_type = "< 0.00001"
}



## ----p-r2-disturbance----------------------------------------------------------------------------------------------------------------------------------------------------

disturbance_null = lmer(regional_mean_bioarea ~ 
                          metaecosystem_type  + 
                          (1 | day), 
                        data = ds_regional_shrunk_type_n_day, 
                        REML = FALSE,
                        control = lmerControl(optimizer ='optimx', 
                                                     optCtrl=list(method='L-BFGS-B')))

r2_no_disturbance = r.squaredGLMM(disturbance_null)
r2_no_disturbance = round(r2_no_disturbance, digits = 3)

anova = anova(best_model, disturbance_null)
p_no_disturbance = anova$`Pr(>Chisq)`[2]
p_no_disturbance = round(p_no_disturbance, digits = 5)

if (p_no_disturbance < 0.00001) {
  p_no_disturbance = "< 0.00001"
}




## ----child = "no_time_fitted_different_time_periods.Rmd", echo = TRUE, eval = TRUE---------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

R2 = NULL

for (last_point in 3:7) {
  
  filtered_dataset = ds_regional_shrunk_type_n_day %>%
  filter(time_point <= last_point)
  
  full_model = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type +
                            (1 | day),
                        data = filtered_dataset)
  
  no_metaeco_type_model = lm(regional_mean_bioarea ~ 
                            disturbance +
                            (1 | day),
                        data = filtered_dataset)
  
  anova(full_model, no_metaeco_type_model)
  
  R2_full = r.squaredGLMM(full_model)[1,1]
  R2_null = r.squaredGLMM(no_metaeco_type_model)[1,1]
  
  R2[[last_point]] =  R2_full - R2_null
  R2[[last_point]] = round(R2[[last_point]], digits = 3)
  
}



## ----child = "time_fitting.Rmd", echo = TRUE, eval = TRUE----------------------------------------------------------------------------------------------------------------

## ----regional biomass plot-----------------------------------------------------------------------------------------------------------------------------------------------

ds_regional_shrunk_type = ds_regional %>%
    filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L")

ds_regional_shrunk_type%>%
  ggplot(aes(x = day,
             y = regional_mean_bioarea,
             group = day)) + 
  geom_boxplot() +
  labs(x = "day", y = "regional bioarea")


## ----time-function-------------------------------------------------------------------------------------------------------------------------------------------------------

a1 = -0.1
a4 = 1200
a5 = -1

day = seq(0, 30, 0.01)
biomass = a4*(day-a5) * exp(a1*(day-a5))
plot(biomass ~ day)


## ----parameterise-time-function, results= FALSE--------------------------------------------------------------------------------------------------------------------------

model = nls(regional_mean_bioarea ~ a4 * (day-a5) * exp(a1 * (day-a5)), 
            start = list(a1 = -0.1, a4 = 1200, a5 = -1),
            trace = T,
            data = ds_regional_shrunk_type)

a1 = as.numeric(model$m$getPars()[1])
a4 = as.numeric(model$m$getPars()[2])
a5 = as.numeric(model$m$getPars()[3])


## ----show-fitted-parameters----------------------------------------------------------------------------------------------------------------------------------------------
model$m$getPars()


## ----plot-parameterised-time-function------------------------------------------------------------------------------------------------------------------------------------

day = seq(0,30,1)
predicted = a4*(day-a5)*exp(a1*(day-a5))
data_fitted=data.frame(day=day,regional_mean_bioarea=predicted)

ds_regional_shrunk_type%>%
  ggplot(aes(x = day,
             y = regional_mean_bioarea,
             group = day)) + 
  geom_boxplot() +
  labs(x = "day", y = "regional bioarea") +
  geom_line(data=data_fitted,aes(x = day, y=regional_mean_bioarea),color="red", group = 1)


## ----predicted-ds--------------------------------------------------------------------------------------------------------------------------------------------------------

ds_regional_predicted_shrunk_type = ds_regional %>%
  mutate(predicted_from_time = a4*(day-a5)*exp(a1*(day-a5))) %>%
  filter(metaecosystem_type == "S_L" | metaecosystem_type == "M_M")



## ----child = "time_fitted_t2-t7.Rmd", echo = TRUE, eval = TRUE-----------------------------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_regional_predicted_shrunk_type_n_day = ds_regional_predicted_shrunk_type %>%
  filter(time_point >= 2)


## ----eval = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------
## model_slopes_correlated = lmer(regional_mean_bioarea ~
##                             predicted_from_time +
##                             metaecosystem_type  +
##                             disturbance +
##                             predicted_from_time * metaecosystem_type +
##                             predicted_from_time * disturbance +
##                             metaecosystem_type * disturbance +
##                             predicted_from_time * metaecosystem_type * disturbance +
##                             (predicted_from_time | system_nr),
##                         data = ds_regional_predicted_shrunk_type_n_day,
##                         REML = FALSE)
## 
## model_slopes_uncorrelated = lmer(regional_mean_bioarea ~
##                             predicted_from_time +
##                             metaecosystem_type  +
##                             disturbance +
##                             predicted_from_time * metaecosystem_type +
##                             predicted_from_time * disturbance +
##                             metaecosystem_type * disturbance +
##                             predicted_from_time * metaecosystem_type * disturbance +
##                             (predicted_from_time || system_nr),
##                         data = ds_regional_predicted_shrunk_type_n_day,
##                         REML = FALSE)
## 
## anova(model_slopes_uncorrelated, model_slopes_correlated)


## ----time-fitted-random-effects------------------------------------------------------------------------------------------------------------------------------------------
model_1 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * metaecosystem_type +
                            predicted_from_time * disturbance +
                            metaecosystem_type * disturbance +
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)

model_2 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * disturbance +
                            metaecosystem_type * disturbance +
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)

anova(model_1, model_2)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_3 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            metaecosystem_type * disturbance +
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)

anova(model_2, model_3)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_4 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            predicted_from_time * metaecosystem_type * disturbance +
                            (predicted_from_time || system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)

anova(model_3, model_4)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_5 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            (predicted_from_time || system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)

anova(model_4, model_5)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Take off disturbance slopes
model_8 = lmer(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance + 
                            (1 | system_nr),
                        data = ds_regional_predicted_shrunk_type_n_day,
                        REML = FALSE)

anova(model_5, model_8)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Take off random effects
model_9 = lm(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            metaecosystem_type  + 
                            disturbance,
                        data = ds_regional_predicted_shrunk_type_n_day)

anova(model_8, model_9)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

best_model = model_9

summary(best_model)
#R squared: 0.795

#Take off metaecosystem type
best_model_without_metaeco_type = lm(regional_mean_bioarea ~ 
                            predicted_from_time + 
                            disturbance,
                        data = ds_regional_predicted_shrunk_type_n_day)

anova(best_model, best_model_without_metaeco_type)
#they are different.
summary(best_model_without_metaeco_type)
#R squared: 0.718

R2_metaecosystem = summary(best_model)$adj.r.squared - summary(best_model_without_metaeco_type)$adj.r.squared
R2_metaecosystem = round(R2_metaecosystem, digits = 3)



## ----child = "time_fitted_each_time_point.Rmd", echo = TRUE, eval = TRUE-------------------------------------------------------------------------------------------------

## ----day-8---------------------------------------------------------------------------------------------------------------------------------------------------------------

time_point_input = 2

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_8 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_8 = round(R2_day_8, digits = 3)


time_point_input = 3

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_12 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_12 = round(R2_day_12, digits = 3)


time_point_input = 4

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_16 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_16 = round(R2_day_16, digits = 3)




time_point_input = 5

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_20 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_20 = round(R2_day_20, digits = 3)



time_point_input = 6

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_24 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_24 = round(R2_day_24, digits = 3)



time_point_input = 7

filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point == time_point_input)

model_1 = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)

model_2 = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)

anova(model_1, model_2)

summary(model_1)
summary(model_2)
R2_day_28 = summary(model_1)$adj.r.squared - summary(model_2)$adj.r.squared
R2_day_28 = round(R2_day_28, digits = 3)



## ----child = "time_fitted_different_time_periods.Rmd", echo = TRUE, eval = TRUE------------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

R2 = NULL

for (last_point in 3:7) {
  
  filtered_dataset = ds_regional_predicted_shrunk_type %>%
  filter(time_point <= last_point)
  
  full_model = lm(regional_mean_bioarea ~ 
                            disturbance +
                            metaecosystem_type,
                        data = filtered_dataset)
  
  no_metaeco_type_model = lm(regional_mean_bioarea ~ 
                            disturbance,
                        data = filtered_dataset)
  
  anova(full_model, no_metaeco_type_model)
  
  summary(full_model)
  summary(no_metaeco_type_model)
  R2[[last_point]] = summary(full_model)$adj.r.squared - summary(no_metaeco_type_model)$adj.r.squared
  R2[[last_point]] = round(R2[[last_point]], digits = 3)
  
}



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds %>%
    filter (  eco_metaeco_type == "S" | 
              eco_metaeco_type == "S (S_S)" |
              eco_metaeco_type == "S (S_L)") %>%
    ggplot(aes(x = day,
               y = bioarea_per_volume,
               fill = eco_metaeco_type,
               color = eco_metaeco_type)) +
    geom_point(stat = "summary", fun = "mean") +
    geom_line(stat = "summary", fun = "mean") +
    labs(x = "Day", 
         y = "Local biomass (bioarea/µl)", 
         color = '', 
         fill = '') +
    scale_fill_discrete(labels = c("Isolated patch", 
                                   "Patch connected to patch of the same size", 
                                   "Patch connected to larger patch")) +
    scale_color_discrete(labels = c("Isolated patch", 
                                    "Patch connected to patch of the same size", 
                                    "Patch connected to larger patch")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


## ----child = "S_S_and_S_L.Rmd"-------------------------------------------------------------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 = ds %>%
  filter (eco_metaeco_type == "S (S_S)" | 
          eco_metaeco_type == "S (S_L)") %>%
  filter(time_point >= 2) #Let's take off the first two time points which are before the first disturbance event.


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 %>%
    ggplot(aes(x = day,
               y = bioarea_per_volume,
               fill = eco_metaeco_type,
               color = eco_metaeco_type)) +
    geom_point(stat = "summary", fun = "mean") +
    geom_line(stat = "summary", fun = "mean") +
    labs(x = "Day", 
         y = "Local biomass (bioarea/µl)", 
         color = '', 
         fill = '') +
    scale_fill_discrete(labels = c("Patch connected to patch of the same size", 
                                   "Patch connected to larger patch")) +
    scale_color_discrete(labels = c("Patch connected to patch of the same size", 
                                    "Patch connected to larger patch")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_model = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fixed_effects_model = lm(bioarea_per_volume ~ 
                           metaecosystem_type  + 
                           disturbance +
                           metaecosystem_type * disturbance, 
                         data = ds_local_S_t2_t7)

anova(full_model, fixed_effects_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = full_model


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_interaction_model = lmer(bioarea_per_volume ~ 
                              metaecosystem_type + 
                              disturbance  + 
                              (metaecosystem_type || day) + 
                              (disturbance || day), 
                            data = ds_local_S_t2_t7, 
                            REML = FALSE)

anova(best_model, no_interaction_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_interaction_model


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_metaeco_slopes_model = lmer(bioarea_per_volume ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (disturbance || day), 
                       data = ds_local_S_t2_t7, 
                       REML = FALSE,
                       control = lmerControl (optimizer = "Nelder_Mead")
                       )

anova(best_model, no_metaeco_slopes_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_disturbance_slopes_model = lmer(bioarea_per_volume ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (1 | day), 
                       data = ds_local_S_t2_t7, 
                       REML = FALSE,
                       control = lmerControl (optimizer = "Nelder_Mead")
                       )

anova(best_model, no_disturbance_slopes_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_disturbance_slopes_model
summary(best_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model.null = lm(bioarea_per_volume ~ 
                  1, 
                  data = ds_local_S_t2_t7)

r2_best = r.squaredGLMM(best_model)
r2_best = round(r2_best, digits = 3)

anova = anova(best_model, model.null)
p_best = anova$`Pr(>Chisq)`[2]
p_best = round(p_best, digits = 5)

if (p_best < 0.00001) {
  p_best = "< 0.00001"
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
metaecosystem_type_null = lmer(bioarea_per_volume ~  
                                 disturbance  + 
                                 (1 | day),
                               data = ds_local_S_t2_t7, 
                               REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead"))

r2_no_metaecosystem_type = r.squaredGLMM(metaecosystem_type_null)
r2_no_metaecosystem_type = round(r2_no_metaecosystem_type, digits = 3)

anova = anova(best_model, metaecosystem_type_null)
p_no_metaecosystem_type = anova$`Pr(>Chisq)`[2]
p_no_metaecosystem_type = round(p_no_metaecosystem_type, digits = 5)

if (p_no_metaecosystem_type < 0.00001) {
  p_no_metaecosystem_type = "< 0.00001"
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
disturbance_null = lmer(bioarea_per_volume ~ 
                          metaecosystem_type  + 
                          (1 | day), 
                        data = ds_local_S_t2_t7, 
                        REML = FALSE,
                        control = lmerControl(optimizer ='optimx', 
                                                     optCtrl=list(method='L-BFGS-B')))

r2_no_disturbance = r.squaredGLMM(disturbance_null)
r2_no_disturbance = round(r2_no_disturbance, digits = 3)

anova = anova(best_model, disturbance_null)
p_no_disturbance = anova$`Pr(>Chisq)`[2]
p_no_disturbance = round(p_no_disturbance, digits = 5)

if (p_no_disturbance < 0.00001) {
  p_no_disturbance = "< 0.00001"
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
R2 = NULL
for (last_point in 3:7) {
  
  filtered_dataset = ds_local_S_t2_t7 %>%
  filter(time_point <= last_point)
  
  full_model = lmer(bioarea_per_volume ~ 
                      disturbance +
                      metaecosystem_type + 
                      (1 | day),
                    data = filtered_dataset,
                    REML = FALSE)
  
  no_metaeco_type_model = lmer(bioarea_per_volume ~ 
                               disturbance +
                               (1 | day),
                             data = filtered_dataset,
                             REML = FALSE)
  
  anova(full_model, no_metaeco_type_model)
  
  R2_full = r.squaredGLMM(full_model)[1,1]
  R2_null = r.squaredGLMM(no_metaeco_type_model)[1,1]
  
  R2[[last_point]] =  R2_full - R2_null
  R2[[last_point]] = round(R2[[last_point]], digits = 3)
  
}



## ----child = "S_and_S-S_L.Rmd"-------------------------------------------------------------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 = ds %>%
  filter (eco_metaeco_type == "S" | 
          eco_metaeco_type == "S (S_L)") %>%
  filter(time_point >= 2) #Let's take off the first two time points which are before the first disturbance event.


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 %>%
    ggplot(aes(x = day,
               y = bioarea_per_volume,
               fill = eco_metaeco_type,
               color = eco_metaeco_type)) +
    geom_point(stat = "summary", fun = "mean") +
    geom_line(stat = "summary", fun = "mean") +
    labs(x = "Day", 
         y = "Local biomass (bioarea/µl)", 
         color = '', 
         fill = '') +
    scale_fill_discrete(labels = c("Isolated patch", 
                                   "Patch connected to larger patch")) +
    scale_color_discrete(labels = c("Isolated patch", 
                                    "Patch connected to larger patch")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_model = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fixed_effects_model = lm(bioarea_per_volume ~ 
                           metaecosystem_type  + 
                           disturbance +
                           metaecosystem_type * disturbance, 
                         data = ds_local_S_t2_t7)

anova(full_model, fixed_effects_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = full_model


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_interaction_model = lmer(bioarea_per_volume ~ 
                              metaecosystem_type + 
                              disturbance  + 
                              (metaecosystem_type || day) + 
                              (disturbance || day), 
                            data = ds_local_S_t2_t7, 
                            REML = FALSE)

anova(best_model, no_interaction_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_interaction_model


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_metaeco_slopes_model = lmer(bioarea_per_volume ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (disturbance || day), 
                       data = ds_local_S_t2_t7, 
                       REML = FALSE,
                       control = lmerControl (optimizer = "Nelder_Mead")
                       )

anova(best_model, no_metaeco_slopes_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_disturbance_slopes_model = lmer(bioarea_per_volume ~ 
                         metaecosystem_type + 
                         disturbance  + 
                         (1 | day), 
                       data = ds_local_S_t2_t7, 
                       REML = FALSE,
                       control = lmerControl (optimizer = "Nelder_Mead")
                       )

anova(best_model, no_disturbance_slopes_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_model = no_disturbance_slopes_model
summary(best_model)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model.null = lm(bioarea_per_volume ~ 
                  1, 
                  data = ds_local_S_t2_t7)

r2_best = r.squaredGLMM(best_model)
r2_best = round(r2_best, digits = 3)

anova = anova(best_model, model.null)
p_best = anova$`Pr(>Chisq)`[2]
p_best = round(p_best, digits = 5)

if (p_best < 0.00001) {
  p_best = "< 0.00001"
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
metaecosystem_type_null = lmer(bioarea_per_volume ~  
                                 disturbance  + 
                                 (1 | day),
                               data = ds_local_S_t2_t7, 
                               REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead"))

r2_no_metaecosystem_type = r.squaredGLMM(metaecosystem_type_null)
r2_no_metaecosystem_type = round(r2_no_metaecosystem_type, digits = 3)

anova = anova(best_model, metaecosystem_type_null)
p_no_metaecosystem_type = anova$`Pr(>Chisq)`[2]
p_no_metaecosystem_type = round(p_no_metaecosystem_type, digits = 5)

if (p_no_metaecosystem_type < 0.00001) {
  p_no_metaecosystem_type = "< 0.00001"
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
disturbance_null = lmer(bioarea_per_volume ~ 
                          metaecosystem_type  + 
                          (1 | day), 
                        data = ds_local_S_t2_t7, 
                        REML = FALSE,
                        control = lmerControl(optimizer ='optimx', 
                                                     optCtrl=list(method='L-BFGS-B')))

r2_no_disturbance = r.squaredGLMM(disturbance_null)
r2_no_disturbance = round(r2_no_disturbance, digits = 3)

anova = anova(best_model, disturbance_null)
p_no_disturbance = anova$`Pr(>Chisq)`[2]
p_no_disturbance = round(p_no_disturbance, digits = 5)

if (p_no_disturbance < 0.00001) {
  p_no_disturbance = "< 0.00001"
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
R2 = NULL
for (last_point in 3:7) {
  
  filtered_dataset = ds_local_S_t2_t7 %>%
  filter(time_point <= last_point)
  
  full_model = lmer(bioarea_per_volume ~ 
                      disturbance +
                      metaecosystem_type + 
                      (1 | day),
                    data = filtered_dataset,
                    REML = FALSE)
  
  no_metaeco_type_model = lmer(bioarea_per_volume ~ 
                               disturbance +
                               (1 | day),
                             data = filtered_dataset,
                             REML = FALSE)
  
  anova(full_model, no_metaeco_type_model)
  
  R2_full = r.squaredGLMM(full_model)[1,1]
  R2_null = r.squaredGLMM(no_metaeco_type_model)[1,1]
  
  R2[[last_point]] =  R2_full - R2_null
  R2[[last_point]] = round(R2[[last_point]], digits = 3)
  
}



## ----child = "S_and_S-S_S.Rmd"-------------------------------------------------------------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 = ds %>%
  filter (eco_metaeco_type == "S" | 
          eco_metaeco_type == "S (S_S)") %>%
  filter(time_point >= 2) #Let's take off the first two time points which are before the first disturbance event.


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 %>%
    ggplot(aes(x = day,
               y = bioarea_per_volume,
               fill = eco_metaeco_type,
               color = eco_metaeco_type)) +
    geom_point(stat = "summary", fun = "mean") +
    geom_line(stat = "summary", fun = "mean") +
    labs(x = "Day", 
         y = "Local biomass (bioarea/µl)", 
         color = '', 
         fill = '') +
    scale_fill_discrete(labels = c("Isolated patch", 
                                   "Patch connected to patch of the same size")) +
    scale_color_discrete(labels = c("Isolated patch", 
                                    "Patch connected to patch of the same size")) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_model = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------

no_metaeco_type_model = lmer(bioarea_per_volume ~ 
                    disturbance + 
                    (disturbance || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)

anova(full_model, no_metaeco_type_model)



## ----child = "L_and_L-L_L.Rmd"-------------------------------------------------------------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 = ds %>%
  filter (eco_metaeco_type == "L" | 
          eco_metaeco_type == "L (L_L)") %>%
  filter(time_point >= 2) #Let's take off the first two time points which are before the first disturbance event.


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 %>%
    ggplot(aes(x = day,
               y = bioarea_per_volume,
               group= interaction(day, eco_metaeco_type),
               fill = eco_metaeco_type,
               color = eco_metaeco_type)) +
  geom_boxplot() + 
  labs(x = "Day", 
       y = "Local biomass (bioarea/µl)", 
       color = '', 
       fill = '') +
  scale_fill_discrete(labels = c("Isolated patch (large)", 
                                 "Patch connected to patch of the same size (large)")) +
  scale_color_discrete(labels = c("Isolated patch (large)", 
                                  "Patch connected to patch of the same size (large)")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_model = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_metaeco_type_model = lmer(bioarea_per_volume ~ 
                    disturbance + 
                    (disturbance || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)

anova(full_model, no_metaeco_type_model)



## ----child = "L_and_L-S_L.Rmd"-------------------------------------------------------------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 = ds %>%
  filter (eco_metaeco_type == "L" | 
          eco_metaeco_type == "L (S_L)") %>%
  filter(time_point >= 2) #Let's take off the first two time points which are before the first disturbance event.


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ds_local_S_t2_t7 %>%
    ggplot(aes(x = day,
               y = bioarea_per_volume,
               group= interaction(day, eco_metaeco_type),
               fill = eco_metaeco_type,
               color = eco_metaeco_type)) +
  geom_boxplot() + 
  labs(x = "Day", 
       y = "Local biomass (bioarea/µl)", 
       color = '', 
       fill = '') +
  scale_fill_discrete(labels = c("Isolated patch", 
                                 "Patch connected to smaller patch")) +
  scale_color_discrete(labels = c("Isolated patch", 
                                  "Patch connected to smaller patch")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_model = lmer(bioarea_per_volume ~ 
                    metaecosystem_type  + 
                    disturbance + 
                    metaecosystem_type * disturbance + 
                    (metaecosystem_type || day) + 
                    (disturbance || day) + 
                    (metaecosystem_type*disturbance  || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_metaeco_type_model = lmer(bioarea_per_volume ~ 
                    disturbance + 
                    (disturbance || day),
                  data = ds_local_S_t2_t7, 
                  REML = FALSE)

anova(full_model, no_metaeco_type_model)



## ----running-time, echo = FALSE, eval = FALSE----------------------------------------------------------------------------------------------------------------------------
## end_time = Sys.time()
## print(end_time - start_time) #For some reason it doesn't work

