rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("grid")
library("gridExtra") 
library("lme4")

############################ --- LOAD THE DATA, THEN MODIFY THEM --- #################################################

#LOAD THE DATA
culture_info = read.csv("/Users/ema/github/PatchSizePilot/data/PatchSizePilot_culture_info.csv", header = TRUE)
load("~/github/PatchSizePilot/data/population/t0.RData");t0 = pop_output
load("~/github/PatchSizePilot/data/population/t1.RData"); t1 = pop_output
load("~/github/PatchSizePilot/data/population/t2.RData"); t2 = pop_output
load("~/github/PatchSizePilot/data/population/t3.RData"); t3 = pop_output
load("~/github/PatchSizePilot/data/population/t4.RData"); t4 = pop_output
load("~/github/PatchSizePilot/data/population/t5.RData"); t5 = pop_output
load("~/github/PatchSizePilot/data/population/t6.RData"); t6 = pop_output
load("~/github/PatchSizePilot/data/population/t7.RData"); t7 = pop_output
rm(pop_output)

#NUMBER VIDEO REPLICATES
t0$time = NA; t1$time = NA
t6$replicate_video = t6$replicate; t6$replicate = NULL
t7$replicate_video = t7$replicate; t7$replicate = NULL
t0$replicate_video = 1:12
t1$replicate_video = 1
t2$replicate_video = 1
t3$replicate_video = 1
t4$replicate_video = 1
t5$replicate_video = 1

#CREATE AN ELONGATED VERSION OF T0 (SO THAT EACH OF THE 110 CULTURES CAN HAVE 12 VIDEO REPLICATES AT T0)
elongating_t0 = NULL
for (video in 1:nrow(t0)){
  for (ID in 1:nrow(culture_info)) {
    elongating_t0 = rbind(elongating_t0, t0[video,])
  }
}

ID_vector = rep(1:nrow(culture_info), times = nrow(t0))
elongating_t0$culture_ID = ID_vector

#MERGE TIME POINTS POPULATION DATA WITH INFORMATION ABOUT EACH CULTURE, THEN MERGE TIME POINTS
t0 = merge(culture_info,elongating_t0, by="culture_ID")
t1 = merge(culture_info,t1,by="culture_ID")
t2 = merge(culture_info,t2,by="culture_ID")
t3 = merge(culture_info,t3,by="culture_ID")
t4 = merge(culture_info,t4,by="culture_ID")
t5 = merge(culture_info,t5,by="culture_ID")
t6 = merge(culture_info,t6,by="culture_ID")
t7 = merge(culture_info,t7,by="culture_ID")
ds = rbind(t0, t1, t2, t3, t4, t5, t6, t7); rm(elongating_t0, t0, t1, t2, t3, t4, t5, t6, t7)

#SWITCH FROM DATA POINT TO DAY
ds$day = ds$time_point; ds$time_point = NULL
ds$day[ds$day=="t0"] = "0"
ds$day[ds$day=="t1"] = "4"
ds$day[ds$day=="t2"] = "8"
ds$day[ds$day=="t3"] = "12"
ds$day[ds$day=="t4"] = "16"
ds$day[ds$day=="t5"] = "20"
ds$day[ds$day=="t6"] = "24"
ds$day[ds$day=="t7"] = "28"
ds$day = as.numeric(ds$day)

#REORDER THE LEVELS OF ECO_METAECO_TYPE
ds$eco_metaeco_type = factor(ds$eco_metaeco_type, levels=c('S', 'S (S_S)', 'S (S_L)', 'M', 'M (M_M)', 'L', 'L (L_L)', 'L (S_L)'))

#SELECT USEFUL COLUMNS AND REORDER THEM
ds = ds %>% 
  select(culture_ID, patch_size, disturbance, metaecosystem_type, bioarea_per_volume, replicate_video, day, metaecosystem, system_nr, eco_metaeco_type)
ds = ds[, c("culture_ID", "system_nr", "disturbance", "day", "patch_size", "metaecosystem", "metaecosystem_type", "eco_metaeco_type", "replicate_video","bioarea_per_volume")]

#CREATE THE PLOT LIST






#################### --- MIXED EFFECT MODEL --- #######################

#CREATE DATASET WITH REGIONAL BIOMASS
ds_regional = ds %>%
    filter (metaecosystem == "yes", day != 0) %>%
    group_by(culture_ID, system_nr, disturbance, day, patch_size, metaecosystem_type) %>%
    summarise(patch_mean_bioarea_across_videos = mean(bioarea_per_volume)) %>%
    group_by(system_nr, disturbance, day,metaecosystem_type) %>%
    summarise(regional_mean_bioarea = mean(patch_mean_bioarea_across_videos)) %>%
    filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L")

mixed.model.2 = lmer(regional_mean_bioarea ~ metaecosystem_type + disturbance  + (metaecosystem_type || day) + (disturbance || day), 
                   data = ds_regional, REML = FALSE)
summary(mixed.model.2)

mixed.model.3 = lmer(regional_mean_bioarea ~ metaecosystem_type  + (1 | day), 
                     data = ds_regional, REML = FALSE)
summary(mixed.model.3)

mixed.model.4 = lmer(regional_mean_bioarea ~ metaecosystem_type  + disturbance + metaecosystem_type * disturbance + (1| day),
                           data = ds_regional, REML = FALSE)
summary(mixed.model.4)

mixed.model.largest = lmer(regional_mean_bioarea ~ metaecosystem_type  + disturbance + metaecosystem_type * disturbance + (metaecosystem_type || day) + (disturbance || day) + (metaecosystem_type*disturbance  || day),
                     data = ds_regional, REML = FALSE)


anova(mixed.model.1, mixed.model.largest)




### --- WIHT HANK FITTING DISTRIBUTIONS --- ####

# 
# ds_regional = ds_regional %>%
#   filter(system_nr == 56)
# 
# attach(ds_regional)
# model = nls(regional_mean_bioarea ~ a*exp(-1*(day-b)^2/c),start = list(a=2000,b=5,c=200),trace = T)
# model = nls()
# 
# ds_regional %>%
#   ggplot (aes(x = day,
#               y = regional_mean_bioarea)) +
#   labs(x = "Day", y = "Regional biomass (average bioarea between 2 patches/µl)") +
#   geom_point()
# 
# ds_regional = ds %>%
#   filter (metaecosystem == "yes", day != 0) %>%
#   group_by(culture_ID, system_nr, disturbance, day, patch_size, metaecosystem_type) %>%
#   summarise(patch_mean_bioarea_across_videos = mean(bioarea_per_volume)) %>%
#   group_by(system_nr, disturbance, day,metaecosystem_type) %>%
#   summarise(regional_mean_bioarea = mean(patch_mean_bioarea_across_videos)) %>%
#   filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L") %>%
#   filter(system_nr == 21)
# 
# ds_regional %>%
#   ggplot (aes(x = day,
#               y = regional_mean_bioarea)) +
#   labs(x = "Day", y = "Regional biomass (average bioarea between 2 patches/µl)") +
#   geom_point()
# 
# attach(ds_regional)
# model = nls(regional_mean_bioarea ~ a*exp(-1*(1 + epsilon * (day-mu)/sigma)^(-1/epsilon)), start = list(a=2,epsilon = 1, mu =  1, sigma = 1), trace = T)
# model = nls(regional_mean_bioarea ~ a*exp(-1*(day-b)^2/c^2),start = list(a=200,b=5,c=3),trace = T)






####################### --- PLOT: BIOMASS CLEANED --- ##################################


# p = NULL
# 
# biomass_plots = function(disturbance_input){
# 
# p = NULL
# 
# p[[1]] <<- ds %>%
#   filter ( disturbance == disturbance_input) %>%
#   filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
#   group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
#   summarise(patch_mean_bioarea_across_videos = mean(bioarea_per_volume)) %>%
#   group_by(day,metaecosystem_type, system_nr) %>%
#   summarise(bioarea_per_volume = mean(patch_mean_bioarea_across_videos)) %>%
#   ggplot (aes(x = day,
#               y = bioarea_per_volume,
#               group = interaction(day, metaecosystem_type),
#               fill = metaecosystem_type,
#               color = metaecosystem_type)) +
#   geom_boxplot() +
#   labs(x = "Day", y = "Regional biomass (average bioarea between 2 patches/µl)", color='Meta-ecosystem type', fill='Meta-ecosystem type') +
#   scale_y_continuous(limits = c(0, 6250)) +
#   scale_x_continuous(limits = c(-2, 30)) +
#   scale_fill_discrete(labels = c("Patches of same size", "Patches of different size")) +
#   scale_color_discrete(labels = c("Patches of same size", "Patches of different size")) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# p[[2]] <<- ds %>%
#   filter ( disturbance == disturbance_input) %>%
#   filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
#   group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
#   summarise(patch_mean_bioarea_across_videos = mean(bioarea_per_volume)) %>%
#   group_by(day,metaecosystem_type, system_nr) %>%
#   summarise(bioarea_per_volume = mean(patch_mean_bioarea_across_videos)) %>%
#   ggplot (aes(x = day,
#               y = bioarea_per_volume,
#               fill = metaecosystem_type,
#               color = metaecosystem_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(x = "Day", y = "Regional biomass (average bioarea between 2 patches/µl)", color='Meta-ecosystem type', fill='Meta-ecosystem type') +
#   scale_y_continuous(limits = c(0, 6250)) +
#   scale_x_continuous(limits = c(-2, 30)) +
#   scale_fill_discrete(labels = c("Patches of same size", "Patches of different size")) +
#   scale_color_discrete(labels = c("Patches of same size", "Patches of different size")) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# p[[3]] <<- ds %>%
#   filter(disturbance == disturbance_input)%>%
#   filter(metaecosystem == "no") %>%
#   ggplot(aes(x = day,
#              y = bioarea_per_volume,
#              group = interaction(day, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(x = "Day", y = "Local biomass (bioarea/µl)", color='Patch type', fill='Patch type') +
#   scale_y_continuous(limits=c(0, 6250)) +
#   scale_x_continuous(limits = c(-2, 30)) +
#   scale_fill_discrete(labels = c("Small (closed ecosystem)", "Medium (closed ecosystem)", "Large (closed ecosystem)")) +
#   scale_color_discrete(labels = c("Small (closed ecosystem)", "Medium (closed ecosystem)", "Large (closed ecosystem)")) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# p[[4]] <<- ds %>%
#   filter(disturbance == disturbance_input)%>%
#   filter(metaecosystem == "no") %>%
#   ggplot(aes(x = day,
#              y = bioarea_per_volume,
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line(stat = "summary", fun = "mean") +
#   labs(x = "Day", y = "Local biomass (bioarea/µl)", color='Patch type', fill='Patch type') +
#   scale_y_continuous(limits=c(0, 6250)) +
#   scale_x_continuous(limits = c(-2, 30)) +
#   scale_fill_discrete(labels = c("Small (closed ecosystem)", "Medium (closed ecosystem)", "Large (closed ecosystem)")) +
#   scale_color_discrete(labels = c("Small (closed ecosystem)", "Medium (closed ecosystem)", "Large (closed ecosystem)")) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# p[[5]] <<- ds %>%
#   filter(disturbance == disturbance_input)%>%
#   filter (eco_metaeco_type == "S" | eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
#   filter (eco_metaeco_type != "S") %>% #If I want to add the S ecosystem patch I can just commment this out
#   ggplot(aes(x = day,
#              y = bioarea_per_volume,
#              group = interaction(day, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(x = "Day", y = "Local biomass (bioarea/µl)", color='Patch type', fill='Patch type') +
#   scale_y_continuous(limits=c(0, 6250)) +
#   scale_x_continuous(limits = c(-2, 30)) +
#   scale_fill_discrete(labels = c("Connected to same sized patch", "Connected to larger patch")) +
#   scale_color_discrete(labels = c("Connected to same sized patch", "Connected to larger patch")) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# p[[6]] <<- ds %>%
#   filter(disturbance == disturbance_input)%>%
#   filter (eco_metaeco_type == "S" | eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
#   filter (eco_metaeco_type != "S") %>% #If I want to add the S ecosystem patch I can just commment this out
#   ggplot(aes(x = day,
#              y = bioarea_per_volume,
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line(stat = "summary", fun = "mean") +
#   labs(x = "Day", y = "Local biomass (bioarea/µl)", color='Patch type', fill='Patch type') +
#   scale_y_continuous(limits=c(0, 6250)) +
#   scale_x_continuous(limits = c(-2, 30)) +
#   scale_fill_discrete(labels = c("Connected to same sized patch", "Connected to larger patch")) +
#   scale_color_discrete(labels = c("Connected to same sized patch", "Connected to larger patch")) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# }
# 
# biomass_plots("low")
# grid = grid.arrange(p[[1]],p[[3]],p[[5]],p[[2]],p[[4]],p[[6]],
#                     ncol=3, nrow=2,
#                     top = textGrob("Low disturbance",gp=gpar(fontsize=20,font=3)))
# #ggsave("/Users/ema/github/PatchSizePilot/results/biomass/Clean_biomass_low.jpg", grid, width = 22, height = 13)
# 
# biomass_plots("high")
# grid = grid.arrange(p[[1]],p[[3]],p[[5]],p[[2]],p[[4]],p[[6]],
#              ncol=3, nrow=2,
#              top = textGrob("High disturbance",gp=gpar(fontsize=20,font=3)))
#ggsave("/Users/ema/github/PatchSizePilot/results/biomass/Clean_biomass_high.jpg", grid, width = 22, height = 13)
# 
# 
# 
# 
# 
# ######################### --- PLOT: BIOMASS DIFFERENCE BETWEEN ECOSYSTEMS --- ###############################
# 
# 
# biomass_difference = data.frame(matrix(ncol = 4, nrow = 0))
# colnames(biomass_difference) = c('day', 'system_nr', 'metaecosystem_type', "biomass_difference")
# ds_temp_2 = biomass_difference
# 
# for (DAY in 1:max(ds$day)) {
#   if (any(ds$day==DAY) == TRUE) {
#     for (SYSTEM_NR in 1:max(ds$system_nr)){
#     ds_temp = ds%>%
#       filter(metaecosystem == "yes") %>%
#       filter(day == DAY, system_nr == SYSTEM_NR)
#     if (dim(ds_temp)[1] != 0) {
#       max = max(ds_temp$bioarea_per_volume)
#       min = min(ds_temp$bioarea_per_volume)
#       ds_temp_2[1,]$day = DAY
#       ds_temp_2[1,]$system_nr = SYSTEM_NR
#       ds_temp_2[1,]$metaecosystem_type = ds_temp[1,]$metaecosystem_type
#       ds_temp_2[1,]$biomass_difference = (max/min)
#       biomass_difference = rbind(biomass_difference, ds_temp_2)
#       }
#     }
#   }
# }
# 
# bio_diff.raw = biomass_difference %>%
#   filter(metaecosystem_type != "S_L") %>%
#   filter(biomass_difference != Inf) %>%
#   ggplot (aes(x= day,
#               y = biomass_difference,
#               group = interaction(day, metaecosystem_type),
#               fill = metaecosystem_type,
#               color = metaecosystem_type)) +
#   geom_boxplot() +
#   geom_hline(yintercept=1,linetype=2) +
#   xlab("Day") +
#   ylab("Biomass ratio between ecosystems (bioarea/volume)") +
#   labs(color='Meta-ecosystem type')  +
#   labs(fill='Meta-ecosystem type') +
#   scale_x_continuous(limits = c(0,30)) +
#   scale_y_continuous(limits = c(0,16), breaks=c(0,2,4,6,8,10,12,14,16))
# 
# 
# bio_diff.average = biomass_difference %>%
#   filter(metaecosystem_type != "S_L") %>%
#   filter(biomass_difference != Inf) %>%
#   ggplot (aes(x= day,
#               y = biomass_difference,
#               fill = metaecosystem_type,
#               color = metaecosystem_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line(stat = "summary", fun = "mean") +
#   geom_hline(yintercept=1,linetype=2) +
#   xlab("Day") +
#   ylab("Biomass ratio between ecosystems (bioarea/volume)")+
#   labs(color='Meta-ecosystem type')  +
#   labs(fill='Meta-ecosystem type') +
#   scale_x_continuous(limits = c(0,30)) +
#   scale_y_continuous(limits = c(0,16), breaks=c(0,2,4,6,8,10,12,14,16))
# 
# 
# grid = grid.arrange(bio_diff.raw, bio_diff.average,
#              ncol=1, nrow=2,
#              top = textGrob("Biomass ratio between ecosystems", gp=gpar(fontsize=20,font=3)))
# #ggsave("/Users/ema/github/PatchSizePilot/results/biomass/Biomass_differences.jpg", grid, width = 22, height = 13)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ####################### --- PLOT: BIOMASS COMPLETE --- ##################################
# 
# biomass_plots_all = function(disturbance_input){
# 
# p = NULL
# 
# p[[1]] <<- ds %>%
#   filter ( disturbance == "low") %>%
#   filter (metaecosystem == "yes") %>%
#   group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
#   summarise(patch_mean_bioarea_across_videos = mean(bioarea_per_volume)) %>%
#   group_by(day,metaecosystem_type, system_nr) %>%
#   summarise(bioarea_per_volume = mean(patch_mean_bioarea_across_videos)) %>%
#   ggplot (aes(x = day,
#               y = bioarea_per_volume,
#               group = interaction (day, metaecosystem_type),
#               fill = metaecosystem_type,
#               color = metaecosystem_type)) +
#   geom_boxplot() +
#   xlab("Day") +
#   ylab("Regional biomass (average bioarea between 2 patches/µl)") +
#   labs(color='Meta-ecosystem type')  +
#   labs(fill='Meta-ecosystem type') +
#   scale_y_continuous(limits = c(0, 6250)) +
#   scale_x_continuous(limits = c(-2, 30))
# 
# p[[2]] <<- ds %>%
#   filter ( disturbance == "low") %>%
#   filter (metaecosystem == "yes") %>%
#   group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
#   summarise(patch_mean_bioarea_across_videos = mean(bioarea_per_volume)) %>%
#   group_by(day,metaecosystem_type, system_nr) %>%
#   summarise(bioarea_per_volume = mean(patch_mean_bioarea_across_videos)) %>%
#   ggplot (aes(x = day,
#               y = bioarea_per_volume,
#               fill = metaecosystem_type,
#               color = metaecosystem_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   xlab("Day") +
#   ylab("Regional biomass (average bioarea between 2 patches/µl)") +
#   labs(color='Meta-ecosystem type')  +
#   labs(fill='Meta-ecosystem type') +
#   scale_y_continuous(limits = c(0, 6250)) +
#   scale_x_continuous(limits = c(-2, 30))
# 
# p[[3]] <<- ds %>%
#   filter(disturbance == "low")%>%
#   ggplot(aes(x = day,
#              y = bioarea_per_volume,
#              group = interaction(day, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   xlab("Day") +
#   ylab("Local biomass (bioarea/µl)") +
#   labs(color='Patch type')  +
#   labs(fill='Patch type') +
#   scale_y_continuous(limits=c(0, 6250)) +
#   scale_x_continuous(limits = c(-2, 30))
# 
# p[[4]] <<- ds %>%
#   filter(disturbance == "low")%>%
#   ggplot(aes(x = day,
#              y = bioarea_per_volume,
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line(stat = "summary", fun = "mean") +
#   xlab("Day") +
#   ylab("Local biomass (bioarea/µl)") +
#   labs(color='Patch type')  +
#   labs(fill='Patch type') +
#   scale_y_continuous(limits=c(0, 6250)) +
#   scale_x_continuous(limits = c(-2, 30))
# 
# }
# 
# biomass_plots_all("low")
# grid = grid.arrange(p[[1]],p[[3]],p[[2]],p[[4]],
#                     ncol=2, nrow=2,
#                     top = textGrob("Low disturbance",gp=gpar(fontsize=20,font=3)))
# #ggsave("/Users/ema/github/PatchSizePilot/results/biomass/All_biomass_low.jpg", grid, width = 22, height = 13)
# 
# biomass_plots_all("high")
# grid = grid.arrange(p[[1]],p[[3]],p[[2]],p[[4]],
#                     ncol=2, nrow=2,
#                     top = textGrob("High disturbance",gp=gpar(fontsize=20,font=3)))
# #ggsave("/Users/ema/github/PatchSizePilot/results/biomass/All_biomass_high.jpg", grid, width = 22, height = 13)