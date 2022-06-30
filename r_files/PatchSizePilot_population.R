rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("grid")
library("gridExtra") 

############################ --- LOAD THE DATA --- ######################################################


culture_info = read.csv("/Users/ema/github/PatchSizePilot/data/PatchSizePilot_dataset.csv", header = TRUE)
load("~/github/PatchSizePilot/data/population/t0.RData");t0 = pop_output
load("~/github/PatchSizePilot/data/population/t1.RData"); t1 = pop_output
load("~/github/PatchSizePilot/data/population/t2.RData"); t2 = pop_output
load("~/github/PatchSizePilot/data/population/t3.RData"); t3 = pop_output
load("~/github/PatchSizePilot/data/population/t4.RData"); t4 = pop_output
load("~/github/PatchSizePilot/data/population/t5.RData"); t5 = pop_output
load("~/github/PatchSizePilot/data/population/t6.RData"); t6 = pop_output
load("~/github/PatchSizePilot/data/population/t7.RData"); t7 = pop_output
rm(pop_output)















########################## --- MODIFY COLUMN NAME AND CONTENT (t0-t7) --- #######################################


t0$time = "to find out"
t1$time = "to find out"
t6$replicate_video = t6$replicate; t6$replicate = NULL
t7$replicate_video = t7$replicate; t7$replicate = NULL
t0$replicate_video = 1:12
t1$replicate_video = 1
t2$replicate_video = 1
t3$replicate_video = 1
t4$replicate_video = 1
t5$replicate_video = 1















########################## --- MERGE DATASETS --- #######################################


culture_info$culture_ID = culture_info$id; culture_info$id = NULL

elongating_t0 = NULL
for (video in 1:nrow(t0)){
  for (ID in 1:nrow(culture_info)) {
    elongating_t0 = rbind(elongating_t0, t0[video,])
  }
}

ID_vector = rep(1:nrow(culture_info), times = nrow(t0))
elongating_t0$culture_ID = ID_vector

t0 = merge(culture_info,elongating_t0, by="culture_ID")
t1 = merge(culture_info,t1,by="culture_ID")
t2 = merge(culture_info,t2,by="culture_ID")
t3 = merge(culture_info,t3,by="culture_ID")
t4 = merge(culture_info,t4,by="culture_ID")
t5 = merge(culture_info,t5,by="culture_ID")
t6 = merge(culture_info,t6,by="culture_ID")
t7 = merge(culture_info,t7,by="culture_ID")
ds = rbind(t0, t1, t2, t3, t4, t5, t6, t7); rm(elongating_t0, t0, t1, t2, t3, t4, t5, t6, t7)














########################## --- MODIFY COLUMN NAME AND CONTENT (ds) --- #######################################


ds$time_points = NULL
ds$indiv_ml = ds$indiv_per_volume*1000
ds$treatment_replicate = ds$replicate; ds$replicate = NULL
ds$system_nr = ds$metaecosystem; ds$metaecosystem = NULL
ds$day = ds$time_point; ds$time_point = NULL
ds$disturbance_ml = ds$disturbance.1; ds$disturbance.1 = NULL
ds$metaecosystem_type = ds$metaeco; ds$metaeco = NULL
ds$patch_size_ml = ds$patch_size; ds$patch_size = NULL
ds$eco_metaeco_type = ds$ecosystem; ds$ecosystem = NULL

ds$day[ds$day=="t0"] = "0"
ds$day[ds$day=="t1"] = "4"
ds$day[ds$day=="t2"] = "8"
ds$day[ds$day=="t3"] = "12"
ds$day[ds$day=="t4"] = "16"
ds$day[ds$day=="t5"] = "20"
ds$day[ds$day=="t6"] = "24"
ds$day[ds$day=="t7"] = "28"
ds$day = as.numeric(ds$day)

ds$patch_size[ds$patch_size_ml == 7.5] = "S"
ds$patch_size[ds$patch_size_ml == 22.5] = "M"
ds$patch_size[ds$patch_size_ml == 37.5] = "L"

ds$metaecosystem[ds$metaecosystem_type=="L"] = "no"
ds$metaecosystem[ds$metaecosystem_type=="L_L"] = "yes"
ds$metaecosystem[ds$metaecosystem_type=="M"] = "no"
ds$metaecosystem[ds$metaecosystem_type=="M_M"] = "yes"
ds$metaecosystem[ds$metaecosystem_type=="S"] = "no"
ds$metaecosystem[ds$metaecosystem_type=="S_L"] = "yes"
ds$metaecosystem[ds$metaecosystem_type=="S_S"] = "yes"

ds$metaecosystem_type[ds$metaecosystem_type=="S"] = NA
ds$metaecosystem_type[ds$metaecosystem_type=="M"] = NA
ds$metaecosystem_type[ds$metaecosystem_type=="L"] = NA

ds$eco_metaeco_type = paste0(ds$patch_size, " (", ds$metaecosystem_type, ")")
ds$eco_metaeco_type[ds$eco_metaeco_type == "S (NA)"] = "S"
ds$eco_metaeco_type[ds$eco_metaeco_type == "M (NA)"] = "M"
ds$eco_metaeco_type[ds$eco_metaeco_type == "L (NA)"] = "L"
ds$eco_metaeco_type = factor(ds$eco_metaeco_type, levels=c('S', 'S (S_S)', 'S (S_L)', 'M', 'M (M_M)', 'L', 'L (L_L)', 'L (S_L)'))

ds = ds %>% select(culture_ID, patch_size, disturbance, metaecosystem_type, bioarea_per_volume, replicate_video, day, metaecosystem, system_nr, eco_metaeco_type, indiv_ml)
col_order <- c("culture_ID", "system_nr", "disturbance", "day", "patch_size", "metaecosystem", "metaecosystem_type", "eco_metaeco_type", "replicate_video","bioarea_per_volume","indiv_ml")
ds = ds[, col_order]















####################### --- PLOT: BIOMASS LOW DISTURBANCE (complete) --- ##################################


p1 = ds %>%
  filter ( disturbance == "low") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaecosystem_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
              y = bioarea_per_volume,
              group = interaction (day, metaecosystem_type),
              fill = metaecosystem_type,
              color = metaecosystem_type)) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Regional biomass (average bioarea between 2 patches/µl)") +
  labs(color='Meta-ecosystem type')  +
  labs(fill='Meta-ecosystem type') +
  scale_y_continuous(limits = c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p1

p2 = ds %>%
  filter ( disturbance == "low") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaecosystem_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
              y = bioarea_per_volume,
              fill = metaecosystem_type,
              color = metaecosystem_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line (stat = "summary", fun = "mean") +
  xlab("Day") +
  ylab("Regional biomass (average bioarea between 2 patches/µl)") +
  labs(color='Meta-ecosystem type')  +
  labs(fill='Meta-ecosystem type') +
  scale_y_continuous(limits = c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p2

p3 = ds %>%
  filter(disturbance == "low")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p3


p4 = ds %>%
  filter(disturbance == "low")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p4

grid = grid.arrange(p1,p3,p2,p4,
             ncol=2, nrow=2,
             top = textGrob("Low disturbance",gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/biomass/All_biomass_low.jpg", grid, width = 22, height = 13)
















####################### --- PLOT: BIOMASS HIGH DISTURBANCE (complete) --- ##################################


high.biomass.reg.raw = ds %>%
  filter ( disturbance == "high") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaecosystem_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
              y = bioarea_per_volume,
              group = interaction(day, metaecosystem_type),
              fill = metaecosystem_type,
              color = metaecosystem_type)) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Regional biomass (average bioarea between 2 patches/µl)") +
  labs(color='Meta-ecosystem type')  +
  labs(fill='Meta-ecosystem type') +
  scale_y_continuous(limits = c(0,6250)) +
  scale_x_continuous(limits = c(-2, 30))


high.biomass.reg.mean = ds %>%
  filter ( disturbance == "high") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaecosystem_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
              y = bioarea_per_volume,
              fill = metaecosystem_type,
              color = metaecosystem_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line (stat = "summary", fun = "mean") +
  xlab("Day") +
  ylab("Regional biomass (average bioarea between 2 patches/µl)") +
  labs(color='Meta-ecosystem type')  +
  labs(fill='Meta-ecosystem type') +
  scale_y_continuous(limits = c(0,6250)) +
  scale_x_continuous(limits = c(-2, 30))

high.biomass.local.raw = ds %>%
  filter(disturbance == "high")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))


high.biomass.local.mean = ds %>%
  filter(disturbance == "high")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0,6250)) +
  scale_x_continuous(limits = c(-2, 30))


grid = grid.arrange(high.biomass.reg.raw, high.biomass.local.raw, high.biomass.reg.mean, high.biomass.local.mean,
             ncol=2, nrow=2,
             top = textGrob("High disturbance",gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/biomass/All_biomass_high.jpg", grid, width = 22, height = 13)















####################### --- PLOT: BIOMASS LOW DISTURBANCE (cleaned) --- ##################################


p1 = ds %>%
  filter ( disturbance == "low") %>%
  filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
  group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaecosystem_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
              y = bioarea_per_volume,
              group = interaction(day, metaecosystem_type),
              fill = metaecosystem_type,
              color = metaecosystem_type)) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Regional biomass (average bioarea between 2 patches/µl)") +
  labs(color='Meta-ecosystem type')  +
  labs(fill='Meta-ecosystem type') +
  scale_y_continuous(limits = c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p1

p2 = ds %>%
  filter ( disturbance == "low") %>%
  filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
  group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaecosystem_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
              y = bioarea_per_volume,
              fill = metaecosystem_type,
              color = metaecosystem_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line (stat = "summary", fun = "mean") +
  xlab("Day") +
  ylab("Regional biomass (average bioarea between 2 patches/µl)") +
  labs(color='Meta-ecosystem type')  +
  labs(fill='Meta-ecosystem type') +
  scale_y_continuous(limits = c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p2

p3 = ds %>%
  filter(disturbance == "low")%>%
  filter(metaecosystem == "no") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p3


p4 = ds %>%
  filter(disturbance == "low")%>%
  filter(metaecosystem == "no") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p4

p5 = ds %>%
  filter(disturbance == "low")%>%
  filter (eco_metaeco_type == "S" | eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p5


p6 = ds %>%
  filter(disturbance == "low")%>%
  filter (eco_metaeco_type == "S" | eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p6


grid = grid.arrange(p1,p3,p5,p2,p4,p6,
             ncol=3, nrow=2,
             top = textGrob("Low disturbance",gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/biomass/Clean_biomass_low.jpg", grid, width = 22, height = 13)















####################### --- PLOT: BIOMASS HIGH DISTURBANCE (cleaned) --- ##################################


p1 = ds %>%
  filter ( disturbance == "high") %>%
  filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
  group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaecosystem_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
              y = bioarea_per_volume,
              group = interaction(day, metaecosystem_type),
              fill = metaecosystem_type,
              color = metaecosystem_type)) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Regional biomass (average bioarea between 2 patches/µl)") +
  labs(color='Meta-ecosystem type')  +
  labs(fill='Meta-ecosystem type') +
  scale_y_continuous(limits = c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p1

p2 = ds %>%
  filter ( disturbance == "high") %>%
  filter (metaecosystem_type == "S_L" | metaecosystem_type == "M_M") %>%
  group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaecosystem_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
              y = bioarea_per_volume,
              fill = metaecosystem_type,
              color = metaecosystem_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line (stat = "summary", fun = "mean") +
  xlab("Day") +
  ylab("Regional biomass (average bioarea between 2 patches/µl)") +
  labs(color='Meta-ecosystem type')  +
  labs(fill='Meta-ecosystem type') +
  scale_y_continuous(limits = c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p2

p3 = ds %>%
  filter(disturbance == "high")%>%
  filter(metaecosystem == "no") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p3


p4 = ds %>%
  filter(disturbance == "high")%>%
  filter(metaecosystem == "no") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p4

p5 = ds %>%
  filter(disturbance == "high")%>%
  filter (eco_metaeco_type == "S" | eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             group = interaction(day, eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p5


p6 = ds %>%
  filter(disturbance == "high")%>%
  filter (eco_metaeco_type == "S" | eco_metaeco_type == "S (S_S)" | eco_metaeco_type == "S (S_L)") %>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  labs(color='Patch type')  +
  labs(fill='Patch type') +
  scale_y_continuous(limits=c(0, 6250)) +
  scale_x_continuous(limits = c(-2, 30))
p6


grid = grid.arrange(p1,p3,p5,p2,p4,p6,
             ncol=3, nrow=2,
             top = textGrob("High disturbance",gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/biomass/Clean_biomass_high.jpg", grid, width = 22, height = 13)














######################### --- PLOT: BIOMASS DIFFERENCE BETWEEN ECOSYSTEMS --- ###############################


biomass_difference = data.frame(matrix(ncol = 4, nrow = 0))
colnames(biomass_difference) = c('day', 'system_nr', 'metaecosystem_type', "biomass_difference")
ds_temp_2 = biomass_difference

for (DAY in 1:max(ds$day)) {
  if (any(ds$day==DAY) == TRUE) {
    for (SYSTEM_NR in 1:max(ds$system_nr)){
    ds_temp = ds%>%
      filter(metaecosystem == "yes") %>%
      filter(day == DAY, system_nr == SYSTEM_NR)
    if (dim(ds_temp)[1] != 0) {
      max = max(ds_temp$bioarea_per_volume)
      min = min(ds_temp$bioarea_per_volume)
      ds_temp_2[1,]$day = DAY
      ds_temp_2[1,]$system_nr = SYSTEM_NR
      ds_temp_2[1,]$metaecosystem_type = ds_temp[1,]$metaecosystem_type
      ds_temp_2[1,]$biomass_difference = (max/min)
      biomass_difference = rbind(biomass_difference, ds_temp_2)
      } 
    }
  }
}

bio_diff.raw = biomass_difference %>%
  filter(metaecosystem_type != "S_L") %>%
  filter(biomass_difference != Inf) %>%
  ggplot (aes(x= day,
              y = biomass_difference,
              group = interaction(day, metaecosystem_type),
              fill = metaecosystem_type,
              color = metaecosystem_type)) +
  geom_boxplot() +
  geom_hline(yintercept=1,linetype=2) +
  xlab("Day") +
  ylab("Biomass ratio between ecosystems (bioarea/volume)") +
  labs(color='Meta-ecosystem type')  +
  labs(fill='Meta-ecosystem type') +
  scale_x_continuous(limits = c(0,30)) +
  scale_y_continuous(limits = c(0,16), breaks=c(0,2,4,6,8,10,12,14,16))
  

bio_diff.average = biomass_difference %>%
  filter(metaecosystem_type != "S_L") %>%
  filter(biomass_difference != Inf) %>%
  ggplot (aes(x= day, 
              y = biomass_difference,
              fill = metaecosystem_type,
              color = metaecosystem_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  geom_hline(yintercept=1,linetype=2) +
  xlab("Day") +
  ylab("Biomass ratio between ecosystems (bioarea/volume)")+
  labs(color='Meta-ecosystem type')  +
  labs(fill='Meta-ecosystem type') +
  scale_x_continuous(limits = c(0,30)) +
  scale_y_continuous(limits = c(0,16), breaks=c(0,2,4,6,8,10,12,14,16))


grid = grid.arrange(bio_diff.raw, bio_diff.average,
             ncol=1, nrow=2,
             top = textGrob("Biomass ratio between ecosystems", gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/biomass/Biomass_differences.jpg", grid, width = 22, height = 13)















# ####################### --- PLOT: COMMUNITY ABUNDANCE --- ##################################
# 
# 
# low.abundance.reg.raw = ds %>%
#   filter ( disturbance == "low") %>%
#   filter (metaecosystem == "yes") %>%
#   group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
#   summarise(indiv_ml = mean(indiv_ml)) %>%
#   group_by(day,metaecosystem_type, system_nr) %>%
#   summarise(indiv_ml = mean(indiv_ml)) %>%
#   ggplot (aes(x = reorder(day, sort(as.numeric(day))),
#               y = indiv_ml,
#               fill = metaecosystem_type,
#               color = metaecosystem_type)) +
#   geom_boxplot() +
#   # labs(title = "Disturbance = low (raw data)") +
#   xlab("Day") +
#   ylab("Regional abundance (average individuals between 2 patches/µl)") +
#   scale_y_continuous(limits = c(0,1750))
# 
# high.abundance.reg.raw = ds %>%
#   filter ( disturbance == "high") %>%
#   filter (metaecosystem == "yes") %>%
#   group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
#   summarise(indiv_ml = mean(indiv_ml)) %>%
#   group_by(day,metaecosystem_type, system_nr) %>%
#   summarise(indiv_ml = mean(indiv_ml)) %>%
#   ggplot (aes(x = reorder(day, sort(as.numeric(day))),
#               y = indiv_ml,
#               fill = metaecosystem_type,
#               color = metaecosystem_type)) +
#   geom_boxplot() +
#   # labs(title = "Disturbance = high (raw data)") +
#   xlab("Day") +
#   ylab("Regional abundance (average individuals between 2 patches/µl)") +
#   scale_y_continuous(limits = c(0,1750))
# 
# low.abundance.reg.mean = ds %>%
#   filter ( disturbance == "low") %>%
#   filter (metaecosystem == "yes") %>%
#   group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
#   summarise(indiv_ml = mean(indiv_ml)) %>%
#   group_by(day,metaecosystem_type, system_nr) %>%
#   summarise(indiv_ml = mean(indiv_ml)) %>%
#   ggplot (aes(x = day,
#               y = indiv_ml,
#               fill = metaecosystem_type,
#               color = metaecosystem_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   # labs(title = "Disturbance = low (averaged data)") +
#   xlab("Day") +
#   ylab("Regional abundance (average individuals between 2 patches/µl)") +
#   scale_y_continuous(limits = c(0,1750))
# 
# high.abundance.reg.mean = ds %>%
#   filter ( disturbance == "high") %>%
#   filter (metaecosystem == "yes") %>%
#   group_by(culture_ID, day, metaecosystem_type, system_nr, patch_size) %>%
#   summarise(indiv_ml = mean(indiv_ml)) %>%
#   group_by(day,metaecosystem_type, system_nr) %>%
#   summarise(indiv_ml = mean(indiv_ml)) %>%
#   ggplot (aes(x = day,
#               y = indiv_ml,
#               fill = metaecosystem_type,
#               color = metaecosystem_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   # labs(title = "Disturbance = high (averaged data)") +
#   xlab("Day") +
#   ylab("Regional abundance (average individuals between 2 patches/µl)") +
#   scale_y_continuous(limits = c(0,1750))
# 
# low.abundance.local.raw = ds %>%
#   filter(disturbance == "low")%>%
#   ggplot(aes(x = reorder(day, sort(as.numeric(day))),
#              y = indiv_ml,
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   # labs(title = "Disturbance = low (raw data)") +
#   xlab("Day") +
#   ylab("Community abundance (indiv/ml)") +
#   scale_y_continuous(limits=c(0, 2250))
# 
# high.abundance.local.raw = ds %>%
#   filter(disturbance == "high")%>%
#   ggplot(aes(x = reorder(day, sort(as.numeric(day))),
#              y = indiv_ml,
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   # labs(title = "Disturbance = high (raw data)") +
#   xlab("Day") +
#   ylab("Community abundance (indiv/ml)") +
#   scale_y_continuous(limits=c(0,2250))
# 
# low.abundance.local.mean = ds %>%
#   filter(disturbance == "low")%>%
#   ggplot(aes(x = day,
#              y = indiv_ml,
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line(stat = "summary", fun = "mean") +
#   # labs(title = "Disturbance = low (averaged data)") +
#   xlab("Day") +
#   ylab("Community abundance (indiv/ml)") +
#   scale_y_continuous(limits=c(0, 2250))
# 
# high.abundance.local.mean = ds %>%
#   filter(disturbance == "high")%>%
#   ggplot(aes(x = day,
#              y = indiv_ml,
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line(stat = "summary", fun = "mean") +
#   # labs(title = "Disturbance = high (averaged data)") +
#   xlab("Day") +
#   ylab("Community abundance (indiv/ml)") +
#   scale_y_continuous(limits=c(0,2250))
# 
# grid.arrange(low.abundance.reg.raw, low.abundance.local.raw, low.abundance.reg.mean, low.abundance.local.mean,
#              ncol=2, nrow=2,
#              top = textGrob("Abundance, disturbance = low",gp=gpar(fontsize=20,font=3)))
# 
# grid.arrange(high.abundance.reg.raw, high.abundance.local.raw, high.abundance.reg.mean, high.abundance.local.mean,
#              ncol=2, nrow=2,
#              top = textGrob("Abundance, disturbance = high",gp=gpar(fontsize=20,font=3)))
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
