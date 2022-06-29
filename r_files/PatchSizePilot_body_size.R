rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("grid")
library("gridExtra") 

culture_info = read.csv("/Users/ema/github/PatchSizePilot/data/PatchSizePilot_dataset.csv", header = TRUE)
load("~/github/PatchSizePilot/data/morphology/t0.RData");t0 = morph_mvt
load("~/github/PatchSizePilot/data/morphology/t1.RData");t1 = morph_mvt
load("~/github/PatchSizePilot/data/morphology/t2.RData");t2 = morph_mvt
load("~/github/PatchSizePilot/data/morphology/t3.RData");t3 = morph_mvt
load("~/github/PatchSizePilot/data/morphology/t4.RData");t4 = morph_mvt
load("~/github/PatchSizePilot/data/morphology/t5.RData");t5 = morph_mvt
load("~/github/PatchSizePilot/data/morphology/t6.RData");t6 = morph_mvt
load("~/github/PatchSizePilot/data/morphology/t7.RData");t7 = morph_mvt
rm(morph_mvt)

t0$time = "to find out"
t1$time = "to find out"

t1$replicate_video = 1
t2$replicate_video = 1
t3$replicate_video = 1
t4$replicate_video = 1
t5$replicate_video = 1
t6$replicate_video = t6$replicate; t6$replicate = NULL
t7$replicate_video = t7$replicate; t7$replicate = NULL

t0$replicate_video[t0$file == "sample_00001"] = 1
t0$replicate_video[t0$file == "sample_00002"] = 2
t0$replicate_video[t0$file == "sample_00003"] = 3
t0$replicate_video[t0$file == "sample_00004"] = 4
t0$replicate_video[t0$file == "sample_00005"] = 5
t0$replicate_video[t0$file == "sample_00006"] = 6
t0$replicate_video[t0$file == "sample_00007"] = 7
t0$replicate_video[t0$file == "sample_00008"] = 8
t0$replicate_video[t0$file == "sample_00009"] = 9
t0$replicate_video[t0$file == "sample_00010"] = 10
t0$replicate_video[t0$file == "sample_00011"] = 11
t0$replicate_video[t0$file == "sample_00012"] = 12

culture_info$culture_ID = culture_info$id

t1 = merge(culture_info,t1,by="culture_ID")
t2 = merge(culture_info,t2,by="culture_ID")
t3 = merge(culture_info,t3,by="culture_ID")
t4 = merge(culture_info,t4,by="culture_ID")
t5 = merge(culture_info,t5,by="culture_ID")
t6 = merge(culture_info,t6,by="culture_ID")
t7 = merge(culture_info,t7,by="culture_ID")
ds = rbind(t1, t2, t3, t4, t5, t6, t7); rm(t1, t2, t3, t4, t5, t6, t7)

ds$time_points = NULL
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
ds$eco_metaeco_type = factor(ds$eco_metaeco_type, levels=c('S (S)', 'M (M)', 'L (L)', 'S (S_S)', 'S (S_L)', 'M (M_M)', 'L (L_L)', 'L (S_L)'))

ds = ds %>% select(culture_ID, patch_size, disturbance, metaecosystem_type, mean_area, replicate_video, day, metaecosystem, system_nr, eco_metaeco_type)
col_order <- c("culture_ID", "system_nr", "disturbance", "day", "patch_size", "metaecosystem", "metaecosystem_type", "eco_metaeco_type", "replicate_video","mean_area")
ds = ds[, col_order]

### --- PLOTS --- ###
low.day.4 = ds %>%
  filter (disturbance == "low") %>%
  filter (day == 4, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3)  +
  labs(title = "Day 4")

low.day.8 = ds %>%
  filter (disturbance == "low") %>%
  filter (day == 8, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) +
  labs(title = "Day 8")

low.day.12 = ds %>%
  filter (disturbance == "low") %>%
  filter (day == 12, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) + 
  labs(title = "Day 12")

low.day.16 = ds %>%
  filter (disturbance == "low") %>%
  filter (day == 16, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) + 
  labs(title = "Day 16")

low.day.20 = ds %>%
  filter (disturbance == "low") %>%
  filter (day == 20, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) +
  labs(title = "Day 20")

low.day.24 = ds %>%
  filter (disturbance == "low") %>%
  filter (day == 24, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) + 
  labs(title = "Day 24")

low.day.28 = ds %>%
  filter (disturbance == "low") %>%
  filter (day == 28, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) +
  labs(title = "Day 28")


grid.arrange(low.day.4, low.day.8, low.day.12, low.day.16, low.day.20, low.day.24, low.day.28,
             ncol=3, nrow=3,
             top = textGrob("Body size distribution (low disturbance)", gp=gpar(fontsize=20,font=3)))



high.day.4 = ds %>%
  filter (disturbance == "high") %>%
  filter (day == 4, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3)  +
  labs(title = "Day 4")

high.day.8 = ds %>%
  filter (disturbance == "high") %>%
  filter (day == 8, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) +
  labs(title = "Day 8")

high.day.12 = ds %>%
  filter (disturbance == "high") %>%
  filter (day == 12, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) + 
  labs(title = "Day 12")

high.day.16 = ds %>%
  filter (disturbance == "high") %>%
  filter (day == 16, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) + 
  labs(title = "Day 16")

high.day.20 = ds %>%
  filter (disturbance == "high") %>%
  filter (day == 20, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) +
  labs(title = "Day 20")

high.day.24 = ds %>%
  filter (disturbance == "high") %>%
  filter (day == 24, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) + 
  labs(title = "Day 24")

high.day.28 = ds %>%
  filter (disturbance == "high") %>%
  filter (day == 28, !is.na(eco_metaeco_type)) %>%
  ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
  geom_density(alpha=.3) +
  labs(title = "Day 28")


grid.arrange(high.day.4, high.day.8, high.day.12, high.day.16, high.day.20, high.day.24, high.day.28,
             ncol=3, nrow=3,
             top = textGrob("Body size distribution (high disturbance)", gp=gpar(fontsize=20,font=3)))

### --- BINNING --- ###
size_classes = seq(0, max(ds$mean_area), by = max(ds$mean_area)/12) #As in "How pulse disturbances shape size-abundance pyramids"

bin_lower_limit = size_classes[1]
bin_upper_limit = size_classes[2]

a = ds%>%
  filter(bin_lower_limit<=mean_area, mean_area <= bin_upper_limit) %>%
  group_by(culture_ID, system_nr, disturbance, day, patch_size, metaecosystem, metaecosystem_type, eco_metaeco_type, replicate_video) %>% #Group first by replicate video
  summarise(n=n())
