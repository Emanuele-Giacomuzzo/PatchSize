rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("grid")
library("gridExtra") 

### --- IMPORT DATASET --- ###
info = read.csv("/Users/ema/github/PatchSizePilot/data/PatchSizePilot_dataset.csv", header = TRUE)
load("~/github/PatchSizePilot/data/population/t0.RData")
t0 = pop_output
load("~/github/PatchSizePilot/data/population/t1.RData")
t1 = pop_output
load("~/github/PatchSizePilot/data/population/t2.RData")
t2 = pop_output
load("~/github/PatchSizePilot/data/population/t3.RData")
t3 = pop_output
load("~/github/PatchSizePilot/data/population/t4.RData")
t4 = pop_output
load("~/github/PatchSizePilot/data/population/t5.RData")
t5 = pop_output
load("~/github/PatchSizePilot/data/population/t6.RData")
t6 = pop_output
load("~/github/PatchSizePilot/data/population/t7.RData")
t7 = pop_output
rm(pop_output)



### --- MODIFY DATASET --- ###

#Modify datsets of time points
t0$time = "to find out"
t1$time = "to find out"


#Modify info dataset
elongating_t0 = NULL
for (video in 1:nrow(t0)){
  for (ID in 1:nrow(info)) {
    elongating_t0 = rbind(elongating_t0, t0[video,])
  }
}

info$culture_ID = info$id
info$id = NULL
culture_IDs = rep(1:nrow(info), times = nrow(t0))
elongating_t0$culture_ID = culture_IDs

#Change the column from replicate to replicate video so that it can be distinguished from treatment replicate
t6$replicate_video = t6$replicate
t6$replicate = NULL
t7$replicate_video = t7$replicate
t7$replicate = NULL

#Merge each data point with its information
t0 = merge(info,elongating_t0, by="culture_ID")
t1 = merge(info,t1,by="culture_ID")
t2 = merge(info,t2,by="culture_ID")
t3 = merge(info,t3,by="culture_ID")
t4 = merge(info,t4,by="culture_ID")
t5 = merge(info,t5,by="culture_ID")
t6 = merge(info,t6,by="culture_ID")
t7 = merge(info,t7,by="culture_ID")

#Give the replicate video 
t0$replicate_video = 1:12
t1$replicate_video = 1
t2$replicate_video = 1
t3$replicate_video = 1
t4$replicate_video = 1
t5$replicate_video = 1

#Bind all data points together 
ds = rbind(t0, t1, t2, t3, t4, t5, t6, t7)
rm(elongating_t0, t0, t1, t2, t3, t4, t5, t6, t7)

ds$system_nr = ds$metaecosystem
ds$metaecosystem = NULL

#Transform the dates from time point to days from the start of the experiment
ds$time_points = NULL
ds$day = ds$time_point
ds$day[ds$day=="t0"] = "0"
ds$day[ds$day=="t1"] = "4"
ds$day[ds$day=="t2"] = "8"
ds$day[ds$day=="t3"] = "12"
ds$day[ds$day=="t4"] = "16"
ds$day[ds$day=="t5"] = "20"
ds$day[ds$day=="t6"] = "24"
ds$day[ds$day=="t7"] = "28"
ds$day = as.numeric(ds$day)

#Set up column on whether a patch is part of a metaecosystem
ds$metaecosystem[ds$metaeco=="L"] = "no"
ds$metaecosystem[ds$metaeco=="L_L"] = "yes"
ds$metaecosystem[ds$metaeco=="M"] = "no"
ds$metaecosystem[ds$metaeco=="M_M"] = "yes"
ds$metaecosystem[ds$metaeco=="S"] = "no"
ds$metaecosystem[ds$metaeco=="S_L"] = "yes"
ds$metaecosystem[ds$metaeco=="S_S"] = "yes"

#Change disturbance volume column
ds$disturbance_ml = ds$disturbance.1
ds$disturbance.1 = NULL

#Change metaeco column
ds$metaeco[ds$metaeco=="S"] = NA
ds$metaeco[ds$metaeco=="M"] = NA
ds$metaeco[ds$metaeco=="L"] = NA

#Create a new column of patch_size
ds$patch_size_ml = ds$patch_size
ds$patch_size[ds$patch_size_ml == 7.5] = "S"
ds$patch_size[ds$patch_size_ml == 22.5] = "M"
ds$patch_size[ds$patch_size_ml == 37.5] = "L"

#create the ecosystem column 
ds$ecosystem = paste0(ds$patch_size, " (", ds$metaeco, ")")

#Reorder the levels of ecosystem 
ds$ecosystem = factor(ds$ecosystem, 
                      levels=c('S (S)', 'M (M)', 'L (L)', 'S (S_S)', 'S (S_L)', 'M (M_M)', 'L (L_L)', 'L (S_L)'))

#Transform individuals/microlitres to individuals/millilitres
ds$indiv_ml = ds$indiv_per_volume*1000

#Get rid of useless columns, change the name of the columns that are left
ds = ds %>% select(culture_ID, patch_size, disturbance, metaeco, bioarea_per_volume, replicate_video, day, metaecosystem, system_nr, ecosystem, indiv_ml)


#Change name of columns
ds$treatment_replicate = ds$replicate
ds$replicate = NULL
ds$metaeco_type = ds$metaeco
ds$metaeco = NULL
ds$ecosystem_type = ds$ecosystem
ds$ecosystem = NULL
ds$eco_metaeco_type = ds$ecosystem_type
ds$ecosystem_type = NULL

#Change order of columns 
col_order <- c("culture_ID", "disturbance", "day", "metaecosystem", "metaeco_type", "system_nr", "replicate_video", "patch_size", "eco_metaeco_type","bioarea_per_volume","indiv_ml")
ds = ds[, col_order]


# ----- PLOTS ------- #

low.biomass.reg.raw = ds %>%
  filter ( disturbance == "low") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaeco_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaeco_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = reorder(day, sort(as.numeric(day))),
              y = bioarea_per_volume,
              fill = metaeco_type,
              color = metaeco_type)) +
  geom_boxplot() +
  # labs(title = "Disturbance = low (raw data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/µl)") +
  scale_y_continuous(limits = c(0, 5250))

high.biomass.reg.raw = ds %>%
  filter ( disturbance == "high") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaeco_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaeco_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = reorder(day, sort(as.numeric(day))),
              y = bioarea_per_volume,
              fill = metaeco_type,
              color = metaeco_type)) +
  geom_boxplot() +
  # labs(title = "Disturbance = high (raw data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/µl)") +
  scale_y_continuous(limits = c(0,5250))

low.biomass.reg.mean = ds %>%
  filter ( disturbance == "low") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaeco_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaeco_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
              y = bioarea_per_volume,
              fill = metaeco_type,
              color = metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line (stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = low (averaged data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/µl)") +
  scale_y_continuous(limits = c(0, 5250))

high.biomass.reg.mean = ds %>%
  filter ( disturbance == "high") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaeco_type, system_nr, patch_size) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  group_by(day,metaeco_type, system_nr) %>%
  summarise(bioarea_per_volume = mean(bioarea_per_volume)) %>%
  ggplot (aes(x = day,
              y = bioarea_per_volume,
              fill = metaeco_type,
              color = metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line (stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = high (averaged data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/µl)") +
  scale_y_continuous(limits = c(0,5250))

low.biomass.local.raw = ds %>%
  filter(disturbance == "low")%>%
  filter(metaecosystem == "yes")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  # labs(title = "Disturbance = low (raw data)") +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  scale_y_continuous(limits=c(0, 5500))

high.biomass.local.raw = ds %>%
  filter(disturbance == "high")%>%
  filter(metaecosystem == "yes")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  # labs(title = "Disturbance = high (raw data)") +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  scale_y_continuous(limits=c(0, 5500))

low.biomass.local.mean = ds %>%
  filter(disturbance == "low")%>%
  filter(metaecosystem == "yes")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = low (averaged data)") +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  scale_y_continuous(limits=c(0, 5500))

high.biomass.local.mean = ds %>%
  filter(disturbance == "high")%>%
  filter(metaecosystem == "yes")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = high (averaged data)") +
  xlab("Day") +
  ylab("Local biomass (bioarea/µl)") +
  scale_y_continuous(limits=c(0,5500))

low.biomass.closed.raw = ds %>%
  filter(disturbance == "low")%>%
  filter(metaecosystem == "no")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = bioarea_per_volume,
             fill = patch_size,
             color = patch_size)) +
  geom_boxplot() +
  # labs(title = "Disturbance = low (raw data)") +
  xlab("Day") +
  ylab("Closed ecosystem biomass (bioarea/µl)") +
  scale_y_continuous(limits=c(0, 5500))

high.biomass.closed.raw = ds %>%
  filter(disturbance == "high")%>%
  filter(metaecosystem == "no")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = bioarea_per_volume,
             fill = patch_size,
             color = patch_size)) +
  geom_boxplot() +
  # labs(title = "Disturbance = high (raw data)") +
  xlab("Day") +
  ylab("Closed ecosystem biomass (bioarea/µl)") +
  scale_y_continuous(limits=c(0, 5500))

low.biomass.closed.mean = ds %>%
  filter(disturbance == "low")%>%
  filter(metaecosystem == "no")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = patch_size,
             color = patch_size)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = low (averaged data)") +
  xlab("Day") +
  ylab("Closed ecosystem biomass (bioarea/µl)") +
  scale_y_continuous(limits=c(0, 5500))

high.biomass.closed.mean = ds %>%
  filter(disturbance == "high")%>%
  filter(metaecosystem == "no")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = patch_size,
             color = patch_size)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = high (averaged data)") +
  xlab("Day") +
  ylab("Closed ecosystem biomass (bioarea/µl)") +
  scale_y_continuous(limits=c(0,5500))

low.abundance.reg.raw = ds %>%
  filter ( disturbance == "low") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaeco_type, system_nr, patch_size) %>%
  summarise(indiv_ml = mean(indiv_ml)) %>%
  group_by(day,metaeco_type, system_nr) %>%
  summarise(indiv_ml = mean(indiv_ml)) %>%
  ggplot (aes(x = reorder(day, sort(as.numeric(day))),
              y = indiv_ml,
              fill = metaeco_type,
              color = metaeco_type)) +
  geom_boxplot() +
  # labs(title = "Disturbance = low (raw data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/µl)") +
  scale_y_continuous(limits = c(0,1750))

high.abundance.reg.raw = ds %>%
  filter ( disturbance == "high") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaeco_type, system_nr, patch_size) %>%
  summarise(indiv_ml = mean(indiv_ml)) %>%
  group_by(day,metaeco_type, system_nr) %>%
  summarise(indiv_ml = mean(indiv_ml)) %>%
  ggplot (aes(x = reorder(day, sort(as.numeric(day))),
              y = indiv_ml,
              fill = metaeco_type,
              color = metaeco_type)) +
  geom_boxplot() +
  # labs(title = "Disturbance = high (raw data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/µl)") +
  scale_y_continuous(limits = c(0,1750))
  
low.abundance.reg.mean = ds %>%
  filter ( disturbance == "low") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaeco_type, system_nr, patch_size) %>%
  summarise(indiv_ml = mean(indiv_ml)) %>%
  group_by(day,metaeco_type, system_nr) %>%
  summarise(indiv_ml = mean(indiv_ml)) %>%
  ggplot (aes(x = day,
              y = indiv_ml,
              fill = metaeco_type,
              color = metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line (stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = low (averaged data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/µl)") +
  scale_y_continuous(limits = c(0,1750))

high.abundance.reg.mean = ds %>%
  filter ( disturbance == "high") %>%
  filter (metaecosystem == "yes") %>%
  group_by(culture_ID, day, metaeco_type, system_nr, patch_size) %>%
  summarise(indiv_ml = mean(indiv_ml)) %>%
  group_by(day,metaeco_type, system_nr) %>%
  summarise(indiv_ml = mean(indiv_ml)) %>%
  ggplot (aes(x = day,
              y = indiv_ml,
              fill = metaeco_type,
              color = metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line (stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = high (averaged data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/µl)") +
  scale_y_continuous(limits = c(0,1750))

low.abundance.local.raw = ds %>%
  filter(disturbance == "low")%>%
  filter(metaecosystem == "yes")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = indiv_ml,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  # labs(title = "Disturbance = low (raw data)") +
  xlab("Day") +
  ylab("Community abundance (indiv/ml)") +
  scale_y_continuous(limits=c(0, 2250))

high.abundance.local.raw = ds %>%
  filter(disturbance == "high")%>%
  filter(metaecosystem == "yes")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = indiv_ml,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot() +
  # labs(title = "Disturbance = high (raw data)") +
  xlab("Day") +
  ylab("Community abundance (indiv/ml)") +
  scale_y_continuous(limits=c(0,2250))

low.abundance.local.mean = ds %>%
  filter(disturbance == "low")%>%
  filter(metaecosystem == "yes")%>%
  ggplot(aes(x = day,
             y = indiv_ml,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = low (averaged data)") +
  xlab("Day") +
  ylab("Community abundance (indiv/ml)") +
  scale_y_continuous(limits=c(0, 2250))

high.abundance.local.mean = ds %>%
  filter(disturbance == "high")%>%
  filter(metaecosystem == "yes")%>%
  ggplot(aes(x = day,
             y = indiv_ml,
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = high (averaged data)") +
  xlab("Day") +
  ylab("Community abundance (indiv/ml)") +
  scale_y_continuous(limits=c(0,2250))

low.abundance.closed.raw = ds %>%
  filter(disturbance == "low")%>%
  filter(metaecosystem == "no")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = indiv_ml,
             fill = patch_size,
             color = patch_size)) +
  geom_boxplot() +
  # labs(title = "Disturbance = low (raw data)") +
  xlab("Day") +
  ylab("Closed ecosystem abundance (indiv/ml)") +
  scale_y_continuous(limits=c(0, 1850))

high.abundance.closed.raw = ds %>%
  filter(disturbance == "high")%>%
  filter(metaecosystem == "no")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = indiv_ml,
             fill = patch_size,
             color = patch_size)) +
  geom_boxplot() +
  # labs(title = "Disturbance = high (raw data)") +
  xlab("Day") +
  ylab("Closed ecosystem biomass (indiv/ml)") +
  scale_y_continuous(limits=c(0, 1850))

low.abundance.closed.mean = ds %>%
  filter(disturbance == "low")%>%
  filter(metaecosystem == "no")%>%
  ggplot(aes(x = day,
             y = indiv_ml,
             fill = patch_size,
             color = patch_size)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = low (averaged data)") +
  xlab("Day") +
  ylab("Closed ecosystem biomass (indiv/ml)") +
  scale_y_continuous(limits=c(0, 1850))

high.abundance.closed.mean = ds %>%
  filter(disturbance == "high")%>%
  filter(metaecosystem == "no")%>%
  ggplot(aes(x = day,
             y = indiv_ml,
             fill = patch_size,
             color = patch_size)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  # labs(title = "Disturbance = high (averaged data)") +
  xlab("Day") +
  ylab("Closed ecosystem biomass (indiv/ml)") +
  scale_y_continuous(limits=c(0,1850))

grid.arrange(low.biomass.reg.raw, low.biomass.local.raw, low.biomass.closed.raw, low.biomass.reg.mean, low.biomass.local.mean, low.biomass.closed.mean,
             ncol=3, nrow=2,
             top = textGrob("Biomass, disturbance = low",gp=gpar(fontsize=20,font=3)))

grid.arrange(high.biomass.reg.raw, high.biomass.local.raw, high.biomass.closed.raw, high.biomass.reg.mean, high.biomass.local.mean, high.biomass.closed.mean,
             ncol=3, nrow=2,
             top = textGrob("Biomass, disturbance = high",gp=gpar(fontsize=20,font=3)))

grid.arrange(low.abundance.reg.raw, low.abundance.local.raw, low.abundance.closed.raw, low.abundance.reg.mean, low.abundance.local.mean, low.abundance.closed.mean,
             ncol=3, nrow=2,
             top = textGrob("Abundance, disturbance = low",gp=gpar(fontsize=20,font=3)))

grid.arrange(high.abundance.reg.raw, high.abundance.local.raw, high.abundance.closed.raw, high.abundance.reg.mean, high.abundance.local.mean, high.abundance.closed.mean,
             ncol=3, nrow=2,
             top = textGrob("Abundance, disturbance = high",gp=gpar(fontsize=20,font=3)))