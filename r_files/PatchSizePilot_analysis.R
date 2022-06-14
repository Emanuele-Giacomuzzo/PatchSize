rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("grid")
library("gridExtra") 

info = read.csv("/Users/ema/github/PatchSizePilot/data/PatchSizePilot_dataset.csv", header = TRUE)
info$culture_ID = info$id
info$id = NULL

load("~/github/PatchSizePilot/data/data_points/t0.RData")
t0 = pop_output
load("~/github/PatchSizePilot/data/data_points/t1.RData")
t1 = pop_output
load("~/github/PatchSizePilot/data/data_points/t2.RData")
t2 = pop_output
load("~/github/PatchSizePilot/data/data_points/t3.RData")
t3 = pop_output
load("~/github/PatchSizePilot/data/data_points/t4.RData")
t4 = pop_output
load("~/github/PatchSizePilot/data/data_points/t5.RData")
t5 = pop_output
load("~/github/PatchSizePilot/data/data_points/t6.RData")
t6 = pop_output
load("~/github/PatchSizePilot/data/data_points/t7.RData")
t7 = pop_output
rm(pop_output)

elongating_t0 = NULL
for (video in 1:nrow(t0)){
  for (ID in 1:nrow(info)) {
    elongating_t0 = rbind(elongating_t0, t0[video,])
  }
}
culture_IDs = rep(1:nrow(info), times = nrow(t0))
elongating_t0$culture_ID = culture_IDs


t6$replicate_video = t6$replicate
t6$replicate = NULL
t7$replicate_video = t7$replicate
t7$replicate = NULL

t0 = merge(info,elongating_t0, by="culture_ID")
t1 = merge(info,t1,by="culture_ID")
t2 = merge(info,t2,by="culture_ID")
t3 = merge(info,t3,by="culture_ID")
t4 = merge(info,t4,by="culture_ID")
t5 = merge(info,t5,by="culture_ID")
t6 = merge(info,t6,by="culture_ID")
t7 = merge(info,t7,by="culture_ID")

t0$time = "to find out"
t1$time = "to find out"

t0$replicate_video = 1:12
t1$replicate_video = 1
t2$replicate_video = 1
t3$replicate_video = 1
t4$replicate_video = 1
t5$replicate_video = 1

ds = rbind(t0, t1, t2, t3, t4, t5, t6, t7)
rm(elongating_t0, t0, t1, t2, t3, t4, t5, t6, t7)

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

ds$metaecosystem_nr = ds$metaecosystem
ds$metaecosystem = NULL

ds$system[ds$metaeco=="L"] = "closed_ecosystem"
ds$system[ds$metaeco=="L_L"] = "metaecosystem"
ds$system[ds$metaeco=="M"] = "closed_ecosystem"
ds$system[ds$metaeco=="M_M"] = "metaecosystem"
ds$system[ds$metaeco=="S"] = "closed_ecosystem"
ds$system[ds$metaeco=="S_L"] = "metaecosystem"
ds$system[ds$metaeco=="S_S"] = "metaecosystem"

ds$disturbance_ml = ds$disturbance.1
ds$disturbance.1 = NULL

ds$metaeco[ds$metaeco=="S"] = closed_ecosystem
ds$metaeco[ds$metaeco=="M"] = closed_ecosystem
ds$metaeco[ds$metaeco=="L"] = closed_ecosystem

ds$patch_size_ml = ds$patch_size
ds$patch_size[ds$patch_size_ml == 7.5] = "S"
ds$patch_size[ds$patch_size_ml == 22.5] = "M"
ds$patch_size[ds$patch_size_ml == 37.5] = "L"

ds$system_nr = ds$metaecosystem_nr
ds$metaecosystem_nr = NULL

ds$ecosystem = paste0(ds$patch_size, " (", ds$metaeco, ")")

#Reorder here the levels of ds$ecosystem
ds$ecosystem = factor(ds$ecosystem, 
                      levels=c('S (S)', 'M (M)', 'L (L)', 'S (S_S)', 'S (S_L)', 'M (M_M)', 'L (L_L)', 'L (S_L)'))
#ds$ecosystem = ds[order(levels(ds$ecosystem)),]

# ---- LOCAL BIOMASS ---- #
p1 = ds %>%
  filter(disturbance == "low")%>%
  filter(system == "metaecosystem")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = bioarea_per_volume,
             fill = ecosystem,
             color = ecosystem)) +
  geom_boxplot() +
  labs(title = "Disturbance = low (raw data)") +
  xlab("Day") +
  ylab("Local biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0, 5500))

p2 = ds %>%
  filter(disturbance == "high")%>%
  filter(system == "metaecosystem")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = bioarea_per_volume,
             fill = ecosystem,
             color = ecosystem)) +
  geom_boxplot() +
  labs(title = "Disturbance = high (raw data)") +
  xlab("Day") +
  ylab("Local biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0, 5500))

p3 = ds %>%
  filter(disturbance == "low")%>%
  filter(system == "metaecosystem")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = ecosystem,
             color = ecosystem)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Disturbance = low (averaged data)") +
  xlab("Day") +
  ylab("Local biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0, 5500))

p4 = ds %>%
  filter(disturbance == "high")%>%
  filter(system == "metaecosystem")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = ecosystem,
             color = ecosystem)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Disturbance = high (averaged data)") +
  xlab("Day") +
  ylab("Local biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0,5500))

grid.arrange(p1,p2,p3,p4, ncol=2, nrow=2,
             top = textGrob("Local biomass",gp=gpar(fontsize=20,font=3)))


# ---- REGIONAL BIOMASS ----- #

p1 = ds %>%
  filter(disturbance == "low")%>%
  filter(system == "metaecosystem")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = bioarea_per_volume,
             fill = metaeco,
             color = metaeco)) +
  geom_boxplot() +
  labs(title = "Disturbance = low (raw data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0, 5500))

p2 = ds %>%
  filter(disturbance == "high")%>%
  filter(system == "metaecosystem")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = bioarea_per_volume,
             fill = metaeco,
             color = metaeco)) +
  geom_boxplot() +
  labs(title = "Disturbance = high (raw data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0, 5500))


p3 = ds %>%
  filter(disturbance == "low")%>%
  filter(system == "metaecosystem")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = metaeco,
             color = metaeco)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Disturbance = low (averaged data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0, 5500))

p4 = ds %>%
  filter(disturbance == "high")%>%
  filter(system == "metaecosystem")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = metaeco,
             color = metaeco)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Disturbance = high (averaged data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0,5500))

grid.arrange(p1,p2,p3,p4, ncol=2, nrow=2,
             top = textGrob("Regional biomass",gp=gpar(fontsize=20,font=3)))

# ---- SINGLE ECOSYSTEMS BIOMASS ----- #

p1 = ds %>%
  filter(disturbance == "low")%>%
  filter(system == "closed_ecosystem")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = bioarea_per_volume,
             fill = ecosystem,
             color = ecosystem)) +
  geom_boxplot() +
  labs(title = "Disturbance = low (raw data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0, 5500))

p2 = ds %>%
  filter(disturbance == "high")%>%
  filter(system == "closed_ecosystem")%>%
  ggplot(aes(x = reorder(day, sort(as.numeric(day))),
             y = bioarea_per_volume,
             fill = metaeco,
             color = metaeco)) +
  geom_boxplot() +
  labs(title = "Disturbance = high (raw data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0, 5500))


p3 = ds %>%
  filter(disturbance == "low")%>%
  filter(system == "closed_ecosystem")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = metaeco,
             color = metaeco)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Disturbance = low (averaged data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0, 5500))

p4 = ds %>%
  filter(disturbance == "high")%>%
  filter(system == "closed_ecosystem")%>%
  ggplot(aes(x = day,
             y = bioarea_per_volume,
             fill = metaeco,
             color = metaeco)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Disturbance = high (averaged data)") +
  xlab("Day") +
  ylab("Regional biomass (bioarea/volume)") +
  scale_y_continuous(limits=c(0,5500))

grid.arrange(p1,p2,p3,p4, ncol=2, nrow=2,
             top = textGrob("closed_ecosystem ecosystems biomass",gp=gpar(fontsize=20,font=3)))