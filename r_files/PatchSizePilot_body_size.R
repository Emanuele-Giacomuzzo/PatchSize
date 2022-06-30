rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("grid")
library("gridExtra") 


############################ --- LOAD THE DATA --- ######################################################


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















########################## --- MODIFY COLUMN NAME AND CONTENT (t0-t7, culture_info) --- #######################################


culture_info$culture_ID = culture_info$id; culture_info$id = NULL

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















########################## --- MERGE DATASETS --- #######################################



long_t0 = t0 %>% slice(rep(1:n(), max(culture_info$culture_ID)))

ID_vector = NULL
ID_vector_elongating = NULL
for (ID in 1:max(culture_info$culture_ID)){
  ID_vector = rep(ID, times = nrow(t0))
  ID_vector_elongating = c(ID_vector_elongating, ID_vector)
}

long_t0$culture_ID = ID_vector_elongating


t0 = merge(culture_info,long_t0, by="culture_ID")
t1 = merge(culture_info,t1,by="culture_ID")
t2 = merge(culture_info,t2,by="culture_ID")
t3 = merge(culture_info,t3,by="culture_ID")
t4 = merge(culture_info,t4,by="culture_ID")
t5 = merge(culture_info,t5,by="culture_ID")
t6 = merge(culture_info,t6,by="culture_ID")
t7 = merge(culture_info,t7,by="culture_ID")
ds = rbind(t0, t1, t2, t3, t4, t5, t6, t7); rm(t0, t1, t2, t3, t4, t5, t6, t7)
















########################## --- MODIFY COLUMN NAME AND CONTENT (ds) --- #######################################


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
ds$eco_metaeco_type[ds$eco_metaeco_type == "S (NA)"] = "S"
ds$eco_metaeco_type[ds$eco_metaeco_type == "M (NA)"] = "M"
ds$eco_metaeco_type[ds$eco_metaeco_type == "L (NA)"] = "L"
ds$eco_metaeco_type = factor(ds$eco_metaeco_type, levels=c('S', 'S (S_S)', 'S (S_L)', 'M', 'M (M_M)', 'L', 'L (L_L)', 'L (S_L)'))

ds = ds %>% select(culture_ID, patch_size, disturbance, metaecosystem_type, mean_area, replicate_video, day, metaecosystem, system_nr, eco_metaeco_type)
col_order <- c("culture_ID", "system_nr", "disturbance", "day", "patch_size", "metaecosystem", "metaecosystem_type", "eco_metaeco_type", "replicate_video","mean_area")
ds = ds[, col_order]















##################################### --- CREATE SIZE CLASSES DATASET --- ##########################


size_classes = seq(0, max(ds$mean_area), by = max(ds$mean_area)/11) #As in "How pulse disturbances shape size-abundance pyramids"
elongating_size_classes = NULL

for (n in 1:length(size_classes)){
  
  bin_lower_limit = size_classes[n]
  bin_upper_limit = size_classes[n+1]
  
  size_class = ds%>%
    filter(bin_lower_limit<=mean_area, mean_area <= bin_upper_limit) %>%
    group_by(culture_ID, system_nr, disturbance, day, patch_size, metaecosystem, metaecosystem_type, eco_metaeco_type, replicate_video) %>% #Group by video
    summarise(n=n()) %>%
    group_by(culture_ID, system_nr, disturbance, day, patch_size, metaecosystem, metaecosystem_type, eco_metaeco_type) %>% #Group by ID
    summarise(n=mean(n))
  size_class$size_class = n
  elongating_size_classes = rbind(elongating_size_classes, size_class)
  
}

ds_classes = elongating_size_classes
ds_classes$log_n = log(ds_classes$n + 1)

for (i in 1:length(size_classes)){
  
  ds_classes$size_class[ds_classes$size_class==i] = mean(c(size_classes[i] + size_classes[i+1]))
  
}

ds_classes$log_size_class = log(ds_classes$size_class)
ds_classes$log_size_class = round(ds_classes$log_size_class, digits = 1)















################### --- SIZE CLASS PLOT FUNCTIONS --- #######################################


plot_data_mean = function (day_input, disturbance_input, patch_type){
  
  if (patch_type == "S") {
    
    temporary_plot <<- ds_classes %>%
      filter(day == day_input, disturbance == disturbance_input, patch_size == "S") %>%
      ggplot(aes(x = log_size_class,
                 y = log_n,
                 group = eco_metaeco_type,
                 color = eco_metaeco_type)) +
      geom_point(stat = "summary", fun = "mean") +
      geom_line (stat = "summary", fun = "mean") +
      labs(title = paste0("Day = ", day_input), x = "log(body-size)", y = "Mean abundance + 1 (log)") +
      scale_y_continuous(limits = c(0,5))+
      scale_x_continuous(limits = c(7,10.5))}
  
  if (patch_type == "closed") {
    
    temporary_plot <<- ds_classes %>%
      filter(day == day_input, disturbance == disturbance_input, metaecosystem == "no") %>%
      ggplot(aes(x = log_size_class,
                 y = log_n,
                 group = eco_metaeco_type,
                 color = eco_metaeco_type)) +
      geom_point(stat = "summary", fun = "mean") +
      geom_line (stat = "summary", fun = "mean") +
      labs(title = paste0("Day = ", day_input), x = "log(body-size)", y = "Mean abundance + 1 (log)") +
      scale_y_continuous(limits = c(0,5))+
      scale_x_continuous(limits = c(7,10.5))}
  
  
}

plot_data_raw = function (day_input, disturbance_input, patch_type){
  
  if (patch_type == "S") {
    
    temporary_plot <<-  ds_classes %>%
      filter(day == day_input, disturbance == disturbance_input, patch_size == "S") %>%
      ggplot(aes(x = log_size_class,
                 y = log_n,
                 group = interaction(log_size_class, eco_metaeco_type),
                 fill = eco_metaeco_type,
                 color = eco_metaeco_type)) +
      geom_boxplot() +
      labs(title = paste0("Day = ", day_input), x = "log(body-size)", y = "Mean abundance + 1 (log)") +
      scale_y_continuous(limits = c(0,5))+
      scale_x_continuous(limits = c(7,10.5))}
  
  if (patch_type == "closed") {
    
    temporary_plot <<- ds_classes %>%
      filter(day == day_input, disturbance == disturbance_input, metaecosystem == "no") %>%
      ggplot(aes(x = log_size_class,
                 y = log_n,
                 group = interaction(log_size_class, eco_metaeco_type),
                 fill = eco_metaeco_type,
                 color = eco_metaeco_type)) +
      geom_boxplot() +
      labs(title = paste0("Day = ", day_input), x = "log(body-size)", y = "Mean abundance + 1 (log)") +
      scale_y_continuous(limits = c(0,5))+
      scale_x_continuous(limits = c(7,10.5))}
  
  
}















################### --- PLOT SIZE CLASSE USING FUNCTIONS --- #######################################


plot_list = NULL; n=0
for (day in unique(ds$day)){
  plot = plot_data_raw(day,"low", "S")
  n=n+1
  plot_list[[n]] = temporary_plot
}
grid = grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    ncol=3, nrow=3,
                    top = textGrob("Low disturbance", gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/body_size/LS_raw.jpg", grid, width = 22, height = 13)


plot_list = NULL; n=0
for (day in unique(ds$day)){
  plot = plot_data_mean(day,"low", "S")
  n=n+1
  plot_list[[n]] = temporary_plot
}
grid = grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    ncol=3, nrow=3,
                    top = textGrob("Low disturbance", gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/body_size/LS_mean.jpg", grid, width = 22, height = 13)

plot_list = NULL; n=0
for (day in unique(ds$day)){
  plot = plot_data_raw(day,"high", "S")
  n=n+1
  plot_list[[n]] = temporary_plot
}
grid = grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    ncol=3, nrow=3,
                    top = textGrob("High disturbance", gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/body_size/HS_raw.jpg", grid, width = 22, height = 13)


plot_list = NULL; n=0
for (day in unique(ds$day)){
  plot = plot_data_mean(day,"high", "S")
  n=n+1
  plot_list[[n]] = temporary_plot
}
grid = grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    ncol=3, nrow=3,
                    top = textGrob("High disturbance", gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/body_size/HS_mean.jpg", grid, width = 22, height = 13)

plot_list = NULL; n=0
for (day in unique(ds$day)){
  plot = plot_data_raw(day,"low", "closed")
  n=n+1
  plot_list[[n]] = temporary_plot
}
grid = grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    ncol=3, nrow=3,
                    top = textGrob("Low disturbance", gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/body_size/LCLOSED_raw.jpg", grid, width = 22, height = 13)


plot_list = NULL; n=0
for (day in unique(ds$day)){
  plot = plot_data_mean(day,"low", "closed")
  n=n+1
  plot_list[[n]] = temporary_plot
}
grid = grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    ncol=3, nrow=3,
                    top = textGrob("Low disturbance", gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/body_size/LCLOSED_mean.jpg", grid, width = 22, height = 13)

plot_list = NULL; n=0
for (day in unique(ds$day)){
  plot = plot_data_raw(day,"high", "closed")
  n=n+1
  plot_list[[n]] = temporary_plot
}
grid = grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    ncol=3, nrow=3,
                    top = textGrob("High disturbance", gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/body_size/HCLOSED_raw.jpg", grid, width = 22, height = 13)


plot_list = NULL; n=0
for (day in unique(ds$day)){
  plot = plot_data_mean(day,"high", "closed")
  n=n+1
  plot_list[[n]] = temporary_plot
}
grid = grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                    plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]],
                    ncol=3, nrow=3,
                    top = textGrob("High disturbance", gp=gpar(fontsize=20,font=3)))
ggsave("/Users/ema/github/PatchSizePilot/results/body_size/HCLOSED_mean.jpg", grid, width = 22, height = 13)













##### --- PLAYING AROUND --- #####















################################# --- PLOT DENSITY DISTRIBUTIONS --- #########################################
# 
# 
# low.day.4 = ds %>%
#   filter (disturbance == "low") %>%
#   filter (day == 4, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3)  +
#   labs(title = "Day 4")
# 
# low.day.8 = ds %>%
#   filter (disturbance == "low") %>%
#   filter (day == 8, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) +
#   labs(title = "Day 8")
# 
# low.day.12 = ds %>%
#   filter (disturbance == "low") %>%
#   filter (day == 12, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) + 
#   labs(title = "Day 12")
# 
# low.day.16 = ds %>%
#   filter (disturbance == "low") %>%
#   filter (day == 16, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) + 
#   labs(title = "Day 16")
# 
# low.day.20 = ds %>%
#   filter (disturbance == "low") %>%
#   filter (day == 20, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) +
#   labs(title = "Day 20")
# 
# low.day.24 = ds %>%
#   filter (disturbance == "low") %>%
#   filter (day == 24, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) + 
#   labs(title = "Day 24")
# 
# low.day.28 = ds %>%
#   filter (disturbance == "low") %>%
#   filter (day == 28, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) +
#   labs(title = "Day 28")
# 
# high.day.4 = ds %>%
#   filter (disturbance == "high") %>%
#   filter (day == 4, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3)  +
#   labs(title = "Day 4")
# 
# high.day.8 = ds %>%
#   filter (disturbance == "high") %>%
#   filter (day == 8, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) +
#   labs(title = "Day 8")
# 
# high.day.12 = ds %>%
#   filter (disturbance == "high") %>%
#   filter (day == 12, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) + 
#   labs(title = "Day 12")
# 
# high.day.16 = ds %>%
#   filter (disturbance == "high") %>%
#   filter (day == 16, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) + 
#   labs(title = "Day 16")
# 
# high.day.20 = ds %>%
#   filter (disturbance == "high") %>%
#   filter (day == 20, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) +
#   labs(title = "Day 20")
# 
# high.day.24 = ds %>%
#   filter (disturbance == "high") %>%
#   filter (day == 24, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) + 
#   labs(title = "Day 24")
# 
# high.day.28 = ds %>%
#   filter (disturbance == "high") %>%
#   filter (day == 28, !is.na(eco_metaeco_type)) %>%
#   ggplot(aes(x = log(mean_area), fill = eco_metaeco_type)) +
#   geom_density(alpha=.3) +
#   labs(title = "Day 28")
# 
# 
# grid.arrange(high.day.4, high.day.8, high.day.12, high.day.16, high.day.20, high.day.24, high.day.28,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (high disturbance)", gp=gpar(fontsize=20,font=3)))
# 
# grid.arrange(low.day.4, low.day.8, low.day.12, low.day.16, low.day.20, low.day.24, low.day.28,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (low disturbance)", gp=gpar(fontsize=20,font=3)))






























########################## --- PLOT SIZE CLASSES - LOW DISTURBANCE - ALL PATCHES --- #######################################


# low.0.raw = 
#   ds_classes %>%
#   filter(day == 0, disturbance == "low") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 0, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5)) +
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.0.mean = 
#   ds_classes %>%
#   filter(day == 0, disturbance == "low") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 0, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.4.raw = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "low") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 4, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5)) +
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.4.mean = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "low") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 4, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.8.raw = 
#   ds_classes %>%
#     filter(day == 8, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = interaction(log_size_class, eco_metaeco_type),
#                fill = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_boxplot() +
#     labs(title = "Day = 8, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5)) +
#   scale_x_continuous(limits = c(7,10.5))
#   
# low.8.mean = 
#   ds_classes %>%
#     filter(day == 8, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_point(stat = "summary", fun = "mean") +
#     geom_line (stat = "summary", fun = "mean") +
#     labs(title = "Day = 8, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
#   
# low.12.raw = 
#   ds_classes %>%
#     filter(day == 12, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = interaction(log_size_class, eco_metaeco_type),
#                fill = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_boxplot() +
#     labs(title = "Day = 12, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
#   
# low.12.mean = 
#   ds_classes %>%
#     filter(day == 12, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_point(stat = "summary", fun = "mean") +
#     geom_line (stat = "summary", fun = "mean") +
#     labs(title = "Day = 12, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
#   
# low.16.raw = 
#   ds_classes %>%
#     filter(day == 16, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = interaction(log_size_class, eco_metaeco_type),
#                fill = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_boxplot() +
#     labs(title = "Day = 16, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.16.mean = 
#   ds_classes %>%
#     filter(day == 16, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_point(stat = "summary", fun = "mean") +
#     geom_line (stat = "summary", fun = "mean") +
#     labs(title = "Day = 16, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.20.raw = 
#   ds_classes %>%
#     filter(day == 20, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = interaction(log_size_class, eco_metaeco_type),
#                fill = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_boxplot() +
#     labs(title = "Day = 20, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
#   
# low.20.mean = 
#   ds_classes %>%
#     filter(day == 20, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_point(stat = "summary", fun = "mean") +
#     geom_line (stat = "summary", fun = "mean") +
#     labs(title = "Day = 20, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.24.raw = 
#   ds_classes %>%
#     filter(day == 24, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = interaction(log_size_class, eco_metaeco_type),
#                fill = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_boxplot() +
#     labs(title = "Day = 24, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
#   
# low.24.mean = 
#   ds_classes %>%
#     filter(day == 24, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_point(stat = "summary", fun = "mean") +
#     geom_line (stat = "summary", fun = "mean") +
#     labs(title = "Day = 24, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
#   
# low.28.raw = 
#   ds_classes %>%
#     filter(day == 28, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = interaction(log_size_class, eco_metaeco_type),
#                fill = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_boxplot() +
#     labs(title = "Day = 28, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
#   
# low.28.mean = 
#   ds_classes %>%
#     filter(day == 28, disturbance == "low") %>%
#     ggplot(aes(x = log_size_class,
#                y = log_n,
#                group = eco_metaeco_type,
#                color = eco_metaeco_type)) +
#     geom_point(stat = "summary", fun = "mean") +
#     geom_line (stat = "summary", fun = "mean") +
#     labs(title = "Day = 28, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#     scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# 
# grid = grid.arrange(low.0.raw, low.4.raw, low.8.raw, low.12.raw, low.16.raw, low.20.raw, low.24.raw, low.28.raw,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (low disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/l_all_raw.jpg", grid, width = 22, height = 13)
# 
# 
# grid = grid.arrange(low.0.mean, low.4.mean, low.8.mean, low.12.mean, low.16.mean, low.20.mean, low.24.mean, low.28.mean,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (low disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/l_all_mean.jpg", grid, width = 22, height = 13)
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
# 
# ########################## --- PLOT SIZE CLASSES - LOW DISTURBANCE - SMALL PATCHES --- #######################################
# 
# 
# low.0.raw = 
#   ds_classes %>%
#   filter(day == 0, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 0, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.0.mean = 
#   ds_classes %>%
#   filter(day == 0, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 0, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.4.raw = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 4, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.4.mean = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 4, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.8.raw = 
#   ds_classes %>%
# filter(day == 8, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 8, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.8.mean = 
#   ds_classes %>%
# filter(day == 8, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 8, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.12.raw = 
#   ds_classes %>%
# filter(day == 12, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 12, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.12.mean = 
#   ds_classes %>%
# filter(day == 12, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 12, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.16.raw = 
#   ds_classes %>%
# filter(day == 16, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 16, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.16.mean = 
#   ds_classes %>%
# filter(day == 16, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 16, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.20.raw = 
#   ds_classes %>%
# filter(day == 20, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 20, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.20.mean = 
#   ds_classes %>%
# filter(day == 20, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 20, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.24.raw = 
#   ds_classes %>%
# filter(day == 24, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 24, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.24.mean = 
#   ds_classes %>%
# filter(day == 24, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 24, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.28.raw = 
#   ds_classes %>%
# filter(day == 28, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 28, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.28.mean = 
#   ds_classes %>%
# filter(day == 28, disturbance == "low", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 28, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# 
# grid = grid.arrange(low.0.raw, low.4.raw, low.8.raw, low.12.raw, low.16.raw, low.20.raw, low.24.raw, low.28.raw,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (low disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/l_small_raw.jpg", grid, width = 22, height = 13)
# 
# 
# 
# grid.arrange(low.0.raw, low.4.mean, low.8.mean, low.12.mean, low.16.mean, low.20.mean, low.24.mean, low.28.mean,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (low disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/l_small_mean.jpg", grid, width = 22, height = 13)
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
# ########################## --- PLOT SIZE CLASSES - LOW DISTURBANCE - CLOSED ECOSYSTEMS --- #######################################
# 
# 
# low.0.raw = 
#   ds_classes %>%
#   filter(day == 0, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 0, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.0.mean = 
#   ds_classes %>%
#   filter(day == 0, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 0, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.4.raw = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 4, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.4.mean = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 4, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.8.raw = 
#   ds_classes %>%
#   filter(day == 8, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 8, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.8.mean = 
#   ds_classes %>%
#   filter(day == 8, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 8, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.12.raw = 
#   ds_classes %>%
#   filter(day == 12, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 12, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.12.mean = 
#   ds_classes %>%
#   filter(day == 12, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 12, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.16.raw = 
#   ds_classes %>%
#   filter(day == 16, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 16, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.16.mean = 
#   ds_classes %>%
#   filter(day == 16, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 16, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.20.raw = 
#   ds_classes %>%
#   filter(day == 20, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 20, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.20.mean = 
#   ds_classes %>%
#   filter(day == 20, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 20, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.24.raw = 
#   ds_classes %>%
#   filter(day == 24, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 24, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.24.mean = 
#   ds_classes %>%
#   filter(day == 24, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 24, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.28.raw = 
#   ds_classes %>%
#   filter(day == 28, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 28, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# low.28.mean = 
#   ds_classes %>%
#   filter(day == 28, disturbance == "low", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 28, Disturbance = low", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# 
# grid = grid.arrange(low.0.raw, low.4.raw, low.8.raw, low.12.raw, low.16.raw, low.20.raw, low.24.raw, low.28.raw,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (low disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/l_closed_raw.jpg", grid, width = 22, height = 13)
# 
# 
# grid = grid.arrange(low.0.mean, low.4.mean, low.8.mean, low.12.mean, low.16.mean, low.20.mean, low.24.mean, low.28.mean,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (low disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/l_closed_mean.jpg", grid, width = 22, height = 13)
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
# ########################## --- PLOT SIZE CLASSES - HIGH DISTURBANCE - ALL PATCHES --- #######################################
# 
# 
# high.0.raw = 
#   ds_classes %>%
#   filter(day == 0, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 0, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.0.mean = 
#   ds_classes %>%
#   filter(day == 0, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 0, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.4.raw = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 4, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.4.mean = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 4, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.8.raw = 
#   ds_classes %>%
#   filter(day == 8, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 8, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.8.mean = 
#   ds_classes %>%
#   filter(day == 8, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 8, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.12.raw = 
#   ds_classes %>%
#   filter(day == 12, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 12, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.12.mean = 
#   ds_classes %>%
#   filter(day == 12, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 12, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.16.raw = 
#   ds_classes %>%
#   filter(day == 16, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 16, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.16.mean = 
#   ds_classes %>%
#   filter(day == 16, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 16, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.20.raw = 
#   ds_classes %>%
#   filter(day == 20, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 20, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.20.mean = 
#   ds_classes %>%
#   filter(day == 20, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 20, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.24.raw = 
#   ds_classes %>%
#   filter(day == 24, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 24, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.24.mean = 
#   ds_classes %>%
#   filter(day == 24, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 24, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.28.raw = 
#   ds_classes %>%
#   filter(day == 28, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 28, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.28.mean = 
#   ds_classes %>%
#   filter(day == 28, disturbance == "high") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 28, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# 
# grid = grid.arrange(high.0.raw, high.4.raw, high.8.raw, high.12.raw, high.16.raw, high.20.raw, high.24.raw, high.28.raw,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (high disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/h_all_raw.jpg", grid, width = 22, height = 13)
# 
# 
# grid = grid.arrange(high.0.mean, high.4.mean, high.8.mean, high.12.mean, high.16.mean, high.20.mean, high.24.mean, high.28.mean,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (high disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/h_all_mean.jpg", grid, width = 22, height = 13)
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
# ########################## --- PLOT SIZE CLASSES - HIGH DISTURBANCE - SMALL PATCHES --- #######################################
# 
# 
# high.0.raw = 
#   ds_classes %>%
#   filter(day == 0, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 0, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.0.mean = 
#   ds_classes %>%
#   filter(day == 0, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 0, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.4.raw = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 4, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.4.mean = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 4, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.8.raw = 
#   ds_classes %>%
#   filter(day == 8, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 8, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.8.mean = 
#   ds_classes %>%
#   filter(day == 8, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 8, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.12.raw = 
#   ds_classes %>%
#   filter(day == 12, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 12, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.12.mean = 
#   ds_classes %>%
#   filter(day == 12, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 12, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.16.raw = 
#   ds_classes %>%
#   filter(day == 16, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 16, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.16.mean = 
#   ds_classes %>%
#   filter(day == 16, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 16, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.20.raw = 
#   ds_classes %>%
#   filter(day == 20, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 20, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.20.mean = 
#   ds_classes %>%
#   filter(day == 20, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 20, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.24.raw = 
#   ds_classes %>%
#   filter(day == 24, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 24, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.24.mean = 
#   ds_classes %>%
#   filter(day == 24, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 24, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.28.raw = 
#   ds_classes %>%
#   filter(day == 28, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 28, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.28.mean = 
#   ds_classes %>%
#   filter(day == 28, disturbance == "high", patch_size == "S") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 28, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# 
# grid = grid.arrange(high.0.raw, high.4.raw, high.8.raw, high.12.raw, high.16.raw, high.20.raw, high.24.raw, high.28.raw,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (high disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/h_small_raw.jpg", grid, width = 22, height = 13)
# 
# 
# 
# grid = grid.arrange(high.0.mean, high.4.mean, high.8.mean, high.12.mean, high.16.mean, high.20.mean, high.24.mean, high.28.mean,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (high disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/h_small_mean.jpg", grid, width = 22, height = 13)
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
# ########################## --- PLOT SIZE CLASSES - HIGH DISTURBANCE - CLOSED ECOSYSTEMS --- #######################################
# 
# 
# high.4.raw = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 4, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.4.mean = 
#   ds_classes %>%
#   filter(day == 4, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 4, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.8.raw = 
#   ds_classes %>%
#   filter(day == 8, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 8, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.8.mean = 
#   ds_classes %>%
#   filter(day == 8, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 8, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.12.raw = 
#   ds_classes %>%
#   filter(day == 12, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 12, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.12.mean = 
#   ds_classes %>%
#   filter(day == 12, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 12, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.16.raw = 
#   ds_classes %>%
#   filter(day == 16, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 16, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.16.mean = 
#   ds_classes %>%
#   filter(day == 16, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 16, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.20.raw = 
#   ds_classes %>%
#   filter(day == 20, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 20, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.20.mean = 
#   ds_classes %>%
#   filter(day == 20, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 20, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.24.raw = 
#   ds_classes %>%
#   filter(day == 24, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 24, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.24.mean = 
#   ds_classes %>%
#   filter(day == 24, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 24, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.28.raw = 
#   ds_classes %>%
#   filter(day == 28, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = interaction(log_size_class, eco_metaeco_type),
#              fill = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_boxplot() +
#   labs(title = "Day = 28, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# high.28.mean = 
#   ds_classes %>%
#   filter(day == 28, disturbance == "high", metaecosystem == "no") %>%
#   ggplot(aes(x = log_size_class,
#              y = log_n,
#              group = eco_metaeco_type,
#              color = eco_metaeco_type)) +
#   geom_point(stat = "summary", fun = "mean") +
#   geom_line (stat = "summary", fun = "mean") +
#   labs(title = "Day = 28, Disturbance = high", x = "log(body-size)", y = "Mean abundance + 1 (log)") +
#   scale_y_continuous(limits = c(0,5))+
#   scale_x_continuous(limits = c(7,10.5))
# 
# 
# grid = grid.arrange(high.4.raw, high.8.raw, high.12.raw, high.16.raw, high.20.raw, high.24.raw, high.28.raw,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (high disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/h_closed_raw.jpg", grid, width = 22, height = 13)
# 
# grid = grid.arrange(high.4.mean, high.8.mean, high.12.mean, high.16.mean, high.20.mean, high.24.mean, high.28.mean,
#              ncol=3, nrow=3,
#              top = textGrob("Body size distribution (high disturbance)", gp=gpar(fontsize=20,font=3)))
# ggsave("/Users/ema/github/PatchSizePilot/results/body_size/h_closed_mean.jpg", grid, width = 22, height = 13)
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
