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



long_t0 = t0 %>% 
  slice(rep(1:n(), max(culture_info$culture_ID)))

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


size_classes = seq(0, max(ds$mean_area), 
                   by = max(ds$mean_area)/12) #As in "How pulse disturbances shape size-abundance pyramids"

elongating_size_classes = NULL
for (class in 1:length(size_classes)){
  
  bin_lower_limit = size_classes[class]
  bin_upper_limit = size_classes[class+1]
  
  size_class = ds%>%
    filter(bin_lower_limit<=mean_area, mean_area <= bin_upper_limit) %>%
    group_by(culture_ID, system_nr, disturbance, day, patch_size, metaecosystem, metaecosystem_type, eco_metaeco_type, replicate_video) %>% #Group by video
    summarise(abundance = n()) %>%
    group_by(culture_ID, system_nr, disturbance, day, patch_size, metaecosystem, metaecosystem_type, eco_metaeco_type) %>% #Group by ID
    summarise(abundance = mean(abundance))
  size_class$size_class = class
  elongating_size_classes = rbind(elongating_size_classes, size_class)
  
}

ds_classes = elongating_size_classes
ds_classes$log_abundance = log(ds_classes$abundance + 1)

ds_classes$log_size_class = 0 #initialise
for (i in 1:length(size_classes)){
  
  ds_classes$log_size_class[ds_classes$size_class==i] = log(mean(c(size_classes[i] + size_classes[i+1])))
  
}
ds_classes$log_size_class = round(ds_classes$log_size_class, digits = 1)
















################### --- SIZE CLASS MULTIPLE ECOSYSTEM PLOT FUNCTIONS --- #######################################


plot_data_mean = function (day_input, disturbance_input, patch_type){
  
  if (patch_type == "S") {
    
    temporary_plot <<- ds_classes %>%
      filter(day == day_input, disturbance == disturbance_input, patch_size == "S") %>%
      ggplot(aes(x = log_size_class,
                 y = log_abundance,
                 group = eco_metaeco_type,
                 color = eco_metaeco_type)) +
      geom_point(stat = "summary", fun = "mean") +
      geom_line (stat = "summary", fun = "mean") +
      labs(title = paste0("Day = ", day_input), x = "log body size (µm2)", y = "log mean abundance + 1 (indiv/µl)") +
      labs(color='Patch type')  +
      labs(fill='Patch type') +
      scale_y_continuous(limits = c(0,5)) +
      scale_x_continuous(limits = c(7,10.5))}
  
  if (patch_type == "closed") {
    
    temporary_plot <<- ds_classes %>%
      filter(day == day_input, disturbance == disturbance_input, metaecosystem == "no") %>%
      ggplot(aes(x = log_size_class,
                 y = log_abundance,
                 group = eco_metaeco_type,
                 color = eco_metaeco_type)) +
      geom_point(stat = "summary", fun = "mean") +
      geom_line (stat = "summary", fun = "mean") +
      labs(title = paste0("Day = ", day_input), x = "log body size (µm2)", y = "log mean abundance + 1 (indiv/µl)") +
      labs(color='Patch type')  +
      labs(fill='Patch type') +
      scale_y_continuous(limits = c(0,5))+
      scale_x_continuous(limits = c(7,10.5))}
  
  
}

plot_data_raw = function (day_input, disturbance_input, patch_type){
  
  if (patch_type == "S") {
    
    temporary_plot <<-  ds_classes %>%
      filter(day == day_input, disturbance == disturbance_input, patch_size == "S") %>%
      ggplot(aes(x = log_size_class,
                 y = log_abundance,
                 group = interaction(log_size_class, eco_metaeco_type),
                 fill = eco_metaeco_type,
                 color = eco_metaeco_type)) +
      geom_boxplot() +
      labs(title = paste0("Day = ", day_input), x = "log body size (µm2)", y = "log mean abundance + 1 (indiv/µl)") +
      labs(color='Patch type')  +
      labs(fill='Patch type') +
      scale_y_continuous(limits = c(0,5))+
      scale_x_continuous(limits = c(7,10.5))}
  
  if (patch_type == "closed") {
    
    temporary_plot <<- ds_classes %>%
      filter(day == day_input, disturbance == disturbance_input, metaecosystem == "no") %>%
      ggplot(aes(x = log_size_class,
                 y = log_abundance,
                 group = interaction(log_size_class, eco_metaeco_type),
                 fill = eco_metaeco_type,
                 color = eco_metaeco_type)) +
      geom_boxplot() +
      labs(title = paste0("Day = ", day_input), x = "log body size (µm2)", y = "log mean abundance + 1 (indiv/µl)") +
      labs(color='Patch type')  +
      labs(fill='Patch type') +
      scale_y_continuous(limits = c(0,5))+
      scale_x_continuous(limits = c(7,10.5))}
  
  
}















################### --- SIZE CLASS MULTIPLE ECOSYSTEM PLOTS --- #######################################


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
#ggsave("/Users/ema/github/PatchSizePilot/results/body_size/small_low_raw.pdf", grid, width = 22, height = 13)


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
#ggsave("/Users/ema/github/PatchSizePilot/results/body_size/small_low_mean.pdf", grid, width = 22, height = 13)

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
#ggsave("/Users/ema/github/PatchSizePilot/results/body_size/small_high_raw.pdf", grid, width = 22, height = 13)


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
#ggsave("/Users/ema/github/PatchSizePilot/results/body_size/small_high_mean.pdf", grid, width = 22, height = 13)

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
#ggsave("/Users/ema/github/PatchSizePilot/results/body_size/closed_low_raw.pdf", grid, width = 22, height = 13)


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
#ggsave("/Users/ema/github/PatchSizePilot/results/body_size/closed_low_mean.pdf", grid, width = 22, height = 13)

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
#ggsave("/Users/ema/github/PatchSizePilot/results/body_size/closed_high_raw.pdf", grid, width = 22, height = 13)


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
#ggsave("/Users/ema/github/PatchSizePilot/results/body_size/closed_high_mean.pdf", grid, width = 22, height = 13)















################### --- SIZE CLASS SMALL ECOSYSTEMS OVER TIME --- #######################################

ds_classes %>%
  filter(eco_metaeco_type == "S") %>%
  ggplot(aes(x = log_size_class,
             y = log_abundance,
             group = interaction(log_size_class, day),
             fill = day,
             color = day)) +
  geom_boxplot()  +
  scale_color_gradient(low="blue", high="yellow") +
  scale_fill_gradient(low="blue", high="yellow")

  
ds_classes %>%
  filter(eco_metaeco_type == "S") %>%
  
  ggplot(aes(x = log_size_class,
             y = log_abundance,
             group = interaction(log_size_class, day),
             fill = day,
             color = day)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean", aes(group = day)) +
  scale_color_gradient(low="blue", high="yellow") +
  scale_fill_gradient(low="blue", high="yellow")















################### --- SINGLE SIZE CLASS OVER TIME --- #######################################

ds_classes %>%
  filter(size_class == min(ds_classes$size_class)) %>%
  filter(metaecosystem == "no") %>%
  ggplot(aes(x = day,
             y = log_abundance,
             group = interaction(day,eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot()

ds_classes %>%
  filter(size_class == min(ds_classes$size_class)) %>%
  filter(metaecosystem == "no") %>%
  ggplot(aes(x = day,
             y = log_abundance,
             group = interaction(day,eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean", aes(group = eco_metaeco_type)) 

ds_classes %>%
  filter(size_class == min(ds_classes$size_class)) %>%
  filter(patch_size == "S") %>%
  ggplot(aes(x = day,
             y = log_abundance,
             group = interaction(day,eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_boxplot()

ds_classes %>%
  filter(size_class == min(ds_classes$size_class)) %>%
  filter(patch_size == "S") %>%
  ggplot(aes(x = day,
             y = log_abundance,
             group = interaction(day,eco_metaeco_type),
             fill = eco_metaeco_type,
             color = eco_metaeco_type)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean", aes(group = eco_metaeco_type)) 

# for (day in 1:unique(ds_classes$day)){
#   
#   ds_classes %>%
#     
#   
# }