## ------------------------------------------------------------------------------------------------------------------------------------------
full = lm(regional_mean_bioarea ~
              metaecosystem_type +
              disturbance +
              disturbance : metaecosystem_type,
            data = ds_regional %>%
              filter(time_point == 3) %>%
              filter(metaecosystem_type == "M_M" |
                     metaecosystem_type == "S_L"))


## ------------------------------------------------------------------------------------------------------------------------------------------
no_MD = lm(regional_mean_bioarea ~
              metaecosystem_type +
              disturbance,
            data = ds_regional %>%
              filter(time_point == 3) %>%
              filter(metaecosystem_type == "M_M" |
                     metaecosystem_type == "S_L"))

anova(full, no_MD)
AIC(full, no_MD)


## ------------------------------------------------------------------------------------------------------------------------------------------
columns = c("time_point", "R2", "R2_M")
single_points = matrix(ncol = length(columns), 
                       nrow = 7)
single_points = as.data.frame(single_points)
colnames(single_points) = columns

for (t in 1:7) {
  
  full_model = lm(regional_mean_bioarea ~ 
               disturbance +
               metaecosystem_type,
             data = ds_regional %>%
               filter(metaecosystem_type == "M_M" |
                      metaecosystem_type == "S_L") %>%
               filter(time_point == t))

  no_M_model = lm(regional_mean_bioarea ~ 
              disturbance,
            data = ds_regional %>%
              filter(metaecosystem_type == "M_M" |
                      metaecosystem_type == "S_L") %>%
              filter(time_point == t))
  
  R2_full_model = summary(full_model)$adj.r.squared
  R2_no_M_model = summary(no_M_model)$adj.r.squared
  R2_M = R2_full_model - R2_no_M_model
  
  single_points$time_point[t] = t
  single_points$R2[t] = R2_full_model
  single_points$R2_M[t] = R2_M
  
}

single_points = round(single_points, digits = 2)
single_points = single_points[2:nrow(single_points),]
datatable(single_points,
          rownames = FALSE,
          colnames = c("Time point", "R2 model", "R2 meta-ecosystem type"))

