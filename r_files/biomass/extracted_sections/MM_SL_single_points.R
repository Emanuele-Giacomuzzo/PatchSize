## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full = lm(regional_mean_bioarea ~
              metaecosystem_type +
              disturbance +
              disturbance : metaecosystem_type,
            data = ds_regional %>%
              filter(time_point == 3) %>%
              filter(metaecosystem_type == "M_M" |
                     metaecosystem_type == "S_L"))


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
no_MD = lm(regional_mean_bioarea ~
              metaecosystem_type +
              disturbance,
            data = ds_regional %>%
              filter(time_point == 3) %>%
              filter(metaecosystem_type == "M_M" |
                     metaecosystem_type == "S_L"))

anova(full, no_MD)
AIC(full, no_MD)

