## ------------------------------------------------------------------------------------------------------------------------------------
chosen_time_point = 1


## ------------------------------------------------------------------------------------------------------------------------------------
full = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))


## ------------------------------------------------------------------------------------------------------------------------------------
no_D = lm(total_regional_bioarea ~
            metaecosystem_type +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_D)


## ------------------------------------------------------------------------------------------------------------------------------------
no_M = lm(total_regional_bioarea ~
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_M)


## ------------------------------------------------------------------------------------------------------------------------------------
chosen_time_point = 2


## ------------------------------------------------------------------------------------------------------------------------------------
full = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))


## ------------------------------------------------------------------------------------------------------------------------------------
no_M = lm(total_regional_bioarea ~
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_M)


## ------------------------------------------------------------------------------------------------------------------------------------
no_D = lm(total_regional_bioarea ~
            metaecosystem_type +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_D)


## ------------------------------------------------------------------------------------------------------------------------------------
no_MD = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_MD)


## ------------------------------------------------------------------------------------------------------------------------------------
best_model = no_MD

par(mfrow=c(2,3))
plot(best_model, which = 1:5)

R2_full = glance(best_model)$r.squared

no_M = lm(total_regional_bioarea ~
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

R2_no_M = glance(no_M)$r.squared
R2_M = R2_full - R2_no_M

R2_full = round(R2_full, digits = 2)
R2_M = round(R2_M, digits = 2)


## ------------------------------------------------------------------------------------------------------------------------------------
chosen_time_point = 3


## ------------------------------------------------------------------------------------------------------------------------------------
full = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))


## ------------------------------------------------------------------------------------------------------------------------------------
no_M = lm(total_regional_bioarea ~
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_M)


## ------------------------------------------------------------------------------------------------------------------------------------
no_D = lm(total_regional_bioarea ~
            metaecosystem_type +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_D)


## ------------------------------------------------------------------------------------------------------------------------------------
no_MD = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_MD)


## ------------------------------------------------------------------------------------------------------------------------------------
best_model = full

par(mfrow=c(2,3))
plot(best_model, which = 1:5)

R2_full = glance(best_model)$r.squared

no_M = lm(total_regional_bioarea ~
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

R2_no_M = glance(no_M)$r.squared
R2_M = R2_full - R2_no_M

R2_full = round(R2_full, digits = 2)
R2_M = round(R2_M, digits = 2)


## ------------------------------------------------------------------------------------------------------------------------------------
chosen_time_point = 4


## ------------------------------------------------------------------------------------------------------------------------------------
full = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))


## ------------------------------------------------------------------------------------------------------------------------------------
no_M = lm(total_regional_bioarea ~
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_M)


## ------------------------------------------------------------------------------------------------------------------------------------
no_D = lm(total_regional_bioarea ~
            metaecosystem_type +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_D)


## ------------------------------------------------------------------------------------------------------------------------------------
no_MD = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_MD)


## ------------------------------------------------------------------------------------------------------------------------------------
best_model = no_MD

par(mfrow=c(2,3))
plot(best_model, which = 1:5)

R2_full = glance(best_model)$r.squared

no_M = lm(total_regional_bioarea ~
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

R2_no_M = glance(no_M)$r.squared
R2_M = R2_full - R2_no_M

R2_full = round(R2_full, digits = 2)
R2_M = round(R2_M, digits = 2)


## ------------------------------------------------------------------------------------------------------------------------------------
chosen_time_point = 5


## ------------------------------------------------------------------------------------------------------------------------------------
full = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))


## ------------------------------------------------------------------------------------------------------------------------------------
no_M = lm(total_regional_bioarea ~
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_M)


## ------------------------------------------------------------------------------------------------------------------------------------
no_D = lm(total_regional_bioarea ~
            metaecosystem_type +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_D)


## ------------------------------------------------------------------------------------------------------------------------------------
no_MD = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_MD)


## ------------------------------------------------------------------------------------------------------------------------------------
best_model = no_MD

par(mfrow=c(2,3))
plot(best_model, which = 1:5)

R2_full = glance(best_model)$r.squared

no_M = lm(total_regional_bioarea ~
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

R2_no_M = glance(no_M)$r.squared
R2_M = R2_full - R2_no_M

R2_full = round(R2_full, digits = 2)
R2_M = round(R2_M, digits = 2)


## ------------------------------------------------------------------------------------------------------------------------------------
chosen_time_point = 6


## ------------------------------------------------------------------------------------------------------------------------------------
full = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))


## ------------------------------------------------------------------------------------------------------------------------------------
no_M = lm(total_regional_bioarea ~
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_M)


## ------------------------------------------------------------------------------------------------------------------------------------
no_D = lm(total_regional_bioarea ~
            metaecosystem_type +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_D)


## ------------------------------------------------------------------------------------------------------------------------------------
no_MD = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_MD)


## ------------------------------------------------------------------------------------------------------------------------------------
best_model = no_MD

par(mfrow=c(2,3))
plot(best_model, which = 1:5)

R2_full = glance(best_model)$r.squared

no_M = lm(total_regional_bioarea ~
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

R2_no_M = glance(no_M)$r.squared
R2_M = R2_full - R2_no_M

R2_full = round(R2_full, digits = 2)
R2_M = round(R2_M, digits = 2)


## ------------------------------------------------------------------------------------------------------------------------------------
chosen_time_point = 7


## ------------------------------------------------------------------------------------------------------------------------------------
full = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))


## ------------------------------------------------------------------------------------------------------------------------------------
no_M = lm(total_regional_bioarea ~
            disturbance +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_M)


## ------------------------------------------------------------------------------------------------------------------------------------
no_D = lm(total_regional_bioarea ~
            metaecosystem_type +
            metaecosystem_type * disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_D)


## ------------------------------------------------------------------------------------------------------------------------------------
no_MD = lm(total_regional_bioarea ~
            metaecosystem_type +
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

AIC(full,no_MD)


## ------------------------------------------------------------------------------------------------------------------------------------
best_model = no_MD

par(mfrow=c(2,3))
plot(best_model, which = 1:5)

R2_full = glance(best_model)$r.squared

no_M = lm(total_regional_bioarea ~
            disturbance,
          data = ds_regional_biomass %>%
                            filter(time_point == chosen_time_point) %>%
                            filter(metaecosystem_type == "M_M" | metaecosystem_type == "S_L"))

R2_no_M = glance(no_M)$r.squared
R2_M = R2_full - R2_no_M

R2_full = round(R2_full, digits = 2)
R2_M = round(R2_M, digits = 2)

