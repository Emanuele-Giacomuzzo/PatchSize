rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("gridExtra")
library(lme4)
setwd('/Volumes/GoogleDrive/My\ Drive/study_n_work/7_PhD/projects/PatchSizePilot')

# --- IMPORT & TRANSFORM DATA --- #

df = read.csv( file = "data/PatchSizePilot_dataset.csv", header = TRUE )

#Transform data from wide to long & change name of the columns
df$id = as.character(df$id)
df$ex1 = df$water_add_before_t3/2
df$ex2 = df$ex1
df$ex3 = df$water_add_after_t3
df$ex4 = df$water_add_after_t4
df$ex5 = df$water_add_after_t5
df$ex6 = df$water_add_after_t6
df$disturbance = df$disturbance.1
df$disturbance.1 = NULL
df$water_add_before_t3 = NULL
df$water_add_after_t3 = NULL
df$water_add_after_t4 = NULL
df$water_add_after_t5 = NULL
df$water_add_after_t6 = NULL
df = gather(df, exchange, evaporation, ex1:ex6)

# --- PLOTS --- #

#Evaporation ~ time
df %>%
  filter(exchange != 'ex1') %>%
  ggplot(aes (x = exchange,
              y = evaporation)) + 
  geom_boxplot() + 
  geom_jitter()

#Evaporation ~ patch size
df$patch_size = as.character(df$patch_size)
df %>%
  ggplot(aes (x = patch_size,
              y = evaporation)) + 
  geom_boxplot() + 
  geom_jitter()

#Evaporation ~ disturbance
df$disturbance = as.character(df$disturbance)
df %>%
  ggplot(aes (x = disturbance,
              y = evaporation)) + 
  geom_boxplot() + 
  geom_jitter()

#Evaporation ~ tube id
df %>%
  ggplot(aes (x = reorder(id, sort(as.numeric(id))),
              y = evaporation)) +
  geom_boxplot() +
  theme(legend.position="none") +
  xlab("Tube ID") +
  ylab("Water evaporation (ml)") 

#Evaporation ~ patch size * disturbance
df$disturbance = as.character(df$disturbance)
df$patch_size = as.character(df$patch_size)
df %>%
  ggplot(aes (x = disturbance,
              y = evaporation,
              fill = patch_size)) + 
  geom_boxplot() + 
  geom_jitter()

# --- LINEAR REGRESSIONS --- #

#Evaporation ~ patch_size + disturbance + patch_size*disturbance
mr.model = lm(evaporation  ~ patch_size + disturbance  + patch_size*disturbance, data=df)
summary(mr.model)
dev.off(dev.list()["RStudioGD"])
par(mfrow = c(2,3))
plot(mr.model, which = 1:5)

#Evaporation ~ patch_size
df$patch_size = as.numeric(df$patch_size)
plot(x = df$patch_size, y = df$evaporation)
patch.size.model = lm(evaporation ~ patch_size, data=df)
summary(patch.size.model)
coef(patch.size.model)
abline(patch.size.model)
dev.off(dev.list()["RStudioGD"])
par(mfrow = c(2,3))
plot(patch.size.model, which = 1:5)

#Evaporation ~ disturbance
df$disturbance = as.numeric(df$disturbance)
dev.off(dev.list()["RStudioGD"])
plot(x = df$disturbance, y = df$evaporation)
disturbance.model = lm(evaporation ~ disturbance, data = df)
summary(disturbance.model)
coef(disturbance.model)
abline(disturbance.model)
dev.off(dev.list()["RStudioGD"])
par(mfrow = c(2,3))
plot(disturbance.model, which = 1:5)

#Evaporation ~ disturbance*patch_size
dev.off(dev.list()["RStudioGD"])
plot(x = df$disturbance * df$patch_size, y = df$evaporation)
interaction.model = lm(evaporation ~ disturbance*patch_size, data = df)
summary(interaction.model)
coef(interaction.model)
abline(interaction.model) #Doesn't work 
dev.off(dev.list()["RStudioGD"])
par(mfrow = c(2,3))
plot(interaction.model, which = 1:5)
#The normal Q-Q plot doesn't look great. Let's try to transform.

#Evaporation ~ log(disturbance)*patch_size
df$disturbance = as.numeric(df$disturbance)
interaction.model = lm(evaporation ~ disturbance*patch_size, data = df)
summary(interaction.model)
coef(interaction.model)
dev.off(dev.list()["RStudioGD"])
par(mfrow = c(2,3))
plot(interaction.model, which = 1:5)
#Can't transform the variables. Every time I try to transform data, it gives me the same exact result. Is it
#because when we have interacting variables it doens't make sense to transform them, as they become categorical?



#----MIXED EFFECT MODEL (CHECK WHETHER TREATMENTS INFLUENCED WATER EVAPORATION, BY INCLUDING THE RANDOM EFFECTS OF TUBE ID)----#
#As we want to check whether treatments affected the evaporation rate of the tubes, we are going to using a mulitple regression model. Because there might an effect of the tube identity
#I'm going to run a mixed effect model. The identity of the tube will be the random effect. We want to quantify how much the treatments  
#affected the evaporation rate. Treatments: time (here referred to as exchange), patch size, and disturbance. 
#1: construct model
mixed.model = lmer(evaporation  ~ exchange + patch_size + disturbance  + exchange*patch_size + exchange*disturbance + patch_size*disturbance + exchange*patch_size*disturbance + 
                     (1 + exchange|id) + (1 + patch_size|id) + (1 + disturbance|id) + (1 + exchange*patch_size|id) + (1 + exchange*disturbance|id) + (1 + patch_size*disturbance|id) + 
                     (1 + exchange*patch_size*disturbance|id), data=df)
#It tells me that the number of observations is less than the number of random effects.
#The error is as follows:
#
#Error: number of observations (=658) <= number of random effects (=660) for term (1 + exchange | id); 
#the random-effects parameters and the residual variance (or scale parameter) are probably unidentifiable
#
#I don't understand why this error pops up now but it didn't in the previous model. The previous model
#was as follows:
#mixed.model = lmer(evaporation  ~ patch_size + disturbance  + patch_size*disturbance + (1 + patch_size|id) + (1 + disturbance|id) + (1 + patch_size*disturbance|id), data=df, REML=FALSE)
#Is it possible that the problem was that in the previous code I didn't include "REML = FALSE"? No, it gives me exactly the same error.
#Let's then try to model without the slopes. Would that be possible?
mixed.model = lmer(evaporation  ~ exchange + patch_size + disturbance  + exchange*patch_size + exchange*disturbance + patch_size*disturbance + exchange*patch_size*disturbance + 
                     (1 + exchange|id) + (1 + patch_size|id) + (1 + disturbance|id) + (1 + exchange*patch_size|id) + (1 + exchange*disturbance|id) + (1 + patch_size*disturbance|id) + 
                     (1 + exchange*patch_size*disturbance|id), data=df)



# mixed.model = lmer(evaporation  ~ exchange + patch_size + disturbance  + exchange*patch_size + exchange*disturbance + patch_size*disturbance + exchange*patch_size*disturbance + 
#                      (1 + exchange|id) + (1 + patch_size|id) + (1 + disturbance|id) + (1 + exchange*patch_size|id) + (1 + exchange*disturbance|id) + (1 + patch_size*disturbance|id) + 
#                      (1 + exchange*patch_size*disturbance|id), data=df)
# # summary(mixed.model)
# #What are scaled residuals? (maybe not that important) The max scaled residual is 4.2, which seems quite high to me as the largest evaporation rate was 3. The residuals in the model
# #are then not displayed but only their standard deviation. The variance associated with the id of the tubes is 0. Does this mean that I should keep the mixed effect model, or should I
# #take off the random part and just go for the multiple regression? I guess yes, but only after I have found the model to be significant and respect my assumptions.  
# 
# #2: find p-values for intercepts (likelihood ratio test)
# 
# #p-value: patch size
# mixed.model = lmer(evaporation  ~ patch_size + disturbance  + patch_size*disturbance + (1 + patch_size|id) + (1 + disturbance|id) + (1 + patch_size*disturbance|id), data=df, REML=FALSE)
# mixed.model.null = lmer(evaporation  ~ disturbance  + patch_size*disturbance + (1 + patch_size|id) + (1 + disturbance|id) + (1 + patch_size*disturbance|id), data=df, REML=FALSE)
# anova(mixed.model.null, mixed.model)
# #Same BIC
# #Why are df = 0 ???
# 
# #p-value: disturbance
# mixed.model = lmer(evaporation  ~ patch_size + disturbance  + patch_size*disturbance + (1 + patch_size|id) + (1 + disturbance|id) + (1 + patch_size*disturbance|id), data=df, REML=FALSE)
# mixed.model.null = lmer(evaporation  ~ patch_size  + patch_size*disturbance + (1 + patch_size|id) + (1 + disturbance|id) + (1 + patch_size*disturbance|id), data=df, REML=FALSE)
# anova(mixed.model.null, mixed.model)
# #Same BIC
# #Why are df = 0 ????
# 
# #p-value: patch size * disturbance
# mixed.model = lmer(evaporation  ~ patch_size + disturbance  + patch_size*disturbance + (1 + patch_size|id) + (1 + disturbance|id) + (1 + patch_size*disturbance|id), data=df, REML=FALSE)
# mixed.model.null = lmer(evaporation  ~ patch_size  + (1 + patch_size|id) + (1 + disturbance|id) + (1 + patch_size*disturbance|id), data=df, REML=FALSE)
# anova(mixed.model.null, mixed.model)
# #Significant!
# 
# #3: diagnostics
# mixed.model = lmer(evaporation  ~ patch_size + disturbance  + patch_size*disturbance + (1 + patch_size|id) + (1 + disturbance|id) + (1 + patch_size*disturbance|id), data=df, REML=FALSE)
# summary(mixed.model)
# dev.off(dev.list()["RStudioGD"])
# plot(mixed.model, which = 1)
# qqnorm(residuals(mixed.model))