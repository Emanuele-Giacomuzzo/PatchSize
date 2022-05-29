rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("gridExtra")
library(lme4)
setwd('/Volumes/GoogleDrive/My\ Drive/study_n_work/7_PhD/projects/PatchSizePilot')

# --- IMPORT & TRANSFORM DATA --- #
df = read.csv( file = "data/PatchSizePilot_dataset.csv", header = TRUE )
df$id = as.character(df$id)

#Transform data from wide to long & change name of the columns
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
#Effects of time
df %>%
  ggplot(aes (x = exchange,
              y = evaporation)) + 
  geom_boxplot() + 
  geom_jitter()
#Effects of patch size
df$patch_size = as.character(df$patch_size)
df %>%
  ggplot(aes (x = patch_size,
              y = evaporation)) + 
  geom_boxplot() + 
  geom_jitter()
#Effects of disturbance
df$disturbance = as.character(df$disturbance)
df %>%
  ggplot(aes (x = disturbance,
              y = evaporation)) + 
  geom_boxplot() + 
  geom_jitter()
#Effects of tube id
df %>%
  ggplot(aes (x = reorder(id, sort(as.numeric(id))),
              y = evaporation)) +
  geom_boxplot() +
  theme(legend.position="none") +
  xlab("Tube ID") +
  ylab("Water evaporation (ml)") 

#----MIXED EFFECT MODEL (CHECK WHETHER TREATMENTS INFLUENCED WATER EVAPORATION, BY INCLUDING THE RANDOM EFFECTS OF TUBE ID)----#
mixed.model = lmer(evaporation  ~ patch_size + disturbance  + patch_size*disturbance + (1|id), data=df)
summary(mixed.model)

mixed.model = lmer(evaporation  ~ patch_size + disturbance  + patch_size*disturbance + (1|id), data=df, REML=FALSE)
mixed.model.null = lmer(evaporation  ~ disturbance  + patch_size*disturbance + (1|id), data=df, REML=FALSE)
anova(mixed.model.null, mixed.model)

mixed.model = lmer(evaporation  ~ patch_size + disturbance  + patch_size*disturbance + (1|id), data=df, REML=FALSE)
mixed.model.null = lmer(evaporation  ~ patch_size  + patch_size*disturbance + (1|id), data=df, REML=FALSE)
anova(mixed.model.null, mixed.model) #Why are df = 0 ????

mixed.model = lmer(evaporation  ~ patch_size + disturbance  + patch_size*disturbance + (1|id), data=df, REML=FALSE)
mixed.model.null = lmer(evaporation  ~ patch_size  + disturbance + (1|id), data=df, REML=FALSE)
anova(mixed.model.null, mixed.model) 