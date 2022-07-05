rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("gridExtra")
library(lme4)

# --- IMPORT & TRANSFORM DATA --- #

df = read.csv( file = "/Users/ema/github/PatchSizePilot/data/PatchSizePilot_dataset.csv", header = TRUE )

############# --- TRANSFORM COLUMNS ---- ###########
df$disturbance.1 = NULL
df$id = as.character(df$id)

df$ex1 = df$water_add_before_t3/2; df$water_add_before_t3 = NULL
df$ex2 = df$ex1
df$ex3 = df$water_add_after_t3; df$water_add_after_t3 = NULL
df$ex4 = df$water_add_after_t4; df$water_add_after_t4 = NULL
df$ex5 = df$water_add_after_t5; df$water_add_after_t5 = NULL
df$ex6 = df$water_add_after_t6; df$water_add_after_t6 = NULL
df$disturbance = df$disturbance.1

############ --- DATA FROM WIDE TO LONG --- ###########
df = gather(df, exchange, evaporation, ex1:ex6)
df[df == "ex1"] = "1"
df[df == "ex2"] = "2"
df[df == "ex3"] = "3"
df[df == "ex4"] = "4"
df[df == "ex5"] = "5"
df[df == "ex6"] = "6"
df$exchange = as.numeric(df$exchange)

df %>%
  select(id, patch_size,)



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
df$patch_size = as.numeric(df$patch_size)

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
df$patch_size = as.numeric(df$patch_size)
df$disturbance = as.numeric(df$disturbance)


#----MIXED EFFECT MODEL (CHECK WHETHER TREATMENTS INFLUENCED WATER EVAPORATION, BY INCLUDING THE RANDOM EFFECTS OF TUBE ID)----#
mixed.model = lmer(evaporation  ~ exchange + patch_size + disturbance  + exchange*patch_size + exchange*disturbance + patch_size*disturbance + exchange*patch_size*disturbance + 
                     (1 + exchange|id) + (1 + patch_size|id) + (1 + disturbance|id) + (1 + exchange*patch_size|id) + (1 + exchange*disturbance|id) + (1 + patch_size*disturbance|id) + 
                     (1 + exchange*patch_size*disturbance|id), data=df)