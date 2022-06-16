rm( list = ls(  ) )
cat( "\014" )
library("tidyverse")
library("grid")
library("gridExtra") 

info = read.csv("/Users/ema/github/PatchSizePilot/data/PatchSizePilot_dataset.csv", header = TRUE)
info$culture_ID = info$id
info$id = NULL

