## ----experimental-cultures-------------------------------------------------------------------------------------------------------------------------------------
culture_info = read.csv(here("data", "PatchSizePilot_culture_info.csv"), header = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------
datatable(culture_info[,1:10],
          rownames = FALSE,
          options = list(scrollX = TRUE),
          filter = list(position = 'top', 
                        clear = FALSE))

