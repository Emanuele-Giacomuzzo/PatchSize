## ----low-disturbance-plots, eval = TRUE, echo = FALSE, message = FALSE-------------------------------------

biomass_plots("low")
p[[1]]
p[[2]]
p[[3]]
p[[4]]
p[[5]]
p[[6]]


## ----low-disturbance-grid, eval = FALSE--------------------------------------------------------------------
## 
## grid = grid.arrange(p[[1]],p[[3]],p[[5]],p[[2]],p[[4]],p[[6]],
##                     ncol=3, nrow=2,
##                     top = textGrob("Low disturbance",gp=gpar(fontsize=20,font=3)))
## ggsave(here("results", "biomass", "Clean_biomass_low.jpg"), grid, width = 22, height = 13)


## ----high-disturbance-plots, eval = TRUE, echo = FALSE, message = FALSE------------------------------------

biomass_plots("high")
p[[1]]
p[[2]]
p[[3]]
p[[4]]
p[[5]]
p[[6]]


## ----high-disturbance-grid, eval = FALSE-------------------------------------------------------------------
## 
## biomass_plots("high")
## 
## grid = grid.arrange(p[[1]],p[[3]],p[[5]],p[[2]],p[[4]],p[[6]],
##              ncol=3,
##              nrow=2,
##              top = textGrob("High disturbance",
##                             gp = gpar(fontsize = 20,
##                                       font = 3)))
## 
## ggsave(here("results", "biomass", "Clean_biomass_high.jpg"),
##        grid,
##        width = 22,
##        height = 13)

