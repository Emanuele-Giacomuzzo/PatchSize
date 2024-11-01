## ----setup, include=FALSE, message=FALSE--------------------------------------
library(knitr)
opts_chunk$set(eval = identical(Sys.getenv("NOT_CRAN"), "true"))

## ----libs,message=FALSE, eval=TRUE--------------------------------------------
library(glmmTMB)
set.seed(1)
nt <- min(parallel::detectCores(),5)

## ----simulate1----------------------------------------------------------------
#  N <- 3e5
#  xdata <- rnorm(N, 1, 2)
#  ydata <- 0.3 + 0.4*xdata + rnorm(N, 0, 0.25)

## ----fit1---------------------------------------------------------------------
#  system.time(
#    model1 <- glmmTMB(formula = ydata ~ 1 + xdata,
#                      control = glmmTMBControl(parallel = 1))
#    )

## ----fit2---------------------------------------------------------------------
#  system.time(
#    model2 <- glmmTMB(formula = ydata ~ 1 + xdata,
#                      control = glmmTMBControl(parallel = nt))
#    )

## ----simulate2----------------------------------------------------------------
#  xdata <- rnorm(N, 1, 2)
#  groups <- 200
#  data_use <- data.frame(obs = 1:N)
#  data_use <- within(data_use,
#  {
#  
#    group_var <- rep(seq(groups), times = nrow(data_use) / groups)
#    group_intercept <- rnorm(groups, 0, 0.1)[group_var]
#    xdata <- xdata
#    ydata <- 0.3 + group_intercept + 0.5*xdata + rnorm(N, 0, 0.25)
#  })

## ----fit3---------------------------------------------------------------------
#  (t_serial <- system.time(
#    model3 <- glmmTMB(formula = ydata ~ 1 + xdata + (1 | group_var), data = data_use, control = glmmTMBControl(parallel = 1))
#   )
#  )

## ----fit4---------------------------------------------------------------------
#  (t_parallel <- system.time(
#       update(model3,  control = glmmTMBControl(parallel = nt))
#   )
#  )

## ----SI-----------------------------------------------------------------------
#  print(sessionInfo(), locale=FALSE)

