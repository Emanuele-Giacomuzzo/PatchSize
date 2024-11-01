params <-
list(EVAL = FALSE)

## ----glmmTMB, load_lib,echo=FALSE---------------------------------------------
library(glmmTMB)
knitr::opts_chunk$set(eval = if (isTRUE(exists("params"))) params$EVAL else FALSE)

## ----eval = FALSE-------------------------------------------------------------
#  #' @rdname nbinom2
#  #' @export
#  zo_truncated_poisson <- function(link="log") {
#      r <- list(family="zo_truncated_poisson",
#                variance=function(lambda) {
#                    stop("haven't implemented variance function")
#                    ## should figure this out ...
#                    ## (lambda+lambda^2)/(1-exp(-lambda)) - lambda^2/((1-exp(-lambda))^2)
#                })
#      return(make_family(r,link))
#  }

## ----eval = FALSE-------------------------------------------------------------
#  .noDispersionFamilies <- c("binomial", "poisson", "truncated_poisson",
#                             "zo_truncated_poisson")

## ----testzo, eval = FALSE-----------------------------------------------------
#  library(glmmTMB)
#  set.seed(101)
#  dd <- data.frame(y = rpois(500, exp(1)))
#  table(dd$y)
#  ##  0   1   2   3   4   5   6   7   8   9
#  ## 34  91 117 116  68  45  17   7   3   2
#  dd <- dd[dd$y>1,,drop=FALSE]
#  table(dd$y)
#  ##   2   3   4   5   6   7   8   9
#  ## 117 116  68  45  17   7   3   2
#  glmmTMB(y ~ 1, family = "zo_truncated_poisson", data = dd)

