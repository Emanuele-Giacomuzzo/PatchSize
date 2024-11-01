## ----opts, include = FALSE----------------------------------------------------
## only run chunks if we have all required pkgs
knitr::opts_chunk$set(eval = all(sapply(c("purrr", "blme", "broom.mixed", "dplyr", "ggplot2"), require,
                                        character.only = TRUE)),
                      fig.width = 7, fig.height = 5)
## https://stackoverflow.com/questions/71683610/using-ggplot2-why-does-changing-the-color-palette-result-in-all-grey

## ----pkgs, message = FALSE----------------------------------------------------
library(glmmTMB)
library(lme4)
library(blme)
library(broom.mixed)
library(purrr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
OkIt <- unname(palette.colors(n = 8, palette = "Okabe-Ito"))[-1]

## ----culcita_dat--------------------------------------------------------------
cdat <- readRDS(system.file("vignette_data", "culcita.rds", package = "glmmTMB"))
cdatx <- cdat[-20,]

## ----culcita_mod1-------------------------------------------------------------
form <- predation~ttt + (1 | block)
cmod_glmer <- glmer(form, data = cdatx, family = binomial)
cmod_glmmTMB <- glmmTMB(form, data = cdatx, family = binomial)
cmod_bglmer <- bglmer(form,
  data = cdatx, family = binomial,
  fixef.prior = normal(cov = diag(9, 4))
)

## ----culcita_prior------------------------------------------------------------
cprior <- data.frame(prior = rep("normal(0,3)", 2),
                     class = rep("fixef", 2),
                     coef = c("(Intercept)", ""))
print(cprior)
cmod_glmmTMB_p <- update(cmod_glmmTMB, priors = cprior)

## ----culcita_check------------------------------------------------------------
stopifnot(all.equal(coef(summary(cmod_bglmer)),
          coef(summary(cmod_glmmTMB_p))$cond,
          tolerance = 5e-2))

## ----culcita_comp-------------------------------------------------------------
cmods <- ls(pattern = "cmod_[bg].*")
cmod_list <- mget(cmods) |> setNames(gsub("cmod_", "", cmods))
cres <- (purrr::map_dfr(cmod_list,
  ~ tidy(., conf.int = TRUE, effects = "fixed"),
  .id = "model"
)
|> select(model, term, estimate, lwr = conf.low, upr = conf.high)
  |> mutate(across(
    model,
    ~ factor(., levels = c(
      "glmer", "glmmTMB",
      "glmmTMB_p", "bglmer"
    ))
  ))
)
ggplot(cres, aes(x = estimate, y = term, colour = model)) +
  geom_pointrange(aes(xmin = lwr, xmax = upr),
    position = position_dodge(width = 0.5)
  ) +
  scale_colour_manual(values = OkIt)

## ----gophertortoise-----------------------------------------------------------
gdat <- readRDS(system.file("vignette_data", "gophertortoise.rds", package = "glmmTMB"))
form <- shells~prev + offset(log(Area)) + factor(year) + (1 | Site)
gmod_glmer <- glmer(form, family = poisson, data = gdat)
gmod_bglmer <- bglmer(form, family = poisson, data = gdat)
## cov.prior = gamma(shape = 2.5, rate = 0, common.scale = TRUE, posterior.scale = "sd"))
gmod_glmmTMB <- glmmTMB(form, family = poisson, data = gdat) ## 1e-5
## bglmer default corresponds to gamma(Inf, 2.5)
gprior <- data.frame(prior = "gamma(1e8, 2.5)",
                     class = "ranef",
                     coef = "")
gmod_glmmTMB_p <- update(gmod_glmmTMB, priors = gprior)
vc1 <- c(VarCorr(gmod_glmmTMB_p)$cond$Site)
vc2 <- c(VarCorr(gmod_bglmer)$Site)
stopifnot(all.equal(vc1, vc2, tolerance = 5e-4))

## ----pack_models--------------------------------------------------------------
gmods <- ls(pattern = "gmod_[bg].*")
gmod_list <- mget(gmods) |> setNames(gsub("gmod_", "", gmods))

## ----gopher_comp, echo = FALSE, warning = FALSE, results = "hide", cache = TRUE----
t1 <- tidy(gmod_bglmer, conf.int = TRUE, conf.method = "profile",
           effects = "ran_pars", devtol = Inf, quiet = TRUE)
t2 <- tidy(gmod_glmer, conf.int = TRUE, conf.method = "profile",
           effects = "ran_pars", quiet = TRUE)
## subscript out of bounds ... ??
## tidy(gmod_glmmTMB_p, conf.int = TRUE, conf.method = "profile", effects = "ran_pars")
## confint(gmod_glmmTMB_p, method = "profile", parm = "theta_",
##         include_nonest= TRUE)
## debug(expand_ci_with_mapped) 
## getParnames doesn't include RE parms ... ?? need full = TRUE?
t3A <- (exp(confint(profile(gmod_glmmTMB_p, stderr = 0.05, parm = "theta_")))
    |> as.data.frame()
    |> setNames(c("conf.low", "conf.high"))
    |> mutate(estimate = attr(VarCorr(gmod_glmmTMB_p)$cond$Site, "stddev"), .before = 1))
t3B <- tibble(estimate = attr(VarCorr(gmod_glmmTMB)$cond$Site, "stddev"),
              conf.low = NA,
              conf.high = NA)
gres <- (dplyr::bind_rows(list(bglmer = t1, glmer = t2, glmmTMB = t3B, glmmTMB_p = t3A),
                 .id = "model")
    |> select(model, estimate, lwr = conf.low, upr = conf.high)
)
ggplot(gres, aes(x = estimate, y = model)) + 
    geom_pointrange(aes(xmin = lwr, xmax = upr),
                    position = position_dodge(width = 0.5))

## ----deps, eval = FALSE-------------------------------------------------------
#  	
#  	rd <- \(x) tools::package_dependencies("brms", recursive = TRUE)[[x]]
#  ## rd <- \(x) packrat:::recursivePackageDependencies(x, ignores = "", lib.loc =    .libPaths()[1])
#  ## not sure why packrat and tools get different answers, but difference
#  ## doesn't matter much
#  brms_dep <- rd("brms")
#  glmmTMB_dep <- rd("glmmTMB")
#  length(setdiff(brms_dep, glmmTMB_dep))

## ----brms_priors, eval = FALSE------------------------------------------------
#  ## requires brms to evaluate, wanted to avoid putting it in Suggests: ...
#  bprior <- c(prior_string("normal(0,10)", class = "b"),
#              prior(normal(1,2), class = b, coef = treat),
#              prior_(~cauchy(0,2), class = ~sd,
#                     group = ~subject, coef = ~Intercept))

## ----fake_brms_priors, echo = FALSE-------------------------------------------
bprior <- structure(list(prior = c("normal(0,10)", "normal(1, 2)", "cauchy(0, 2)"
), class = c("b", "b", "sd"), coef = c("", "treat", "Intercept"
), group = c("", "", "subject"), resp = c("", "", ""), dpar = c("", 
"", ""), nlpar = c("", "", ""), lb = c(NA_character_, NA_character_, 
NA_character_), ub = c(NA_character_, NA_character_, NA_character_
), source = c("user", "user", "user")), row.names = c(NA, -3L
), class = c("brmsprior", "data.frame"))

## ----brms_prior_str-----------------------------------------------------------
str(bprior)

