## ----setopts,echo=FALSE,message=FALSE-----------------------------------------
library("knitr")
opts_chunk$set(fig.width=5,fig.height=5,
               error=FALSE,
               out.width="0.8\\textwidth",echo=TRUE)
## https://tex.stackexchange.com/questions/148188/knitr-xcolor-incompatible-color-definition/254482
knit_hooks$set(document = function(x) {sub('\\usepackage[]{color}', '\\usepackage{xcolor}', x, fixed = TRUE)})
Rver <- paste(R.version$major,R.version$minor,sep=".")
used.pkgs <- c("glmmTMB","bbmle")  ## packages to report below

## ----solaris_check, echo=FALSE------------------------------------------------
## https://stackoverflow.com/questions/23840523/check-if-os-is-solaris
is.solaris <- function() {
  grepl('SunOS', Sys.info()['sysname'])
}
is.windows <- function() {
  .Platform$OS.type == "windows"
}
is.cran <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}
huxtable_OK <- (!is.solaris()) && !(is.windows() && is.cran())

## ----packages,message=FALSE---------------------------------------------------
library(glmmTMB)
library(car)
library(emmeans)
library(effects)
library(multcomp)
library(MuMIn)
require(DHARMa, quietly = TRUE) ## may be missing ...
library(broom)
library(broom.mixed)
require(dotwhisker, quietly = TRUE)
library(ggplot2); theme_set(theme_bw())
library(texreg)
library(xtable)
if (huxtable_OK) library(huxtable)
## retrieve slow stuff
L <- gt_load("vignette_data/model_evaluation.rda")

## ----examples,eval=TRUE-------------------------------------------------------
owls_nb1 <- glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent +
                        (1|Nest)+offset(log(BroodSize)),
                    contrasts=list(FoodTreatment="contr.sum",
                                   SexParent="contr.sum"),
                    family = nbinom1,
                    zi = ~1, data=Owls)

## ----fit_model3,cache=TRUE----------------------------------------------------
data("cbpp",package="lme4")
cbpp_b1 <- glmmTMB(incidence/size~period+(1|herd),
                   weights=size,family=binomial,
                   data=cbpp)
## simulated three-term Beta example
set.seed(1001)
dd <- data.frame(z=rbeta(1000,shape1=2,shape2=3),
                 a=rnorm(1000),b=rnorm(1000),c=rnorm(1000))
simex_b1 <- glmmTMB(z~a*b*c,family=beta_family,data=dd)

## ----dharma_sim,eval=FALSE,message=FALSE--------------------------------------
#  owls_nb1_simres <- simulateResiduals(owls_nb1)

## ----fake_dharma_plotfig, eval=FALSE------------------------------------------
#  plot(owls_nb1_simres)

## ----dharma_plotfig,fig.width=8,fig.height=4, echo=FALSE----------------------
if (require(DHARMa, quietly = TRUE)) plot(owls_nb1_simres)

## ----caranova1----------------------------------------------------------------
if (requireNamespace("car") && getRversion() >= "3.6.0") {
    Anova(owls_nb1)  ## default type II
    Anova(owls_nb1,type="III")
}

## ----effects1,fig.width=8,fig.height=4----------------------------------------
effects_ok <- (requireNamespace("effects") && getRversion() >= "3.6.0")
if (effects_ok) {
    (ae <- allEffects(owls_nb1))
    plot(ae)
}

## ----effects2, fig.width=12,fig.height=12-------------------------------------
if (effects_ok) {
  plot(allEffects(simex_b1))
}

## ----emmeans1-----------------------------------------------------------------
emmeans(owls_nb1, poly ~ FoodTreatment | SexParent)

## ----hurdle-------------------------------------------------------------------
owls_hnb1 <- update(owls_nb1, family = truncated_nbinom1, ziformula = ~.)

## ----emmeans2-----------------------------------------------------------------
emmeans(owls_hnb1, ~ FoodTreatment * SexParent, component = "cond", type = "response")
# --- or ---
emmeans(owls_hnb1, ~ FoodTreatment * SexParent, component = "cmean")

## ----emmeans3-----------------------------------------------------------------
emmeans(owls_hnb1, ~ FoodTreatment * SexParent, component = "response")

## ----drop1_eval,cache=TRUE----------------------------------------------------
system.time(owls_nb1_d1 <- drop1(owls_nb1,test="Chisq"))

## ----print_drop1--------------------------------------------------------------
print(owls_nb1_d1)

## ----dredge1------------------------------------------------------------------
print(owls_nb1_dredge)

## ----plot_dredge1,fig.width=8,fig.height=8------------------------------------
op <- par(mar=c(2,5,14,3))
plot(owls_nb1_dredge)
par(op) ## restore graphics parameters

## ----mumin_MA-----------------------------------------------------------------
model.avg(owls_nb1_dredge)

## ----glht_use-----------------------------------------------------------------
g1 <- glht(cbpp_b1, linfct = mcp(period = "Tukey"))
summary(g1)

## ----broom_mixed,fig.height=3,fig.width=5-------------------------------------
if (requireNamespace("broom.mixed") && requireNamespace("dotwhisker")) {
  t1 <- broom.mixed::tidy(owls_nb1, conf.int = TRUE)
  t1 <- transform(t1,
                  term=sprintf("%s.%s", component, term))

  if (packageVersion("dotwhisker")>"0.4.1") {
    dw <- dwplot(t1)
  } else {
    owls_nb1$coefficients <- TRUE  ## hack!
    dw <- dwplot(owls_nb1,by_2sd=FALSE)
  }
  print(dw+geom_vline(xintercept=0,lty=2))
}

## ----xtable_prep--------------------------------------------------------------
ss <- summary(owls_nb1)
## print table; add space, 
pxt <- function(x,title) {
  cat(sprintf("{\n\n\\textbf{%s}\n\\ \\\\\\vspace{2pt}\\ \\\\\n",title))
  print(xtable(x), floating=FALSE); cat("\n\n")
  cat("\\ \\\\\\vspace{5pt}\\ \\\\\n")
}


## ----xtable_sum,eval=FALSE----------------------------------------------------
#  pxt(lme4::formatVC(ss$varcor$cond),"random effects variances")
#  pxt(coef(ss)$cond,"conditional fixed effects")
#  pxt(coef(ss)$zi,"conditional zero-inflation effects")

## ----xtable_sum_real,results="asis",echo=FALSE--------------------------------
if (requireNamespace("xtable")) {
  pxt(lme4::formatVC(ss$varcor$cond),"random effects variances")
  pxt(coef(ss)$cond,"conditional fixed effects")
  pxt(coef(ss)$zi,"conditional zero-inflation effects")
}

## ----texreg1,results="asis"---------------------------------------------------
source(system.file("other_methods","extract.R",package="glmmTMB"))
texreg(owls_nb1,caption="Owls model", label="tab:owls")

## ----huxtable,results="asis"--------------------------------------------------
if (!huxtable_OK) {
  cat("Sorry, huxtable+LaTeX is unreliable on this platform; skipping\n")
} else {
  cc <- c("intercept (mean)"="(Intercept)",
          "food treatment (starvation)"="FoodTreatment1",
          "parental sex (M)"="SexParent1",
          "food $\\times$ sex"="FoodTreatment1:SexParent1")
  h0 <- huxreg(" " = owls_nb1, # give model blank name so we don't get '(1)'
               tidy_args = list(effects="fixed"),
               coefs = cc,
               error_pos = "right",
               statistics = "nobs" # don't include logLik and AIC
               )
  names(h0)[2:3] <- c("estimate", "std. err.")
  ## allow use of math notation in name
  h1 <- set_cell_properties(h0,row=5,col=1,escape_contents=FALSE)
  cat(to_latex(h1,tabular_only=TRUE))
}

## ----load_infl----------------------------------------------------------------
source(system.file("other_methods","influence_mixed.R", package="glmmTMB"))

## ----infl, eval=FALSE---------------------------------------------------------
#  owls_nb1_influence_time <- system.time(
#    owls_nb1_influence <- influence_mixed(owls_nb1, groups="Nest")
#  )

## ----plot_infl----------------------------------------------------------------
car::infIndexPlot(owls_nb1_influence)

## ----plot_infl2,fig.width=10,fig.height=6,out.width="\\textwidth"-------------
inf <- as.data.frame(owls_nb1_influence[["fixed.effects[-Nest]"]])
inf <- transform(inf,
                 nest=rownames(inf),
                 cooks=cooks.distance(owls_nb1_influence))
inf$ord <- rank(inf$cooks)
if (require(reshape2)) {
  inf_long <- melt(inf, id.vars=c("ord","nest"))
  gg_infl <- (ggplot(inf_long,aes(ord,value))
    + geom_point()
    + facet_wrap(~variable, scale="free_y")
    ## n.b. may need expand_scale() in older ggplot versions ?
    + scale_x_reverse(expand=expansion(mult=0.15))
    + scale_y_continuous(expand=expansion(mult=0.15))
    + geom_text(data=subset(inf_long,ord>24),
                aes(label=nest),vjust=-1.05)
  )
  print(gg_infl)
}

## ----save_out,echo=FALSE------------------------------------------------------
## store time-consuming stuff
save("owls_nb1",
     "owls_nb1_simres",
     "owls_nb1_dredge",
     "owls_nb1_influence",
     "owls_nb1_influence_time",
     file="../inst/vignette_data/model_evaluation.rda",
     version=2 ## for compatibility with R < 3.6.0
     )

