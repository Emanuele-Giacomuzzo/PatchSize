params <-
list(EVAL = FALSE)

## ----setup, include=FALSE, message=FALSE--------------------------------------
library(knitr)
library(glmmTMB)
library(MASS)    ## for mvrnorm()
library(TMB)     ## for tmbprofile()
## devtools::install_github("kaskr/adcomp/TMB")  ## get development version
knitr::opts_chunk$set(echo = TRUE, eval=if (exists("params")) params$EVAL else FALSE)
do_image <- exists("params") && params$EVAL
## want to *store* images within package
save_vig_dir <- file.path("inst","vignette_data")
pkg_dir <- "glmmTMB"
## guess where we are ...
if (grepl("/vignettes$",getwd())) {  ## in vignettes dir
  save_vig_dir <- file.path("../",save_vig_dir)
} else if (grepl(paste0("/",pkg_dir,"$"),getwd())) { ## in repo head
  save_vig_dir <- file.path(pkg_dir,save_vig_dir)
}
## want to *retrieve* images from system files
use_vig_dir <- system.file("vignette_data",package="glmmTMB")
mkfig <- function(expr,fn) {
  png(normalizePath(file.path(save_vig_dir,fn)))
  eval(substitute(expr))
  invisible(dev.off())
}
usefig <- function(fn) {
  knitr::include_graphics(file.path(use_vig_dir,fn))
}
## turned off caching for now: got error in chunk 'fit.us.2'
## Error in retape() : 
##   Error when reading the variable: 'thetaf'. Please check data and parameters.
## In addition: Warning message:
## In retape() : Expected object. Got NULL.
set.seed(1)
## run this in interactive session if you actually want to evaluate chunks ...
## Sys.setenv(NOT_CRAN="true")

## ----covstruct-table, echo  = FALSE, eval = TRUE------------------------------
ctab <- read.delim(sep = "#", comment = "",
                   header = TRUE,
                   check.names = FALSE,
           text = "
 Covariance                       # Notation      # no. parameters # Requirement  # Parameters
 Unstructured (general positive definite)      # `us`          #  $n(n+1)/2$     # # See [Mappings]
 Heterogeneous Toeplitz           # `toep`        #  $2n-1$         #     # log-SDs ($\\theta_1-\\theta_n$); correlations $\\rho_k = \\theta_{n+k}/\\sqrt{1+\\theta_{n+k}^2}$, $k = \\textrm{abs}(i-j+1)$
 Het. compound symmetry  # `cs`          #  $n+1$    #      # log-SDs ($\\theta_1-\\theta_n$); correlation $\\rho = \\theta_{n+1}/\\sqrt{1+\\theta_{n+1}^2}$
 Homogenous diagonal     # `homdiag`     #  $1$      #  # log-SD
 Het. diagonal           # `diag`        #  $n$            #  # log-SDs
 AR(1)                            # `ar1`         #  $2$            # Unit spaced levels # log-SD; $\\rho = \\left(\\theta_2/\\sqrt{1+\\theta_2^2}\\right)^{d_{ij}}$
 Ornstein-Uhlenbeck               # `ou`          #  $2$            # Coordinates  # log-SD; log-OU rate ($\\rho = \\exp(-\\exp(\\theta_2) d_{ij})$)
 Spatial exponential              # `exp`         #  $2$            # Coordinates # log-SD; log-scale ($\\rho = \\exp(-\\exp(-\\theta_2) d_{ij})$)
 Spatial Gaussian                 # `gau`         #  $2$            # Coordinates # log-SD; log-scale ($\\rho = \\exp(-\\exp(-2\\theta_2) d_{ij}^2$)
 Spatial Matèrn                   # `mat`         #  $3$            # Coordinates # log-SD, log-range, log-shape (power)
 Reduced-rank                     # `rr`          #  $nd-d(d-1)/2$  # rank (d)    
 Proptional                       # `propto`      #  $1$            # Variance-covariance matrix
"
)
knitr::kable(ctab)

## ----sim1, eval=TRUE----------------------------------------------------------
n <- 25                                              ## Number of time points
x <- MASS::mvrnorm(mu = rep(0,n),
             Sigma = .7 ^ as.matrix(dist(1:n)) )    ## Simulate the process using the MASS package
y <- x + rnorm(n)                                   ## Add measurement noise

## ----simtimes-----------------------------------------------------------------
#  times <- factor(1:n, levels=1:n)
#  head(levels(times))

## ----simgroup-----------------------------------------------------------------
#  group <- factor(rep(1,n))

## ----simcomb------------------------------------------------------------------
#  dat0 <- data.frame(y, times, group)

## ----fitar1, eval=FALSE-------------------------------------------------------
#  glmmTMB(y ~ ar1(times + 0 | group), data=dat0)

## ----ar0fit,echo=FALSE--------------------------------------------------------
#  glmmTMB(y ~ ar1(times + 0 | group), data=dat0)

## ----simGroup-----------------------------------------------------------------
#  simGroup <- function(g, n=6, phi=0.7) {
#      x <- MASS::mvrnorm(mu = rep(0,n),
#               Sigma = phi ^ as.matrix(dist(1:n)) )   ## Simulate the process
#      y <- x + rnorm(n)                               ## Add measurement noise
#      times <- factor(1:n)
#      group <- factor(rep(g,n))
#      data.frame(y, times, group)
#  }
#  simGroup(1)

## ----simGroup2----------------------------------------------------------------
#  dat1 <- do.call("rbind", lapply(1:1000, simGroup) )

## ----fit.ar1------------------------------------------------------------------
#  (fit.ar1 <- glmmTMB(y ~ ar1(times + 0 | group), data=dat1))

## ----fit.us-------------------------------------------------------------------
#  fit.us <- glmmTMB(y ~ us(times + 0 | group), data=dat1, dispformula=~0)
#  fit.us$sdr$pdHess ## Converged ?

## ----fit.us.vc----------------------------------------------------------------
#  VarCorr(fit.us)

## ----fit.toep-----------------------------------------------------------------
#  fit.toep <- glmmTMB(y ~ toep(times + 0 | group), data=dat1,
#                      dispformula=~0)
#  fit.toep$sdr$pdHess ## Converged ?

## ----fit.toep.vc--------------------------------------------------------------
#  (vc.toep <- VarCorr(fit.toep))

## ----fit.toep.vc.diag---------------------------------------------------------
#  vc1 <- vc.toep$cond[[1]] ## first term of var-cov for RE of conditional model
#  summary(diag(vc1))
#  summary(vc1[row(vc1)!=col(vc1)])

## ----fit.toep.reml------------------------------------------------------------
#  fit.toep.reml <- update(fit.toep, REML=TRUE)
#  vc1R <- VarCorr(fit.toep.reml)$cond[[1]]
#  summary(diag(vc1R))
#  summary(vc1R[row(vc1R)!=col(vc1R)])

## ----fit.cs-------------------------------------------------------------------
#  fit.cs <- glmmTMB(y ~ cs(times + 0 | group), data=dat1, dispformula=~0)
#  fit.cs$sdr$pdHess ## Converged ?

## ----fit.cs.vc----------------------------------------------------------------
#  VarCorr(fit.cs)

## ----anova1-------------------------------------------------------------------
#  anova(fit.ar1, fit.toep, fit.us)

## ----anova2-------------------------------------------------------------------
#  anova(fit.cs, fit.toep)

## ----sample2------------------------------------------------------------------
#  x <- sample(1:2, 10, replace=TRUE)
#  y <- sample(1:2, 10, replace=TRUE)

## ----numFactor----------------------------------------------------------------
#  (pos <- numFactor(x,y))

## ----parseNumLevels-----------------------------------------------------------
#  parseNumLevels(levels(pos))

## ----numFactor2---------------------------------------------------------------
#  dat1$times <- numFactor(dat1$times)
#  levels(dat1$times)

## ----fit.ou-------------------------------------------------------------------
#  fit.ou <- glmmTMB(y ~ ou(times + 0 | group), data=dat1)
#  fit.ou$sdr$pdHess ## Converged ?

## ----fit.ou.vc----------------------------------------------------------------
#  VarCorr(fit.ou)

## ----fit.mat------------------------------------------------------------------
#  fit.mat <- glmmTMB(y ~ mat(times + 0 | group), data=dat1, dispformula=~0)
#  fit.mat$sdr$pdHess ## Converged ?

## ----fit.mat.vc---------------------------------------------------------------
#  VarCorr(fit.mat)

## ----fit.gau------------------------------------------------------------------
#  fit.gau <- glmmTMB(y ~ gau(times + 0 | group), data=dat1, dispformula=~0)
#  fit.gau$sdr$pdHess ## Converged ?

## ----fit.gau.vc---------------------------------------------------------------
#  VarCorr(fit.gau)

## ----fit.exp------------------------------------------------------------------
#  fit.exp <- glmmTMB(y ~ exp(times + 0 | group), data=dat1)
#  fit.exp$sdr$pdHess ## Converged ?

## ----fit.exp.vc---------------------------------------------------------------
#  VarCorr(fit.exp)

## ----spatial_data-------------------------------------------------------------
#  d <- data.frame(z = as.vector(volcano),
#                  x = as.vector(row(volcano)),
#                  y = as.vector(col(volcano)))

## ----spatial_sub_sample-------------------------------------------------------
#  set.seed(1)
#  d$z <- d$z + rnorm(length(volcano), sd=15)
#  d <- d[sample(nrow(d), 100), ]

## ----volcano_data_image_fake,eval=FALSE---------------------------------------
#  volcano.data <- array(NA, dim(volcano))
#  volcano.data[cbind(d$x, d$y)] <- d$z
#  image(volcano.data, main="Spatial data", useRaster=TRUE)

## ----volcano_data_image_real,echo=FALSE---------------------------------------
#  if (do_image) {
#    volcano.data <- array(NA, dim(volcano))
#    volcano.data[cbind(d$x, d$y)] <- d$z
#    mkfig(image(volcano.data, main="Spatial data"),"volcano_data.png")
#  }

## ----volcano_image,eval=TRUE,echo=FALSE---------------------------------------
usefig("volcano_data.png")

## ----spatial_add_pos_and_group------------------------------------------------
#  d$pos <- numFactor(d$x, d$y)
#  d$group <- factor(rep(1, nrow(d)))

## ----fit_spatial_model, cache=TRUE--------------------------------------------
#  f <- glmmTMB(z ~ 1 + exp(pos + 0 | group), data=d)

## ----confint_sigma------------------------------------------------------------
#  confint(f, "sigma")

## ----newdata_corner-----------------------------------------------------------
#  newdata <- data.frame( pos=numFactor(expand.grid(x=1:3,y=1:3)) )
#  newdata$group <- factor(rep(1, nrow(newdata)))
#  newdata

## ----predict_corner-----------------------------------------------------------
#  predict(f, newdata, type="response", allow.new.levels=TRUE)

## ----predict_column-----------------------------------------------------------
#  predict_col <- function(i) {
#      newdata <- data.frame( pos = numFactor(expand.grid(1:87,i)))
#      newdata$group <- factor(rep(1,nrow(newdata)))
#      predict(f, newdata=newdata, type="response", allow.new.levels=TRUE)
#  }

## ----predict_all--------------------------------------------------------------
#  pred <- sapply(1:61, predict_col)

## ----image_results_fake,eval=FALSE--------------------------------------------
#  image(pred, main="Reconstruction")

## ----image_results_real,echo=FALSE--------------------------------------------
#  if (do_image) {
#    mkfig(image(pred, main="Reconstruction", useRaster=TRUE),
#          "volcano_results.png")
#  }

## ----results_image,eval=TRUE,echo=FALSE---------------------------------------
usefig("volcano_results.png")

## ----fit.us.2-----------------------------------------------------------------
#  vv0 <- VarCorr(fit.us)
#  vv1 <- vv0$cond$group          ## extract 'naked' V-C matrix
#  n <- nrow(vv1)
#  rpars <- getME(fit.us,"theta") ## extract V-C parameters
#  ## first n parameters are log-std devs:
#  all.equal(unname(diag(vv1)),exp(rpars[1:n])^2)
#  ## now try correlation parameters:
#  cpars <- rpars[-(1:n)]
#  length(cpars)==n*(n-1)/2      ## the expected number
#  cc <- diag(n)
#  cc[upper.tri(cc)] <- cpars
#  L <- crossprod(cc)
#  D <- diag(1/sqrt(diag(L)))
#  round(D %*% L %*% D,3)
#  round(unname(attr(vv1,"correlation")),3)

## ----other_check--------------------------------------------------------------
#  all.equal(c(cov2cor(vv1)),c(fit.us$obj$env$report(fit.us$fit$parfull)$corr[[1]]))

## ----fit.us.profile,cache=TRUE------------------------------------------------
#  ## want $par, not $parfull: do NOT include conditional modes/'b' parameters
#  ppar <- fit.us$fit$par
#  length(ppar)
#  range(which(names(ppar)=="theta")) ## the last n*(n+1)/2 parameters
#  ## only 1 fixed effect parameter
#  tt <- tmbprofile(fit.us$obj,2,trace=FALSE)

## ----fit.us.profile.plot_fake,eval=FALSE--------------------------------------
#  confint(tt)
#  plot(tt)

## ----fit.us.profile.plot_real,echo=FALSE--------------------------------------
#  mkfig(plot(tt),"us_profile_plot.png")

## ----us_profile_image,eval=TRUE,echo=FALSE------------------------------------
usefig("us_profile_plot.png")

## ----fit.cs.profile,cache=TRUE------------------------------------------------
#  ppar <- fit.cs$fit$par
#  length(ppar)
#  range(which(names(ppar)=="theta")) ## the last n*(n+1)/2 parameters
#  ## only 1 fixed effect parameter, 1 dispersion parameter
#  tt2 <- tmbprofile(fit.cs$obj,3,trace=FALSE)

## ----fit.cs.profile.plot_fake,eval=FALSE--------------------------------------
#  plot(tt2)

## ----fit.cs.profile.plot_real,echo=FALSE--------------------------------------
#  mkfig(plot(tt2),"cs_profile_plot.png")

## ----fit.cs.profile_image,echo=FALSE,eval=TRUE--------------------------------
usefig("cs_profile_plot.png")

## ----rr_ex, eval = FALSE------------------------------------------------------
#  ## fit rank-reduced models with varying dimension
#  dvec <- 2:10
#  fit_list <- lapply(dvec,
#                     function(d) {
#                         glmmTMB(abund ~ Species + rr(Species + 0|id, d = d),
#                                 data = spider_long)
#                     })
#  names(fit_list) <- dvec
#  ## compare fits via AIC
#  aic_vec <- sapply(fit_list, AIC)
#  delta_aic  <- aic_vec - min(aic_vec, na.rm = TRUE)

## ----spider-re-plot, message=FALSE, fig.width = 10, fig.height=7--------------
#  spider_rr <- glmmTMB(abund ~ Species + rr(Species + 0|id, d = 3),
#                       data = spider_long)
#  re <- as.data.frame(ranef(spider_rr))
#  re <- within(re, {
#      ## sites in numeric order
#      grp <- factor(grp, levels = unique(grp))
#      ## species in site-1-predicted-abundance order
#      term <- reorder(term, condval, function(x) x[1])
#      lwr <- condval - 2*condsd
#      upr <- condval + 2*condsd
#  })
#  if (require("ggplot2")) {
#      ggplot(re, aes(grp, condval)) +
#          geom_pointrange(aes(ymin=lwr, ymax = upr)) +
#          facet_wrap(~term, scale = "free")
#  }

## ----get-fl-------------------------------------------------------------------
#  source(system.file("misc", "extract_rr.R", package = "glmmTMB"))
#  rr_info <- extract_rr(spider_rr)
#  lapply(rr_info, dim)

## ----spider-biplot, fig.width = 8, fig.height=8-------------------------------
#  par(las = 1)
#  afac <- 4
#  sp_names <- abbreviate(gsub("Species", "", rownames(rr_info$fl)))
#  plot(rr_info$fl[,1], rr_info$fl[,2], xlab = "factor 1", ylab = "factor 2", pch = 16, cex = 2)
#  text(rr_info$b[,1]*afac*1.05, rr_info$b[,2]*afac*1.05, rownames(rr_info$b))
#  arrows(0, 0, rr_info$b[,1]*afac, rr_info$b[,2]*afac)
#  text(rr_info$fl[,1], rr_info$fl[,2], sp_names, pos = 3, col = 2)

## ----propto_ex, eval = FALSE--------------------------------------------------
#  require(ade4)
#  require(ape)
#  data(carni70)
#  carnidat <- data.frame(species = rownames(carni70$tab), carni70$tab)
#  tree <- read.tree(text=carni70$tre)
#  phylo_varcov <- vcv(tree)# phylogenetic variance-covariance matrix
#  # row/column names of phylo_varcov must match factor levels in data
#  rownames(phylo_varcov) <- colnames(rownames(phylo_varcov)) <- gsub(".", "_", rownames(phylo_varcov))
#  carnidat$dummy <- factor(1) # a dummy grouping variable must be added to the dataset
#  
#  fit_phylo <- glmmTMB(log(range) ~ log(size) + propto(0 + species | dummy, phylo_varcov),
#                       data = carnidat)
#  

## ----mm_int, eval = TRUE------------------------------------------------------
model.matrix(~f, data.frame(f=factor(c("c", "s", "v"))))

## ----mm_noint, eval = TRUE----------------------------------------------------
model.matrix(~0+f, data.frame(f=factor(c("c", "s", "v"))))

