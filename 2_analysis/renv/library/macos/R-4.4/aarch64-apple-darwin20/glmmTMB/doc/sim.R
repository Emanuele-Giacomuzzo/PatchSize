## ----setup, include=FALSE, message=FALSE--------------------------------------
library(knitr)
knitr::opts_chunk$set(echo = TRUE)

## ----libs,message=FALSE-------------------------------------------------------
library(glmmTMB)
library(ggplot2); theme_set(theme_bw())

## ----fit1---------------------------------------------------------------------
data(Owls)
owls_nb1 <- glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent +
                             (1|Nest)+offset(log(BroodSize)),
                          family = nbinom1,
                          ziformula = ~1, data=Owls)

## ----sim----------------------------------------------------------------------
simo=simulate(owls_nb1, seed=1)
Simdat=Owls
Simdat$SiblingNegotiation=simo[[1]]
Simdat=transform(Simdat,  
			NegPerChick = SiblingNegotiation/BroodSize, 
			type="simulated")
Owls$type = "observed"	
Dat=rbind(Owls, Simdat)	

## ----plots,fig.width=7--------------------------------------------------------

ggplot(Dat,  aes(NegPerChick, colour=type))+geom_density()+facet_grid(FoodTreatment~SexParent)

## ----sleepstudy---------------------------------------------------------------
data("sleepstudy", package = "lme4")
set.seed(101)
ss_sim <- transform(sleepstudy,
                    Reaction = simulate_new(~ Days,
                                            newdata = sleepstudy,
                                            family = gaussian,
                                            newparams = list(beta = c(250, 10),
                                                        betadisp = 2*log(50)))[[1]])

## ----simlm--------------------------------------------------------------------
ss_fit <- glmmTMB(Reaction ~ Days, sleepstudy)
ss_simlm <- transform(sleepstudy,
                      Reaction = simulate(ss_fit)[[1]])

## ----ss_plot, fig.width = 10--------------------------------------------------
library(ggplot2); theme_set(theme_bw())
ss_comb <- rbind(data.frame(sleepstudy, sample = "real"),
                 data.frame(ss_sim, sample = "simulated"),
                 data.frame(ss_simlm, sample = "simulated (from fit)")
                 )
ggplot(ss_comb, aes(x = Days, y = Reaction, colour = Subject)) +
    geom_line() +
    facet_wrap(~sample)

## ----rho-to-theta-------------------------------------------------------------
rho_to_theta <- function(rho) rho/sqrt(1-rho^2)
## tests
stopifnot(all.equal(get_cor(rho_to_theta(-0.2)), -0.2))
## equivalent to general function
stopifnot(all.equal(rho_to_theta(-0.2), put_cor(-0.2, input_val = "vec")))

## ----sim1---------------------------------------------------------------------
n_id <- 10
dd <- expand.grid(trt = factor(c("A", "B")),
                  id = factor(1:n_id),
                  time = 1:6)

## ----form---------------------------------------------------------------------
form1 <- ~trt*time+(1+time|id)
colnames(model.matrix(lme4::nobars(form1), data = dd))

## ----params2------------------------------------------------------------------
## intercept, trtB effect, slope, trt x slope interaction
beta_vec <- c(1, 2, 0.1, 0.2)

## ----params3------------------------------------------------------------------
sdvec <-  c(1.5,0.15)
corval <- -0.2
thetavec <- c(log(sdvec), rho_to_theta(corval))
par1 <- list(beta = beta_vec,
             betadisp = log(1),  ## log(theta)
             theta = thetavec)

## ----sim3---------------------------------------------------------------------
dd$y <- simulate_new(form1,
                     newdata = dd,
                     seed = 101,
                     family = nbinom2,
                     newparams = par1)[[1]]
range(dd$y)

## ----sim-by-hand--------------------------------------------------------------
library(lme4)
set.seed(101)
X <- model.matrix(nobars(form1), data  = dd)
## generate random effects values
rt <- mkReTrms(findbars(form1),
               model.frame(subbars(form1), data = dd))
Z <- t(rt$Zt)
## construct covariance matrix
Sigma <- diag(sdvec) %*% matrix(c(1, corval, corval, 1), 2) %*% diag(sdvec)
lmer_thetavec <- t(chol(Sigma))[c(1,2,4)]
## plug values into Cholesky factor of random effects covariance matrix
rt$Lambdat@x <- lmer_thetavec[rt$Lind]
u <- rnorm(nrow(rt$Lambdat))
b <- t(rt$Lambdat) %*% u
eta <- drop(X %*% par1$beta + t(rt$Zt) %*% b)
mu <- exp(eta)
y <- rnbinom(nrow(dd), size = 1, mu = mu)
range(y) ## range varies a lot

## ----mvrnorm------------------------------------------------------------------
b <- MASS::mvrnorm(1, mu = rep(0,2*n_id),
                   Sigma = Matrix::.bdiag(replicate(n_id,
                                                    Sigma,
                                                    simplify = FALSE)))

## ----acf1---------------------------------------------------------------------
set.seed(101)
n <- 1000                                                 ## Number of time points
x <- MASS::mvrnorm(mu = rep(0,n),
                   Sigma = .7 ^ as.matrix(dist(1:n)) )    ## Simulate the process using the MASS package
## as.matrix(dist(1:n)) constructs a banded matrix with m_{ij} = abs(i-j)
y <- x + rnorm(n)                                         ## Add measurement noise/nugget
dat0 <- data.frame(y, 
                 times = factor(1:n, levels=1:n),
                 group = factor(rep(1, n)))

## ----sim_new_ar1--------------------------------------------------------------
phi_to_AR1 <- function(phi) phi/sqrt(1-phi^2)
s2 <- simulate_new(~ ar1(times + 0 | group), 
                   newdata = dat0,
                   seed = 101,
                   newparams = list(
                       beta = 0,   
                       betadisp = 0,
                       theta = c(0, phi_to_AR1(0.7)))
                   )[[1]]

## ----plot_acf-----------------------------------------------------------------
a1 <- drop(acf(dat0$y, plot = FALSE)$acf)
a2 <- drop(acf(s2, plot = FALSE)$acf)
par(las = 1, bty = "l")
matplot(0:(length(a1)-1), cbind(a1, a2), pch = 1,
        ylab = "autocorrelation", xlab = "lag")
curve(0.7^x/2, add = TRUE, col = 4, lwd = 2)

