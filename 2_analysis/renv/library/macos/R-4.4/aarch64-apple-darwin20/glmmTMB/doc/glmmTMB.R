## ----setopts,echo=FALSE,message=FALSE, eval = TRUE----------------------------
library("knitr")
opts_chunk$set(fig.width=5,fig.height=5,
               out.width="0.8\\textwidth",
               echo = TRUE, error = FALSE,
               eval = identical(Sys.getenv("NOT_CRAN"), "true"))
Rver <- paste(R.version$major,R.version$minor,sep=".")
used.pkgs <- c("glmmTMB","bbmle")  ## packages to report below

## ----pkgversions, echo=FALSE, eval=TRUE---------------------------------------
pkgver <- vapply(sort(used.pkgs),function(x) as.character(packageVersion(x)),"")
print(pkgver,quote=FALSE)

## ----citation,eval=FALSE,echo=FALSE-------------------------------------------
#  print(citation("glmmTMB"),style="latex")

## ----pkgs,message=FALSE, eval=TRUE--------------------------------------------
library("glmmTMB")
library("bbmle")    ## for AICtab
library("ggplot2")
## cosmetic
theme_set(theme_bw()+
  theme(panel.spacing=grid::unit(0,"lines")))

## ----owltransform,warning=FALSE-----------------------------------------------
#  Owls <- transform(Owls,
#                    Nest=reorder(Nest,NegPerChick),
#                    NCalls=SiblingNegotiation,
#                    FT=FoodTreatment)

## ----owlplot1,echo=FALSE,message=FALSE,warning=FALSE,eval=FALSE---------------
#  G0 <- ggplot(Owls,aes(x=reorder(Nest,NegPerChick),
#                        y=NegPerChick))+
#    labs(x="Nest",y="Negotiations per chick")+coord_flip()+
#    facet_grid(FoodTreatment~SexParent)
#  G0+stat_sum(aes(size=..n..),alpha=0.5)+
#        scale_size_continuous(name="# obs",
#                              breaks=seq(1,9,by=2))+
#      theme(axis.title.x=element_text(hjust=0.5,size=12),
#           axis.text.y=element_text(size=7))

## ----glmmTMBfit---------------------------------------------------------------
#  fit_zipoisson <- glmmTMB(NCalls~(FT+ArrivalTime)*SexParent+
#                                       offset(log(BroodSize))+(1|Nest),
#                                       data=Owls,
#                                       ziformula=~1,
#                                       family=poisson)

## ----zipoisssum---------------------------------------------------------------
#  summary(fit_zipoisson)

## ----glmmTMBnbinomfit---------------------------------------------------------
#  fit_zinbinom <- update(fit_zipoisson,family=nbinom2)

## ----glmmTMBnbinom1fit--------------------------------------------------------
#  fit_zinbinom1 <- update(fit_zipoisson,family=nbinom1)

## ----glmmTMBnbinom1vfit-------------------------------------------------------
#  fit_zinbinom1_bs <- update(fit_zinbinom1,
#                             . ~ (FT+ArrivalTime)*SexParent+
#                                 BroodSize+(1|Nest))

## ----aictab-------------------------------------------------------------------
#  AICtab(fit_zipoisson,fit_zinbinom,fit_zinbinom1,fit_zinbinom1_bs)

## ----glmmTMBnbinomhfit--------------------------------------------------------
#  fit_hnbinom1 <-  update(fit_zinbinom1_bs,
#                          ziformula=~.,
#                          data=Owls,
#                          family=truncated_nbinom1)

## ----hurdle_AIC---------------------------------------------------------------
#  AICtab(fit_zipoisson,fit_zinbinom,
#         fit_zinbinom1,fit_zinbinom1_bs,
#         fit_hnbinom1)

## ----contraception_sum,echo=FALSE, eval=TRUE----------------------------------
data("Contraception",package="mlmRev")
nc <- nrow(Contraception)
nl <- length(levels(Contraception$district))
load("contraceptionTimings.rda")
meandiff <- mean(with(tmatContraception,
                      time[pkg=="glmer"]/time[pkg=="glmmTMB"]))

## ----contraception,echo=FALSE,warning=FALSE,fig.cap="Timing for fitting the replicated Contraception data set."----
#  ## NaN from geom_smooth because glmmADMB only has two points/
#  ## can't compute confidence intervals
#  ## suppressWarnings() doesn't actually work within ggplot ...
#  ## instead set/reset options("warn")
#  op <- options(warn = -1)
#  ggplot(tmatContraception,
#         aes(n, time, colour=pkg)) + geom_point() +
#      scale_y_log10(breaks=c(1,2,5,10,20,50,100)) +
#      scale_x_log10(breaks=c(1,2,4,10,20,40)) +
#      labs(x="Replication (x 1934 obs.)",y="Elapsed time (s)") +
#      geom_smooth(method="lm", formula = y ~ x) +
#      scale_colour_brewer(palette="Set1")
#  options(op)

## ----insteval,echo=FALSE,warning=FALSE,fig.cap="Timing for fitting subsets of the InstEval data set.", eval = TRUE----
load("InstEvalTimings.rda")
n_InstEval <- 73421L  ## seems silly to require lme4 just to get this number
meandiff_inst2 <- with(tmatInstEval,
     time[pkg=="lmer"]/time[pkg=="glmmTMB"])
ggplot(tmatInstEval,aes(n,time,colour=pkg))+geom_point()+
  scale_y_log10(breaks=c(1,2,5,10,20,50,100,200))+
      scale_x_log10(breaks=c(0.1,0.2,0.5,1.0))+
  labs(x=sprintf("Replication (x %d obs.)",n_InstEval),
       y="Elapsed time (s)")+
  geom_smooth(method="lm", formula = y ~ x)+
  scale_colour_brewer(palette="Set1")

