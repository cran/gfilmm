## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig.width = 5, 
  fig.height = 4.5
)

## ----simulations_linreg-------------------------------------------------------
set.seed(666L)
n <- 30L
x <- 1L:n
y <- rnorm(n, mean = x, sd = 2)
y_rounded <- round(y, digits = 1L)
dat <- data.frame(
  ylwr = y_rounded - 0.05,
  yupr = y_rounded + 0.05,
  x = x
)

## ----gfi_linreg---------------------------------------------------------------
library(gfilmm)
fidSims <- gfilmm(
  y = ~ cbind(ylwr, yupr), # interval data
  fixed = ~ x,             # fixed effects
  random = NULL,           # random effects
  data = dat,              # data
  N = 10000L               # number of simulations
)

## ----gfiSummary_linreg--------------------------------------------------------
gfiSummary(fidSims)

## ----lm_linreg----------------------------------------------------------------
lmfit <- lm(y ~ x)
confint(lmfit)

## ----gfiCDF_linreg------------------------------------------------------------
Fslope <- gfiCDF(~ x, fidSims)
plot(Fslope, main = "Slope", ylab = expression("Pr("<="x)"))

## ----gfiDensity_linreg--------------------------------------------------------
library(kde1d)
kfit <- kde1d(fidSims$VERTEX[["x"]], weights = fidSims$WEIGHT, mult = 4)
curve(dkde1d(x, kfit), from = 0.7, to = 1.1)

## -----------------------------------------------------------------------------
fpd <- gfilmmPredictive(fidSims, newdata = data.frame(x = c(1, 30)))
gfiSummary(fpd)

## -----------------------------------------------------------------------------
predict(lmfit, newdata = data.frame(x = c(1, 30)), interval = "prediction")

## ----simulations_AOV1R--------------------------------------------------------
mu           <- 10000 # grand mean
sigmaBetween <- 2
sigmaWithin  <- 3
n            <- 8L # sample size per group

set.seed(666L)
groupmeans <- rnorm(2L, mu, sigmaBetween)
y1         <- rnorm(n, groupmeans[1L], sigmaWithin) 
y2         <- rnorm(n, groupmeans[2L], sigmaWithin) 
y          <- c(y1, y2)
y_rounded  <- round(c(y1, y2), digits = 1L)
dat        <- data.frame(
                ylwr = y_rounded - 0.05,
                yupr = y_rounded + 0.05,
                group = gl(2L, n)
              )

## ----gfilmm_AOV1R-------------------------------------------------------------
fidSims <- gfilmm(~ cbind(ylwr, yupr), ~ 1, ~ group, data = dat, N = 10000L)

## ----gfiSummary_AOV1R---------------------------------------------------------
gfiSummary(fidSims)

## ----KenwardRoger, message=FALSE----------------------------------------------
library(lmerTest)
library(emmeans)
fit <- lmer(y ~ (1|group), data = dat)
emmeans(fit, ~ 1)

## ----gfiConfInt_CV_AOV1R------------------------------------------------------
gfiConfInt(~ sqrt(sigma_group^2 + sigma_error^2)/`(Intercept)`, fidSims)

