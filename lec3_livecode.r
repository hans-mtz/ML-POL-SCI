## ----setup, include=FALSE-------------------------------------------------------------------------
library(tibble)
library(ggplot2)
library(tidyr)
library(dplyr)
mytheme <- function(){
     theme(axis.title=element_text(size=15), axis.text=element_text(size=12),
                           text=element_text(size=12), title=element_text(size=15))
}

uwogray <- "#807F83"
purples <- c("#4F2683", "#8463AD", "#26064E", "#643F94", "#3B166A")
pal2 <- c("#4F2683","#807F83")
# pal3 <- c("#4F2683", "#811D7C", "302E87")
pal3 <- c(rgb(79,38,131, maxColorValue = 255), rgb(129,29,124, maxColorValue = 255), rgb(48,46,135, maxColorValue = 255))
# pal4 <- c("#4F2683", "#C1582C", "#1E845A", "#C1B52C")
pal4 <- c(rgb(79,38,131, maxColorValue = 255), rgb(193,88,44, maxColorValue = 255), rgb(30,132,90, maxColorValue = 255), rgb(193,181,44, maxColorValue = 255))


## ---- echo=FALSE, cache=TRUE, fig.height=6, fig.width=12, out.width="80%", fig.align="center"-----
set.seed(2493)
qtl <- c(.5, .75, .9, .95, .99)
samps <- list()
samps[[1]] <- matrix(rnorm(500*1500), ncol=1500)
samps[[2]] <- matrix(rnorm(1000*1500), ncol=1500)
samps[[3]] <- matrix(rnorm(1500*1500), ncol=1500)
samps[[4]] <- matrix(rnorm(2500*1500), ncol=1500)
samps[[5]] <- matrix(rnorm(5000*1500), ncol=1500)
  
out <- lapply(samps, function(x)t(apply(x, 2, quantile, probs=qtl)))
out <- do.call(rbind, out)  
out <- as_tibble(out)
names(out) <- paste0("p", c(50,75,90,95,99))
out <- out %>% mutate(n = factor(rep(1:5, each=1500), 
                                 labels=c("500", "1000", "1500", "2500", "5000")))
out <- out %>% pivot_longer(p50:p99, names_to="qtl", values_to="vals")
sds <- out %>% group_by(n, qtl) %>% summarise(sd = sd(vals))

sds %>% ggplot(aes(x=n, y=sd)) + 
  geom_linerange(aes(ymin=0, ymax=sd), col=uwogray) + 
  geom_point() + 
  facet_wrap(~qtl, nrow=2) + 
  theme_bw() + 
  mytheme() + 
  labs(x= "N", y="SD")


## ----withboot, echo=T-----------------------------------------------------------------------------
## Bootstrapping the median with the rsample package
set.seed(123)
## set number of observations
n <- 25
## draw x variable from a chi-squared distribution
library(tibble)
dat <- tibble(x = rchisq(n,1,1))
## set random number generator
set.seed(123)
## define function that we will bootstrap
# function must have variables term and estimate
med_fun <- function(split, ...){
  tibble(
    term="median",
    estimate = median(analysis(split)$x))
}

## do the bootstrapping
library(rsample)
library(purrr)
bsres <- bootstraps(dat, 1000, apparent=TRUE) %>% 
  mutate(median = map(splits, med_fun)) 

## calculate confidence intervals  
bind_rows(int_pctl(bsres, median),
int_bca(bsres, median, .fn=med_fun))



## ----bscor, echo=T, cache=TRUE--------------------------------------------------------------------
set.seed(123)
## load Mroz data
data(Mroz, package="carData")
## define function to be bootstrapped
cor.fun <- function(split, ...){
  r <- analysis(split) %>% 
    group_by(hc) %>%
    summarise(r = cor(age, lwg, use="complete")) %>% 
    select(r) %>% 
    pull  
  tibble(term=c("r_no", "r_yes", "diff"), 
         estimate = c(r, diff(r)))  
}
set.seed(45343)
bsres <- bootstraps(Mroz, 1000, strata = "hc", apparent=TRUE) %>% 
  mutate(diffcor = map(splits, cor.fun))


## ----printcor, echo=T, cache=TRUE-----------------------------------------------------------------
int_pctl(bsres, "diffcor")
int_bca(bsres, "diffcor", .fn=cor.fun)


## -------------------------------------------------------------------------------------------------
reg_fun <- function(split, ...){
  mod <- lm(leader_lib ~ educ + province + retroper, 
            data=analysis(split))
  tibble(
    term = names(coef(mod)), 
    estimate = coef(mod))
}
library(rio)
ces <- import("ces19.dta")
ces <- factorize(ces)
bsres <- bootstraps(ces, times=100, apparent=TRUE) %>% 
  mutate(coef = map(splits, reg_fun))



## -------------------------------------------------------------------------------------------------
int_pctl(bsres, "coef")
int_bca(bsres, "coef", .fn=reg_fun)


## -------------------------------------------------------------------------------------------------
data("Prestige", package="carData")
Prestige <- na.omit(Prestige)

ggplot(Prestige, aes(x=income, y=prestige)) + 
  geom_point() + 
  geom_smooth(method="loess")

mods <- list()
mods[[1]] <- glm(prestige ~ income + education + type, data=Prestige)
mods[[2]] <- glm(prestige ~ log(income) + education + type, data=Prestige)
mods[[3]] <- glm(prestige ~ poly(income, 2, raw=TRUE) + education + type, data=Prestige)
mods[[4]] <- glm(prestige ~ poly(income, 3, raw=TRUE) + education + type, data=Prestige)
mods[[5]] <- glm(prestige ~ poly(income, 4, raw=TRUE) + education + type, data=Prestige)
mods[[6]] <- glm(prestige ~ poly(income, 5, raw=TRUE) + education + type, data=Prestige)
mods[[7]] <- glm(prestige ~ poly(income, 6, raw=TRUE) + education + type, data=Prestige)


## -------------------------------------------------------------------------------------------------
set.seed(25343)
cv_fun <- function(split, ...){
  tmp <- lapply(mods, function(x)update(x, .~., data=analysis(split)))
  preds <- sapply(tmp, function(x)predict(x, newdata=assessment(split)))
  mse <- colMeans((assessment(split)$prestige-preds)^2)
  tibble(
    err = mse, 
    model = factor(c("linear", "log", paste0("poly-", 2:6))))
}

v <- vfold_cv(Prestige, v=10) %>%
      mutate(err = map(splits, cv_fun)) %>% 
      unnest(err) %>% 
      group_by(model) %>% 
      summarise(err= mean(err))
v


## ----cv0, eval=FALSE------------------------------------------------------------------------------
ggplot(v, aes(x=model, y=err)) +
  geom_point(aes(x=model, y=err)) +
  geom_line(aes(x=as.numeric(model), y=err)) +
  theme_bw() +
  labs(x="Model", y="Cross-Validation Error")


## ----cv1, eval=FALSE------------------------------------------------------------------------------
set.seed(25343)
v2 <- vfold_cv(Prestige, v=10, repeats=25) %>%
      mutate(err = map(splits, cv_fun)) %>%
      unnest(err) %>%
      group_by(model) %>%
      summarise(err = mean(err))
v2 <- v2 %>% filter(!(model %in% c("poly-6", "poly-5")))  
ggplot(v2, aes(x=model, y=err)) +
    geom_point(aes(x=model, y=err)) +
    geom_line(aes(x=as.numeric(model), y=err)) +
    theme_bw() +
    labs(x="Model", y="Cross-Validation Error")




## ----loo1, eval=FALSE-----------------------------------------------------------------------------
loo <- sapply(mods, function(x)
  mean(x$residuals^2/(1-hatvalues(x))))
plot.dat <- tibble(
  err = loo,
  model = factor(c("linear",
                   "log",
                   paste0("poly-", 2:6)))
)
ggplot(plot.dat, aes(x=model, y=err)) +
  geom_point(aes(x=model, y=err)) +
  geom_line(aes(x=as.numeric(model), y=err)) +
  theme_bw() +
  labs(x="Model", y="LOO Cross-Validation Error")



## -------------------------------------------------------------------------------------------------
library(rio)
bindat <- na.omit(import("bindat.dta"))
mods <- list()
for(i in 1:15){
  args <- list(formula = as.formula(paste0("repbin ~ poly(voice_mean, ", i, 
                                         ") + log(pop) + gdppc10k")), 
             data= bindat, 
             family=binomial)
  mods[[i]] <- do.call(glm, args)
}

## -------------------------------------------------------------------------------------------------
set.seed(65903)
cv_fun <- function(split, ...){
  tmp <- lapply(mods, function(x)update(x, .~., data=analysis(split)))
  preds <- sapply(tmp, function(x)predict(x, newdata=assessment(split), 
                                           type="response"))
  err <- colMeans(assessment(split)$repbin != round(preds))
  tibble(
    err = err, 
    order = 1:15)
}

cv.err <- vfold_cv(bindat, v=10) %>%
      mutate(err = map(splits, cv_fun)) %>% 
      unnest(err) %>% 
      group_by(order) %>% 
      summarise(err= mean(err))


## ----cvbin, eval=FALSE----------------------------------------------------------------------------
ggplot(cv.err, aes(x=order,
                     y=err)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x="Polynomial Order",
       y="Cross-validation Error")


## -------------------------------------------------------------------------------------------------
data("Prestige", package="carData")
Prestige <- na.omit(Prestige)
mods <- list()
mods[[1]] <- glm(prestige ~ income + education + type, data=Prestige)
mods[[2]] <- glm(prestige ~ log(income) + education + type, data=Prestige)
mods[[3]] <- glm(prestige ~ poly(income, 2, raw=TRUE) + education + type, data=Prestige)
mods[[4]] <- glm(prestige ~ poly(income, 3, raw=TRUE) + education + type, data=Prestige)
mods[[5]] <- glm(prestige ~ poly(income, 4, raw=TRUE) + education + type, data=Prestige)
mods[[6]] <- glm(prestige ~ poly(income, 5, raw=TRUE) + education + type, data=Prestige)
mods[[7]] <- glm(prestige ~ poly(income, 6, raw=TRUE) + education + type, data=Prestige)

Prestige$obs <- 1:nrow(Prestige)
bs_fun <- function(split, ...){
  tmp <- lapply(mods, function(x)update(x, .~., data=analysis(split)))
  preds <- sapply(tmp, function(x)predict(x, newdata=assessment(split)))
  mse <- (assessment(split)$prestige-preds)^2
  colnames(mse) <- factor(c("linear", "log", paste0("poly-", 2:6)))
  mse <- as_tibble(mse) %>%
    mutate(obs = assessment(split)$obs) %>% 
    pivot_longer(-obs, names_to="model", values_to="error")
  mse
}


## ----oobcv, eval=FALSE----------------------------------------------------------------------------
b <- bootstraps(Prestige, times=100) %>%
      mutate(err = map(splits, bs_fun)) %>%
      unnest(err) %>%
      group_by(obs, model) %>%
      summarise(error = mean(error)) %>%
      ungroup %>%
      group_by(model) %>%
      summarise(error = mean(error))
ggplot(b) +
  geom_point(aes(x=model, y=error)) +
  geom_line(aes(x=1:nrow(b), y=error)) +
  theme_bw() +
  labs(x="Model", y="Cross-Validation Error")



## ----eval=FALSE-----------------------------------------------------------------------------------
mystery <- read.csv("https://quantoid.net/files/9592/mystery.csv")

mystery <- read.csv("https://quantoid.net/files/9592/mystery.csv")
mods <- list()
for(i in 1:25){
  form <- paste0("y ~ poly(x, ", i, ")")
  args <- list(formula = form, data=mystery)
  mods[[i]] <- do.call(lm, args)
}

cv_fun <- function(split, ...){
  tmp <- lapply(mods, function(x)update(x, .~., data=analysis(split)))
  preds <- sapply(tmp, function(x)predict(x, newdata=assessment(split)))
  mse <- colMeans((assessment(split)$y-preds)^2)
  tibble(
    err = mse, 
    order = 1:25)
}

v <- vfold_cv(mystery, v=10, repeats=25) %>%
  mutate(err = map(splits, cv_fun)) %>% 
  unnest(err) %>% 
  group_by(order) %>% 
  summarise(err= mean(err))
v

ggplot(v, aes(x=order, y=err)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(x="Polynomial Order", y="CV Error") #+ 
#coord_cartesian(ylim=c(0.15, .2))

library(ggeffects)
effs <- lapply(1:length(mods), function(i){
  ggpredict(mods[[i]], terms="x [n=50]")%>% 
    mutate(order=i)
})

effs <- bind_rows(effs)
ggplot() + 
  geom_point(data=mystery, 
             aes(x=x, y=y), shape=1) + 
  geom_ribbon(data=effs, 
              aes(x=x, 
                  y=predicted, 
                  ymin=conf.low, 
                  ymax=conf.high), 
              fill="red", 
              col="transparent", 
              alpha=.5) + 
  geom_line(data=effs, 
            aes(x=x, 
                y=predicted), col="red") + 
  facet_wrap(~order) + 
  theme_bw() 

newx <- seq(min(mystery$x), max(mystery$x), length=50)
reg_fun <- function(split, ...){
  mod <- lm(y ~ poly(x,8), data=analysis(split))
  fit <- predict(mod, newdata=data.frame(x=newx))
  tibble(
    term = 1:50, 
    estimate = fit)
}
bsres <- bootstraps(mystery, times=1000, apparent=TRUE) %>% 
  mutate(coef = map(splits, reg_fun))

out <- bind_rows(
  int_pctl(bsres, coef), 
  int_bca(bsres, coef, .fn=reg_fun))
out <- out %>% 
  mutate(x = newx[term])

ggplot() + 
  geom_point(data=mystery, aes(x=x, y=y), size=.5, shape=1, col="gray50", alpha=.5) + 
  geom_ribbon(data=out, aes(x=x, y=.estimate, ymin=.lower, ymax=.upper, alpha=.method, fill=.method)) + 
  scale_fill_manual(values=c("red", "blue")) + 
  scale_alpha_manual(values=c(.3,.3)) + 
  geom_line(data=out, aes(x=x, y=.estimate)) + 
  theme_bw()

