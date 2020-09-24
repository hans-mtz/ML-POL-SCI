library(ggplot2)
library(tidyr)
library(dplyr)

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

# boot package

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
  preds <- sapply(mods, function(x)predict(x, newdata=assessment(split),
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


