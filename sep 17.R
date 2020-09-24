install.packages("rio")
library(rio)
library(tidyverse)
library(dplyr)
library(tidyr)
library(rsample)
library(car)
library(ROCR)
library(tibble)
library(ggplot2)
library(dplyr)

install.packages(c("rsample","car","ROCR"))

ces <- import("ces19.dta")
dat <- ces %>%
  select(c(gender:retrocan, immig:leader_ndp, vote)) %>%
  factorize() %>%
  filter(province != "Quebec") %>%
  mutate(vote_ndp = case_when(vote == "NDP" ~ 1, TRUE ~ 0),
         vote_ndp = ifelse(is.na(vote), NA, vote_ndp)) %>%
  select(-vote) %>%
  na.omit()

## ----------------------------------------------------------------------------------------------------
library(rsample)
set.seed(11111)
s <- initial_split(dat, prop = .6)
train_dat <- training(s)
test_dat <- testing(s)

#spliting observations into two groups

## ----------------------------------------------------------------------------------------------------
library(car)
mod <- glm(vote_ndp ~ .,
           data=train_dat,
           family=binomial)
Anova(mod)

## ---- fig.show='hide'--------------------------------------------------------------------------------
library(ROCR)

## names(x) in S3 -> slotNames(x) in S4
## x$element in S3 -> s@element in S4
pred <- prediction(
  # predicted values
  fitted(mod),
  # observed values
  train_dat$vote_ndp)
perf <- performance(pred,
                    "tpr",
                    "fpr")
#area under the curve stored in y.values
performance(pred, "auc")@y.values

## ----echo=FALSE, fig.align="center", out.width="85%", fig.height=6, fig.width=6----------------------
plot(perf)
abline(a=0, b=1, lty=2)

## ----------------------------------------------------------------------------------------------------
step_sel <- step(mod, direction="backward")
## look @ step_sel$Anova to see the order of removal
step_sel$anova

## ----------------------------------------------------------------------------------------------------
mods <- list()
mods[[1]] <- mod
mods[[2]] <- update(mod, . ~ . -retroper)
mods[[3]] <- update(mod, . ~ . -retroper - immig)
mods[[4]] <- update(mod, . ~ . -retroper -immig - moral)
mods[[5]] <- update(mod, . ~ . -retroper -immig - moral - cynicism)
mods[[6]] <- update(mod, . ~ . -retroper -immig - moral - cynicism - continent)
mods[[7]] <- update(mod, . ~ . -retroper -immig - moral - cynicism - continent - union)
mods[[8]] <- update(mod, . ~ . -retroper -immig - moral - cynicism - continent - union - educ)
mods[[9]] <- update(mod, . ~ . -retroper -immig - moral - cynicism - continent - union - educ - agegrp)


## ----------------------------------------------------------------------------------------------------
test.mods <- lapply(mods, function(x)update(x, .~., data=test_dat))

## ----------------------------------------------------------------------------------------------------
pred_train <- lapply(mods, function(x)prediction(fitted(x), model.response(model.frame(x))))
pred_test <- lapply(test.mods, function(x)prediction(fitted(x), model.response(model.frame(x))))

## ----------------------------------------------------------------------------------------------------
auc_train <- sapply(pred_train, function(x)performance(x, "auc")@y.values[[1]])
auc_test <- sapply(pred_test, function(x)performance(x, "auc")@y.values[[1]])
auc_dat <- tibble(
  nvars =rep(0:8, 2),
  auc = c(auc_train, auc_test),
  type = factor(rep(1:2, each=9), labels=c("Training", "Testing"))
)

## ----auctt, eval=FALSE-------------------------------------------------------------------------------

ggplot(auc_dat, aes(x=nvars, y=auc, colour=type)) +
  geom_line() +
  theme_bw() +
  theme(aspect.ratio=1) +
  labs(x="# Variables Removed", y="AUC", colour="Sample")

## ----------------------------------------------------------------------------------------------------
roc_train <- lapply(pred_train, function(x)performance(x, "tpr", "fpr"))
roc_test <- lapply(pred_test, function(x)performance(x, "tpr", "fpr"))


## ----------------------------------------------------------------------------------------------------
roc_train <- lapply(1:length(roc_train), function(i)tibble(x=c(roc_train[[i]]@x.values), y=c(roc_train[[i]]@y.values), variable = i) %>% unnest(everything()))
roc_test <- lapply(1:length(roc_test), function(i)tibble(x=c(roc_test[[i]]@x.values), y=c(roc_test[[i]]@y.values), variable = i) %>% unnest(everything()))


## ----------------------------------------------------------------------------------------------------
roc_train <- do.call(bind_rows, roc_train)
roc_test <- do.call(bind_rows, roc_test)
roc_train <- roc_train %>% mutate(data= factor(1, levels=1:2, labels=c("Training", "Testing")))
roc_test <- roc_test %>% mutate(data= factor(2, levels=1:2, labels=c("Training", "Testing")))
roc_data <- bind_rows(roc_train, roc_test)
roc_data <- roc_data %>%
  mutate(variable = factor(variable, labels=c("none", "retroper", "immig", "moral",
                                              "cynicism", "continent", "union",
                                              "educ", "agegrp")))

## ---- rocs, eval=FALSE-------------------------------------------------------------------------------
ggplot(roc_data, aes(x=x, y=y, colour=as.factor(variable))) +
  geom_line(size=.1) +
  geom_abline(intercept=0, slope=1) +
  facet_wrap(~data) +
  theme_bw() +
  theme(aspect.ratio=1) +
  labs(x = "False Positive Rate",
       y = "True Positive Rate",
       colour = "Removed Variable")
