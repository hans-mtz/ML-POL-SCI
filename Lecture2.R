## ----setup, include=FALSE----------------------------------------------------------------------------
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(message = FALSE, warning = FALSE, dev="png", tidy=TRUE,  tidy.opts = list(only.comment=TRUE,  width.cutoff=80))
knitr::opts_chunk$set(fig.retina=2)
knitr::opts_hooks$set(fig.callout = function(options) {
  if (options$fig.callout) {
    options$echo <- FALSE
    options$out.height <- "99%"
    # options$fig.width <- 16
    # options$fig.height <- 8
  }
  options
})
library(tibble)
library(ggplot2)
library(formatR)
library(knitr)
library(pander)
library(xtable)
library(dplyr)
library(xaringanthemer)
style_mono_accent(base_color = "#4F2683", code_font_size=".65rem", text_font_size = "1.25rem")
mytheme <- function(){
    theme_xaringan() + theme(axis.title=element_text(size=15), axis.text=element_text(size=12),
                           text=element_text(size=12), title=element_text(size=15))
}


## /* custom.css */

## .left-code {

##   color: #777;

##   width: 35%;

##   height: 92%;

##   float: left;

## }

## .right-plot {

##   width: 63%;

##   float: right;

##   padding-left: 1%;

## }

## .right-plot-shift {

##   width: 63%;

##   float: right;

##   padding-left: 1%;

##   position:relative;

##   top: -100px;

## }

## .right-plot-shift2 {

##   width: 63%;

##   float: right;

##   padding-left: 1%;

##   position:relative;

##   top: -50px;

## }

## 
## .plot-callout {

##   height: 225px;

##   width: 450px;

##   bottom: 5%;

##   right: 5%;

##   position: absolute;

##   padding: 0px;

##   z-index: 100;

## }

## .plot-callout img {

##   width: 100%;

##   border: 4px solid #23373B;

## }

## 
## .pull-right-shift {

##   float: right;

##   width: 47%;

##   position: relative;

##   top: -100px;

## }

## .pull-right-shift2 {

##   float: right;

##   width: 47%;

##   position: relative;

##   top: -50px;

## }

## 
## .pull-right ~ * {

##   clear: both;

## }

## .nobullet li {

##   list-style-type: none;

## }

## 
## .mycol {

##   float: left;

##   width: 30%;

##   padding: 5px;

## }

## 
## /* Clear floats after image containers */

## .myrow::after {

##   content: "";

##   clear: both;

##   display: table;

## }

## blockquote {

##     margin: 0;

## }

## 
## blockquote p {

##     padding: 15px;

##     background: #eee;

##     border-radius: 5px;

## }

## 
## blockquote p::before {

##     content: '\201C';

## }

## 
## blockquote p::after {

##     content: '\201D';

## }

## 
## 

## ----echo=FALSE--------------------------------------------------------------------------------------
htmltools::tags$script(
  src="https://kit.fontawesome.com/dc38d08ff3.js",
  crossorigin="anonymous")


## ---- out.height="50%", fig.align="center", echo=FALSE-----------------------------------------------
knitr::include_graphics("flex_interp.png")


## ---- echo=FALSE, cache=TRUE-------------------------------------------------------------------------
require(tibble)
out <- NULL
for(i in 1:250){
  x <- runif(250, -2,2)
  yhat <- 2 + 3*x
  y <- yhat + rnorm(250,0, runif(1,.25, 4))
  m <- lm(y ~ x)
  tmp <- tibble(
    mse = mean(m$residuals^2),
    rmse = sqrt(mse),
    deviance = sum(m$residuals^2),
    r2 = cor(y, m$fitted)^2
  )
  out <- rbind(out, tmp)
}


## ----echo=FALSE, fig.align="center", out.width="40%"-------------------------------------------------
library(GGally)
ggpairs(out) + mytheme()


## ---- echo=FALSE, fig.height=6, fig.width=6, out.width="100%", fig.align="center"--------------------
load("xy.rda")
library(ggplot2)
  ggplot(xy, aes(x=x, y=y)) + 
  geom_point(col="#4F2683", pch=1) + 
  geom_smooth(method="lm", se=2, col = "#4F2683", fill="#4F2683", alpha=.15) + 
  theme_bw() + 
  mytheme() + 
  labs(x="X", y="Y") + 
  ggtitle("Higher Bias, Lower Variance") + 
  coord_cartesian(ylim= c(0,12))


## ---- echo=FALSE, fig.height=6, fig.width=6, out.width="100%", fig.align="center"--------------------
ggplot(xy, aes(x=x, y=y)) + 
  geom_point(col="#4F2683", pch=1) + 
  geom_smooth(method="loess", col = "#4F2683", span=.1, fill="#4F2683", alpha=.15) + 
  theme_bw() + 
  mytheme() + 
  labs(x="X", y="Y") + 
  ggtitle("Higher Variance, Lower Bias")+ 
  coord_cartesian(ylim= c(0,12))


## ---- echo=FALSE, fig.height=6, fig.width=6, out.width="100%", fig.align="center"--------------------
ggplot(xy, aes(x=x, y=y)) + 
  geom_point(col="#4F2683", pch=1) + 
  geom_smooth(method="loess", col = "#4F2683", span=.35, fill="#4F2683", alpha=.15) + 
  theme_bw() + 
  mytheme() + 
  labs(x="X", y="Y") + 
  ggtitle("Compromise")+ 
  coord_cartesian(ylim= c(0,12))


## ----echo=FALSE, fig.align="center", out.width="65%"-------------------------------------------------
knitr::include_graphics("bvt.png")


## ----echo=FALSE, fig.align="center", out.width="100%"------------------------------------------------
knitr::include_graphics("confusion.png")


## ----echo=FALSE, fig.align="center", out.width="100%"------------------------------------------------
knitr::include_graphics("roc.png")


## ---- echo=FALSE-------------------------------------------------------------------------------------
library(tibble)
library(DT)
library(rio)
ces <- import("ces19.dta")
get_labs <- function(x){
  l <- attr(x, "label")
  ifelse(is.null(l), "", l)
}

x <- sapply(ces, get_labs)
x <- as_tibble(x, rownames="var")
x$value[1] <- "Year (factor)"
x$value[2] <- "Gender of respondent"
x$value[3] <- "Respondent age group"
x$value[4] <- "Region of residence"
x$value[5] <- "Highest level of education"
x$value[6] <- "Religious denomination of respondent"
x$value[7] <- "Respondent a union member"
x$value[8] <- "Contenintalism index"
x$value[9] <- "Market liberalism index"
x$value[10] <- "Morality index"
x$value[11] <- "Cynicism index"
x$value[12] <- "Retrospective personal economic evaluations"
x$value[13] <- "Retrospective national economic evaluations"
x$value[14] <- "Party ID"
x$value[15] <- "Defense spending preference"
x$value[16] <- "Environmental spending preference"
x$value[17] <- "Immigration (more/less) preference"
x$value[18] <- "Conservative party leader feeling thermometer"
x$value[19] <- "Liberal party leader feeling thermometer"
x$value[20] <- "NDP leader feeling thermometer"
x$value[21] <- "Bloc Quebecois party leader feeling thermometer"
x$value[22] <- "Preference on strengthening ties with USA"
x$value[23] <- "It shold be up to the private sector to create jobs"
x$value[24] <- "People who don't get ahead should blame themselves not 'the system'"
x$value[25] <- "How much should be done to reduce the gap between rich and poor?"
x$value[26] <- "Society would be better if fewer women worked outside the home"
x$value[27] <- "How much should be done for omen"
x$value[28] <- "Feeling thermometer: homosexuals"
x$value[29] <- "Survey weight"
x$value[30] <- "Voice choice in 2019 Federal election"
x$value[31] <- "Turnout in 2019 Federal election"
x$value[32] <- "Year"
x$value[33] <- "Is the incumbent a co-prtisan"
x$value[34] <- "Weight variable (again)"
x$value[35] <- "Phone sample type"
x$value[36] <- "Province of Residence"
datatable(x, options=list(pageLength=5, info=FALSE))


## ----------------------------------------------------------------------------------------------------
library(rio)
library(dplyr)
library(tidyr)
ces <- import("ces19.dta")
dat <- ces %>% 
  select(c(gender:retrocan, immig:leader_ndp, vote)) %>% 
  filter(province != "Quebec") %>% 
  factorize() %>% 
  mutate(vote_ndp = case_when(vote == "NDP" ~ 1, TRUE ~ 0), 
         vote_ndp = ifelse(is.na(vote), NA, vote_ndp)) %>% 
  select(-vote) %>% 
  na.omit()


## ----------------------------------------------------------------------------------------------------
library(rsample)
set.seed(13535)
s <- initial_split(dat, prop=.6)
train_dat <- training(s)
test_dat <- testing(s)


## ----------------------------------------------------------------------------------------------------
library(car)
mod <- glm(vote_ndp ~ ., 
           data=train_dat, 
           family=binomial)
Anova(mod)


## ---- fig.show='hide'--------------------------------------------------------------------------------
library(ROCR)

pred <- prediction(
  fitted(mod), 
  train_dat$vote_ndp)
perf <- performance(pred, 
                    "tpr", 
                    "fpr")
performance(pred, "auc")@y.values


## ----echo=FALSE, fig.align="center", out.width="85%", fig.height=6, fig.width=6----------------------
plot(perf)
abline(a=0, b=1, lty=2)


## ----------------------------------------------------------------------------------------------------
step_sel <- step(mod, direction="backward")


## ----------------------------------------------------------------------------------------------------
mods <- list()
mods[[1]] <- mod
mods[[2]] <- update(mod, . ~ .-relig)
mods[[3]] <- update(mod, . ~ .-relig-retrocan)
mods[[4]] <- update(mod, . ~ .-relig-regrocan-immig)
mods[[5]] <- update(mod, . ~ .-educ-relig-immig-retrocan)
mods[[6]] <- update(mod, . ~ .-educ-relig-immig-cynicism-retrocan)
mods[[7]] <- update(mod, . ~ .-educ-relig-immig-cynicism-moral-retrocan)
mods[[8]] <- update(mod, . ~ .-educ-relig-immig-cynicism-gender-retrocan-moral)
mods[[9]] <- update(mod, . ~ .-educ-relig-immig-cynicism-gender-retrocan-moral-union)
mods[[10]] <- update(mod, . ~ .-educ-relig-immig-cynicism-gender-retrocan-moral-union-retroper)
mods[[11]] <- update(mod, . ~ .-educ-relig-immig-cynicism-gender-retrocan-union-continent-moral-retroper)


## ----------------------------------------------------------------------------------------------------
test.mods <- lapply(mods, function(x)update(x, .~., data=test_dat))


## ----------------------------------------------------------------------------------------------------
pred_train <- lapply(mods, function(x)prediction(fitted(x), model.response(model.frame(x))))
pred_test <- lapply(test.mods, function(x)prediction(fitted(x), model.response(model.frame(x))))


## ----------------------------------------------------------------------------------------------------
auc_train <- sapply(pred_train, function(x)performance(x, "auc")@y.values[[1]])
auc_test <- sapply(pred_test, function(x)performance(x, "auc")@y.values[[1]])
auc_dat <- tibble(
  nvars =rep(0:10, 2), 
  auc = c(auc_train, auc_test), 
  type = factor(rep(1:2, each=11), labels=c("Training", "Testing"))
)


## ----auctt, eval=FALSE-------------------------------------------------------------------------------
## ggplot(auc_dat, aes(x=nvars, y=auc, colour=type)) +
##   geom_line() +
##   theme_bw() +
##   theme(aspect.ratio=1) +
##   labs(x="# Variables Removed", y="AUC", colour="Sample")


## ----ref.label="auctt", echo=FALSE, eval=TRUE, fig.align="center", out.width="65%"-------------------


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
  mutate(variable = factor(variable, labels=c("none", "relig", "retrocan", "immig", "educ", 
                                              "cynicism", "moral", "gender", "union", 
                                              "retroper", "continent")))


## ---- rocs, eval=FALSE-------------------------------------------------------------------------------
## ggplot(roc_data, aes(x=x, y=y, colour=as.factor(variable))) +
##   geom_line(size=.1) +
##   geom_abline(intercept=0, slope=1) +
##   facet_wrap(~data) +
##   theme_bw() +
##   theme(aspect.ratio=1) +
##   labs(x = "False Positive Rate",
##        y = "True Positive Rate",
##        colour = "Removed Variable")


## ----ref.label="rocs", echo=FALSE, eval=TRUE, fig.align="center", out.width="100%"-------------------


