library(dplyr)
library(tidyr)
library(tidyverse)
library(rio)
library(dplyr)
library(tidyr)
library(tidyverse)
library(rsample)
library(stargazer)
library(leaps)
library(car)
library(recipes)
library(caret)     # for resampling and model training
library(stringr)
library(tibble)
library(glmnet)
library(ModelMetrics)

set.seed(66636)

## Importing data----

#anes16<-import("./ANES/raw/anes_timeseries_2016_dta/anes_timeseries_2016.dta")

## funtion to remove no response codes in data----
# remove_noresponse<-function(data=data,values=c(-1,-8,-9)){
#   for ( i in 1:length(names(data))){
#     data[data[,i] %in% values,i]<-NA
#   }
#   data
# }
# ## function to reduce factor levels
# reduce_fct_levels<-function(data=data,n=3){
#   for ( i in 1:length(names(data))){
#     if (is.factor(data[,i])){
#       data[,i]<-fct_lump_n(data[,i], n = n)
#     }
#   }
#   data
# }
## Wrangling the data

d1 <- anes16 %>%
  select(V161007, V161267x,V161342,V161310x,V161270,V161010d,
         V161268, V161115, V161217:V161223, V161326:V161329,
         V161522, ends_with('x')) %>% 
  rename(internet= V161007,
         int_home = V161326,
         age     = V161267x,
         gender  = V161342,
         race    = V161310x,
         education = V161270,
         region = V161010d,
         income = V161361x,
         trust_peple = V161219,
         gov_waste = V161217,
         unemp = V161142x,
         marital = V161268,
         Pres_for_rel = V161084x,
         occup = V161276x) %>% 
  # remove_noresponse() %>%
  mutate( internet = case_when(
    internet == 2 ~ 0,
    internet == 1 ~ 1)
    ) %>% 
  filter(!is.na(int_home)) %>%
  # drop_na(internet) %>% #I could have also used
  factorize() #%>%
  # reduce_fct_levels()

# d1 %>% 
#   select(internet) %>% 
#   filter(!is.na(internet)) %>%
#   count(internet)
# 
# anes16 %>% 
#   select(V161007) %>% 
#   count(V161007)

#OTHER INTERNET VARS V161326:V161329

# d1 <- anes16 %>%
#   select(V161007, V161267x,V161342,V161310x,V161270,V161010d,
#          c(18:27, 46:53,57:63, 115:138, 151:159, 183:208, 216,
#            246, 253:260, 270, 281, 286:296, 302, 352, 353, 371:374,
#            380, 381, 428, 437, 454, 460:463, 479, 499)) %>% 
#   rename(internet= V161007,
#          age     = V161267x,
#          gender  = V161342,
#          race    = V161310x,
#          education = V161270,
#          region = V161010d) %>% 
#   remove_noresponse() %>% 
#   filter(internet != is.na(internet)) %>% 
#   mutate( internet = case_when(
#     internet == 2 ~ 0,
#     internet == 1 ~ 1
#   )) %>% 
#   factorize() %>% 
#   reduce_fct_levels()
#is.factor(d1$internet)
 d1 %>% glimpse()

##Splitting data----
library(rsample)
set.seed(333363)
s <- initial_split(d1, prop=.6, strata = 'internet')
R <- training(s)
S <- testing(s)

# stringr::str_remove_all(names(mods),"[.]")
# stringr::str_replace_all(names(mods),"[.]","_")

blueprint <- recipe(internet~.,data=R) %>% 
  step_nzv(all_predictors())  %>% 
  step_knnimpute(all_numeric(), neighbors = 6) %>%
  step_other(all_nominal(), threshold = 0.01,
           other = "other") %>%
  step_unknown(all_nominal()) %>%
  step_dummy(all_nominal(), one_hot = FALSE)

train_dat <- prep(blueprint, training = R) %>% 
  bake(., new_data=R)

names(train_dat)<-str_replace_all(names(train_dat),"[.]","_")

test_dat <- prep(blueprint, training = S) %>% 
  bake(., new_data=S)

names(test_dat)<-str_replace_all(names(test_dat),"[.]","_")

## Running theoretical model with training data ####
library(stringr)
#dat<-d1 %>% 
#reduce_fct_levels()

train_dat %>% 
  # select(starts_with(c('age','gender','race','education','region'))) %>% 
  glimpse()

teo_d <-train_dat %>%
    select(starts_with(c('internet','age','gender',
                         'race','education','region'))) %>% 
  rowwise() %>%
  # select(age_X06__Age_group_40_44:age_X10__Age_group_60_64) %>%
  mutate( age_g40_64 =
            sum(c_across(age_X06__Age_group_40_44:age_X10__Age_group_60_64)),
          age_g65 = sum(c_across(age_X11__Age_group_65_69:age_X13__Age_group_75_or_older)),
          south = sum(c_across(contains(c('Alabama', 'Arkansas', 'Delaware',
          'Washington', 'Florida', 'Georgia', 'Kentucky', 'Louisiana', 'Maryland',
          'Mississippi', 'North Carolina', 'Oklahoma', 'South_Carolina', 'Tennessee',
          'Texas', 'Virginia', 'West_Virginia')))))#%>%
  # select(age_g40_64,age_g65,south) %>%
  # glimpse() #%>%
  # summary()
glimpse(teo_d)

tmod <- glm(internet~ age_g40_64+ age_g65+gender_X1__Male +race_X2__Black__non_Hispanic 
            + race_X5__Hispanic+ race_X6__Other_non_Hispanic_incl_multiple_races__WEB__blank__Other__counted_as_a_race_
            + education_X9__High_school_graduate__high_school_diploma_or_equivalent__for_example__GED_
            + education_X10__Some_college_but_no_degree +south, 
            data=teo_d, family = binomial())
summary(tmod)


# teo_d <-train_dat %>% 
#   select(starts_with(c('internet','age','gender','race','education','region'))) %>% 
#   glimpse()
# 
# teo_d %>% glimpse()
# 
# tmod <- glm(internet~age+gender+race+education+region, 
#             data=teo_d)
# summary(tmod)
## FIND BEST MODEL ORIGINAL THEORY MODEL----
##Subsetting to find best model ####
library(leaps)
library(car)
library(recipes)
library(caret)     # for resampling and model training


X1 <- teo_d %>%
  # select(starts_with(c('age','gender','race','education','region'))) %>%
  # filter(internet!=is.na(internet)) %>% 
  select(-internet) %>%
  as.matrix()

y1 <- teo_d %>%
  # filter(internet!=is.na(internet)) %>% 
  select(internet) %>%
  pull

rmodst <- regsubsets(x=X1, y=y1, method="forward",
                     all.best=FALSE, nbest=3, nvmax=10, 
                     really.big = TRUE)


get_best<-function(regsubs=rmodst, depv="internet", data=teo_d){
    m <- as_tibble(summary(regsubs)$which, rownames = "size")
    m <- m %>% add_column(rss=summary(regsubs)$rss)
    m <- m %>% group_by(size) %>% filter(rss == max(rss))
    m <- m %>% select(-`(Intercept)`)
    print(m$rss)
    f<-list()
    for (i in 1:nrow(m)){
      v<-names(m[,as.vector(m[i,]==TRUE)])
      f[[i]]<-paste(depv," ~ ", paste(v, collapse="+"))
    }
    b_m <- lapply(f, function(x)glm(x, data=data, family = binomial()))
    b_m
}

best_t<-get_best(regsubs = rmodst, depv="internet", data=teo_d)
# lapply(best_t, summary)
# 
# summary(best_t[[11]])

stargazer(best_mods[[8]],best_mods[[9]],best_mods[[10]],best_mods[[11]], 
          title="Results", align=TRUE, type = "html",
          covariate.labels = c("Age:18-20","Age:60-64","Age:70-74","Age:75+",
                               "Race:Black","Ed:12th (ND)","Ed:HS","Ed:other",
                               "MS state", "NC state","TX state"))
# 
# 
# 
# length(summary(rmodst)$which)
# 
# mods <- as_tibble(summary(rmodst)$which, rownames = "size")
# mods <- mods %>% add_column(adjr2=summary(rmodst)$adjr2)
# mods <- mods %>% group_by(size) %>% filter(adjr2 == max(adjr2))
# mods <- mods %>% select(-`(Intercept)`)
# forms<-list()
# for (i in 1:11){
#   vars<-names(mods[,as.vector(mods[i,]==TRUE)])
#   forms[[i]]<-paste("internet ~ ", paste(vars, collapse="+"))
# }
# best_t <- lapply(forms, function(f)lm(f, data=train_dat))
# lapply(best_t[5:7], summary)
# 
# forms<-list()
# for (i in 1:12){
#   vars<-names(mods[,as.vector(mods[i,]==TRUE)])
#   forms[[i]]<-paste("internet ~ ", paste(vars, collapse="+"))
# }

# mods %>% 
#   select(adjr2)
#### FINDING THE BEST MODEL ADD VARS----
X <- train_dat %>%
  select(starts_with(c('age','gender',
                       'race','education','region',
                       'income',
                       'trust_peple',
                       'gov_waste',
                       'unemp',
                       'marital',
                       'Pres_for_re',
                       'occup')) ) %>%
  as.matrix()

# install.packages("Matrix")
# library(Matrix)
# rankMatrix(X)


y <- train_dat %>%
  select(internet) %>%
  pull

rmods <- regsubsets(x=X, y=y, method="forward",
                    all.best=FALSE, nbest=3, 
                    nvmax=10, really.big = TRUE)

best_m<-get_best(regsubs = rmods, depv="internet", data=train_dat)
lapply(best_m, summary)

summary(best_m[[11]])

# vis_miss(train_dat[150:200])

stargazer(best_m[[8]],best_m[[9]],best_m[[10]],best_m[[11]], 
          title="Results", align=TRUE, type = "html",
          covariate.labels = c("Age:70-74","Age:75+",
                               "Education: 12th (ND)",
                               "Education:other",
                               "MS state","NC state",
                               "Trust people: MoT","Widowed",
                               "Pres. for. rel.:DS ","Occupation:Retired",
                               "Occupation:Permanently disabled"))

## ploting the best models ----

subsets(rmods, statistic="adjr2", legend=F, abbrev = 7, max.size = 20)

## finding the best ones ----
library(purrr)
cv_fun <- function(split, best_mods=best_m,...){
  tmp <- lapply(best_mods, function(x)update(x, .~., data=analysis(split)))
  preds <- sapply(tmp, function(x)predict(x, newdata=assessment(split)))
  mse <- colMeans((c(assessment(split)$internet)-preds)^2)
  tibble(
    err = mse, 
    size=1:length(best_mods))
}
cv_fun <- function(split, ...){
  preds <- tibble(
    ridge = as.vector(predict(ridge, newx=assessment(split))), 
    lasso= as.vector(predict(lasso, newx=assessment(split))), 
    enet = as.vector(predict(e_net, newx=assessment(split))),
    alasso = as.vector(predict(a_lasso, newx=assessment(split))))
}

set.seed(226559)
v <- vfold_cv(train_dat, v=10, repeats = 10) %>%
  mutate(err = map(splits, cv_fun)) %>% 
  unnest(err) %>% 
  group_by(size) %>% 
  summarise(err= mean(err))
v
v2 <- vfold_cv(train_dat, v=10, repeats = 10) %>%
  mutate(err = map(splits, cv_fun)) %>% 
  unnest(err) %>% 
  group_by(size, id) %>% 
  summarise(err= mean(err)) %>% 
  ungroup() %>% 
  group_by(size) %>% 
  summarise(se=sd(err)/sqrt(n()),
            err= mean(err)) %>% 
  mutate(thresh = min(err) +
           se[which(err==min(err))]) %>% 
  filter(err < thresh) %>% 
  slice(1)
v2

summary(best_m[[1]])

# 
# library(tibble)
# library(dplyr)
# mods <- as_tibble(summary(rmods)$which, rownames = "size")
# mods <- mods %>% add_column(adjr2=summary(rmods)$adjr2)
# mods <- mods %>% group_by(size) %>% filter(adjr2 == max(adjr2))
# # n <- names(mods)[3:10]
# # forms <- apply(mods[,1:12], 1, function(x)
# #   paste("undercount ~ ", paste(n[which(x)], collapse="+")))
# # forms <- gsub("city_state", "city", forms)
# # best_mods <- lapply(forms, function(f)glm(f, data=S))
# 
# mods %>% 
#   select(adjr2)
# 
# # Best model
# #names(mods[,as.vector(mods[12,]==TRUE)])
#   
# # bmod <- glm(internet~age_X13__Age_group_75_or_older+V161084x_X4__Disapprove_strongly
# #            +V161086_X_88__FTF_ONLY__Don_t_know___don_t_know_where_to_rate__
# #            +V161115_Other+V161142_X3__Worse+V161217_X3__Don_t_waste_very_much
# #            +V161219_X3__About_half_the_time
# #            +V161268_X4__Divorced+V161268_Other
# #            +V161275x_X50__Retired__no_other_occupation
# #            +V161326_X2__No, 
# #            data=train_dat)
# # summary(bmod)
# 
# mods<-mods %>% 
#   select(-`(Intercept)`)
# 
# forms<-list()
# for (i in 1:12){
#   vars<-names(mods[,as.vector(mods[i,]==TRUE)])
#   forms[[i]]<-paste("internet ~ ", paste(vars, collapse="+"))
# }
# 
# best_mods <- lapply(forms, function(f)lm(f, data=train_dat))
# 
# lapply(best_mods, summary)
# 
# summary(best_mods[[12]])
# 
# library(stargazer)
# 
# stargazer(best_mods[[11]],best_mods[[12]], 
#           title="Results", align=TRUE, type = "text",
#           covariate.labels = c(""))
# 
# stargazer(best_mods[[9]],best_mods[[10]],best_mods[[11]],best_mods[[12]], 
#           title="Results", align=TRUE, type = "html",
#           covariate.labels = c("Age- 75 or older","Disapproves Pres (Int R.)",
#                                "Rating HC-doesn't know","Health (not VG/G)",
#                                "Unemp-worse","Gov dont waste taxes",
#                                "Trust people (1/2 time)","Divorced",
#                                "Not married/Divorced","Occupation",
#                                "Internet at home"))

# # Specify resampling plan
# cv <- trainControl(
#   method = "repeatedcv", 
#   number = 10, 
#   repeats = 5
# )
# 
# # Construct grid of hyperparameter values
# hyper_grid <- expand.grid(k = seq(2, 25, by = 1))
# 
# # Tune a knn model using grid search
# fit <- train(
#   blueprint, 
#   data = R, 
#   method = "knn", 
#   trControl = cv, 
#   tuneGrid = hyper_grid,
#   metric = "RMSE"
#   
# )

# d1 %>% 
#   count(V161326)
# 
# anes16 %>% 
#   count(V161326, V161007)
# 
# table()


#### REDUCING TO ONLY INTERESTING VARS FOR THIS EXERCISE----
anes16 %>% 
  select(V161007, V161267x,V161342,V161310x,V161270,V161010d,
         V161268, V161115, V161217:V161223, V161326:V161329,
         V161522, ends_with('x')) %>% 
  glimpse()

#### Regularized regressions----
## Ridge

library(glmnet)
# loglam <- seq(6.8, -5, length=100)
# X <- b99 %>% select(-gdppc_mp) %>% as.matrix()
# y <- b99 %>% select(gdppc_mp) %>% pull
# g1 <- glmnet(X, y, alpha=0)
# rcv <- cv.glmnet(X, y, alpha=0, lambda=exp(loglam))
# plot(rcv)

X2<- train_dat %>% 
    select(age_X12__Age_group_70_74     ,
    age_X13__Age_group_75_or_older      ,
    education_X8__12th_grade_no_diploma ,
    education_other                     ,
    region_X28__Mississippi             ,
    region_X37__North_Carolina          ,
    trust_peple_X2__Most_of_the_time    ,
    marital_X3__Widowed                 ,
    Pres_for_rel_X4__Disapprove_strongly,
    occup_X5__R_retired__if_also_working__working__20_hrs_wk_,
    occup_X6__R_permanently_disabled__if_also_working__working__20_hrs_wk_) %>% 
  as.matrix()

y2 <- train_dat %>% 
  select(internet) %>% 
  pull


rcv <- cv.glmnet(X2, y2, alpha=0, family = 'binomial')
ridge <- glmnet(X2, y2, alpha=0, family = 'binomial', lambda = rcv$lambda.min)
# plot(rcv, main = "Ridge penalty \n\n")

# ridge$lambda
# ridge1$lambda.1se
# 
# # Ridge model
# min(rcv$cvm)       # minimum MSE
# 
# rcv$lambda.min     # lambda for this min MSE
# 
# rcv$cvm[rcv$lambda == rcv$lambda.1se]  # 1-SE rule
# 
# rcv$lambda.1se  # lambda for this MSE
## LASSO

lcv <- cv.glmnet(X2, y2, alpha=1, family = 'binomial')
lasso <- glmnet(X2, y2, alpha=1, family = 'binomial',lambda = lcv$lambda.min)
# plot(lcv, main = "Lasso penalty \n\n")

## Elastic Net

# for reproducibility
# set.seed(321)

# grid search across 
e_netcv <- train(
  x = X2,
  y = y2,
  method = "glmnet",
  family = "binomial",
  nested = TRUE,
  # preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

e_netcv
# model with lowest RMSE
e_netcv$bestTune


# results for model with lowest RMSE
e_netcv$results %>%
  filter(alpha == e_netcv$bestTune$alpha, 
         lambda == e_netcv$bestTune$lambda)

ggplot(e_netcv)

e_net <- glmnet(X2, y2, family = 'binomial',
                alpha = e_netcv$bestTune$alpha, 
                lambda = e_netcv$bestTune$lambda)

summary(ridge$beta)

## Adaptive lasso
g<-1
b.e_net<-coef(e_net)
b.e_net <- ifelse(b.e_net == 0, .01, b.e_net)
b.ridge<-coef(ridge)
b.ridge <- ifelse(b.ridge == 0, .01, b.ridge)
b.lasso<-coef(lasso)
b.lasso <- ifelse(b.lasso == 0, .01, b.lasso)
w.enet<-1/(abs(b.e_net)^g)
w.ridge<-1/(abs(b.ridge)^g)
w.lasso<-1/(abs(b.lasso)^g)

a_lasso<-glmnet(X2, y2, alpha=1, family = 'binomial',
                lambda = lcv$lambda.min,
                penalty.factor = w.enet)

##Plotting coefficients of different methods

varnames <- c("Age:70-74","Age:75+",
             "Education: 12th (ND)",
             "Education:other",
             "MS state","NC state",
             "Trust people: MoT","Widowed",
             "Pres. for. rel.:DS ","Occupation:Retired",
             "Occupation:Permanently disabled")

coefs <- tibble(
  b = c(as.vector(coef(ridge)[-1]), 
        as.vector(coef(lasso)[-1]), 
        as.vector(coef(e_net)[-1]),
        as.vector(coef(a_lasso)[-1])), 
  model = factor(rep(1:4, each=length(coef(ridge)[-1])), 
                 labels=c("Ridge", "LASSO","ENET","A-LASSO")), 
  variable = rep(varnames, 4))

# coefs <- coefs %>% 
#   group_by(variable) %>% 
#   mutate(mean = mean(b)) %>% 
#   ungroup() %>% 
#   arrange(mean) %>% 
#   mutate(variable = factor(variable, levels = variable))
# 
# coefs

p1 <- ggplot(coefs, aes(x=b, y=variable, 
                        colour=model, shape=model)) + 
  geom_point() + 
  theme_bw() + 
  # scale_colour_manual(values=pal4) + 
  geom_vline(xintercept=0, lty=3)
  # mytheme()
  # coord_flip()
p1

### CV for the very best ----

glmnet_grid<-expand.grid(alpha = c(0, e_netcv$bestTune$alpha,1),
                         lambda = c(rcv$lambda.min, lcv$lambda.min,
                                    e_netcv$bestTune$lambda))

vbest <- train(
  x = X2,
  y = y2,
  method = "glmnet",
  family = "binomial",
  nested = TRUE,
  # preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10,
  tuneGrid = glmnet_grid,
  # penalty.factor = c(0,w.enet,w.ridge,w.lasso)
)
vbest

# vbest1 <- train(
#   x = X2,
#   y = y2,
#   method = "glmnet",
#   family = "binomial",
#   nested = TRUE,
#   # preProc = c("zv", "center", "scale"),
#   trControl = trainControl(method = "cv", number = 10),
#   tuneLength = 10,
#   tuneGrid = glmnet_grid #,
#   # penalty.factor = c(0,w.enet,w.ridge,w.lasso)
# )
# vbest1

#### Fitting on testing data


### what?----

install.packages('visdat')
library(visdat)
vis_miss(anes16, cluster=TRUE)

anes16 %>% 
  select(V161007, V161267x,V161342,V161310x,V161270,V161010d,
         V161268, V161115, V161217:V161223, V161326:V161329,
         V161522, ends_with('x')) %>% 
  # remove_noresponse() %>% 
  # vis_miss(cluster=TRUE) %>%
  rename(internet= V161007,
         age     = V161267x,
         gender  = V161342,
         race    = V161310x,
         education = V161270,
         region = V161010d) %>% 
  mutate( internet = case_when(
    internet == 2 ~ 0,
    internet == 1 ~ 1)) %>%
   count(internet)# %>% 
  # select(internet) %>% 
  # glimpse()

vis_miss(teo_d, )
length(teo_d, )

anes16 %>% 
  select(V161361x) %>% 
  count(V161361x)

row.names(coef(ridge))[-1]
row.names(coef(lasso))[-1]
row.names(coef(e_net))[-1]
row.names(coef(a_lasso))[-1]
rep(varnames, 4)
