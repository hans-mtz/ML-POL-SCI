# install.packages('earth')
library(earth)
library(caret)
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
    internet == 1 ~ 1),
    int_home = case_when(
      int_home == 2 ~ 0,
      int_home == 1 ~ 1)
  ) %>% 
  # filter(!is.na(int_home)) %>%
  filter(!is.na(internet)) %>%
  # drop_na(internet) %>% #I could have also used
  factorize() #%>%
# reduce_fct_levels()

##Splitting data----
library(rsample)
set.seed(333363)
s <- initial_split(d1, prop=.6, strata = "internet")
R <- training(s)
S <- testing(s)

blueprint <- #recipe(int_home~.,data=R) %>% 
  recipe(internet~.,data=R) %>% 
  step_nzv(all_predictors())  %>% 
  step_knnimpute(all_numeric(), neighbors = 6) %>%
  # step_other(all_nominal(), threshold = 0.01,
  #            other = "other") %>%
  step_unknown(all_nominal()) %>%
  step_dummy(all_nominal(), one_hot = FALSE)

train_dat <- prep(blueprint, training = R) %>% 
  bake(., new_data=R)

names(train_dat)<-str_replace_all(names(train_dat),"[.]","_")

test_dat <- prep(blueprint, training = S) %>% 
  bake(., new_data=S)

names(test_dat)<-str_replace_all(names(test_dat),"[.]","_")


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

# for (i in 1:12){
#   NAs<-sum(is.na(X[,i]))
#   print(NAs)
# }

# y1 <- train_dat %>%
#   select(int_home) %>%
#   pull

y <- train_dat %>%
  select(internet) %>%
  pull

####  LM ####

rmods <- regsubsets(x=X, y=y, method="forward",
                    all.best=FALSE, nbest=3, 
                    nvmax=10, really.big = TRUE)

best_m<-get_best(regsubs = rmods, depv="internet", data=train_dat)
lapply(best_m, summary)



#### MARS ####

hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = 2:15
)


set.seed(3333)  # for reproducibility
cv_mars <- train(
  x = X,
  y = y,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

cv_mars$bestTune
cv_mars$results %>%
  filter(nprune == cv_mars$bestTune$nprune, degree == cv_mars$bestTune$degree)


ggplot(cv_mars)

cv_mars$finalModel %>%
  summary()

cv_mars$finalModel %>%
  fitted()


# variable importance plots
dotPlot(varImp(cv_mars))

### Internet HOME
# cv_mars2 <- train(
#   x = X,
#   y = y1,
#   method = "earth",
#   metric = "RMSE",
#   trControl = trainControl(method = "cv", number = 10),
#   tuneGrid = hyper_grid
# )
# 
# cv_mars2$bestTune
# cv_mars2$results %>%
#   filter(nprune == cv_mars2$bestTune$nprune, degree == cv_mars2$bestTune$degree)
# 
# 
# ggplot(cv_mars2)
# 
# cv_mars2$finalModel %>%
#   summary()
# 
# # variable importance plots
# dotPlot(varImp(cv_mars2))

#### Polywog ####

# install.packages("polywog")
library(polywog)

p <- train_dat %>%
  select(starts_with(c('age','gender',
                       'race','education','region',
                       'income',
                       'trust_peple',
                       'gov_waste',
                       'unemp',
                       'marital',
                       'Pres_for_re',
                       'occup')), 'internet' )

poly <-polywog(internet ~ ., data=p, degree=2, family = "binomial")

summary(poly)$coefficients %>% 
  as_tibble(rownames = "variable") %>% 
  filter(Estimate != 0)


# cv_poly <- function(split, ..., degree){
#   m <- polywog(log(wages) ~ age +
#                  education, data=analysis(split), degree=degree)
#   f <- predict(m, newdata=assessment(split))
#   e <- log(analysis(split)$wages) - f
#   mean(e^2)
# }
# out <- sapply(1:5, function(i){
#   vfold_cv(train,
#            v=10,
#            repeats = 10) %>%
#     mutate(err = map(splits,
#                      cv_poly,
#                      degree=i)) %>%
#     unnest(err) %>%
#     summarise(err= mean(err)) %>%
#     select(err) %>%
#     pull
# })

#### CART ####
library(rpart)
library(dplyr)
library(car)


bp <- #recipe(int_home~.,data=R) %>% 
  recipe(internet~.,data=R) %>% 
  step_nzv(all_predictors())  %>% 
  step_knnimpute(all_numeric(), neighbors = 6) %>%
  # step_other(all_nominal(), threshold = 0.01,
  #            other = "other") %>%
  step_unknown(all_nominal()) #%>%
  # step_dummy(all_nominal(), one_hot = FALSE)

train_dat1 <- prep(bp, training = R) %>% 
  bake(., new_data=R)

names(train_dat1)<-str_replace_all(names(train_dat1),"[.]","_")

test_dat1 <- prep(bp, training = S) %>% 
  bake(., new_data=S)

names(test_dat1)<-str_replace_all(names(test_dat1),"[.]","_")

crt <- train_dat1 %>%
  select(starts_with(c('age','gender',
                       'race','education','region',
                       'income',
                       'trust_peple',
                       'gov_waste',
                       'unemp',
                       'marital',
                       'Pres_for_re',
                       'occup')), 'internet' )



summary(cart)

# caret cross validation results
cart_cv <- train(
  internet ~ .,
  data = crt,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 20
)

cart_cv$bestTune$cp

library(vip)
vip(cart_cv)

dotPlot(varImp(cart_cv))

ggplot(cart_cv)

cart <- rpart(internet ~ ., data=crt, cp =cart_cv$bestTune$cp)
cart

plot(cart)
text(cart)


#### Random forest ####

# Modeling packages
# install.packages(c('ranger','h2o'))
library(ranger)   # a c++ implementation of random forest 
library(h2o)      # a java-based implementation of random forest

#Initialize
h2o.no_progress()
h2o.init(max_mem_size = "5g")

#Convert data to h2o
h2o_train<-as.h2o(crt)

# set the response column to Sale_Price
response <- "internet"

# set the predictor names
predictors <- setdiff(colnames(crt), response)

# number of features
n_features <- length(setdiff(colnames(crt), response))

# hyperparameter grid
hyper_grid <- list(
  mtries = floor(n_features * c(.05, .15, .25, .333, .4)),
  min_rows = c(1, 3, 5, 10),
  max_depth = c(10, 20, 30),
  sample_rate = c(.55, .632, .70, .80)
)

# random grid search strategy
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001,   # stop if improvement is < 0.1%
  stopping_rounds = 10,         # over the last 10 models
  max_runtime_secs = 60*3     # or stop search after 5 min.
)

# perform grid search 
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_random_grid_1",
  x = predictors, 
  y = response, 
  training_frame = h2o_train,
  hyper_params = hyper_grid,
  ntrees = n_features * 5,
  seed = 1333,
  stopping_metric = "RMSE",   
  stopping_rounds = 10,           # stop if last 10 trees added 
  stopping_tolerance = 0.005,     # don't improve RMSE by 0.5%
  search_criteria = search_criteria
)

random_grid

# collect the results and sort by our model performance metric 
# of choice
random_grid_perf <- h2o.getGrid(
  grid_id = "rf_random_grid_1", 
  sort_by = "mse", 
  decreasing = FALSE
)
random_grid_perf

h2o_rf2 <- h2o.randomForest(
  x = predictors, 
  y = response,
  training_frame = h2o_train, 
  nfolds=10, 
  max_depth=30, 
  min_rows=5.0, 
  mtries=3, 
  sample_rate=.8, 
  ntrees = 2500,
  seed = 3333
)

h2o_rf2



vip(h2o_rf2)

as.vector(predict(h2o_rf2,h2o_train)$predict)
#### GBM ####

h2o.no_progress()
h2o.init(max_mem_size = "5g")


hyper_grid <- list(
  max_depth=1:7,
  learn_rate = c(.001, .01, .1, .25),
  sample_rate = c(.5, .7, .9, 1), 
  col_sample_rate = c(.5, .7, .9, 1),
  col_sample_rate_per_tree = c(.5, .7, .9, 1)
  )

search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001,   # stop if improvement is < 0.1%
  stopping_rounds = 10,         # over the last 10 models
  max_runtime_secs = 60*3      # or stop search after 5 min.
)

# perform grid search 
grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "gbm_random_grid_1",
  x = predictors, 
  y = response,
  training_frame = h2o_train,
  hyper_params = hyper_grid,
  ntrees = 600,
  # learn_rate = 0.01,
  # max_depth = 7,
  # min_rows = 5,
  nfolds = 10,
  stopping_metric = "RMSE",
  stopping_rounds = 10,
  stopping_tolerance = 0.005,
  search_criteria = search_criteria,
  seed = 3333
)

grid
# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "gbm_random_grid_1", 
  sort_by = "mse", 
  decreasing = FALSE
)

grid_perf

# Grab the model_id for the top model, chosen by cross validation error
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let’s get performance metrics on the best model
h2o.performance(model = best_model, xval = TRUE)

g <- h2o.gbm(predictors, 
             response, 
             training_frame = h2o_train, 
             learn_rate = .1,
             max_depth=5,
             ntrees=100, 
             sample_rate=0.9,
             col_sample_rate = 0.5,
             col_sample_rate_per_tree = 0.7,
             nfolds=10)
g
vip(g)

#### Comparison on testing data ####

fit_training<- data.frame(
  lm = predict(best_m[[11]]),
  mars = as.vector(predict(cv_mars$finalModel)),
  poly = predict(poly),
  cart = as.vector(predict(cart_cv$finalModel)),
  rf = as.vector(predict(h2o_rf2,h2o_train)$predict),
  gbm = as.vector(predict(g,h2o_train)$predict)
)
  
fit_training

cor(fit_training,y)^2

apply(fit_training,2,function(x)rmse(y,x))
apply(fit_training,2,function(x)mse(y,x))

data.frame(
  cor.y_hat.y= as.vector(cor(fit_training, y)^2),
  rmse=apply(fit_training, 2,function(x)rmse(y,x)),
  mse=apply(fit_training, 2,function(x)mse(y,x))
)

fit_test<- data.frame(
  lm = predict(best_m[[11]],test_dat),
  mars = as.vector(predict(cv_mars$finalModel, test_dat)),
  poly = predict(poly, test_dat),
  cart = as.vector(predict(cart, newdata =  test_dat1)),
  rf = as.vector(predict(h2o_rf2,as.h2o(test_dat1))$predict),
  gbm = as.vector(predict(g,as.h2o(test_dat1))$predict)
)

yt <- test_dat %>%
  select(internet) %>%
  pull

fit_test

data.frame(
  cor.y_hat.y= as.vector(cor(fit_test,yt)^2),
  rmse=apply(fit_test,2,function(x)rmse(yt,x)),
  mse=apply(fit_test,2,function(x)mse(yt,x))
  )

apply(fit_test,2,function(x)rmse(yt,x))
apply(fit_test,2,function(x)mse(yt,x))

save.image()

save(h2o_rf2,g,
     file = "h2o_mods.RData")

h2o.shutdown()
