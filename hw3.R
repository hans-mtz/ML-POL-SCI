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
getwd()
anes16<-import("./ML/ANES/raw/anes_timeseries_2016_dta/anes_timeseries_2016.dta")

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


# Wrangling data ----------------------------------------------------------

library(recipes)
blueprint <- recipe(~ ., data=d1) %>%
  step_nzv(all_predictors())  %>% 
  step_knnimpute(all_numeric(), neighbors = 6) %>%
  step_other(all_nominal(), threshold = 0.01,
             other = "other") %>%
  step_unknown(all_nominal()) %>%
  step_YeoJohnson(all_numeric()) %>% 
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal(), one_hot = FALSE)
prep_data <- recipes::prep(blueprint, d1)
tmp_data <- bake(prep_data, 
                 new_data=d1)

names(tmp_data)

# Select variables to cluster
pol_prof <- tmp_data %>%
  select(starts_with(c('trust_peple',
                       'gov_waste',
                       'Pres_for_re'))) #%>%
  # as.matrix()
pol_prof

## GLRM for the political profile----


library(h2o)
h2o.no_progress()  # turn off progress bars
h2o.init(max_mem_size = "5g")  # connect to H2O instance
pp_h <- as.h2o(pol_prof)
split <- h2o.splitFrame(pp_h, ratios = 0.6, seed = 123)
train <- split[[1]]
valid <- split[[2]]
# Create hyperparameter search grid
params <- expand.grid(
  regularization_x = c("None", "NonNegative", "L1"),
  regularization_y = c("None", "NonNegative", "L1"),
  gamma_x = seq(0, 1, by = .25),
  gamma_y = seq(0, 1, by = .25),
  error = 0,
  stringsAsFactors = FALSE
)

for(i in seq_len(nrow(params))) {
  # Create model
  glrm_pp <- h2o.glrm(
    training_frame = train,
    k = 2, 
    loss = "Quadratic",
    regularization_x = params$regularization_x[i], 
    regularization_y = params$regularization_y[i],
    gamma_x = params$gamma_x[i],
    gamma_y = params$gamma_y[i],
    transform = "STANDARDIZE", 
    max_runtime_secs = 1000,
    seed = 333
  )
  # Predict on validation set and extract error
  validate <- h2o.performance(glrm_pp, valid)
  params$error[i] <- validate@metrics$numerr
}

t1 <- params %>% arrange(error) %>% head(10)

glrm_pp <- h2o.glrm(
  training_frame = pp_h,
  k = 2, 
  loss = "Quadratic",
  regularization_x = "None", 
  regularization_y = "NonNegative",
  gamma_x = 0,
  gamma_y = 0,
  transform = "STANDARDIZE", 
  max_runtime_secs = 1000,
  seed = 333
)

summary(glrm_pp)

t2<- glrm_pp@model$importance

glrm_pp@model

labels <- colnames(glrm_pp@model$archetypes)
t(glrm_pp@model$archetypes) %>% 
  as_tibble() %>% 
  mutate(label = labels) %>% 
  arrange(desc(Arch1),desc(Arch2))

a <- glrm_pp@model$archetypes
a <- t(a) %>% as_tibble() %>% 
  mutate(label = labels)
ggplot(a, aes(x=reorder(label, Arch1, mean))) + 
  geom_point(aes(y=Arch1, colour="Archetype 1")) + 
  geom_point(aes(y=Arch2, colour="Archetype 2")) + 
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  labs(x="", y="Archetype Value", 
       colour="") + 
  theme(legend.position="top") + 
  coord_flip()

p1 <- t(glrm_pp@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, reorder(feature, Arch1))) +
  geom_point()

p2 <- t(glrm_pp@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch2, reorder(feature, Arch2))) +
  geom_point()

# p2 <- t(glrm_pp@model$archetypes) %>% 
#   as.data.frame() %>% 
#   mutate(feature = row.names(.)) %>%
#   ggplot(aes(Arch1, Arch2, label = feature)) +
#   geom_text()

gridExtra::grid.arrange(p1, p2, nrow = 1)

save(t1,t2, file = 'HW3.Rdata')


# GLRM for the whole set --------------------------------------------------

library(h2o)
h2o.no_progress()  # turn off progress bars
h2o.init(max_mem_size = "5g")  # connect to H2O instance
d1_h <- as.h2o(tmp_data)
split <- h2o.splitFrame(d1_h, ratios = 0.6, seed = 123)
train <- split[[1]]
valid <- split[[2]]
# Create hyperparameter search grid
params <- expand.grid(
  regularization_x = c("None", "NonNegative", "L1"),
  regularization_y = c("None", "NonNegative", "L1"),
  gamma_x = seq(0, 1, by = .25),
  gamma_y = seq(0, 1, by = .25),
  error = 0,
  stringsAsFactors = FALSE
)

for(i in seq_len(nrow(params))) {
  # Create model
  glrm_model <- h2o.glrm(
    training_frame = train,
    k = 2, 
    loss = "Quadratic",
    regularization_x = params$regularization_x[i], 
    regularization_y = params$regularization_y[i],
    gamma_x = params$gamma_x[i],
    gamma_y = params$gamma_y[i],
    transform = "STANDARDIZE", 
    max_runtime_secs = 1000,
    seed = 123
  )
  # Predict on validation set and extract error
  validate <- h2o.performance(glrm_model, valid)
  params$error[i] <- validate@metrics$numerr
}

params %>% arrange(error) %>% head(10)

glrm_model <- h2o.glrm(
  training_frame = d1_h,
  k = 2, 
  loss = "Quadratic",
  regularization_x = "None", 
  regularization_y = "NonNegative",
  gamma_x = 0,
  gamma_y = 0,
  transform = "STANDARDIZE", 
  max_runtime_secs = 1000,
  seed = 123
)

summary(glrm_model)


glrm_model@model$importance

labels <- colnames(glrm_model@model$archetypes)
t(glrm_model@model$archetypes) %>% 
  as_tibble() %>% 
  mutate(label = labels) %>% 
  arrange(desc(Arch1),desc(Arch2))

a <- glrm_model@model$archetypes
a <- t(a) %>% as_tibble() %>% 
  mutate(label = row.names(.))
ggplot(a, aes(x=reorder(label, Arch1, mean))) + 
  geom_point(aes(y=Arch1, colour="Archetype 1")) + 
  geom_point(aes(y=Arch2, colour="Archetype 2")) + 
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  labs(x="", y="Archetype Value", 
       colour="") + 
  theme(legend.position="top") + 
  coord_flip()

p1 <- t(glrm_model@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, reorder(feature, Arch1))) +
  geom_point()

p2 <- t(glrm_model@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, Arch2, label = feature)) +
  geom_text()

gridExtra::grid.arrange(p1, p2, nrow = 1)
