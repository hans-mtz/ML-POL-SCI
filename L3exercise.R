mystery <- read.csv("https://quantoid.net/files/9592/mystery.csv")

mods <- list()
mods[[1]] <- glm(y ~ x, data=mystery)
mods[[2]] <- glm(y ~ poly(x,2,raw=TRUE), data=mystery)
mods[[3]] <- glm(y ~ poly(x,3,raw=TRUE), data=mystery)
mods[[4]] <- glm(y ~ poly(x,4,raw=TRUE), data=mystery)
mods[[5]] <- glm(y ~ poly(x,5,raw=TRUE), data=mystery)
mods[[6]] <- glm(y ~ poly(x,6,raw=TRUE), data=mystery)
mods[[7]] <- glm(y ~ poly(x,7,raw=TRUE), data=mystery)
mods[[8]] <- glm(y ~ poly(x,8,raw=TRUE), data=mystery)
mods[[9]] <- glm(y ~ poly(x,9,raw=TRUE), data=mystery)
# mods[[10]] <- glm(y ~ log(x), data=mystery)

set.seed(25343)
cv_fun <- function(split, ...){
  tmp <- lapply(mods, function(x)update(x, .~., data=analysis(split)))
  preds <- sapply(tmp, function(x)predict(x, newdata=assessment(split)))
  mse <- colMeans((assessment(split)$y-preds)^2)
  tibble(
    err = mse,
    model = factor(c("linear", paste0("poly-", 2:9),"log")))
}

v <- vfold_cv(mystery, v=10) %>%
  mutate(err = map(splits, cv_fun)) %>%
  unnest(err) %>%
  group_by(model) %>%
  summarise(err= mean(err))
v

ggplot(v, aes(x=model, y=err)) +
  geom_point(aes(x=model, y=err)) +
  geom_line(aes(x=as.numeric(model), y=err)) +
  theme_bw() +
  labs(x="Model", y="Cross-Validation Error")

reg_fun <- function(split, ...){
  mod <- lm(y~poly(x,8), data=analysis(split))
  fit <- predict(mod, newdata = data.frame())
  tibble(
    term = names(coef(mod)),
    estimate = coef(mod))
}
library(rio)
ces <- import("ces19.dta")
ces <- factorize(ces)
bsres <- bootstraps(ces, times=100, apparent=TRUE) %>%
  mutate(coef = map(splits, reg_fun))
