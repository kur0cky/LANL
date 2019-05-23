library(tidyverse)
library(tidymodels)
library(xgboost)
library(ranger)
library(ggforce)

features_boruta <- read_rds("data/features/features_boruta.RDS")
features_boruta_scale <- read_rds("data/features/features_boruta_scale.RDS")
sample <- read_csv("data/sample_submission.csv")
tr_te <- read_csv("data/features/features.csv") %>% 
  select(target, id, quake_index, features_boruta, sd.x, max)
tr <- tr_te %>% 
  drop_na() 
te <- tr_te %>% 
  filter(is.na(target))
# recipes----


rec <- recipe(target ~ . -id -quake_index, data = tr) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  # step_corr(all_numeric(), -all_outcomes(), -id, -quake_index, -sd.x, -max, threshold = .9) %>% 
  step_pca(all_numeric(), -all_outcomes(), -id, -quake_index, -sd.x, -max, threshold = .97) %>% 
  prep()

tr_bake <- bake(rec, tr)
te_bake <- bake(rec, te)

df_cv <- vfold_cv(tr_bake, strata = "target", v = 10)

options(warn=-1); df_cv <- df_cv %>% 
  mutate(fit = map(splits,
                   ~ MASS::stepAIC(glm(target ~ . -id -quake_index, data = analysis(.x),
                                       family = gaussian("log")))),
         pred = map2(fit, splits,
                     ~ predict(.x, assessment(.y), type = "response")),
         score = map2_dbl(pred, splits, 
                          ~ mean(abs(.x - assessment(.y)$target))))
df_cv %>% 
  summarise(mae = mean(score),
            mae_sd = sd(score),
            mae_min = min(score),
            mae_max = max(score)) %>% 
  unlist(); options(warn = 0) 
df_cv %>% 
  mutate(data = map(splits, assessment)) %>% 
  select(pred, data) %>% 
  unnest() %>% 
  mutate(pred = if_else(target <0.3125 & target > 0.275, 0.317, pred)) %>% 
  mutate(id = as.integer(str_remove(id, "train_"))) %>% 
  ggplot(aes(id, pred))+
  geom_point(colour = "blue", size=.5)+
  geom_line(aes(y = target))
df_cv %>% 
  mutate(data = map(splits, assessment)) %>% 
  select(pred, data) %>% 
  unnest() %>% 
  mutate(pred = if_else(target <0.3125 & target > 0.275, 0.317, pred)) %>% 
  summarise(MAE = mean(abs(target- pred))) %>% unlist
lapply(df_cv$fit, function(x) summary(x)$coefficients[,4]) %>% 
  do.call("rbind",.) %>% 
  as_tibble() %>% 
  gather(feature, pvalue) %>% 
  ggplot(aes(feature, pvalue))+
  geom_boxplot()+
  coord_flip()

df_cv <- df_cv %>% 
  mutate(fit = map(splits,
                   ~ ranger(target ~ . -id -quake_index,
                            data = analysis(.x),
                            num.trees = 500,
                            mtry = 4,
                            # splitrule = "extratrees",
                            importance = "impurity")),
         pred = map2(fit, splits,
                     ~ predict(.x, assessment(.y))$predictions),
         score = map2_dbl(pred, splits, 
                          ~ mean(abs(.x - assessment(.y)$target))))

df_cv %>% 
  mutate(data = map(splits, assessment)) %>% 
  select(pred, data) %>% 
  unnest() %>% 
  mutate(id = as.integer(str_remove(id, "train_"))) %>% 
  ggplot(aes(id, pred))+
  geom_point(colour = "blue", size=.5)+
  geom_line(aes(y = target))
df_cv %>% 
  summarise(mae = mean(score),
            mae_sd = sd(score),
            mae_min = min(score),
            mae_max = max(score))
map(df_cv$fit, ~ importance(.x)) %>% 
  do.call("rbind",.) %>% 
  as_tibble() %>% 
  gather(feature, importance) %>% 
  ggplot(aes(feature, importance))+
  geom_boxplot()+
  coord_flip()

# xgb----
dtrain <- tr %>% 
  select(-target, -id, -quake_index) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = tr_bake$target)
dtest <- te_bake %>% 
  select(colnames(dtrain)) %>% 
  as.matrix() %>% 
  xgb.DMatrix()


fair <- function(preds, dtrain) {
  d <- getinfo(dtrain, 'label') - preds
  c = .9
  den = abs(d) + c
  grad = -c*d / den
  hess = c*c / den ^ 2
  return(list(grad = grad, hess = hess))
}
logcosh <- function(preds, dtrain) {
  x <- getinfo(dtrain, 'label') - preds
  grad = -tanh(x)
  hess = 1 / cosh(x)^2
  return(list(grad = grad, hess = hess))
}
param <- list(max_depth = 5,
              min_child_weight = 2,
              colsample_bytree = 0.9,
              subsample = 0.9,
              eta = .02,
              silent = 1, 
              booster = "gbtree",
              # objective = "reg:gamma",
              # objective = logcosh,
              objective = fair,
              eval_metric = "mae",
              nthread = 1)

set.seed(1)
cv <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 10,
             early_stopping_rounds = 50,
             folds = df_cv$splits %>% map(function(x) which(!(1:nrow(tr_bake) %in% x$in_id))),
             prediction = TRUE)
fit_xgb <- xgb.train(params = param, dtrain, nrounds = cv$best_iteration)
xgb.importance(colnames(dtrain), fit_xgb) %>% 
  as_tibble %>% 
  ggplot(aes(Feature, Gain))+
  geom_bar(stat = "identity")+
  coord_flip()

sub <- te %>% 
  select(seg_id = id) %>% 
  mutate(time_to_failure = predict(fit_xgb, dtest)) %>% 
  right_join(select(sample, seg_id), by = "seg_id") %>% 
  mutate(time_to_failure = if_else(time_to_failure < 0, 0, time_to_failure)) %>% 
  replace_na(list(time_to_failure = 5.33))
sub %>% 
  write_csv("data/submit/kur0cky_EDA1.csv")

plot(tr_bake$target, cv$pred)

tibble(target = tr_bake$target,
       pred = cv$pred,
       # pred = predict(fit_xgb, dtrain)
       ) %>% 
  mutate(index = n():1) %>% 
  ggplot(aes(index))+
  geom_line(aes(y=target), colour = "black")+
  geom_point(aes(y = pred), colour = "blue", size = .7)
tibble(target = tr_bake$target,
       pred = cv$pred,
       # pred = predict(fit_xgb, dtrain)
       ) %>% 
  mutate(index = n():1) %>% 
  mutate(pred = if_else(target <0.3125 & target > 0.275, 0.317, pred)) %>%
  summarise(MAE = mean(abs(target- pred))) %>% unlist

tibble(target = tr_bake$target,
       pred = cv$pred,
       # pred = predict(fit_xgb, dtrain)
) %>% 
  ggplot(aes(target, abs(target - pred)))+
  geom_point() +
  facet_zoom(x = target < 0.4)
