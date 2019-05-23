# feature selection xgboost

library(tidyverse)
library(tidymodels)
library(data.table)
library(RcppRoll)
library(xgboost)

# data preparing----
features_all <- read_rds("data/features/features_boruta_all.rds")
features_type <- read_rds("data/features/features_boruta_type.RDS")
features_type <- read_rds("data/features/features_boruta_type.RDS")
features_after <- read_rds("data/features/features_boruta_after.RDS")
features_normal <- read_rds("data/features/features_boruta_normal.RDS")
features_scaled <- read_rds("data/features/features_boruta_scaled.RDS")
features_scale <- read_rds("data/features/features_boruta_scale.RDS")
folds <- read_csv("data/processed/folds.csv")
sample <- read_csv("data/sample_submission.csv")

tr_te <- read_csv("data/features/features.csv") 
tr <- tr_te %>% 
  drop_na(TTF) 

# type----
index <- tr$acc_sd < 100
validation_set <- folds[index,] %>% 
  select(id, fold_index) %>% 
  mutate(flg = T) %>% 
  spread(fold_index, flg, fill=F) %>% 
  select(-id) %>% 
  lapply(which)
label = if_else(tr$TTF < 0.3, 1L, 0L)[index]
dtrain <- tr %>% 
  filter(index) %>% 
  select(features_type) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)
params_type <- list(max_depth = 4,
                    min_child_weight = 2,
                    colsample_bytree = 0.7,
                    subsample = 0.9,
                    eta = .03,
                    booster = "gbtree",
                    objective = "binary:logistic",
                    eval_metric = "logloss",
                    nthread = 1)
set.seed(1234)

cv_type <- xgb.cv(params = params_type, dtrain, nrounds = 10000, nfold = 10,
                  early_stopping_rounds = 50,
                  verbose = 1,
                  folds = validation_set,
                  print_every_n = 10,
                  prediction = TRUE)
fit_type <- xgb.train(params = params_type, dtrain, nrounds = cv_type$best_iteration)
impo <- xgb.importance(colnames(dtrain), fit_type)

hcorr_type <- tr[index,] %>% 
  select(features_type) %>% 
  cor %>% 
  as.data.frame() %>% 
  rownames_to_column("feature1") %>%  
  as_tibble() %>% 
  gather(feature2, corr, -feature1) %>% 
  filter(feature1 != feature2) %>% 
  arrange(desc(abs(corr))) %>% 
  filter(corr > .95) %>% 
  left_join(impo, by = c("feature1" = "Feature")) %>% 
  left_join(impo, by = c("feature2" = "Feature")) %>% 
  mutate(feature = if_else(Gain.x > Gain.y, feature2, feature1)) %>% 
  distinct(feature) %>% 
  drop_na() %>% 
  .$feature

dtrain <- tr %>% 
  filter(index) %>% 
  select(features_type) %>% 
  select(-hcorr_type) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)

features_type_list <- list()
score_type <- list()
for(i in 1:(ncol(dtrain)-1)){
  cv_type <- xgb.cv(params = params_type, dtrain, nrounds = 10000, nfold = 10,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    folds = validation_set,
                    print_every_n = 10,
                    prediction = TRUE)
  fit_type <- xgb.train(params = params_type, dtrain, nrounds = cv_type$best_iteration)
  impo <- xgb.importance(colnames(dtrain), fit_type)
  features_type_list[[i]] <- impo$Feature
  score_type[[i]] <- cv_type$evaluation_log[cv_type$best_iteration,]
  dtrain <- tr %>% 
    filter(index) %>% 
    select(impo$Feature[1:(nrow(impo)-1)]) %>% 
    as.matrix() %>% 
    xgb.DMatrix(label = label)
  print(dim(dtrain))
}

score_type <- do.call(rbind, score_type)
do.call(rbind, score_type) %>% 
  filter(test_logloss_mean == min(test_logloss_mean))
features_type_list[[]]

score_type %>% 
  mutate(index = 1:n()) %>% 
  ggplot(aes(index, test_logloss_mean))+
  geom_ribbon(aes(ymin = test_logloss_mean - test_logloss_std,
                  ymax = test_logloss_mean + test_logloss_std),
              alpha = .3)+
  geom_line()
score_type %>% 
  mutate(index = 1:n()) %>% 
  arrange(test_logloss_mean) %>% 
  head()
features_type_list[[49]] %>% 
  write_rds("data/features/features_xgb_type.rds")

# all----

index <- tr$acc_sd < 100
validation_set <- folds[index,] %>% 
  select(id, fold_index) %>% 
  mutate(flg = T) %>% 
  spread(fold_index, flg, fill=F) %>% 
  select(-id) %>% 
  lapply(which)
label = tr$TTF[index]
dtrain <- tr %>% 
  filter(index) %>% 
  select(features_all) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)
fair <- function(preds, dtrain) {
  d <- getinfo(dtrain, 'label') - preds
  c = .9
  den = abs(d) + c
  grad = -c*d / den
  hess = c*c / den ^ 2
  return(list(grad = grad, hess = hess))
}
params_all <- list(max_depth = 5,
              min_child_weight = 2,
              colsample_bytree = 0.9,
              subsample = 0.9,
              eta = .03,
              silent = 1, 
              booster = "gbtree",
              objective = fair,
              eval_metric = "mae",
              nthread = 1)
set.seed(1234)

cv_all <- xgb.cv(params = params_all, dtrain, nrounds = 10000, nfold = 10,
                 early_stopping_rounds = 50,
                 verbose = 1,
                 folds = validation_set,
                 print_every_n = 10,
                 prediction = TRUE)
fit_all <- xgb.train(params = params_all, dtrain, nrounds = cv_all$best_iteration)
impo <- xgb.importance(colnames(dtrain), fit_all)

hcorr_all <- tr[index,] %>% 
  select(features_all) %>% 
  cor %>% 
  as.data.frame() %>% 
  rownames_to_column("feature1") %>%  
  as_tibble() %>% 
  gather(feature2, corr, -feature1) %>% 
  filter(feature1 != feature2) %>% 
  arrange(desc(abs(corr))) %>% 
  filter(corr > .95) %>% 
  left_join(impo, by = c("feature1" = "Feature")) %>% 
  left_join(impo, by = c("feature2" = "Feature")) %>% 
  mutate(feature = if_else(Gain.x > Gain.y, feature2, feature1)) %>% 
  distinct(feature) %>% 
  drop_na() %>% 
  .$feature

dtrain <- tr %>% 
  filter(index) %>% 
  select(features_all) %>% 
  select(-hcorr_all) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)

features_all_list <- list()
score_all <- list()
(ncol(dtrain)-1)
for(i in 48:63){
  cv_all <- xgb.cv(params = params_all, dtrain, nrounds = 10000, nfold = 10,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    folds = validation_set,
                    print_every_n = 10,
                    prediction = TRUE)
  fit_all <- xgb.train(params = params_all, dtrain, nrounds = cv_all$best_iteration)
  impo <- xgb.importance(colnames(dtrain), fit_all)
  features_all_list[[i]] <- impo$Feature
  score_all[[i]] <- cv_all$evaluation_log[cv_all$best_iteration,]
  dtrain <- tr %>% 
    filter(index) %>% 
    select(impo$Feature[1:(nrow(impo)-1)]) %>% 
    as.matrix() %>% 
    xgb.DMatrix(label = label)
  print(dim(dtrain));gc()
}

score_all <- do.call(rbind, score_all)

score_all %>% 
  mutate(index = 1:n()) %>% 
  ggplot(aes(index, test_mae_mean))+
  geom_ribbon(aes(ymin = test_mae_mean - test_mae_std,
                  ymax = test_mae_mean + test_mae_std),
              alpha = .3)+
  geom_line()
score_all %>% 
  mutate(index = 1:n()) %>% 
  arrange(test_mae_mean) %>% 
  head()
features_all_list[[50]] %>% 
  write_rds("data/features/features_xgb_all.rds")

# after----

index <- (tr$acc_sd < 100 & tr$TTF < 0.3)
validation_set <- folds[index,] %>% 
  select(id, fold_index) %>% 
  mutate(flg = T) %>% 
  spread(fold_index, flg, fill=F) %>% 
  select(-id) %>% 
  lapply(which)
label = tr$TTF[index]
dtrain <- tr %>% 
  filter(index) %>% 
  select(features_after) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)
fair <- function(preds, dtrain) {
  d <- getinfo(dtrain, 'label') - preds
  c = .9
  den = abs(d) + c
  grad = -c*d / den
  hess = c*c / den ^ 2
  return(list(grad = grad, hess = hess))
}
params_after <- list(max_depth = 5,
                   min_child_weight = 2,
                   colsample_bytree = 0.9,
                   subsample = 0.9,
                   eta = .03,
                   silent = 1, 
                   booster = "gbtree",
                   objective = fair,
                   eval_metric = "mae",
                   nthread = 1)
set.seed(1234)

cv_after <- xgb.cv(params = params_after, dtrain, nrounds = 10000, nfold = 10,
                 early_stopping_rounds = 50,
                 verbose = 1,
                 folds = validation_set,
                 print_every_n = 10,
                 prediction = TRUE)
fit_after <- xgb.train(params = params_after, dtrain, nrounds = cv_after$best_iteration)
impo <- xgb.importance(colnames(dtrain), fit_after)

hcorr_after <- tr[index,] %>% 
  select(features_after) %>% 
  cor %>% 
  as.data.frame() %>% 
  rownames_to_column("feature1") %>%  
  as_tibble() %>% 
  gather(feature2, corr, -feature1) %>% 
  filter(feature1 != feature2) %>% 
  arrange(desc(abs(corr))) %>% 
  filter(corr > .95) %>% 
  left_join(impo, by = c("feature1" = "Feature")) %>% 
  left_join(impo, by = c("feature2" = "Feature")) %>% 
  mutate(feature = if_else(Gain.x > Gain.y, feature2, feature1)) %>% 
  distinct(feature) %>% 
  drop_na() %>% 
  .$feature

dtrain <- tr %>% 
  filter(index) %>% 
  select(features_after) %>% 
  select(-hcorr_after) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)

features_after_list <- list()
score_after <- list()

for(i in 1:(ncol(dtrain)-1)){
  cv_after <- xgb.cv(params = params_after, dtrain, nrounds = 10000, nfold = 10,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   folds = validation_set,
                   print_every_n = 10,
                   prediction = TRUE)
  fit_after <- xgb.train(params = params_after, dtrain, nrounds = cv_after$best_iteration)
  impo <- xgb.importance(colnames(dtrain), fit_after)
  features_after_list[[i]] <- impo$Feature
  score_after[[i]] <- cv_after$evaluation_log[cv_after$best_iteration,]
  dtrain <- tr %>% 
    filter(index) %>% 
    select(impo$Feature[1:(nrow(impo)-1)]) %>% 
    as.matrix() %>% 
    xgb.DMatrix(label = label)
  print(dim(dtrain));gc()
}

score_after <- do.call(rbind, score_after)

score_after %>% 
  mutate(index = 1:n()) %>% 
  ggplot(aes(index, test_mae_mean))+
  geom_ribbon(aes(ymin = test_mae_mean - test_mae_std,
                  ymax = test_mae_mean + test_mae_std),
              alpha = .3)+
  geom_line()
score_after %>% 
  mutate(index = 1:n()) %>% 
  arrange(test_mae_mean) %>% 
  head()
features_after_list[[21]] %>% 
  write_rds("data/features/features_xgb_after.rds")

# normal----

index <- (tr$acc_sd < 100 & tr$TTF > 0.3)
validation_set <- folds[index,] %>% 
  select(id, fold_index) %>% 
  mutate(flg = T) %>% 
  spread(fold_index, flg, fill=F) %>% 
  select(-id) %>% 
  lapply(which)
label = tr$TTF[index]
dtrain <- tr %>% 
  filter(index) %>% 
  select(features_normal) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)
fair <- function(preds, dtrain) {
  d <- getinfo(dtrain, 'label') - preds
  c = .9
  den = abs(d) + c
  grad = -c*d / den
  hess = c*c / den ^ 2
  return(list(grad = grad, hess = hess))
}
params_normal <- list(max_depth = 5,
                     min_child_weight = 2,
                     colsample_bytree = 0.9,
                     subsample = 0.9,
                     eta = .03,
                     silent = 1, 
                     booster = "gbtree",
                     objective = fair,
                     eval_metric = "mae",
                     nthread = 1)
set.seed(1234)

cv_normal <- xgb.cv(params = params_normal, dtrain, nrounds = 10000, nfold = 10,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   folds = validation_set,
                   print_every_n = 10,
                   prediction = TRUE)
fit_normal <- xgb.train(params = params_normal, dtrain, nrounds = cv_normal$best_iteration)
impo <- xgb.importance(colnames(dtrain), fit_normal)

hcorr_normal <- tr[index,] %>% 
  select(features_normal) %>% 
  cor %>% 
  as.data.frame() %>% 
  rownames_to_column("feature1") %>%  
  as_tibble() %>% 
  gather(feature2, corr, -feature1) %>% 
  filter(feature1 != feature2) %>% 
  arrange(desc(abs(corr))) %>% 
  filter(corr > .95) %>% 
  left_join(impo, by = c("feature1" = "Feature")) %>% 
  left_join(impo, by = c("feature2" = "Feature")) %>% 
  mutate(feature = if_else(Gain.x > Gain.y, feature2, feature1)) %>% 
  distinct(feature) %>% 
  drop_na() %>% 
  .$feature

dtrain <- tr %>% 
  filter(index) %>% 
  select(features_normal) %>% 
  select(-hcorr_normal) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)

features_normal_list <- list()
score_normal <- list()

for(i in 1:(ncol(dtrain)-1)){
  cv_normal <- xgb.cv(params = params_normal, dtrain, nrounds = 10000, nfold = 10,
                     early_stopping_rounds = 50,
                     verbose = 1,
                     folds = validation_set,
                     print_every_n = 10,
                     prediction = TRUE)
  fit_normal <- xgb.train(params = params_normal, dtrain, nrounds = cv_normal$best_iteration)
  impo <- xgb.importance(colnames(dtrain), fit_normal)
  features_normal_list[[i]] <- impo$Feature
  score_normal[[i]] <- cv_normal$evaluation_log[cv_normal$best_iteration,]
  dtrain <- tr %>% 
    filter(index) %>% 
    select(impo$Feature[1:(nrow(impo)-1)]) %>% 
    as.matrix() %>% 
    xgb.DMatrix(label = label)
  print(dim(dtrain));gc()
}

score_normal <- do.call(rbind, score_normal)

score_normal %>% 
  mutate(index = 1:n()) %>% 
  ggplot(aes(index, test_mae_mean))+
  geom_ribbon(aes(ymin = test_mae_mean - test_mae_std,
                  ymax = test_mae_mean + test_mae_std),
              alpha = .3)+
  geom_line()
score_normal %>% 
  mutate(index = 1:n()) %>% 
  arrange(test_mae_mean) %>% 
  head()
features_normal_list[[48]] %>% 
  write_rds("data/features/features_xgb_normal.rds")

# scaled----

index <- (tr$acc_sd < 100)
validation_set <- folds[index,] %>% 
  select(id, fold_index) %>% 
  mutate(flg = T) %>% 
  spread(fold_index, flg, fill=F) %>% 
  select(-id) %>% 
  lapply(which)
label <- tr %>% 
  mutate(wave_index = folds$wave_index) %>% 
  group_by(wave_index) %>% 
  mutate(scaled = TTF / max(TTF)) %>% 
  ungroup() %>% 
  filter(index) %>% 
  .$scaled
dtrain <- tr %>% 
  filter(index) %>% 
  select(features_scaled) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)
fair <- function(preds, dtrain) {
  d <- getinfo(dtrain, 'label') - preds
  c = .9
  den = abs(d) + c
  grad = -c*d / den
  hess = c*c / den ^ 2
  return(list(grad = grad, hess = hess))
}
params_scaled <- list(max_depth = 5,
                      min_child_weight = 2,
                      colsample_bytree = 0.9,
                      subsample = 0.9,
                      eta = .03,
                      silent = 1, 
                      booster = "gbtree",
                      objective = fair,
                      eval_metric = "mae",
                      nthread = 1)
set.seed(1234)

cv_scaled <- xgb.cv(params = params_scaled, dtrain, nrounds = 10000, nfold = 10,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    folds = validation_set,
                    print_every_n = 10,
                    prediction = TRUE)
fit_scaled <- xgb.train(params = params_scaled, dtrain, nrounds = cv_scaled$best_iteration)
impo <- xgb.importance(colnames(dtrain), fit_scaled)

hcorr_scaled <- tr[index,] %>% 
  select(features_scaled) %>% 
  cor %>% 
  as.data.frame() %>% 
  rownames_to_column("feature1") %>%  
  as_tibble() %>% 
  gather(feature2, corr, -feature1) %>% 
  filter(feature1 != feature2) %>% 
  arrange(desc(abs(corr))) %>% 
  filter(corr > .95) %>% 
  left_join(impo, by = c("feature1" = "Feature")) %>% 
  left_join(impo, by = c("feature2" = "Feature")) %>% 
  mutate(feature = if_else(Gain.x > Gain.y, feature2, feature1)) %>% 
  distinct(feature) %>% 
  drop_na() %>% 
  .$feature

dtrain <- tr %>% 
  filter(index) %>% 
  select(features_scaled) %>% 
  select(-hcorr_scaled) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)

features_scaled_list <- list()
score_scaled <- list()

for(i in 1:(ncol(dtrain)-1)){
  cv_scaled <- xgb.cv(params = params_scaled, dtrain, nrounds = 10000, nfold = 10,
                      early_stopping_rounds = 50,
                      verbose = 1,
                      folds = validation_set,
                      print_every_n = 10,
                      prediction = TRUE)
  fit_scaled <- xgb.train(params = params_scaled, dtrain, nrounds = cv_scaled$best_iteration)
  impo <- xgb.importance(colnames(dtrain), fit_scaled)
  features_scaled_list[[i]] <- impo$Feature
  score_scaled[[i]] <- cv_scaled$evaluation_log[cv_scaled$best_iteration,]
  dtrain <- tr %>% 
    filter(index) %>% 
    select(impo$Feature[1:(nrow(impo)-1)]) %>% 
    as.matrix() %>% 
    xgb.DMatrix(label = label)
  print(dim(dtrain));gc()
}

score_scaled <- do.call(rbind, score_scaled)

score_scaled %>% 
  mutate(index = 1:n()) %>% 
  ggplot(aes(index, test_mae_mean))+
  geom_ribbon(aes(ymin = test_mae_mean - test_mae_std,
                  ymax = test_mae_mean + test_mae_std),
              alpha = .3)+
  geom_line()
score_scaled %>% 
  mutate(index = 1:n()) %>% 
  arrange(test_mae_mean) %>% 
  head()
features_scaled_list[[46]] %>% 
  write_rds("data/features/features_xgb_scaled.rds")

# scale----

index <- (tr$acc_sd < 100)
validation_set <- folds[index,] %>% 
  select(id, fold_index) %>% 
  mutate(flg = T) %>% 
  spread(fold_index, flg, fill=F) %>% 
  select(-id) %>% 
  lapply(which)
label <- tr %>% 
  mutate(wave_index = folds$wave_index) %>% 
  group_by(wave_index) %>% 
  mutate(scale = max(TTF)) %>% 
  ungroup() %>% 
  filter(index) %>% 
  .$scale
dtrain <- tr %>% 
  filter(index) %>% 
  select(features_scale) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)
fair <- function(preds, dtrain) {
  d <- getinfo(dtrain, 'label') - preds
  c = .9
  den = abs(d) + c
  grad = -c*d / den
  hess = c*c / den ^ 2
  return(list(grad = grad, hess = hess))
}
params_scale <- list(max_depth = 5,
                      min_child_weight = 2,
                      colsample_bytree = 0.9,
                      subsample = 0.9,
                      eta = .03,
                      silent = 1, 
                      booster = "gbtree",
                      objective = "reg:linear",
                      eval_metric = "mae",
                      nthread = 1)
set.seed(1234)

cv_scale <- xgb.cv(params = params_scale, dtrain, nrounds = 10000, nfold = 10,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    folds = validation_set,
                    print_every_n = 10,
                    prediction = TRUE)
fit_scale <- xgb.train(params = params_scale, dtrain, nrounds = cv_scale$best_iteration)
impo <- xgb.importance(colnames(dtrain), fit_scale)

hcorr_scale <- tr[index,] %>% 
  select(features_scale) %>% 
  cor %>% 
  as.data.frame() %>% 
  rownames_to_column("feature1") %>%  
  as_tibble() %>% 
  gather(feature2, corr, -feature1) %>% 
  filter(feature1 != feature2) %>% 
  arrange(desc(abs(corr))) %>% 
  filter(corr > .95) %>% 
  left_join(impo, by = c("feature1" = "Feature")) %>% 
  left_join(impo, by = c("feature2" = "Feature")) %>% 
  mutate(feature = if_else(Gain.x > Gain.y, feature2, feature1)) %>% 
  distinct(feature) %>% 
  drop_na() %>% 
  .$feature

dtrain <- tr %>% 
  filter(index) %>% 
  select(features_scale) %>% 
  select(-hcorr_scale) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)

features_scale_list <- list()
score_scale <- list()

for(i in 1:(ncol(dtrain)-1)){
  cv_scale <- xgb.cv(params = params_scale, dtrain, nrounds = 10000, nfold = 10,
                      early_stopping_rounds = 50,
                      verbose = 1,
                      folds = validation_set,
                      print_every_n = 10,
                      prediction = TRUE)
  fit_scale <- xgb.train(params = params_scale, dtrain, nrounds = cv_scale$best_iteration)
  impo <- xgb.importance(colnames(dtrain), fit_scale)
  features_scale_list[[i]] <- impo$Feature
  score_scale[[i]] <- cv_scale$evaluation_log[cv_scale$best_iteration,]
  dtrain <- tr %>% 
    filter(index) %>% 
    select(impo$Feature[1:(nrow(impo)-1)]) %>% 
    as.matrix() %>% 
    xgb.DMatrix(label = label)
  print(dim(dtrain));gc()
}

score_scale <- do.call(rbind, score_scale)

score_scale %>% 
  mutate(index = 1:n()) %>% 
  ggplot(aes(index, test_mae_mean))+
  geom_ribbon(aes(ymin = test_mae_mean - test_mae_std,
                  ymax = test_mae_mean + test_mae_std),
              alpha = .3)+
  geom_line()
score_scale %>% 
  mutate(index = 1:n()) %>% 
  arrange(test_mae_mean) %>% 
  head()
features_scale_list[[46]] %>% 
  write_rds("data/features/features_xgb_scale.rds")

