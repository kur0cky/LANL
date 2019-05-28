# for stacking
library(tidyverse)
library(tidymodels)
library(data.table)
library(RcppRoll)
library(xgboost)
library(ggforce)

# data preparing----
feature <- scan("feature.txt", what="")
features_all <- features_after <- features_normal <-  features_scaled <- features_scale <- feature
features_type <- scan("feature_type.txt", what="")
folds <- read_csv("data/processed/folds.csv")
sample <- read_csv("data/sample_submission.csv")

tr_te <- read_csv("data/features/features.csv") 
tr <- tr_te %>% 
  drop_na(TTF) %>% 
  bind_cols(folds) %>% 
  mutate(type = case_when(tr$acc_sd > 100 ~ 2L,
                          TTF < 0.3 ~ 1L,
                          TRUE ~ 0L))
te <- tr_te %>% 
  filter(is.na(TTF))

res <- folds %>% 
  bind_cols(select(tr, TTF, type))

folds_list <- res %>% 
  mutate(flg = TRUE) %>% 
  select(id, fold_index, flg) %>% 
  spread(fold_index, flg, fill = FALSE) %>% 
  select(-id)

# pulse判別----
pulse_flg <- res$type == 2
fit_pulse <- MASS::lda(pulse_flg ~ acc_sd + acc_AD_q.99,
                       data = tr)

# type分類モデル----
tmp <- tr %>% 
  filter(res$type %in% c(0, 1))
folds <- folds_list  %>% 
  filter(res$type %in% c(0, 1)) %>%
  lapply(which)
label <- res %>% 
  filter(type %in% c(0, 1)) %>% 
  .$type
dtrain <- tmp %>% 
  select(features_type) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)
param <- list(max_depth = 5,
              min_child_weight = 3,
              colsample_bytree = 0.7,
              subsample = 0.9,
              eta = .03,
              booster = "gbtree",
              objective = "binary:logistic",
              eval_metric = "logloss",
              nthread = 1)
set.seed(1)
cv_type <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 10,
                  early_stopping_rounds = 50,
                  verbose = 1,
                  print_every_n = 10,
                  folds = folds,
                  prediction = TRUE)
fit_type <- xgb.train(params = param, dtrain, nrounds = cv_type$best_iteration)
xgb.importance(colnames(dtrain), fit_type) %>% 
  ggplot(aes(reorder(Feature, Gain), Gain))+
  geom_bar(stat = "identity")+
  coord_flip()
pred_type <- cv_type$pred
type <- label
fit_glm_type <- glm(label ~ pred_type,
                    family = binomial)
res <- res %>%
  select(-starts_with("pred_type")) %>% 
  left_join(tibble(id = filter(res, type != 2)$id,
                   pred_type = fit_glm_type$fitted.value),
            by = "id")
res %>%
  ggplot(aes(type, pred_type, colour = TTF))+
  geom_point(position = "jitter")+
  scale_colour_viridis_c()

# 回帰 all----
tmp <- tr %>% 
  filter(res$type %in% c(0, 1))
folds <- folds_list  %>%  
  filter(res$type %in% c(0, 1)) %>% 
  lapply(which)
label <- res %>% 
  filter(type %in% c(0, 1)) %>% 
  .$TTF
dtrain <- tmp %>% 
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
param <- list(max_depth = 5,
              min_child_weight = 2,
              colsample_bytree = 0.9,
              subsample = 0.9,
              eta = .05,
              silent = 1, 
              booster = "gbtree",
              # objective = "reg:gamma",
              # objective = logcosh,
              # objective = "multi:softprob",
              objective = fair,
              # objective = rmse,
              eval_metric = "mae",
              # num_class = 3,
              nthread = 1)
set.seed(1)
cv_all <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 10,
                 early_stopping_rounds = 50,
                 verbose = 1,
                 print_every_n = 10,
                 folds = folds,
                 prediction = TRUE)
fit_all <- xgb.train(params = param, dtrain, nrounds = cv_all$best_iteration)
xgb.importance(colnames(dtrain), fit_all) %>% 
  ggplot(aes(Feature, Gain))+
  geom_bar(stat = "identity")+
  coord_flip()
res <- res %>%
  select(-starts_with("pred_all")) %>% 
  left_join(tibble(id = filter(res, type != 2)$id,
                   pred_all = cv_all$pred),
            by = "id")
# 回帰 scaled----
tmp <- tr %>% 
  mutate(wave_index = res$wave_index) %>% 
  filter(res$type %in% c(0)) %>% 
  filter(wave_index != 1) %>% 
  group_by(wave_index) %>% 
  mutate(scaled = TTF/max(TTF)) %>% 
  ungroup()
folds <- folds_list  %>%  
  filter(res$type %in% c(0), res$wave_index != 1) %>% 
  lapply(which)
label = tmp$scaled
dtrain <- tmp %>% 
  select(features_scaled) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)
dtest <- tr %>% 
  filter(res$wave_index == 1 | res$type %in% c(1)) %>% 
  select(features_scaled) %>% 
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
param <- list(max_depth = 5,
              min_child_weight = 2,
              colsample_bytree = 0.9,
              subsample = 0.9,
              eta = .05,
              silent = 1, 
              booster = "gbtree",
              # objective = "reg:gamma",
              # objective = logcosh,
              # objective = "multi:softprob",
              objective = fair,
              eval_metric = "mae",
              # num_class = 3,
              nthread = 1)
set.seed(1)
cv_scaled <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 10,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    print_every_n = 10,
                    folds = folds,
                    prediction = TRUE)
fit_scaled <- xgb.train(params = param, dtrain, nrounds = cv_scaled$best_iteration)
xgb.importance(colnames(dtrain), fit_scaled) %>% 
  ggplot(aes(Feature, Gain))+
  geom_bar(stat = "identity")+
  coord_flip()

res <- res %>%
  select(-starts_with("pred_scaled")) %>% 
  left_join(tibble(id = filter(res, wave_index == 1|type %in% c(1))$id,
                   pred_scaled =predict(fit_scaled, dtest)) %>% 
              bind_rows(tibble(id = filter(res, type==0, wave_index!=1)$id,
                               pred_scaled = cv_scaled$pred)) ,
            by = "id")
# 回帰 scale----
tmp <- tr %>% 
  mutate(wave_index = res$wave_index) %>% 
  filter(res$type %in% c(0)) %>% 
  filter(wave_index != 1) %>% 
  group_by(wave_index) %>% 
  mutate(scale = max(TTF)) %>% 
  ungroup()
folds <- folds_list  %>%  
  filter(res$type %in% c(0), res$wave_index != 1) %>% 
  lapply(which)
label = tmp$scale
dtrain <- tmp %>% 
  select(features_scale) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)
dtest <- tr %>% 
  filter(res$wave_index == 1 | res$type %in% c(1)) %>% 
  select(features_scale) %>% 
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
param <- list(max_depth = 5,
              min_child_weight = 2,
              colsample_bytree = 0.9,
              subsample = 0.9,
              eta = .05,
              silent = 1, 
              booster = "gbtree",
              # objective = fair,
              objective = "reg:linear",
              eval_metric = "mae",
              # num_class = 3,
              nthread = 1)
set.seed(1)
cv_scale <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 10,
                   early_stopping_rounds = 50,
                   verbose = 1,
                   print_every_n = 10,
                   folds = folds,
                   prediction = TRUE)
fit_scale <- xgb.train(params = param, dtrain, nrounds = cv_scale$best_iteration)
xgb.importance(colnames(dtrain), fit_scale) %>% 
  ggplot(aes(Feature, Gain))+
  geom_bar(stat = "identity")+
  coord_flip()

res <- res %>%
  select(-ends_with("pred_scale")) %>%
  left_join(tibble(id = filter(res, wave_index == 1|type %in% c(1))$id,
                   pred_scale =predict(fit_scale, dtest)) %>% 
              bind_rows(tibble(id = filter(res, type==0, wave_index!=1)$id,
                               pred_scale = cv_scale$pred)) ,
            by = "id")


# 回帰 normal----

tmp <- tr %>% 
  filter(res$type %in% c(0))
folds <- folds_list  %>% 
  filter(res$type %in% c(0)) %>% 
  lapply(which)
label <- res %>% 
  filter(type %in% c(0)) %>% 
  .$TTF
dtrain <- tmp %>% 
  select(features_normal) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)
dtest <- tr %>% 
  filter(res$type %in% c(1)) %>% 
  select(features_normal) %>% 
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
param <- list(max_depth = 5,
              min_child_weight = 2,
              colsample_bytree = 0.9,
              subsample = 0.9,
              eta = .05,
              silent = 1, 
              booster = "gbtree",
              # objective = "reg:gamma",
              # objective = logcosh,
              # objective = "multi:softprob",
              objective = fair,
              eval_metric = "mae",
              # num_class = 3,
              nthread = 1)
set.seed(1)
cv_N <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 10,
               early_stopping_rounds = 50,
               verbose = 1,
               print_every_n = 10,
               folds = folds,
               prediction = TRUE)
fit_N <- xgb.train(params = param, dtrain, nrounds = cv_N$best_iteration)
xgb.importance(colnames(dtrain), fit_N) %>% 
  ggplot(aes(Feature, Gain))+
  geom_bar(stat = "identity")+
  coord_flip()
res <- res %>% 
  select(-starts_with("pred_N")) %>% 
  left_join(
    tibble(id = filter(res, type %in% c(1))$id,
           pred_N = predict(fit_N, dtest)) %>%
      bind_rows(tibble(id = filter(res, type == 0)$id,
                       pred_N = cv_N$pred)),
    by = "id")

# 回帰 after----
tmp <- tr %>% 
  filter(res$type %in% c(1))
folds <- folds_list  %>% 
  filter(res$type %in% c(1)) %>% 
  lapply(which)
label <- res %>% 
  filter(type %in% c(1)) %>% 
  .$TTF
dtrain <- tmp %>% 
  select(features_after) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = label)
dtest <- tr %>% 
  filter(res$type %in% c(0)) %>% 
  select(features_after) %>% 
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
param <- list(max_depth = 5,
              min_child_weight = 2,
              colsample_bytree = 0.9,
              subsample = 0.9,
              eta = .05,
              silent = 1, 
              booster = "gbtree",
              # objective = "reg:gamma",
              # objective = logcosh,
              # objective = "multi:softprob",
              objective = fair,
              eval_metric = "mae",
              # num_class = 3,
              nthread = 1)
set.seed(1)
cv_A <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 10,
               early_stopping_rounds = 50,
               verbose = 1,
               print_every_n = 10,
               folds = folds,
               prediction = TRUE)
fit_A <- xgb.train(params = param, dtrain, nrounds = cv_A$best_iteration)
xgb.importance(colnames(dtrain), fit_A) %>% 
  ggplot(aes(Feature, Gain))+
  geom_bar(stat = "identity")+
  coord_flip()

res <- res %>% 
  select(-ends_with("pred_A")) %>% 
  left_join(
    tibble(id = filter(res, type %in% c(0))$id,
           pred_A = predict(fit_A, dtest)) %>%
      bind_rows(tibble(id = filter(res, type == 1)$id,
                       pred_A = cv_A$pred)),
    by = "id")

# 回帰 TFF----
tmp <- tr %>% 
  filter(type %in% c(0, 1),
         wave_index != 1) %>% 
  group_by(wave_index) %>% 
  mutate(TFF = max(TTF) - TTF) %>% 
  ungroup()
folds <- folds_list  %>%  
  filter(res$type %in% c(0, 1)) %>% 
  lapply(which)
label <- tmp %>% 
  .$TFF
dtrain <- tmp %>% 
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
param <- list(max_depth = 5,
              min_child_weight = 2,
              colsample_bytree = 0.9,
              subsample = 0.9,
              eta = .05,
              silent = 1, 
              booster = "gbtree",
              # objective = "reg:gamma",
              # objective = logcosh,
              # objective = "multi:softprob",
              objective = fair,
              # objective = rmse,
              eval_metric = "mae",
              # num_class = 3,
              nthread = 1)
set.seed(1)
cv_all <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 10,
                 early_stopping_rounds = 50,
                 verbose = 1,
                 print_every_n = 10,
                 folds = folds,
                 prediction = TRUE)
fit_all <- xgb.train(params = param, dtrain, nrounds = cv_all$best_iteration)
xgb.importance(colnames(dtrain), fit_all) %>% 
  ggplot(aes(Feature, Gain))+
  geom_bar(stat = "identity")+
  coord_flip()
res <- res %>%
  select(-starts_with("pred_all")) %>% 
  left_join(tibble(id = filter(res, type != 2)$id,
                   pred_all = cv_all$pred),
            by = "id")

# result----

tr_stack <- res %>% 
  select(TTF, starts_with("pred")) %>% 
  drop_na()

fit_ranger <- ranger(TTF ~ ., data = tr_stack, 
                     num.trees = 1500,
                     mtry = 1,
                     importance = "impurity")
ranger::importance(fit_ranger) %>% 
  barplot()
plot(tr_stack$TTF, fit_ranger$predictions)
fit_ranger
