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
  mutate(type = case_when(acc_sd > 100 ~ 2L,
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


# 回帰 TFF----
tmp <- tr %>% 
  filter(type %in% c(0, 1),
         wave_index != 1) %>% 
  group_by(wave_index) %>% 
  mutate(TFF = max(TTF) - TTF) %>% 
  ungroup()
folds <- folds_list  %>%  
  filter(tr$type %in% c(0, 1),
         tr$wave_index != 1) %>% 
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
cv_tff <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 10,
                 early_stopping_rounds = 50,
                 verbose = 1,
                 print_every_n = 10,
                 folds = folds,
                 prediction = TRUE)
fit_tff <- xgb.train(params = param, dtrain, nrounds = cv_tff$best_iteration)
xgb.importance(colnames(dtrain), fit_tff) %>% 
  ggplot(aes(Feature, Gain))+
  geom_bar(stat = "identity")+
  coord_flip()
res <- res %>%
  select(-starts_with("pred_tff")) %>% 
  left_join(tibble(id = filter(tr, type != 2, wave_index != 1)$id,
                   pred_tff = cv_tff$pred),
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
mean(abs((tr_stack$TTF - fit_ranger$predictions)))

fit_stack <- glm(TTF ~ .-pred_all, data = tr_stack, family= Gamma("log"))

mean(abs(tr_stack$TTF - fit_stack$fitted.values))
mean(abs(tr_stack$TTF - tr_stack$pred_all))
