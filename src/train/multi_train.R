library(tidyverse)
library(tidymodels)
library(data.table)
library(RcppRoll)
library(xgboost)

features_boruta <- read_rds("data/features/features_bourta.RDS")
features_type <- read_rds("data/features/features_type.RDS")
sample <- read_csv("data/sample_submission.csv")
tr_te <- read_csv("data/features/features.csv") %>% 
  select(target, id, quake_index, 
         features_boruta, sd.x, max, starts_with("sta_lta"),
         starts_with("roll"), -starts_with("q"),quake_index, starts_with("hilbert"))

tr <- tr_te %>% 
  drop_na(target) %>%
  mutate(type = case_when(target < 0.3125 & target > 0.275 ~ "pulse",
                          target <= 0.275 ~ "after",
                          # target > 10 ~ "before",
                          TRUE ~ "normal"))
te <- tr_te %>% 
  filter(is.na(target))
# recipes----


rec <- recipe(target ~ . -id -quake_index, data = tr) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  # step_corr(all_numeric(), -all_outcomes(), -id, -quake_index, -sd.x, -max, threshold = .95) %>%
  # step_pca(all_numeric(), -all_outcomes(), -id, -quake_index, -sd.x, -max, threshold = .97) %>%
  # step_ica(all_numeric(), -all_outcomes(), -id, -quake_index, -sd.x, -max, num_comp = 20) %>%
  prep()

tr_bake <- bake(rec, tr)  %>% 
  mutate(type = case_when(target < 0.3125 & target > 0.275 ~ 3L,
                          target <= 0.275 ~ 1L,
                          # target > 10 ~ 2L,
                          TRUE ~ 0L)) 
validation_set <- tr_bake %>% 
  mutate(validation_set = row_number(target) %% 10) %>% 
  select(id, validation_set) %>% 
  mutate(flg = T) %>% 
  spread(validation_set, flg, fill=FALSE) %>% 
  right_join(select(tr_bake, id), by = "id") 
res <- tr %>% 
  select(id, target, type)
te_bake <- bake(rec, te)
# pulse判別----
pulse_flg <- tr_bake$type == 3
pred_pulse <- MASS::lda(pulse_flg ~ sd.x*max, data = tr_bake,
                        CV =T)$class
fit_pulse <- MASS::lda(pulse_flg ~ sd.x*max, data = tr_bake)
res <- res %>% 
  mutate(pred_pulse = pred_pulse)

# type分類モデル----
tmp <- tr_bake %>% 
  filter(type %in% c(0,1))
folds <- validation_set %>% 
  semi_join(tmp, by = "id") %>% 
  select(-id) %>% 
  lapply(which)

dtrain <- tmp %>% 
  select(-target, -id, -quake_index, -type) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = tmp$type)
param <- list(max_depth = 4,
              min_child_weight = 5,
              colsample_bytree = 0.7,
              subsample = 0.9,
              eta = .05,
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
type <- tmp$type
fit_glm_type <- glm(type ~ pred_type,
                    family = binomial)
res <- res %>% 
  left_join(tibble(id = as.character(tmp$id),
                   pred_type = fit_glm_type$fitted.value))
res %>% 
  ggplot(aes(type, pred_type))+
  geom_point(position = "jitter")
# 回帰 all----
tmp <- tr_bake
folds <- validation_set %>% 
  semi_join(tmp, by = "id") %>% 
  select(-id) %>% 
  lapply(which)
dtrain <- tmp %>% 
  select(-target, -id, -quake_index, -type) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = tmp$target)

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
res <- tibble(id = tr_bake$id,
              pred_all = cv_all$pred) %>% 
  right_join(res) 
# 回帰 scaled----
tmp <- tr_bake %>% 
  filter(type %in% c(0)) %>% 
  filter(quake_index != min(quake_index)) %>% 
  group_by(quake_index) %>% 
  mutate(target = target/max(target)) %>% 
  ungroup()
folds <- validation_set %>% 
  semi_join(tmp, by = "id") %>% 
  select(-id) %>% 
  lapply(which)
dtrain <- tmp %>% 
  select(-target, -id, -quake_index, -type) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = tmp$target)
dtest <- tr_bake %>% 
  filter(quake_index == min(quake_index) | type %in% c(1)) %>% 
  select(-target, -id, -quake_index, -type) %>% 
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
res <- tibble(id = filter(tr_bake, quake_index == min(quake_index)|type %in% c(1))$id,
       pred_scaled =predict(fit_scaled, dtest)) %>% 
  bind_rows(tibble(id = tmp$id,
                   pred_scaled = cv_scaled$pred)) %>% 
  right_join(res) 
# 回帰 scale----
tmp <- tr_bake %>% 
  filter(type %in% c(0)) %>% 
  filter(quake_index != min(quake_index)) %>% 
  group_by(quake_index) %>% 
  mutate(target = max(target)) %>% 
  ungroup()
folds <- validation_set %>% 
  semi_join(tmp, by = "id") %>% 
  select(-id) %>% 
  lapply(which)
dtrain <- tmp %>% 
  select(-target, -id, -quake_index, -type) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = tmp$target)
dtest <- tr_bake %>% 
  filter(quake_index == min(quake_index) | type %in% c(1)) %>% 
  select(-target, -id, -quake_index, -type) %>% 
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
res <- tibble(id = filter(tr_bake, quake_index == min(quake_index)|type %in% c(1))$id,
              pred_scale =predict(fit_scale, dtest)) %>% 
  bind_rows(tibble(id = tmp$id,
                   pred_scale = cv_scale$pred)) %>% 
  right_join(res) 

# 回帰 normal----

tmp <- tr_bake %>% 
  filter(type %in% c(0))
folds <- validation_set %>% 
  semi_join(tmp, by = "id") %>% 
  select(-id) %>% 
  lapply(which)
dtrain <- tmp %>% 
  select(-target, -id, -quake_index, -type) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = tmp$target)
dtest <- tr_bake %>% 
  filter(type %in% c(1)) %>% 
  select(-target, -id, -quake_index, -type) %>% 
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
res <- tibble(id = filter(tr_bake, type %in% c(1))$id,
       pred_N =predict(fit_N, dtest)) %>% 
  bind_rows(tibble(id = tmp$id,
                   pred_N = cv_N$pred)) %>% 
  right_join(res) 

# 回帰 after----
tmp <- tr_bake %>% 
  filter(type %in% c(1))
folds <- validation_set %>% 
  semi_join(tmp, by = "id") %>% 
  select(-id) %>% 
  lapply(which)
dtrain <- tmp %>% 
  select(-target, -id, -quake_index, -type) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = tmp$target)
dtest <- tr_bake %>% 
  filter(type %in% c(0)) %>% 
  select(-target, -id, -quake_index, -type) %>% 
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
res <- tibble(id = filter(tr_bake, type %in% c(0))$id,
       pred_A =predict(fit_A, dtest)) %>% 
  bind_rows(tibble(id = tmp$id,
                   pred_A = cv_A$pred)) %>% 
  right_join(res)

# 結果----
res <- res %>% 
  mutate(pred_scaling = pred_scale * pred_scaled,
         pred = pred_N*(1-pred_type) + pred_A*pred_type,
         pred = (pred*1 + pred_scaling*1)/2,
         pred = if_else(pred_type > .98, pred_A, pred),
         pred = if_else(pred_pulse == TRUE, 0.25, pred)) 
res %>% 
  summarise(MAE = mean(abs(target - pred))) %>% 
  unlist

res %>% 
  mutate(id = -as.integer(str_remove(id, "train_"))) %>%
  ggplot(aes(id))+
  geom_line(aes(y = target))+
  geom_point(aes(y = pred), size=.3, colour = "blue")
res %>% 
  ggplot(aes(target, pred_N))+
  geom_abline(slope=1, colour="blue", size=2)+
  geom_point(size = .3)

res %>% 
  mutate(abs_error = abs(target - pred)) %>% 
  arrange(target) %>%
  mutate(roll_mean_abs_error = roll_mean(abs_error, n=100L, fill=NA)) %>% 
  ggplot(aes(target, roll_mean_abs_error))+
  geom_point()

res %>% 
  ggplot(aes(pred, pred_all, colour=target))+
  geom_point(size=1)+
  scale_colour_viridis_c(direction = -1)

res %>% 
  ggplot(aes(pred_all, pred_scaling))+
  geom_point(size=.5)
# pulse 0.28

res %>% 
  mutate(pred_scaling = pred_scaling*(1-pred_type) + pred_A*pred_type,
         pred_scaling = if_else(pred_pulse == TRUE, 0.25, pred_scaling)) %>% 
  select(id, target, pred, pred_scaling, pred_all) %>% 
  select(-id) %>% 
  GGally::ggpairs(size = .5)

res %>% 
  mutate(pred_scaling = pred_scaling*(1-pred_type) + pred_A*pred_type,
         pred_scaling = if_else(pred_pulse == TRUE, 0.25, pred_scaling)) %>% 
  select(id, target, pred, pred_scaling, target, pred_all) %>% 
  mutate(ensemble = pred/2 + pred_scaling/2) %>% 
  gather(type, pred, -target, -id) %>% 
  group_by(type) %>% 
  summarise(MAE = mean(abs(target - pred))) %>% 
  as.data.frame()

# res %>% 
#   mutate(pred_scaling = pred_scaling*(1-pred_type) + pred_A*pred_type,
#          pred_scaling = if_else(pred_pulse == TRUE, 0.25, pred_scaling)) %>% 
#   select(id, target, pred, pred_scaling, target, pred_all) %>% 
#   mutate(ensemble = pred/2 + pred_scaling/2) %>% 
#   gather(type, pred, -target, -id) %>% 
#   group_by(type) %>% 
#   summarise(MAE = mean(abs(target - pred))) %>% 
#   as.data.frame()

# submit----
dtest <- te_bake %>% 
  select(-target, -id, -quake_index, -type) %>% 
  as.matrix() %>% 
  xgb.DMatrix()
pred <- te %>% 
  select(seg_id = id) %>% 
  mutate(pred_pulse = predict(fit_pulse, te_bake)$class,
         pred_type = predict(fit_type, dtest),
         pred_typeb = predict(fit_glm_type, tibble(pred_type),
                              type = "response"),
         pred_all = predict(fit_all, dtest),
         pred_scaled = predict(fit_scaled, dtest),
         pred_scale = predict(fit_scale, dtest),
         pred_scaling = pred_scaled * pred_scale,
         pred_N = predict(fit_N, dtest),
         pred_A = predict(fit_A, dtest)) %>% 
  mutate(pred = pred_N*(1-pred_type) + pred_A*pred_type,
         pred = if_else(pred_pulse == TRUE, 0.25, pred),
         pred_scaling = pred_scaling*(1-pred_type) + pred_A*pred_type,
         pred_scaling = if_else(pred_pulse == TRUE, 0.25, pred_scaling),
         pred = pred/2 + pred_scaling/2
         )
sample %>% 
  left_join(pred, by = "seg_id") %>% 
  transmute(seg_id, time_to_failure = pred) %>% 
  write_csv("data/submit/simple_blending3.csv")
