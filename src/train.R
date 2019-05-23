library(tidyverse)
library(tidymodels)
library(ranger)

tr_te <- read_csv("data/features/features.csv")
sample <- read_csv("data/sample_submission.csv")
colnames(tr_te) <- str_remove(colnames(tr_te), "%")

tr <- tr_te %>% 
  drop_na()

te <- tr_te %>% 
  filter(is.na(target)) %>% 
  drop_na(envelope_2)

rec <- recipe(target ~ . -id, data = tr) %>% 
  # step_spatialsign(all_numeric(), -all_outcomes(), -id) %>%
  step_center(all_numeric(), -all_outcomes(), -id) %>%
  step_scale(all_numeric(), -all_outcomes(), -id) %>%
  step_corr(all_numeric(), -all_outcomes(), -quake_index, threshold = .95) %>%
  # step_pca(all_numeric(), - all_outcomes(), -quake_index, threshold = .95) %>%
  # step_center(all_numeric()) %>%
  # step_scale(all_numeric()) %>%
  prep()

tr_bake <- bake(rec, tr)
te_bake <- bake(rec, te) 
set.seed(1111)
fit <- glm(target ~ .-id -quake_index,
           data = tr_bake,
           family = Gamma(link = "log"))
step_fit <- MASS::stepAIC(fit)
summary(step_fit)
fit <- glm(target ~ . - id -peak_acf_lag -peak_acf_start_lag-sd_flg-peak_acf_end_lag-spec_ar_mode,
           data = tr_bake)
summary(fit)

plot(tr$target, predict(fit))
sub <- te %>% 
  select(seg_id = id) %>% 
  mutate(time_to_failure = predict(fit, te_bake)) %>% 
  right_join(select(sample, seg_id), by = "seg_id") %>% 
  mutate(time_to_failure = if_else(time_to_failure < 0, 0, time_to_failure)) %>% 
  replace_na(list(time_to_failure = 5.33))
  

sample <- sample %>% 
  mutate(time_to_failure = if_else(time_to_failure < 0, 0, time_to_failure)) %>% 
  replace_na(list(time_to_failure = 5.33))

sub %>% 
  write_csv("data/submit/lm_basic_feature_and_spectrum.csv")

# glmnet ----
set.seed(1)
cvfit <- cv.glmnet(x = as.matrix(select(tr_bake, -target, -quake_index, -id)), 
                   y = tr_bake$target, 
                   standardize = TRUE,
                   alpha = 1,
                   nfolds = 1000)
fit_lasso <- glmnet(x = as.matrix(select(tr_bake, -target, -quake_index, -id)), 
                    y = tr_bake$target, 
                    standardize = TRUE,
                    alpha = 1,
                    lambda = cvfit$lambda.min)

# hierNet----
library(hierNet)

fit=hierNet.path(x = as.matrix(select(tr_bake,q75,q90,mean_garch_fit,q25,p_garch_res.5,envelope_4)),
                 y = tr_bake$target,
                 minlam = 10,
                 maxlam = 150)
fitcv=hierNet.cv(fit,
                 x = as.matrix(select(tr_bake,q75,q90,mean_garch_fit,q25,p_garch_res.5,envelope_4)),
                 y = tr_bake$target)
print(fitcv)
plot(fitcv)

lamhat=fitcv$lamhat.1se
fit2=hierNet(x=as.matrix(select(tr_bake,q75,q90,mean_garch_fit,q25,p_garch_res.5,envelope_4)),
             y=tr_bake$target,
             lam=lamhat)
yhat=predict(fit2,x=as.matrix(select(tr_bake,q75,q90,mean_garch_fit,q25,p_garch_res.5,envelope_4)))

# ranger----

fit_ranger <- ranger(target ~ . -id -quake_index
                     -peak_acf_lag -peak_acf_start_lag-sd_flg-peak_acf_end_lag-spec_ar_mode
                     -mean_arma_res
                     +roll_1000_sd_p5 +roll_1000_sd_p10 +roll_1000_sd_p15 
                       -roll_1000_sd_p20 -roll_1000_sd_p25 -roll_1000_sd_p30 
                       -roll_1000_sd_p35 -roll_1000_sd_p40 -roll_1000_sd_p45 
                       -roll_1000_sd_p50 -roll_1000_sd_p55 -roll_1000_sd_p60 
                       -roll_1000_sd_p65 -roll_1000_sd_p70 -roll_1000_sd_p75 
                       -roll_1000_sd_p80 -roll_1000_sd_p85 -roll_1000_sd_p90 
                       +roll_1000_sd_p95 ,
                     data = tr,
                     mtry=2,
                     num.trees=3000,
                     importance = "impurity")

ranger::importance(fit_ranger) %>% 
  sort()

plot(tr$target, predict(fit_ranger, tr)$prediction)
sub <- te %>% 
  select(id) %>% 
  mutate(time_to_failure = predict(fit_ranger, te)$prediction) %>% 
  right_join(select(sample, -time_to_failure), by = c("id" = "seg_id")) %>% 
  rename(seg_id = id) %>% 
  replace_na(list(time_to_failure = 5.33))
sub %>% 
  write_csv("data/submit/RF_arma_garch_basic_spectrum_acf.csv")
# xgb----
dtrain <- xgb.DMatrix(as.matrix(select(tr_bake, -target, -id, -quake_index)),
                      label =  tr$target)
dtest <- xgb.DMatrix(as.matrix(select(te_bake, -target, -id, -quake_index))
                     )
param <- list(max_depth = 3,
              min_child_weight = 2,
              colsample_bytree = 0.9,
              subsample = 0.9,
              eta = .05,
              silent = 1, 
              booster = "gbtree",
              objective = "reg:gamma",
              eval_metric = "mae",
              nthread = 1)
set.seed(1)
cv <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 20,
             early_stopping_rounds = 100,
             prediction = TRUE)
predict(cv, dtest)
set.seed(1)
bst <- xgb.train(params = param, dtrain, nrounds = cv$best_iteration)
xgb.importance(colnames(dtrain), bst)
# lgb----
library(lightgbm)

train_mat <- tr_bake %>% 
  select(-id, -target) %>% 
  as.matrix()
train_lab  <- tr_bake$target
test_mat <- te_bake %>% 
  select(-id, -target) %>% 
  as.matrix()

dtrain = lgb.Dataset(data=train_mat, label=train_lab)
params = list(num_leaves= 10,
  min_data_in_leaf= 10, 
  min_sum_hessian_in_leaf= 10.0,
  min_child_samples=10,
  boost_from_average=FALSE,
  objective="regression",
  max_depth= -1,
  metric = "gamma",
  learning_rate= 0.001,
  boosting= "gbdt",
  feature_fraction= 0.91,
  bagging_freq= 1,
  bagging_fraction= 0.2,
  bagging_seed= 42,
  metric= mae,
  reg_alpha= 0.4002,
  reg_lambda=0.8003,
  verbosity= -1,
  nthread= -1,
  random_state= 42)

lgb.cv(params, dtrain, nfold=5, min_data=1, learning_rate=.1, early_stopping_rounds=100)


# target ~ mean + median + kurtosis + q99 + q90 + q01 + spec_ar_mode + 
#   envelope_2 + envelope_3 + envelope_4 + envelope_5 + envelope_6 + 
#   a0 + a1 + b1 + sd_garch_fit + sd_garch_res + p_garch_res.5 + 
#   mean_garch_res + p_garch_fit.5 + p_garch_fit.95 + ar1 + ma1 + 
#   ma2 + sd_arma_fit + sd_arma_res + mean_arma_res + mean_arma_fit + 
#   peak_acf + peak_acf_start_lag + peak_acf_end_lag + V1 + V3 + 
#   V4 + V5