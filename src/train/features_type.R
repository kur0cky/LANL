# feature selection with boruta
library(Boruta)
library(tidyverse)
features_kstest <- read_rds("data/features/features_ks-test.RDS")
tr <- read_csv("data/features/features.csv") %>% 
  drop_na() %>% 
  select(target, id, features_kstest) %>% 
  mutate(type = case_when(target < 0.3125 & target > 0.275 ~ "pulse",
                          target <= 0.275 ~ "after",
                          # target > 10 ~ 2L,
                          TRUE ~ "normal")) %>% 
  filter(type != "pulse") %>% 
  mutate(type = as.factor(type))
set.seed(111)
res_boruta <- Boruta(type ~ .-target -id,
                     data = tr,
                     # maxRuns = 100,
                     doTrace=2)

print(res_boruta)
plot(res_boruta)  
attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp))
  # write_csv("data/features/boruta_type.csv")
plotImpHistory(res_boruta)
boruta_type <- read_csv("data/features/boruta_type.csv") %>% 
  # 相関みて.95より大きいものを排除。
  # どちらを消すかはimportance見て決めている
  filter(!(feature %in% c("sd.y",
                          "sd_arma_fit",
                          "hilbert_sd",
                          "p_garch_fit.5",
                          "kurtosis.y",
                          "mean_arma_fit",
                          "env_res_median",
                          "p_arma_res.5",
                          "mean_garch_fit",
                          "p_arma_res.95",
                          "mean_garch_fit",
                          "p_arma_res.95",
                          "sd_arma_res",
                          "ar1",
                          "sh",
                          "skewness.y",
                          "env_res_q.75",
                          "p_arma_fit.5",
                          "hilbert_kurtosis",
                          "hilbert_mean"
                          )))


boruta_features <- boruta_type %>% 
  filter(decision == "Confirmed") %>% 
  .$feature

tr <- tr %>% 
  select(target, id, type, boruta_features)

tr[,-c(1:3)] %>% 
  cor %>% 
  as.data.frame() %>% 
  rownames_to_column("feature1") %>%
  gather(feature2, cor, -feature1) %>% 
  as_tibble() %>% 
  filter(feature1 != feature2) %>%
  arrange(desc(abs(cor))) %>% View()
  

# CV----
tmp <- tr %>% 
  filter(type != "pulse")
validation_set <- tr %>% 
  mutate(validation_set = row_number(target) %% 10) %>% 
  select(id, validation_set) %>% 
  mutate(flg = T) %>% 
  spread(validation_set, flg, fill=FALSE) %>% 
  right_join(select(tr, id), by = "id") 
folds <- validation_set %>% 
  semi_join(tmp, by = "id") %>% 
  select(-id) %>% 
  lapply(which)

dtrain <- tmp %>% 
  select(-target, -id, -type) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = as.integer(as.factor(tmp$type))-1)
param <- list(max_depth = 4,
              min_child_weight = 5,
              colsample_bytree = .7,
              subsample = .9,
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
  ggplot(aes(x = Feature,
             # reorder(Feature, Gain),
             Gain))+
  geom_bar(stat = "identity")+
  coord_flip()
pred_type <- cv_type$pred
type <- tmp$type
fit_glm_type <- glm(type ~ pred_type,
                    family = binomial)
tibble(id = as.character(tmp$id),
       pred_type = fit_glm_type$fitted.value,
       type = tmp$type) %>% 
  ggplot(aes(type, pred_type))+
  geom_point(position = "jitter")

# feature_selection backward----
tmp <- tr %>% 
  filter(type != "pulse")
validation_set <- tr %>% 
  mutate(validation_set = row_number(target) %% 10) %>% 
  select(id, validation_set) %>% 
  mutate(flg = T) %>% 
  spread(validation_set, flg, fill=FALSE) %>% 
  right_join(select(tr, id), by = "id") 
folds <- validation_set %>% 
  semi_join(tmp, by = "id") %>% 
  select(-id) %>% 
  lapply(which)


param <- list(max_depth = 4,
              min_child_weight = 5,
              colsample_bytree = .7,
              subsample = .9,
              eta = .05,
              booster = "gbtree",
              objective = "binary:logistic",
              eval_metric = "logloss",
              nthread = 1)
set.seed(1)
features <- list()
score <- list()
features[[1]] <- tmp %>% 
  select(-target, -id, -type) %>% 
  colnames
for(i in 50:60){
  dtrain <- tmp %>% 
    select(features[[i]]) %>% 
    as.matrix() %>% 
    xgb.DMatrix(label = as.integer(as.factor(tmp$type))-1)
  set.seed(1)
  cv_type <- xgb.cv(params = param, dtrain, nrounds = 10000, nfold = 10,
                    early_stopping_rounds = 50,
                    verbose = 1,
                    print_every_n = 10,
                    folds = folds,
                    prediction = TRUE)
  score[[i]] <- cv_type$evaluation_log[cv_type$best_iteration,4:5]
  fit_type <- xgb.train(params = param, dtrain, nrounds = cv_type$best_iteration)
  impo <- xgb.importance(colnames(dtrain), fit_type)
  features[[i+1]] <- c(head(impo, nrow(impo)-1)$Feature)
  xgb.importance(colnames(dtrain), fit_type) %>% 
    ggplot(aes(Feature, Gain))+
    geom_bar(stat = "identity")+
    coord_flip()
}
score <- score %>% 
  do.call("rbind",.) %>% 
  as_tibble() %>% 
  mutate(index=1:n()) 
score %>% 
  ggplot(aes(index, test_logloss_mean))+
  geom_line()+
  geom_ribbon(aes(ymin = test_logloss_mean - test_logloss_std,
                  ymax = test_logloss_mean + test_logloss_std), alpha=.2)

features[[41]] %>% 
  write_rds("data/features/features_type.RDS")