library(lightgbm)
library(Matrix)


smat_train <- sparse.model.matrix(smoker ~ ., data = df_train)
smat_train <- tmp %>% 
  select(-id, -type) %>% 
  sparse.model.matrix(target ~ .,data = .)
dtrain <- lgb.Dataset(data = as.matrix(smat_train), 
                      label = tmp$target,
                      free_raw_data = FALSE)
params <- list(objective = "regression_l1",
               metric = "mae",
               num_leaves = 40,
               # min_data_in_leaf = 10,
               max_depth = -1,
               boosting = "gbdt",
               feature_fraction = 0.85
               )
model <- lgb.cv(params,
                dtrain,
                1000,
                nfold = 5,
                min_data = 1,
                learning_rate = .05,
                early_stopping_rounds = 10)
