library(tidyverse)
library(tidymodels)
library(xgboost)
library(ranger)
library(ggforce)

features_boruta <- read_rds("data/features/features_boruta.RDS")
features_boruta_scale <- read_rds("data/features/features_boruta_scale.RDS")
sample <- read_csv("data/sample_submission.csv")
tr_te <- read_csv("data/features/features.csv") %>% 
  select(target, id, quake_index, features_boruta_scale, sd.x, max)
tr <- tr_te %>% 
  drop_na() %>% 
  filter(quake_index !=1)
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

tr_bake <- tr_bake %>% 
  group_by(quake_index) %>% 
  mutate(scale = max(target),
         target_scaled = target / scale)

df_cv <- vfold_cv(tr_bake, strata = "target", v = 10)

df_cv <- df_cv %>% 
  mutate(scaled_fit = map(splits,
                   ~ ranger(target_scaled ~ . -id -quake_index-target-scale,
                            data = analysis(.x),
                            num.trees = 500,
                            mtry = 4,
                            classification = F,
                            # splitrule = "extratrees",
                            importance = "impurity")),
         pred_scaled = map2(scaled_fit, splits,
                     ~ predict(.x, assessment(.y))$predictions),
         scale_fit = map(splits,
                         ~ ranger(scale ~ . -id -quake_index-target-target_scaled,
                                  data = analysis(.x),
                                  num.trees = 500,
                                  mtry = 4,
                                  classification = F,
                                  # splitrule = "extratrees",
                                  importance = "impurity")),
         pred_scale = map2(scale_fit, splits,
                            ~ predict(.x, assessment(.y))$predictions))
df_cv %>% 
  mutate(data = map(splits, assessment)) %>% 
  select(pred_scale, pred_scaled, data) %>% 
  unnest() %>% 
  mutate(pred = pred_scaled * pred_scale,
         id = as.integer(str_remove(id, "train_"))) %>% 
  summarise(MAE = mean(abs(target - pred)))

df_cv %>% 
  mutate(data = map(splits, assessment)) %>% 
  select(pred_scale, pred_scaled, data) %>% 
  unnest() %>% 
  mutate(pred = pred_scaled * pred_scale,
         id = as.integer(str_remove(id, "train_"))) %>% 
  ggplot(aes(id, pred))+
  geom_point(colour = "blue", size = 1)+
  geom_line(aes(y = target))



df_cv <- df_cv %>% 
  mutate(fit = map(splits,
                   ~ ranger(scale ~ . -id -quake_index-target-target_scaled,
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
  ggplot(aes(scale, pred))+
  geom_point(size=.2)+
  geom_abline(intercept = 0, slope = 1)+
  geom_smooth(method = "glm")
