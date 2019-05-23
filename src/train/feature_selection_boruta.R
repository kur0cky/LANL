# feature selection with boruta
library(Boruta)
library(tidyverse)
features_kstest <- read_rds("data/features/features_kstest.rds")
tr <- read_csv("data/features/features.csv") %>% 
  drop_na() %>% 
  select(TTF, features_kstest)
  # filter(quake_index != 1) %>% 
  # group_by(quake_index) %>% 
  # mutate(scale = max(target),
  #        target_scaled = target / scale)


# normal----
set.seed(111)
res_boruta <- Boruta(TTF ~ .,
                     data = tr,
                     maxRuns = 200,
                     doTrace=2)

print(res_boruta)
plot(res_boruta)
attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp))
plotImpHistory(res_boruta)



attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp)) %>% 
  filter(decision == "Confirmed") %>% 
  .$feature %>% 
  write_rds("data/features/features_boruta_all.rds")



# type
tr_type <- tr %>% 
  filter(acc_sd < 100) %>% 
  mutate(type = case_when(TTF < 0.3 ~ "after",
                          TRUE ~ "normal"),
         type = factor(type, levels = c("normal", "after"))) %>% 
  select(-TTF)
res_boruta <- Boruta(type ~ .,
                     data = tr_type,
                     maxRuns = 200,
                     doTrace=2)

print(res_boruta)
plot(res_boruta)
attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp))
plotImpHistory(res_boruta)



attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp)) %>% 
  filter(decision == "Confirmed") %>% 
  .$feature %>% 
  write_rds("data/features/features_boruta_type.rds")

# 
tr_after <- tr %>% 
  filter(TTF < 0.3)
set.seed(111)
res_boruta <- Boruta(TTF ~ .,
                     data = tr_after,
                     maxRuns = 200,
                     doTrace=2)

print(res_boruta)
plot(res_boruta)
attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp))
plotImpHistory(res_boruta)



attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp)) %>% 
  filter(decision == "Confirmed") %>% 
  .$feature %>% 
  write_rds("data/features/features_boruta_after.rds")


# normal----
tr_normal <- tr %>% 
  filter(TTF > 0.3)
set.seed(111)
res_boruta <- Boruta(TTF ~ .,
                     data = tr_normal,
                     maxRuns = 200,
                     doTrace=2)

print(res_boruta)
plot(res_boruta)
attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp))
plotImpHistory(res_boruta)



attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp)) %>% 
  filter(decision == "Confirmed") %>% 
  .$feature %>% 
  write_rds("data/features/features_boruta_normal.rds")


# scaled----
tr_scaled <- tr %>%
  mutate(wave_index = folds$wave_index) %>% 
  group_by(wave_index) %>% 
  mutate(scaled = TTF / max(TTF)) %>% 
  ungroup() %>% 
  select(-TTF, -wave_index)
set.seed(111)
res_boruta <- Boruta(scaled ~ .,
                     data = tr_scaled,
                     maxRuns = 200,
                     doTrace=2)

print(res_boruta)
plot(res_boruta)
attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp))
plotImpHistory(res_boruta)



attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp)) %>% 
  filter(decision == "Confirmed") %>% 
  .$feature %>% 
  write_rds("data/features/features_boruta_scaled.rds")

# scale----
tr_scale <- tr %>%
  mutate(wave_index = folds$wave_index) %>% 
  group_by(wave_index) %>% 
  mutate(scale = max(TTF)) %>% 
  ungroup() %>% 
  select(-TTF, -wave_index)
set.seed(111)
res_boruta <- Boruta(scale ~ .,
                     data = tr_scale,
                     maxRuns = 200,
                     doTrace=2)

print(res_boruta)
plot(res_boruta)
attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp))
plotImpHistory(res_boruta)



attStats(res_boruta) %>%
  rownames_to_column("feature") %>%
  arrange(desc(meanImp)) %>% 
  filter(decision == "Confirmed") %>% 
  .$feature %>% 
  write_rds("data/features/features_boruta_scale.rds")
