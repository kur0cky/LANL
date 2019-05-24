# BSACF feature

library(tidyverse)

BSPACF <- read_rds("data/processed/BSPACF.rds")
tr <- read_csv("data/features/basic_features.csv")

BSPACF_mean <- lapply(BSPACF, function(x) apply(x, 2, mean)) %>% 
  do.call(rbind, .) %>% 
  as_tibble()
BSPACF_mean_PC <- BSPACF_mean %>% 
  prcomp() %>% 
  .$x %>%
  .[,1:4] %>% 
  as_tibble()
colnames(BSPACF_mean_PC) <- str_c("BSPACF_mean_", colnames(BSPACF_mean_PC))

BSPACF_sd <- lapply(BSPACF, function(x) apply(x, 2, sd)) %>% 
  do.call(rbind, .) %>% 
  as_tibble()
BSPACF_sd_PC <- BSPACF_sd %>% 
  prcomp() %>% #summary %>% .$importance %>% .[1,] %>% plot
  .$x %>%
  .[,1:5] %>% 
  as_tibble()
colnames(BSPACF_sd_PC) <- str_c("BSPACF_sd_", colnames(BSPACF_sd_PC))

BSPACF_ci <- lapply(BSPACF, function(x) apply(x, 2, quantile, prob=.9) - apply(x, 2, quantile, prob=.1)) %>% 
  do.call(rbind, .) %>% 
  as_tibble()
BSPACF_ci_PC <- BSPACF_ci %>% 
  prcomp() %>% #summary %>% .$importance %>% .[1,] %>% plot
  .$x %>%
  .[,1:4] %>% 
  as_tibble()
colnames(BSPACF_ci_PC) <- str_c("BSPACF_ci_", colnames(BSPACF_ci_PC))

BSPACF_mean_PC %>% 
  write_csv("data/features/BSPACF_mean_PC.csv")
BSPACF_sd_PC %>% 
  write_csv("data/features/BSPACF_sd_PC.csv")
BSPACF_ci_PC %>% 
  write_csv("data/features/BSPACF_ci_PC.csv")
