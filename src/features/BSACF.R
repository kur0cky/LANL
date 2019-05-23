# BSACF feature

library(tidyverse)

BSACF <- read_rds("data/processed/BSACF.rds")
tr <- read_csv("data/features/basic_features.csv")

BSACF_mean <- lapply(BSACF, function(x) apply(x, 2, mean)) %>% 
  do.call(rbind, .) %>% 
  as_tibble()
BSACF_mean_PC <- BSACF_mean %>% 
  prcomp() %>% 
  .$x %>%
  .[,1:4] %>% 
  as_tibble()
colnames(BSACF_mean_PC) <- str_c("BSACF_mean_", colnames(BSACF_mean_PC))

BSACF_sd <- lapply(BSACF, function(x) apply(x, 2, sd)) %>% 
  do.call(rbind, .) %>% 
  as_tibble()
BSACF_sd_PC <- BSACF_sd %>% 
  prcomp() %>% #summary %>% .$importance
  .$x %>%
  .[,1:8] %>% 
  as_tibble()
colnames(BSACF_sd_PC) <- str_c("BSACF_sd_", colnames(BSACF_sd_PC))

BSACF_ci <- lapply(BSACF, function(x) apply(x, 2, quantile, prob=.9) - apply(x, 2, quantile, prob=.1)) %>% 
  do.call(rbind, .) %>% 
  as_tibble()
BSACF_ci_PC <- BSACF_ci %>% 
  prcomp() %>% #summary %>% .$importance %>% .[1,] %>% plot
  .$x %>%
  .[,1:7] %>% 
  as_tibble()
colnames(BSACF_ci_PC) <- str_c("BSACF_ci_", colnames(BSACF_ci_PC))

BSACF_mean_PC %>% 
  write_csv("data/features/BSACF_mean_PC.csv")
BSACF_sd_PC %>% 
  write_csv("data/features/BSACF_sd_PC.csv")
BSACF_ci_PC %>% 
  write_csv("data/features/BSACF_ci_PC.csv")
