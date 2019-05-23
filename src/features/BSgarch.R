library(tidyverse)

BSgarch <- read_rds("data/processed/BSgarch.rds")

BSgarch_mean <- lapply(BSgarch, function(x) apply(x, 2, mean)) %>% 
  do.call(rbind, .) %>% 
  as_tibble()
BSgarch_sd <- lapply(BSgarch, function(x) apply(x, 2, sd)) %>% 
  do.call(rbind, .) %>% 
  as_tibble()
BSgarch_ci <- lapply(BSgarch, 
                     function(x) apply(x, 2, quantile, prob=.9) - apply(x, 2, quantile, prob=.1)) %>% 
  do.call(rbind, .) %>% 
  as_tibble()
colnames(BSgarch_mean) <- str_c("BSgarch_mean_", colnames(BSgarch_mean))
colnames(BSgarch_sd) <- str_c("BSgarch_sd_", colnames(BSgarch_sd))
colnames(BSgarch_ci) <- str_c("BSgarch_ci_", colnames(BSgarch_ci))

BSgarch_mean %>% 
  write_csv("data/features/BS_garch_mean.csv")
BSgarch_sd %>% 
  write_csv("data/features/BS_garch_sd.csv")
BSgarch_ci %>% 
  write_csv("data/features/BS_garch_ci.csv")
