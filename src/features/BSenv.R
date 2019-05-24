BSenv <- read_rds("data/processed/BSenvelope.rds")

BSenv_mean <- lapply(BSenv, function(x) apply(x, 2, mean)) %>% 
  do.call(rbind,.)

BSenv_sd <- lapply(BSenv, function(x) apply(x, 2, sd)) %>% 
  do.call(rbind,.)

BSenv_ci <- lapply(BSenv, function(x) apply(x, 2, quantile, prob=.9) - apply(x, 2, quantile, prob=.1)) %>% 
  do.call(rbind,.)

BSenv_mean_PC <- BSenv_mean %>% 
  prcomp %>% #summary %>% .$importance %>% .[1,1:20] %>% plot
  .$x %>% 
  .[,1:10] %>% 
  as_tibble()
colnames(BSenv_mean_PC) <- str_c("BSenv_mean_", colnames(BSenv_mean_PC))

BSenv_sd_PC <- BSenv_sd %>% 
  prcomp %>% #summary %>% .$importance %>% .[1,1:40] %>% plot
  .$x %>% 
  .[,1:6] %>% 
  as_tibble()
colnames(BSenv_sd_PC) <- str_c("BSenv_sd_", colnames(BSenv_sd_PC))

BSenv_ci_PC <- BSenv_ci %>% 
  prcomp %>% #summary %>% .$importance %>% .[1,1:40] %>% plot
  .$x %>% 
  .[,1:9] %>% 
  as_tibble()
colnames(BSenv_ci_PC) <- str_c("BSenv_ci_", colnames(BSenv_ci_PC))


BSenv_mean_PC %>% 
  write_csv("data/features/BSenv_mean_PC.csv")
BSenv_sd_PC %>% 
  write_csv("data/features/BSenv_sd_PC.csv")
BSenv_ci_PC %>% 
  write_csv("data/features/BSenv_ci_PC.csv")
