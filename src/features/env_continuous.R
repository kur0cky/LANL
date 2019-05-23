# 上がってから、下がるで1セット
library(tidyverse)
library(data.table)
library(dtwclust)
library(zoo)

tr_features <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    env10 <- compute_envelope(abs(scale(df_seg$acoustic_data, scale=FALSE)), 10L)$upper
    diff <- case_when(diff(env10) > 0 ~ 1L,
                      diff(env10) < 0 ~ -1L,
                      TRUE ~ NA_integer_) %>% 
      na.locf()
    env_cont_10 <- which(diff(diff) == 2) %>% diff
    env100 <- compute_envelope(abs(scale(df_seg$acoustic_data, scale=FALSE)), 100L)$upper
    env1000 <- compute_envelope(scale(df_seg$acoustic_data, scale=FALSE), 1000L)$upper
    tr_features[[(i-1)*200+j]] <- c(env_cont_10_sd = sd(env_cont_10),
                                    env_10_sum_10 = sum(if_else(env10 > 10, env10, 0)),
                                    env_10_sum_20 = sum(if_else(env10 > 20, env10, 0)),
                                    env_100_sum_10 = sum(if_else(env100 > 10, env10, 0)),
                                    env_100_sum_20 = sum(if_else(env100 > 20, env10, 0)),
                                    env_1000_sum_10 = sum(if_else(env1000 > 10, env10, 0)),
                                    env_1000_sum_20 = sum(if_else(env1000 > 20, env10, 0)),
                                    env_10_count_10 = sum(env10 > 10),
                                    env_10_count_20 = sum(env10 > 20),
                                    env_100_count_10 = sum(env100 > 10),
                                    env_100_count_20 = sum(env100 > 20),
                                    env_1000_count_10 = sum(env1000 > 10),
                                    env_1000_count_20 = sum(env1000 > 20)
                                    )
  }
  print(i);gc()
}

tr <- do.call("rbind", tr_features) %>% 
  as_tibble() %>% 
  drop_na()
colnames(tr) <- colnames(tr) %>% 
  str_remove("%")

paths <- list.files("data/test")
te_features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  env10 <- compute_envelope(abs(scale(df_seg$acoustic_data, scale=FALSE)), 10L)$upper
  diff <- case_when(diff(env10) > 0 ~ 1L,
                    diff(env10) < 0 ~ -1L,
                    TRUE ~ NA_integer_) %>% 
    na.locf()
  env_cont_10 <- which(diff(diff) == 2) %>% diff
  env100 <- compute_envelope(abs(scale(df_seg$acoustic_data, scale=FALSE)), 100L)$upper
  env1000 <- compute_envelope(scale(df_seg$acoustic_data, scale=FALSE), 1000L)$upper
  te_features[[i]] <- c(env_cont_10_sd = sd(env_cont_10),
                        env_10_sum_10 = sum(if_else(env10 > 10, env10, 0)),
                        env_10_sum_20 = sum(if_else(env10 > 20, env10, 0)),
                        env_100_sum_10 = sum(if_else(env100 > 10, env10, 0)),
                        env_100_sum_20 = sum(if_else(env100 > 20, env10, 0)),
                        env_1000_sum_10 = sum(if_else(env1000 > 10, env10, 0)),
                        env_1000_sum_20 = sum(if_else(env1000 > 20, env10, 0)),
                        env_10_count_10 = sum(env10 > 10),
                        env_10_count_20 = sum(env10 > 20),
                        env_100_count_10 = sum(env100 > 10),
                        env_100_count_20 = sum(env100 > 20),
                        env_1000_count_10 = sum(env1000 > 10),
                        env_1000_count_20 = sum(env1000 > 20)
  )
  if(i %% 10 == 0)print(i)
}
te <- do.call("rbind", te_features) %>% 
  as_tibble()
colnames(te) <- colnames(te) %>% 
  str_remove("%")

env_cont <- bind_rows(tr, te)
env_cont %>% 
  write_csv("data/features/env_cont.csv")
