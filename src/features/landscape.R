# TDA features

library(tidyverse)
library(data.table)
library(RcppRoll)
library(TDA)
library(tictoc)

# 1----
# tr_features <- list()
# tseq <- seq(0,200,2)
# for(i in 1:21){
#   df <- fread(str_c("data/processed/train_", i, ".csv"))
#   for(j in 1:200){
#     df_seg <- df[1:150000 + (j-1)*150000,]
#     seg <- abs(scale(df_seg$acoustic_data, scale = F)[,1]) %>% 
#       roll_mean(10L)
#     df_pc <- tibble(y =seg) %>% 
#       mutate(lag = lag(seg, 1),
#              lag2 = lag(seg, 2),
#              lag3 = lag(seg, 3),
#              lag4 = lag(seg, 4),
#              lag5 = lag(seg, 5),
#              lag6 = lag(seg, 6),
#              lag7 = lag(seg, 7),
#              lag8 = lag(seg, 8),
#              lag9 = lag(seg, 9),
#              lag10 = lag(seg, 10)) %>% 
#       drop_na() %>% 
#       prcomp() %>% 
#       .$x
#     tr_features[[(i-1)*200 + j]] <- list()
#     for(k in 1:15){
#       mat <- df_pc %>% 
#         .[1:10000 + (k-1)*9900,1:2]
#       diag <- alphaComplexDiag(mat) %>% 
#         .[[1]] %>% 
#         c() %>% 
#         matrix(ncol=3)
#       tr_features[[(i-1)*200 + j]][[k]] <- c(landscape(diag, dimension = 1, KK = 1, tseq = seq(0,200, 2)))
#       gc()
#     }
#     print(j)
#   }
#   print(i);gc()
# }
# 
# tr <-  tr_features %>% 
#   head(2000) 
# write_rds(tr, "data/processed/tr1_landscape.rds")
# 
# 
# # 2----
# tr_features <- list()
# tseq <- seq(0,200,2)
# for(i in 11:21){
#   df <- fread(str_c("data/processed/train_", i, ".csv"))
#   for(j in 1:200){
#     df_seg <- df[1:150000 + (j-1)*150000,]
#     seg <- abs(scale(df_seg$acoustic_data, scale = F)[,1]) %>% 
#       roll_mean(10L)
#     df_pc <- tibble(y =seg) %>% 
#       mutate(lag = lag(seg, 1),
#              lag2 = lag(seg, 2),
#              lag3 = lag(seg, 3),
#              lag4 = lag(seg, 4),
#              lag5 = lag(seg, 5),
#              lag6 = lag(seg, 6),
#              lag7 = lag(seg, 7),
#              lag8 = lag(seg, 8),
#              lag9 = lag(seg, 9),
#              lag10 = lag(seg, 10)) %>% 
#       drop_na() %>% 
#       prcomp() %>% 
#       .$x
#     tr_features[[(i-1)*200 + j]] <- list()
#     for(k in 1:15){
#       mat <- df_pc %>% 
#         .[1:10000 + (k-1)*9900,1:2]
#       diag <- alphaComplexDiag(mat) %>% 
#         .[[1]] %>% 
#         c() %>% 
#         matrix(ncol=3)
#       tr_features[[(i-1)*200 + j]][[k]] <- c(landscape(diag, dimension = 1, KK = 1, tseq = seq(0,200, 2)))
#       gc()
#     }
#     print(j)
#   }
#   print(i);gc()
# }
# 
# tr <-  tr_features[2001:4194]
# write_rds(tr, "data/processed/tr2_landscape.rds")
# 
# # te----
# paths <- list.files("data/test")
# te_features <- list()
# for(i in 1:length(list.files("data/test"))){
#   df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
#   seg <- abs(scale(df_seg$acoustic_data, scale = F)[,1]) %>% 
#     roll_mean(10L)
#   df_pc <- tibble(y =seg) %>% 
#     mutate(lag = lag(seg, 1),
#            lag2 = lag(seg, 2),
#            lag3 = lag(seg, 3),
#            lag4 = lag(seg, 4),
#            lag5 = lag(seg, 5),
#            lag6 = lag(seg, 6),
#            lag7 = lag(seg, 7),
#            lag8 = lag(seg, 8),
#            lag9 = lag(seg, 9),
#            lag10 = lag(seg, 10)) %>% 
#     drop_na() %>% 
#     prcomp() %>% 
#     .$x
#   te_features[[i]] <- list()
#   for(k in 1:15){
#     mat <- df_pc %>% 
#       .[1:10000 + (k-1)*9900,1:2]
#     diag <- alphaComplexDiag(mat) %>% 
#       .[[1]] %>% 
#       c() %>% 
#       matrix(ncol=3)
#     te_features[[i]][[k]] <- c(landscape(diag, dimension = 1, KK = 1, tseq = seq(0,200, 2)))
#     gc()
#   }
#   seg <- scale(df_seg$acoustic_data, scale = F)
#   print(i)
# }
# 
# te <- te_features
# te %>% write_rds("data/processed/te_landscape.rds")


lands <- c(read_rds("data/processed/tr1_landscape.rds"),
           read_rds("data/processed/tr2_landscape.rds"),
           read_rds("data/processed/te_landscape.rds"))

land_mean <- lapply(lands, function(x) apply(do.call(rbind,x), 2, mean)) %>% 
  do.call(rbind,.) %>% 
  .[,-1] %>% 
  as_tibble()
land_q90 <- lapply(lands, function(x) apply(do.call(rbind,x), 2, quantile, prob=.9)) %>% 
  do.call(rbind,.) %>% 
  .[,-1] %>% 
  as_tibble()
land_q10 <- lapply(lands, function(x) apply(do.call(rbind,x), 2, quantile, prob=.9)) %>% 
  do.call(rbind,.) %>% 
  .[,-1] %>% 
  as_tibble()
land_q50 <- lapply(lands, function(x) apply(do.call(rbind,x), 2, quantile, prob=.5)) %>% 
  do.call(rbind,.) %>% 
  .[,-1] %>% 
  as_tibble()

write_csv(land_mean, "data/processed/land_mean.csv")
write_csv(land_q10, "data/processed/land_q10.csv")
write_csv(land_q90, "data/processed/land_q90.csv")
write_csv(land_q50, "data/processed/land_q50.csv")

tibble(land_mean = sapply(lapply(lands, unlist), mean),
       land_median = sapply(lapply(lands, unlist), median),
       land_sd = sapply(lapply(lands, unlist), sd),
       land_q25 = sapply(lapply(lands, unlist), quantile, prob=.25),
       land_q90 = sapply(lapply(lands, unlist), quantile, prob = .9)) %>% 
  write_csv("data/features/landscape.csv")
