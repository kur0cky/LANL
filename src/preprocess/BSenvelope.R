# 上がってから、下がるで1セット
library(tidyverse)
library(data.table)
library(RcppRoll)

tr_features <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    seg <- scale(df_seg$acoustic_data, scale = F)[,1]
    tr_features[[j + (i-1)*200]] <- list()
    for(k in 1:150){
      spec <- log(Mod(fft(seg[1:1000 + (k-1)*1000]))/1000)
      spec[!is.finite(spec)] <- median(spec)
      ceps <- ceps_res <- Mod(fft(spec)/1000)
      a <- 10
      ceps[a:(length(ceps)-a+1)] <- 0
      env <- (Mod(fft(ceps, inverse=TRUE)))[1:500]
      tr_features[[(i-1)*200 + j]][[k]] <- env[(0:100)*5]
    }
    print(j)
  }
  print(i);gc()
}
tr_features <- tr_features %>% 
  head(4194)
tr_env_list <- lapply(tr_features, function(x) do.call(rbind, x))

paths <- list.files("data/test")
te_features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  seg <- scale(df_seg$acoustic_data, scale = F)[,1]
  te_features[[i]] <- list()
  for(k in 1:150){
    spec <- log(Mod(fft(seg[1:1000 + (k-1)*1000]))/1000)
    spec[!is.finite(spec)] <- median(spec)
    ceps <- ceps_res <- Mod(fft(spec)/1000)
    a <- 10
    ceps[a:(length(ceps)-a+1)] <- 0
    env <- (Mod(fft(ceps, inverse=TRUE)))[1:500]
    te_features[[i]][[k]] <- env[(0:100)*5]
  }
  if(i %% 10 == 0)print(i)
}
te_env_list <- lapply(te_features, function(x) do.call(rbind, x))

BSenvelope <- c(tr_env_list, te_env_list)
BSenvelope %>% 
  write_rds("data/processed/BSenvelope.rds")
