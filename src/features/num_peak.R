#  roll num_peaks
library(tidyverse)
library(data.table)
library(RcppRoll)
library(pracma)


tr_features <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    seg <- abs(scale(df_seg$acoustic_data, scale = F)[,1])
    tr_features[[j + (i-1)*200]] <- list()
    for(k in 1:150){
      tr_features[[j + (i-1)*200]][[k]] <- seg[1:1000 + (k-1)*1000] %>% 
        findpeaks(nups = 5, ndown=5) %>% 
        nrow()
    }
    print(j)
  }
  print(i);gc()
}
tr_features <- tr_features %>% 
  head(4194)
tr_num_peak_10_mean <- tr_features %>% 
  lapply(function(x) sum(unlist(x))/150) %>% 
  unlist()

paths <- list.files("data/test")
te_features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  seg <- abs(scale(df_seg$acoustic_data, scale = F)[,1])
  te_features[[i]] <- list()
  for(k in 1:150){
    te_features[[i]][[k]] <- seg[1:1000 + (k-1)*1000] %>% 
      findpeaks(nups = 5, ndown=5) %>% 
      nrow()
  }
  if(i %% 10 == 0)print(i)
}
te_features <- te_features %>% 
  head(4194)
te_num_peak_10_mean <- te_features %>% 
  lapply(function(x) sum(unlist(x))/150) %>% 
  unlist()

tibble(num_peak_10_mean = c(tr_num_peak_10_mean, te_num_peak_10_mean)) %>% 
  write_csv("data/features/num_peak_10_mean.csv")
