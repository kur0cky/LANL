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
      tr_features[[j + (i-1)*200]][[k]] <- seg[1:1000 + (k-1)*1000] %>% 
        acf(plot=F) %>% 
        .$acf %>% 
        .[,1,] %>% 
        .[-1]
    }
    print(j)
  }
  print(i);gc()
}
tr_features <- tr_features %>% 
  head(4194)
tr_acf_list <- lapply(tr_features, function(x) do.call(rbind, x))

tr <-  tr_features %>% 
  head(4194) %>% 
  do.call("rbind",.) %>% 
  as_tibble()

for(i in 1:ncol(tr)){
  tr[!is.finite(tr[,i][[1]]),i] <- 0
}

colnames(tr) <- colnames(tr) %>% 
  str_remove("%")

paths <- list.files("data/test")
te_features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  seg <- scale(df_seg$acoustic_data, scale = F)[,1]
  te_features[[i]] <- list()
  for(k in 1:150){
    te_features[[i]][[k]] <- seg[1:1000 + (k-1)*1000] %>% 
      acf(plot=F) %>% 
      .$acf %>% 
      .[,1,] %>% 
      .[-1]
  }
  if(i %% 10 == 0)print(i)
}
te_acf_list <- lapply(te_features, function(x) do.call(rbind, x))

BSACF <- c(tr_acf_list, te_acf_list)
BSACF %>% 
  write_rds("data/processed/BSACF.rds")
