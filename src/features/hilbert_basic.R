# hilbert transformation features
# basic statistics
library(tidyverse)
library(data.table)
library(seewave)

tr_features <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    hilb <- Mod(hilbert(df_seg$acoustic_data, f = 4000000, fftw=F))
    tr_features[[(i-1)*200+j]] <- c(hilbert_mean = mean(hilb),
                                    hilbert_median = median(hilb),
                                    hilbert_sd = sd(hilb),
                                    hilbert_skew = e1071::skewness(hilb),
                                    hilbert_kurtosis = e1071::kurtosis(hilb))
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
  hilb <- Mod(hilbert(df_seg$acoustic_data, f = 4000000, fftw=F))
  te_features[[i]] <- c(hilbert_mean = mean(hilb),
                        hilbert_median = median(hilb),
                        hilbert_sd = sd(hilb),
                        hilbert_skew = e1071::skewness(hilb),
                        hilbert_kurtosis = e1071::kurtosis(hilb))
  if(i %% 10 == 0)print(i)
}
te <- do.call("rbind", te_features) %>% 
  as_tibble()
colnames(te) <- colnames(te) %>% 
  str_remove("%")

hilbert_basics <- bind_rows(tr, te)
hilbert_basics %>% 
  write_csv("data/features/hilbert_basics.csv")


# ex----

index <- c(0)
tr <- tibble()
features <- list()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000
  
  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]$acoustic_data
    hilb <- Mod(hilbert(df_seq, f = 4000000, fftw=F))
    features[[sum(index[1:i])+ j]] <- c(hilbert_mean = mean(hilb),
                                        hilbert_median = median(hilb),
                                        hilbert_sd = sd(hilb),
                                        hilbert_skew = e1071::skewness(hilb),
                                        hilbert_kurtosis = e1071::kurtosis(hilb))
    print(sum(index[1:i])+ j)
  }
}
tr_hilbert_basic <- features %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_c("train_", 1:length(features))) %>% 
  select(id, everything())
write_csv(tr_hilbert_basic, "data/features/tr_hilbert_basic.csv")

paths <- list.files("data/test")
features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seq <- read_csv(str_c("data/test/", paths[i]), col_types = "d")$acoustic_data
  hilb <- Mod(hilbert(df_seq, f = 4000000, fftw=F))
  features[[i]] <- c(hilbert_mean = mean(hilb),
                     hilbert_median = median(hilb),
                     hilbert_sd = sd(hilb),
                     hilbert_skew = e1071::skewness(hilb),
                     hilbert_kurtosis = e1071::kurtosis(hilb))
  if(i %% 10 == 0)print(i)
}

te_hilbert_basic <- features %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_remove(paths, ".csv")) %>% 
  select(id, everything())
write_csv(te_hilbert_basic, "data/features/te_hilbert_basic.csv")
