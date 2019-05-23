library(tidyverse)
library(data.table)
library(e1071)

tr_features <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    tr_features[[(i-1)*200+j]] <- c(df_seg[150000,2],
                                    acc_mean = mean(df_seg$acoustic_data),
                                    acc_sd = sd(df_seg$acoustic_data),
                                    acc_median = median(df_seg$acoustic_data),
                                    acc_kurt = kurtosis(df_seg$acoustic_data),
                                    acc_skew = skewness(df_seg$acoustic_data),
                                    acc_AD_q = c(quantile(abs(df_seg$acoustic_data - mean(df_seg$acoustic_data)),
                                                          probs = c(.01, .05, .25, .75, .95, .99)))) %>% unlist()
  }
  print(i)
}

tr <- do.call("rbind", tr_features) %>% 
  as_tibble() %>% 
  rename(TTF = time_to_failure)
colnames(tr) <- colnames(tr) %>% 
  str_remove("%")

paths <- list.files("data/test")
te_features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  te_features[[i]] <- c(acc_mean = mean(df_seg$acoustic_data),
                        acc_sd = sd(df_seg$acoustic_data),
                        acc_median = median(df_seg$acoustic_data),
                        acc_kurt = kurtosis(df_seg$acoustic_data),
                        acc_skew = skewness(df_seg$acoustic_data),
                        acc_AD_q = c(quantile(abs(df_seg$acoustic_data - mean(df_seg$acoustic_data)),
                                              probs = c(.01, .05, .25, .75, .95, .99)))) %>% unlist()
  if(i %% 10 == 0)print(i)
}
te <- do.call("rbind", te_features) %>% 
  as_tibble()
colnames(te) <- colnames(te) %>% 
  str_remove("%")

basic_features <- bind_rows(tr, te)
basic_features %>% 
  write_csv("data/features/basic_features.csv")


