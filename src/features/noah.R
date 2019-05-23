# thanks to Noah
# https://www.kaggle.com/zikazika/useful-new-features-and-a-optimised-model

library(tidyverse)
library(data.table)
library(seismicRoll)

tr_features <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    tr_features[[(i-1)*200+j]] <- c(sta_lta1 = mean(roll_stalta(df_seg$acoustic_data, n_sta = 500, 
                                                                n_lta = 10000,
                                                                increment = 100), na.rm=T),
                                    sta_lta2 = mean(roll_stalta(df_seg$acoustic_data, n_sta = 5000, 
                                                                n_lta = 100000,
                                                                increment = 1000), na.rm=T),
                                    sta_lta3 = mean(roll_stalta(df_seg$acoustic_data, n_sta = 3333, 
                                                                n_lta = 6666,
                                                                increment = 666), na.rm=T),
                                    sta_lta4 = mean(roll_stalta(df_seg$acoustic_data, n_sta = 10000,
                                                                n_lta = 25000,
                                                                increment = 2000), na.rm=T),
                                    max_to_min = max(df_seg$acoustic_data) / abs(min(df_seg$acoustic_data)),
                                    max_to_min_diff = max(df_seg$acoustic_data) - abs(min(df_seg$acoustic_data)))
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
  te_features[[i]] <- c(sta_lta1 = mean(roll_stalta(df_seg$acoustic_data, n_sta = 500, 
                                                    n_lta = 10000,
                                                    increment = 100), na.rm=T),
                        sta_lta2 = mean(roll_stalta(df_seg$acoustic_data, n_sta = 5000, 
                                                    n_lta = 100000,
                                                    increment = 1000), na.rm=T),
                        sta_lta3 = mean(roll_stalta(df_seg$acoustic_data, n_sta = 3333, 
                                                    n_lta = 6666,
                                                    increment = 666), na.rm=T),
                        sta_lta4 = mean(roll_stalta(df_seg$acoustic_data, n_sta = 10000,
                                                    n_lta = 25000,
                                                    increment = 2000), na.rm=T),
                        max_to_min = max(df_seg$acoustic_data) / abs(min(df_seg$acoustic_data)),
                        max_to_min_diff = max(df_seg$acoustic_data) - abs(min(df_seg$acoustic_data)))
  if(i %% 10 == 0)print(i)
}
te <- do.call("rbind", te_features) %>% 
  as_tibble()
colnames(te) <- colnames(te) %>% 
  str_remove("%")

noah <- bind_rows(tr, te)
noah %>% 
  write_csv("data/features/noah.csv")

# ex----

index <- c(0)
tr <- tibble()
features <- list()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000
  
  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]$acoustic_data
    features[[sum(index[1:i])+ j]] <- c(sta_lta1 = mean(roll_stalta(df_seq, n_sta = 500, 
                                                                    n_lta = 10000,
                                                                    increment = 250), na.rm=T),
                                        sta_lta2 = mean(roll_stalta(df_seq, n_sta = 5000, 
                                                                    n_lta = 100000,
                                                                    increment = 2500), na.rm=T),
                                        sta_lta3 = mean(roll_stalta(df_seq, n_sta = 3333, 
                                                                    n_lta = 6666,
                                                                    increment = 1666), na.rm=T),
                                        sta_lta4 = mean(roll_stalta(df_seq, n_sta = 10000,
                                                                    n_lta = 25000,
                                                                    increment = 5000), na.rm=T),
                                        max_to_min = max(df_seq) / abs(min(df_seq)),
                                        max_to_min_diff = max(df_seq) - abs(min(df_seq)))
    print(sum(index[1:i])+ j)
  }
}
tr_noah <- features %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_c("train_", 1:length(features))) %>% 
  select(id, everything())
write_csv(tr_noah, "data/features/tr_noah.csv")

paths <- list.files("data/test")
features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seq <- read_csv(str_c("data/test/", paths[i]), col_types = "d")$acoustic_data
  features[[i]] <- c(sta_lta1 = mean(roll_stalta(df_seq, n_sta = 500, 
                                                 n_lta = 10000,
                                                 increment = 250), na.rm=T),
                     sta_lta2 = mean(roll_stalta(df_seq, n_sta = 5000, 
                                                 n_lta = 100000,
                                                 increment = 2500), na.rm=T),
                     sta_lta3 = mean(roll_stalta(df_seq, n_sta = 3333, 
                                                 n_lta = 6666,
                                                 increment = 1666), na.rm=T),
                     sta_lta4 = mean(roll_stalta(df_seq, n_sta = 10000,
                                                 n_lta = 25000,
                                                 increment = 5000), na.rm=T),
                     max_to_min = max(df_seq) / abs(min(df_seq)),
                     max_to_min_diff = max(df_seq) - abs(min(df_seq)))
  if(i %% 10 == 0)print(i)
}

te_noah <- features %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_remove(paths, ".csv")) %>% 
  select(id, everything())
write_csv(te_noah, "data/features/te_noah.csv")
