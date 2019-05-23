library(tidyverse)
library(RcppRoll)
library(data.table)

roll_sd <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    roll_sd[[(i-1)*200 + j]] <- roll_sd(df_seg$acoustic_data,
                                        n = 100L) %>% 
      quantile( 0.05* c(1:19))
  }
  print(i);gc()
}

tr_roll_sd <- do.call("rbind", roll_sd) %>% 
  as.data.frame
colnames(tr_roll_sd) <- str_c("roll_100_sd_", colnames(tr_roll_sd)) %>% 
  str_remove("%")

paths <- list.files("data/test")
roll_sd <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  roll_sd[[i]] <- roll_sd(df_seg$acoustic_data,
                                      n = 100L) %>% 
    quantile( 0.05* c(1:19))
  if(i %% 10 == 0)print(i)
}
te_roll_sd <- do.call("rbind", roll_sd) %>% 
  as.data.frame()
colnames(te_roll_sd) <- str_c("roll_100_sd_", colnames(te_roll_sd)) %>% 
  str_remove("%")

bind_rows(tr_roll_sd, te_roll_sd) %>% 
  write_csv("data/processed/roll_sd.csv")


#ex----
index <- c(0)
tr <- tibble()
roll_sd <- list()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000
  
  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]
    roll_sd[[sum(index[1:i])+ j]] <- roll_sd(df_seq$acoustic_data,
                                             n = 1000L) %>% 
      quantile( 0.05* c(1:19))
    print(sum(index[1:i])+ j)
  }
}

tr_roll_sd <- roll_sd %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_c("train_", 1:length(roll_sd))) %>% 
  select(id, everything())
colnames(tr_roll_sd) <- c("id",
                          str_c("roll_1000_sd_p",1:19*5))
write_csv(tr_roll_sd, "data/processed/tr_roll_sd.csv")

paths <- list.files("data/test")
roll_sd <- list()
for(i in 1018:length(list.files("data/test"))){
  df_seq <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  roll_sd[[i]] <- roll_sd(df_seq$acoustic_data,
                                           n = 1000L) %>% 
    quantile( 0.05* c(1:19))
  if(i %% 10 == 0)print(i)
}

te_roll_sd <- roll_sd %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_remove(paths, ".csv")) %>% 
  select(id, everything())
colnames(te_roll_sd) <- c("id",
                          str_c("roll_1000_sd_p",1:19*5))
write_csv(te_roll_sd, "data/processed/te_roll_sd.csv")
