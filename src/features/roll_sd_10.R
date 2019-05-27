# rolling sd feature
library(tidyverse)
library(RcppRoll)
library(data.table)

roll_sd <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    roll_sd[[(i-1)*200 + j]] <- roll_sd(df_seg$acoustic_data,
                                        n = 10L) %>% 
      quantile( c(1:19 * 0.05) )
  }
  print(i);gc()
}

tr_roll_10_sd <- do.call("rbind", roll_sd) %>% 
  as.data.frame
colnames(tr_roll_10_sd) <- str_c("roll_10_sd_", colnames(tr_roll_10_sd)) %>% 
  str_remove("%")

tr_roll_10_sd %>% mutate(TTF = tr$TTF) %>% ggplot(aes(roll_10_sd_5, TTF))+geom_point()

paths <- list.files("data/test")
roll_sd <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  roll_sd[[i]] <- roll_sd(df_seg$acoustic_data,
                          n = 10L) %>% 
    quantile( c(1:19 * 0.05) )
  if(i %% 10 == 0)print(i)
}
te_roll_10_sd <- do.call("rbind", roll_sd) %>% 
  as.data.frame
colnames(te_roll_10_sd) <- str_c("roll_10_sd_", colnames(te_roll_10_sd)) %>% 
  str_remove("%")

bind_rows(tr_roll_10_sd, te_roll_10_sd) %>% 
  prcomp() %>% 
  .$x %>% 
  .[,1] %>% 
  tibble(roll_10_sd_PC1 = .) %>% 
  write_csv("data/features/roll_sd_10.csv")
