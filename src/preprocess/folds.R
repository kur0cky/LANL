library(tidyverse)
library(data.table)

batch <- seq(0,660000000, length = 23)

start_ttf <- end_ttf <- list()

for(i in 1:length(batch)){
  df <- fread("data/train.csv",
              nrows = batch[i+1] - batch[i],
              skip = batch[i],
              # select = c("time_to_failure")
              col.names = c("acoustic_data", "time_to_failure"))[,2,]
  start_ttf[[i]] <- df[(0:199) * 150000 + 1]$time_to_failure
  end_ttf[[i]] <- df[1:200 * 150000]$time_to_failure
  gc()
  print(i)
}

df <- tibble(start_ttf = unlist(start_ttf),
             end_ttf = unlist(end_ttf)) %>% 
  drop_na() %>% 
  mutate(id = str_c("train_", str_pad(1:n(), width=4, side="left", pad=0)),
         straddle_flg = if_else(start_ttf < end_ttf, 1L, 0L),
         wave_index = cumsum(straddle_flg) + 1)  %>% 
  select(id, everything()) 

# folds_dfをcsvで共有いたします
# fold のindexは0はじまりにしてあります。
folds_df <- df %>% z
  arrange(end_ttf) %>% 
  mutate(fold_index = row_number(end_ttf) %% 10) %>% 
  select(id, target = end_ttf, fold_index, straddle_flg, wave_index) %>% 
  arrange(id)
# write_csv(folds_df, "data/processed/folds.csv")

# lgb.cvやxgb.cvのfoldsに渡す際は以下をやると各foldのindexを渡すリストになります。
folds_list <- folds_df %>% 
  select(id, fold_index) %>% 
  mutate(flg = TRUE) %>% 
  spread(fold_index, flg, fill=FALSE) %>% 
  select(-id) %>% 
  lapply(which)
# write_rds(folds_list, "data/processed/folds.rds")
rm(list = ls());gc()
