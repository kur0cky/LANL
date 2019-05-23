library(tidyverse)
library(seewave)
library(data.table)

tr_features <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    tr_features[[(i-1)*200+j]] <- specprop(seewave::spec(df_seg$acoustic_data, 
                                                         f = 4000000, 
                                                         plot =F)) %>% 
      unlist()
  }
  print(i);gc()
}

tr <- do.call("rbind", tr_features) %>% 
  as_tibble() %>% 
  drop_na()
colnames(tr) <- str_c("spec_", colnames(tr))

paths <- list.files("data/test")
te_features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  te_features[[i]] <- specprop(seewave::spec(df_seg$acoustic_data, 
                                             f = 4000000, 
                                             plot =F)) %>% unlist()
  if(i %% 10 == 0)print(i)
}
te <- do.call("rbind", te_features) %>% 
  as_tibble()
colnames(te) <- str_c("spec_", colnames(te))

spec_basics <- bind_rows(tr, te)
spec_basics %>% 
  write_csv("data/features/spec_basics.csv")


# ex----
index <- c(0)
tr <- tibble()
spectrum_features <- list()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000
  
  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]
    spectrum_features[[sum(index[1:i])+ j]] <- specprop(seewave::spec(df_seq$acoustic_data, 
                                                                      f = 4000000, 
                                                                      plot =F)) %>% 
      unlist()
    print(sum(index[1:i])+ j)
  }
}

tr_spec <- spectrum_features %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_c("train_", 1:length(roll_sd))) %>% 
  select(id, everything(), -prec.x , -cent, )
write_csv(tr_spec, "data/features/tr_spec.csv")

paths <- list.files("data/test")
spectrum_features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seq <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  spectrum_features[[i]] <- specprop(seewave::spec(df_seq$acoustic_data, 
                                                   f = 4000000, 
                                                   plot =F)) %>% 
    unlist()
  if(i %% 10 == 0)print(i)
}

te_spec <- spectrum_features %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_remove(paths, ".csv")) %>% 
  select(id, everything(), -prec.x , -cent, )
write_csv(te_spec, "data/features/te_spec.csv")
