# acf peak
library(tidyverse)
library(pracma)
library(tseries)


index <- c(0)
params <- list()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000
  
  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]
    acf <- acf(df_seq$acoustic_data, plot = FALSE)$acf[-1,,1]
    params[[sum(index[1:i]) + j]] <- findpeaks(acf)[1,]
    print(sum(index[1:i])+ j);
  }
}
tr_acf_peak <- params %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  rename(peak_acf = V1, 
         peak_acf_lag = V2,
         peak_acf_start_lag = V3,
         peak_acf_end_lag = V4) %>% 
  mutate(id = str_c("train_", 1:length(params))) %>% 
  select(id, everything())
  head()
write_csv(tr_acf_peak, "data/features/tr_acf_peak.csv")

paths <- list.files("data/test")
params <- list()
for(i in 1:length(list.files("data/test"))){
  df_seq <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  acf <- acf(df_seq$acoustic_data, plot = FALSE)$acf[-1,,1]
  params[[i]] <- findpeaks(acf)[1,]
  if(i %% 10 == 0) print(i)
}

te_acf_peak <- params %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  rename(peak_acf = V1, 
         peak_acf_lag = V2,
         peak_acf_start_lag = V3,
         peak_acf_end_lag = V4) %>% 
  mutate(id = str_remove(paths, ".csv")) %>% 
  select(id, everything())
write_csv(te_acf_peak, "data/features/te_acf_peak.csv")
