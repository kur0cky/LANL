library(tidyverse)
library(data.table)

batch <- seq(0,660000000, length = 23)

for(i in 1:length(batch)){
  df <- fread("data/train.csv",
              nrows = batch[i+1] - batch[i],
              skip = batch[i],
              # select = c("time_to_failure")
              col.names = c("acoustic_data", "time_to_failure"))
  df %>% 
    fwrite(str_c("data/processed/train_", i, ".csv"))
  print(i);gc()
}

for(i in 1:length(quakebreaks)){
  df <- fread("data/train.csv",
              nrows = quakebreaks[i+1] - quakebreaks[i],
              skip = quakebreaks[i] - 1,
              col.names = c("acoustic_data", "time_to_failure"))
  df %>% 
    fwrite(str_c("data/processed/quake_", i, ".csv"))
  gc()
}

rm(df);gc()