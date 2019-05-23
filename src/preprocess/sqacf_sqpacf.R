# squared acf
library(tidyverse)
library(data.table)

index <- c(0)
acf <- list()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000

  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]
    acf[[sum(index[1:i])+ j]] <- acf(df_seq$acoustic_data^2, plot = FALSE)$acf[-1,,1]
    print(sum(index[1:i])+ j);
  }
}
len <- length(acf)
paths <- list.files("data/test")
for(i in 1:length(list.files("data/test"))){
  df_seq <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  acf[[i + len]] <- acf(df_seq$acoustic_data^2, plot = FALSE)$acf[-1,,1]
  if(i %% 10 == 0) print(i)
}

acf <- do.call("rbind", acf)
acf %>% t() %>% matplot(type = "l")
acf_df <- acf %>% 
  as_tibble() %>% 
  mutate(id = read_csv("data/processed/envelope.csv")$id) %>% 
  select(id, everything())
colnames(acf_df) <- c("id", str_c("sqacf_", 1:51))
write_csv(acf_df, "data/processed/sqacf.csv")


index <- c(0)
pacf <- list()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000
  
  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]
    pacf[[sum(index[1:i])+ j]] <- pacf(df_seq$acoustic_data^2, plot = FALSE)$acf[,,1]
    print(sum(index[1:i])+ j);
  }
}
gc()
len <- length(pacf)
paths <- list.files("data/test")
for(i in 1:length(list.files("data/test"))){
  df_seq <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  pacf[[i + len]] <- pacf(df_seq$acoustic_data^2, plot = FALSE)$acf[,,1]
  if(i %% 10 == 0) print(i)
}

pacf <- do.call("rbind", pacf)
pacf %>% t() %>% matplot(type = "l")
pacf_df <- pacf %>% 
  as_tibble() %>% 
  mutate(id = read_csv("data/processed/envelope.csv")$id) %>% 
  select(id, everything())
colnames(pacf_df) <- c("id", str_c("sqpacf_", 1:51))
write_csv(pacf_df, "data/processed/sqpacf.csv")
