# acf peak
library(tidyverse)
library(pracma)
library(tseries)
library(cluster)

acf <- pacf <- sqacf <- sqpacf <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    acf[[(i-1)*200 + j]] <- acf(df_seg$acoustic_data, plot=F)$acf[,,1]
    pacf[[(i-1)*200 + j]] <- pacf(df_seg$acoustic_data, plot=F)$acf[,,1]
    sqacf[[(i-1)*200 + j]] <- acf(df_seg$acoustic_data^2, plot=F)$acf[,,1]
    sqpacf[[(i-1)*200 + j]] <- pacf(df_seg$acoustic_data^2, plot=F)$acf[,,1]
  }
  print(i);gc()
}

tr_acf <- acf %>% 
  lapply(function(x) x[-1]) %>% 
  do.call("rbind",.) %>% 
  as_tibble()
tr_pacf <- pacf %>% 
  do.call("rbind",.) %>% 
  as_tibble()
tr_sqacf <- sqacf %>% 
  lapply(function(x) x[-1]) %>% 
  do.call("rbind",.) %>% 
  as_tibble()
tr_sqpacf <- sqpacf %>% 
  do.call("rbind",.) %>% 
  as_tibble()

paths <- list.files("data/test")
acf <- pacf <- sqacf <- sqpacf <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  acf[[i]] <- acf(df_seg$acoustic_data, plot=F)$acf[,,1]
  pacf[[i]] <- pacf(df_seg$acoustic_data, plot=F)$acf[,,1]
  sqacf[[i]] <- acf(df_seg$acoustic_data^2, plot=F)$acf[,,1]
  sqpacf[[i]] <- pacf(df_seg$acoustic_data^2, plot=F)$acf[,,1]
  if(i %% 10 == 0)print(i)
}
te_acf <- acf %>% 
  lapply(function(x) x[-1]) %>% 
  do.call("rbind",.) %>% 
  as_tibble()
te_pacf <- pacf %>% 
  do.call("rbind",.) %>% 
  as_tibble()
te_sqacf <- sqacf %>% 
  lapply(function(x) x[-1]) %>% 
  do.call("rbind",.) %>% 
  as_tibble()
te_sqpacf <- sqpacf %>% 
  do.call("rbind",.) %>% 
  as_tibble()

bind_rows(tr_acf, te_acf) %>% 
  write_csv("data/processed/acf.csv")
bind_rows(tr_pacf, te_pacf) %>% 
  write_csv("data/processed/pacf.csv")
bind_rows(tr_sqacf, te_sqacf) %>% 
  write_csv("data/processed/sqacf.csv")
bind_rows(tr_sqpacf, te_sqpacf) %>% 
  write_csv("data/processed/sqpacf.csv")


# ex----

index <- c(0)
acf <- list()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000

  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]
    acf[[sum(index[1:i])+ j]] <- acf(df_seq$acoustic_data, plot = FALSE)$acf[-1,,1]
    print(sum(index[1:i])+ j);
  }
}
len <- length(acf)
paths <- list.files("data/test")
for(i in 1:length(list.files("data/test"))){
  df_seq <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  acf[[i + len]] <- acf(df_seq$acoustic_data, plot = FALSE)$acf[-1,,1]
  if(i %% 10 == 0) print(i)
}

acf <- do.call("rbind", acf)
acf %>%
  write_rds("data/processed/acf.RDS")
acf <- read_rds("data/processed/acf.RDS")
pacf <- read_rds("data/processed/pacf.RDS")
acf %>% t() %>% matplot(type = "l")
acf_df <- acf %>% 
  as_tibble() %>% 
  mutate(id = read_csv("data/processed/envelope.csv")$id) %>% 
  select(id, everything())
colnames(acf_df) <- c("id", str_c("acf_", 1:51))
pacf_df <- pacf %>% 
  as_tibble() %>% 
  mutate(id = read_csv("data/processed/envelope.csv")$id) %>% 
  select(id, everything())
colnames(pacf_df) <- c("id", str_c("pacf_", 1:50))
write_csv(acf_df, "data/processed/acf.csv")
write_csv(pacf_df, "data/processed/pacf.csv")
