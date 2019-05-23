# spectrum envelope
# spectral density from AR fit
library(tidyverse)
library(data.table)

envelope <- envelope_res <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    spec <- log(Mod(fft(df_seg$acoustic_data))/150000)
    spec[!is.finite(spec)] <- median(spec)
    ceps <- ceps_res <- Mod(fft(spec)/150000)
    a <- 15
    ceps[a:(length(ceps)-a+1)] <- 0
    ceps_res[1:(a-1)] <- 0
    ceps_res[(length(ceps)-a+2):length(ceps)]<- 0
    env <- (Mod(fft(ceps, inverse=TRUE)))[1:75000]
    env_res <- (Mod(fft(ceps_res, inverse=TRUE)))[2:75000]
    envelope[[(i-1)*200 + j]] <- env[seq(0,75000, length=101)]
    envelope_res[[(i-1)*200 + j]] <- c(env_res_mean = mean(na.omit(env_res)),
                                       env_res_median = median(na.omit(env_res)),
                                       env_res_sd = sd(na.omit(env_res)),
                                       env_res_sd_ratio = sd(na.omit(env_res))/sd(env),
                                       env_res_q=quantile(na.omit(env_res, probs=c(.05,.25,.75,.95))))
  }
  print(i);gc()
}

tr_envelope <- do.call("rbind", envelope) %>% 
  as.data.frame
colnames(tr_envelope) <- str_c("envelope_", 1:100)

tr_envelope_res <- do.call("rbind", envelope_res) %>% 
  as_tibble()
colnames(tr_envelope_res) <- str_remove(colnames(tr_envelope_res), "%")

paths <- list.files("data/test")
envelope <- envelope_res <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  spec <- log(Mod(fft(df_seg$acoustic_data))/150000)
  spec[!is.finite(spec)] <- median(spec)
  ceps <- ceps_res <- Mod(fft(spec)/150000)
  a <- 15
  ceps[a:(length(ceps)-a+1)] <- 0
  ceps_res[1:(a-1)] <- 0
  ceps_res[(length(ceps)-a+2):length(ceps)]<- 0
  env <- (Mod(fft(ceps, inverse=TRUE)))[1:75000]
  env_res <- (Mod(fft(ceps_res, inverse=TRUE)))[2:75000]
  envelope[[(i-1)*200 + j]] <- env[seq(0,75000, length=101)]
  envelope_res[[(i-1)*200 + j]] <- c(env_res_mean = mean(na.omit(env_res)),
                                     env_res_median = median(na.omit(env_res)),
                                     env_res_sd = sd(na.omit(env_res)),
                                     env_res_sd_ratio = sd(na.omit(env_res))/sd(env),
                                     env_res_q=quantile(na.omit(env_res, probs=c(.05,.25,.75,.95))))
  if(i %% 10 == 0)print(i)
}
te_envelope <- do.call("rbind", envelope) %>% 
  as.data.frame()
colnames(te_envelope) <- str_c("envelope_", 1:100)

te_envelope_res <- do.call("rbind", envelope_res) %>% 
  as_tibble()
colnames(te_envelope_res) <- str_remove(colnames(te_envelope_res), "%")

bind_rows(tr_envelope, te_envelope) %>% 
  write_csv("data/processed/envelope.csv")

bind_rows(tr_envelope_res, te_envelope_res) %>% 
  write_csv("data/features/envelope_res.csv")

# ex----
index <- c(0)
envelope <- list()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000
  
  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]
    spec <- log(Mod(fft(df_seq$acoustic_data))/150000)
    ceps <- Mod(fft(spec)/150000)
    a <- 15
    ceps[a:(length(ceps)-a+1)] <- 0
    env <- (Mod(fft(ceps, inverse=TRUE)))[1:75000]
    envelope[[sum(index[1:i])+ j]] <- env[seq(0,75000, length=101)]
    print(sum(index[1:i])+ j)
  }
}

tr_envelope <- do.call("rbind", envelope) %>% 
  as.data.frame %>% 
  mutate(id = str_c("train_", 1:length(envelope))) %>% 
  select(id, everything()) %>% 
  left_join(read_csv("data/processed/tr.csv") %>% 
              select(id, target), by = "id")
colnames(tr_envelope) <- c("id", str_c("envelope_", 1:100))

# write_csv(tr_envelope, "data/features/tr_envelope.csv")


envelope <- list()
paths <- list.files("data/test")
for(i in 1:length(list.files("data/test"))){
  seq_df <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  spec <- log(Mod(fft(seq_df$acoustic_data))/150000)
  ceps <- Mod(fft(spec)/150000)
  a <- 15
  ceps[a:(length(ceps)-a+1)] <- 0
  env <- (Mod(fft(ceps, inverse=TRUE)))[1:75000]
  envelope[[i]] <- env[seq(0,75000, length=101)]
  if(i %% 10 == 0)print(i)
}

te_envelope <- do.call("rbind", envelope) %>% 
  as.data.frame() %>% 
  mutate(id = str_remove(paths, ".csv")) %>% 
  select(id, everything())
colnames(te_envelope) <- c("id", str_c("envelope_", 1:100))
# write_csv(te_envelope, "data/features/te_envelope.csv")

bind_rows(tr_envelope, te_envelope) %>% 
  write_csv("data/processed/envelope.csv")
