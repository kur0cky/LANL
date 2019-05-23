# envelope residuals
library(tidyverse)
library(data.table)

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
    ceps[1:(a)] <- 0
    ceps[(length(ceps)-a+1):length(ceps)]<- 0
    env <- (Mod(fft(ceps, inverse=TRUE)))[2:75000]
    envelope[[sum(index[1:i])+ j]] <- c(env_res_mean = mean(na.omit(env)),
                                        env_res_median = median(na.omit(env)),
                                        env_res_sd = sd(na.omit(env)),
                                        env_res_q=quantile(na.omit(env, probs=c(.05,.25,.75,.95)))
                                        )
    print(sum(index[1:i])+ j)
  }
}

tr_envelope <- do.call("rbind", envelope) %>% 
  as.data.frame %>% 
  mutate(id = str_c("train_", 1:length(envelope))) %>% 
  select(id, everything()) 
# write_csv(tr_envelope, "data/features/tr_envelope.csv")


envelope <- list()
paths <- list.files("data/test")
for(i in 1:length(list.files("data/test"))){
  seq_df <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  spec <- log(Mod(fft(seq_df$acoustic_data))/150000)
  ceps <- Mod(fft(spec)/150000)
  a <- 15
  ceps[1:(a)] <- 0
  ceps[(length(ceps)-a+1):length(ceps)]<- 0
  env <- (Mod(fft(ceps, inverse=TRUE)))[2:75000]
  envelope[[i]] <- c(env_res_mean = mean(na.omit(env)),
                     env_res_median = median(na.omit(env)),
                     env_res_sd = sd(na.omit(env)),
                     env_res_q=quantile(na.omit(env, probs=c(.05,.25,.75,.95)))
  )
  if(i %% 10 == 0)print(i)
}

te_envelope <- do.call("rbind", envelope) %>% 
  as.data.frame() %>% 
  mutate(id = str_remove(paths, ".csv")) %>% 
  select(id, everything())
# write_csv(te_envelope, "data/features/te_envelope.csv")

bind_rows(tr_envelope, te_envelope) %>% 
  write_csv("data/features/envelope_res.csv")
