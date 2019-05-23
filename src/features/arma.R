# arma fit
library(tidyverse)
library(data.table)

tr_features <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    fit_arma <- tseries::arma(df_seg$acoustic_data, c(2,2))
    tr_features[[(i-1)*200+j]] <- c(fit_arma$coef,
                                    arma_res_sd = sd(na.omit(fit_arma$residuals)),
                                    arma_fit_sd = sd(na.omit(fit_arma$fitted.values)),
                                    arma_css = fit_arma$css,
                                    arma_aic = summary(fit_arma)$aic)
  }
  print(i);gc()
}

tr <- do.call("rbind", tr_features) %>% 
  as_tibble()

paths <- list.files("data/test")
te_features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  fit_arma <- tseries::arma(df_seg$acoustic_data, c(2,2))
  te_features[[(i-1)*200+j]] <- c(fit_arma$coef,
                                  arma_res_sd = sd(na.omit(fit_arma$residuals)),
                                  arma_fit_sd = sd(na.omit(fit_arma$fitted.values)),
                                  arma_css = fit_arma$css,
                                  arma_aic = summary(fit_arma)$aic)
  if(i %% 10 == 0)print(i)
}
te <- do.call("rbind", te_features) %>% 
  as_tibble()

arma_features <- bind_rows(tr, te) %>% 
  mutate(arma_sd_fit_res = arma_res_sd / arma_fit_sd)
arma_features %>% 
  write_csv("data/features/arma_features.csv")

# ex----
index <- c(0)
arma_params <- list()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000
  
  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]
    fit_arma <- tseries::arma(df_seq$acoustic_data, c(2,2))
    arma_params[[sum(index[1:i])+j]] <- c(fit_arma$coef[1:4], 
                                           sd_arma_fit = sd(na.omit(fit_arma$fitted.values)),
                                           sd_arma_res = sd(na.omit(fit_arma$residuals)),
                                           p_arma_res = quantile(na.omit(fit_arma$residuals), c(.05,.95)),
                                           mean_arma_res = mean(na.omit(fit_arma$residuals)),
                                           p_arma_fit = quantile(na.omit(fit_arma$fitted.values), c(.05, .95)),
                                           mean_arma_fit = mean(na.omit(fit_arma$fitted.values))
    )
    print(sum(index[1:i])+ j);gc()
  }
}
tr_arma <- arma_params %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_c("train_", 1:length(arma_params))) %>% 
  select(id, everything())
write_csv(tr_arma, "data/features/tr_arma.csv")

paths <- list.files("data/test")
arma_params <- list()
for(i in 1600:length(list.files("data/test"))){
  df_seq <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  fit_arma <- tseries::arma(df_seq$acoustic_data, c(2,2))
  arma_params[[i]] <- c(fit_arma$coef, 
                         sd_arma_fit = sd(na.omit(fit_arma$fitted.values)),
                         sd_arma_res = sd(na.omit(fit_arma$residuals)),
                         p_arma_res = quantile(na.omit(fit_arma$residuals), c(.05,.95)),
                         mean_arma_res = mean(na.omit(fit_arma$residuals)),
                         p_arma_fit = quantile(na.omit(fit_arma$fitted.values), c(.05, .95)),
                         mean_arma_fit = mean(na.omit(fit_arma$fitted.values))
  )
  if(i %% 10 == 0)print(i);gc()
}

te_arma <- arma_params %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_remove(paths, ".csv")) %>% 
  select(id, everything())
write_csv(te_arma, "data/features/te_arma.csv")
