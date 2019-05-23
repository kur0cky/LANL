# garch fit
library(tidyverse)
library(data.table)

tr_features <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    fit_garch <- tseries::garch(df_seg$acoustic_data,
                                trace = FALSE)
    tr_features[[(i-1)*200+j]] <- c(fit_garch$coef, 
                                    garch_fit_sd = sd(na.omit(fit_garch$fitted.values)),
                                    garch_res_sd = sd(na.omit(fit_garch$residuals)),
                                    garch_likeli = fit_garch$n.likeli,
                                    garch_lb = summary(fit_garch)$l.b.test$statistic,
                                    garch_jb = summary(fit_garch)$j.b.test$statistic)
  }
  print(i);gc()
}

tr <- do.call("rbind", tr_features) %>% 
  as_tibble() %>% 
  drop_na()

paths <- list.files("data/test")
te_features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  fit_garch <- tseries::garch(df_seg$acoustic_data,
                              trace = FALSE)
  te_features[[(i-1)*200+j]] <- c(fit_garch$coef, 
                                  garch_fit_sd = sd(na.omit(fit_garch$fitted.values)),
                                  garch_res_sd = sd(na.omit(fit_garch$residuals)),
                                  garch_likeli = fit_garch$n.likeli,
                                  garch_lb = summary(fit_garch)$l.b.test$statistic,
                                  garch_jb = summary(fit_garch)$j.b.test$statistic)
  if(i %% 10 == 0)print(i)
}
te <- do.call("rbind", te_features) %>% 
  as_tibble()
colnames(te) <- colnames(te) %>% 
  str_remove(".X-squared")

garch_features <- bind_rows(tr, te) %>% 
  rename(garch_res_sd = sd_garch_res, garch_fit_sd = sd_garch_fit) %>% 
  mutate(garch_sd_fit_res = garch_res_sd / garch_fit_sd)
garch_features %>% 
  write_csv("data/features/garch_features.csv")


# ex----

index <- c(0)
garch_params <- list()
for(i in 1:17){
  df <- fread(str_c("data/processed/quake_", i, ".csv"))
  index[i+1] <- nrow(df) %/% 150000
  
  for(j in 1:(nrow(df) %/% 150000)){
    df_seq <- df[((j-1)*150000 + 1):(j*150000),,]
    fit_garch <- tseries::garch(df_seq$acoustic_data,
                                trace = FALSE)
    garch_params[[sum(index[1:i])+j]] <- c(fit_garch$coef, 
                                           sd_garch_fit = sd(na.omit(fit_garch$fitted.values)),
                                           sd_garch_res = sd(na.omit(fit_garch$residuals)),
                                           p_garch_res = quantile(na.omit(fit_garch$residuals), c(.05,.95)),
                                           mean_garch_res = mean(na.omit(fit_garch$residuals)),
                                           p_garch_fit = quantile(na.omit(fit_garch$fitted.values[,1]), c(.05, .95)),
                                           mean_garch_fit = mean(na.omit(fit_garch$fitted.values[,1]))
                                           )
    print(sum(index[1:i])+ j);gc()
  }
}
tr_garch <- garch_params %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_c("train_", 1:length(garch_params))) %>% 
  select(id, everything())
write_csv(tr_garch, "data/features/tr_garch.csv")

paths <- list.files("data/test")
garch_params <- list()
for(i in 1:length(list.files("data/test"))){
  df_seq <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  fit_garch <- tseries::garch(df_seq$acoustic_data,
                              trace = FALSE)
  garch_params[[i]] <- c(fit_garch$coef, 
                         sd_garch_fit = sd(na.omit(fit_garch$fitted.values)),
                         sd_garch_res = sd(na.omit(fit_garch$residuals)),
                         p_garch_res = quantile(na.omit(fit_garch$residuals), c(.05,.95)),
                         mean_garch_res = mean(na.omit(fit_garch$residuals)),
                         p_garch_fit = quantile(na.omit(fit_garch$fitted.values[,1]), c(.05, .95)),
                         mean_garch_fit = mean(na.omit(fit_garch$fitted.values[,1]))
  )
  if(i %% 10 == 0)print(i);gc()
}

te_garch <- garch_params %>% 
  do.call("rbind",.) %>% 
  as.data.frame() %>% 
  mutate(id = str_remove(paths, ".csv")) %>% 
  select(id, everything())
write_csv(te_garch, "data/features/te_garch.csv")
