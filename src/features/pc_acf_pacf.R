library(tidyverse)

acf <- read_csv("data/processed/acf.csv")
pacf <- read_csv("data/processed/pacf.csv")
sqacf <- read_csv("data/processed/sqacf.csv")
sqpacf <- read_csv("data/processed/sqpacf.csv")
envelope <- read_csv("data/processed/envelope.csv")
roll_sd <- read_csv("data/processed/roll_sd.csv")


pc_acf <- prcomp(acf[,-1])$x[,1:10] %>% 
  as_tibble()
colnames(pc_acf)<- c(str_c("acf_PC", 1:10))
pc_pacf <- prcomp(pacf[,-1])$x[,1:10] %>% 
  as_tibble()
colnames(pc_pacf)<- c(str_c("pacf_PC", 1:10))
pc_sqacf <- prcomp(sqacf[,-1])$x[,1:10] %>% 
  as_tibble()
colnames(pc_sqacf)<- c(str_c("sqacf_PC", 1:10))
pc_sqpacf <- prcomp(sqpacf[,-1])$x[,1:10] %>% 
  as_tibble()
colnames(pc_sqpacf)<- c(str_c("sqpacf_PC", 1:10))

pc_envelope <- envelope %>% 
  apply(2, scale) %>% 
  prcomp() %>% 
  .$x %>% 
  .[,1:10] %>% 
  as_tibble()
colnames(pc_envelope)<- c(str_c("envelope_PC", 1:10))

pc_roll_sd <- roll_sd %>% 
  apply(2, scale) %>% 
  prcomp() %>% 
  .$x %>% 
  .[,1:10] %>% 
  as_tibble()
colnames(pc_roll_sd)<- c(str_c("roll_sd_PC", 1:10))

write_csv(pc_acf, "data/features/pc_acf.csv")
write_csv(pc_pacf, "data/features/pc_pacf.csv")
write_csv(pc_sqacf, "data/features/pc_sqacf.csv")
write_csv(pc_sqpacf, "data/features/pc_sqpacf.csv")
write_csv(pc_envelope, "data/features/pc_envelope.csv")
write_csv(pc_roll_sd, "data/features/pc_roll_sd.csv")
