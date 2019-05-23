library(tidyverse)

land_mean <- read_csv("data/processed/land_mean.csv")
land_q10 <- read_csv("data/processed/land_q10.csv")
land_q90 <- read_csv("data/processed/land_q90.csv")
land_q50 <- read_csv("data/processed/land_q50.csv")


pc_land_mean <- prcomp(land_mean[,-1])$x[,1:10] %>% 
  as_tibble()
colnames(pc_land_mean)<- c(str_c("land_mean_PC", 1:10))
pc_land_q10 <- prcomp(land_q10[,-1])$x[,1:10] %>% 
  as_tibble()
colnames(pc_land_q10)<- c(str_c("land_q10_PC", 1:10))
pc_land_q90 <- prcomp(land_q90[,-1])$x[,1:10] %>% 
  as_tibble()
colnames(pc_land_q90)<- c(str_c("land_q90_PC", 1:10))
pc_land_q50 <- prcomp(land_q50[,-1])$x[,1:10] %>% 
  as_tibble()
colnames(pc_land_q50)<- c(str_c("land_q50_PC", 1:10))


write_csv(pc_land_mean, "data/features/pc_land_mean.csv")
write_csv(pc_land_q10, "data/features/pc_land_q10.csv")
write_csv(pc_land_q90, "data/features/pc_land_q90.csv")
write_csv(pc_land_q50, "data/features/pc_land_q50.csv")
