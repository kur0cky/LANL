# rolling_sd PCA
library(tidyverse)
tr_roll <- read_csv("data/processed/tr_roll_sd.csv")
te_roll <- read_csv("data/processed/te_roll_sd.csv")

df <- bind_rows(tr_roll, te_roll)

pca <- df[,-1] %>% 
  apply(2, scale) %>% 
  prcomp()

roll_pc <- pca$x[,1:5] %>% 
  as_tibble()
roll_pc %>% 
  mutate(id = df$id) %>% 
  select(id,
         roll_sd_pc1 = PC1,
         roll_sd_pc2 = PC2,
         roll_sd_pc3 = PC3,
         roll_sd_pc4 = PC4,
         roll_sd_pc5 = PC5,
         ) %>% 
  write_csv("data/features/pc_roll_sd.csv")
