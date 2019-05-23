library(tidyverse)

envelope <- read_csv("data/processed/envelope.csv") %>% 
  drop_na()

pc_envelope <- prcomp(envelope[,-1])$x[,1:7] %>% 
  as_tibble() %>% 
  mutate(id = envelope$id) %>% 
  select(id, everything())
colnames(pc_envelope)<- c("id", str_c("envelope_PC", 1:7))

write_csv(pc_envelope, "data/features/pc_envelope.csv")
