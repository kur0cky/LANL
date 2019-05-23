# feature selection ks-test

# new----

tr_te <- read_csv("data/features/features.csv")

tr <- tr_te %>% 
  drop_na() %>% 
  select(-TTF)
te <- tr_te %>% 
  filter(is.na(TTF)) %>% 
  select(-TTF)

pvalue <- c()
feature <- c()
for(i in 1:ncol(tr)) {
  pvalue[i] <- ks.test(tr[,i][[1]], te[,i][[1]], exact=T)$p.value
  feature[i] <- colnames(tr[,i])
}
kstest <- tibble(feature ,
                 pvalue)
kstest %>% 
  arrange(pvalue) %>% 
  filter(pvalue > 0.1) %>% 
  .$feature %>% 
  write_rds("data/features/features_kstest.RDS")
