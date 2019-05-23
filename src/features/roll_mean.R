# 上がってから、下がるで1セット
library(tidyverse)
library(data.table)
library(RcppRoll)

tr_features <- list()
for(i in 1:21){
  df <- fread(str_c("data/processed/train_", i, ".csv"))
  for(j in 1:200){
    df_seg <- df[1:150000 + (j-1)*150000,]
    seg <- scale(df_seg$acoustic_data, scale = F)
    rollm10 <- roll_mean(abs(seg), n=10L)
    rollm100 <- roll_mean(abs(seg), n=100L)
    rollm1000 <- roll_mean(abs(seg), n=1000L)
    rollm10_oversd <- rollm10 > (median(rollm10) + sd(rollm10))
    rollm100_oversd <- rollm100 > (median(rollm100) + sd(rollm100))
    rollm1000_oversd <- rollm1000 > (median(rollm1000) + sd(rollm1000))
    rollm10rle3 <- rle(rollm10_oversd[,1])$lengths[rle(rollm10_oversd[,1])$values]
    rollm100rle3 <- rle(rollm100_oversd[,1])$lengths[rle(rollm100_oversd[,1])$values]
    rollm1000rle3 <- rle(rollm1000_oversd[,1])$lengths[rle(rollm1000_oversd[,1])$values]
    tr_features[[(i-1)*200+j]] <- c(roll_mean_100_mean = mean(rollm100, na.rm=T),
                                    roll_mean_10_rate_oversd = sum(rollm10_oversd, na.rm=T) / length(rollm10),
                                    roll_mean_10_sum_oversd = sum(rollm10[rollm10_oversd], na.rm=T) / length(rollm10),
                                    roll_mean_100_rate_oversd = sum(rollm100_oversd, na.rm=T) / length(rollm100),
                                    roll_mean_100_sum_oversd = sum(rollm100[rollm100_oversd], na.rm=T) / length(rollm100),
                                    roll_mean_1000_rate_oversd = sum(rollm1000_oversd, na.rm=T) / length(rollm1000),
                                    roll_mean_1000_sum_oversd = sum(rollm1000[rollm1000_oversd], na.rm=T) / length(rollm1000),
                                    roll_mean_10_rle3_mean = mean(rollm10rle3[rollm10rle3 > median(rollm10rle3)], na.rm=T),
                                    roll_mean_10_rle3_6sum = sum(head(sort(rollm10rle3, decreasing = T)), na.rm=T),
                                    roll_mean_100_rle3_mean = mean(rollm100rle3[rollm100rle3 > median(rollm100rle3)], na.rm=T),
                                    roll_mean_100_rle3_6sum = sum(head(sort(rollm100rle3, decreasing = T)), na.rm=T),
                                    roll_mean_1000_rle3_mean = mean(rollm1000rle3[rollm1000rle3 > median(rollm1000rle3)], na.rm=T),
                                    roll_mean_1000_rle3_6sum = sum(head(sort(rollm1000rle3, decreasing = T)), na.rm=T)
                                    )
  }
  print(i);gc()
}

tr <-  tr_features %>% 
  head(4194) %>% 
  do.call("rbind",.) %>% 
  as_tibble()

for(i in 1:ncol(tr)){
  tr[!is.finite(tr[,i][[1]]),i] <- 0
}

colnames(tr) <- colnames(tr) %>% 
  str_remove("%")

paths <- list.files("data/test")
te_features <- list()
for(i in 1:length(list.files("data/test"))){
  df_seg <- read_csv(str_c("data/test/", paths[i]), col_types = "d")
  seg <- scale(df_seg$acoustic_data, scale = F)
  rollm10 <- roll_mean(abs(seg), n=10L)
  rollm100 <- roll_mean(abs(seg), n=100L)
  rollm1000 <- roll_mean(abs(seg), n=1000L)
  rollm10_oversd <- rollm10 > (median(rollm10) + sd(rollm10))
  rollm100_oversd <- rollm100 > (median(rollm100) + sd(rollm100))
  rollm1000_oversd <- rollm1000 > (median(rollm1000) + sd(rollm1000))
  rollm10rle3 <- rle(rollm10_oversd[,1])$lengths[rle(rollm10_oversd[,1])$values]
  rollm100rle3 <- rle(rollm100_oversd[,1])$lengths[rle(rollm100_oversd[,1])$values]
  rollm1000rle3 <- rle(rollm1000_oversd[,1])$lengths[rle(rollm1000_oversd[,1])$values]
  te_features[[i]] <- c(roll_mean_100_mean = mean(rollm100, na.rm=T),
                        roll_mean_10_rate_oversd = sum(rollm10_oversd, na.rm=T) / length(rollm10),
                        roll_mean_10_sum_oversd = sum(rollm10[rollm10_oversd], na.rm=T) / length(rollm10),
                        roll_mean_100_rate_oversd = sum(rollm100_oversd, na.rm=T) / length(rollm100),
                        roll_mean_100_sum_oversd = sum(rollm100[rollm100_oversd], na.rm=T) / length(rollm100),
                        roll_mean_1000_rate_oversd = sum(rollm1000_oversd, na.rm=T) / length(rollm1000),
                        roll_mean_1000_sum_oversd = sum(rollm1000[rollm1000_oversd], na.rm=T) / length(rollm1000),
                        roll_mean_10_rle3_mean = mean(rollm10rle3[rollm10rle3 > median(rollm10rle3)], na.rm=T),
                        roll_mean_10_rle3_6sum = sum(head(sort(rollm10rle3, decreasing = T)), na.rm=T),
                        roll_mean_100_rle3_mean = mean(rollm100rle3[rollm100rle3 > median(rollm100rle3)], na.rm=T),
                        roll_mean_100_rle3_6sum = sum(head(sort(rollm100rle3, decreasing = T)), na.rm=T),
                        roll_mean_1000_rle3_mean = mean(rollm1000rle3[rollm1000rle3 > median(rollm1000rle3)], na.rm=T),
                        roll_mean_1000_rle3_6sum = sum(head(sort(rollm1000rle3, decreasing = T)), na.rm=T)
  )
  if(i %% 10 == 0)print(i)
}
te <- do.call("rbind", te_features) %>% 
  as_tibble()
colnames(te) <- colnames(te) %>% 
  str_remove("%")
for(i in 1:ncol(te)){
  te[!is.finite(te[,i][[1]]),i] <- 0
}

roll_mean <- bind_rows(tr, te)
roll_mean %>% 
  write_csv("data/features/roll_mean.csv")
