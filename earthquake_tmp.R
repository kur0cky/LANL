library(tidyverse)
library(data.table)
tr <- read_csv("data/train.csv", n_max = 5666574)
sample <- read_csv("data/sample_submission.csv")


train_vars  <- fread("data/train.csv", nrows = 0)
chunk_size  <- 10 ^ 8
quake_break <- vector("list")

for (i in c(1:7)){
  
  row_offset <- (i - 1) * chunk_size
  
  train <- fread("data/train.csv",
                 select = c(2),
                 skip = row_offset,
                 nrows = chunk_size,
                 col.names = names(train_vars)[2])
  
  quake_break[[i]] <- which(diff(train$time_to_failure) > 0) + (row_offset)
  
  rm(train)
}
quake_break
# 4096レコードごとに、time_to_failureがガクンと減る。ビンとビンの間に12micro秒のギャップが入るらしい。
# 観測は4MHz。
# testデータのcsvはすべて150000レコード。0.0375秒

# CVは2前後の人が多そう
# LBは1.5弱

# sdがバカでかいものはすぐ地震くる。
# min, max, min - mean, max - mean
# mean - median
# quantile features
# min - max
# max(abs(min, max))

# rolling features

# acf --> dtw

# rolling features
# ar features




# 特徴量管理

# 1. basic features
#   - mean, median, sd, skew, kurtosis, max, min, quantile
# 2. spectral
#   - spectrum envelope
#   - spectrum with ARmodel
# 3. AR model
#   - coef, resid_summary, fitted_value_summary
# 4. GARCH model
#   - coef, resid_summary, fitted_value_summary
# 5. auto correlation
#   - MDS scale after euclidean distance among ACFs

