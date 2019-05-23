library(tidyverse)

# new----
folds <- read_csv("data/processed/folds.csv")
basic_features <- read_csv("data/features/basic_features.csv")
hilbert_basics <- read_csv("data/features/hilbert_basics.csv")
spec_basics <- read_csv("data/features/spec_basics.csv")
noah <- read_csv("data/features/noah.csv")
arma_features <- read_csv("data/features/arma_features.csv")
garch_features <- read_csv("data/features/garch_features.csv")
envelope_res <- read_csv("data/features/envelope_res.csv")

pc_acf <- read_csv("data/features/pc_acf.csv")
pc_pacf <- read_csv("data/features/pc_pacf.csv")
pc_sqacf <- read_csv("data/features/pc_sqacf.csv")
pc_sqpacf <- read_csv("data/features/pc_sqpacf.csv")
pc_roll_sd <- read_csv("data/features/pc_roll_sd.csv")
pc_envelope <- read_csv("data/features/pc_envelope.csv")

roll_mean <- read_csv("data/features/roll_mean.csv")

landscape <- read_csv("data/features/landscape.csv")
pc_land_mean <- read_csv("data/features/pc_land_mean.csv")
pc_land_q10 <- read_csv("data/features/pc_land_q10.csv")
pc_land_q90 <- read_csv("data/features/pc_land_q90.csv")
pc_land_q50 <- read_csv("data/features/pc_land_q50.csv")


df <- basic_features %>%
  bind_cols(hilbert_basics) %>%
  bind_cols(spec_basics) %>%
  bind_cols(noah) %>%
  bind_cols(arma_features) %>%
  bind_cols(garch_features) %>%
  bind_cols(envelope_res) %>%
  bind_cols(pc_acf) %>%
  bind_cols(pc_pacf) %>%
  bind_cols(pc_sqacf) %>%
  bind_cols(pc_sqpacf) %>%
  bind_cols(pc_roll_sd) %>%
  bind_cols(pc_envelope) %>%
  bind_cols(roll_mean) %>%
  bind_cols(landscape) %>% 
  bind_cols(pc_land_mean) %>% 
  bind_cols(pc_land_q10) %>% 
  bind_cols(pc_land_q50) %>% 
  bind_cols(pc_land_q90)
df %>% 
  write_csv("data/features/features.csv")


# ex----
# 
# tr <- read_csv("data/processed/tr.csv") 
# te <- read_csv("data/processed/te.csv")
# 
# tr_spec <- read_csv("data/features/tr_spec.csv")
# te_spec <- read_csv("data/features/te_spec.csv")
# 
# tr_spec_ar <- read_csv("data/features/tr_spec_ar.csv", 
#                        col_types = "cdd")
# te_spec_ar <- read_csv("data/features/te_spec_ar.csv", 
#                        col_types = "cdd")
# 
# tr_garch <- read_csv("data/features/tr_garch.csv")
# te_garch <- read_csv("data/features/te_garch.csv")
# 
# tr_arma <- read_csv("data/features/tr_arma.csv")
# te_arma <- read_csv("data/features/te_arma.csv")
# 
# tr_hilbert_basic <- read_csv("data/features/tr_hilbert_basic.csv")
# te_hilbert_basic <- read_csv("data/features/te_hilbert_basic.csv")
# 
# tr_acf_peak <- read_csv("data/features/tr_acf_peak.csv")
# te_acf_peak <- read_csv("data/features/te_acf_peak.csv")
# 
# # tr_roll_sd <- read_csv("data/features/tr_roll_sd.csv")
# # te_roll_sd <- read_csv("data/features/te_roll_sd.csv")
# 
# pc_acf <- read_csv("data/features/pc_acf.csv")
# pc_pacf <- read_csv("data/features/pc_pacf.csv")
# pc_sqacf <- read_csv("data/features/pc_sqacf.csv")
# pc_sqpacf <- read_csv("data/features/pc_sqpacf.csv")
# pc_envelope <- read_csv("data/features/pc_envelope.csv")
# pc_roll_sd <- read_csv("data/features/pc_roll_sd.csv")
# 
# tr_noah <- read_csv("data/features/tr_noah.csv")
# te_noah <- read_csv("data/features/te_noah.csv")
# 
# envelope_res <- read_csv("data/features/envelope_res.csv")
# 
# df <- tr %>% 
#   bind_rows(te) %>% 
#   select(id, target, quake_index, 
#          everything()
#          ) %>% 
#   left_join(bind_rows(tr_spec, te_spec), by = "id") %>%
#   left_join(bind_rows(tr_spec_ar, te_spec_ar), by = "id") %>%
#   left_join(bind_rows(tr_garch, te_garch), by = "id") %>%
#   left_join(bind_rows(tr_arma, te_arma), by = "id") %>%
#   left_join(bind_rows(tr_hilbert_basic, te_hilbert_basic), by = "id") %>%
#   left_join(bind_rows(tr_acf_peak, te_acf_peak), by = "id") %>%
#   left_join(bind_rows(tr_noah, te_noah), by = "id") %>%
#   left_join(pc_acf, by = "id") %>%
#   left_join(pc_pacf, by = "id") %>%
#   left_join(pc_sqacf, by = "id") %>%
#   left_join(pc_sqpacf, by = "id") %>%
#   left_join(pc_envelope, by = "id") %>%
#   left_join(envelope_res, by = "id") %>%
#   left_join(pc_roll_sd, by = "id")
# # %>%
# #   mutate(sd_flg = sd > quantile(sd, .99))
# colnames(df) <- str_remove(colnames(df), "%")
# 
# write_csv(df, "data/features/features.csv")
