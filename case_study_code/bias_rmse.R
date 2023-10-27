### this file summarizes the bias and rMSE of each model

library(stringr)
library(dplyr)
data_e <- get(paste0("D_INT", kk))
data_e$time_OS <- as.numeric(data_e$time_OS)
data_e$delta_OS <- as.numeric(data_e$delta_OS)
data_pred_JM <- get(paste0("JM_mean", kk))
data_pred_copula <- get(paste0("copula_mean", kk))
data_pred_survival <- get(paste0("survival_mean", kk))
data_pred_MS <- get(paste0("MS_mean", kk))
dataset_1 <- subset(dataset_1, ID %in% data_e$ID)
true_OS <- dataset_1$time_OS[which(data_e$delta_OS != dataset_1$delta_OS)]
true_OS_dates <- sort(true_OS + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)])[total_death - cutoff[kk]]
assign(paste0("true_OS_dates", kk), true_OS_dates)
true_trt <- dataset_1$trt[which(data_e$delta_OS != dataset_1$delta_OS)]
dat_MS <- get(paste0("dat", kk))
dat_MS <- subset(dat_MS, is.na(t))
tmp_MS <- as.data.frame(cbind(dat_MS$ID, data_pred_MS$t[(length(data_pred_MS$t) - nrow(dat_MS) + 1):length(data_pred_MS$t)]))
colnames(tmp_MS) <- c("ID", "t")
rep_ID <- unique((tmp_MS %>% group_by(ID) %>% filter(n() > 1))[, 1]) #
get_location <- get(paste0("dat", kk))
location_rep <- matrix(NA, nrow(rep_ID), 2)
if (nrow(rep_ID) > 0) {
  for (location_i in c(1:nrow(rep_ID))) {
    location_rep[location_i, ] <- which(get_location$ID == as.numeric(rep_ID[location_i, ]))
  }
}
dat_MS <- unique(transform(tmp_MS, Total = ave(t, ID, FUN = sum))[-2]) %>% arrange(ID)
colnames(dat_MS) <- c("ID", "t")
pred_col_JM <- str_detect(names(data_pred_JM), "Y.mix_rep")
pred_OS_JM <- data_pred_JM[pred_col_JM][which(data_e$delta_OS != dataset_1$delta_OS)] * 365
pred_OS_JM_dates <- sort(round((pred_OS_JM)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)])[total_death - cutoff[kk] - result_correct[kk]]
enroll_OS_JM_dates <- (round((pred_OS_JM)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)] - round((pred_OS_JM)))[names(pred_OS_JM_dates)]
MS_ID <- dat_MS$ID %in% data_e$ID[which(data_e$delta_OS != dataset_1$delta_OS)]
pred_OS_MS <- (dat_MS$t * 365 + tail(get(paste0("trun_t", kk)), nrow(dat_MS)))[MS_ID]
names(pred_OS_MS) <- which(MS_ID == TRUE)
pred_OS_MS_dates <- sort(round((pred_OS_MS)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)])[total_death - cutoff[kk] - result_correct[kk]]
enroll_OS_MS_dates <- (round((pred_OS_MS)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)] - round((pred_OS_MS)))[names(pred_OS_MS_dates)]

JM_quantile <- coda::HPDinterval(get(paste0("JM_sample", kk)), prob = 0.9)[[1]][names(pred_OS_JM_dates), ]
assign(paste0("JM_HDI_final", kk), JM_quantile * 365)
assign(paste0("JM_point", kk), pred_OS_JM_dates)
assign(paste0("JM_enroll", kk), enroll_OS_JM_dates)

pred_col_copula <- str_detect(names(data_pred_copula), "Y.os.pfs_rep")
pred_OS_copula <- data_pred_copula[pred_col_copula][which(data_e$delta_OS != dataset_1$delta_OS)] * 365
pred_OS_copula_dates <- sort(round((pred_OS_copula)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)])[total_death - cutoff[kk] - result_correct[kk]]
enroll_OS_copula_dates <- (round((pred_OS_copula)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)] - round((pred_OS_copula)))[names(pred_OS_copula_dates)]

copula_quantile <- coda::HPDinterval(get(paste0("copula_sample", kk)), prob = 0.9)[[1]][names(pred_OS_copula_dates), ]
assign(paste0("copula_HDI_final", kk), copula_quantile * 365)
assign(paste0("copula_point", kk), pred_OS_copula_dates)
assign(paste0("copula_enroll", kk), enroll_OS_copula_dates)

pred_col_survival <- str_detect(names(data_pred_survival), "Y.os.wb_rep")
pred_OS_survival <- data_pred_survival[pred_col_survival][which(data_e$delta_OS != dataset_1$delta_OS)] * 365
pred_OS_survival_dates <- sort(round((pred_OS_survival)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)])[total_death - cutoff[kk] - result_correct[kk]]
enroll_OS_survival_dates <- (round((pred_OS_survival)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)] - round((pred_OS_survival)))[names(pred_OS_survival_dates)]

survival_quantile <- coda::HPDinterval(get(paste0("survival_sample", kk)), prob = 0.9)[[1]][names(pred_OS_survival_dates), ]
assign(paste0("survival_HDI_final", kk), survival_quantile * 365)
assign(paste0("survival_point", kk), pred_OS_survival_dates)
assign(paste0("survival_enroll", kk), enroll_OS_survival_dates)

MS_sample <- get(paste0("MS_sample", kk))
MS_quantile <- HDInterval::hdi(MS_sample[, c((ncol(MS_sample) - nrow(dat_MS) + 1):ncol(MS_sample))], 0.9)[, MS_ID][, which.max(pred_OS_MS)] +
  tail(get(paste0("trun_t", kk)), nrow(dat_MS))[MS_ID][which.max(pred_OS_MS)]
library(readr)
key_number <- parse_number(colnames(MS_sample)[c((ncol(MS_sample) - nrow(dat_MS) + 1):ncol(MS_sample))][MS_ID][which.max(pred_OS_MS)])
if (key_number %in% location_rep) {
  MS_quantile <- MS_quantile + HDInterval::hdi(MS_sample[, paste0("t[", location_rep[which(location_rep == key_number, arr.ind = TRUE)[1], 1], "]")], 0.9)
}
assign(paste0("MS_HDI_final", kk), MS_quantile * 365)
assign(paste0("MS_point", kk), max(pred_OS_MS_dates))
assign(paste0("MS_enroll", kk), enroll_OS_MS_dates)


library(Metrics)
true_date <- true_OS + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)]
JM_date <- round((pred_OS_JM)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)]
copula_date <- round((pred_OS_copula)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)]
survival_date <- round((pred_OS_survival)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)]
MS_date <- round((pred_OS_MS)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)]

JM_Q <- coda::HPDinterval(get(paste0("JM_sample", kk)), prob = 0.9)[[1]][names(pred_OS_JM), ]
copula_Q <- coda::HPDinterval(get(paste0("copula_sample", kk)), prob = 0.9)[[1]][names(pred_OS_copula), ]
survival_Q <- coda::HPDinterval(get(paste0("survival_sample", kk)), prob = 0.9)[[1]][names(pred_OS_survival), ]
MS_Q <- HDInterval::hdi(MS_sample[, c((ncol(MS_sample) - nrow(dat_MS) + 1):ncol(MS_sample))], 0.9)[, MS_ID]


result_bias_rmse <- rbind(
  c(
    mean((sort(true_date) - sort(JM_date))) / 30.25,
    sqrt(mean(as.numeric((sort(true_date) - sort(JM_date)))^2)) / 30.25
  ),
  c(
    mean((sort(true_date) - sort(copula_date))) / 30.25,
    sqrt(mean(as.numeric((sort(true_date) - sort(copula_date)))^2)) / 30.25
  ),
  c(
    mean((sort(true_date) - sort(survival_date))) / 30.25,
    sqrt(mean(as.numeric((sort(true_date) - sort(survival_date)))^2)) / 30.25
  ),
  c(
    mean((sort(true_date) - sort(MS_date))) / 30.25,
    sqrt(mean(as.numeric((sort(true_date) - sort(MS_date)))^2)) / 30.25
  )
)

colnames(result_bias_rmse) <- c("bias", "rmse")
rownames(result_bias_rmse) <- c("JM", "copula", "survival", "MS")
result_bias_rmse

