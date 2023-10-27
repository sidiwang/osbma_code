## calculate the bias, rMSE, CR, and CI Width of the multi-state model,
## and combine it with the results of other three models
source("~/simulation_code/summary_sim_result.R")

MSresult_1_100 <- NULL
for (i in list.files()[1:20]) {
  load(i)
  MSresult_1_100 <- rbind(MSresult_1_100, result_mean)
}

MSresult_1_200 <- NULL
for (i in list.files()[21:40]) {
  load(i)
  MSresult_1_200 <- rbind(MSresult_1_200, result_mean)
}

MSresult_1_300 <- NULL
for (i in list.files()[41:60]) {
  load(i)
  MSresult_1_300 <- rbind(MSresult_1_300, result_mean)
}

MSresult_2_100 <- NULL
for (i in list.files()[61:80]) {
  load(i)
  MSresult_2_100 <- rbind(MSresult_2_100, result_mean)
}

MSresult_2_200 <- NULL
for (i in list.files()[81:100]) {
  load(i)
  MSresult_2_200 <- rbind(MSresult_2_200, result_mean)
}

MSresult_2_300 <- NULL
for (i in list.files()[101:120]) {
  load(i)
  MSresult_2_300 <- rbind(MSresult_2_300, result_mean)
}

MSresult_3_100 <- NULL
for (i in list.files()[121:140]) {
  load(i)
  MSresult_3_100 <- rbind(MSresult_3_100, result_mean)
}

MSresult_3_200 <- NULL
for (i in list.files()[141:160]) {
  load(i)
  MSresult_3_200 <- rbind(MSresult_3_200, result_mean)
}

MSresult_3_300 <- NULL
for (i in list.files()[161:180]) {
  load(i)
  MSresult_3_300 <- rbind(MSresult_3_300, result_mean)
}

rmse2 <- function(x, y, na.rm = TRUE) {
  res <- sqrt(mean((x - y)^2, na.rm = na.rm))
  return(res)
}


###### edit the numbers to get different result #####
result <- cbind(result_1_200)
result_MS <- MSresult_1_200
colnames(result_MS)[1] <- "true_OS_MS"
#####################################################
sum_result <- rbind(
  c(
    mean(result[, "true_OS"] - result[, "JM"], na.rm = TRUE),
    mean(result[, "true_OS"] - result[, "copula"], na.rm = TRUE),
    mean(result[, "true_OS"] - result[, "survival"], na.rm = TRUE),
    mean(result_MS[, "true_OS_MS"] - result_MS[, "MS"])
  ) / 30.25,
  c(
    rmse2(result[, "true_OS"], result[, "JM"], na.rm = TRUE),
    rmse2(result[, "true_OS"], result[, "copula"], na.rm = TRUE),
    rmse2(result[, "true_OS"], result[, "survival"], na.rm = TRUE),
    rmse2(result_MS[, "true_OS_MS"], result_MS[, "MS"], na.rm = TRUE)
  ) / 30.25,
  c(
    sum(result[, "true_OS"] <= result[, "JM_high"] & result[, "true_OS"] >= result[, "JM_low"], na.rm = TRUE) / nrow(result),
    sum(result[, "true_OS"] <= result[, "copula_high"] & result[, "true_OS"] >= result[, "copula_low"], na.rm = TRUE) / nrow(result),
    sum(result[, "true_OS"] <= result[, "survival_high"] & result[, "true_OS"] >= result[, "survival_low"], na.rm = TRUE) / nrow(result),
    sum(result_MS[, "true_OS_MS"] <= result_MS[, "MS_high"] & result_MS[, "true_OS_MS"] >= result_MS[, "MS_low"], na.rm = TRUE) / nrow(result_MS)
  ),
  c(
    mean(result[, "JM_high"] - result[, "JM_low"], na.rm = TRUE) / 30.25,
    mean(result[, "copula_high"] - result[, "copula_low"], na.rm = TRUE) / 30.25,
    mean(result[, "survival_high"] - result[, "survival_low"], na.rm = TRUE) / 30.25,
    mean(result_MS[, "MS_high"] - result_MS[, "MS_low"], na.rm = TRUE) / 30.25
  )
)

rownames(sum_result) <- c("bias", "rmse", "CR", "Width")
colnames(sum_result) <- c("JM", "copula", "survival", "MS")
sum_result
