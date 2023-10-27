## calculate the bias, rMSE, CR, and CI Width of the Multivariate Joint Modeling Approach, 
## the Copula model, and the mariginal model

result_1_100 = NULL
for (i in list.files()[1:100]){
  load(i)
  result_1_100 = rbind(result_1_100, result_mean)
}

result_1_200 = NULL
for (i in list.files()[101:200]){
  load(i)
  result_1_200 = rbind(result_1_200, result_mean)
}

result_1_300 = NULL
for (i in list.files()[201:300]){
  load(i)
  result_1_300 = rbind(result_1_300, result_mean)
}

result_2_100 = NULL
for (i in list.files()[301:400]){
  load(i)
  result_2_100 = rbind(result_2_100, result_mean)
}

result_2_200 = NULL
for (i in list.files()[401:500]){
  load(i)
  result_2_200 = rbind(result_2_200, result_mean)
}

result_2_300 = NULL
for (i in list.files()[501:600]){
  load(i)
  result_2_300 = rbind(result_2_300, result_mean)
}

result_3_100 = NULL
for (i in list.files()[601:700]){
  load(i)
  result_3_100 = rbind(result_3_100, result_mean)
}

result_3_200 = NULL
for (i in list.files()[701:800]){
  load(i)
  result_3_200 = rbind(result_3_200, result_mean)
}

result_3_300 = NULL
for (i in list.files()[801:900]){
  load(i)
  result_3_300 = rbind(result_3_300, result_mean)
}



rmse2 <- function(x, y, na.rm = TRUE){
  res <- sqrt(mean((x-y)^2, na.rm = na.rm))
  return(res)
}

result = result_3_300
sum_result = rbind(c(mean(result[, "true_OS"] - result[, "JM"], na.rm = TRUE),
                     mean(result[, "true_OS"] - result[, "copula"], na.rm = TRUE),
                     mean(result[, "true_OS"] - result[, "survival"], na.rm = TRUE))/30.25,
                   c(rmse2(result[, "true_OS"], result[, "JM"], na.rm = TRUE),
                     rmse2(result[, "true_OS"], result[, "copula"], na.rm = TRUE),
                     rmse2(result[, "true_OS"], result[, "survival"], na.rm = TRUE))/30.25,
                   c(sum(result[, "true_OS"] <= result[, "JM_high"] & result[, "true_OS"] >= result[, "JM_low"], na.rm = TRUE)/nrow(result),
                     sum(result[, "true_OS"] <= result[, "copula_high"] & result[, "true_OS"] >= result[, "copula_low"], na.rm = TRUE)/nrow(result),
                     sum(result[, "true_OS"] <= result[, "survival_high"] & result[, "true_OS"] >= result[, "survival_low"], na.rm = TRUE)/nrow(result)),
                   c(mean(result[, "JM_high"] - result[, "JM_low"], na.rm = TRUE)/30.25,
                     mean(result[, "copula_high"] - result[, "copula_low"], na.rm = TRUE)/30.25,
                     mean(result[, "survival_high"] - result[, "survival_low"], na.rm = TRUE)/30.25))

rownames(sum_result) = c("bias", "rmse", "CR", "Width")
colnames(sum_result) = c("JM", "copula", "survival")
sum_result

 
