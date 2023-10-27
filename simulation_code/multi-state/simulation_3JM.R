#!/usr/bin/env Rscript
setwd("~/simulation_code/multi-state")
library(dplyr)
library(nlme)
library(Metrics)
library(stringr)
library("joineR")
library(survival)
library(survminer)
load("simulated_data.RData")
args <- commandArgs(trailingOnly = TRUE)
# test if there is at least one argument: if not, return an error
print(args)
if (length(args) == 0) {
  stop("At least one argument must be supplied (input file).n", call. = FALSE)
} else {
  if (args[[1]] == 1) {
    datalist_i <- list_ind
  } else if (args[[1]] == 2) {
    datalist_i <- list_NL
  } else {
    datalist_i <- list_TL
  }
  cutoff_i <- as.numeric(args[[2]])
  sample_i <- as.numeric(args[[3]])
}


result_mean <- result_median <- NULL
source("multi.state.R")

for (simulation_i in c((sample_i):(sample_i + 4))) {
  dataset <- datalist_i[[simulation_i]]

  colnames(dataset) <- c("SID", "alpha", "beta", "EVL1N", "TRT01PN", "EVL_TIME", "DMTSUM", "TT_NL", "TT_OS")

  dataset$TT_NL <- ceiling(dataset$TT_NL)
  dataset$TT_OS <- ceiling(dataset$TT_OS)
  dataset$CS_NL <- ifelse(dataset$TT_OS < dataset$TT_NL, 1, 0)
  dataset$TT_NL <- ifelse(dataset$CS_NL == 1, dataset$TT_OS, dataset$TT_NL)
  dataset <- dataset[-which(dataset$EVL_TIME > dataset$TT_OS), ]
  dataset <- dataset %>%
    arrange(SID, EVL1N) %>%
    group_by(SID) %>%
    mutate(pct_increase = (DMTSUM - cummin(DMTSUM)) / DMTSUM) %>%
    mutate(increase = DMTSUM - cummin(DMTSUM)) %>%
    mutate(TT_TL = ifelse(pct_increase >= 0.2 & increase >= 5, EVL_TIME, Inf)) %>%
    mutate(TT_TL = min(TT_TL))
  dataset$CS_TL <- ifelse(dataset$TT_TL == Inf, 1, 0)
  dataset$TT_TL <- ifelse(dataset$TT_TL == Inf, dataset$TT_OS, dataset$TT_TL)
  dataset$CS_OS <- 0
  dataset$TT_TTP <- pmin(dataset$TT_NL, dataset$TT_TL)
  dataset$CS_TTP <- ifelse(dataset$CS_TL + dataset$CS_NL == 2, 1, 0)
  dataset$TT_EVENT <- pmin(dataset$TT_NL, dataset$TT_OS, dataset$TT_TL)
  dataset$CS_EVENT <- 0

  cutoff <- c(100, 200, 300, 350)
  OS_dates <- data.frame("ID" = dataset$SID[which(dataset$CS_OS == 0)], "OS" = dataset$TT_OS[which(dataset$CS_OS == 0)])
  OS_dates <- unique(OS_dates)
  datecutoff <- sort(OS_dates$OS)[cutoff]




  for (ii in cutoff_i) {
    # make cutoff dataset
    if (sum(dataset$EVL_TIME > datecutoff[ii]) > 0) {
      D_INT <- dataset[-which(dataset$EVL_TIME > datecutoff[ii]), ]
    } else {
      D_INT <- dataset
    }

    D_INT$CS_NL <- ifelse(D_INT$TT_NL > datecutoff[ii] | D_INT$TT_NL >= D_INT$TT_OS, 1, D_INT$CS_NL)
    D_INT$TT_NL <- ifelse(D_INT$TT_NL > datecutoff[ii], NA, D_INT$TT_NL)

    D_INT$CS_TL <- ifelse(D_INT$TT_TL > datecutoff[ii] | D_INT$TT_TL >= D_INT$TT_OS, 1, D_INT$CS_TL)
    D_INT$TT_TL <- ifelse(D_INT$TT_TL > datecutoff[ii], NA, D_INT$TT_TL)

    D_INT$CS_TTP <- ifelse(D_INT$TT_TTP > datecutoff[ii] | D_INT$TT_TTP >= D_INT$TT_OS, 1, D_INT$CS_TTP)
    D_INT$TT_TTP <- ifelse(D_INT$TT_TTP > datecutoff[ii], NA, D_INT$TT_TTP)

    D_INT$CS_EVENT <- ifelse(D_INT$TT_EVENT > datecutoff[ii] | D_INT$TT_EVENT >= D_INT$TT_OS, 1, D_INT$CS_EVENT)
    D_INT$TT_EVENT <- ifelse(D_INT$TT_EVENT > datecutoff[ii], NA, D_INT$TT_EVENT)

    D_INT$CS_OS <- ifelse(D_INT$TT_OS > datecutoff[ii], 1, D_INT$CS_OS)
    D_INT$TT_OS <- ifelse(D_INT$TT_OS > datecutoff[ii], NA, D_INT$TT_OS)



    TT_EVENT_new <- pmin(ifelse(D_INT$CS_OS == 0, D_INT$TT_OS, NA),
      ifelse(D_INT$CS_NL == 0, D_INT$TT_NL, NA),
      ifelse(D_INT$CS_TL == 0, D_INT$TT_TL, NA),
      na.rm = TRUE
    )
    D_INT$TT_EVENT <- ifelse(is.na(D_INT$TT_EVENT), TT_EVENT_new, D_INT$TT_EVENT)
    D_INT$TT_EVENT <- ifelse(is.na(D_INT$TT_EVENT), datecutoff[ii], D_INT$TT_EVENT)
    D_INT$TT_NL <- ifelse(is.na(D_INT$TT_NL), datecutoff[ii], D_INT$TT_NL)
    D_INT$TT_TL <- ifelse(is.na(D_INT$TT_TL), datecutoff[ii], D_INT$TT_TL)
    D_INT$TT_OS <- ifelse(is.na(D_INT$TT_OS), datecutoff[ii], D_INT$TT_OS)
    D_INT$TT_TTP <- ifelse(is.na(D_INT$TT_TTP), datecutoff[ii], D_INT$TT_TTP)

    D_INT$CS_NL <- ifelse(is.na(D_INT$CS_NL), 1, D_INT$CS_NL)
    D_INT$CS_TL <- ifelse(is.na(D_INT$CS_TL), 1, D_INT$CS_TL)
    D_INT$CS_TTP <- ifelse(is.na(D_INT$CS_TTP), 1, D_INT$CS_TTP)


    # clean data


    path.export <- getwd()

    SID <- as.numeric(D_INT$SID)

    D_SURV <- D_INT
    D_SURV$SID <- as.numeric(D_SURV$SID)

    ###################################
    D_INT <- as.data.frame(cbind(
      ID = as.numeric(SID), time = D_INT$EVL1N, evl_time = D_INT$EVL_TIME,
      mu = D_INT$DMTSUM,
      time_PFS = D_INT$TT_EVENT, delta_PFS = D_INT$CS_EVENT,
      time_TTP = D_INT$TT_TTP, delta_TTP = D_INT$CS_TTP,
      time_NL = D_INT$TT_NL, delta_NL = D_INT$CS_NL,
      time_OS = D_INT$TT_OS, delta_OS = D_INT$CS_OS,
      trt = D_INT$TRT01PN
    ))

    D_INT <- D_INT[order(D_INT$ID, D_INT$time), ]

    D_INT <- D_INT[!is.na(D_INT$mu), ]

    for (i in c(1:(ncol(D_INT)))) {
      D_INT[, i] <- as.numeric(D_INT[, i])
    }



    ####### switch the censoring indicator ########

    D_INT$delta_NL <- 1 * (D_INT$delta_NL == 0)
    D_INT$delta_OS <- 1 * (D_INT$delta_OS == 0)
    D_INT$delta_PFS <- 1 * (D_INT$delta_PFS == 0)
    D_INT$delta_TTP <- 1 * (D_INT$delta_TTP == 0)

    ####### convert from unbalanced D_INT to balanced D_INT #######
    D_INT_sort <- D_INT %>%
      arrange(ID, time)

    D_INT <- to.balanced(D_INT_sort,
      id.col = "ID", time.col = "time",
      Y.col = c("mu", "evl_time"), other.col = (c(
        "time_NT", "delta_NT", "time_NL", "delta_NL",
        "time_OS", "delta_OS", "time_PFS", "delta_PFS",
        "time_TTP", "delta_TTP", "trt"
      ))
    )

    assign(paste0("D_INT", ii), D_INT)

    D_INT_MultiState <- D_INT_sort[, c("ID", "evl_time", "time_PFS", "delta_PFS", "time_OS", "delta_OS", "trt")]

    train_length <- nrow(D_INT_MultiState)
    for (qq in unique(D_INT_MultiState$ID)) {
      target <- D_INT_MultiState[max(which(D_INT_MultiState$ID == qq)), ]
      time_diff <- target$time_OS - target$evl_time
      D_INT_MultiState[nrow(D_INT_MultiState) + 1, ] <- target
      D_INT_MultiState$evl_time[nrow(D_INT_MultiState)] <- target$time_OS
    }

    pred_MultiState <- subset(D_INT_MultiState[(train_length + 1):nrow(D_INT_MultiState), ], delta_OS == 0)

    unmatchPFS_ID <- D_INT_MultiState %>%
      group_by(ID) %>%
      filter(!time_PFS %in% evl_time) %>%
      ungroup() %>%
      distinct(ID) %>%
      pull(ID)
    for (ijk in unmatchPFS_ID) {
      toBeCopied <- D_INT_MultiState[which(D_INT_MultiState$ID == ijk)[1], ]
      D_INT_MultiState[nrow(D_INT_MultiState) + 1, ] <- toBeCopied
      D_INT_MultiState$evl_time[nrow(D_INT_MultiState)] <- D_INT_MultiState$time_PFS[which(D_INT_MultiState$ID == ijk)[1]]
    }

    D_INT_MultiState$time_OS <- ifelse(D_INT_MultiState$time_OS < D_INT_MultiState$time_PFS, D_INT_MultiState$time_PFS, D_INT_MultiState$time_OS)
    D_INT_MultiState <- as.data.frame(D_INT_MultiState %>%
      arrange(ID, evl_time))

    D_INT_MultiState <- distinct(D_INT_MultiState)
    D_INT_MultiState$delta_PFS <- ifelse(D_INT_MultiState$time_PFS > D_INT_MultiState$evl_time | D_INT_MultiState$time_PFS == D_INT_MultiState$time_OS, 0, 1)
    D_INT_MultiState$delta_OS <- ifelse(D_INT_MultiState$time_OS > D_INT_MultiState$evl_time, 0, ifelse(D_INT_MultiState$delta_OS == 1, 1, 0))

    D_INT_MultiState$From.State <- ifelse(D_INT_MultiState$time_PFS >= D_INT_MultiState$evl_time, 1, 2)
    D_INT_MultiState$To.State1 <- ifelse(D_INT_MultiState$delta_PFS == 0 & D_INT_MultiState$delta_OS != 1, 1, 0)
    D_INT_MultiState$To.State2 <- ifelse(D_INT_MultiState$delta_PFS == 1 & D_INT_MultiState$delta_OS == 0, 1, 0)
    D_INT_MultiState$To.State3 <- ifelse(D_INT_MultiState$delta_OS == 1, 1, 0)

    D_INT_MultiState$t1 <- ifelse(D_INT_MultiState$time_PFS >= D_INT_MultiState$evl_time, D_INT_MultiState$evl_time, D_INT_MultiState$time_PFS)
    D_INT_MultiState$t2 <- ifelse(D_INT_MultiState$evl_time >= D_INT_MultiState$time_PFS,
      pmin(D_INT_MultiState$evl_time - D_INT_MultiState$time_PFS, D_INT_MultiState$time_OS - D_INT_MultiState$time_PFS), 0
    )

    D_INT_MultiState$t <- ifelse(D_INT_MultiState$t2 == 0, D_INT_MultiState$t1, D_INT_MultiState$t2)

    D_INT_MultiState <- D_INT_MultiState[which((D_INT_MultiState$From.State == 1 & D_INT_MultiState$To.State2 == 1) | (D_INT_MultiState$From.State == 1 & D_INT_MultiState$To.State3 == 1) | (D_INT_MultiState$From.State == 2 & D_INT_MultiState$To.State3 == 1)), ]


    pred_MultiState$From.State <- ifelse(pred_MultiState$time_PFS >= pred_MultiState$evl_time, 1, 2)
    pred_MultiState$To.State2 <- pred_MultiState$To.State1 <- 0
    pred_MultiState$To.State3 <- 1
    pred_MultiState$t1 <- ifelse(pred_MultiState$time_PFS >= pred_MultiState$evl_time, pred_MultiState$evl_time, pred_MultiState$time_PFS)
    pred_MultiState$t2 <- ifelse(pred_MultiState$evl_time >= pred_MultiState$time_PFS,
      pmin(pred_MultiState$evl_time - pred_MultiState$time_PFS, pred_MultiState$time_OS - pred_MultiState$time_PFS), 0
    )
    pred_MultiState$To.State2[which(pred_MultiState$From.State == 1)] <- 1
    pred_MultiState$To.State3[which(pred_MultiState$From.State == 1)] <- 0
    pred_MultiState$t <- NA

    if (length(which(pred_MultiState$From.State == 1)) > 0) {
      pred_stage2 <- pred_MultiState[which(pred_MultiState$From.State == 1), ]
      pred_stage2$From.State <- 2
      pred_stage2$To.State2 <- 0
      pred_stage2$To.State3 <- 1
      # pred_stage2$t1 = 0
      pred_stage2$t2 <- 0

      dat <- rbind(D_INT_MultiState, pred_stage2, pred_MultiState)
    } else {
      dat <- rbind(D_INT_MultiState, pred_MultiState)
    }

    dat$trt <- ifelse(dat$trt == 1, 1, 0)
    dat <- distinct(dat)

    X <- dat$trt
    Z_1 <- X

    n_MCMC_chain <- 2
    no.adapt <- 8000
    BURN.IN <- 1000
    MCMC_SAMPLE <- 10000
    thin <- 10
    longest_survival <- Inf

    t.cen.lim <- cbind(ceiling(ifelse(is.na(dat$t), ifelse(dat$From.State == 1, dat$t1, dat$t2), 0)), ifelse(dat$From.State == 1, longest_survival, longest_survival - dat$t1))
    delta_t <- rep(1, nrow(dat))
    N <- which((dat$From.State == 1 & dat$To.State2 == 1 & is.na(dat$t)) | (dat$From.State == 2 & dat$To.State3 == 1 & is.na(dat$t)))
    NN <- which((dat$From.State == 1 & dat$To.State3 == 1 & !is.na(dat$t)) | (dat$From.State == 2 & dat$To.State3 == 1 & !is.na(dat$t)))
    NNN <- which(dat$From.State == 1 & dat$To.State2 == 1 & !is.na(dat$t))

    dat.ind <- list(
      n = nrow(dat), Y = as.matrix(dat[, c("To.State1", "To.State2", "To.State3")]),
      from = dat$From.State, t = dat$t, cov = Z_1,
      t.cen.lim = t.cen.lim, delta_t = delta_t
    )

    source("multi.state.R")

    inits <- list(
      list(
        R12 = 0.3, R13 = 0.2, R23 = 0.4, B112 = 0.1, B123 = 0.1, B113 = 0.1,
        t = ifelse(is.na(dat$t), t.cen.lim[, 1] + 1, NA)
      ),
      list(
        R12 = 0.2, R13 = 0.1, R23 = 0.5, B112 = 0.6, B123 = 0.6, B113 = 0.9,
        t = ifelse(is.na(dat$t), t.cen.lim[, 1] + 1, NA)
      )
    )

    ######### multi-state model ##########

    out.ind <- NULL
    attempt <- 1
    while (is.null(out.ind) && attempt <= 10) {
      attempt <- attempt + 1

      try({
        out.ind <- R2jags::jags(
          data = dat.ind,
          model = ind_weib,
          n.burnin = BURN.IN,
          n.iter = MCMC_SAMPLE,
          n.thin = thin,
          n.chains = n_MCMC_chain,
          inits = inits,
          parameters.to.save = c(
            "R12", "R13", "R23",
            "B112", "B123", "B113",
            "A", "t"
          ),
          DIC = FALSE # ,
        )
      })
    }

    assign(paste0("trun_t", ii), ifelse(dat$From.State == 1, 0, dat$t1))
    out_post <- out.ind$BUGSoutput$summary
    assign(paste0("dat", ii), dat)
    assign(paste0("MS_median", ii), out.ind$BUGSoutput$median)
    assign(paste0("MS_mean", ii), out.ind$BUGSoutput$mean)
    assign(paste0("MS_sample", ii), out.ind$BUGSoutput$sims.matrix)
    assign(paste0("MS_R2jags", ii), out.ind)
  }


  dataset_1 <- dataset
  SID <- as.numeric(dataset_1$SID)

  ###################################
  dataset_1 <- as.data.frame(cbind(
    ID = as.numeric(SID), time = dataset_1$EVL1N, evl_time = dataset_1$EVL_TIME,
    mu = dataset_1$DMTSUM,
    time_PFS = dataset_1$TT_EVENT, delta_PFS = dataset_1$CS_EVENT,
    time_NL = dataset_1$TT_NL, delta_NL = dataset_1$CS_NL,
    time_OS = dataset_1$TT_OS, delta_OS = dataset_1$CS_OS,
    trt = dataset_1$TRT01PN
  ))

  dataset_1 <- dataset_1[order(dataset_1$ID, dataset_1$time), ]

  dataset_1 <- dataset_1[!is.na(dataset_1$mu), ]

  for (i in c(1:(ncol(dataset_1)))) {
    dataset_1[, i] <- as.numeric(dataset_1[, i])
  }



  ####### switch the censoring indicator ########

  dataset_1$delta_NL <- 1 * (dataset_1$delta_NL == 0)
  dataset_1$delta_OS <- 1 * (dataset_1$delta_OS == 0)
  dataset_1$delta_PFS <- 1 * (dataset_1$delta_PFS == 0)

  ####### convert from unbalanced dataset_1 to balanced dataset_1 #######
  dataset_1_sort <- dataset_1 %>%
    arrange(ID, time)

  dataset_1 <- to.balanced(dataset_1_sort,
    id.col = "ID", time.col = "time",
    Y.col = c("mu", "evl_time"), other.col = (c(
      "time_NT", "delta_NT", "time_NL", "delta_NL",
      "time_OS", "delta_OS", "time_PFS", "delta_PFS",
      "time_TTP", "delta_TTP", "trt"
    ))
  )


  dataset_1$time_OS <- as.numeric(dataset_1$time_OS)
  dataset_1$delta_OS <- as.numeric(dataset_1$delta_OS)

  # evaluation


  for (kk in cutoff_i) {
    data_e <- get(paste0("D_INT", kk))
    data_e$time_OS <- as.numeric(data_e$time_OS)
    data_e$delta_OS <- as.numeric(data_e$delta_OS)
    data_pred_MS <- get(paste0("MS_median", kk))
    true_OS <- dataset_1$time_OS[which(data_e$delta_OS != dataset_1$delta_OS)]
    assign(paste0("true_OS_dates", kk), true_OS)
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
    MS_ID <- dat_MS$ID %in% data_e$ID[which(data_e$delta_OS != dataset_1$delta_OS)]
    pred_OS_MS <- (dat_MS$t + tail(get(paste0("trun_t", kk)), nrow(dat_MS)))[MS_ID]
    names(pred_OS_MS) <- which(MS_ID == TRUE)
    MS_sample <- get(paste0("MS_sample", kk))

    MS_quantile <- HDInterval::hdi(MS_sample[, c((ncol(MS_sample) - nrow(dat_MS) + 1):ncol(MS_sample))], 0.9)[, MS_ID][, which.max(pred_OS_MS)] +
      tail(get(paste0("trun_t", kk)), nrow(dat_MS))[MS_ID][which.max(pred_OS_MS)]

    library(readr)
    key_number <- parse_number(colnames(MS_sample)[c((ncol(MS_sample) - nrow(dat_MS) + 1):ncol(MS_sample))][MS_ID][which.max(pred_OS_MS)])
    if (key_number %in% location_rep) {
      MS_quantile <- MS_quantile + HDInterval::hdi(MS_sample[, paste0("t[", location_rep[which(location_rep == key_number, arr.ind = TRUE)[1], 1], "]")], 0.9)
    }
    assign(paste0("MS_HDI_final", kk), MS_quantile)
    assign(paste0("MS_point", kk), max(pred_OS_MS))

    result_tmp_median <- c(
      max(true_OS),
      get(paste0("MS_point", kk)),
      get(paste0("MS_HDI_final", kk))[1], get(paste0("MS_HDI_final", kk))[2]
    )

    data_e <- get(paste0("D_INT", kk))
    data_e$time_OS <- as.numeric(data_e$time_OS)
    data_e$delta_OS <- as.numeric(data_e$delta_OS)
    data_pred_MS <- get(paste0("MS_mean", kk))
    true_OS <- dataset_1$time_OS[which(data_e$delta_OS != dataset_1$delta_OS)]
    assign(paste0("true_OS_dates", kk), true_OS)
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
    MS_ID <- dat_MS$ID %in% data_e$ID[which(data_e$delta_OS != dataset_1$delta_OS)]
    pred_OS_MS <- (dat_MS$t + tail(get(paste0("trun_t", kk)), nrow(dat_MS)))[MS_ID]
    names(pred_OS_MS) <- which(MS_ID == TRUE)
    MS_sample <- get(paste0("MS_sample", kk))

    MS_quantile <- HDInterval::hdi(MS_sample[, c((ncol(MS_sample) - nrow(dat_MS) + 1):ncol(MS_sample))], 0.9)[, MS_ID][, which.max(pred_OS_MS)] +
      tail(get(paste0("trun_t", kk)), nrow(dat_MS))[MS_ID][which.max(pred_OS_MS)]

    library(readr)
    key_number <- parse_number(colnames(MS_sample)[c((ncol(MS_sample) - nrow(dat_MS) + 1):ncol(MS_sample))][MS_ID][which.max(pred_OS_MS)])
    if (key_number %in% location_rep) {
      MS_quantile <- MS_quantile + HDInterval::hdi(MS_sample[, paste0("t[", location_rep[which(location_rep == key_number, arr.ind = TRUE)[1], 1], "]")], 0.9)
    }
    assign(paste0("MS_HDI_final", kk), MS_quantile)
    assign(paste0("MS_point", kk), max(pred_OS_MS))

    result_tmp_mean <- c(
      max(true_OS),
      get(paste0("MS_point", kk)),
      get(paste0("MS_HDI_final", kk))[1], get(paste0("MS_HDI_final", kk))[2]
    )
  }

  result_mean <- rbind(result_mean, result_tmp_mean)
  colnames(result_mean) <- c("true_OS", "MS", "MS_low", "MS_high")
  result_median <- rbind(result_median, result_tmp_median)
  colnames(result_median) <- c("true_OS", "MS", "MS_low", "MS_high")
}
save(result_mean, result_median, file = paste0("MS", args[[1]], "_", cutoff[cutoff_i], "_", sample_i, ".RData"))
