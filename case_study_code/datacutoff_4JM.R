dataset <- dataset[which(dataset$CS_OS == 0), ]
dataset$DMTSUM <- sqrt(dataset$DMTSUM)
dataset$EVL_TIME[dataset$EVL_TIME < 0] <- 0

dataset$SEXN <- dataset$SEXN - 1
dataset$AGEGR1N <- dataset$AGEGR1N - 1

dataset$RND1O <- as.Date(dataset$RND1O, "%m/%d/%Y")
dataset$ASM_1O <- as.Date(dataset$ASM_1O, "%d-%b-%y")
dataset$NACDT <- as.Date(dataset$NACDT, "%d-%b-%y")
dataset$PFSCSDT <- as.Date(dataset$PFSCSDT, "%d-%b-%y")
dataset$TTPCSDT <- as.Date(dataset$TTPCSDT, "%d-%b-%y")


dataset$TT_EVENTDT <- dataset$TT_EVENT + dataset$RND1O
dataset$TT_OSDT <- dataset$TT_OS + dataset$RND1O
dataset$TT_TLDT <- dataset$TT_TL + dataset$RND1O
dataset$TT_NTLDT <- dataset$TT_NTL + dataset$RND1O
dataset$TT_NLDT <- dataset$TT_NL + dataset$RND1O
dataset$TT_TTPDT <- dataset$TT_TTP + dataset$RND1O

cutoff <- c(100, 146, 200, 300)
OS_dates <- data.frame("ID" = dataset$SID[which(dataset$CS_OS == 0)], "OS" = dataset$TT_OSDT[which(dataset$CS_OS == 0)])
OS_dates <- unique(OS_dates)
datecutoff <- as.Date(sort(OS_dates$OS)[c(100, 146, 200, 300)])

result_correct <- c(
  sum(as.character(OS_dates[, 2]) == as.character(datecutoff[1])) - 1,
  sum(as.character(OS_dates[, 2]) == as.character(datecutoff[2])) - 1,
  sum(as.character(OS_dates[, 2]) == as.character(datecutoff[3])) - 1,
  sum(as.character(OS_dates[, 2]) == as.character(datecutoff[4])) - 1
)


library(stringr)
library(dplyr)

for (ii in cutoff_i) {
  # make cutoff dataset
  D_INT <- dataset[-which(dataset$ASM_1O > datecutoff[ii]), ]
  D_INT$CS_NL <- ifelse(round(D_INT$TT_NLDT - datecutoff[ii]) > 0 | D_INT$TT_NLDT >= D_INT$TT_OSDT, 1, D_INT$CS_NL)
  D_INT$TT_NL <- ifelse(round(D_INT$TT_NLDT - datecutoff[ii]) > 0, NA, D_INT$TT_NL)

  D_INT$CS_NTL <- ifelse(round(D_INT$TT_NTLDT - datecutoff[ii]) > 0 | D_INT$TT_NTLDT >= D_INT$TT_OSDT, 1, D_INT$CS_NTL)
  D_INT$TT_NTL <- ifelse(round(D_INT$TT_NTLDT - datecutoff[ii]) > 0, NA, D_INT$TT_NTL)

  D_INT$CS_TL <- ifelse(round(D_INT$TT_TLDT - datecutoff[ii]) > 0 | D_INT$TT_TLDT >= D_INT$TT_OSDT, 1, D_INT$CS_TL)
  D_INT$TT_TL <- ifelse(round(D_INT$TT_TLDT - datecutoff[ii]) > 0, NA, D_INT$TT_TL)

  D_INT$CS_OS <- ifelse(round(D_INT$TT_OSDT - datecutoff[ii]) > 0, 1, D_INT$CS_OS)
  D_INT$TT_OS <- ifelse(round(D_INT$TT_OSDT - datecutoff[ii]) > 0, NA, D_INT$TT_OS)

  D_INT$CS_TTP <- ifelse(D_INT$TT_TTPDT - datecutoff[ii] > 0 | D_INT$TT_TTPDT >= D_INT$TT_OSDT, 1, D_INT$CS_TTP)
  D_INT$TT_TTP <- ifelse(D_INT$TT_TTPDT - datecutoff[ii] > 0, NA, D_INT$TT_TTP)

  D_INT$CS_EVENT <- ifelse(round(D_INT$TT_EVENTDT - datecutoff[ii]) > 0 | D_INT$TT_EVENTDT >= D_INT$TT_OSDT, 1, D_INT$CS_EVENT)
  D_INT$TT_EVENT <- ifelse(round(D_INT$TT_EVENTDT - datecutoff[ii]) > 0, NA, D_INT$TT_EVENT)


  TT_EVENT_new <- pmin(ifelse(D_INT$CS_OS == 0, D_INT$TT_OS, NA),
    ifelse(D_INT$CS_NL == 0, D_INT$TT_NL, NA),
    ifelse(D_INT$CS_NTL == 0, D_INT$TT_NTL, NA),
    ifelse(D_INT$CS_TL == 0, D_INT$TT_TL, NA),
    na.rm = TRUE
  )

  D_INT$TT_EVENT <- ifelse(is.na(D_INT$TT_EVENT), TT_EVENT_new, D_INT$TT_EVENT)
  last_TT_EVENT <- as.data.frame(D_INT %>% group_by(SID) %>% slice(tail(row_number(), 1)) %>% dplyr::select(SID, EVL_TIME))

  for (last_i in c(1:nrow(D_INT))) {
    if (is.na(D_INT$TT_EVENT[last_i])) {
      D_INT$TT_EVENT[last_i] <- last_TT_EVENT[which(last_TT_EVENT[, 1] == D_INT$SID[last_i]), 2]
    }
  }

  for (last_i in c(1:nrow(D_INT))) {
    if (is.na(D_INT$TT_NL[last_i]) | D_INT$CS_NL[last_i] == 1) {
      D_INT$CS_NL[last_i] <- 1
      D_INT$TT_NL[last_i] <- last_TT_EVENT[which(last_TT_EVENT[, 1] == D_INT$SID[last_i]), 2]
    }
  }

  for (last_i in c(1:nrow(D_INT))) {
    if (is.na(D_INT$TT_NTL[last_i]) | D_INT$CS_NTL[last_i] == 1) {
      D_INT$CS_NTL[last_i] <- 1
      D_INT$TT_NTL[last_i] <- last_TT_EVENT[which(last_TT_EVENT[, 1] == D_INT$SID[last_i]), 2]
    }
  }

  for (last_i in c(1:nrow(D_INT))) {
    if (is.na(D_INT$TT_OS[last_i])) {
      D_INT$TT_OS[last_i] <- last_TT_EVENT[which(last_TT_EVENT[, 1] == D_INT$SID[last_i]), 2]
    }
  }

  for (last_i in c(1:nrow(D_INT))) {
    if (is.na(D_INT$TT_TTP[last_i]) | D_INT$CS_TTP[last_i] == 1) {
      D_INT$CS_TTP[last_i] <- 1
      D_INT$TT_TTP[last_i] <- last_TT_EVENT[which(last_TT_EVENT[, 1] == D_INT$SID[last_i]), 2]
    }
  }

  for (last_i in c(1:nrow(D_INT))) {
    if (is.na(D_INT$TT_TL[last_i]) | D_INT$CS_TL[last_i] == 1) {
      D_INT$CS_TL[last_i] <- 1
      D_INT$TT_TL[last_i] <- last_TT_EVENT[which(last_TT_EVENT[, 1] == D_INT$SID[last_i]), 2]
    }
  }

  D_INT <- D_INT %>%
    group_by(SID) %>%
    mutate(TT_EVENT_new = ifelse(n() > 1 & abs(TT_EVENT - EVL_TIME[which.min(abs(TT_EVENT - EVL_TIME))]) < 3.5, EVL_TIME[which.min(abs(TT_EVENT - EVL_TIME))], TT_EVENT))

  D_INT$TT_EVENT <- round(D_INT$TT_EVENT_new)
  D_INT$TT_EVENT <- ifelse(D_INT$TT_EVENT == 0, 1, D_INT$TT_EVENT)
  D_INT$TT_OS <- pmax(D_INT$TT_EVENT, round(D_INT$TT_OS))

  D_INT <- D_INT %>%
    group_by(SID) %>%
    mutate(TT_NL_new = ifelse(n() > 1 & abs(TT_NL - EVL_TIME[which.min(abs(TT_NL - EVL_TIME))]) < 3.5, EVL_TIME[which.min(abs(TT_NL - EVL_TIME))], TT_NL))

  D_INT$TT_NL <- round(D_INT$TT_NL_new)

  D_INT <- D_INT %>%
    group_by(SID) %>%
    mutate(TT_NTL_new = ifelse(n() > 1 & abs(TT_NTL - EVL_TIME[which.min(abs(TT_NTL - EVL_TIME))]) < 3.5, EVL_TIME[which.min(abs(TT_NTL - EVL_TIME))], TT_NTL))

  D_INT$TT_NTL <- round(D_INT$TT_NTL_new)

  D_INT <- D_INT %>%
    group_by(SID) %>%
    mutate(TT_TL_new = ifelse(n() > 1 & abs(TT_TL - EVL_TIME[which.min(abs(TT_TL - EVL_TIME))]) < 3.5, EVL_TIME[which.min(abs(TT_TL - EVL_TIME))], TT_TL))

  D_INT$TT_TL <- round(D_INT$TT_TL_new)

  D_INT <- D_INT %>%
    group_by(SID) %>%
    mutate(TT_TTP_new = ifelse(n() > 1 & abs(TT_TTP - EVL_TIME[which.min(abs(TT_TTP - EVL_TIME))]) < 3.5, EVL_TIME[which.min(abs(TT_TTP - EVL_TIME))], TT_TTP))

  D_INT$TT_TTP <- round(D_INT$TT_TTP_new)

  correct_TT_EVENT <- pmin(ifelse(D_INT$CS_OS == 0, D_INT$TT_OS, NA),
    ifelse(D_INT$CS_NL == 0, D_INT$TT_NL, NA),
    ifelse(D_INT$CS_NTL == 0, D_INT$TT_NTL, NA),
    ifelse(D_INT$CS_TL == 0, D_INT$TT_TL, NA),
    na.rm = TRUE
  )

  D_INT$TT_EVENT <- ifelse(is.na(correct_TT_EVENT), D_INT$TT_EVENT, correct_TT_EVENT)

  D_INT$CS_TTP <- ifelse(D_INT$CS_EVENT == 1, 1, ifelse(D_INT$CS_OS == 0 & D_INT$TT_OS == D_INT$TT_EVENT, 1, 0))

  D_INT$TT_NL <- ifelse(D_INT$TT_NL == 0, D_INT$TT_EVENT, D_INT$TT_NL)
  D_INT$TT_NTL <- ifelse(D_INT$TT_NTL == 0, D_INT$TT_EVENT, D_INT$TT_NTL)
  D_INT$TT_TTP <- ifelse(D_INT$TT_TTP == 0, D_INT$TT_EVENT, D_INT$TT_TTP)
  D_INT$TT_TL <- ifelse(D_INT$TT_TL == 0, D_INT$TT_EVENT, D_INT$TT_TL)

  # clean data
  library("joineR")

  path.export <- getwd()

  library(dplyr)

  if (!is.integer(D_INT$SID[1])) {
    D_INT$SID <- substring(D_INT$SID, 15, 22)
  }

  SID <- as.numeric(D_INT$SID)

  D_SURV <- D_INT
  D_SURV$SID <- as.numeric(D_SURV$SID)

  ###################################
  D_INT <- data.frame(
    ID = as.numeric(SID), RND1O = D_INT$RND1O, time = D_INT$EVL1N, evl_time = D_INT$EVL_TIME,
    mu = D_INT$DMTSUM, time_PFS = D_INT$TT_EVENT, delta_PFS = D_INT$CS_EVENT,
    time_TTP = D_INT$TT_TTP, delta_TTP = D_INT$CS_TTP,
    time_NT = D_INT$TT_NTL,
    delta_NT = D_INT$CS_NTL, time_NL = D_INT$TT_NL,
    delta_NL = D_INT$CS_NL, time_OS = D_INT$TT_OS, delta_OS = D_INT$CS_OS,
    hengbl = D_INT$HENGBL,
    sex = D_INT$SEXN,
    age = D_INT$AGEGR1N, strati1 = D_INT$STRATI1N,
    nephrcbl = D_INT$NEPHRCBL,
    trt = D_INT$TRT01PN
  )

  D_INT$hengbl <- ifelse(D_INT$hengbl == "Favorable", 0, ifelse(is.na(D_INT$hengbl), NA, 1))
  D_INT <- D_INT[order(D_INT$ID, D_INT$time), ]

  D_INT <- D_INT[!is.na(D_INT$mu), ]

  for (i in c(3:(ncol(D_INT) - 3))) {
    D_INT[, i] <- as.numeric(D_INT[, i])
  }

  D_INT$trt <- as.numeric(D_INT$trt) - 1
  D_INT$nephrcbl <- ifelse(D_INT$nephrcbl == "Yes", 1, 0)



  ####### switch the censoring indicator ########

  D_INT$delta_NT <- 1 * (D_INT$delta_NT == 0)
  D_INT$delta_NL <- 1 * (D_INT$delta_NL == 0)
  D_INT$delta_OS <- 1 * (D_INT$delta_OS == 0)
  D_INT$delta_PFS <- 1 * (D_INT$delta_PFS == 0)
  D_INT$delta_TTP <- 1 * (D_INT$delta_TTP == 0)

  ####### convert from unbalanced D_INT to balanced D_INT #######
  D_INT_sort <- as.data.frame(D_INT %>%
    arrange(ID, time))


  D_INT <- to.balanced(D_INT_sort,
    id.col = "ID", time.col = "time",
    Y.col = c("mu", "evl_time"), other.col = (c(
      "time_NT", "delta_NT", "time_NL", "delta_NL", "time_OS", "delta_OS", "time_PFS", "delta_PFS", "time_TTP", "delta_TTP",
      "hengbl", "sex", "age", "strati1",
      "nephrcbl", "trt"
    ))
  )

  assign(paste0("D_INT", ii), D_INT)


  ####### prepare data for multi-state model

  D_INT_MultiState <- D_INT_sort[, c("ID", "RND1O", "evl_time", "mu", "time_PFS", "delta_PFS", "time_OS", "delta_OS", "hengbl", "sex", "age", "strati1", "nephrcbl", "trt")]
  D_INT_MultiState <- as.data.frame(D_INT_MultiState %>%
    group_by(ID) %>%
    mutate(baseline_mu = first(mu)))
  D_INT_MultiState$baseline_mu <- (D_INT_MultiState$baseline_mu - mean(D_INT$mu.t0)) / sd(D_INT$mu.t0)
  D_INT_MultiState <- D_INT_MultiState[, c("ID", "RND1O", "evl_time", "baseline_mu", "time_PFS", "delta_PFS", "time_OS", "delta_OS", "hengbl", "sex", "age", "strati1", "nephrcbl", "trt")]
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

  pred_stage2 <- pred_MultiState[which(pred_MultiState$From.State == 1), ]
  pred_stage2$From.State <- 2
  pred_stage2$To.State2 <- 0
  pred_stage2$To.State3 <- 1
  pred_stage2$t2 <- 0

  dat <- rbind(D_INT_MultiState, pred_stage2, pred_MultiState)
  dat$trt <- ifelse(dat$trt == 1, 1, 0)
  dat <- distinct(dat)

  ############# end of preparing data for multi-state model #############

  ### organizing data for JAGS model fitting

  for (i in c((ncol(D_INT) - 11):(ncol(D_INT)))) {
    D_INT[, i] <- as.numeric(D_INT[, i])
  }

  N <- nrow(D_INT)
  Npar_0 <- 6
  Npar <- 7
  delta_NT <- D_INT$delta_NT
  delta_NL <- D_INT$delta_NL
  delta_OS <- D_INT$delta_OS
  delta_PFS <- as.numeric(D_INT$delta_PFS)
  delta_TTP <- as.numeric(D_INT$delta_TTP)
  time_TTP <- D_INT$time_TTP
  time_PFS <- D_INT$time_PFS
  time_OS <- D_INT$time_OS
  Y_os <- D_INT$time_OS
  Y_2 <- D_INT$time_NT
  Y_3 <- D_INT$time_NL
  X <- cbind(
    D_INT$hengbl, D_INT$sex, D_INT$age, D_INT$strati1,
    D_INT$nephrcbl, D_INT$trt
  )
  Z_1 <- cbind(rep(1, N), X)
  Z_2 <- cbind(rep(1, N), X)
  mu_colno <- str_detect(colnames(D_INT), "mu.t")
  Y <- D_INT[mu_colno]

  evl_time_colno <- str_detect(colnames(D_INT), "evl_time.t")
  t <- D_INT[evl_time_colno]


  evl_times <- ncol(Y)
  t.os <- numeric(N)
  t.cen <- numeric(N)
  T <- numeric(N)
  T1 <- numeric(N)
  p <- numeric(4)

  p[1] <- Npar_0 + 7 + Npar
  p[2] <- Npar + 2
  p[3] <- Npar + 2
  p[4] <- Npar + 1



  for (i in 1:N) {
    if (delta_OS[i] == 1) {
      t.os[i] <- Y_os[i]
      t.cen[i] <- 0
    } else {
      t.os[i] <- NA
      t.cen[i] <- Y_os[i]
    }
  }


  for (i in 1:N) {
    d <- Y[i, ]
    d <- d[!is.na(d)]
    T[i] <- length(d)
    Y[i, ] <- c(d, rep(0, evl_times - length(d)))
  }

  for (i in 1:N) {
    d <- t[i, ]
    d <- d[!is.na(d)]
    T1[i] <- length(d)
    t[i, ] <- c(d, rep(0, evl_times - length(d)))
  }

  Y <- cbind(Y, rep(0, N))
  t <- cbind(t, rep(0, N))

  t.os <- as.numeric(t.os)
  t.cen <- as.numeric(t.cen)
  t <- data.matrix(t)
  T <- as.numeric(T)
  Y <- data.matrix(Y)
  Y_2 <- as.numeric(Y_2)
  Y_os <- as.numeric(Y_os)
  delta_NT <- as.numeric(delta_NT)
  delta_OS <- as.numeric(delta_OS)
  Y_3 <- as.numeric(Y_3)
  delta_NL <- as.numeric(delta_NL)
  X <- data.matrix(X)
  Z_1 <- data.matrix(Z_1)
  Z_2 <- data.matrix(Z_2)
  b <- matrix(rep(rep(0, N), 2), ncol = 2)
  b_os.1 <- rep(10, N)
  os.2 <- rep(log(1), N)
  nt.2 <- rep(log(0.4), N)
  os.3 <- rep(log(1), N)
  nl.3 <- rep(log(0.4), N)


  n_MCMC_chain <- 2
  no.adapt <- 20000
  BURN.IN <- 10000
  MCMC_SAMPLE <- 30000
  thin <- 3


  ######### joint model ##########

  jag.model.name <- "ospred_get_posterior_4JM.bug"
  t.cen.lim.wb.os <- round(cbind(ifelse(delta_OS == 1, 0, Y_os / 365), rep(longest_survival / 365, length(delta_OS))), 3)
  t.cen.lim <- round(cbind(ifelse(delta_OS == 1, 0, Y_os / 365), rep(longest_survival / 365, length(delta_OS))), 3)
  t.cen.lim.nl <- round(cbind(ifelse(delta_NL == 1, 0, Y_3 / 365), rep(longest_survival / 365, length(delta_NL))), 3)
  y.lim_t <- y.lim_t.os <- rep(1, length(delta_OS))
  y.lim_wb <- y.lim_wb.os <- rep(1, length(delta_OS))
  y.lim_nl.os <- y.lim_nt.os <- rep(1, length(delta_OS))
  y.lim_nl <- rep(1, length(delta_NL))
  y.lim_nt <- rep(1, length(delta_NT))
  Y.os.t_rep <- Y.os.nl_rep <- Y.os.nt_rep <- Y.os.wb_rep <- round(ifelse(t.cen == 0, NA, (t.cen + 1) / 365), 3)
  Y.nl_rep <- round(ifelse(delta_NL == 1, NA, (Y_3 + 1) / 365), 3)
  Y.nt_rep <- round(ifelse(delta_NT == 1, NA, (Y_2 + 1) / 365), 3)

  TL_ob_n <- which(delta_OS == 1)
  TL_c_n <- which(delta_OS == 0)

  posterior_sample <- NULL
  attempt <- 1
  while (is.null(posterior_sample) && attempt <= 5) {
    attempt <- attempt + 1

    try({
      jag <- rjags::jags.model(file.path(jag.model.name),
        data = list(
          N = N,
          longest_survival = round(longest_survival / 365, 3),
          TL_ob_n = TL_ob_n,
          TL_c_n = TL_c_n,
          Npar_0 = Npar_0,
          Npar = Npar,
          X = X,
          Z_1 = Z_1,
          Z_2 = Z_2,
          t.os = round(t.os / 365, 3),
          t.os.wb = round(t.os / 365, 3),
          t.cen.lim = t.cen.lim,
          t.cen.lim.wb.os = t.cen.lim.wb.os,
          y.lim_t = y.lim_t,
          y.lim_t.os = y.lim_t.os,
          y.lim_nt.os = y.lim_nt.os,
          y.lim_nl.os = y.lim_nl.os,
          y.lim_wb = y.lim_wb,
          y.lim_wb.os = y.lim_wb.os,
          t = round(t / 365, 3),
          T = T,
          Y = Y,
          Y_os = round(Y_os / 365, 3),
          Y_2 = round(Y_2 / 365, 3),
          Y_3 = round(Y_3 / 365, 3),
          delta_NT = delta_NT,
          delta_NL = delta_NL,
          delta_OS = delta_OS,
          evl_times = evl_times
        ),
        inits <- function() {
          list(
            Y.os.nl_rep = ifelse(is.na(Y.os.nl_rep), 1 / 365, Y.os.nl_rep),
            Y.os.nt_rep = ifelse(is.na(Y.os.nt_rep), 1 / 365, Y.os.nt_rep),
            t.os = Y.os.t_rep,
            t.os.wb = Y.os.wb_rep,
            Y.os.t_rep = ifelse(is.na(Y.os.t_rep), 1 / 365, Y.os.t_rep),
            Y.os.wb_rep = ifelse(is.na(Y.os.wb_rep), 1 / 365, Y.os.wb_rep)
          )
        },
        n.chains = n_MCMC_chain, n.adapt = no.adapt
      )
      update(jag, BURN.IN)

      posterior_sample <- rjags::coda.samples(jag,
        c(
          "beta_0", "beta_1", "lambda", "alpha_os.1", "eta_2",
          "alpha_2", "beta_2", "alpha_os.2", "beta_os.2", "eta_3",
          "alpha_3", "beta_3", "alpha_os.3", "beta_os.3",
          "alpha_os.4", "beta_os.4",
          "sigma_os.1", "sigma_os.2", "sigma_os.3",
          "sigma_b1", "sigma_b2", "rho"
        ),
        n.iter = MCMC_SAMPLE, thin = thin
      )
    })
  }

  out_post <- summary(posterior_sample)

  # beta_0, beta_1, alpha_os.1, lambda, sigma_b1, sigma_b2, rho, sigma_os.1
  mg1 <- out_post$statistics[c(
    "beta_0[1]", "beta_0[2]", "beta_0[3]", "beta_0[4]", "beta_0[5]",
    "beta_0[6]", "beta_0[7]", "beta_1[1]", "beta_1[2]", "beta_1[3]",
    "beta_1[4]", "beta_1[5]", "beta_1[6]", "beta_1[7]", "alpha_os.1",
    "lambda", "sigma_b1", "sigma_b2", "rho", "sigma_os.1"
  ), 1]
  seg1 <- out_post$statistics[c(
    "beta_0[1]", "beta_0[2]", "beta_0[3]", "beta_0[4]", "beta_0[5]",
    "beta_0[6]", "beta_0[7]", "beta_1[1]", "beta_1[2]", "beta_1[3]",
    "beta_1[4]", "beta_1[5]", "beta_1[6]", "beta_1[7]", "alpha_os.1",
    "lambda", "sigma_b1", "sigma_b2", "rho", "sigma_os.1"
  ), 2]


  # beta_os.2, alpha_os.2, sigma_os.2
  mg2 <- out_post$statistics[c(
    "beta_os.2[1]", "beta_os.2[2]", "beta_os.2[3]", "beta_os.2[4]",
    "beta_os.2[5]", "beta_os.2[6]", "beta_os.2[7]", "alpha_os.2", "sigma_os.2"
  ), 1]
  seg2 <- out_post$statistics[c(
    "beta_os.2[1]", "beta_os.2[2]", "beta_os.2[3]", "beta_os.2[4]",
    "beta_os.2[5]", "beta_os.2[6]", "beta_os.2[7]", "alpha_os.2", "sigma_os.2"
  ), 2]


  # beta_os.3, alpha_os.3, sigma_os.3
  mg3 <- out_post$statistics[c(
    "beta_os.3[1]", "beta_os.3[2]", "beta_os.3[3]", "beta_os.3[4]",
    "beta_os.3[5]", "beta_os.3[6]", "beta_os.3[7]", "alpha_os.3", "sigma_os.3"
  ), 1]
  seg3 <- out_post$statistics[c(
    "beta_os.3[1]", "beta_os.3[2]", "beta_os.3[3]", "beta_os.3[4]",
    "beta_os.3[5]", "beta_os.3[6]", "beta_os.3[7]", "alpha_os.3", "sigma_os.3"
  ), 2]

  # beta_os.4, alpha_os.4
  mg4 <- out_post$statistics[c(
    "beta_os.4[1]", "beta_os.4[2]", "beta_os.4[3]", "beta_os.4[4]",
    "beta_os.4[5]", "beta_os.4[6]", "beta_os.4[7]", "alpha_os.4"
  ), 1]
  seg4 <- out_post$statistics[c(
    "beta_os.4[1]", "beta_os.4[2]", "beta_os.4[3]", "beta_os.4[4]",
    "beta_os.4[5]", "beta_os.4[6]", "beta_os.4[7]", "alpha_os.4"
  ), 2]


  jag.model.name <- "ospred_4JM.bug"
  t.cen.lim.wb.os <- round(cbind(ifelse(delta_OS == 1, 0, Y_os / 365), rep(longest_survival / 365, length(delta_OS))), 3)
  t.cen.lim <- round(cbind(ifelse(delta_OS == 1, 0, Y_os / 365), rep(longest_survival / 365, length(delta_OS))), 3)
  t.cen.lim.nl <- round(cbind(ifelse(delta_NL == 1, 0, Y_3 / 365), rep(longest_survival / 365, length(delta_NL))), 3)
  y.lim_t <- y.lim_t.os <- rep(1, length(delta_OS))
  y.lim_wb <- y.lim_wb.os <- rep(1, length(delta_OS))
  y.lim_nl.os <- y.lim_nt.os <- rep(1, length(delta_OS))
  y.lim_nl <- rep(1, length(delta_NL))
  y.lim_nt <- rep(1, length(delta_NT))
  Y.os.t_rep <- Y.os.nl_rep <- Y.os.nt_rep <- Y.os.wb_rep <- round(ifelse(t.cen == 0, NA, (t.cen + 1) / 365), 3)
  Y.nl_rep <- round(ifelse(delta_NL == 1, NA, (Y_3 + 1) / 365), 3)
  Y.nt_rep <- round(ifelse(delta_NT == 1, NA, (Y_2 + 1) / 365), 3)

  TL_ob_n <- which(delta_OS == 1)
  TL_c_n <- which(delta_OS == 0)

  posterior_sample <- NULL
  attempt <- 1
  while (is.null(posterior_sample) && attempt <= 3) {
    attempt <- attempt + 1

    try({
      jag <- rjags::jags.model(file.path(jag.model.name),
        data = list(
          N = N,
          longest_survival = round(longest_survival / 365, 3),
          TL_ob_n = TL_ob_n,
          TL_c_n = TL_c_n,
          Npar_0 = Npar_0,
          Npar = Npar,
          X = X,
          Z_1 = Z_1,
          Z_2 = Z_2,
          t.os = round(t.os / 365, 3),
          t.os.wb = round(t.os / 365, 3),
          t.cen.lim = t.cen.lim,
          t.cen.lim.wb.os = t.cen.lim.wb.os,
          y.lim_t = y.lim_t,
          y.lim_t.os = y.lim_t.os,
          y.lim_nt.os = y.lim_nt.os,
          y.lim_nl.os = y.lim_nl.os,
          y.lim_wb = y.lim_wb,
          y.lim_wb.os = y.lim_wb.os,
          t = round(t / 365, 3),
          T = T,
          Y = Y,
          Y_os = round(Y_os / 365, 3),
          Y_2 = round(Y_2 / 365, 3),
          Y_3 = round(Y_3 / 365, 3),
          delta_NT = delta_NT,
          delta_NL = delta_NL,
          delta_OS = delta_OS,
          evl_times = evl_times,
          mg1 = mg1,
          seg1 = seg1,
          mg2 = mg2,
          seg2 = seg2,
          mg3 = mg3,
          seg3 = seg3,
          mg4 = mg4,
          seg4 = seg4,
          p = p
        ),
        inits <- function() {
          list(
            Y.os.nl_rep = ifelse(is.na(Y.os.nl_rep), 1 / 365, Y.os.nl_rep),
            Y.os.nt_rep = ifelse(is.na(Y.os.nt_rep), 1 / 365, Y.os.nt_rep),
            t.os = Y.os.t_rep,
            t.os.wb = Y.os.wb_rep,
            Y.os.t_rep = ifelse(is.na(Y.os.t_rep), 1 / 365, Y.os.t_rep),
            Y.os.wb_rep = ifelse(is.na(Y.os.wb_rep), 1 / 365, Y.os.wb_rep)
          )
        },
        n.chains = n_MCMC_chain, n.adapt = no.adapt
      )

      update(jag, BURN.IN)

      posterior_sample <- rjags::coda.samples(jag,
        c(
          "beta_0", "beta_1", "lambda", "alpha_os.1", "eta_2",
          "alpha_2", "beta_2", "alpha_os.2", "beta_os.2", "eta_3",
          "alpha_3", "beta_3", "alpha_os.3", "beta_os.3",
          "alpha_os.4", "beta_os.4",
          "Y.os.t_rep", "Y.os.nl_rep", "Y.os.nt_rep", "Y.os.wb_rep",
          "sigma_os.1", "sigma_os.2", "sigma_os.3",
          "sigma_b1", "sigma_b2", "rho",
          "Y.mix_rep", "w", "LL", "Pr", "g"
        ),
        n.iter = MCMC_SAMPLE, thin = thin
      )
    })
  }


  out_post <- summary(posterior_sample)
  assign(paste0("JM_median", ii), summary(posterior_sample)$quantiles[, 3])
  assign(paste0("JM_mean", ii), out_post$statistics[, 1])
  assign(paste0("JM_sample", ii), posterior_sample)




  ########### PFSOS #########

  jag.model.name <- "pfsos.bug"
  t.cen.lim <- round(cbind(ifelse(delta_OS == 1, 0, Y_os / 365), rep(longest_survival / 365, length(Y_os))), 3)
  t.cen.lim.pfs <- round(cbind(ifelse(delta_TTP == 1, 0, D_INT$time_TTP / 365), rep(longest_survival / 365, length(delta_TTP))), 3)
  y.lim_OS <- y.lim_OS.pfs <- rep(1, length(delta_OS))
  y.lim_PFS <- rep(1, length(delta_TTP))
  Y.os.pfs_rep <- round(ifelse(t.cen == 0, NA, (t.cen + 1) / 365), 3)


  posterior_sample <- NULL
  attempt <- 1
  while (is.null(posterior_sample) && attempt <= 3) {
    attempt <- attempt + 1

    try({
      jag <- rjags::jags.model(file.path(jag.model.name),
        data = list(
          N = N,
          longest_survival = round(longest_survival / 365, 3),
          Npar = Npar,
          Z_PFS = Z_2,
          t.cen.lim = t.cen.lim,
          y.lim_OS = y.lim_OS,
          Y_os = round(Y_os / 365, 3),
          Y_PFS = round(as.numeric(D_INT$time_TTP) / 365, 3),
          delta_PFS = delta_TTP,
          delta_OS = delta_OS
        ),
        inits <- function() list(Y.os.pfs_rep = ifelse(is.na(Y.os.pfs_rep), 1 / 365, Y.os.pfs_rep)),
        n.chains = n_MCMC_chain, n.adapt = no.adapt
      )

      update(jag, BURN.IN)

      posterior_sample <- rjags::coda.samples(jag,
        c(
          "eta_2",
          "alpha_2", "beta_2", "alpha_os.2", "sigma_os.2",
          "Y.os.pfs_rep"
        ),
        n.iter = MCMC_SAMPLE, thin = thin
      )
    })
  }


  out_post <- summary(posterior_sample)
  assign(paste0("copula_median", ii), summary(posterior_sample)$quantiles[, 3])
  assign(paste0("copula_mean", ii), out_post$statistics[, 1])
  assign(paste0("copula_sample", ii), posterior_sample)

  ########## weibull ##########


  # method 1

  data_s <- D_SURV
  data_s <- data_s[order(data_s$SID, data_s$EVL1N), ]
  data_s$TRT01PN <- as.factor(data_s$TRT01PN)

  data_s_id <- data_s

  # survival regression fit
  first_obs <- data_s_id %>%
    group_by(SID) %>%
    slice(head(row_number(), 1))

  ### survival model for OS ###

  jag.model.name <- "weibull.bug"
  t.cen.lim <- round(cbind(ifelse(delta_OS == 1, 0, Y_os / 365), rep(longest_survival / 365, length(delta_OS))), 3)
  y.lim_wb <- y.lim_wb.os <- rep(1, length(delta_OS))
  Y.os.wb_rep <- round(ifelse(t.cen == 0, NA, (t.cen + 2) / 365), 3)

  TL_ob_n <- which(delta_OS == 1)
  TL_c_n <- which(delta_OS == 0)

  tryCatch(
    {
      jag <- rjags::jags.model(file.path(jag.model.name),
        data = list(
          N = N,
          longest_survival = round(longest_survival / 365, 3),
          TL_ob_n = TL_ob_n,
          TL_c_n = TL_c_n,
          Npar = Npar,
          Z_2 = Z_2,
          t.os.wb = round(t.os / 365, 3),
          t.cen.lim = t.cen.lim
        ),
        inits <- function() {
          list(
            Y.os.wb_rep = ifelse(is.na(Y.os.wb_rep), round(1 / 365, 3), Y.os.wb_rep),
            t.os.wb = Y.os.wb_rep
          )
        },
        n.chains = n_MCMC_chain, n.adapt = no.adapt
      )

      update(jag, BURN.IN)

      posterior_sample <- rjags::coda.samples(jag,
        c(
          "alpha_os.4", "beta_os.4",
          "Y.os.wb_rep"
        ),
        n.iter = MCMC_SAMPLE, thin = thin
      )
    },
    warning = function(war) {},
    error = function(err) {},
    finally = {}
  )

  out_post <- summary(posterior_sample)
  assign(paste0("survival_median", ii), summary(posterior_sample)$quantiles[, 3])
  assign(paste0("survival_mean", ii), out_post$statistics[, 1])
  assign(paste0("survival_sample", ii), posterior_sample)


  ###### multi-state model ######

  X <- cbind(
    dat$trt, dat$hengbl, dat$sex, dat$age, dat$strati1,
    dat$nephrcbl
  )
  Z_1 <- cbind(rep(1, nrow(dat)), X)

  n_MCMC_chain <- 2
  MCMC_SAMPLE <- 30000
  no.adapt <- 8000
  BURN.IN <- 1000
  thin <- 10


  t.cen.lim <- round(cbind(ceiling(ifelse(is.na(dat$t), ifelse(dat$From.State == 1, dat$t1, dat$t2), 0)), ifelse(dat$From.State == 1, 1060, 950)) / 365, 3)
  delta_t <- rep(1, nrow(dat))
  N <- which((dat$From.State == 1 & dat$To.State2 == 1 & is.na(dat$t)) | (dat$From.State == 2 & dat$To.State3 == 1 & is.na(dat$t)))
  NN <- which((dat$From.State == 1 & dat$To.State3 == 1 & !is.na(dat$t)) | (dat$From.State == 2 & dat$To.State3 == 1 & !is.na(dat$t)))
  NNN <- which(dat$From.State == 1 & dat$To.State2 == 1 & !is.na(dat$t))

  dat.ind <- list(
    n = nrow(dat), Y = as.matrix(dat[, c("To.State1", "To.State2", "To.State3")]),
    from = dat$From.State, t = round(dat$t / 365, 3), Z_1 = Z_1, Npar = Npar,
    t.cen.lim = t.cen.lim
  )

  source("multi.state.R")

  inits <- list(
    list(
      R12 = 0.3, R13 = 0.2, R23 = 0.4,
      t = ifelse(is.na(dat$t), t.cen.lim[, 1] + 1 / 365, NA)
    ),
    list(
      R12 = 0.2, R13 = 0.1, R23 = 0.5,
      t = ifelse(is.na(dat$t), t.cen.lim[, 1] + 1 / 365, NA)
    )
  )

  ######### multi-state model ##########

  out.ind <- NULL
  attempt <- 1
  while (is.null(out.ind) && attempt <= 5) {
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
          "beta12", "beta13", "beta23",
          "A", "t"
        ),
        DIC = FALSE
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


### create benchmark dataset for evaluation purpose

dataset_1 <- dataset

if (!is.integer(dataset_1$SID[1])) {
  dataset_1$SID <- substring(dataset_1$SID, 15, 22)
}
SID <- as.numeric(dataset_1$SID)

dataset_1 <- as.data.frame(cbind(
  ID = as.numeric(SID), time = dataset_1$EVL1N, evl_time = dataset_1$EVL_TIME,
  mu = dataset_1$DMTSUM, time_PFS = dataset_1$TT_EVENT, delta_PFS = dataset_1$CS_EVENT,
  time_TTP = dataset_1$TT_TTP, delta_TTP = dataset_1$CS_TTP,
  time_NT = dataset_1$TT_NTL,
  delta_NT = dataset_1$CS_NTL, time_NL = dataset_1$TT_NL,
  delta_NL = dataset_1$CS_NL, time_OS = dataset_1$TT_OS, delta_OS = dataset_1$CS_OS,
  hengbl = dataset_1$HENGBL,
  sex = dataset_1$SEXN,
  age = dataset_1$AGEGR1N, strati1 = dataset_1$STRATI1N,
  nephrcbl = dataset_1$NEPHRCBL,
  trt = dataset_1$TRT01PN
))

dataset_1$hengbl <- ifelse(dataset_1$hengbl == "Favorable", 0, ifelse(is.na(dataset_1$hengbl), NA, 1))
dataset_1 <- dataset_1[order(dataset_1$ID, dataset_1$time), ]

dataset_1 <- dataset_1[!is.na(dataset_1$mu), ]

for (i in c(1:(ncol(dataset_1) - 3))) {
  dataset_1[, i] <- as.numeric(dataset_1[, i])
}

dataset_1$trt <- as.numeric(dataset_1$trt)
dataset_1$nephrcbl <- ifelse(dataset_1$nephrcbl == "Yes", 1, 0)

####### switch the censoring indicator ########

dataset_1$delta_NT <- 1 * (dataset_1$delta_NT == 0)
dataset_1$delta_NL <- 1 * (dataset_1$delta_NL == 0)
dataset_1$delta_OS <- 1 * (dataset_1$delta_OS == 0)
dataset_1$delta_PFS <- 1 * (dataset_1$delta_PFS == 0)
dataset_1$delta_TTP <- 1 * (dataset_1$delta_TTP == 0)

####### convert from unbalanced dataset_1 to balanced dataset_1 #######
dataset_1_sort <- dataset_1 %>%
  arrange(ID, time)

dataset_1 <- joineR::to.balanced(dataset_1_sort,
  id.col = "ID", time.col = "time",
  Y.col = c("mu", "evl_time"), other.col = (c(
    "time_NT", "delta_NT", "time_NL", "delta_NL", "time_OS", "delta_OS", "time_PFS", "delta_PFS",
    "hengbl", "sex", "age", "strati1",
    "nephrcbl", "trt"
  ))
)

dataset_1$time_OS <- as.numeric(dataset_1$time_OS)
dataset_1$delta_OS <- as.numeric(dataset_1$delta_OS)


# evaluation


for (kk in cutoff_i) {
  data_e <- get(paste0("D_INT", kk))
  data_e$time_OS <- as.numeric(data_e$time_OS)
  data_e$delta_OS <- as.numeric(data_e$delta_OS)
  data_pred_JM <- get(paste0("JM_mean", kk))
  data_pred_copula <- get(paste0("copula_mean", kk))
  data_pred_survival <- get(paste0("survival_mean", kk))
  data_pred_MS <- get(paste0("MS_mean", kk))
  dataset_1 <- subset(dataset_1, ID %in% data_e$ID)
  total_death <- ifelse(nrow(dataset_1) < total_death, nrow(dataset_1), total_death)
  true_OS <- dataset_1$time_OS[which(data_e$delta_OS != dataset_1$delta_OS)]
  true_OS_dates <- sort(true_OS + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)])[total_death - cutoff[kk] - result_correct[kk]]
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
  pred_OS_JM <- data_pred_JM[pred_col_JM][which(data_e$delta_OS != dataset_1$delta_OS)]
  pred_OS_JM_dates <- sort(round((pred_OS_JM * 365)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)])[total_death - cutoff[kk] - result_correct[kk]]
  enroll_OS_JM_dates <- (round((pred_OS_JM * 365)) + first_obs$RND1O[which(data_e$delta_OS != dataset_1$delta_OS)] - round((pred_OS_JM * 365)))[names(pred_OS_JM_dates)]
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
  MS_quantile <- HDInterval::hdi(MS_sample[, c((ncol(MS_sample) - nrow(dat_MS) + 1):ncol(MS_sample))], 0.9)[, MS_ID][, as.numeric(names(pred_OS_MS_dates))] +
    tail(get(paste0("trun_t", kk)), nrow(dat_MS))[MS_ID][as.numeric(names(pred_OS_MS_dates))]
  library(readr)
  key_number <- parse_number(colnames(MS_sample)[c((ncol(MS_sample) - nrow(dat_MS) + 1):ncol(MS_sample))][MS_ID][as.numeric(names(pred_OS_MS_dates))])
  if (key_number %in% location_rep) {
    MS_quantile <- MS_quantile + HDInterval::hdi(MS_sample[, paste0("t[", location_rep[which(location_rep == key_number, arr.ind = TRUE)[1], 1], "]")], 0.9)
  }
  assign(paste0("MS_HDI_final", kk), MS_quantile * 365)
  assign(paste0("MS_point", kk), max(pred_OS_MS_dates))
  assign(paste0("MS_enroll", kk), enroll_OS_MS_dates)


  result <- data.frame(
    true_OS_dates,
    get(paste0("JM_point", kk)),
    get(paste0("JM_HDI_final", kk))[1], get(paste0("JM_HDI_final", kk))[2],
    get(paste0("JM_enroll", kk)),
    get(paste0("copula_point", kk)),
    get(paste0("copula_HDI_final", kk))[1], get(paste0("copula_HDI_final", kk))[2],
    get(paste0("copula_enroll", kk)),
    get(paste0("survival_point", kk)),
    get(paste0("survival_HDI_final", kk))[1], get(paste0("survival_HDI_final", kk))[2],
    get(paste0("survival_enroll", kk)),
    get(paste0("MS_point", kk)),
    get(paste0("MS_HDI_final", kk))[1], get(paste0("MS_HDI_final", kk))[2],
    get(paste0("MS_enroll", kk))
  )

  colnames(result) <- c(
    "true_OS",
    "JM", "JM_low", "JM_high", "JM_enroll",
    "copula", "copula_low", "copula_high", "copula_enroll",
    "survival", "survival_low", "survival_high", "survival_enroll",
    "MS", "MS_low", "MS_high", "MS_enroll"
  )

  JM_median <- get(paste0("JM_median", kk))
  JM_mean <- get(paste0("JM_mean", kk))
  JM_sample <- get(paste0("JM_sample", kk))
  copula_median <- get(paste0("copula_median", kk))
  copula_mean <- get(paste0("copula_mean", kk))
  copula_sample <- get(paste0("copula_sample", kk))
  survival_median <- get(paste0("survival_median", kk))
  survival_mean <- get(paste0("survival_mean", kk))
  survival_sample <- get(paste0("survival_sample", kk))
  MS_median <- get(paste0("MS_median", kk))
  MS_mean <- get(paste0("MS_mean", kk))
  MS_sample <- get(paste0("MS_sample", kk))
  MS_R2jags <- get(paste0("MS_R2jags", kk))
}
