#!/usr/bin/env Rscript
setwd("~/simulation_code")
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

for (simulation_i in sample_i) {
  dataset <- datalist_i[[simulation_i]]
  dataset$DMTSUM <- sqrt(dataset$DMTSUM)
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
  dataset$TT_EVENT <- pmin(dataset$TT_NL, dataset$TT_OS, dataset$TT_TL)
  dataset$CS_EVENT <- 0
  dataset$TT_TTP <- pmin(dataset$TT_NL, dataset$TT_TL)
  dataset$CS_TTP <- ifelse(dataset$CS_TL + dataset$CS_NL == 2, 1, 0)

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
        "time_OS", "delta_OS", "time_PFS", "delta_PFS", "time_TTP", "delta_TTP",
        "time_TTP", "delta_TTP", "trt"
      ))
    )

    assign(paste0("D_INT", ii), D_INT)

    for (i in c((ncol(D_INT) - 11):(ncol(D_INT)))) {
      D_INT[, i] <- as.numeric(D_INT[, i])
    }

    N <- nrow(D_INT)
    Npar_0 <- 1
    Npar <- 2
    delta_NL <- D_INT$delta_NL
    delta_OS <- D_INT$delta_OS
    delta_PFS <- as.numeric(D_INT$delta_PFS)
    delta_TTP <- D_INT$delta_TTP
    time_PFS <- D_INT$time_PFS
    time_OS <- D_INT$time_OS
    time_TTP <- D_INT$time_TTP
    Y_os <- D_INT$time_OS
    Y_3 <- D_INT$time_NL
    X <- cbind(D_INT$trt) - 1
    Z_1 <- cbind(rep(1, N), X)
    Z_2 <- Z_1
    mu_colno <- str_detect(colnames(D_INT), "mu.t")
    Y <- D_INT[mu_colno]

    evl_time_colno <- str_detect(colnames(D_INT), "evl_time.t")
    t <- D_INT[evl_time_colno]


    evl_times <- ncol(Y)
    t.os <- numeric(N)
    t.cen <- numeric(N)
    T <- numeric(N)
    T1 <- numeric(N)
    p <- numeric(3)

    p[1] <- Npar_0 + 7 + Npar
    p[2] <- Npar + 2
    p[3] <- Npar + 1



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
    Y_os <- as.numeric(Y_os)
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
    MCMC_SAMPLE <- 50000
    thin <- 15
    longest_survival <- Inf

    ######### joint model ##########

    jag.model.name <- "ospred_get_posterior_4JM.bug"
    t.cen.lim.wb.os <- round(cbind(ifelse(delta_OS == 1, 0, Y_os), rep(longest_survival, length(delta_OS))) / 365, 3)
    t.cen.lim <- round(cbind(ifelse(delta_OS == 1, 0, Y_os), rep(longest_survival, length(delta_OS))) / 365, 3)
    t.cen.lim.nl <- round(cbind(ifelse(delta_NL == 1, 0, Y_3), rep(longest_survival, length(delta_NL))) / 365, 3)
    y.lim_t <- y.lim_t.os <- rep(1, length(delta_OS))
    y.lim_wb <- y.lim_wb.os <- rep(1, length(delta_OS))
    y.lim_nl.os <- rep(1, length(delta_OS))
    y.lim_nl <- rep(1, length(delta_NL))
    Y.os.t_rep <- Y.os.nl_rep <- Y.os.wb_rep <- round(ifelse(t.cen == 0, NA, t.cen + 1) / 365, 3)
    Y.nl_rep <- round(ifelse(delta_NL == 1, NA, Y_3 + 1) / 365, 3)

    TL_ob_n <- which(delta_OS == 1)
    TL_c_n <- which(delta_OS == 0)

    posterior_sample <- NULL
    attempt <- 1
    while (is.null(posterior_sample) && attempt <= 10) {
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
            t = round(t / 365, 3),
            T = T,
            Y = Y,
            Y_os = round(Y_os / 365, 3),
            Y_3 = round(Y_3 / 365, 3),
            delta_NL = delta_NL,
            delta_OS = delta_OS,
            evl_times = evl_times
          ),
          inits <- function() {
            list(
              Y.os.nl_rep = ifelse(is.na(Y.os.nl_rep), 1, Y.os.nl_rep),
              Y.os.t_rep = ifelse(is.na(Y.os.t_rep), 1, Y.os.t_rep),
              Y.os.wb_rep = ifelse(is.na(Y.os.wb_rep), 1, Y.os.wb_rep),
              t.os = Y.os.t_rep,
              t.os.wb = Y.os.wb_rep,
              alpha_os.3 = 4,
              alpha_os.1 = 5,
              alpha_os.4 = 4,
              eta_3 = 1,
              beta_0 = c(7, -0.5),
              beta_1 = c(-20, -0.8),
              lambda = 0.5,
              alpha_3 = 4,
              beta_3 = c(-7, -0.3),
              beta_os.4 = c(-7, -0.5),
              beta_os.3 = c(-15, -1.5),
              sigma_os.1 = 0.85,
              sigma_os.3 = 1.29,
              rho = -0.14,
              sigma_b1 = 6,
              sigma_b2 = 1
            )
          },
          n.chains = n_MCMC_chain, n.adapt = no.adapt
        )
        update(jag, BURN.IN)

        posterior_sample <- rjags::coda.samples(jag,
          c(
            "beta_0", "beta_1", "lambda", "alpha_os.1",
            "eta_3", "alpha_3", "beta_3", "alpha_os.3", "beta_os.3",
            "alpha_os.4", "beta_os.4",
            "Y.os.t_rep", "Y.os.wb_rep", "Y.os.nl_rep",
            "sigma_os.1", "sigma_os.3",
            "sigma_b1", "sigma_b2", "rho"
          ),
          n.iter = MCMC_SAMPLE, thin = thin
        )
      })
    }

    out_post <- summary(posterior_sample)

    # beta_0, beta_1, alpha_os.1, lambda, sigma_b1, sigma_b2, rho, sigma_os.1
    mg1 <- out_post$statistics[c(
      "beta_0[1]", "beta_0[2]", "beta_1[1]", "beta_1[2]", "alpha_os.1",
      "lambda", "sigma_b1", "sigma_b2", "rho", "sigma_os.1"
    ), 1]
    seg1 <- out_post$statistics[c(
      "beta_0[1]", "beta_0[2]", "beta_1[1]", "beta_1[2]", "alpha_os.1",
      "lambda", "sigma_b1", "sigma_b2", "rho", "sigma_os.1"
    ), 2]


    # beta_os.3, alpha_os.3, sigma_os.3
    mg3 <- out_post$statistics[c("beta_os.3[1]", "beta_os.3[2]", "alpha_os.3", "sigma_os.3"), 1]
    seg3 <- out_post$statistics[c("beta_os.3[1]", "beta_os.3[2]", "alpha_os.3", "sigma_os.3"), 2]

    # beta_os.4, alpha_os.4, sigma_os.4
    mg4 <- out_post$statistics[c("beta_os.4[1]", "beta_os.4[2]", "alpha_os.4"), 1]
    seg4 <- out_post$statistics[c("beta_os.4[1]", "beta_os.4[2]", "alpha_os.4"), 2]




    jag.model.name <- "ospred_4JM.bug"
    t.cen.lim.wb.os <- round(cbind(ifelse(delta_OS == 1, 0, Y_os), rep(longest_survival, length(delta_OS))) / 365, 3)
    t.cen.lim <- round(cbind(ifelse(delta_OS == 1, 0, Y_os), rep(longest_survival, length(delta_OS))) / 365, 3)
    t.cen.lim.nl <- round(cbind(ifelse(delta_NL == 1, 0, Y_3), rep(longest_survival, length(delta_NL))) / 365, 3)
    y.lim_t <- y.lim_t.os <- rep(1, length(delta_OS))
    y.lim_wb <- y.lim_wb.os <- rep(1, length(delta_OS))
    y.lim_nl.os <- rep(1, length(delta_OS))
    y.lim_nl <- rep(1, length(delta_NL))
    Y.os.t_rep <- Y.os.nl_rep <- Y.os.wb_rep <- round(ifelse(t.cen == 0, NA, t.cen + 1) / 365, 3)
    Y.nl_rep <- round(ifelse(delta_NL == 1, NA, Y_3 + 1) / 365, 3)
    Y.os.t_rep <- Y.os.nl_rep <- Y.os.wb_rep <- round(ifelse(t.cen == 0, NA, t.cen + 1) / 365, 3)

    TL_ob_n <- which(delta_OS == 1)
    TL_c_n <- which(delta_OS == 0)

    posterior_sample <- NULL
    attempt <- 1
    while (is.null(posterior_sample) && attempt <= 10) {
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
            t = round(t / 365, 3),
            T = T,
            Y = Y,
            Y_os = round(Y_os / 365, 3),
            Y_3 = round(Y_3 / 365, 3),
            delta_NL = delta_NL,
            delta_OS = delta_OS,
            evl_times = evl_times,
            mg1 = mg1,
            seg1 = seg1,
            mg3 = mg3,
            seg3 = seg3,
            mg4 = mg4,
            seg4 = seg4,
            p = p
          ),
          inits <- function() {
            list(
              Y.os.nl_rep = ifelse(is.na(Y.os.nl_rep), 1, Y.os.nl_rep),
              Y.os.t_rep = ifelse(is.na(Y.os.t_rep), 1, Y.os.t_rep),
              Y.os.wb_rep = ifelse(is.na(Y.os.wb_rep), 1, Y.os.wb_rep),
              t.os = Y.os.t_rep,
              t.os.wb = Y.os.wb_rep,
              alpha_os.3 = 4,
              alpha_os.1 = 5,
              alpha_os.4 = 4,
              eta_3 = 1,
              beta_0 = c(7, -0.5),
              beta_1 = c(-20, -0.8),
              lambda = 0.5,
              alpha_3 = 4,
              beta_3 = c(-7, -0.3),
              beta_os.4 = c(-7, -0.5),
              beta_os.3 = c(-15, -1.5),
              sigma_os.1 = 0.85,
              sigma_os.3 = 1.29,
              rho = -0.14,
              sigma_b1 = 6,
              sigma_b2 = 1
            )
          },
          n.chains = n_MCMC_chain, n.adapt = no.adapt
        )

        update(jag, BURN.IN)

        posterior_sample <- rjags::coda.samples(jag,
          c(
            "beta_0", "beta_1", "lambda", "alpha_os.1", "eta_3",
            "alpha_3", "beta_3", "alpha_os.3", "beta_os.3",
            "alpha_os.4", "beta_os.4",
            "Y.os.t_rep", "Y.os.wb_rep", "Y.os.nl_rep",
            "sigma_os.1", "sigma_os.3",
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
    t.cen.lim <- round(cbind(ifelse(delta_OS == 1, 0, Y_os), rep(longest_survival, length(Y_os))) / 365, 3)
    t.cen.lim.pfs <- round(cbind(ifelse(delta_TTP == 1, 0, D_INT$time_TTP), rep(longest_survival, length(delta_TTP))) / 365, 3)
    y.lim_OS <- y.lim_OS.pfs <- rep(1, length(delta_OS))
    y.lim_PFS <- rep(1, length(delta_TTP))
    Y.os.pfs_rep <- round(ifelse(t.cen == 0, NA, t.cen + 1) / 365, 3)


    posterior_sample <- NULL
    attempt <- 1
    while (is.null(posterior_sample) && attempt <= 10) {
      attempt <- attempt + 1

      try({
        jag <- rjags::jags.model(file.path(jag.model.name),
          data = list(
            N = N,
            longest_survival = round(longest_survival / 365, 3),
            Npar = Npar,
            Z_PFS = Z_2,
            Y_os = round(Y_os / 365, 3),
            t.cen.lim = t.cen.lim,
            y.lim_OS = y.lim_OS,
            Y_PFS = round(as.numeric(D_INT$time_TTP) / 365, 3), # Y_3,
            delta_PFS = delta_TTP,
            delta_OS = delta_OS
          ),
          inits <- function() {
            list(
              Y.os.pfs_rep = ifelse(is.na(Y.os.pfs_rep), 1, Y.os.pfs_rep),
              alpha_os.2 = 5,
              eta_2 = 0.5,
              beta_2 = c(-5, 0.25),
              beta_os.2 = c(-15, 1.75)
            )
          },
          n.chains = n_MCMC_chain, n.adapt = no.adapt
        )
        update(jag, BURN.IN)

        posterior_sample <- rjags::coda.samples(jag,
          c(
            "eta_2",
            "beta_2", "alpha_os.2", "beta_os.2",
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

    jag.model.name <- "weibull.bug"
    t.cen.lim <- round(cbind(ifelse(delta_OS == 1, 0, Y_os), rep(longest_survival, length(delta_OS))) / 365, 3)
    y.lim_wb <- y.lim_wb.os <- rep(1, length(delta_OS))
    y.lim_wb.os <- rep(1, length(delta_OS))
    Y.os.wb_rep <- round(ifelse(t.cen == 0, NA, t.cen + 1) / 365, 3)

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
            t.cen.lim = t.cen.lim # ,
          ),
          inits <- function() {
            list(
              Y.os.wb_rep = ifelse(is.na(Y.os.wb_rep), 1, Y.os.wb_rep),
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
    data_pred_JM <- get(paste0("JM_mean", kk))
    data_pred_copula <- get(paste0("copula_mean", kk))
    data_pred_survival <- get(paste0("survival_mean", kk))
    true_OS <- dataset_1$time_OS[which(data_e$delta_OS != dataset_1$delta_OS)]
    assign(paste0("true_OS_dates", kk), true_OS)
    true_trt <- dataset_1$trt[which(data_e$delta_OS != dataset_1$delta_OS)]
    pred_col_JM <- str_detect(names(data_pred_JM), "Y.mix_rep")
    pred_OS_JM <- data_pred_JM[pred_col_JM][which(data_e$delta_OS != dataset_1$delta_OS)] * 365

    JM_quantile <- coda::HPDinterval(get(paste0("JM_sample", kk)), prob = 0.9)[[1]][names(which.max(pred_OS_JM)), ]
    assign(paste0("JM_HDI_final", kk), JM_quantile * 365)
    assign(paste0("JM_point", kk), max(pred_OS_JM))


    pred_col_copula <- str_detect(names(data_pred_copula), "Y.os.pfs_rep")
    pred_OS_copula <- data_pred_copula[pred_col_copula][which(data_e$delta_OS != dataset_1$delta_OS)] * 365


    copula_quantile <- coda::HPDinterval(get(paste0("copula_sample", kk)), prob = 0.9)[[1]][names(which.max(pred_OS_copula)), ]
    assign(paste0("copula_HDI_final", kk), copula_quantile * 365)
    assign(paste0("copula_point", kk), max(pred_OS_copula))

    pred_col_survival <- str_detect(names(data_pred_survival), "Y.os.wb_rep")
    pred_OS_survival <- data_pred_survival[pred_col_survival][which(data_e$delta_OS != dataset_1$delta_OS)] * 365

    survival_quantile <- coda::HPDinterval(get(paste0("survival_sample", kk)), prob = 0.9)[[1]][names(which.max(pred_OS_survival)), ]
    assign(paste0("survival_HDI_final", kk), survival_quantile * 365)
    assign(paste0("survival_point", kk), max(pred_OS_survival))

    result_tmp_mean <- c(
      max(true_OS),
      get(paste0("JM_point", kk)),
      get(paste0("JM_HDI_final", kk))[1], get(paste0("JM_HDI_final", kk))[2],
      get(paste0("copula_point", kk)),
      get(paste0("copula_HDI_final", kk))[1], get(paste0("copula_HDI_final", kk))[2],
      get(paste0("survival_point", kk)),
      get(paste0("survival_HDI_final", kk))[1], get(paste0("survival_HDI_final", kk))[2],
      data_pred_JM[c("w[1]", "w[2]", "w[3]")]
    )
    print(result_tmp_mean)
  }


  for (kk in cutoff_i) {
    data_e <- get(paste0("D_INT", kk))
    data_e$time_OS <- as.numeric(data_e$time_OS)
    data_e$delta_OS <- as.numeric(data_e$delta_OS)
    data_pred_JM <- get(paste0("JM_median", kk))
    data_pred_copula <- get(paste0("copula_median", kk))
    data_pred_survival <- get(paste0("survival_median", kk))
    true_OS <- dataset_1$time_OS[which(data_e$delta_OS != dataset_1$delta_OS)]
    assign(paste0("true_OS_dates", kk), true_OS)
    true_trt <- dataset_1$trt[which(data_e$delta_OS != dataset_1$delta_OS)]
    pred_col_JM <- str_detect(names(data_pred_JM), "Y.mix_rep")
    pred_OS_JM <- data_pred_JM[pred_col_JM][which(data_e$delta_OS != dataset_1$delta_OS)] * 365

    JM_quantile <- coda::HPDinterval(get(paste0("JM_sample", kk)), prob = 0.9)[[1]][names(which.max(pred_OS_JM)), ]
    assign(paste0("JM_HDI_final", kk), JM_quantile * 365)
    assign(paste0("JM_point", kk), max(pred_OS_JM))

    pred_col_copula <- str_detect(names(data_pred_copula), "Y.os.pfs_rep")
    pred_OS_copula <- data_pred_copula[pred_col_copula][which(data_e$delta_OS != dataset_1$delta_OS)] * 365


    copula_quantile <- coda::HPDinterval(get(paste0("copula_sample", kk)), prob = 0.9)[[1]][names(which.max(pred_OS_copula)), ]
    assign(paste0("copula_HDI_final", kk), copula_quantile * 365)
    assign(paste0("copula_point", kk), max(pred_OS_copula))


    pred_col_survival <- str_detect(names(data_pred_survival), "Y.os.wb_rep")
    pred_OS_survival <- data_pred_survival[pred_col_survival][which(data_e$delta_OS != dataset_1$delta_OS)] * 365

    survival_quantile <- coda::HPDinterval(get(paste0("survival_sample", kk)), prob = 0.9)[[1]][names(which.max(pred_OS_survival)), ]
    assign(paste0("survival_HDI_final", kk), survival_quantile * 365)
    assign(paste0("survival_point", kk), max(pred_OS_survival))


    result_tmp_median <- c(
      max(true_OS),
      get(paste0("JM_point", kk)),
      get(paste0("JM_HDI_final", kk))[1], get(paste0("JM_HDI_final", kk))[2],
      get(paste0("copula_point", kk)),
      get(paste0("copula_HDI_final", kk))[1], get(paste0("copula_HDI_final", kk))[2],
      get(paste0("survival_point", kk)),
      get(paste0("survival_HDI_final", kk))[1], get(paste0("survival_HDI_final", kk))[2],
      data_pred_JM[c("w[1]", "w[2]", "w[3]")]
    )
    print(result_tmp_median)
  }

  result_mean <- rbind(result_mean, result_tmp_mean)
  colnames(result_mean) <- c("true_OS", "JM", "JM_low", "JM_high", "copula", "copula_low", "copula_high", "survival", "survival_low", "survival_high", "w[1]", "w[2]", "w[3]")

  result_median <- rbind(result_median, result_tmp_median)
  colnames(result_median) <- c("true_OS", "JM", "JM_low", "JM_high", "copula", "copula_low", "copula_high", "survival", "survival_low", "survival_high", "w[1]", "w[2]", "w[3]")
}
save(result_mean, result_median, file = paste0(args[[1]], "_", cutoff[cutoff_i], "_", sample_i, ".RData"))
