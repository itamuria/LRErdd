
#' Calculate de regression discontinuity p-value
#'
#' @param dataset The dataset with the variables
#' @param covariate One or more covariate
#' @param CT control and treatment variable
#' @param Nsim Number of simulations
#' @param low_lim lowest limit of each of the covariate
#' @param high_lim lowest limit of each of the covariate
#' @param cutvar number to divide the treatment
#' @return pvalues and other properties
#' @export

rd_pvalue <- function(dataset = last, covariate = "AGE", CT = vec, Nsim = 1000, cutvar = 30, low_lim = NULL, high_lim = NULL) {
    
    # if(!is.null(low_lim) & is.null(high_lim)) { dataset <- } else if((is.null(low_lim)*1+is.null(high_lim)*1)==1) { }
    
    # ds.ct <- dataset[,CT]
    
    ds.ct <- CT
    
    # only with one covariate
    if (length(covariate) == 1) {
        ds <- dataset[, covariate]
        
        X.bart <- mean(ds[ds.ct == 1], na.rm = TRUE)
        X.barc <- mean(ds[ds.ct == 0], na.rm = TRUE)
        
        # X.bart-X.barc
        
        T.obs <- abs(X.bart - X.barc)
        T.obs2 <- (X.bart - X.barc)
        
        # Nsim<-10000
        p.dif <- 0
        
        for (k in 1:Nsim) {
            # print(k)
            Wrep <- sample(ds.ct, replace = FALSE)
            Trep <- abs(mean(ds[Wrep == 1], na.rm = TRUE) - mean(ds[Wrep == 0], na.rm = TRUE))
            # print(Trep) print(T.obs)
            p.dif <- p.dif + 1 * (Trep >= T.obs)
        }
        p.dif
        
        pvalue <- round(p.dif/Nsim, 4)
        pvalue
    }
    
    lista <- list(pvalue, len = dim(dataset)[1], covariate, T.obs2)
    
    # with more than one covariate else if (length(covariate)>1) { }
    
    return(lista)
    # MAB #cases, bandwidth, variable names, mean diference, p-value (taula osoa 4 baliorekin var bakoitzeko)
    
}



#' Filtering the datasaet with ranges
#'
#' @param dataset The dataset with the variables
#' @param covariate One or more covariate
#' @param CT control and treatment variable
#' @param Nsim Number of simulations
#' @param low_lim lowest limit of each of the covariate
#' @param high_lim lowest limit of each of the covariate
#' @param cutvar number to divide the treatment
#' @return pvalues and other properties
#' @export

rd_filtering <- function(dataset = last, covariate = "AGE", num_out = 4, CT = "W", Nsim = 1000, cutvar = 30, low_lim = NULL, high_lim = NULL) {
    r1 <- dataset$BMI
    min.r <- min(r1)
    max.r <- max(r1)
    r1.m <- r1[r1 < cutvar]
    r1.M <- r1[r1 > cutvar]
    
    pr <- seq(min.r, cutvar, length = 5)/min.r - 1
    
    qrm <- quantile(r1, prob = pr)[-(num_out + 1)]
    
    pr2 <- (seq(cutvar, max.r, length = 5))
    
    qrM <- quantile(r1, prob = pr2)[-(num_out + 1)]
}


#' Fisher p-values adjusted
#'
#' @param dataset The dataset with the variables
#' @param forcing_bin_var_name forcing_bin_var_name
#' @param forcing_var_name forcing_var_name
#' @param Y_name Y_name
#' @param niter niter
#' @param bandwidth bandwidth
#' @param cut_value cut_value
#' @return data frame with variable and value and bandwidth
#' @export
#'
rand_pajd <- function(dataset = data, forcing_var_name = "S", covariates = c("sex", "HSHumanity", "HSTech", "HSOther", "hsgrade", "Y2004"), niter = 1000, bandwidth = 1000, cut_value = 15000, whichunder = 1) {
    
    s0 <- cut_value
    S <- dataset[, forcing_var_name]  #Forcing variable
    
    if (whichunder == 0) {
        Z <- ifelse(S >= s0, 0, 1)
    } else if (whichunder == 1) {
        Z <- ifelse(S >= s0, 1, 0)
    }
    
    print(covariates)
    print(dim(dataset))
    
    
    X <- data.frame(dataset[, covariates])
    # if(length(covariates)==1) names(X) <- covariates
    
    print(dim(X))
    
    h <- bandwidth
    ### NOT RUN: IT TAKES A WHILE
    K <- niter
    
    # We can using, e.g. K=100 K<-100
    M <- K
    
    dataset.h <- dataset[S >= s0 - h & S <= s0 + h, ]
    print(dim(dataset.h))
    
    Nh <- nrow(dataset.h)
    
    
    Sh <- dataset.h[, forcing_var_name]
    

    
    if (whichunder == 0) {
        Zh <- ifelse(Sh >= s0, 0, 1)
    } else if (whichunder == 1) {
        Zh <- ifelse(Sh >= s0, 1, 0)
    }
    
    Xh <- data.frame(dataset.h[, covariates])
    # if(length(covariates)==1) names(Xh) <- covariates
    
    Th.obs <- apply(Xh, 2, Tave, Zh)
    p.values.obs <- matrix(0, K, ncol(Xh))
    Th.HYP <- matrix(0, K, ncol(Xh))
    
    for (j in 1:K) {
        Zh.hyp <- sample(Zh, Nh, replace = TRUE)
        Th.hyp <- apply(Xh, 2, Tave, Zh.hyp)
        p.values.obs[j, ] <- as.numeric(Th.hyp >= Th.obs)
        Th.HYP[j, ] <- Th.hyp
        rm(Zh.hyp, Th.hyp)
    }
    Pvalues.obs <- apply(p.values.obs, 2, mean)
    
    print(paste0("Observerd: ",Pvalues.obs))
    
    Pvalues.hyp.obs <- rep(0, M)
    Adj.pvalues <- matrix(0, M, ncol(Xh))
    
    for (j in 1:M) {
        Th.hyp.obs <- matrix(Th.HYP[j, ], (K - 1), ncol(Xh), byrow = T)
        Pvalues.hyp <- apply(Th.hyp.obs >= Th.HYP[-j, ], 2, mean)
        Pvalues.hyp.obs[j] <- min(Pvalues.hyp)
        rm(Pvalues.hyp)
        Adj.pvalues[j, ] <- 1 * (Pvalues.hyp.obs[j] <= Pvalues.obs)
    }
    
    adj.pvalues <- apply(Adj.pvalues, 2, mean)
    
    print(paste0("Ajustado: ",adj.pvalues))
    
    names(adj.pvalues) <- names(X)
    dff <- data.frame(names(adj.pvalues), Pvalues.obs, adj.pvalues)
    names(dff)[1] <- c("Variables")
    rownames(dff) <- NULL
    
    return(dff)
    
}  # function end




#' Fisher p-values adjusted bw
#'
#' @param dataset The dataset with the variables
#' @param forcing_bin_var_name forcing_bin_var_name
#' @param forcing_var_name forcing_var_name
#' @param Y_name Y_name
#' @param niter niter
#' @param bandwidth bandwidth
#' @param cut_value cut_value
#' @return data frame with variable and value and bandwidth
#' @export
#'
rand_pajd_bw <- function(dataset = data, forcing_var_name = "S", covariates = c("sex", "HSHumanity", "HSTech", "HSOther", "hsgrade", "Y2004"), niter = 1000, bandwidth = c(500, 1000, 5000), cut_value = 15000, 
    whichunder = 1) {
    s0 <- cut_value
    S <- dataset[, forcing_var_name]  #Forcing variable
    
    if (whichunder == 0) {
        Z <- ifelse(S >= s0, 0, 1)
    } else if (whichunder == 1) {
        Z <- ifelse(S >= s0, 1, 0)
    }
    
    print(covariates)
    print(dim(dataset))
    
    X <- dataset[, covariates]
    
    print(dim(X))
    
    lenbw <- length(bandwidth)
    ss2 <- NULL
    namesbuffer <- NULL
    
    for (h in 1:lenbw) {
        
        ss <- rand_pajd(dataset = dataset, forcing_var_name = forcing_var_name, covariates = covariates, niter = niter, bandwidth = bandwidth[h], cut_value = cut_value, whichunder = whichunder)
        ss <- as.vector(ss[, 3])
        ss2 <- c(ss2, ss)
        namesbuffer <- c(namesbuffer, paste0("p-value[buf=", bandwidth[h], "]"))
    }
    
    df <- data.frame(matrix(ss2, length(covariates), lenbw))
    df2 <- data.frame(covariates, df)
    names(df2) <- c("Names", namesbuffer)
    return(df2)
}



#' Calculating Fisher Exact p-value
#'
#' @param dataset The dataset with the variables
#' @param forcing_bin_var_name forcing_bin_var_name
#' @param Y_name Y_name
#' @param niter niter
#' @return data frame with variable and value
#' @export
#'
sharp_fep <- function(dataset, forcing_bin_var_name = "Z", Y_name = "dropout", niter = 100) {
  print(paste0("forcing_bin_var_name-",forcing_bin_var_name))
    M <- niter
    print(paste0("M-",M))
    Y <- as.numeric(dataset[, Y_name])
    print(summary(Y))
    print(head(paste0(Y)))
    Z <- dataset[, forcing_bin_var_name]
    print(summary(Z))
    print(head(paste0(Z)))
    
    m1 <- mean(Y[dataset[, forcing_bin_var_name] == 1])
    print(paste0("m1-",m1))
    m0 <- mean(Y[dataset[, forcing_bin_var_name] == 0])
    print(paste0("m0-",m0))
    tave <- m1 - m0
    print(paste0("tave-",tave))
    tobs <- abs(tave)
    print(paste0("tobs-",tobs))
    Nh <- length(Y)
    
    
    p.value <- 0
    for (m in 1:M) {
        Z.m <- sample(Z, Nh, replace = TRUE)
        thyp.m <- abs(mean(Y[Z.m == 1]) - mean(Y[Z.m == 0]))
        p.value <- p.value + as.numeric(thyp.m >= tobs)
        # print(m)
    }
    p.value <- p.value/M
    
    # Results - Fisher Exact P-value for h0: Yi(0)=Yi(1)
    res1 <- c(Nh, tave, tobs, p.value)
    res1 <- data.frame(res1)
    res1 <- data.frame(c("Nh", "tave", "tobs", "p.value"), res1)
    names(res1) <- c("Variable", "Value")
    return(res1)
}



#' Fisher p-values
#'
#' @param dataset The dataset with the variables
#' @param forcing_bin_var_name forcing_bin_var_name
#' @param forcing_var_name forcing_var_name
#' @param Y_name Y_name
#' @param niter niter
#' @param bandwidth bandwidth
#' @param cut_value cut_value
#' @return data frame with variable and value and bandwidth
#' @export
#'
sharp_fep_bw <- function(dataset = data, forcing_var_name = "S", Y_name = "dropout", niter = 1000, bandwidth = c(500, 1000, 1500), cut_value = 15000, whichunder = 1) {
    # for each bandwidth
    len_bw <- length(bandwidth)
    
    # Cut value
    s0 <- cut_value
    if (whichunder == 1) {
        dataset$assigVar <- ifelse(dataset[, forcing_var_name] <= cut_value, 1, 0)
    } else if (whichunder == 0) {
        dataset$assigVar <- ifelse(dataset[, forcing_var_name] <= cut_value, 0, 1)
    }
    
    Sh <- dataset[, forcing_var_name]
    pbalioak <- c()
    nak <- c()
    
    for (b in 1:len_bw) {
        
        # bandwidth
        h <- bandwidth[b]
        
        # Filter dataste with bw
        dat_bw <- dataset[Sh >= s0 - h & Sh <= s0 + h, ]
        
        # number of records N
        N <- dim(dat_bw)[1]
        print(N)
        
        ft <- sharp_fep(dataset = dat_bw, forcing_bin_var_name = "assigVar", Y_name = Y_name, niter = 1000)
        pbalioak <- c(pbalioak, ft[, 2])
        
    }  # for b
    
    dfp <- data.frame(matrix(pbalioak, ncol = 4, byrow = T))
    dfp[, 2:3] <- round(dfp[, 2:3], 4)
    
    df <- cbind(bandwidth, dfp)
    names(df) <- c("Bandwidth", "N", "Difference in average outcomes by treatment status
Statistic", "Absolute value of difference in average outcomes", "p-value")
    return(df)
    
}  # function end




#' SHARP RDD: RANDOMIZATION-BASED INFERENCE - NEYMAN APPROACH
#'
#' @param Y Outcome variable
#' @param Z forcing binary variable
#' @param cin Confidence interval
#' @return a vector with values
#' @export
#'
sharp_neyman <- function(Y, Z, cin) {
    
    # Y=yg;Z=zg
    Y = as.numeric(Y)
    Z = as.numeric(Z)
    
    m1 <- mean(Y[Z == 1])
    m0 <- mean(Y[Z == 0])
    
    ### Estimate of tau.Us0
    tau <- m1 - m0
    tau
    
    ### Estimate of the Variance
    
    # binomial or # continuous
    if (length(table(Y)) == 2) {
        Vneyman <- (m1 * (1 - m1))/sum(Z == 1) + (m0 * (1 - m0))/sum(Z == 0)
        
    } else if (length(table(Y)) > 2) {
        Vneyman <- (var(Y[Z == 1]))/sum(Z == 1) + (var(Y[Z == 0]))/sum(Z == 0)
    }
    Vneyman
    
    ### 95% Confidence interval
    cig <- 1 - (100 - cin)/2/100
    zz <- qnorm(cig)
    tau - zz * sqrt(Vneyman)
    tau + zz * sqrt(Vneyman)
    
    vector <- c(length(Y), tau, sqrt(Vneyman), tau - zz * sqrt(Vneyman), tau + zz * sqrt(Vneyman))
    return(vector)
}


#' Sharp - neyman bandwidth
#'
#' @param dataset The dataset with the variables
#' @param forcing_bin_var_name forcing_bin_var_name
#' @param forcing_var_name forcing_var_name
#' @param Y_name Y_name
#' @param niter niter
#' @param bandwidth bandwidth
#' @param cut_value cut_value
#' @return data frame with variable and value and bandwidth
#' @export
#'
sharp_neyman_bw <- function(dataset = data, forcing_var_name = "S", Y_name = "dropout", niter = 1000, bandwidth = c(500, 1000, 1500), cut_value = 15000, whichunder = 1, cin = 95) {
    # for each bandwidth
    len_bw <- length(bandwidth)
    
    # Cut value
    s0 <- cut_value
    if (whichunder == 1) {
        dataset$assigVar <- ifelse(dataset[, forcing_var_name] <= cut_value, 1, 0)
    } else if (whichunder == 0) {
        dataset$assigVar <- ifelse(dataset[, forcing_var_name] <= cut_value, 0, 1)
    }
    
    Sh <- dataset[, forcing_var_name]
    pbalioak <- c()
    nak <- c()
    
    for (b in 1:len_bw) {
        
        # bandwidth
        h <- bandwidth[b]
        
        # Filter dataste with bw
        dat_bw <- dataset[Sh >= s0 - h & Sh <= s0 + h, ]
        
        # number of records N
        N <- dim(dat_bw)[1]
        print(N)
        zg <- as.numeric(dat_bw[, "assigVar"])
        yg <- as.numeric(dat_bw[, Y_name])
        
        ft <- sharp_neyman(Y = yg, Z = zg, cin = cin)
        pbalioak <- c(pbalioak, ft)
        
    }  # for b
    
    dfp <- data.frame(matrix(pbalioak, ncol = 5, byrow = T))
    dfp[, 2:5] <- round(dfp[, 2:5], 4)
    
    df <- cbind(bandwidth, dfp)
    names(df) <- c("Bandwidth", "N", "Average causal effect", "Neyman SE", paste0(cin, "% CI: Lower bound"), paste0(cin, "% CI: upper bound"))
    return(df)
    
}  # function end

#' Fuzzy - neyman
#'
#' @param Y Outcome variable
#' @param Z forcing binary variable
#' @param Wh selected records
#' @return data frame with variable and value and bandwidth
#' @export
#'
fuzzy_neyman <- function(Y, Wh, Z, cin = 95) {
    m.y1 <- mean(Y[Z == 1])
    m.y0 <- mean(Y[Z == 0])
    tau.y <- m.y1 - m.y0
    tau.y
    
    m.w1 <- mean(Wh[Z == 1])
    m.w0 <- mean(Wh[Z == 0])  #zero by design
    tau.w <- m.w1 - m.w0
    tau.w
    
    tau.c <- tau.y/tau.w
    tau.c
    
    # binomial or # continuous
    if (length(table(Y)) == 2) {
        Vneyman.Y <- (m.y1 * (1 - m.y1))/sum(Z == 1) + (m.y0 * (1 - m.y0))/sum(Z == 0)
        
    } else if (length(table(Y)) > 2) {
        Vneyman.Y <- (var(Y[Z == 1]))/sum(Z == 1) + (var(Y[Z == 0]))/sum(Z == 0)
    }
    
    # Vneyman.Y <- { m.y1 * (1 - m.y1) }/sum(Z == 1) + { m.y0 * (1 - m.y0) }/sum(Z == 0)
    Vneyman.Y
    sqrt(Vneyman.Y)
    
    if (length(table(Y)) == 2) {
        Vneyman.W <- (m.w1 * (1 - m.w1))/sum(Z == 1) + (m.w0 * (1 - m.w0))/sum(Z == 0)
        
    } else if (length(table(Y)) > 2) {
        Vneyman.W <- (var(Y[Z == 1]))/sum(Z == 1) + (var(Y[Z == 0]))/sum(Z == 0)
    }
    
    # Vneyman.W <- { m.w1 * (1 - m.w1) }/sum(Z == 1) + { m.w0 * (1 - m.w0) }/sum(Z == 0)
    
    Vneyman.W
    sqrt(Vneyman.W)
    
    cov.tau.w.tau.y <- {
        sum(Y[Z == 1] == 1 & Wh[Z == 1] == 1)/sum(Z == 1) - m.w1 * m.y1
    }/sum(Z == 1)
    
    Vtau.c <- {
        1/tau.w^2
    } * Vneyman.Y + {
        tau.y^2/tau.w^4
    } * Vneyman.W - 2 * {
        tau.y/tau.w^3
    } * cov.tau.w.tau.y
    Vtau.c
    sqrt(Vtau.c)
    
    ### 95% Confidence interval
    cig <- 1 - (100 - cin)/2/100
    zz <- qnorm(cig)
    # zz<- qnorm(0.975)
    tau.c - zz * sqrt(Vtau.c)
    tau.c + zz * sqrt(Vtau.c)
    
    # Using 2SLS library(sem) tsls(Y ~ Wh, ~ Z)
    
    vector <- c(tau.w, tau.y, tau.c, sqrt(Vneyman.W), sqrt(Vneyman.Y), sqrt(Vtau.c), tau.w - zz * sqrt(Vneyman.W), tau.y - zz * sqrt(Vneyman.Y), tau.c - zz * sqrt(Vtau.c), tau.w + zz * sqrt(Vneyman.W), 
        tau.y + zz * sqrt(Vneyman.Y), tau.c + zz * sqrt(Vtau.c))
    return(vector)
}


#' Fuzzy - neyman bandwidth
#'
#' @param dataset The dataset with the variables
#' @param forcing_bin_var_name forcing_bin_var_name
#' @param forcing_var_name forcing_var_name
#' @param Y_name Y_name
#' @param niter niter
#' @param bandwidth bandwidth
#' @param cut_value cut_value
#' @param W selected
#' @return data frame with variable and value and bandwidth
#' @export
#'
fuzzy_neyman_bw <- function(dataset = data, forcing_var_name = "S", Y_name = "dropout", niter = 1000, W = "W", bandwidth = c(500, 1000, 1500), cut_value = 15000, whichunder = 1, cin = 95) {
    # for each bandwidth
    len_bw <- length(bandwidth)
    
    # Cut value
    s0 <- cut_value
    if (whichunder == 1) {
        dataset$assigVar <- ifelse(dataset[, forcing_var_name] <= cut_value, 1, 0)
    } else if (whichunder == 0) {
        dataset$assigVar <- ifelse(dataset[, forcing_var_name] <= cut_value, 0, 1)
    }
    
    Sh <- dataset[, forcing_var_name]
    pbalioak <- c()
    nak <- c()
    
    df2 <- data.frame(matrix(888, 1, 6))
    
    names(df2) <- c("Bandwidth", "Estimand", "Average causal effect", "Neyman SE", paste0(cin, "% CI: Lower bound"), paste0(cin, "% CI: upper bound"))
    
    for (b in 1:len_bw) {
        
        # bandwidth
        h <- bandwidth[b]
        
        # Filter dataste with bw
        dat_bw <- dataset[Sh >= s0 - h & Sh <= s0 + h, ]
        
        # number of records N
        N <- dim(dat_bw)[1]
        print(N)
        zg <- as.numeric(dat_bw[, "assigVar"])
        yg <- as.numeric(dat_bw[, Y_name])
        wg <- as.numeric(dat_bw[, W])
        
        ft <- fuzzy_neyman(Y = yg, Wh = wg, Z = zg, cin = cin)
        
        dfp <- data.frame(matrix(ft, ncol = 4))
        dfp[, 1:4] <- round(dfp[, 1:4], 4)
        
        df <- cbind(rep(h, 3), c("ITT.W", "ITT.Y", "CACE"), dfp)
        names(df) <- c("Bandwidth", "Estimand", "Average causal effect", "Neyman SE", paste0(cin, "% CI: Lower bound"), paste0(cin, "% CI: upper bound"))
        # names(df) <- c('Bandwidth','N','Average causal effect','Neyman SE',paste0(cin,'% CI: Lower bound'),paste0(cin,'% CI: upper bound'))
        df2 <- rbind(df2, df)
        
    }  # for b
    
    df2 <- df2[-1, ]
    return(df2)
    
}  # function end


#' Fuzzy - FEP binary one side
#'
#' @param dataset The dataset with the variables
#' @param forcing_bin_var_name forcing_bin_var_name
#' @param forcing_var_name forcing_var_name
#' @param Y_name Y_name
#' @param niter niter
#' @param bandwidth bandwidth
#' @param cut_value cut_value
#' @param W selected
#' @param M2 number of iterations
#' @return data frame with variable and value and bandwidth
#' @export
#'
fuzzy_fep1sided <- function(dataset, Yg, Wg, Zg, Y_name, M2 = 10) {
    Y <- Yg
    Z <- Zg
    W <- Wg
    
    Nh <- nrow(dataset)
    ## IV-ESTIMATOR
    ITT.Y <- mean(Y[Z == 1]) - mean(Y[Z == 0])
    ITT.W <- mean(W[Z == 1]) - mean(W[Z == 0])
    CACE.IV.obs <- ITT.Y/ITT.W
    CACE.IV.obs
    
    ## MLE
    names(dataset)[which(names(dataset) == Y_name)] <- "Y"
    CACE.MLE.obs <- EM.bin(dat = dataset)
    
    # Posterior Mean
    CACE.PM.obs <- mcmc.bin(n.iter = 1000, n.burn = 500, dat = dataset)
    
    Tobs <- c(CACE.IV.obs, CACE.MLE.obs, CACE.PM.obs)
    M <- as.numeric(as.character(M2))
    nit <- 2 * M
    nburn <- nit - M
    
    ## Impute missing compliance statuses
    G <- mcmc.bin.h0(n.iter = nit, n.burn = nburn, dat = dataset)$Gstatus
    
    ## Impute missing potential outcomes under the null
    Y1 <- Y0 <- Y
    Tsim <- matrix(0, M, 3)
    colnames(Tsim) <- c("CACE.IV", "CACE.MLE", "CACE.PM")
    
    for (i in 1:M) {
        ## Draw a random hypothetical assignment
        Zh.sim <- sample(Z, Nh, replace = TRUE)
        
        ## Re-observe the data
        dataset.sim <- data.frame(Z = Zh.sim, W = 0 * (1 - Zh.sim) + G[i, ] * Zh.sim, Y = Y0 * (1 - Zh.sim) + Y1 * Zh.sim)
        
        ## Calculate the test statistic on these data
        ITT.Y <- mean(dataset.sim$Y[dataset.sim$Z == 1]) - mean(dataset.sim$Y[dataset.sim$Z == 0])
        ITT.W <- mean(dataset.sim$W[dataset.sim$Z == 1]) - mean(dataset.sim$W[dataset.sim$Z == 0])
        CACE.IV.sim <- ITT.Y/ITT.W
        CACE.MLE.sim <- EM.bin(dat = dataset.sim)
        CACE.PM.sim <- mcmc.bin(n.iter = 1000, n.burn = 500, dat = dataset.sim)
        
        Tsim[i, ] <- c(CACE.IV.sim, CACE.MLE.sim, CACE.PM.sim)
        print(i)
    }  ##End loop over M
    
    ## Calculate the posterior predictive p-value
    pppv <- apply(abs(Tsim) >= abs(Tobs), 2, mean)
    
    
    return(list(pppv, Tsim, Tobs))
}

#' Fuzzy - FEP binary two sided
#'
#' @param dataset The dataset with the variables
#' @param forcing_bin_var_name forcing_bin_var_name
#' @param forcing_var_name forcing_var_name
#' @param Y_name Y_name
#' @param niter niter
#' @param bandwidth bandwidth
#' @param cut_value cut_value
#' @param W selected
#' @param M2 number of iterations
#' @return data frame with variable and value and bandwidth
#' @export
#'
fuzzy_fep2sided <- function(dataset, Y, W, Z, Y_name, M2 = 10) {
    
    Nh <- nrow(dataset)
    ## IV-ESTIMATOR
    ITT.Y <- mean(Y[Z == 1]) - mean(Y[Z == 0])
    ITT.W <- mean(W[Z == 1]) - mean(W[Z == 0])
    CACE.IV.obs <- ITT.Y/ITT.W
    CACE.IV.obs
    
    ## MLE
    names(dataset)[which(names(dataset) == Y_name)] <- "Y"
    CACE.MLE.obs <- EM.bin2(dat = dataset)
    
    # Posterior Mean
    CACE.PM.obs <- mcmc.bin2(n.iter = 1000, n.burn = 500, dat = dataset)
    
    Tobs <- c(CACE.IV.obs, CACE.MLE.obs, CACE.PM.obs)
    M <- as.numeric(as.character(M2))
    nit <- 2 * M
    nburn <- nit - M
    
    ## Impute missing compliance statuses
    G <- mcmc.bin.h02(n.iter = nit, n.burn = nburn, dat = dataset)$Gstatus
    
    ## Impute missing potential outcomes under the null
    Y1 <- Y0 <- Y
    Tsim <- matrix(0, M, 3)
    colnames(Tsim) <- c("CACE.IV", "CACE.MLE", "CACE.PM")
    
    for (i in 1:M) {
        
        ## Draw a random hypothetical assignment
        Zh.sim <- sample(dataset$Z, Nh, replace = TRUE)
        
        Wh.sim <- 0 * {
            (1 - Zh.sim) * (G[i, ] == 1 | G[i, ] == 3) + Zh.sim * (G[i, ] == 1)
        } + 1 * {
            (1 - Zh.sim) * (G[i, ] == 2) + Zh.sim * (G[i, ] == 2 | G[i, ] == 3)
        }
        ## Re-observe the data
        dath.sim <- data.frame(Z = Zh.sim, W = Wh.sim, Y = Y0 * (1 - Zh.sim) + Y1 * Zh.sim)
        
        ## Calculate the test statistic on these data
        ITT.Y <- mean(dath.sim$Y[dath.sim$Z == 1]) - mean(dath.sim$Y[dath.sim$Z == 0])
        ITT.W <- mean(dath.sim$W[dath.sim$Z == 1]) - mean(dath.sim$W[dath.sim$Z == 0])
        CACE.IV.sim <- ITT.Y/ITT.W
        CACE.MLE.sim <- EM.bin2(dat = dath.sim)
        CACE.PM.sim <- mcmc.bin2(n.iter = 1000, n.burn = 500, dat = dath.sim)
        
        Tsim[i, ] <- c(CACE.IV.sim, CACE.MLE.sim, CACE.PM.sim)
        
    }  ##End loop over M
    
    ## Calculate the posterior predictive p-value
    pppv <- apply(abs(Tsim) >= abs(Tobs), 2, mean)
    
    
    return(list(pppv, Tsim, Tobs))
}

#' Fuzzy - FEP 1 sided
#'
#' @param dataset The dataset with the variables
#' @param forcing_bin_var_name forcing_bin_var_name
#' @param forcing_var_name forcing_var_name
#' @param Y_name Y_name
#' @param niter niter
#' @param bandwidth bandwidth
#' @param cut_value cut_value
#' @param W selected
#' @param M2 number of iterations
#' @return data frame with variable and value and bandwidth
#' @export
#'
fuzzy_fep_numeric1sided <- function(dataset, Y, W, Z, Y_name, M2 = 10) {
    
    G <- NULL
    G[Z == 1 & W == 1] <- 1
    G[Z == 1 & W == 0] <- 0
    G[Z == 0] <- rbinom(sum(Z == 0), 1, sum(Z == 1 & W == 1)/sum(Z == 1))
    Y <- NULL
    Y[Z == 0 & G == 1] <- rnorm(sum(Z == 0 & G == 1), 40, 9)
    Y[Z == 1 & G == 1] <- rnorm(sum(Z == 1 & G == 1), 60, 4)
    Y[G == 0] <- rnorm(sum(G == 0), 50, 12)
    rm(G)
    dataset <- data.frame(Z = Z, W = W, Y = Y)
    Nh <- nrow(dataset)
    
    # Nh<- nrow(dataset) IV-ESTIMATOR
    ITT.Y <- mean(Y[Z == 1]) - mean(Y[Z == 0])
    ITT.W <- mean(W[Z == 1]) - mean(W[Z == 0])
    CACE.IV.obs <- ITT.Y/ITT.W
    CACE.IV.obs
    
    ## MLE
    names(dataset)[which(names(dataset) == Y_name)] <- "Y"
    CACE.MLE.obs <- EM.gauss(dat = dataset)
    
    # Posterior Mean
    CACE.PM.obs <- mcmc.gauss(n.iter = 1000, n.burn = 500, dat = dataset)
    
    Tobs <- c(CACE.IV.obs, CACE.MLE.obs, CACE.PM.obs)
    M <- as.numeric(as.character(M2))
    nit <- 2 * M
    nburn <- nit - M
    
    ## Impute missing compliance statuses
    G <- mcmc.gauss.h0(n.iter = nit, n.burn = nburn, dat = dataset)$Gstatus
    
    ## Impute missing potential outcomes under the null
    Y1 <- Y0 <- Y
    Tsim <- matrix(0, M, 3)
    colnames(Tsim) <- c("CACE.IV", "CACE.MLE", "CACE.PM")
    
    for (i in 1:M) {
        ## Draw a random hypothetical assignment
        Zh.sim <- sample(Z, Nh, replace = TRUE)
        
        ## Re-observe the data
        dataset.sim <- data.frame(Z = Zh.sim, W = 0 * (1 - Zh.sim) + G[i, ] * Zh.sim, Y = Y0 * (1 - Zh.sim) + Y1 * Zh.sim)
        
        ## Calculate the test statistic on these data
        ITT.Y <- mean(dataset.sim$Y[dataset.sim$Z == 1]) - mean(dataset.sim$Y[dataset.sim$Z == 0])
        ITT.W <- mean(dataset.sim$W[dataset.sim$Z == 1]) - mean(dataset.sim$W[dataset.sim$Z == 0])
        CACE.IV.sim <- ITT.Y/ITT.W
        CACE.MLE.sim <- EM.gauss(dat = dataset.sim)
        CACE.PM.sim <- mcmc.gauss(n.iter = 1000, n.burn = 500, dat = dataset.sim)
        
        Tsim[i, ] <- c(CACE.IV.sim, CACE.MLE.sim, CACE.PM.sim)
        print(i)
    }  ##End loop over M
    
    ## Calculate the posterior predictive p-value
    pppv <- apply(abs(Tsim) >= abs(Tobs), 2, mean)
    
    
    return(list(pppv, Tsim, Tobs))
}

#' Fuzzy - FEP 2 sided
#'
#' @param dataset The dataset with the variables
#' @param forcing_bin_var_name forcing_bin_var_name
#' @param forcing_var_name forcing_var_name
#' @param Y_name Y_name
#' @param niter niter
#' @param bandwidth bandwidth
#' @param cut_value cut_value
#' @param W selected
#' @param M2 number of iterations
#' @return data frame with variable and value and bandwidth
#' @export
#'
fuzzy_fep_numeric2sided <- function(dataset, Y, W, Z, Y_name, M2 = 10) {
    
    G <- NULL
    G[Zh == 0 & Wh == 1] <- 2
    G[Zh == 1 & Wh == 0] <- 1
    
    
    pnt <- sum(Zh == 1 & Wh == 0)/sum(Zh == 1)
    pat <- sum(Zh == 0 & Wh == 1)/sum(Zh == 0)
    pc <- 1 - pnt - pat
    
    u <- rbinom(sum(Zh == 0 & Wh == 0), 1, pc/{
        pc + pnt
    })
    G[(Zh == 0 & Wh == 0)] <- 3 * (u == 1) + 1 * (u == 0)
    u <- rbinom(sum(Zh == 1 & Wh == 1), 1, pc/{
        pc + pat
    })
    G[(Zh == 1 & Wh == 1)] <- 3 * (u == 1) + 2 * (u == 0)
    
    Yh <- NULL
    Yh[Zh == 0 & G == 3] <- rnorm(sum(Zh == 0 & G == 3), 40, 9)
    Yh[Zh == 1 & G == 3] <- rnorm(sum(Zh == 1 & G == 3), 60, 4)
    Yh[G == 1] <- rnorm(sum(G == 1), 50, 12)
    Yh[G == 2] <- rnorm(sum(G == 2), 45, 8)
    rm(G, u, pc, pat, pnt)
    dath <- data.frame(Z = Zh, W = Wh, Y = Yh)
    Nh <- nrow(dath)
    
    # Nh<- nrow(dataset) IV-ESTIMATOR
    ITT.Y <- mean(Y[Z == 1]) - mean(Y[Z == 0])
    ITT.W <- mean(W[Z == 1]) - mean(W[Z == 0])
    CACE.IV.obs <- ITT.Y/ITT.W
    CACE.IV.obs
    
    ## MLE
    names(dataset)[which(names(dataset) == Y_name)] <- "Y"
    CACE.MLE.obs <- EM.gauss2(dat = dataset)
    
    # Posterior Mean
    CACE.PM.obs <- mcmc.gauss2(n.iter = 1000, n.burn = 500, dat = dataset)
    
    Tobs <- c(CACE.IV.obs, CACE.MLE.obs, CACE.PM.obs)
    M <- as.numeric(as.character(M2))
    nit <- 2 * M
    nburn <- nit - M
    
    ## Impute missing compliance statuses
    G <- mcmc.gauss.h02(n.iter = nit, n.burn = nburn, dat = dataset)$Gstatus
    
    ## Impute missing potential outcomes under the null
    Y1 <- Y0 <- Y
    Tsim <- matrix(0, M, 3)
    colnames(Tsim) <- c("CACE.IV", "CACE.MLE", "CACE.PM")
    
    for (i in 1:M) {
        ## Draw a random hypothetical assignment
        Zh.sim <- sample(dath$Z, Nh, replace = TRUE)
        
        Wh.sim <- 0 * {
            (1 - Zh.sim) * (G[i, ] == 1 | G[i, ] == 3) + Zh.sim * (G[i, ] == 1)
        } + 1 * {
            (1 - Zh.sim) * (G[i, ] == 2) + Zh.sim * (G[i, ] == 2 | G[i, ] == 3)
        }
        ## Re-observe the data
        dath.sim <- data.frame(Z = Zh.sim, W = Wh.sim, Y = Y0 * (1 - Zh.sim) + Y1 * Zh.sim)
        
        ## Calculate the test statistic on these data
        ITT.Y <- mean(dath.sim$Y[dath.sim$Z == 1]) - mean(dath.sim$Y[dath.sim$Z == 0])
        ITT.W <- mean(dath.sim$W[dath.sim$Z == 1]) - mean(dath.sim$W[dath.sim$Z == 0])
        CACE.IV.sim <- ITT.Y/ITT.W
        CACE.MLE.sim <- EM.gauss(dat = dath.sim)
        CACE.PM.sim <- mcmc.gauss(n.iter = 1000, n.burn = 500, dat = dath.sim)
        
        Tsim[i, ] <- c(CACE.IV.sim, CACE.MLE.sim, CACE.PM.sim)
        
    }  ##End loop over M
    
    ## Calculate the posterior predictive p-value
    pppv <- apply(abs(Tsim) >= abs(Tobs), 2, mean)
    
    
    return(list(pppv, Tsim, Tobs))
}

#' Fuzzy - FEP bandwidth
#'
#' @param dataset The dataset with the variables
#' @param forcing_bin_var_name forcing_bin_var_name
#' @param forcing_var_name forcing_var_name
#' @param Y_name Y_name
#' @param niter niter
#' @param bandwidth bandwidth
#' @param cut_value cut_value
#' @param W selected
#' @return data frame with variable and value and bandwidth
#' @export
#'
fuzzy_fep_bw <- function(dataset = data, forcing_var_name = "S", Y_name = "dropout", niter = 1000, W_name = "W", bandwidth = c(500, 1000, 1500), cut_value = 15000, M2 = 5, whichunder = 1, typemod = "binary", 
    typesided = "onesided") {
    
    dataset$W <- dataset[, W_name]
    dataset$Y <- dataset[, Y_name]
    
    # for each bandwidth
    len_bw <- length(bandwidth)
    
    # Cut value
    s0 <- cut_value
    if (whichunder == 1) {
        dataset$assigVar <- ifelse(dataset[, forcing_var_name] <= cut_value, 1, 0)
    } else if (whichunder == 0) {
        dataset$assigVar <- ifelse(dataset[, forcing_var_name] <= cut_value, 0, 1)
    }
    
    dataset$Z <- dataset$assigVar
    
    Sh <- dataset[, forcing_var_name]
    pbalioak <- c()
    nak <- c()
    
    df2 <- data.frame(matrix(888, 1, 4))
    
    names(df2) <- c("Bandwidth", "Statistic: IV estimate of CACE", "Statistic: MLE of CACE", "Statistic: Posterior median of CACE")
    
    for (b in 1:len_bw) {
        
        # bandwidth
        h <- bandwidth[b]
        
        # Filter dataste with bw
        dat_bw <- dataset[Sh >= s0 - h & Sh <= s0 + h, ]
        
        # number of records N
        N <- dim(dat_bw)[1]
        print(N)
        zg <- dat_bw[, "assigVar"]
        yg <- dat_bw[, Y_name]
        wg <- dat_bw[, W_name]
        
        
        # if numeric or dichotomic
        if (typemod == "binary") {
            if (typesided == "onesided") {
                fu <- fuzzy_fep1sided(dat_bw, Y = yg, W = wg, Z = zg, Y_name = Y_name, M2 = M2)
                ft <- c(h, fu[[1]])
            } else if (typesided == "twosided") {
                fu <- fuzzy_fep2sided(dat_bw, Y = yg, W = wg, Z = zg, Y_name = Y_name, M2 = M2)
                ft <- c(h, fu[[1]])
            }
            
        } else if (typemod == "numeric") {
            if (typesided == "onesided") {
                fu <- fuzzy_fep_numeric1sided(dat_bw, Y = yg, W = wg, Z = zg, Y_name = Y_name, M2 = M2)
                ft <- c(h, fu[[1]])
            } else if (typesided == "twosided") {
                fu <- fuzzy_fep_numeric2sided(dat_bw, Y = yg, W = wg, Z = zg, Y_name = Y_name, M2 = M2)
                ft <- c(h, fu[[1]])
            }
            
        }
        
        df2 <- rbind(df2, ft)
        
    }  # for b
    
    df2 <- df2[-1, ]
    return(list(df2, fu[[2]], fu[[3]]))
    
}  # function end



#' Tave
#'
#' @param x The dataset with the variables
#' @param z forcing_bin_var_name
#' @return data frame with variable and value and bandwidth
#' @export
#'
Tave <- function(x, z) {
    x0 <- mean(x[z == 0])
    x1 <- mean(x[z == 1])
    return(abs(x1 - x0))
    
}



