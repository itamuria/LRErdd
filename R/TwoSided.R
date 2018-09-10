library(gtools) ##Dirichlet

#' Calculate MLE - EM
#'
#' @param theta theta
#' @param dat dat
#' @return result. We use Conditional probabilities using Bayes' Rule and we calculate the Expected compliance behavior
#' @export

E.step.bin2 <- function(theta, dat){
  
  # Conditional probabilities using Bayes' Rule
  
  num0 <- theta$pi.c*dbinom(dat$Y,1,theta$py.c0)        # C,0
  den0 <- theta$pi.nt*dbinom(dat$Y,1,theta$py.nt) + theta$pi.c*dbinom(dat$Y,1,theta$py.c0) 
  
  num1 <- theta$pi.c*dbinom(dat$Y,1,theta$py.c1)        # C,1
  den1 <- theta$pi.at*dbinom(dat$Y,1,theta$py.at) + theta$pi.c*dbinom(dat$Y,1,theta$py.c1) 
  
  pG<-NULL
  pG[dat$Z == 1 & dat$W == 1]  <- num1[dat$Z == 1 & dat$W == 1] /den1[dat$Z == 1 & dat$W == 1] 
  pG[dat$Z == 0 & dat$W == 0]  <- num0[dat$Z == 0 & dat$W == 0] /den0[dat$Z == 0 & dat$W == 0]
  
  # Expected compliance behavior
  G.c <- G.at <- G.nt <- NULL
  
  G.c[dat$Z == 1 & dat$W == 1] <- pG[dat$Z == 1 & dat$W == 1] 
  G.c[dat$Z == 0 & dat$W == 1] <- 0
  G.c[dat$Z == 1 & dat$W == 0] <- 0
  G.c[dat$Z == 0 & dat$W == 0] <- pG[dat$Z == 0 & dat$W == 0] 
  
  G.at[dat$Z == 1 & dat$W == 1] <- 1-pG[dat$Z == 1 & dat$W == 1] 
  G.at[dat$Z == 0 & dat$W == 1] <- 1
  G.at[dat$Z == 1 & dat$W == 0] <- 0
  G.at[dat$Z == 0 & dat$W == 0] <- 0 
  
  
  G.nt[dat$Z == 1 & dat$W == 1] <- 0 
  G.nt[dat$Z == 0 & dat$W == 1] <- 0
  G.nt[dat$Z == 1 & dat$W == 0] <- 1
  G.nt[dat$Z == 0 & dat$W == 0] <- 1-pG[dat$Z == 0 & dat$W == 0] 
  
  G<- cbind(G.nt, G.at, G.c)
  rm(G.nt, G.at, G.c)
  return(G)
}

#' Calculate M.step.bin
#'
#' @param G G
#' @param dat dat
#' @return result. 
#' @export

M.step.bin2 <- function(G, dat){
  theta<-list()
  theta$pi.nt <- mean(G[,1])
  theta$pi.at <- mean(G[,2])
  theta$pi.c  <- mean(G[,3])    
  
  theta$py.c0<-sum(dat$Y*(1-dat$Z)*G[,3])/sum((1-dat$Z)*G[,3])
  theta$py.c1<-sum(dat$Y*dat$Z*G[,3])/sum(dat$Z*G[,3])
  
  theta$py.nt<-sum(dat$Y*G[,1])/sum(G[,1])
  theta$py.at<-sum(dat$Y*G[,2])/sum(G[,2])
  
  return(theta)
  
}



#' Calculate EM.bin
#'
#' @param G G
#' @param tol tol
#' @param maxit maxit
#' @return result. tolerance fixed at 0.0001
#' @export

EM.bin2 <- function(dat, tol = 1e-4, maxit = 1000){

  pi.g<-rdirichlet(1,c(1,1,1))
  theta <- list(pi.nt  = pi.g[1], pi.at= pi.g[2], pi.c=pi.g[3],
                py.nt = rbeta(1,1,1),
                py.at = rbeta(1,1,1),
                py.c0 = rbeta(1,1,1),
                py.c1 = rbeta(1,1,1))
  rm(pi.g)
  plus<-function(x){
    yy<-(x+0.01)
    (x+0.01)*(yy<1)+(x-0.01)*(yy>1)
  }
  
  theta1<- lapply(theta,plus)
  
  n.it <- 0

  while(
    max(abs(unlist(theta1) - unlist(theta))) > tol 
    
  ){
    
    theta1 <- theta
    
    G     <- E.step.bin2(theta, dat)
    theta <- M.step.bin2(G, dat)
    
    n.it <- n.it + 1
    if(n.it == maxit){break}
  }
  
  out <- list(theta = theta, G=G,  converged = (n.it < maxit))
  CACE.MLE <- theta$py.c1-theta$py.c0
  return(CACE.MLE)
}

#' Calculate POSTERIOR MEAN - DA + MCMC
#'
#' @param theta theta
#' @param tol tol
#' @return result. Impute missing compliance statuses under the null, ## Z=0 & W=0 (NT+C)
#' @export

da.bin2 <-  function(theta, dat){
  
  G<- NULL  
  G[dat$Z==0 & dat$W==1] <- 2 ##Always - takers
  G[dat$Z==1 & dat$W==0] <- 1 ##Never - takers
  
  num <- theta$pi[3]*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.c0)
  den <- theta$pi[1]*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.nt)+
    theta$pi[3]*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.c0)
  
  pc.00 <- num/den 
  pc.00[num==0]<-0
  u.00 <- rbinom(sum(dat$Z==0 & dat$W==0), 1, pc.00)
  
  G[dat$Z==0 & dat$W==0]<- 3*(u.00==1) + 1*(u.00==0)
  rm(num, den, pc.00, u.00)
  
  num <- theta$pi[3]*dbinom(dat$Y[dat$Z==1 & dat$W==1],1, theta$py.c1)
  den <- theta$pi[2]*dbinom(dat$Y[dat$Z==1 & dat$W==1],1, theta$py.at)+
    theta$pi[3]*dbinom(dat$Y[dat$Z==1 & dat$W==1],1, theta$py.c1)
  
  pc.11 <- num/den
  pc.11[num==0]<-0
  u.11 <- rbinom(sum(dat$Z==1 & dat$W==1), 1, pc.11)
  
  G[dat$Z==1 & dat$W==1]<- 3*(u.11==1) + 2*(u.11==0)
  
  rm(num, den, pc.11, u.11)
  return(G)
}

#' Calculate mcmc.bin
#'
#' @param n.iter number of iterations
#' @param n.burn burn-in step
#' @param dat data set
#' @return result. 
#' @export

mcmc.bin2 <- function(n.iter, n.burn, dat){

  n.draws<- n.iter-n.burn
  theta.prior <- list(a = c(1,1,1), ay.nt = c(1,1), ay.at = c(1,1),  ay.c0= c(1,1), ay.c1= c(1,1))

  theta <-   list(pi = rdirichlet(1, theta.prior$a), 
                  py.nt  = rbeta(1, theta.prior$ay.nt[1], theta.prior$ay.nt[2]),
                  py.at  = rbeta(1, theta.prior$ay.at[1], theta.prior$ay.at[2]),
                  py.c0  = rbeta(1, theta.prior$ay.c0[1], theta.prior$ay.c0[2]),
                  py.c1  = rbeta(1, theta.prior$ay.c1[1], theta.prior$ay.c1[2]))
  
  THETA<-matrix(0, n.draws, length(unlist(theta)))
  colnames(THETA)<- c("pi.nt", "pi.at", "pi.c", "py.nt", "py.at", "py.c0", "py.c1")
  
  CACE <- NULL
  
  for(j in 1:n.iter){
    G<- da.bin2(theta, dat)
    theta$pi <- rdirichlet(1, {theta.prior$a + c(sum(G==1), sum(G==2), sum(G==3))})
    
    theta$py.nt <- rbeta(1,{theta.prior$ay.nt[1] + sum(G==1 & dat$Y==1)},  
                         {theta.prior$ay.nt[2] + sum(G==1 & dat$Y==0)})
    
    theta$py.at <- rbeta(1,{theta.prior$ay.at[1] + sum(G==2 & dat$Y==1)},  
                         {theta.prior$ay.at[2] + sum(G==2 & dat$Y==0)})
    
    theta$py.c0 <- rbeta(1,{theta.prior$ay.c0[1] + sum(G==3 & dat$Z==0 & dat$Y==1)},  
                         {theta.prior$ay.c0[2] + sum(G==3 & dat$Z==0 & dat$Y==0)})
    
    theta$py.c1 <- rbeta(1,{theta.prior$ay.c1[1] + sum(G==3 & dat$Z==1 & dat$Y==1)},  
                         {theta.prior$ay.c1[2] + sum(G==3 & dat$Z==1 & dat$Y==0)})
    
    if(j > n.burn){
      jj<- j-n.burn
      THETA[jj, ]<-unlist(theta)
      CACE[jj]<-  theta$py.c1- theta$py.c0
    }
  } # END LOOP OVER j=1,..., n.iter
  
  
  CACE.PM <-median(CACE)   
  
  return(CACE.PM)
}


#' Impute missing compliance statuses under the NULL
#'
#' @param theta theta
#' @param dat data set
#' @return result.   ## Z=0 & W=0 (NT+C)
#' @export

da.bin.h02 <-  function(theta, dat){
  
  G<- NULL  
  G[dat$Z==0 & dat$W==1] <- 2 ##Always - takers
  G[dat$Z==1 & dat$W==0] <- 1 ##Never - takers
  
  num <- theta$pi[3]*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.c)
  den <- theta$pi[1]*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.nt)+
    theta$pi[3]*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.c)
  
  pc.00 <- num/den 
  u.00 <- rbinom(sum(dat$Z==0 & dat$W==0), 1, pc.00)
  
  G[dat$Z==0 & dat$W==0]<- 3*(u.00==1) + 1*(u.00==0)
  rm(num, den, pc.00, u.00)
  
  num <- theta$pi[3]*dbinom(dat$Y[dat$Z==1 & dat$W==1],1, theta$py.c)
  den <- theta$pi[2]*dbinom(dat$Y[dat$Z==1 & dat$W==1],1, theta$py.at)+
    theta$pi[3]*dbinom(dat$Y[dat$Z==1 & dat$W==1],1, theta$py.c)
  pc.11 <- num/den 
  u.11 <- rbinom(sum(dat$Z==1 & dat$W==1), 1, pc.11)
  
  G[dat$Z==1 & dat$W==1]<- 3*(u.11==1) + 2*(u.11==0)
  
  rm(num, den, pc.11, u.11)
  return(G)
}

#' Calculate mcmc.bin.h02
#'
#' @param n.iter number of iterations
#' @param n.burn burn-in step
#' @param dat data set
#' @return result.   ## Z=0 & W=0 (NT+C)
#' @export

mcmc.bin.h02 <- function(n.iter, n.burn, dat){

  n.draws<- n.iter-n.burn
  theta.prior <- list(a = c(1,1,1), ay.nt = c(1,1), ay.at = c(1,1),  ay.c= c(1,1))

  theta <-   list(pi = rdirichlet(1, theta.prior$a), 
                  py.nt = rbeta(1, theta.prior$ay.nt[1], theta.prior$ay.nt[2]),
                  py.at = rbeta(1, theta.prior$ay.at[1], theta.prior$ay.at[2]),
                  py.c  = rbeta(1, theta.prior$ay.c[1],  theta.prior$ay.c[2]))
  

  THETA<-matrix(0, n.draws, length(unlist(theta)))
  colnames(THETA)<- c("pi.nt", "pi.at", "pi.c", "py.nt", "py.at", "py.c")
  Gstatus <- matrix(0, n.draws, nrow(dat))
  
  for(j in 1:n.iter){
    G<- da.bin.h02(theta, dat)
    theta$pi <- rdirichlet(1, {theta.prior$a + c(sum(G==1), sum(G==2), sum(G==3))})
    
    theta$py.nt <- rbeta(1,{theta.prior$ay.nt[1] + sum(G==1 & dat$Y==1)},  
                         {theta.prior$ay.nt[2] + sum(G==1 & dat$Y==0)})
    
    theta$py.at <- rbeta(1,{theta.prior$ay.at[1] + sum(G==2 & dat$Y==1)},  
                         {theta.prior$ay.at[2] + sum(G==2 & dat$Y==0)})
    
    theta$py.c <- rbeta(1,{theta.prior$ay.c[1] + sum(G==3 & dat$Y==1)},  
                        {theta.prior$ay.c[2] + sum(G==3 & dat$Y==0)})
    
    if(j > n.burn){
      jj<- j-n.burn
      THETA[jj, ]<-unlist(theta)
      Gstatus[jj,]<- G
    }
    
  } # END LOOP OVER j=1,..., n.iter
  
  list(THETA=THETA, Gstatus=Gstatus)
}



#' Calculate MLE - EM
#'
#' @param theta theta
#' @param dat dat
#' @return result. We use Conditional probabilities using Bayes' Rule and we calculate the Expected compliance behavior
#' @export

E.step.gauss2 <- function(theta, dat){

  num0 <- theta$pi.c*dnorm(dat$Y,theta$mu.c0,sqrt(theta$sigma2.c0))        # C,0
  den0 <- theta$pi.nt*dnorm(dat$Y,theta$mu.nt,sqrt(theta$sigma2.nt)) + theta$pi.c*dnorm(dat$Y,theta$mu.c0,sqrt(theta$sigma2.c0)) 
  
  num1 <- theta$pi.c*dnorm(dat$Y,theta$mu.c1,sqrt(theta$sigma2.c1))        # C,1
  den1 <- theta$pi.at*dnorm(dat$Y,theta$mu.at,sqrt(theta$sigma2.at)) + theta$pi.c*dnorm(dat$Y,theta$mu.c1,sqrt(theta$sigma2.c1))  
  
  pG<-NULL
  pG[dat$Z == 0 & dat$W == 0] <- num0[dat$Z == 0 & dat$W == 0]/den0[dat$Z == 0 & dat$W == 0]
  pG[dat$Z == 1 & dat$W == 1] <- num1[dat$Z == 1 & dat$W == 1]/den1[dat$Z == 1 & dat$W == 1]

  G<-matrix(0,nrow(dat),3)
  
  G[dat$Z == 1 & dat$W == 1,1] <- 0
  G[dat$Z == 1 & dat$W == 0,1] <- 1
  G[dat$Z == 0 & dat$W == 1,1] <- 0 
  G[dat$Z == 0 & dat$W == 0,1] <- 1-pG[dat$Z == 0 & dat$W == 0]
  
  G[dat$Z == 1 & dat$W == 1,2] <- 1-pG[dat$Z == 1 & dat$W == 1]
  G[dat$Z == 1 & dat$W == 0,2] <- 0
  G[dat$Z == 0 & dat$W == 1,2] <- 1 
  G[dat$Z == 0 & dat$W == 0,2] <- 0
  
  G[dat$Z == 1 & dat$W == 1,3] <- pG[dat$Z == 1 & dat$W == 1]
  G[dat$Z == 1 & dat$W == 0,3] <- 0
  G[dat$Z == 0 & dat$W == 1,3] <- 0 
  G[dat$Z == 0 & dat$W == 0,3] <- pG[dat$Z == 0 & dat$W == 0]
  
  return(G)
}


#' Calculate M.step.gaus
#'
#' @param G G
#' @param dat dat
#' @return result. 
#' @export

M.step.gauss2<- function(G, dat){
  theta<-list()
  theta$pi.nt <- mean(G[,1])    
  theta$pi.at <- mean(G[,2])    
  theta$pi.c  <- mean(G[,3])
  
  theta$mu.c0<-sum(dat$Y*(1-dat$Z)*G[,3])/sum((1-dat$Z)*G[,3])
  theta$mu.c1<-sum(dat$Y*dat$Z*G[,3])/sum(dat$Z*G[,3])
  theta$mu.nt<-sum(dat$Y*G[,1])/sum(G[,1])
  theta$mu.at<-sum(dat$Y*G[,2])/sum(G[,2])
  
  theta$sigma2.c0 <- sum((dat$Y-theta$mu.c0)^2*(1 - dat$Z)*G[,3])/sum((1 - dat$Z)*G[,3])                 # C,0
  theta$sigma2.c1 <- sum((dat$Y-theta$mu.c1)^2*dat$Z*G[,3])/sum(dat$Z*G[,3])                            # C,1
  theta$sigma2.nt <- sum((dat$Y-theta$mu.nt)^2*G[,1])/sum(G[,1]) 
  theta$sigma2.at <- sum((dat$Y-theta$mu.at)^2*G[,2])/sum(G[,2])
  return(theta)
  
}

#' Calculate EM.gaus
#'
#' @param G G
#' @param tol tol
#' @param maxit maxit
#' @return result. tolerance fixed at 0.0001
#' @export

EM.gauss2 <- function(dat, tol = 1e-4, maxit = 1000){
  
  pi.g <- rdirichlet(1, c(1,1,1))
  theta <- list(pi.nt  = pi.g[1],pi.at  = pi.g[2], pi.c  = pi.g[3],
                mu.nt = mean(dat$Y) + rnorm(1,0,sd(dat$Y)), sigma2.nt = runif(1,0,var(dat$Y)),
                mu.at = mean(dat$Y) + rnorm(1,0,sd(dat$Y)), sigma2.at = runif(1,0,var(dat$Y)),
                mu.c0 = mean(dat$Y) + rnorm(1,0,sd(dat$Y)), sigma2.c0 = runif(1,0,var(dat$Y)),
                mu.c1 = mean(dat$Y) + rnorm(1,0,sd(dat$Y)), sigma2.c1 = runif(1,0,var(dat$Y))
  )
  
  plus<-function(x){
    yy<-x+1
    return(yy)
  }
  
  theta1<- lapply(theta,plus)

  n.it <- 0

  while(
    max(abs(unlist(theta1) - unlist(theta))) > tol
    
  ){
    
    theta1 <- theta
    
    G     <- E.step.gauss2(theta, dat)
    theta <- M.step.gauss2(G, dat)
    
    n.it <- n.it + 1
    if(n.it == maxit){break}
  }
  
  out <- list(theta = theta, G=G,  converged = (n.it < maxit))
  CACE.MLE <- theta$mu.c1-theta$mu.c0
  return(CACE.MLE)
}

#' Calculate POSTERIOR MEAN - DA + MCMC
#'
#' @param theta theta
#' @param tol tol
#' @return result. Impute missing compliance statuses under the null, ## Z=0 & W=0 (NT+C)
#' @export

da.gauss2 <-  function(theta, dat){
  
  G<- NULL  
  G[dat$Z==0 & dat$W==1] <- 2 ##AT
  G[dat$Z==1 & dat$W==0] <- 1 ##Never - takers

  num <- theta$pi[3]*dnorm(dat$Y[dat$Z==0 & dat$W==0], theta$mu.c0, sqrt(theta$sigma2.c0))
  den <- theta$pi[1]*dnorm(dat$Y[dat$Z==0 & dat$W==0], theta$mu.nt, sqrt(theta$sigma2.nt)) +
    theta$pi[3]*dnorm(dat$Y[dat$Z==0 & dat$W==0], theta$mu.c0, sqrt(theta$sigma2.c0))
  
  pc.00 <- num/den 
  pc.00[num==0]<-0
  u.00<- rbinom(sum(dat$Z==0 & dat$W==0), 1, pc.00)
  G[dat$Z==0 & dat$W==0]<- 3*(u.00==1) + 1*(u.00==0)
  
  rm(num, den, pc.00, u.00)
  
  num <- theta$pi[3]*dnorm(dat$Y[dat$Z==1 & dat$W==1], theta$mu.c1, sqrt(theta$sigma2.c1))
  den <- theta$pi[2]*dnorm(dat$Y[dat$Z==1 & dat$W==1], theta$mu.at, sqrt(theta$sigma2.at)) +
    theta$pi[3]*dnorm(dat$Y[dat$Z==1 & dat$W==1], theta$mu.c1, sqrt(theta$sigma2.c1))
  
  pc.11 <- num/den 
  pc.11[num==0]<-0
  u.11<- rbinom(sum(dat$Z==1 & dat$W==1), 1, pc.11)
  G[dat$Z==1 & dat$W==1]<- 3*(u.11==1) + 2*(u.11==0)
  
  rm(num, den, pc.11, u.11)
  
  return(G)
}


#' Calculate mcmc.gaus
#'
#' @param n.iter number of iterations
#' @param n.burn burn-in step
#' @param dat data set
#' @return result. 
#' @export

mcmc.gauss <- function(n.iter, n.burn, dat){
  
  n.draws<- n.iter-n.burn
  theta.prior<- list(a=c(1,1,1))

  theta <-   list(pi  = rdirichlet(1,c(1, 1,1)), 
                  mu.nt  = mean(dat$Y) + rnorm(1, 0, 1), 
                  sigma2.nt = runif(1, var(dat$Y)/2, 2*var(dat$Y)),
                  mu.at  = mean(dat$Y) + rnorm(1, 0, 1), 
                  sigma2.at = runif(1, var(dat$Y)/2, 2*var(dat$Y)),
                  mu.c0  = mean(dat$Y) + rnorm(1, 0, 1), 
                  sigma2.c0 = runif(1, var(dat$Y)/2, 2*var(dat$Y)),
                  mu.c1  = mean(dat$Y) + rnorm(1, 0, 1), 
                  sigma2.c1 = runif(1, var(dat$Y)/2, 2*var(dat$Y))
  )
  
  THETA<-matrix(0, n.draws, length(unlist(theta)))
  colnames(THETA)<- c("pi.nt", "pi.at", "pi.c", "mu.nt", "sigma2.nt",  
                      "mu.at", "sigma2.at",  
                      "mu.c0", "sigma2.c0", "mu.c1", "sigma2.c1")
  
  CACE <- NULL
  
  for(j in 1:n.iter){

    G<- da.gauss2(theta, dat)
    theta$pi <- rdirichlet(1, c({theta.prior$a[1] + sum(G==1)}, {theta.prior$a[2] + sum(G==2)}, {theta.prior$a[3] + sum(G==3)}))
    
    n.nt<- sum(G==1)
    s2.nt <- var(dat$Y[G==1])
    theta$sigma2.nt <- {(n.nt-1)*s2.nt}/ rchisq(1, df={n.nt-1})
    theta$mu.nt <- rnorm(1, mean=mean(dat$Y[G==1]), sd=sqrt(theta$sigma2.nt/n.nt))
    rm(n.nt, s2.nt)
    
    n.at<- sum(G==2)
    s2.at <- var(dat$Y[G==2])
    theta$sigma2.at <- {(n.at-1)*s2.at}/ rchisq(1, df={n.at-1})
    theta$mu.at <- rnorm(1, mean=mean(dat$Y[G==2]), sd=sqrt(theta$sigma2.at/n.at))
    rm(n.at, s2.at)
    
    n.c0<- sum(G==3 & dat$Z==0)
    s2.c0 <- var(dat$Y[G==3 & dat$Z==0])
    theta$sigma2.c0 <- {(n.c0-1)*s2.c0}/ rchisq(1, df={n.c0-1})
    theta$mu.c0 <- rnorm(1, mean=mean(dat$Y[G==3 & dat$Z==0]), sd=sqrt(theta$sigma2.c0/n.c0))
    rm(n.c0, s2.c0)
    
    n.c1<- sum(G==3 & dat$Z==1)
    s2.c1 <- var(dat$Y[G==3 & dat$Z==1])
    theta$sigma2.c1 <- {(n.c1-1)*s2.c1}/ rchisq(1, df={n.c1-1})
    theta$mu.c1 <- rnorm(1, mean=mean(dat$Y[G==3 & dat$Z==1]), sd=sqrt(theta$sigma2.c1/n.c1))
    rm(n.c1, s2.c1)
    
    if(j > n.burn){
      jj <- j - n.burn 
      THETA[jj, ]<-unlist(theta)
      CACE[jj]<-  theta$mu.c1- theta$mu.c0
    }
  } # END LOOP OVER j=1,..., n.iter
  
  
  CACE.PM <-median(CACE)   

  return(CACE.PM)
  
}

#' Impute missing compliance statuses under the NULL
#'
#' @param theta theta
#' @param dat data set
#' @return result.   ## Z=0 & W=0 (NT+C)
#' @export

da.gauss.h02 <-  function(theta, dat){
  
  G<- NULL  
  G[dat$Z==1 & dat$W==0] <- 1 ##Never - takers
  G[dat$Z==0 & dat$W==1] <- 2 ##Always - takers
  
  num <- theta$pi[3]*dnorm(dat$Y[dat$Z==0 & dat$W==0],theta$mu.c, sqrt(theta$sigma2.c))
  den <- theta$pi[1]*dnorm(dat$Y[dat$Z==0 & dat$W==0],theta$mu.nt, sqrt(theta$sigma2.nt))+
    theta$pi[3]*dnorm(dat$Y[dat$Z==0 & dat$W==0],theta$mu.c, sqrt(theta$sigma2.c))
  
  pc.00 <- num/den 
  pc.00[num==0]<-0
  u.00<-rbinom(sum(dat$Z==0 & dat$W==0), 1, pc.00)
  G[dat$Z==0 & dat$W==0] <- 3*(u.00==1) + 1*(u.00==0)
  
  rm(num, den, pc.00, u.00)
  
  num <- theta$pi[3]*dnorm(dat$Y[dat$Z==1 & dat$W==1],theta$mu.c, sqrt(theta$sigma2.c))
  den <- theta$pi[2]*dnorm(dat$Y[dat$Z==1 & dat$W==1],theta$mu.at, sqrt(theta$sigma2.at))+
    theta$pi[3]*dnorm(dat$Y[dat$Z==1 & dat$W==1],theta$mu.c, sqrt(theta$sigma2.c))
  
  pc.11 <- num/den 
  pc.11[num==0]<-0
  u.11<-rbinom(sum(dat$Z==1 & dat$W==1), 1, pc.11)
  G[dat$Z==1 & dat$W==1] <- 3*(u.11==1) + 2*(u.11==0)
  
  rm(num, den, pc.11, u.11)
  
  return(G)
}


#' Calculate mcmc.gaus.h02
#'
#' @param n.iter number of iterations
#' @param n.burn burn-in step
#' @param dat data set
#' @return result.   ## Z=0 & W=0 (NT+C)
#' @export

mcmc.gauss.h02 <- function(n.iter, n.burn, dat){

  n.draws<- n.iter-n.burn
  theta.prior<- list(a=c(1,1, 1))
  
  theta <-   list(pi   = rdirichlet(1, c(1, 1,1)), 
                  mu.nt  = mean(dat$Y) + rnorm(1, 0, 1), 
                  sigma2.nt = runif(1, var(dat$Y)/2, 2*var(dat$Y)),
                  mu.at  = mean(dat$Y) + rnorm(1, 0, 1), 
                  sigma2.at = runif(1, var(dat$Y)/2, 2*var(dat$Y)),
                  mu.c   = mean(dat$Y) + rnorm(1, 0, 1), 
                  sigma2.c = runif(1, var(dat$Y)/2, 2*var(dat$Y))
  )
  THETA<-matrix(0, n.draws, length(unlist(theta)))
  colnames(THETA)<- c("pi.nt", "pi.at", "pi.c", "mu.nt",  "sigma2.nt",
                      "mu.at",  "sigma2.at", "mu.c", "sigma2.c")
  Gstatus <- matrix(0, n.draws, nrow(dat))
  
  for(j in 1:n.iter){
    G<- da.gauss.h02(theta, dat)
    theta$pi.c <- rdirichlet(1, 
                             c({theta.prior$a[1] + sum(G==1)}, {theta.prior$a[2] + sum(G==2)}, {theta.prior$a[3] + sum(G==3)}))
    
    n.nt<- sum(G==1)
    s2.nt <- var(dat$Y[G==1])
    theta$sigma2.nt <- {(n.nt-1)*s2.nt}/ rchisq(1, df={n.nt-1})
    theta$mu.nt <- rnorm(1, mean=mean(dat$Y[G==1]), sd=sqrt(theta$sigma2.nt/n.nt))
    rm(n.nt, s2.nt)
    
    n.at<- sum(G==2)
    s2.at <- var(dat$Y[G==2])
    theta$sigma2.at <- {(n.at-1)*s2.at}/ rchisq(1, df={n.at-1})
    theta$mu.at <- rnorm(1, mean=mean(dat$Y[G==2]), sd=sqrt(theta$sigma2.at/n.at))
    rm(n.at, s2.at)
    
    n.c  <- sum(G==1)
    s2.c <- var(dat$Y[G==3])
    theta$sigma2.c <- {(n.c-1)*s2.c}/ rchisq(1, df={n.c-1})
    theta$mu.c <- rnorm(1, mean=mean(dat$Y[G==3]), sd=sqrt(theta$sigma2.c/n.c))
    rm(n.c, s2.c)
    
    
    if(j > n.burn){
      jj<- j-n.burn
      THETA[jj, ]<-unlist(theta)
      Gstatus[jj,] <-G
    }
    
  } # END LOOP OVER j=1,..., n.iter
  
  list(THETA=THETA, Gstatus=Gstatus)
}





