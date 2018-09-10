
#' Calculate MLE - EM
#'
#' @param theta theta
#' @param dat dat
#' @return result. We use Conditional probabilities using Bayes' Rule and we calculate the Expected compliance behavior
#' @export

E.step.bin <- function(theta, dat){

  num <- theta$pi.c*dbinom(dat$Y,1,theta$py.c0)        # C,0
  den <- (1-theta$pi.c)*dbinom(dat$Y,1,theta$py.nt) + theta$pi.c*dbinom(dat$Y,1,theta$py.c0) 
  pG <- num/den

  G<-NULL
  G[dat$Z == 1 & dat$W == 1] <- 1
  G[dat$Z == 1 & dat$W == 0] <- 0
  G[dat$Z == 0 & dat$W == 0] <- pG[dat$Z == 0 & dat$W == 0] 
  
  return(G)
}

#' Calculate M.step.bin
#'
#' @param G G
#' @param dat dat
#' @return result. 
#' @export

M.step.bin <- function(G, dat){
  theta<-list()
 theta$pi.c <- mean(G)    
  
 theta$py.c0<-sum(dat$Y*(1-dat$Z)*G)/sum((1-dat$Z)*G)
 theta$py.c1<-sum(dat$Y*dat$Z*G)/sum(dat$Z*G)
 theta$py.nt<-sum(dat$Y*(1-G))/sum((1-G))
  
 return(theta)
  
}

#' plus
#'
#' @param x x
#' @return result. 
#' @export

plus<-function(x){
  yy<-(x+0.01)
  (x+0.01)*(yy<1)+(x-0.01)*(yy>1)
}

#' Calculate EM.bin
#'
#' @param G G
#' @param tol tol
#' @param maxit maxit
#' @return result. tolerance fixed at 0.0001
#' @export

EM.bin <- function(dat, tol = 1e-4, maxit = 1000){
  
  
  # Starting points ###########################################################
  
theta <- list(pi.c  = rbeta(1,1,1),
               py.nt = rbeta(1,1,1),
               py.c0 = rbeta(1,1,1),
               py.c1 = rbeta(1,1,1))
                
  
  
 theta1<- lapply(theta,plus)
 
 
  # EM algorithm ###############################################################
  
  n.it <- 0
  #printing.time <- 0
  
  while(
    max(abs(unlist(theta1) - unlist(theta))) > tol 
    
  ){
    
    theta1 <- theta
    
    G     <- E.step.bin(theta, dat)
    theta <- M.step.bin(G, dat)

    n.it <- n.it + 1
    if(n.it == maxit){break}
  }
  
  out <- list(theta = theta, G=G,  converged = (n.it < maxit))
  CACE.MLE <- theta$py.c1-theta$py.c0
  #rm(phi)
  return(CACE.MLE)
}

#' Calculate POSTERIOR MEAN - DA + MCMC
#'
#' @param theta theta
#' @param tol tol
#' @return result. Impute missing compliance statuses under the null, ## Z=0 & W=0 (NT+C)
#' @export

da.bin <-  function(theta, dat){
  
  G<- NULL  
  G[dat$Z==1 & dat$W==1] <- 1 ##Compliers
  G[dat$Z==1 & dat$W==0] <- 0 ##Never - takers
  
  
  num <- theta$pi.c*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.c0)
  den <- (1-theta$pi.c)*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.nt)+
            theta$pi.c*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.c0)
  
  pc.00 <- num/den 
  G[dat$Z==0 & dat$W==0]<- rbinom(sum(dat$Z==0 & dat$W==0), 1, pc.00)
 
  rm(num, den, pc.00)
   
  return(G)
}

#' Calculate mcmc.bin
#'
#' @param n.iter number of iterations
#' @param n.burn burn-in step
#' @param dat data set
#' @return result. 
#' @export

mcmc.bin <- function(n.iter, n.burn, dat){
  
  n.draws<- n.iter-n.burn
  theta.prior <- list(a = c(1,1), ay.nt = c(1,1), ay.c0= c(1,1), ay.c1= c(1,1))
  
  theta <-   list(pi.c   = rbeta(1, theta.prior$a[1], theta.prior$a[2]), 
                  py.nt  = rbeta(1, theta.prior$ay.nt[1], theta.prior$ay.nt[2]),
                  py.c0  = rbeta(1, theta.prior$ay.c0[1],  theta.prior$ay.c0[2]),
                  py.c1  = rbeta(1, theta.prior$ay.c1[1],  theta.prior$ay.c1[2]))

  THETA<-matrix(0, n.draws, length(unlist(theta)))
  colnames(THETA)<- c( "pi.c", "py.nt",  "py.c0", "py.c1")
  
  CACE <- NULL
  
  for(j in 1:n.iter){
   
    G<- da.bin(theta, dat)
    ###Draw the parameters (under the null)
    theta$pi.c <- rbeta(1, {theta.prior$a[1] + sum(G==1)}, {theta.prior$a[2] + sum(G==0)})
    
    theta$py.nt <- rbeta(1,{theta.prior$ay.nt[1] + sum(G==0 & dat$Y==1)},  
                           {theta.prior$ay.nt[2] + sum(G==0 & dat$Y==0)})

    theta$py.c0 <- rbeta(1,{theta.prior$ay.c0[1] + sum(G==1 & dat$Z==0 & dat$Y==1)},  
                           {theta.prior$ay.c0[2] + sum(G==1 & dat$Z==0 & dat$Y==0)})
    
    theta$py.c1 <- rbeta(1,{theta.prior$ay.c1[1] + sum(G==1 & dat$Z==1 & dat$Y==1)},  
                         {theta.prior$ay.c1[2] + sum(G==1 & dat$Z==1 & dat$Y==0)})
    
    if(j > n.burn){
      jj <- j - n.burn 
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

da.bin.h0 <-  function(theta, dat){
  
  G<- NULL  
  G[dat$Z==1 & dat$W==0] <- 0 ##Never - takers
  G[dat$Z==1 & dat$W==1] <- 1 ##Compliers
  
 
  num <- theta$pi.c*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.c)
  den <- (1-theta$pi.c)*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.nt)+
            theta$pi.c*dbinom(dat$Y[dat$Z==0 & dat$W==0],1, theta$py.c)
  
  pc.00 <- num/den 
  G[dat$Z==0 & dat$W==0] <- rbinom(sum(dat$Z==0 & dat$W==0), 1, pc.00)
   
  rm(num, den, pc.00)
   
  return(G)
}

#' Calculate mcmc.bin.h0
#'
#' @param n.iter number of iterations
#' @param n.burn burn-in step
#' @param dat data set
#' @return result.   ## Z=0 & W=0 (NT+C)
#' @export

mcmc.bin.h0 <- function(n.iter, n.burn, dat){

  n.draws<- n.iter-n.burn
  theta.prior <- list(a = c(1,1), ay.nt = c(1,1), ay.at = c(1,1),  ay.c= c(1,1))
  
  theta <-   list(pi.c  = rbeta(1, theta.prior$a[1], theta.prior$a[2]), 
                  py.nt = rbeta(1, theta.prior$ay.nt[1], theta.prior$ay.nt[2]),
                  py.c  = rbeta(1, theta.prior$ay.c[1],  theta.prior$ay.c[2]))
  
  THETA<-matrix(0, n.draws, length(unlist(theta)))
  colnames(THETA)<- c("pi.c", "py.nt",  "py.c")
  Gstatus <- matrix(0, n.draws, nrow(dat))
  
  for(j in 1:n.iter){
  G<- da.bin.h0(theta, dat)
  theta$pi.c <- rbeta(1, {theta.prior$a[1] + sum(G==1)}, {theta.prior$a[2] + sum(G==0)})
  
  theta$py.nt <- rbeta(1,{theta.prior$ay.nt[1] + sum(G==0 & dat$Y==1)},  
                         {theta.prior$ay.nt[2] + sum(G==0 & dat$Y==0)})

  theta$py.c <- rbeta(1,{theta.prior$ay.c[1] + sum(G==1 & dat$Y==1)},  
                        {theta.prior$ay.c[2] + sum(G==1 & dat$Y==0)})
  if(j > n.burn){
    jj<- j-n.burn
    THETA[jj, ]<-unlist(theta)
    Gstatus[jj,] <-G
  }
  
} # END LOOP OVER j=1,..., n.iter

  list(THETA=THETA, Gstatus=Gstatus)
}


####################################################################################
####################################################################################
### CONTINUOUS OUTCOME
####################################################################################
####################################################################################
library(geoR) #Scaled-inverse chisq
 ###MLE - EM
 E.step.gauss <- function(theta, dat){
   
   # Conditional probabilities using Bayes' Rule
   
   num <- theta$pi.c*dnorm(dat$Y,theta$mu.c0,sqrt(theta$sigma2.c0))        # C,0
   den <- (1-theta$pi.c)*dnorm(dat$Y,theta$mu.nt,sqrt(theta$sigma2.nt)) + theta$pi.c*dnorm(dat$Y,theta$mu.c0,sqrt(theta$sigma2.c0)) 
   pG <- num/den
   
   # Expected compliance behavior
   G<-NULL
   G[dat$Z == 1 & dat$W == 1] <- 1
   G[dat$Z == 1 & dat$W == 0] <- 0
   G[dat$Z == 0 & dat$W == 0] <- pG[dat$Z == 0 & dat$W == 0] 
   
   return(G)
 }
 

 M.step.gauss<- function(G, dat){
   theta<-list()
   theta$pi.c <- mean(G)    
   
   theta$mu.c0<-sum(dat$Y*(1-dat$Z)*G)/sum((1-dat$Z)*G)
   theta$mu.c1<-sum(dat$Y*dat$Z*G)/sum(dat$Z*G)
   theta$mu.nt<-sum(dat$Y*(1-G))/sum((1-G))
    
   
   theta$sigma2.c0 <- sum((dat$Y-theta$mu.c0)^2*(1 - dat$Z)*G)/sum((1 - dat$Z)*G)                 # C,0
   theta$sigma2.c1 <- sum((dat$Y-theta$mu.c1)^2*dat$Z*G)/sum(dat$Z*G)                            # C,1
   theta$sigma2.nt <- sum((dat$Y-theta$mu.nt)^2*(1 - G))/sum((1 - G)) 
   
   return(theta)
   
 }

#tolerance fixed at 0.0001
EM.gauss <- function(dat, tol = 1e-4, maxit = 1000){


  # Starting points ###########################################################

  theta <- list(pi.c  = rbeta(1,1,1),
          mu.nt = mean(dat$Y) + rnorm(1,0,sd(dat$Y)), sigma2.nt = runif(1,0,var(dat$Y)),
          mu.c0 = mean(dat$Y) + rnorm(1,0,sd(dat$Y)), sigma2.c0 = runif(1,0,var(dat$Y)),
          mu.c1 = mean(dat$Y) + rnorm(1,0,sd(dat$Y)), sigma2.c1 = runif(1,0,var(dat$Y))
          )

  plus<-function(x){
    yy<-x+1
    return(yy)
  }

  theta1<- lapply(theta,plus)


  # EM algorithm ###############################################################

  n.it <- 0
  #printing.time <- 0

  while(
    max(abs(unlist(theta1) - unlist(theta))) > tol

  ){

    theta1 <- theta

    G     <- E.step.gauss(theta, dat)
    theta <- M.step.gauss(G, dat)

    n.it <- n.it + 1
    if(n.it == maxit){break}
  }

  out <- list(theta = theta, G=G,  converged = (n.it < maxit))
  CACE.MLE <- theta$mu.c1-theta$mu.c0
  #rm(phi)
  return(CACE.MLE)
}

###POSTERIOR MEAN - DA + MCMC
da.gauss <-  function(theta, dat){
  
  ## Impute missing compliance statuses under the null
  G<- NULL  
  G[dat$Z==1 & dat$W==1] <- 1 ##Compliers
  G[dat$Z==1 & dat$W==0] <- 0 ##Never - takers
  
  ## Z=0 & W=0 (NT+C)
  num <- theta$pi.c*dnorm(dat$Y[dat$Z==0 & dat$W==0], theta$mu.c0, sqrt(theta$sigma2.c0))
  den <- (1-theta$pi.c)*dnorm(dat$Y[dat$Z==0 & dat$W==0], theta$mu.nt, sqrt(theta$sigma2.nt)) +
    theta$pi.c*dnorm(dat$Y[dat$Z==0 & dat$W==0], theta$mu.c0, sqrt(theta$sigma2.c0))
  
  pc.00 <- num/den 
  pc.00[num==0]<-0
  G[dat$Z==0 & dat$W==0]<- rbinom(sum(dat$Z==0 & dat$W==0), 1, pc.00)
  
  rm(num, den, pc.00)
  
  return(G)
}

mcmc.gauss <- function(n.iter, n.burn, dat){
  
  ## INPUT
  ## n.iter = number of iterations
  ## n.burn = burn-in step
  ## dat= data set
  
  n.draws<- n.iter-n.burn
  ## theta.prior = list with parameters for the prior (Uniform prior)
  theta.prior<- list(a=c(1,1))
  ##theta  = list with  pi.c, mu.nt  sigma2.nt mu.c0 sigma2.c0 mu.c1 sigma2.c1
  ##Initial values 
  theta <-   list(pi.c   = rbeta(1, 1, 1), 
                  mu.nt  = mean(dat$Y) + rnorm(1, 0, 1), 
                  sigma2.nt = runif(1, var(dat$Y)/2, 2*var(dat$Y)),
                  mu.c0  = mean(dat$Y) + rnorm(1, 0, 1), 
                  sigma2.c0 = runif(1, var(dat$Y)/2, 2*var(dat$Y)),
                  mu.c1  = mean(dat$Y) + rnorm(1, 0, 1), 
                  sigma2.c1 = runif(1, var(dat$Y)/2, 2*var(dat$Y))
                )
  
  ##OUTPUT : THETA
  THETA<-matrix(0, n.draws, length(unlist(theta)))
  colnames(THETA)<- c( "pi.c", "mu.nt", "sigma2.nt",  
                       "mu.c0", "sigma2.c0", "mu.c1", "sigma2.c1")
  
  CACE <- NULL
  
  for(j in 1:n.iter){
    #print(j)
    G<- da.gauss(theta, dat)
    ###Draw the parameters (under the null)
    
    theta$pi.c <- rbeta(1, {theta.prior$a[1] + sum(G==1)}, {theta.prior$a[2] + sum(G==0)})
        
    
#    n.nt<- sum(G==0)
#    sc.nt <- sum({dat$Y[G==0]-theta$mu.nt}^2)
#    theta$sigma2.nt <- {sc.nt}/ rchisq(1, df=n.nt)
#    theta$mu.nt <- rnorm(1, mean=mean(dat$Y[G==0]), sd=sqrt(theta$sigma2.nt/n.nt))
#    rm(n.nt, sc.nt)
#    
#    n.c0<- sum(G==1 & dat$Z==0)
#    sc.c0 <- sum({dat$Y[G==1 & dat$Z==0]-theta$mu.c0}^2)
#    theta$sigma2.c0 <- {sc.c0}/ rchisq(1, df=n.c0)
#    if(theta$sigma2.c0==0){
#      print(unlist(theta))
#      print(sc.c0)
#      print(n.c0)
#    print(dat$Y[G==1 & dat$Z==0])
#    }
#    theta$mu.c0 <- rnorm(1, mean=mean(dat$Y[G==1 & dat$Z==0]), sd=sqrt(theta$sigma2.c0/n.c0))
#    rm(n.c0, sc.c0)
#    
#    n.c1  <- sum(G==1 & dat$Z==1)
#    sc.c1 <- sum({dat$Y[G==1 & dat$Z==1]-theta$mu.c1}^2)
#    theta$sigma2.c1 <- {sc.c1}/ rchisq(1, df=n.c1)
#    theta$mu.c1 <- rnorm(1, mean=mean(dat$Y[G==1 & dat$Z==1]), sd=sqrt(theta$sigma2.c1/n.c1))
#    rm(n.c1, sc.c1)
    
     ##Drawing from the joint distribution of mu, sigma2 for NT
     n.nt<- sum(G==0)
     s2.nt <- var(dat$Y[G==0])
     theta$sigma2.nt <- {(n.nt-1)*s2.nt}/ rchisq(1, df={n.nt-1})
     theta$mu.nt <- rnorm(1, mean=mean(dat$Y[G==0]), sd=sqrt(theta$sigma2.nt/n.nt))
     rm(n.nt, s2.nt)
	 
	 
	 ##Drawing from the joint distribution of mu, sigma2 for C,0
     n.c0<- sum(G==1 & dat$Z==0)
     s2.c0 <- var(dat$Y[G==1 & dat$Z==0])
     theta$sigma2.c0 <- {(n.c0-1)*s2.c0}/ rchisq(1, df={n.c0-1})
     theta$mu.c0 <- rnorm(1, mean=mean(dat$Y[G==1 & dat$Z==0]), sd=sqrt(theta$sigma2.c0/n.c0))
     rm(n.c0, s2.c0)
     
	 ##Drawing from the joint distribution of mu, sigma2 for C,1
     n.c1<- sum(G==1 & dat$Z==1)
     s2.c1 <- var(dat$Y[G==1 & dat$Z==1])
     theta$sigma2.c1 <- {(n.c1-1)*s2.c1}/ rchisq(1, df={n.c1-1})
     theta$mu.c1 <- rnorm(1, mean=mean(dat$Y[G==1 & dat$Z==1]), sd=sqrt(theta$sigma2.c1/n.c1))
     rm(n.c1, s2.c1)
    

   # print(table(G,dat$Z))
  
  
    if(j > n.burn){
      jj <- j - n.burn 
      THETA[jj, ]<-unlist(theta)
      CACE[jj]<-  theta$mu.c1- theta$mu.c0
    }
  } # END LOOP OVER j=1,..., n.iter
  
  
  CACE.PM <-median(CACE)   
  
  #return(THETA)
  return(CACE.PM)
  
}

########################################################
###Impute missing compliance statuses under the NULL
########################################################

da.gauss.h0 <-  function(theta, dat){
  
  ## Impute missing compliance statuses under the null
  G<- NULL  
  G[dat$Z==1 & dat$W==0] <- 0 ##Never - takers
  G[dat$Z==1 & dat$W==1] <- 1 ##Compliers
  
  ## Z=0 & W=0 (NT+C)
  num <- theta$pi.c*dnorm(dat$Y[dat$Z==0 & dat$W==0],theta$mu.c, sqrt(theta$sigma2.c))
  den <- (1-theta$pi.c)*dnorm(dat$Y[dat$Z==0 & dat$W==0],theta$mu.nt, sqrt(theta$sigma2.nt))+
    theta$pi.c*dnorm(dat$Y[dat$Z==0 & dat$W==0],theta$mu.c, sqrt(theta$sigma2.c))
  
  pc.00 <- num/den 
  pc.00[num==0]<-0
  G[dat$Z==0 & dat$W==0] <- rbinom(sum(dat$Z==0 & dat$W==0), 1, pc.00)
  
  rm(num, den, pc.00)
  
  return(G)
}

mcmc.gauss.h0 <- function(n.iter, n.burn, dat){
  
  ## INPUT
  ## n.iter = number of iterations
  ## n.burn = burn-in step
  ## dat= data set
  
  n.draws<- n.iter-n.burn
  ## theta.prior = list with parameters for the prior (uniform)
  theta.prior<- list(a=c(1,1))

  ## Under the null theta  = list with  pi.c, mu.nt, sigma2.nt, mu.c sigma2.c
  ##Initial values 
  ##Initial values 
  theta <-   list(pi.c   = rbeta(1, 1, 1), 
		  mu.nt  = mean(dat$Y) + rnorm(1, 0, 1), 
		  sigma2.nt = runif(1, var(dat$Y)/2, 2*var(dat$Y)),
		  mu.c   = mean(dat$Y) + rnorm(1, 0, 1), 
		  sigma2.c = runif(1, var(dat$Y)/2, 2*var(dat$Y))
  			)
  ##OUTPUT : THETA
  THETA<-matrix(0, n.draws, length(unlist(theta)))
  colnames(THETA)<- c("pi.c", "mu.nt",  "sigma2.nt", "mu.c", "sigma2.c")
  Gstatus <- matrix(0, n.draws, nrow(dat))
  
  for(j in 1:n.iter){
    G<- da.gauss.h0(theta, dat)
    ###Draw the parameters (under the null)
    theta$pi.c <- rbeta(1, {theta.prior$a[1] + sum(G==1)}, {theta.prior$a[2] + sum(G==0)})
    
    n.nt<- sum(G==0)
    s2.nt <- var(dat$Y[G==0])
    theta$sigma2.nt <- {(n.nt-1)*s2.nt}/ rchisq(1, df={n.nt-1})
    theta$mu.nt <- rnorm(1, mean=mean(dat$Y[G==0]), sd=sqrt(theta$sigma2.nt/n.nt))
    rm(n.nt, s2.nt)
    
    n.c  <- sum(G==1)
    s2.c <- var(dat$Y[G==1])
    theta$sigma2.c <- {(n.c-1)*s2.c}/ rchisq(1, df={n.c-1})
    theta$mu.c <- rnorm(1, mean=mean(dat$Y[G==1]), sd=sqrt(theta$sigma2.c/n.c))
    rm(n.c, s2.c)
    
    
    if(j > n.burn){
      jj<- j-n.burn
      THETA[jj, ]<-unlist(theta)
      Gstatus[jj,] <-G
    }
    
  } # END LOOP OVER j=1,..., n.iter
  
  list(THETA=THETA, Gstatus=Gstatus)
}


