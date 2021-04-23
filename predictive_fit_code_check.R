a <- as.matrix(rlnorm(nitem, meanlog = 0, sdlog = 1), ncol=1) #lognormal
b <- as.matrix(rnorm(nitem, mean = 0, sd = 1), ncol=1) #normal
c <- as.matrix(rbeta(nitem, shape1 = 5, shape2 = 17), ncol=1) #beta
ability <- as.matrix(rnorm(sample.size, mean = 0, sd = 1), ncol=1)

## Generate data using 3PL
dat <- simdata(a = a, 
               d = b, 
               N = sample.size, 
               itemtype = '3PL', 
               guess = c, 
               Theta = ability)


## Fit model using 3PL
model3PL <- mirt(data=dat, 1, itemtype='3PL', SE=F, verbose=FALSE)
coef = as.data.frame(coef(model3PL, simplify=T)$items[,2]) %>%
  tibble::rownames_to_column(., "Item no") %>%
  mutate("b"= b) %>%
  rename(b_est= "coef(model3PL, simplify = T)$items[, 2]")

### Does this look ok?
plot(coef$b,coef$b_est)
plot(a,coef(model3PL, simplify=T)$items[,1])
plot(c,coef(model3PL, simplify=T)$items[,3])

## Get model parameters 
est <- function(mod) {
  co <- coef(mod)
  co <- co[-length(co)]#why do i do this?
  pars <- do.call("rbind", co)
  theta <- fscores(mod, method = "ML", full.scores = TRUE)  ##note: this is where the ability scoring happens. we'll talk about the details of this component next week.
  nc <- ncol(theta)
  if (nc == 1) 
    theta <- as.numeric(theta) else theta <- theta[, ncol(resp) + 1]
  list(theta = theta, pars_diff = pars[, 2], pars_discrim = pars[, 1], pars_guess=pars[,3])
}


  n1 <- length(est_1PL$theta)
  n2 <- length(est_1PL$pars_diff)
  th <- matrix(est_1PL$theta, n1, n2, byrow = FALSE)
  b_est <- matrix(est_1PL$pars_diff, n1, n2, byrow = TRUE)
  a_est <- matrix(est_1PL$pars_discrim, n1, n2, byrow = TRUE)
  c_est <- matrix(est_1PL$pars_guess, n1, n2, byrow = TRUE)
  kern <- exp(a_est*(th + b_est))
  p=c_est + (1-c_est)*(kern/(1 + kern))
  plot(th[,5],p[,5])



est_1PL = est(model3PL)
est_1PL$pars_diff

  pars <- coef(model3PL, simplify=T)$items
  theta <- fscores(model3PL, method = "ML", full.scores = TRUE)  
  nc <- ncol(theta)
  if (nc == 1) 
    theta <- as.numeric(theta) else theta <- theta[, ncol(dat) + 1]
 
## Probability of correct response
  b_est = -pars[,2]
  a_est= pars[,1]
  c_est= pars[,3]
  kern <- exp(a_est*(theta + b_est))
  p=c_est + (1-c_est)*(kern/(1 + kern))
  plot(theta,p)
  
  b_est <- matrix(est$pars_diff, n1, n2, byrow = TRUE)
    a_est <- matrix(est$pars_discrim, n1, n2, byrow = TRUE)
    c_est <- matrix(est$pars_guess, n1, n2, byrow = TRUE)
    kern <- exp(a_est*(th + b_est))
    c_est + (1-c_est)*(kern/(1 + kern))
    plot(th[,1],p[,1])
  }
  
  
plot(theta)

### Get probability
get_p <- function(est) {
  n1 <- length(est$theta)
  n2 <- length(est$pars_diff)
  th <- matrix(est$theta, n1, n2, byrow = FALSE)
  b_est <- matrix(est$pars_diff, n1, n2, byrow = TRUE)
  a_est <- matrix(est$pars_discrim, n1, n2, byrow = TRUE)
  c_est <- matrix(est$pars_guess, n1, n2, byrow = TRUE)
  kern <- exp(a_est*(th + b_est))
  c_est + (1-c_est)*(kern/(1 + kern))
  plot(th[,1],p[,1])
}


