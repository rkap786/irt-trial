Replicate Ben’s IRT paper
================
radhika
1/21/2021

``` r
#install.packages("dplyr")
library(tidyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr) 
library(ggplot2)
library(readr)
library(norm)
library(knitr)
library(styler)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
#install.packages("kableExtra")

#install.packages("psych")
library(psych)
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(mirt)
```

    ## Loading required package: stats4

    ## Loading required package: lattice

``` r
#install.packages("lmtest")
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

### Setting intial paramters using Kang and Cohen

``` r
a1 <- c(1.1005, 2.2093, 1.4493, 0.7514, 1.5789,  0.6425, 1.6254, 1.3415, 0.918, 1.8027, 0.8159, 0.9375, 0.9126, 1.9395, 0.3746, 0.673, 0.4166, 1.2093, 0.9486, 1.4916)
a2 <- c(0.5659, 0.6128, 1.1037, 1.9886, 0.5691, 1.0346, 1.1384, 3.3488, 2.6306, 0.6652, 1.0342, 1.0163, 1.2945, 1.6521, 0.9696, 1.2369, 0.7812, 0.7728, 0.5441, 1.4025)

b1 <- c("0.4078", "0.5696", "–1.0610", "–0.2437", "0.3206","–1.3762","–0.9800", "–0.6881", "–0.3526", "0.2400", "0.5917", "1.8891", "–0.2690", "0.3673", "–0.9681", "–1.2601", "0.5225", "–1.3356", "0.9515", "0.9811")
b2 <- c("–0.1257", "–0.7826", "0.0615", "0.4244", "–0.7350", "0.9836", "–1.2651", "–0.2252", "–0.6576", "1.7007", "1.0805", "–2.0452", "0.1627", "0.0573", "1.2171", "2.1226", "0.4228", "–0.1656", "–0.2055", "1.2841")

c1 <- c(0.2228, 0.2332, 0.2337, 0.1445, 0.2581, 0.2712, 0.1232, 0.1954, 0.2709, 0.2984, 0.0587,  0.1405, 0.2339, 0.2387,0.3527, 0.1206,0.1244, 0.1167, 0.2787, 0.1923)

c2 <- c(0.3426, 0.1925, 0.2324, 0.1396, 0.2059, 0.3124, 0.1832, 0.1811, 0.2537, 0.2184, 0.2261, 0.3464, 0.1455, 0.3861, 0.1046, 0.1656, 0.2696, 0.178, 0.1961, 0.2917)

# full set
a <- c(a1, a2)

b <- c(b1, b2)
b <- as.numeric(stringr::str_replace(b, "–", "-"))
b <- -b
b1 <- as.numeric(stringr::str_replace(b1, "–", "-"))
b1 <- -b1

c <- c(c1, c2)
```

### Generate data - function

``` r
fun_simulate_data= function(nitem, sample.size, model, a, b,c, ability) {

### Null

a_null<- matrix(rep( 1, len=nitem), ncol = 1)

#Simulate response data 
    if (model == "1PL"){

 dat <- simdata(a = a_null, 
                d = -b, 
                N = sample.size, 
                itemtype = '2PL', 
                Theta = ability)
    }
 
 if (model == "2PL"){

 dat <- simdata(a = a, 
                d = -b, 
                N = sample.size, 
                itemtype = '2PL', 
                Theta = ability)
 }
 
 if (model == "3PL"){

 dat <- simdata(a = a, 
                d = -b, 
                N = sample.size, 
                itemtype = '3PL', 
                guess = c, 
                Theta = ability)
 }

  return(dat)
  
 }

#Check if dataset is correctly generated

# model1PL <- mirt(data=dat, 1,itemtype='Rasch', SE=FALSE, verbose=FALSE, technical = list(NCYCLES=5000))
# coef = as.data.frame(coef(model1PL, simplify=T)$items[,2]) %>%
#   tibble::rownames_to_column(., "Item no") %>%
#   mutate("b"= b) %>%
#   rename(b_est= "coef(model1PL, simplify = T)$items[, 2]")
# 
# plot(coef$b,coef$b_est)

# model3PL <- mirt(data=dat, 1, itemtype='3PL', SE=F, verbose=FALSE)
# coef = as.data.frame(coef(model3PL, simplify=T)$items[,2]) %>%
#   tibble::rownames_to_column(., "Item no") %>%
#   mutate("b"= b) %>%
#   rename(b_est= "coef(model3PL, simplify = T)$items[, 2]")
#  plot(coef$b,coef$b_est)
```

### Split dataset into test and train

``` r
##Function to generate test and train dataset
fun_split_data = function(nitem, sample.size, data) {


m0=matrix(rbinom(nitem*sample.size, 1, .9), ncol=nitem)

m0_na_train <- ifelse(m0==0,NA,m0)
# m0 has 10% of values randomly set to 0. These values are set to missing
m0_na_test <- ifelse(m0==0,1,NA)

# Train dataset
dat_mr_train= data * m0_na_train  # Train dataset - 10% values are set to NA
dat_mr_test= data * m0_na_test # Test dataset - 90% of values are set to NA, keep the 10% values dropped from train

return(list(dat_mr_train, dat_mr_test,m0_na_test))

}
```

### Estimate IRT paramters (true and estimated using IRT), and calculate log likelihood

``` r
### Get estimated paramters
est <- function(mod) {
  co <- coef(mod)
  co <- co[-length(co)]#why do i do this?
  pars <- do.call("rbind", co)
  theta <- fscores(mod, method = "ML", full.scores = TRUE)  
  nc <- ncol(theta)
  if (nc == 1) 
    theta <- as.numeric(theta) else theta <- theta[, ncol(resp) + 1]
  list(theta = theta, pars_diff = pars[, 2], pars_discrim = pars[, 1], pars_guess=pars[,3])
}


## Get estimated probability
get_p <- function(est) {
  n1 <- length(est$theta)
  n2 <- length(est$pars_diff)
  th <- matrix(est$theta, n1, n2, byrow = FALSE)
  b_est <- matrix(est$pars_diff, n1, n2, byrow = TRUE)
  a_est <- matrix(est$pars_discrim, n1, n2, byrow = TRUE)
  c_est <- matrix(est$pars_guess, n1, n2, byrow = TRUE)
  kern <- exp(a_est*(th + b_est))
  round(c_est + (1-c_est)*(kern/(1 + kern)),3)
}

get_p_true <- function(a,b,c,ability) {
  ability <- as.matrix(rnorm(sample.size, mean = 0, sd = 1), ncol=1)
  n1 <- length(ability)
  n2 <- length(b)
  th <- matrix(ability, n1, n2, byrow = FALSE)
  b2 <- matrix(b, n1, n2, byrow = TRUE)
  a2 <- matrix(a, n1, n2, byrow = TRUE)
  c2 <- matrix(c, n1, n2, byrow = TRUE)
  c2 + (1 - c2) * boot::inv.logit(a2 * th + b2)
  #kern <- exp(a2*(th + b2))
  #p_true = c2 + (1-c2)*(kern/(1 + kern))
 }



elplMR_MD <- function(p_model, p_true){
  p_model[p_model<0.0000001] =0.0000001 
  p_model[p_model>0.999999] =0.999999
    p_model_correct <- (p_true* log(p_model) + (1 - p_true)* log(1 - p_model))
    sum(p_model_correct, na.rm=T)
}


#p_model_correct <- log(p_model^p_true * (1 - p_model)^(1 - p_true))
#p= est_3PL_mr^m0_na_test * (1-est_3PL_mr) (1-m0_na_test)

#p_est_1PL[p_est_1PL<0.000000001] =0.000000001 

#check= p_true* log(p_est_1PL) + (1 - p_true) *log(1 - p_est_1PL)
#check1= p_true* log(p_est_2PL) + (1 - p_true) *log(1 - p_est_2PL)
# check1=log(p_est_2PL^p_true * (1 - p_est_2PL)^(1 - p_true))


# sig = function(result) {
# result = data.frame(result)
# 
# result = result %>%
#     arrange(LL)
#   
#   
#   ll_1 = result[1,2]
#   ll_2 = result[2,2]
#   
#   model1PL
#   stat <- -2 * (ll_1pl-ll_2pl)
#   pval <- pchisq(stat, df = 3, lower.tail = FALSE)
#   
# }
```

## Run code chunks above

This is where we: 1. Simulate data 2. Split into test and train 3.
Estimate model paramters using train (using 1PL, 2PL and 3PL) 4. Predict
probability of correct response for full data matrix (for 1PL, 2PL and
3PL) 5. Check which model’s results are closest to the results from true
probability of correct response

``` r
### Predictive fit - Missing response

predict_fit_mr= function(nitem, sample.size, model, a,b,c,ability) {

#Simulate response data 

dat = fun_simulate_data(nitem, sample.size, model, a,b,c,ability)
dat_split = fun_split_data(nitem, sample.size, dat)


### break into test and train
dat_mr_train =  dat_split[[1]] ## some resp marked to NA
dat_mr_test = dat_split[[2]] ##true resp here
m0_na_test = dat_split[[3]] ## tracks which items set to NA

#colMeans(dat_mr_train, na.rm = T)
#colMeans(dat_mr_test, na.rm = T)


#Estimate item parameters for train dataset (with missing data)
model1PL <- mirt(dat_mr_train, 1, itemtype='Rasch', SE=F, verbose=FALSE)
model2PL <- mirt(dat_mr_train, 1, itemtype='2PL', SE=F, verbose=FALSE)
model3PL <- mirt(dat_mr_train, 1, itemtype='3PL', SE=F, verbose=FALSE)
#model3PL_v2 <- mirt(dat, 1, itemtype='3PL', SE=F, verbose=FALSE, technical = list(NCYCLES=5000))


est_1PL_mr = est(model1PL)
est_2PL_mr = est(model2PL)
est_3PL_mr = est(model3PL)
#coef(model3PL)
#coef(model3PL_v2)

#p_check=get_p(est_3PL_mr)
# Predicted probabilities for missing cells
p_est_1PL <- get_p(est_1PL_mr) *m0_na_test
p_est_2PL <- get_p(est_2PL_mr) *m0_na_test
p_est_3PL <- get_p(est_3PL_mr) *m0_na_test

p_true <- get_p_true(a,b,c,ability) *m0_na_test

    
# Calculate log likelihood
results = rbind(c(model = "1PL", LL=round(elplMR_MD(p_est_1PL, p_true),3)),
                c(model = "2PL", LL=round(elplMR_MD(p_est_2PL, p_true),3)),
                c(model = "3PL", LL=round(elplMR_MD(p_est_3PL, p_true),3)))





return(results)
}
```

### Store results

This chuck runs the code above, and stores results for which model won

``` r
#50 items, 1000 respondents
nitem=40
sample.size=500
iter=10
model="3PL"

c1=0
c2=0
c3=0

ability <- as.matrix(rnorm(sample.size, mean = 0, sd = 1), ncol=1)


# a <- as.matrix(rlnorm(nitem, meanlog = 0, sdlog = 0.5), ncol=1) #lognormal
# b <- as.matrix(rnorm(nitem, mean = 1, sd = 1), ncol=1) #normal
# c <- as.matrix(rbeta(nitem, shape1 = 5, shape2 = 17), ncol=1) #beta

for (i in 1:iter){
result1 = predict_fit_mr(nitem,sample.size, model="3PL", a,b,c,ability )
print(result1)
max = which.max(result1[,2])
if(max==1) {c1=c1+1}
if(max==2) {c2=c2+1}
if(max==3) {c3=c3+1}

#print (i)
#print (c1)
#print (c2)
#print (c3)
}
```

    ##      model LL         
    ## [1,] "1PL" "-1888.757"
    ## [2,] "2PL" "-2091.283"
    ## [3,] "3PL" "-2155.528"
    ##      model LL         
    ## [1,] "1PL" "-1937.829"
    ## [2,] "2PL" "-1983.106"
    ## [3,] "3PL" "-2059.228"
    ##      model LL         
    ## [1,] "1PL" "-1898.538"
    ## [2,] "2PL" "-1995.98" 
    ## [3,] "3PL" "-2048.309"
    ##      model LL         
    ## [1,] "1PL" "-1922.639"
    ## [2,] "2PL" "-2000.729"
    ## [3,] "3PL" "-2075.635"

    ## EM cycles terminated after 500 iterations.

    ##      model LL         
    ## [1,] "1PL" "-1912.41" 
    ## [2,] "2PL" "-2035.233"
    ## [3,] "3PL" "-2061.165"
    ##      model LL         
    ## [1,] "1PL" "-1857.41" 
    ## [2,] "2PL" "-1920.037"
    ## [3,] "3PL" "-1974.758"
    ##      model LL         
    ## [1,] "1PL" "-1900.912"
    ## [2,] "2PL" "-2055.395"
    ## [3,] "3PL" "-2108.997"
    ##      model LL         
    ## [1,] "1PL" "-1892.959"
    ## [2,] "2PL" "-1993.295"
    ## [3,] "3PL" "-2050.473"
    ##      model LL         
    ## [1,] "1PL" "-1873.839"
    ## [2,] "2PL" "-2008.136"
    ## [3,] "3PL" "-2067.332"

    ## EM cycles terminated after 500 iterations.

    ##      model LL         
    ## [1,] "1PL" "-1902.788"
    ## [2,] "2PL" "-2000.255"
    ## [3,] "3PL" "-2060.467"

``` r
table = data.frame(
  "DGM" = model,
  "Wins_1PL" = c1,
  "Wins_2PL" = c2,
  "Wins_3PL" = c3
)

print(table)
```

    ##   DGM Wins_1PL Wins_2PL Wins_3PL
    ## 1 3PL       10        0        0

``` r
#knitr::kable(table,
#             col.names = c('DGM', '1PL', '2PL', '3PL'))
```
