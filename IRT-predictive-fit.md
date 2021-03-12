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

``` r
### Kang & Cohen parameters
# 
# a1 <- c(1.1005, NA, 2.2093, NA, 1.4493, NA, 0.7514, NA, 1.5789, NA, 0.6425, NA, 1.6254, NA, 1.3415, NA, 0.918, NA, 1.8027, NA, 0.8159, NA, 0.9375, NA, 0.9126, NA, 1.9395, NA, 0.3746, NA, 0.673, NA, 0.4166, NA, 1.2093, NA, 0.9486, NA, 1.4916)
# b1 <- c("0.4078", NA, "0.5696", NA, "–1.0610", NA, "–0.2437", NA, "0.3206", NA, "–1.3762", NA, "–0.9800", NA, "–0.6881", NA, "–0.3526", NA, "0.2400", NA, "0.5917", NA, "1.8891", NA, "–0.2690", NA, "0.3673", NA, "–0.9681", NA, "–1.2601", NA, "0.5225", NA, "–1.3356", NA, "0.9515", NA, "0.9811")
# c1 <- c(0.2228, NA, 0.2332, NA, 0.2337, NA, 0.1445, NA, 0.2581, NA, 0.2712, NA, 0.1232, NA, 0.1954, NA, 0.2709, NA, 0.2984, NA, 0.0587, NA, 0.1405, NA, 0.2339, NA, 0.2387, NA, 0.3527, NA, 0.1206, NA, 0.1244, NA, 0.1167, NA, 0.2787, NA, 0.1923)
# a2 <- c(0.5659, NA, 0.6128, NA, 1.1037, NA, 1.9886, NA, 0.5691, NA, 1.0346, NA, 1.1384, NA, 3.3488, NA, 2.6306, NA, 0.6652, NA, 1.0342, NA, 1.0163, NA, 1.2945, NA, 1.6521, NA, 0.9696, NA, 1.2369, NA, 0.7812, NA, 0.7728, NA, 0.5441, NA, 1.4025)
# b2 <- c("–0.1257", NA, "–0.7826", NA, "0.0615", NA, "0.4244", NA, "–0.7350", NA, "0.9836", NA, "–1.2651", NA, "–0.2252", NA, "–0.6576", NA, "1.7007", NA, "1.0805", NA, "–2.0452", NA, "0.1627", NA, "0.0573", NA, "1.2171", NA, "2.1226", NA, "0.4228", NA, "–0.1656", NA, "–0.2055", NA, "1.2841")
# c2 <- c(0.3426, NA, 0.1925, NA, 0.2324, NA, 0.1396, NA, 0.2059, NA, 0.3124, NA, 0.1832, NA, 0.1811, NA, 0.2537, NA, 0.2184, NA, 0.2261, NA, 0.3464, NA, 0.1455, NA, 0.3861, NA, 0.1046, NA, 0.1656, NA, 0.2696, NA, 0.178, NA, 0.1961, NA, 0.2917)
# 
# # clean up
# a <- c(a1, a2)
# a <- a[!is.na(a)]
# b <- c(b1, b2)
# b <- as.numeric(stringr::str_replace(b, "–", "-"))
# b <- b[!is.na(b)]
# b <- -b
# c <- c(c1, c2)
# c <- c[!is.na(c)]
# 
# a1=a1[!is.na(a1)]
# 
# b1 <- as.numeric(stringr::str_replace(b1, "–", "-"))
# b1 <- b1[!is.na(b1)]
# b1 <- -b1
# 
# c1=c1[!is.na(c1)]
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

# model1PL <- mirt(data=dat, 1,itemtype='Rasch', SE=FALSE, verbose=FALSE)
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

``` r
### Get estimated paramters
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
}

elplMR_MD <- function(p_model, p_true){
    p_model_correct <- log(p_model^p_true * (1 - p_model)^(1 - p_true))
    p_model_correct=ifelse(is.finite(p_model_correct),p_model_correct,0)
    sum(p_model_correct)
}
```

``` r
### Predictive fit - Missing response

predict_fit_mr= function(nitem, sample.size, model, a,b,c,ability) {

#Simulate response data 

dat = fun_simulate_data(nitem, sample.size, model, a,b,c,ability)
dat_split = fun_split_data(nitem, sample.size, dat)

### break into test and train
dat_mr_train =  dat_split[[1]]
dat_mr_test = dat_split[[2]]
m0_na_test = dat_split[[3]]

#Estimate item parameters for train dataset (with missing data)
model1PL <- mirt(dat_mr_train, 1, itemtype='Rasch', SE=F, verbose=FALSE)
model2PL <- mirt(dat_mr_train, 1, itemtype='2PL', SE=F, verbose=FALSE)
model3PL <- mirt(dat_mr_train, 1, itemtype='3PL', SE=F, verbose=FALSE)


est_1PL_mr = est(model1PL)
est_2PL_mr = est(model2PL)
est_3PL_mr = est(model3PL)


# Predicted probabilities for missing cells
p_est_1PL <- get_p(est_1PL_mr) *m0_na_test
p_est_2PL <- get_p(est_2PL_mr) *m0_na_test
p_est_3PL <- get_p(est_3PL_mr) *m0_na_test

    
# Calculate log likelihood
results = rbind(c(model = "1PL", LL=round(elplMR_MD(p_est_1PL, dat_mr_test),3)),
                c(model = "2PL", LL=round(elplMR_MD(p_est_2PL, dat_mr_test),3)),
                c(model = "3PL", LL=round(elplMR_MD(p_est_3PL, dat_mr_test),3)))

return(results)
}
```

``` r
#50 items, 1000 respondents
nitem=20
sample.size=500
iter=500

c1=0
c2=0
c3=0

ability <- as.matrix(rnorm(sample.size, mean = 0, sd = 1), ncol=1)

a <- as.matrix(rlnorm(nitem, meanlog = 0, sdlog = 1), ncol=1) #lognormal
b <- as.matrix(rnorm(nitem, mean = 0, sd = 1), ncol=1) #normal
c <- as.matrix(rbeta(nitem, shape1 = 5, shape2 = 17), ncol=1) #beta

for (i in 1:iter){
result1 = predict_fit_mr(nitem,sample.size, model = "3PL", a,b,c,ability )
max = which.max(result1[,2])
if(max==1) {c1=c1+1}
if(max==2) {c2=c2+1}
if(max==3) {c3=c3+1}

print(i)
print(c1)
print(c2)
print(c3)
print(result1)
print(min)

}
```

    ## [1] 1
    ## [1] 1
    ## [1] 0
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-708.46" 
    ## [2,] "2PL" "-782.106"
    ## [3,] "3PL" "-953.535"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 2
    ## [1] 1
    ## [1] 1
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-717.007"
    ## [2,] "2PL" "-682.472"
    ## [3,] "3PL" "-787.099"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 3
    ## [1] 2
    ## [1] 1
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-718.405" 
    ## [2,] "2PL" "-851.868" 
    ## [3,] "3PL" "-1005.084"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 4
    ## [1] 3
    ## [1] 1
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-722.431"
    ## [2,] "2PL" "-761.829"
    ## [3,] "3PL" "-942.954"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 5
    ## [1] 4
    ## [1] 1
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-777.917"
    ## [2,] "2PL" "-838.827"
    ## [3,] "3PL" "-942.982"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 6
    ## [1] 4
    ## [1] 2
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-727.641"
    ## [2,] "2PL" "-703.98" 
    ## [3,] "3PL" "-888.942"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 7
    ## [1] 5
    ## [1] 2
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-713.83" 
    ## [2,] "2PL" "-732.356"
    ## [3,] "3PL" "-816.566"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 8
    ## [1] 6
    ## [1] 2
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-676.11" 
    ## [2,] "2PL" "-725.139"
    ## [3,] "3PL" "-852.829"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 9
    ## [1] 7
    ## [1] 2
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-688.808"
    ## [2,] "2PL" "-727.021"
    ## [3,] "3PL" "-912.003"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 10
    ## [1] 8
    ## [1] 2
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-708.367"
    ## [2,] "2PL" "-764.623"
    ## [3,] "3PL" "-958.826"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 11
    ## [1] 9
    ## [1] 2
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-708.93" 
    ## [2,] "2PL" "-755.017"
    ## [3,] "3PL" "-867.468"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 12
    ## [1] 10
    ## [1] 2
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-732.792"
    ## [2,] "2PL" "-842.959"
    ## [3,] "3PL" "-970.435"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 13
    ## [1] 11
    ## [1] 2
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-745.525"
    ## [2,] "2PL" "-757.72" 
    ## [3,] "3PL" "-903.98" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 14
    ## [1] 11
    ## [1] 3
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-690.546"
    ## [2,] "2PL" "-666.804"
    ## [3,] "3PL" "-826.946"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 15
    ## [1] 11
    ## [1] 4
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-722.177"
    ## [2,] "2PL" "-677.987"
    ## [3,] "3PL" "-807.137"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 16
    ## [1] 12
    ## [1] 4
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-677.734"
    ## [2,] "2PL" "-685.84" 
    ## [3,] "3PL" "-848.058"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 17
    ## [1] 13
    ## [1] 4
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-729.553"
    ## [2,] "2PL" "-795.213"
    ## [3,] "3PL" "-871.211"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 18
    ## [1] 14
    ## [1] 4
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-717.141"
    ## [2,] "2PL" "-783.632"
    ## [3,] "3PL" "-885.393"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 19
    ## [1] 14
    ## [1] 5
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-732.695"
    ## [2,] "2PL" "-698.632"
    ## [3,] "3PL" "-871.049"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 20
    ## [1] 15
    ## [1] 5
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-799.099"
    ## [2,] "2PL" "-807.689"
    ## [3,] "3PL" "-934.317"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 21
    ## [1] 16
    ## [1] 5
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-698.901"
    ## [2,] "2PL" "-716.915"
    ## [3,] "3PL" "-841.92" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 22
    ## [1] 17
    ## [1] 5
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-761.142"
    ## [2,] "2PL" "-771.475"
    ## [3,] "3PL" "-878.008"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 23
    ## [1] 17
    ## [1] 6
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-717.823"
    ## [2,] "2PL" "-685.783"
    ## [3,] "3PL" "-885.09" 
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 24
    ## [1] 18
    ## [1] 6
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-717.564"
    ## [2,] "2PL" "-740.368"
    ## [3,] "3PL" "-854.368"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 25
    ## [1] 18
    ## [1] 7
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-738.656"
    ## [2,] "2PL" "-724.438"
    ## [3,] "3PL" "-877.392"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 26
    ## [1] 19
    ## [1] 7
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-741.562"
    ## [2,] "2PL" "-782.165"
    ## [3,] "3PL" "-925.387"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 27
    ## [1] 20
    ## [1] 7
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-728.277"
    ## [2,] "2PL" "-792.606"
    ## [3,] "3PL" "-938.185"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 28
    ## [1] 21
    ## [1] 7
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-736.994"
    ## [2,] "2PL" "-801.284"
    ## [3,] "3PL" "-926.261"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 29
    ## [1] 22
    ## [1] 7
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-728.807"
    ## [2,] "2PL" "-874.617"
    ## [3,] "3PL" "-961.34" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 30
    ## [1] 22
    ## [1] 8
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-703.489"
    ## [2,] "2PL" "-683.233"
    ## [3,] "3PL" "-849.326"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 31
    ## [1] 22
    ## [1] 9
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-745.051"
    ## [2,] "2PL" "-722.903"
    ## [3,] "3PL" "-863.911"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 32
    ## [1] 22
    ## [1] 10
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-711.464"
    ## [2,] "2PL" "-692.597"
    ## [3,] "3PL" "-904.745"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 33
    ## [1] 23
    ## [1] 10
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-790.745"
    ## [2,] "2PL" "-913.773"
    ## [3,] "3PL" "-935.278"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 34
    ## [1] 24
    ## [1] 10
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-744.041"
    ## [2,] "2PL" "-776.124"
    ## [3,] "3PL" "-973.628"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 35
    ## [1] 25
    ## [1] 10
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-763.605" 
    ## [2,] "2PL" "-898.836" 
    ## [3,] "3PL" "-1089.553"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 36
    ## [1] 26
    ## [1] 10
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-776.82"  
    ## [2,] "2PL" "-865.119" 
    ## [3,] "3PL" "-1008.441"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 37
    ## [1] 27
    ## [1] 10
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-738.82" 
    ## [2,] "2PL" "-758.198"
    ## [3,] "3PL" "-882.023"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 38
    ## [1] 27
    ## [1] 11
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-757.582"
    ## [2,] "2PL" "-742.468"
    ## [3,] "3PL" "-959.196"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 39
    ## [1] 28
    ## [1] 11
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-713.444"
    ## [2,] "2PL" "-824.512"
    ## [3,] "3PL" "-967.27" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 40
    ## [1] 29
    ## [1] 11
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-710.076"
    ## [2,] "2PL" "-786.677"
    ## [3,] "3PL" "-938.343"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 41
    ## [1] 30
    ## [1] 11
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-718.097" 
    ## [2,] "2PL" "-842.283" 
    ## [3,] "3PL" "-1042.003"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 42
    ## [1] 30
    ## [1] 12
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-734.223"
    ## [2,] "2PL" "-704.649"
    ## [3,] "3PL" "-855.64" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 43
    ## [1] 31
    ## [1] 12
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-819.101"
    ## [2,] "2PL" "-940.261"
    ## [3,] "3PL" "-1090.18"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 44
    ## [1] 31
    ## [1] 13
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-726.101"
    ## [2,] "2PL" "-713.657"
    ## [3,] "3PL" "-911.135"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 45
    ## [1] 31
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-680.773"
    ## [2,] "2PL" "-653.402"
    ## [3,] "3PL" "-803.131"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 46
    ## [1] 32
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-712.373"
    ## [2,] "2PL" "-748.606"
    ## [3,] "3PL" "-866.146"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 47
    ## [1] 33
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-712.579"
    ## [2,] "2PL" "-769.561"
    ## [3,] "3PL" "-921.056"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 48
    ## [1] 34
    ## [1] 14
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-781.911" 
    ## [2,] "2PL" "-910.892" 
    ## [3,] "3PL" "-1041.959"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 49
    ## [1] 35
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-751.975"
    ## [2,] "2PL" "-816.729"
    ## [3,] "3PL" "-943.4"  
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 50
    ## [1] 36
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-752.846"
    ## [2,] "2PL" "-840.112"
    ## [3,] "3PL" "-840.342"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 51
    ## [1] 37
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-756.973"
    ## [2,] "2PL" "-786.285"
    ## [3,] "3PL" "-930.311"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 52
    ## [1] 38
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-723.802"
    ## [2,] "2PL" "-781.054"
    ## [3,] "3PL" "-842.8"  
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 53
    ## [1] 39
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-721.052"
    ## [2,] "2PL" "-795.605"
    ## [3,] "3PL" "-908.341"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 54
    ## [1] 40
    ## [1] 14
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-900.034" 
    ## [2,] "2PL" "-1115.208"
    ## [3,] "3PL" "-1080.708"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 55
    ## [1] 41
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-687.346"
    ## [2,] "2PL" "-758.322"
    ## [3,] "3PL" "-829.029"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 56
    ## [1] 42
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-695.905"
    ## [2,] "2PL" "-722.643"
    ## [3,] "3PL" "-861.496"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 57
    ## [1] 43
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-693.961"
    ## [2,] "2PL" "-745.944"
    ## [3,] "3PL" "-830.955"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 58
    ## [1] 44
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-703.935"
    ## [2,] "2PL" "-796.783"
    ## [3,] "3PL" "-752.789"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 59
    ## [1] 45
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-790.777"
    ## [2,] "2PL" "-840.552"
    ## [3,] "3PL" "-911.605"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 60
    ## [1] 46
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-709.971"
    ## [2,] "2PL" "-718.474"
    ## [3,] "3PL" "-908.721"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 61
    ## [1] 47
    ## [1] 14
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-727.334"
    ## [2,] "2PL" "-795.001"
    ## [3,] "3PL" "-988.575"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 62
    ## [1] 47
    ## [1] 15
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-724.42" 
    ## [2,] "2PL" "-709.913"
    ## [3,] "3PL" "-868.601"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 63
    ## [1] 48
    ## [1] 15
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-819.749"
    ## [2,] "2PL" "-870.733"
    ## [3,] "3PL" "-926.76" 
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 64
    ## [1] 49
    ## [1] 15
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-741.511"
    ## [2,] "2PL" "-744.239"
    ## [3,] "3PL" "-847.663"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 65
    ## [1] 50
    ## [1] 15
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-781.529" 
    ## [2,] "2PL" "-933.013" 
    ## [3,] "3PL" "-1106.728"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 66
    ## [1] 51
    ## [1] 15
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-761.178" 
    ## [2,] "2PL" "-846.187" 
    ## [3,] "3PL" "-1032.725"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 67
    ## [1] 51
    ## [1] 16
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-683.524"
    ## [2,] "2PL" "-631.33" 
    ## [3,] "3PL" "-881.999"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 68
    ## [1] 52
    ## [1] 16
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-718.864" 
    ## [2,] "2PL" "-935.237" 
    ## [3,] "3PL" "-1033.282"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 69
    ## [1] 53
    ## [1] 16
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-656.732"
    ## [2,] "2PL" "-738.931"
    ## [3,] "3PL" "-807.593"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 70
    ## [1] 54
    ## [1] 16
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-723.445"
    ## [2,] "2PL" "-749.007"
    ## [3,] "3PL" "-869.627"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 71
    ## [1] 55
    ## [1] 16
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-718.04" 
    ## [2,] "2PL" "-824.174"
    ## [3,] "3PL" "-934.936"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 72
    ## [1] 55
    ## [1] 17
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-788.515"
    ## [2,] "2PL" "-784.035"
    ## [3,] "3PL" "-896.978"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 73
    ## [1] 56
    ## [1] 17
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-693.892" 
    ## [2,] "2PL" "-891.461" 
    ## [3,] "3PL" "-1011.033"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 74
    ## [1] 56
    ## [1] 18
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-750.026"
    ## [2,] "2PL" "-715.752"
    ## [3,] "3PL" "-873.701"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 75
    ## [1] 57
    ## [1] 18
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-698.438"
    ## [2,] "2PL" "-784.134"
    ## [3,] "3PL" "-871.514"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 76
    ## [1] 58
    ## [1] 18
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-789.047" 
    ## [2,] "2PL" "-857.912" 
    ## [3,] "3PL" "-1018.313"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 77
    ## [1] 59
    ## [1] 18
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-734.164"
    ## [2,] "2PL" "-740.947"
    ## [3,] "3PL" "-905.537"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 78
    ## [1] 60
    ## [1] 18
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-718.751"
    ## [2,] "2PL" "-772.441"
    ## [3,] "3PL" "-940.264"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 79
    ## [1] 61
    ## [1] 18
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-732.698"
    ## [2,] "2PL" "-755.124"
    ## [3,] "3PL" "-882.523"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 80
    ## [1] 62
    ## [1] 18
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-768.707" 
    ## [2,] "2PL" "-786.418" 
    ## [3,] "3PL" "-1033.349"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 81
    ## [1] 62
    ## [1] 19
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-674.547"
    ## [2,] "2PL" "-668.836"
    ## [3,] "3PL" "-849.433"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 82
    ## [1] 63
    ## [1] 19
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-770.016"
    ## [2,] "2PL" "-819.277"
    ## [3,] "3PL" "-962.529"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 83
    ## [1] 64
    ## [1] 19
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-694.456"
    ## [2,] "2PL" "-856.396"
    ## [3,] "3PL" "-951.708"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 84
    ## [1] 64
    ## [1] 20
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-766.374"
    ## [2,] "2PL" "-755.162"
    ## [3,] "3PL" "-842.505"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 85
    ## [1] 65
    ## [1] 20
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-728.403"
    ## [2,] "2PL" "-793.211"
    ## [3,] "3PL" "-932.082"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 86
    ## [1] 66
    ## [1] 20
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-701.479"
    ## [2,] "2PL" "-753.448"
    ## [3,] "3PL" "-885.647"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 87
    ## [1] 67
    ## [1] 20
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-672.927"
    ## [2,] "2PL" "-701.394"
    ## [3,] "3PL" "-884.721"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 88
    ## [1] 68
    ## [1] 20
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-658.865"
    ## [2,] "2PL" "-684.377"
    ## [3,] "3PL" "-783.897"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 89
    ## [1] 69
    ## [1] 20
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-726.825"
    ## [2,] "2PL" "-754.186"
    ## [3,] "3PL" "-856.14" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 90
    ## [1] 70
    ## [1] 20
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-719.69" 
    ## [2,] "2PL" "-755.385"
    ## [3,] "3PL" "-886.46" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 91
    ## [1] 71
    ## [1] 20
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-724.12" 
    ## [2,] "2PL" "-743.736"
    ## [3,] "3PL" "-831.795"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 92
    ## [1] 72
    ## [1] 20
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-710.024"
    ## [2,] "2PL" "-717.743"
    ## [3,] "3PL" "-853.559"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 93
    ## [1] 72
    ## [1] 21
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-661.238"
    ## [2,] "2PL" "-612.308"
    ## [3,] "3PL" "-740.493"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 94
    ## [1] 73
    ## [1] 21
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-715.207"
    ## [2,] "2PL" "-733.221"
    ## [3,] "3PL" "-920.548"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 95
    ## [1] 74
    ## [1] 21
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-742.743"
    ## [2,] "2PL" "-792.195"
    ## [3,] "3PL" "-965.173"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 96
    ## [1] 75
    ## [1] 21
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-766.878"
    ## [2,] "2PL" "-777.292"
    ## [3,] "3PL" "-950.403"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 97
    ## [1] 75
    ## [1] 22
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-772.176"
    ## [2,] "2PL" "-760.685"
    ## [3,] "3PL" "-950.591"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 98
    ## [1] 76
    ## [1] 22
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-735.278"
    ## [2,] "2PL" "-780.861"
    ## [3,] "3PL" "-886.95" 
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 99
    ## [1] 77
    ## [1] 22
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-725.356"
    ## [2,] "2PL" "-849.254"
    ## [3,] "3PL" "-992.292"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 100
    ## [1] 78
    ## [1] 22
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-686.211"
    ## [2,] "2PL" "-744.925"
    ## [3,] "3PL" "-892.528"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 101
    ## [1] 78
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-729.648"
    ## [2,] "2PL" "-728.281"
    ## [3,] "3PL" "-885.648"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 102
    ## [1] 79
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-735.798"
    ## [2,] "2PL" "-815.093"
    ## [3,] "3PL" "-949.725"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 103
    ## [1] 80
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-767.264"
    ## [2,] "2PL" "-846.027"
    ## [3,] "3PL" "-994.286"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 104
    ## [1] 81
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-749.725"
    ## [2,] "2PL" "-804.478"
    ## [3,] "3PL" "-939.289"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 105
    ## [1] 82
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-715.342"
    ## [2,] "2PL" "-777.259"
    ## [3,] "3PL" "-880.832"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 106
    ## [1] 83
    ## [1] 23
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-787.197" 
    ## [2,] "2PL" "-869.772" 
    ## [3,] "3PL" "-1043.654"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 107
    ## [1] 84
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-779.78" 
    ## [2,] "2PL" "-822.132"
    ## [3,] "3PL" "-896.393"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 108
    ## [1] 85
    ## [1] 23
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-744.198" 
    ## [2,] "2PL" "-842.526" 
    ## [3,] "3PL" "-1011.276"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 109
    ## [1] 86
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-696.357"
    ## [2,] "2PL" "-733.607"
    ## [3,] "3PL" "-869.723"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 110
    ## [1] 87
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-690.835"
    ## [2,] "2PL" "-801.34" 
    ## [3,] "3PL" "-979.155"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 111
    ## [1] 88
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-815.451"
    ## [2,] "2PL" "-873.943"
    ## [3,] "3PL" "-921.877"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 112
    ## [1] 89
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-792.69" 
    ## [2,] "2PL" "-825.932"
    ## [3,] "3PL" "-974.99" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 113
    ## [1] 90
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-727.466"
    ## [2,] "2PL" "-846.334"
    ## [3,] "3PL" "-923.84" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 114
    ## [1] 91
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-759.784"
    ## [2,] "2PL" "-989.223"
    ## [3,] "3PL" "-1168.1" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 115
    ## [1] 92
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-730.201"
    ## [2,] "2PL" "-808.28" 
    ## [3,] "3PL" "-936.72" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 116
    ## [1] 93
    ## [1] 23
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-699.417"
    ## [2,] "2PL" "-751.768"
    ## [3,] "3PL" "-936.081"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 117
    ## [1] 93
    ## [1] 24
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-806.817"
    ## [2,] "2PL" "-767.374"
    ## [3,] "3PL" "-946.231"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 118
    ## [1] 94
    ## [1] 24
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-720.405"
    ## [2,] "2PL" "-817.717"
    ## [3,] "3PL" "-992.392"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 119
    ## [1] 95
    ## [1] 24
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-731.378"
    ## [2,] "2PL" "-745.307"
    ## [3,] "3PL" "-955.526"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 120
    ## [1] 96
    ## [1] 24
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-795.013"
    ## [2,] "2PL" "-842.855"
    ## [3,] "3PL" "-974.121"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 121
    ## [1] 97
    ## [1] 24
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-695.308"
    ## [2,] "2PL" "-720.345"
    ## [3,] "3PL" "-907.822"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 122
    ## [1] 98
    ## [1] 24
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-762.07" 
    ## [2,] "2PL" "-873.37" 
    ## [3,] "3PL" "-998.688"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 123
    ## [1] 99
    ## [1] 24
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-648.426"
    ## [2,] "2PL" "-675.53" 
    ## [3,] "3PL" "-807.537"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 124
    ## [1] 100
    ## [1] 24
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-817.637"
    ## [2,] "2PL" "-929.868"
    ## [3,] "3PL" "-975.221"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 125
    ## [1] 100
    ## [1] 25
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-691.682"
    ## [2,] "2PL" "-680.621"
    ## [3,] "3PL" "-872.983"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 126
    ## [1] 100
    ## [1] 26
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-766.367"
    ## [2,] "2PL" "-734.484"
    ## [3,] "3PL" "-876.597"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 127
    ## [1] 100
    ## [1] 27
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-689.99" 
    ## [2,] "2PL" "-680.959"
    ## [3,] "3PL" "-875.625"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 128
    ## [1] 101
    ## [1] 27
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-763.092"
    ## [2,] "2PL" "-883.776"
    ## [3,] "3PL" "-996.759"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 129
    ## [1] 102
    ## [1] 27
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-728.195"
    ## [2,] "2PL" "-811.253"
    ## [3,] "3PL" "-945.192"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 130
    ## [1] 103
    ## [1] 27
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-742.111"
    ## [2,] "2PL" "-803.816"
    ## [3,] "3PL" "-971.966"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 131
    ## [1] 104
    ## [1] 27
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-804.551"
    ## [2,] "2PL" "-942.269"
    ## [3,] "3PL" "-974.811"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 132
    ## [1] 104
    ## [1] 28
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-718.038"
    ## [2,] "2PL" "-682.562"
    ## [3,] "3PL" "-799.948"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 133
    ## [1] 105
    ## [1] 28
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-850.966"
    ## [2,] "2PL" "-949.12" 
    ## [3,] "3PL" "-946.261"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 134
    ## [1] 106
    ## [1] 28
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-688.666"
    ## [2,] "2PL" "-787.967"
    ## [3,] "3PL" "-963.108"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 135
    ## [1] 107
    ## [1] 28
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-754.66"  
    ## [2,] "2PL" "-868.115" 
    ## [3,] "3PL" "-1052.707"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 136
    ## [1] 108
    ## [1] 28
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-712.062"
    ## [2,] "2PL" "-816.572"
    ## [3,] "3PL" "-865.283"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 137
    ## [1] 109
    ## [1] 28
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-732.362"
    ## [2,] "2PL" "-781.335"
    ## [3,] "3PL" "-903.131"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 138
    ## [1] 110
    ## [1] 28
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-704.212"
    ## [2,] "2PL" "-714.498"
    ## [3,] "3PL" "-845.674"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 139
    ## [1] 111
    ## [1] 28
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-741.308"
    ## [2,] "2PL" "-745.274"
    ## [3,] "3PL" "-978.141"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 140
    ## [1] 111
    ## [1] 29
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-647.703"
    ## [2,] "2PL" "-644.465"
    ## [3,] "3PL" "-788.126"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 141
    ## [1] 111
    ## [1] 30
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-805.774"
    ## [2,] "2PL" "-797.71" 
    ## [3,] "3PL" "-908.486"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 142
    ## [1] 112
    ## [1] 30
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-729.663"
    ## [2,] "2PL" "-762.905"
    ## [3,] "3PL" "-933.194"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 143
    ## [1] 113
    ## [1] 30
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-709.831"
    ## [2,] "2PL" "-779.256"
    ## [3,] "3PL" "-857.139"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 144
    ## [1] 114
    ## [1] 30
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-838.491"
    ## [2,] "2PL" "-927.054"
    ## [3,] "3PL" "-951.688"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 145
    ## [1] 115
    ## [1] 30
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-772.639" 
    ## [2,] "2PL" "-828.034" 
    ## [3,] "3PL" "-1035.185"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 146
    ## [1] 116
    ## [1] 30
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-669.671"
    ## [2,] "2PL" "-729.781"
    ## [3,] "3PL" "-869.834"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 147
    ## [1] 117
    ## [1] 30
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-690.023"
    ## [2,] "2PL" "-694.37" 
    ## [3,] "3PL" "-879.204"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 148
    ## [1] 118
    ## [1] 30
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-720.99" 
    ## [2,] "2PL" "-893.036"
    ## [3,] "3PL" "-960.927"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 149
    ## [1] 118
    ## [1] 31
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-716.605"
    ## [2,] "2PL" "-705.871"
    ## [3,] "3PL" "-758.456"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 150
    ## [1] 119
    ## [1] 31
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-711.057"
    ## [2,] "2PL" "-738.495"
    ## [3,] "3PL" "-928.921"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 151
    ## [1] 119
    ## [1] 32
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-728.409"
    ## [2,] "2PL" "-699.383"
    ## [3,] "3PL" "-898.636"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 152
    ## [1] 119
    ## [1] 33
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-723.617"
    ## [2,] "2PL" "-668.963"
    ## [3,] "3PL" "-884.944"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 153
    ## [1] 120
    ## [1] 33
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-700.857"
    ## [2,] "2PL" "-789.132"
    ## [3,] "3PL" "-846.737"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 154
    ## [1] 121
    ## [1] 33
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-696.585"
    ## [2,] "2PL" "-781.719"
    ## [3,] "3PL" "-838.713"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 155
    ## [1] 122
    ## [1] 33
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-763.723"
    ## [2,] "2PL" "-795.43" 
    ## [3,] "3PL" "-910.863"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 156
    ## [1] 123
    ## [1] 33
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-702.843"
    ## [2,] "2PL" "-704.295"
    ## [3,] "3PL" "-879.938"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 157
    ## [1] 124
    ## [1] 33
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-789.151"
    ## [2,] "2PL" "-882.632"
    ## [3,] "3PL" "-987.457"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 158
    ## [1] 124
    ## [1] 34
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-701.65" 
    ## [2,] "2PL" "-693.861"
    ## [3,] "3PL" "-891.124"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 159
    ## [1] 125
    ## [1] 34
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-740.327"
    ## [2,] "2PL" "-789.8"  
    ## [3,] "3PL" "-949.053"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 160
    ## [1] 125
    ## [1] 35
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-718.718"
    ## [2,] "2PL" "-675.796"
    ## [3,] "3PL" "-843.514"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 161
    ## [1] 126
    ## [1] 35
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-752.063"
    ## [2,] "2PL" "-764.883"
    ## [3,] "3PL" "-960.061"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 162
    ## [1] 127
    ## [1] 35
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-729.349"
    ## [2,] "2PL" "-732.935"
    ## [3,] "3PL" "-871.896"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 163
    ## [1] 128
    ## [1] 35
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-719.855" 
    ## [2,] "2PL" "-867.198" 
    ## [3,] "3PL" "-1000.155"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 164
    ## [1] 128
    ## [1] 36
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-780.355"
    ## [2,] "2PL" "-777.599"
    ## [3,] "3PL" "-905.053"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 165
    ## [1] 129
    ## [1] 36
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-760.73"  
    ## [2,] "2PL" "-1011.808"
    ## [3,] "3PL" "-1042.498"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 166
    ## [1] 130
    ## [1] 36
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-731.52" 
    ## [2,] "2PL" "-926.657"
    ## [3,] "3PL" "-990.794"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 167
    ## [1] 131
    ## [1] 36
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-758.131" 
    ## [2,] "2PL" "-1017.696"
    ## [3,] "3PL" "-1141.544"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 168
    ## [1] 132
    ## [1] 36
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-710.07" 
    ## [2,] "2PL" "-875.81" 
    ## [3,] "3PL" "-972.835"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 169
    ## [1] 133
    ## [1] 36
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-723.907"
    ## [2,] "2PL" "-785.195"
    ## [3,] "3PL" "-986.108"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 170
    ## [1] 133
    ## [1] 37
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-741.215"
    ## [2,] "2PL" "-703.533"
    ## [3,] "3PL" "-910.154"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 171
    ## [1] 134
    ## [1] 37
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-693.757"
    ## [2,] "2PL" "-716.245"
    ## [3,] "3PL" "-837.06" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 172
    ## [1] 135
    ## [1] 37
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-701.99" 
    ## [2,] "2PL" "-737.308"
    ## [3,] "3PL" "-906.199"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 173
    ## [1] 136
    ## [1] 37
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-724.911"
    ## [2,] "2PL" "-728.493"
    ## [3,] "3PL" "-860.584"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 174
    ## [1] 137
    ## [1] 37
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-679.919"
    ## [2,] "2PL" "-689.359"
    ## [3,] "3PL" "-833.015"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 175
    ## [1] 138
    ## [1] 37
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-739.959"
    ## [2,] "2PL" "-761.736"
    ## [3,] "3PL" "-942.52" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 176
    ## [1] 139
    ## [1] 37
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-743.49" 
    ## [2,] "2PL" "-782.723"
    ## [3,] "3PL" "-940.686"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 177
    ## [1] 139
    ## [1] 38
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-764.467"
    ## [2,] "2PL" "-760.02" 
    ## [3,] "3PL" "-956.529"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 178
    ## [1] 140
    ## [1] 38
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-776.872" 
    ## [2,] "2PL" "-861.186" 
    ## [3,] "3PL" "-1042.127"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 179
    ## [1] 141
    ## [1] 38
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-738.491" 
    ## [2,] "2PL" "-868.627" 
    ## [3,] "3PL" "-1012.937"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 180
    ## [1] 142
    ## [1] 38
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-758.229"
    ## [2,] "2PL" "-805.411"
    ## [3,] "3PL" "-937.796"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 181
    ## [1] 143
    ## [1] 38
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-733.673"
    ## [2,] "2PL" "-795.779"
    ## [3,] "3PL" "-959.987"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 182
    ## [1] 144
    ## [1] 38
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-795.293"
    ## [2,] "2PL" "-893.594"
    ## [3,] "3PL" "-839.281"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 183
    ## [1] 145
    ## [1] 38
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-757.983"
    ## [2,] "2PL" "-767.294"
    ## [3,] "3PL" "-859.005"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 184
    ## [1] 146
    ## [1] 38
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-673.206"
    ## [2,] "2PL" "-738.606"
    ## [3,] "3PL" "-845.538"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 185
    ## [1] 147
    ## [1] 38
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-677.026"
    ## [2,] "2PL" "-719.435"
    ## [3,] "3PL" "-836.667"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 186
    ## [1] 148
    ## [1] 38
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-694.931"
    ## [2,] "2PL" "-835.965"
    ## [3,] "3PL" "-949.301"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 187
    ## [1] 149
    ## [1] 38
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-773.765" 
    ## [2,] "2PL" "-890.316" 
    ## [3,] "3PL" "-1036.925"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 188
    ## [1] 149
    ## [1] 39
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-796.428" 
    ## [2,] "2PL" "-789.723" 
    ## [3,] "3PL" "-1010.871"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 189
    ## [1] 150
    ## [1] 39
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-705.416"
    ## [2,] "2PL" "-843.644"
    ## [3,] "3PL" "-933.129"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 190
    ## [1] 151
    ## [1] 39
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-845.684" 
    ## [2,] "2PL" "-922.533" 
    ## [3,] "3PL" "-1012.579"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 191
    ## [1] 152
    ## [1] 39
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-743.28" 
    ## [2,] "2PL" "-747.266"
    ## [3,] "3PL" "-916.34" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 192
    ## [1] 153
    ## [1] 39
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-782.679" 
    ## [2,] "2PL" "-892.744" 
    ## [3,] "3PL" "-1062.287"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 193
    ## [1] 154
    ## [1] 39
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-778.666"
    ## [2,] "2PL" "-842.52" 
    ## [3,] "3PL" "-997.506"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 194
    ## [1] 155
    ## [1] 39
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-717.306"
    ## [2,] "2PL" "-806.073"
    ## [3,] "3PL" "-948.895"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 195
    ## [1] 156
    ## [1] 39
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-707.472"
    ## [2,] "2PL" "-756.236"
    ## [3,] "3PL" "-870.11" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 196
    ## [1] 157
    ## [1] 39
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-753.057"
    ## [2,] "2PL" "-776.921"
    ## [3,] "3PL" "-910.815"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 197
    ## [1] 158
    ## [1] 39
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-773.664"
    ## [2,] "2PL" "-846.766"
    ## [3,] "3PL" "-932.989"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 198
    ## [1] 158
    ## [1] 40
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-721.18" 
    ## [2,] "2PL" "-704.056"
    ## [3,] "3PL" "-856.18" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 199
    ## [1] 158
    ## [1] 41
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-732.221"
    ## [2,] "2PL" "-717.732"
    ## [3,] "3PL" "-773.925"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 200
    ## [1] 158
    ## [1] 42
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-680.598"
    ## [2,] "2PL" "-660.418"
    ## [3,] "3PL" "-800.432"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 201
    ## [1] 159
    ## [1] 42
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-681.704"
    ## [2,] "2PL" "-697.605"
    ## [3,] "3PL" "-898.834"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 202
    ## [1] 160
    ## [1] 42
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-781.775"
    ## [2,] "2PL" "-863.95" 
    ## [3,] "3PL" "-914.581"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 203
    ## [1] 161
    ## [1] 42
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-779.842"
    ## [2,] "2PL" "-850.808"
    ## [3,] "3PL" "-954.801"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 204
    ## [1] 162
    ## [1] 42
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-757.288"
    ## [2,] "2PL" "-843.976"
    ## [3,] "3PL" "-857.795"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 205
    ## [1] 163
    ## [1] 42
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-753.856"
    ## [2,] "2PL" "-788.324"
    ## [3,] "3PL" "-899.707"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 206
    ## [1] 163
    ## [1] 43
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-713.093"
    ## [2,] "2PL" "-703.471"
    ## [3,] "3PL" "-814.425"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 207
    ## [1] 164
    ## [1] 43
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-735.844" 
    ## [2,] "2PL" "-863.882" 
    ## [3,] "3PL" "-1056.829"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 208
    ## [1] 165
    ## [1] 43
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-790.126" 
    ## [2,] "2PL" "-1083.959"
    ## [3,] "3PL" "-1157.96" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 209
    ## [1] 165
    ## [1] 44
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-713.132"
    ## [2,] "2PL" "-709.219"
    ## [3,] "3PL" "-911.428"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 210
    ## [1] 166
    ## [1] 44
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-719.203"
    ## [2,] "2PL" "-729.736"
    ## [3,] "3PL" "-888.617"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 211
    ## [1] 167
    ## [1] 44
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-778.952" 
    ## [2,] "2PL" "-806.48"  
    ## [3,] "3PL" "-1010.791"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 212
    ## [1] 168
    ## [1] 44
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-738.197"
    ## [2,] "2PL" "-840.502"
    ## [3,] "3PL" "-839.551"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 213
    ## [1] 169
    ## [1] 44
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-679.144"
    ## [2,] "2PL" "-703.719"
    ## [3,] "3PL" "-845.049"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 214
    ## [1] 170
    ## [1] 44
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-757.113"
    ## [2,] "2PL" "-836.631"
    ## [3,] "3PL" "-988.599"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 215
    ## [1] 170
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-765.693"
    ## [2,] "2PL" "-734.995"
    ## [3,] "3PL" "-884.169"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 216
    ## [1] 171
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-707.016"
    ## [2,] "2PL" "-775.115"
    ## [3,] "3PL" "-861.481"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 217
    ## [1] 172
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-798.396"
    ## [2,] "2PL" "-855.79" 
    ## [3,] "3PL" "-946.565"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 218
    ## [1] 173
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-701.133"
    ## [2,] "2PL" "-845.639"
    ## [3,] "3PL" "-950.652"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 219
    ## [1] 174
    ## [1] 45
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-744.594" 
    ## [2,] "2PL" "-958.357" 
    ## [3,] "3PL" "-1013.064"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 220
    ## [1] 175
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-735.752"
    ## [2,] "2PL" "-844.431"
    ## [3,] "3PL" "-942.958"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 221
    ## [1] 176
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-758.436"
    ## [2,] "2PL" "-770.411"
    ## [3,] "3PL" "-889.222"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 222
    ## [1] 177
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-746.234"
    ## [2,] "2PL" "-808.5"  
    ## [3,] "3PL" "-875.727"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 223
    ## [1] 178
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-699.611"
    ## [2,] "2PL" "-789.578"
    ## [3,] "3PL" "-906.264"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 224
    ## [1] 179
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-761.267"
    ## [2,] "2PL" "-839.091"
    ## [3,] "3PL" "-973.428"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 225
    ## [1] 180
    ## [1] 45
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-738.807" 
    ## [2,] "2PL" "-913.571" 
    ## [3,] "3PL" "-1013.959"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 226
    ## [1] 181
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-729.686"
    ## [2,] "2PL" "-757.019"
    ## [3,] "3PL" "-880.908"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 227
    ## [1] 182
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-768.406"
    ## [2,] "2PL" "-839.843"
    ## [3,] "3PL" "-999.83" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 228
    ## [1] 183
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-761.219"
    ## [2,] "2PL" "-806.884"
    ## [3,] "3PL" "-966.845"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 229
    ## [1] 184
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-777.05" 
    ## [2,] "2PL" "-787.478"
    ## [3,] "3PL" "-929.983"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 230
    ## [1] 185
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-725.011"
    ## [2,] "2PL" "-737.694"
    ## [3,] "3PL" "-864.422"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 231
    ## [1] 186
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-718.103"
    ## [2,] "2PL" "-857.701"
    ## [3,] "3PL" "-890.67" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 232
    ## [1] 187
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-724.29" 
    ## [2,] "2PL" "-726.083"
    ## [3,] "3PL" "-869.386"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 233
    ## [1] 188
    ## [1] 45
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-795.541" 
    ## [2,] "2PL" "-854.918" 
    ## [3,] "3PL" "-1050.887"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 234
    ## [1] 189
    ## [1] 45
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-722.856" 
    ## [2,] "2PL" "-793.133" 
    ## [3,] "3PL" "-1001.459"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 235
    ## [1] 190
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-770.334"
    ## [2,] "2PL" "-800.034"
    ## [3,] "3PL" "-884.872"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 236
    ## [1] 191
    ## [1] 45
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-779.484"
    ## [2,] "2PL" "-902.035"
    ## [3,] "3PL" "-906.183"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 237
    ## [1] 191
    ## [1] 46
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-780.736"
    ## [2,] "2PL" "-772.744"
    ## [3,] "3PL" "-950.667"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 238
    ## [1] 191
    ## [1] 47
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-693.603"
    ## [2,] "2PL" "-689.238"
    ## [3,] "3PL" "-842.047"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 239
    ## [1] 192
    ## [1] 47
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-801.383" 
    ## [2,] "2PL" "-954.404" 
    ## [3,] "3PL" "-1053.365"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 240
    ## [1] 193
    ## [1] 47
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-737.974"
    ## [2,] "2PL" "-907.807"
    ## [3,] "3PL" "-901.322"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 241
    ## [1] 194
    ## [1] 47
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-711.645"
    ## [2,] "2PL" "-753.404"
    ## [3,] "3PL" "-842.606"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 242
    ## [1] 194
    ## [1] 48
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-715.169"
    ## [2,] "2PL" "-692.045"
    ## [3,] "3PL" "-872.551"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 243
    ## [1] 194
    ## [1] 49
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-709.981"
    ## [2,] "2PL" "-682.331"
    ## [3,] "3PL" "-760.644"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 244
    ## [1] 195
    ## [1] 49
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-677.248"
    ## [2,] "2PL" "-691.813"
    ## [3,] "3PL" "-774.153"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 245
    ## [1] 196
    ## [1] 49
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-733.288"
    ## [2,] "2PL" "-817.119"
    ## [3,] "3PL" "-915.431"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 246
    ## [1] 197
    ## [1] 49
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-761.412"
    ## [2,] "2PL" "-788.611"
    ## [3,] "3PL" "-974.251"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 247
    ## [1] 198
    ## [1] 49
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-759.419"
    ## [2,] "2PL" "-903.464"
    ## [3,] "3PL" "-990.529"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 248
    ## [1] 199
    ## [1] 49
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-693.794"
    ## [2,] "2PL" "-774.666"
    ## [3,] "3PL" "-837.961"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 249
    ## [1] 200
    ## [1] 49
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-670.494"
    ## [2,] "2PL" "-757.087"
    ## [3,] "3PL" "-881.458"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 250
    ## [1] 201
    ## [1] 49
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-762.307"
    ## [2,] "2PL" "-877.729"
    ## [3,] "3PL" "-999.083"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 251
    ## [1] 202
    ## [1] 49
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-686.671"
    ## [2,] "2PL" "-742.805"
    ## [3,] "3PL" "-875.537"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 252
    ## [1] 203
    ## [1] 49
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-831.512" 
    ## [2,] "2PL" "-972.718" 
    ## [3,] "3PL" "-1101.088"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 253
    ## [1] 204
    ## [1] 49
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-659.411"
    ## [2,] "2PL" "-729.018"
    ## [3,] "3PL" "-883.726"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 254
    ## [1] 204
    ## [1] 50
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-670.084"
    ## [2,] "2PL" "-650.037"
    ## [3,] "3PL" "-776.73" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 255
    ## [1] 205
    ## [1] 50
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-744.234"
    ## [2,] "2PL" "-953.214"
    ## [3,] "3PL" "-930.896"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 256
    ## [1] 206
    ## [1] 50
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-674.216"
    ## [2,] "2PL" "-717.897"
    ## [3,] "3PL" "-851.069"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 257
    ## [1] 207
    ## [1] 50
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-728.74" 
    ## [2,] "2PL" "-741.912"
    ## [3,] "3PL" "-886.655"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 258
    ## [1] 208
    ## [1] 50
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-747.838"
    ## [2,] "2PL" "-761.041"
    ## [3,] "3PL" "-917.36" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 259
    ## [1] 208
    ## [1] 51
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-767.893"
    ## [2,] "2PL" "-763.577"
    ## [3,] "3PL" "-886.901"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 260
    ## [1] 209
    ## [1] 51
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-794.84"  
    ## [2,] "2PL" "-981.79"  
    ## [3,] "3PL" "-1100.179"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 261
    ## [1] 209
    ## [1] 52
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-764.335"
    ## [2,] "2PL" "-749.246"
    ## [3,] "3PL" "-920.917"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 262
    ## [1] 210
    ## [1] 52
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-733.843"
    ## [2,] "2PL" "-783.027"
    ## [3,] "3PL" "-952.789"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 263
    ## [1] 210
    ## [1] 53
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-701.289"
    ## [2,] "2PL" "-646.498"
    ## [3,] "3PL" "-717.614"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 264
    ## [1] 211
    ## [1] 53
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-712.549"
    ## [2,] "2PL" "-755.062"
    ## [3,] "3PL" "-947.543"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 265
    ## [1] 212
    ## [1] 53
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-665.578"
    ## [2,] "2PL" "-710.902"
    ## [3,] "3PL" "-835.564"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 266
    ## [1] 212
    ## [1] 54
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-718.566"
    ## [2,] "2PL" "-713.025"
    ## [3,] "3PL" "-773.399"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 267
    ## [1] 213
    ## [1] 54
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-775.865" 
    ## [2,] "2PL" "-896.427" 
    ## [3,] "3PL" "-1013.069"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 268
    ## [1] 213
    ## [1] 55
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-752.805"
    ## [2,] "2PL" "-704.714"
    ## [3,] "3PL" "-902.377"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 269
    ## [1] 214
    ## [1] 55
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-686.453"
    ## [2,] "2PL" "-694.707"
    ## [3,] "3PL" "-816.947"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 270
    ## [1] 215
    ## [1] 55
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-808.299"
    ## [2,] "2PL" "-905.396"
    ## [3,] "3PL" "-886.542"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 271
    ## [1] 215
    ## [1] 56
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-718.735"
    ## [2,] "2PL" "-672.406"
    ## [3,] "3PL" "-832.85" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 272
    ## [1] 216
    ## [1] 56
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-814.022" 
    ## [2,] "2PL" "-977.748" 
    ## [3,] "3PL" "-1018.286"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 273
    ## [1] 217
    ## [1] 56
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-679.704"
    ## [2,] "2PL" "-760.34" 
    ## [3,] "3PL" "-853.827"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 274
    ## [1] 217
    ## [1] 57
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-694.826"
    ## [2,] "2PL" "-692.247"
    ## [3,] "3PL" "-824.115"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 275
    ## [1] 218
    ## [1] 57
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-727.506"
    ## [2,] "2PL" "-801.074"
    ## [3,] "3PL" "-985.654"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 276
    ## [1] 219
    ## [1] 57
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-694.083"
    ## [2,] "2PL" "-747.46" 
    ## [3,] "3PL" "-823.646"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 277
    ## [1] 219
    ## [1] 58
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-737.602"
    ## [2,] "2PL" "-690.778"
    ## [3,] "3PL" "-835.5"  
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 278
    ## [1] 220
    ## [1] 58
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-727.32" 
    ## [2,] "2PL" "-766.616"
    ## [3,] "3PL" "-952.378"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 279
    ## [1] 221
    ## [1] 58
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-787.737" 
    ## [2,] "2PL" "-850.662" 
    ## [3,] "3PL" "-1006.253"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 280
    ## [1] 222
    ## [1] 58
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-701.721"
    ## [2,] "2PL" "-705.897"
    ## [3,] "3PL" "-880.513"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 281
    ## [1] 223
    ## [1] 58
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-809.207" 
    ## [2,] "2PL" "-938.077" 
    ## [3,] "3PL" "-1013.181"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 282
    ## [1] 223
    ## [1] 59
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-710.231"
    ## [2,] "2PL" "-692.166"
    ## [3,] "3PL" "-849.052"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 283
    ## [1] 224
    ## [1] 59
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-723.506"
    ## [2,] "2PL" "-828.069"
    ## [3,] "3PL" "-967.601"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 284
    ## [1] 225
    ## [1] 59
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-681.527"
    ## [2,] "2PL" "-822.328"
    ## [3,] "3PL" "-968.897"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 285
    ## [1] 226
    ## [1] 59
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-707.198"
    ## [2,] "2PL" "-776.657"
    ## [3,] "3PL" "-899.555"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 286
    ## [1] 226
    ## [1] 60
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-765.448"
    ## [2,] "2PL" "-758.715"
    ## [3,] "3PL" "-973.38" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 287
    ## [1] 226
    ## [1] 61
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-682.458"
    ## [2,] "2PL" "-650.26" 
    ## [3,] "3PL" "-779.565"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 288
    ## [1] 226
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-731.048"
    ## [2,] "2PL" "-722.743"
    ## [3,] "3PL" "-902.511"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 289
    ## [1] 227
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-750.464"
    ## [2,] "2PL" "-807.567"
    ## [3,] "3PL" "-847.352"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 290
    ## [1] 228
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-679.485"
    ## [2,] "2PL" "-719.963"
    ## [3,] "3PL" "-850.698"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 291
    ## [1] 229
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-750.008"
    ## [2,] "2PL" "-778.226"
    ## [3,] "3PL" "-854.167"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 292
    ## [1] 230
    ## [1] 62
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-701.753" 
    ## [2,] "2PL" "-869.159" 
    ## [3,] "3PL" "-1027.172"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 293
    ## [1] 231
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-817.951"
    ## [2,] "2PL" "-821.559"
    ## [3,] "3PL" "-859.487"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 294
    ## [1] 232
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-708.38" 
    ## [2,] "2PL" "-717.38" 
    ## [3,] "3PL" "-895.602"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 295
    ## [1] 233
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-708.958"
    ## [2,] "2PL" "-748.073"
    ## [3,] "3PL" "-931.256"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 296
    ## [1] 234
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-738.525"
    ## [2,] "2PL" "-760.602"
    ## [3,] "3PL" "-878.145"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 297
    ## [1] 235
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-794.774"
    ## [2,] "2PL" "-921.786"
    ## [3,] "3PL" "-969.535"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 298
    ## [1] 236
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-761.838"
    ## [2,] "2PL" "-851.722"
    ## [3,] "3PL" "-997.795"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 299
    ## [1] 237
    ## [1] 62
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-797.892" 
    ## [2,] "2PL" "-866.064" 
    ## [3,] "3PL" "-1037.177"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 300
    ## [1] 238
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-883.939"
    ## [2,] "2PL" "-1002.94"
    ## [3,] "3PL" "-965.338"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 301
    ## [1] 239
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-659.601"
    ## [2,] "2PL" "-911.182"
    ## [3,] "3PL" "-834.83" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 302
    ## [1] 240
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-720.628"
    ## [2,] "2PL" "-796.557"
    ## [3,] "3PL" "-968.668"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 303
    ## [1] 241
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-676.045"
    ## [2,] "2PL" "-712.586"
    ## [3,] "3PL" "-858.182"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 304
    ## [1] 242
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-725.435"
    ## [2,] "2PL" "-807.536"
    ## [3,] "3PL" "-851.242"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 305
    ## [1] 243
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-685.753"
    ## [2,] "2PL" "-772.519"
    ## [3,] "3PL" "-908.218"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 306
    ## [1] 244
    ## [1] 62
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-763.744"
    ## [2,] "2PL" "-808.592"
    ## [3,] "3PL" "-978.171"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 307
    ## [1] 244
    ## [1] 63
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-729.671"
    ## [2,] "2PL" "-715.92" 
    ## [3,] "3PL" "-830.401"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 308
    ## [1] 245
    ## [1] 63
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-707.637"
    ## [2,] "2PL" "-740.633"
    ## [3,] "3PL" "-928.178"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 309
    ## [1] 246
    ## [1] 63
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-725.927"
    ## [2,] "2PL" "-872.26" 
    ## [3,] "3PL" "-846.668"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 310
    ## [1] 247
    ## [1] 63
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-677.734"
    ## [2,] "2PL" "-836.869"
    ## [3,] "3PL" "-940.947"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 311
    ## [1] 248
    ## [1] 63
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-795.685" 
    ## [2,] "2PL" "-851.163" 
    ## [3,] "3PL" "-1040.909"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 312
    ## [1] 249
    ## [1] 63
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-700.85" 
    ## [2,] "2PL" "-784.79" 
    ## [3,] "3PL" "-873.413"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 313
    ## [1] 250
    ## [1] 63
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-733.946"
    ## [2,] "2PL" "-790.527"
    ## [3,] "3PL" "-983.211"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 314
    ## [1] 251
    ## [1] 63
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-697.253"
    ## [2,] "2PL" "-700.76" 
    ## [3,] "3PL" "-900.237"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 315
    ## [1] 252
    ## [1] 63
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-761.093"
    ## [2,] "2PL" "-789.681"
    ## [3,] "3PL" "-950.228"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 316
    ## [1] 253
    ## [1] 63
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-715.997"
    ## [2,] "2PL" "-731.191"
    ## [3,] "3PL" "-886.109"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 317
    ## [1] 254
    ## [1] 63
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-826.451"
    ## [2,] "2PL" "-898.056"
    ## [3,] "3PL" "-973.831"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 318
    ## [1] 254
    ## [1] 64
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-730.682"
    ## [2,] "2PL" "-691.304"
    ## [3,] "3PL" "-811.586"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 319
    ## [1] 254
    ## [1] 65
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-711.123"
    ## [2,] "2PL" "-665.131"
    ## [3,] "3PL" "-793.816"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 320
    ## [1] 254
    ## [1] 66
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-751.678"
    ## [2,] "2PL" "-710.189"
    ## [3,] "3PL" "-889.044"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 321
    ## [1] 255
    ## [1] 66
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-687.356"
    ## [2,] "2PL" "-775.619"
    ## [3,] "3PL" "-911.119"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 322
    ## [1] 256
    ## [1] 66
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-722.075"
    ## [2,] "2PL" "-787.976"
    ## [3,] "3PL" "-891.904"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 323
    ## [1] 257
    ## [1] 66
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-783.47"  
    ## [2,] "2PL" "-833.931" 
    ## [3,] "3PL" "-1047.993"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 324
    ## [1] 257
    ## [1] 67
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-749.339"
    ## [2,] "2PL" "-708.163"
    ## [3,] "3PL" "-877.358"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 325
    ## [1] 258
    ## [1] 67
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-710.14" 
    ## [2,] "2PL" "-759.672"
    ## [3,] "3PL" "-931.021"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 326
    ## [1] 259
    ## [1] 67
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-750.474"
    ## [2,] "2PL" "-794.19" 
    ## [3,] "3PL" "-926.074"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 327
    ## [1] 260
    ## [1] 67
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-693.593"
    ## [2,] "2PL" "-746.565"
    ## [3,] "3PL" "-838.084"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 328
    ## [1] 260
    ## [1] 68
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-784.408"
    ## [2,] "2PL" "-742.909"
    ## [3,] "3PL" "-937.098"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 329
    ## [1] 261
    ## [1] 68
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-650.066"
    ## [2,] "2PL" "-757.346"
    ## [3,] "3PL" "-834.183"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 330
    ## [1] 262
    ## [1] 68
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-661.348"
    ## [2,] "2PL" "-735.865"
    ## [3,] "3PL" "-878.697"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 331
    ## [1] 263
    ## [1] 68
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-766.837"
    ## [2,] "2PL" "-893.498"
    ## [3,] "3PL" "-1088.62"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 332
    ## [1] 264
    ## [1] 68
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-669.684"
    ## [2,] "2PL" "-706.563"
    ## [3,] "3PL" "-848.735"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 333
    ## [1] 265
    ## [1] 68
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-667.007"
    ## [2,] "2PL" "-693.76" 
    ## [3,] "3PL" "-867.009"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 334
    ## [1] 266
    ## [1] 68
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-750.718"
    ## [2,] "2PL" "-789.372"
    ## [3,] "3PL" "-891.863"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 335
    ## [1] 267
    ## [1] 68
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-773.301"
    ## [2,] "2PL" "-812.419"
    ## [3,] "3PL" "-923.632"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 336
    ## [1] 267
    ## [1] 69
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-770.319"
    ## [2,] "2PL" "-752.61" 
    ## [3,] "3PL" "-932.218"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 337
    ## [1] 268
    ## [1] 69
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-787.06" 
    ## [2,] "2PL" "-874.276"
    ## [3,] "3PL" "-952.497"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 338
    ## [1] 269
    ## [1] 69
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-722.725"
    ## [2,] "2PL" "-825.424"
    ## [3,] "3PL" "-982.082"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 339
    ## [1] 270
    ## [1] 69
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-690.62" 
    ## [2,] "2PL" "-782.179"
    ## [3,] "3PL" "-862.268"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 340
    ## [1] 271
    ## [1] 69
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-703.082"
    ## [2,] "2PL" "-740.453"
    ## [3,] "3PL" "-913.001"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 341
    ## [1] 272
    ## [1] 69
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-747.917"
    ## [2,] "2PL" "-819.232"
    ## [3,] "3PL" "-948.258"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 342
    ## [1] 273
    ## [1] 69
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-753.321"
    ## [2,] "2PL" "-778.586"
    ## [3,] "3PL" "-999.793"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 343
    ## [1] 274
    ## [1] 69
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-759.504"
    ## [2,] "2PL" "-804.049"
    ## [3,] "3PL" "-863.792"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 344
    ## [1] 275
    ## [1] 69
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-685.139"
    ## [2,] "2PL" "-758.052"
    ## [3,] "3PL" "-795.552"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 345
    ## [1] 275
    ## [1] 70
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-682.706"
    ## [2,] "2PL" "-614.876"
    ## [3,] "3PL" "-786.872"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 346
    ## [1] 275
    ## [1] 71
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-732.254"
    ## [2,] "2PL" "-719.621"
    ## [3,] "3PL" "-899.845"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 347
    ## [1] 275
    ## [1] 72
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-761.36" 
    ## [2,] "2PL" "-730.397"
    ## [3,] "3PL" "-881.534"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 348
    ## [1] 276
    ## [1] 72
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-723.42" 
    ## [2,] "2PL" "-729.873"
    ## [3,] "3PL" "-918.11" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 349
    ## [1] 276
    ## [1] 73
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-745.788"
    ## [2,] "2PL" "-744.773"
    ## [3,] "3PL" "-985.189"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 350
    ## [1] 277
    ## [1] 73
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-686.855"
    ## [2,] "2PL" "-794.138"
    ## [3,] "3PL" "-830.251"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 351
    ## [1] 278
    ## [1] 73
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-744.383"
    ## [2,] "2PL" "-768.717"
    ## [3,] "3PL" "-901.221"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 352
    ## [1] 278
    ## [1] 74
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-704.174"
    ## [2,] "2PL" "-683.82" 
    ## [3,] "3PL" "-954.404"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 353
    ## [1] 279
    ## [1] 74
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-736.165"
    ## [2,] "2PL" "-765.659"
    ## [3,] "3PL" "-895.808"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 354
    ## [1] 280
    ## [1] 74
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-716.01" 
    ## [2,] "2PL" "-719.442"
    ## [3,] "3PL" "-855.737"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 355
    ## [1] 281
    ## [1] 74
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-749.91" 
    ## [2,] "2PL" "-792.271"
    ## [3,] "3PL" "-989.522"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 356
    ## [1] 282
    ## [1] 74
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-728.511" 
    ## [2,] "2PL" "-867.955" 
    ## [3,] "3PL" "-1026.816"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 357
    ## [1] 283
    ## [1] 74
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-749.219" 
    ## [2,] "2PL" "-903.924" 
    ## [3,] "3PL" "-1001.045"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 358
    ## [1] 283
    ## [1] 75
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-771.556"
    ## [2,] "2PL" "-749.142"
    ## [3,] "3PL" "-939.897"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 359
    ## [1] 284
    ## [1] 75
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-716.428"
    ## [2,] "2PL" "-811.539"
    ## [3,] "3PL" "-882.287"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 360
    ## [1] 284
    ## [1] 76
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-705.068"
    ## [2,] "2PL" "-695.711"
    ## [3,] "3PL" "-883.389"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 361
    ## [1] 285
    ## [1] 76
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-721.188"
    ## [2,] "2PL" "-757.343"
    ## [3,] "3PL" "-920.983"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 362
    ## [1] 286
    ## [1] 76
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-715.132"
    ## [2,] "2PL" "-774.148"
    ## [3,] "3PL" "-859.221"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 363
    ## [1] 287
    ## [1] 76
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-742.481"
    ## [2,] "2PL" "-780.379"
    ## [3,] "3PL" "-949.998"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 364
    ## [1] 288
    ## [1] 76
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-700.791"
    ## [2,] "2PL" "-703.205"
    ## [3,] "3PL" "-819.577"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 365
    ## [1] 289
    ## [1] 76
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-708.996"
    ## [2,] "2PL" "-715.205"
    ## [3,] "3PL" "-883.84" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 366
    ## [1] 289
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-692.777"
    ## [2,] "2PL" "-636.227"
    ## [3,] "3PL" "-767.347"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 367
    ## [1] 290
    ## [1] 77
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-759.727" 
    ## [2,] "2PL" "-876.716" 
    ## [3,] "3PL" "-1044.492"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 368
    ## [1] 291
    ## [1] 77
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-694.783" 
    ## [2,] "2PL" "-764.759" 
    ## [3,] "3PL" "-1037.301"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 369
    ## [1] 292
    ## [1] 77
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-750.66"  
    ## [2,] "2PL" "-841.577" 
    ## [3,] "3PL" "-1009.517"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 370
    ## [1] 293
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-761.425"
    ## [2,] "2PL" "-851.658"
    ## [3,] "3PL" "-977.578"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 371
    ## [1] 294
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-704.081"
    ## [2,] "2PL" "-736.896"
    ## [3,] "3PL" "-907.04" 
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 372
    ## [1] 295
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-734.315"
    ## [2,] "2PL" "-763.061"
    ## [3,] "3PL" "-884.411"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 373
    ## [1] 296
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-734.856"
    ## [2,] "2PL" "-782.936"
    ## [3,] "3PL" "-868.137"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 374
    ## [1] 297
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-722.315"
    ## [2,] "2PL" "-851.269"
    ## [3,] "3PL" "-941.687"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 375
    ## [1] 298
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-695.676"
    ## [2,] "2PL" "-751.781"
    ## [3,] "3PL" "-870.175"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 376
    ## [1] 299
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-736.361"
    ## [2,] "2PL" "-753.306"
    ## [3,] "3PL" "-874.224"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 377
    ## [1] 300
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-717.714"
    ## [2,] "2PL" "-750.698"
    ## [3,] "3PL" "-853.655"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 378
    ## [1] 301
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-772.29" 
    ## [2,] "2PL" "-902.127"
    ## [3,] "3PL" "-906.421"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 379
    ## [1] 302
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-694.55" 
    ## [2,] "2PL" "-734.53" 
    ## [3,] "3PL" "-936.726"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 380
    ## [1] 303
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-738.742"
    ## [2,] "2PL" "-773.557"
    ## [3,] "3PL" "-918.987"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 381
    ## [1] 304
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-778.903"
    ## [2,] "2PL" "-840.227"
    ## [3,] "3PL" "-858.341"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 382
    ## [1] 305
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-709.404"
    ## [2,] "2PL" "-797.091"
    ## [3,] "3PL" "-947.383"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 383
    ## [1] 306
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-734.804"
    ## [2,] "2PL" "-819.296"
    ## [3,] "3PL" "-927.456"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 384
    ## [1] 307
    ## [1] 77
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-750.52"  
    ## [2,] "2PL" "-854.255" 
    ## [3,] "3PL" "-1013.931"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 385
    ## [1] 308
    ## [1] 77
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-707.61" 
    ## [2,] "2PL" "-768.859"
    ## [3,] "3PL" "-846.537"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 386
    ## [1] 308
    ## [1] 78
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-725.975"
    ## [2,] "2PL" "-672.875"
    ## [3,] "3PL" "-866.318"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 387
    ## [1] 309
    ## [1] 78
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-747.111"
    ## [2,] "2PL" "-853.804"
    ## [3,] "3PL" "-964.856"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 388
    ## [1] 310
    ## [1] 78
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-738.501"
    ## [2,] "2PL" "-840.378"
    ## [3,] "3PL" "-886.424"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 389
    ## [1] 311
    ## [1] 78
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-690.051"
    ## [2,] "2PL" "-738.106"
    ## [3,] "3PL" "-922.906"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 390
    ## [1] 312
    ## [1] 78
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-704.35" 
    ## [2,] "2PL" "-760.989"
    ## [3,] "3PL" "-800.967"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 391
    ## [1] 313
    ## [1] 78
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-697.45" 
    ## [2,] "2PL" "-711.319"
    ## [3,] "3PL" "-824.336"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 392
    ## [1] 314
    ## [1] 78
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-735.575"
    ## [2,] "2PL" "-805.004"
    ## [3,] "3PL" "-944.722"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 393
    ## [1] 315
    ## [1] 78
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-739.527"
    ## [2,] "2PL" "-875.807"
    ## [3,] "3PL" "-953.758"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 394
    ## [1] 315
    ## [1] 79
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-750.341"
    ## [2,] "2PL" "-723.517"
    ## [3,] "3PL" "-912.324"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 395
    ## [1] 315
    ## [1] 80
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-751.97" 
    ## [2,] "2PL" "-711.105"
    ## [3,] "3PL" "-855.163"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 396
    ## [1] 316
    ## [1] 80
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-779.407"
    ## [2,] "2PL" "-838.405"
    ## [3,] "3PL" "-988.757"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 397
    ## [1] 317
    ## [1] 80
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-708.316"
    ## [2,] "2PL" "-763.578"
    ## [3,] "3PL" "-904.874"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 398
    ## [1] 318
    ## [1] 80
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-758.164"
    ## [2,] "2PL" "-888.825"
    ## [3,] "3PL" "-983.173"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 399
    ## [1] 319
    ## [1] 80
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-773.122"
    ## [2,] "2PL" "-824.377"
    ## [3,] "3PL" "-924.482"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 400
    ## [1] 320
    ## [1] 80
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-723.567"
    ## [2,] "2PL" "-727.062"
    ## [3,] "3PL" "-862.983"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 401
    ## [1] 320
    ## [1] 81
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-706.738"
    ## [2,] "2PL" "-661.779"
    ## [3,] "3PL" "-843.208"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 402
    ## [1] 321
    ## [1] 81
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-800.385"
    ## [2,] "2PL" "-835.897"
    ## [3,] "3PL" "-991.351"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 403
    ## [1] 322
    ## [1] 81
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-702.12"  
    ## [2,] "2PL" "-744.401" 
    ## [3,] "3PL" "-1003.066"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 404
    ## [1] 323
    ## [1] 81
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-750.719"
    ## [2,] "2PL" "-857.041"
    ## [3,] "3PL" "-964.528"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 405
    ## [1] 324
    ## [1] 81
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-750.54"  
    ## [2,] "2PL" "-879.662" 
    ## [3,] "3PL" "-1015.493"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 406
    ## [1] 324
    ## [1] 82
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-727.021"
    ## [2,] "2PL" "-671.579"
    ## [3,] "3PL" "-855.968"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 407
    ## [1] 325
    ## [1] 82
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-716.996" 
    ## [2,] "2PL" "-837.834" 
    ## [3,] "3PL" "-1004.332"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 408
    ## [1] 326
    ## [1] 82
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-768.85"  
    ## [2,] "2PL" "-919.285" 
    ## [3,] "3PL" "-1012.023"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 409
    ## [1] 326
    ## [1] 83
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-730.828"
    ## [2,] "2PL" "-671.29" 
    ## [3,] "3PL" "-791.163"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 410
    ## [1] 327
    ## [1] 83
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-736.474" 
    ## [2,] "2PL" "-918.396" 
    ## [3,] "3PL" "-1069.892"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 411
    ## [1] 328
    ## [1] 83
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-784.227"
    ## [2,] "2PL" "-835.78" 
    ## [3,] "3PL" "-983.344"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 412
    ## [1] 329
    ## [1] 83
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-778.258" 
    ## [2,] "2PL" "-905.496" 
    ## [3,] "3PL" "-1003.864"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 413
    ## [1] 330
    ## [1] 83
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-763.497"
    ## [2,] "2PL" "-932.699"
    ## [3,] "3PL" "-969.044"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 414
    ## [1] 331
    ## [1] 83
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-756.046"
    ## [2,] "2PL" "-779.43" 
    ## [3,] "3PL" "-962.695"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 415
    ## [1] 332
    ## [1] 83
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-751.915"
    ## [2,] "2PL" "-886.718"
    ## [3,] "3PL" "-981.596"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 416
    ## [1] 333
    ## [1] 83
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-743.717"
    ## [2,] "2PL" "-863.781"
    ## [3,] "3PL" "-954.221"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 417
    ## [1] 334
    ## [1] 83
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-804.691"
    ## [2,] "2PL" "-893.192"
    ## [3,] "3PL" "-940.981"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 418
    ## [1] 335
    ## [1] 83
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-741.962"
    ## [2,] "2PL" "-786.792"
    ## [3,] "3PL" "-986.338"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 419
    ## [1] 336
    ## [1] 83
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-701.303"
    ## [2,] "2PL" "-770.913"
    ## [3,] "3PL" "-863.893"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 420
    ## [1] 336
    ## [1] 84
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-713.075"
    ## [2,] "2PL" "-691.466"
    ## [3,] "3PL" "-881.383"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 421
    ## [1] 337
    ## [1] 84
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-797.188"
    ## [2,] "2PL" "-911.785"
    ## [3,] "3PL" "-904.917"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 422
    ## [1] 337
    ## [1] 85
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-745.724"
    ## [2,] "2PL" "-731.873"
    ## [3,] "3PL" "-869.816"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 423
    ## [1] 338
    ## [1] 85
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-733.301" 
    ## [2,] "2PL" "-919.163" 
    ## [3,] "3PL" "-1016.122"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 424
    ## [1] 339
    ## [1] 85
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-812.113"
    ## [2,] "2PL" "-924.796"
    ## [3,] "3PL" "-875.785"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 425
    ## [1] 340
    ## [1] 85
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-705.227"
    ## [2,] "2PL" "-775.835"
    ## [3,] "3PL" "-923.446"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 426
    ## [1] 341
    ## [1] 85
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-758.831"
    ## [2,] "2PL" "-824.133"
    ## [3,] "3PL" "-991.794"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 427
    ## [1] 342
    ## [1] 85
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-684.908"
    ## [2,] "2PL" "-714.602"
    ## [3,] "3PL" "-929.75" 
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 428
    ## [1] 343
    ## [1] 85
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-738.852"
    ## [2,] "2PL" "-783.719"
    ## [3,] "3PL" "-981.61" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 429
    ## [1] 344
    ## [1] 85
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-776.151"
    ## [2,] "2PL" "-846.868"
    ## [3,] "3PL" "-855.493"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 430
    ## [1] 344
    ## [1] 86
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-791.223"
    ## [2,] "2PL" "-779.603"
    ## [3,] "3PL" "-904.196"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 431
    ## [1] 345
    ## [1] 86
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-690.958"
    ## [2,] "2PL" "-777.715"
    ## [3,] "3PL" "-944.174"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 432
    ## [1] 346
    ## [1] 86
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-760.462" 
    ## [2,] "2PL" "-906.733" 
    ## [3,] "3PL" "-1082.643"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 433
    ## [1] 347
    ## [1] 86
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-735.937"
    ## [2,] "2PL" "-767.256"
    ## [3,] "3PL" "-851.381"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 434
    ## [1] 347
    ## [1] 87
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-750.234"
    ## [2,] "2PL" "-687.523"
    ## [3,] "3PL" "-787.752"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 435
    ## [1] 348
    ## [1] 87
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-694.496"
    ## [2,] "2PL" "-694.691"
    ## [3,] "3PL" "-825.374"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 436
    ## [1] 349
    ## [1] 87
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-818.251"
    ## [2,] "2PL" "-983.733"
    ## [3,] "3PL" "-914.55" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 437
    ## [1] 350
    ## [1] 87
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-701.806"
    ## [2,] "2PL" "-758.462"
    ## [3,] "3PL" "-885.417"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 438
    ## [1] 350
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-679.684"
    ## [2,] "2PL" "-660.044"
    ## [3,] "3PL" "-791.132"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 439
    ## [1] 351
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-724.32" 
    ## [2,] "2PL" "-772.552"
    ## [3,] "3PL" "-923.901"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 440
    ## [1] 352
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-745.218"
    ## [2,] "2PL" "-809.797"
    ## [3,] "3PL" "-953.291"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 441
    ## [1] 353
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-727.17" 
    ## [2,] "2PL" "-785.999"
    ## [3,] "3PL" "-917.959"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 442
    ## [1] 354
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-730.899"
    ## [2,] "2PL" "-797.177"
    ## [3,] "3PL" "-966.165"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 443
    ## [1] 355
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-744.828"
    ## [2,] "2PL" "-777.053"
    ## [3,] "3PL" "-929.075"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 444
    ## [1] 356
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-750.113"
    ## [2,] "2PL" "-810.167"
    ## [3,] "3PL" "-873.596"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 445
    ## [1] 357
    ## [1] 88
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-766.751" 
    ## [2,] "2PL" "-795.72"  
    ## [3,] "3PL" "-1007.878"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 446
    ## [1] 358
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-769.814"
    ## [2,] "2PL" "-836.573"
    ## [3,] "3PL" "-965.664"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 447
    ## [1] 359
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-648.787"
    ## [2,] "2PL" "-654.671"
    ## [3,] "3PL" "-710.206"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 448
    ## [1] 360
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-661.227"
    ## [2,] "2PL" "-727.406"
    ## [3,] "3PL" "-878.961"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 449
    ## [1] 361
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-739.013"
    ## [2,] "2PL" "-808.356"
    ## [3,] "3PL" "-922.227"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 450
    ## [1] 362
    ## [1] 88
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-868.212" 
    ## [2,] "2PL" "-1052.364"
    ## [3,] "3PL" "-1018.288"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 451
    ## [1] 363
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-685.481"
    ## [2,] "2PL" "-702.632"
    ## [3,] "3PL" "-814.97" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 452
    ## [1] 364
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-719.765"
    ## [2,] "2PL" "-867.883"
    ## [3,] "3PL" "-977.154"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 453
    ## [1] 365
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-699.528"
    ## [2,] "2PL" "-829.584"
    ## [3,] "3PL" "-897.072"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 454
    ## [1] 366
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-748.216"
    ## [2,] "2PL" "-792.524"
    ## [3,] "3PL" "-903.195"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 455
    ## [1] 367
    ## [1] 88
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-778.499"
    ## [2,] "2PL" "-833.429"
    ## [3,] "3PL" "-937.501"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 456
    ## [1] 367
    ## [1] 89
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-709.953"
    ## [2,] "2PL" "-631.25" 
    ## [3,] "3PL" "-825.395"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 457
    ## [1] 368
    ## [1] 89
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-744.768" 
    ## [2,] "2PL" "-900.165" 
    ## [3,] "3PL" "-1054.973"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 458
    ## [1] 368
    ## [1] 90
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-736.298"
    ## [2,] "2PL" "-724.174"
    ## [3,] "3PL" "-872.885"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 459
    ## [1] 368
    ## [1] 91
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-751.929"
    ## [2,] "2PL" "-687.462"
    ## [3,] "3PL" "-909.5"  
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 460
    ## [1] 368
    ## [1] 92
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-760.094"
    ## [2,] "2PL" "-656.148"
    ## [3,] "3PL" "-823.555"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 461
    ## [1] 369
    ## [1] 92
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-700.397"
    ## [2,] "2PL" "-734.834"
    ## [3,] "3PL" "-825.14" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 462
    ## [1] 370
    ## [1] 92
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-695.865"
    ## [2,] "2PL" "-817.581"
    ## [3,] "3PL" "-902.062"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 463
    ## [1] 371
    ## [1] 92
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-729.478"
    ## [2,] "2PL" "-837.435"
    ## [3,] "3PL" "-999.475"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 464
    ## [1] 371
    ## [1] 93
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-726.429"
    ## [2,] "2PL" "-709.182"
    ## [3,] "3PL" "-869.879"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 465
    ## [1] 371
    ## [1] 94
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-697.003"
    ## [2,] "2PL" "-658.057"
    ## [3,] "3PL" "-846.972"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 466
    ## [1] 372
    ## [1] 94
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-754.254"
    ## [2,] "2PL" "-902.425"
    ## [3,] "3PL" "-953.07" 
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 467
    ## [1] 372
    ## [1] 95
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-765.351"
    ## [2,] "2PL" "-745.607"
    ## [3,] "3PL" "-937.315"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 468
    ## [1] 372
    ## [1] 96
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-731.76" 
    ## [2,] "2PL" "-705.386"
    ## [3,] "3PL" "-860.101"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 469
    ## [1] 373
    ## [1] 96
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-788.671"
    ## [2,] "2PL" "-806.562"
    ## [3,] "3PL" "-969.923"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 470
    ## [1] 374
    ## [1] 96
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-702.807"
    ## [2,] "2PL" "-826.13" 
    ## [3,] "3PL" "-924.469"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 471
    ## [1] 375
    ## [1] 96
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-687.09" 
    ## [2,] "2PL" "-752.113"
    ## [3,] "3PL" "-956.759"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 472
    ## [1] 375
    ## [1] 97
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-733.131"
    ## [2,] "2PL" "-708.533"
    ## [3,] "3PL" "-850.109"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 473
    ## [1] 376
    ## [1] 97
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-760.966"
    ## [2,] "2PL" "-769.174"
    ## [3,] "3PL" "-934.283"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 474
    ## [1] 377
    ## [1] 97
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-723.365"
    ## [2,] "2PL" "-791.091"
    ## [3,] "3PL" "-992.767"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 475
    ## [1] 378
    ## [1] 97
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-674.278"
    ## [2,] "2PL" "-692.732"
    ## [3,] "3PL" "-871.304"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 476
    ## [1] 378
    ## [1] 98
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-668.135"
    ## [2,] "2PL" "-651.872"
    ## [3,] "3PL" "-890.656"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 477
    ## [1] 379
    ## [1] 98
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-737.58" 
    ## [2,] "2PL" "-743.488"
    ## [3,] "3PL" "-874.349"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 478
    ## [1] 380
    ## [1] 98
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-770.096"
    ## [2,] "2PL" "-848.743"
    ## [3,] "3PL" "-973.728"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 479
    ## [1] 381
    ## [1] 98
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-759.565" 
    ## [2,] "2PL" "-792.094" 
    ## [3,] "3PL" "-1007.733"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 480
    ## [1] 382
    ## [1] 98
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-703.24" 
    ## [2,] "2PL" "-741.407"
    ## [3,] "3PL" "-860.914"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 481
    ## [1] 383
    ## [1] 98
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-740.665"
    ## [2,] "2PL" "-808.259"
    ## [3,] "3PL" "-956.069"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 482
    ## [1] 384
    ## [1] 98
    ## [1] 0
    ##      model LL         
    ## [1,] "1PL" "-754.628" 
    ## [2,] "2PL" "-1026.576"
    ## [3,] "3PL" "-1088.707"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 483
    ## [1] 385
    ## [1] 98
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-740.988"
    ## [2,] "2PL" "-810.926"
    ## [3,] "3PL" "-899.033"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 484
    ## [1] 385
    ## [1] 99
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-693.501"
    ## [2,] "2PL" "-690.456"
    ## [3,] "3PL" "-829.61" 
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 485
    ## [1] 386
    ## [1] 99
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-711.631"
    ## [2,] "2PL" "-825.899"
    ## [3,] "3PL" "-884.589"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 486
    ## [1] 387
    ## [1] 99
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-745.894"
    ## [2,] "2PL" "-767.519"
    ## [3,] "3PL" "-863.169"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 487
    ## [1] 387
    ## [1] 100
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-697.749"
    ## [2,] "2PL" "-676.568"
    ## [3,] "3PL" "-804.035"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 488
    ## [1] 387
    ## [1] 101
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-734.192"
    ## [2,] "2PL" "-724.797"
    ## [3,] "3PL" "-875.123"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 489
    ## [1] 388
    ## [1] 101
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-719.742"
    ## [2,] "2PL" "-763.372"
    ## [3,] "3PL" "-926.748"
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 490
    ## [1] 389
    ## [1] 101
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-786.725"
    ## [2,] "2PL" "-787.718"
    ## [3,] "3PL" "-907.566"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 491
    ## [1] 390
    ## [1] 101
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-713.094"
    ## [2,] "2PL" "-751.336"
    ## [3,] "3PL" "-856.758"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 492
    ## [1] 390
    ## [1] 102
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-700.394"
    ## [2,] "2PL" "-695.542"
    ## [3,] "3PL" "-839.07" 
    ## function (..., na.rm = FALSE)  .Primitive("min")

    ## EM cycles terminated after 500 iterations.

    ## [1] 493
    ## [1] 391
    ## [1] 102
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-688.897"
    ## [2,] "2PL" "-831.814"
    ## [3,] "3PL" "-917.238"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 494
    ## [1] 392
    ## [1] 102
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-717.321"
    ## [2,] "2PL" "-882.693"
    ## [3,] "3PL" "-960.415"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 495
    ## [1] 393
    ## [1] 102
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-778.146"
    ## [2,] "2PL" "-852.071"
    ## [3,] "3PL" "-963.837"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 496
    ## [1] 393
    ## [1] 103
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-772.258"
    ## [2,] "2PL" "-746.82" 
    ## [3,] "3PL" "-897.215"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 497
    ## [1] 394
    ## [1] 103
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-722.695"
    ## [2,] "2PL" "-888.273"
    ## [3,] "3PL" "-989.812"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 498
    ## [1] 395
    ## [1] 103
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-757.543"
    ## [2,] "2PL" "-850.653"
    ## [3,] "3PL" "-958.222"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 499
    ## [1] 395
    ## [1] 104
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-732.581"
    ## [2,] "2PL" "-714.184"
    ## [3,] "3PL" "-850.647"
    ## function (..., na.rm = FALSE)  .Primitive("min")
    ## [1] 500
    ## [1] 395
    ## [1] 105
    ## [1] 0
    ##      model LL        
    ## [1,] "1PL" "-738.546"
    ## [2,] "2PL" "-718.102"
    ## [3,] "3PL" "-867.176"
    ## function (..., na.rm = FALSE)  .Primitive("min")

``` r
print("MR, DGM 3PL, 500 people")
```

    ## [1] "MR, DGM 3PL, 500 people"

``` r
print(c1)
```

    ## [1] 395

``` r
print(c2)
```

    ## [1] 105

``` r
print(c3)
```

    ## [1] 0
