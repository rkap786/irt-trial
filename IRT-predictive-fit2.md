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

# model1PL <- mirt(data=dat, 1,itemtype='Rasch', SE=FALSE, verbose=FALSE, technical = list(NCYCLES=5000))
# coef = as.data.frame(coef(model1PL, simplify=T)$items[,2]) %>%
#   tibble::rownames_to_column(., "Item no") %>%
#   mutate("b"= b) %>%
#   rename(b_est= "coef(model1PL, simplify = T)$items[, 2]")
# 
# plot(coef$b,coef$b_est)

# model3PL <- mirt(data=dat, 1, itemtype='3PL', SE=F, verbose=FALSE, technical = list(NCYCLES=5000))
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
  theta <- fscores(mod, method = "ML", full.scores = TRUE)  
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
    sum(p_model_correct, na.rm=T)
}

#p_model_correct <- log(p_model^p_true * (1 - p_model)^(1 - p_true))
#p= est_3PL_mr^m0_na_test * (1-est_3PL_mr) (1-m0_na_test)
```

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

colMeans(dat_mr_train, na.rm = T)
colMeans(dat_mr_test, na.rm = T)


#Estimate item parameters for train dataset (with missing data)
model1PL <- mirt(dat_mr_train, 1, itemtype='Rasch', SE=F, verbose=FALSE)
model2PL <- mirt(dat_mr_train, 1, itemtype='2PL', SE=F, verbose=FALSE)
model3PL <- mirt(dat_mr_train, 1, itemtype='3PL', SE=F, verbose=FALSE, technical = list(NCYCLES=5000))
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
iter=50

c1=0
c2=0
c3=0

ability <- as.matrix(rnorm(sample.size, mean = 0, sd = 1), ncol=1)


a <- as.matrix(rlnorm(nitem, meanlog = 0.2, sdlog = 0.3), ncol=1) #lognormal
b <- as.matrix(rnorm(nitem, mean = 0, sd = 1), ncol=1) #normal
c <- as.matrix(rbeta(nitem, shape1 = 20, shape2 = 80), ncol=1) #beta

for (i in 1:iter){
result1 = predict_fit_mr(nitem,sample.size, model = "3PL", a,b,c,ability )
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
    ## [1,] "1PL" "-758.512"
    ## [2,] "2PL" "-767.141"
    ## [3,] "3PL" "-828.798"
    ##      model LL        
    ## [1,] "1PL" "-Inf"    
    ## [2,] "2PL" "-Inf"    
    ## [3,] "3PL" "-748.811"
    ##      model LL        
    ## [1,] "1PL" "-Inf"    
    ## [2,] "2PL" "-Inf"    
    ## [3,] "3PL" "-802.876"
    ##      model LL        
    ## [1,] "1PL" "-763.758"
    ## [2,] "2PL" "-786.636"
    ## [3,] "3PL" "-855.893"
    ##      model LL        
    ## [1,] "1PL" "-742.315"
    ## [2,] "2PL" "-746.216"
    ## [3,] "3PL" "-870.988"
    ##      model LL        
    ## [1,] "1PL" "-745.729"
    ## [2,] "2PL" "-737.932"
    ## [3,] "3PL" "-850.258"
    ##      model LL        
    ## [1,] "1PL" "-796.861"
    ## [2,] "2PL" "-774.31" 
    ## [3,] "3PL" "-840.771"
    ##      model LL        
    ## [1,] "1PL" "-Inf"    
    ## [2,] "2PL" "-Inf"    
    ## [3,] "3PL" "-836.744"

    ## EM cycles terminated after 5000 iterations.

    ##      model LL        
    ## [1,] "1PL" "-Inf"    
    ## [2,] "2PL" "-Inf"    
    ## [3,] "3PL" "-852.008"
    ##      model LL        
    ## [1,] "1PL" "-766.924"
    ## [2,] "2PL" "-747.871"
    ## [3,] "3PL" "-813.147"
    ##      model LL        
    ## [1,] "1PL" "-Inf"    
    ## [2,] "2PL" "-Inf"    
    ## [3,] "3PL" "-823.361"
    ##      model LL        
    ## [1,] "1PL" "-739.957"
    ## [2,] "2PL" "-720.086"
    ## [3,] "3PL" "-804.36" 
    ##      model LL        
    ## [1,] "1PL" "-751.026"
    ## [2,] "2PL" "-752.843"
    ## [3,] "3PL" "-794.956"
    ##      model LL        
    ## [1,] "1PL" "-824.805"
    ## [2,] "2PL" "-811.25" 
    ## [3,] "3PL" "-875.576"
    ##      model LL        
    ## [1,] "1PL" "-737.282"
    ## [2,] "2PL" "-733.962"
    ## [3,] "3PL" "-779.886"

    ## EM cycles terminated after 5000 iterations.

    ##      model LL        
    ## [1,] "1PL" "-774.424"
    ## [2,] "2PL" "-786.817"
    ## [3,] "3PL" "-888.197"
    ##      model LL        
    ## [1,] "1PL" "-758.957"
    ## [2,] "2PL" "-762.609"
    ## [3,] "3PL" "-812.637"
    ##      model LL        
    ## [1,] "1PL" "-803.806"
    ## [2,] "2PL" "-825.342"
    ## [3,] "3PL" "-927.966"
    ##      model LL        
    ## [1,] "1PL" "-812.918"
    ## [2,] "2PL" "-846.655"
    ## [3,] "3PL" "-858.638"
    ##      model LL        
    ## [1,] "1PL" "-Inf"    
    ## [2,] "2PL" "-Inf"    
    ## [3,] "3PL" "-902.661"
    ##      model LL        
    ## [1,] "1PL" "-785.791"
    ## [2,] "2PL" "-755.099"
    ## [3,] "3PL" "-844.194"
    ##      model LL        
    ## [1,] "1PL" "-813.088"
    ## [2,] "2PL" "-783.109"
    ## [3,] "3PL" "-821.811"
    ##      model LL        
    ## [1,] "1PL" "-Inf"    
    ## [2,] "2PL" "-Inf"    
    ## [3,] "3PL" "-824.164"
    ##      model LL        
    ## [1,] "1PL" "-751.984"
    ## [2,] "2PL" "-725.673"
    ## [3,] "3PL" "-878.478"
    ##      model LL        
    ## [1,] "1PL" "-763.54" 
    ## [2,] "2PL" "-779.477"
    ## [3,] "3PL" "-832.532"
    ##      model LL        
    ## [1,] "1PL" "-Inf"    
    ## [2,] "2PL" "-Inf"    
    ## [3,] "3PL" "-882.233"
    ##      model LL        
    ## [1,] "1PL" "-790.281"
    ## [2,] "2PL" "-775.887"
    ## [3,] "3PL" "-890.846"
    ##      model LL        
    ## [1,] "1PL" "-796.98" 
    ## [2,] "2PL" "-827.866"
    ## [3,] "3PL" "-846.199"

    ## EM cycles terminated after 5000 iterations.

    ##      model LL        
    ## [1,] "1PL" "-853.585"
    ## [2,] "2PL" "-869.55" 
    ## [3,] "3PL" "-927.71"

    ## EM cycles terminated after 5000 iterations.

    ##      model LL        
    ## [1,] "1PL" "-738.879"
    ## [2,] "2PL" "-739.129"
    ## [3,] "3PL" "-785.895"
    ##      model LL        
    ## [1,] "1PL" "-803.466"
    ## [2,] "2PL" "-802.745"
    ## [3,] "3PL" "-888.244"
    ##      model LL        
    ## [1,] "1PL" "-740.171"
    ## [2,] "2PL" "-754.913"
    ## [3,] "3PL" "-784.349"
    ##      model LL        
    ## [1,] "1PL" "-765.127"
    ## [2,] "2PL" "-735.768"
    ## [3,] "3PL" "-773.32" 
    ##      model LL       
    ## [1,] "1PL" "-Inf"   
    ## [2,] "2PL" "-Inf"   
    ## [3,] "3PL" "-753.85"
    ##      model LL        
    ## [1,] "1PL" "-722.078"
    ## [2,] "2PL" "-710.498"
    ## [3,] "3PL" "-793.008"
    ##      model LL        
    ## [1,] "1PL" "-724.055"
    ## [2,] "2PL" "-715.593"
    ## [3,] "3PL" "-719.848"
    ##      model LL        
    ## [1,] "1PL" "-752.715"
    ## [2,] "2PL" "-740.673"
    ## [3,] "3PL" "-820.08"

    ## EM cycles terminated after 5000 iterations.

    ##      model LL        
    ## [1,] "1PL" "-Inf"    
    ## [2,] "2PL" "-Inf"    
    ## [3,] "3PL" "-855.635"
    ##      model LL        
    ## [1,] "1PL" "-759.691"
    ## [2,] "2PL" "-767.959"
    ## [3,] "3PL" "-804.88" 
    ##      model LL        
    ## [1,] "1PL" "-764.503"
    ## [2,] "2PL" "-765.891"
    ## [3,] "3PL" "-880.764"
    ##      model LL        
    ## [1,] "1PL" "-814.207"
    ## [2,] "2PL" "-775.881"
    ## [3,] "3PL" "-896.364"
    ##      model LL        
    ## [1,] "1PL" "-772.829"
    ## [2,] "2PL" "-770.413"
    ## [3,] "3PL" "-856.9"  
    ##      model LL        
    ## [1,] "1PL" "-770.496"
    ## [2,] "2PL" "-771.376"
    ## [3,] "3PL" "-841.163"
    ##      model LL        
    ## [1,] "1PL" "-744.248"
    ## [2,] "2PL" "-747.764"
    ## [3,] "3PL" "-816.651"
    ##      model LL        
    ## [1,] "1PL" "-791.949"
    ## [2,] "2PL" "-759.864"
    ## [3,] "3PL" "-825.279"
    ##      model LL        
    ## [1,] "1PL" "-800.662"
    ## [2,] "2PL" "-804.243"
    ## [3,] "3PL" "-900.11" 
    ##      model LL        
    ## [1,] "1PL" "-736.218"
    ## [2,] "2PL" "-726.515"
    ## [3,] "3PL" "-783.79"

    ## EM cycles terminated after 5000 iterations.

    ##      model LL        
    ## [1,] "1PL" "-756.869"
    ## [2,] "2PL" "-757.234"
    ## [3,] "3PL" "-923.643"
    ##      model LL        
    ## [1,] "1PL" "-760.115"
    ## [2,] "2PL" "-741.528"
    ## [3,] "3PL" "-848.203"
    ##      model LL        
    ## [1,] "1PL" "-724.157"
    ## [2,] "2PL" "-735.928"
    ## [3,] "3PL" "-786.042"

``` r
table = data.frame(
  "DGM" = "3PL",
  "Wins_1PL" = c1,
  "Wins_2PL" = c2,
  "Wins_3PL" = c3
)

print(table)
```

    ##   DGM Wins_1PL Wins_2PL Wins_3PL
    ## 1 3PL       20       20       10

``` r
#knitr::kable(table,
#             col.names = c('DGM', '1PL', '2PL', '3PL'))
```