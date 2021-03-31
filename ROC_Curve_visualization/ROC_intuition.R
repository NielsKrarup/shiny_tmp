library(pROC)
library(tidyverse)
#install.packages("NNR", repos = "http://10.78.14.91/cran-internal-prod/latest")


# Standard roc curve ------------------------------------------------------


set.seed(123)
x  <- rnorm(10, mean = 0, sd = 5)
x_mid <- (sort(x)[1:9] + sort(x)[2:10])/2


y1 <- sample(0:1, 10, replace = T)
y2 <- rep(0, length = 10L)
#Cases for three smallest values of x
y2[order(x)[1:3]] <- 1
#only 1 case, center value
y3 <- rep(0, length = 10L)
y3[order(x)[8]] <- 1


#plot together, mfrow
par(mfrow = c(1,2))
plot(x,y1, ylim = c(0,10), pch = y1+1)
points(x,y2+3, col = 2, pch = y2+1)
points(x,y3+7, col = 3, pch = y3+1)

abline(v = x_mid, lty = 2)
df <- as.data.frame(cbind(x, y1, y2, y3))

median(x[y1 == 0]);median(x[y1 == 1])
r1 <- roc(response  = y1, predictor = x, smooth = FALSE, data = df)
sort(df$x)
sort(r1$predictor)
sort(r1$original.predictor)
sort(r1$thresholds)

str(r1)


median(x[y2 == 0]);median(x[y2 == 1])
r2 <- roc(response  = y2, predictor = x, smooth = FALSE, data = df)

median(x[y3 == 0]);median(x[y3 == 1])
r3 <- roc(response  = y3, predictor = x, smooth = FALSE, data = df)



plot(r1, main = "r1");points(r1$specificities, r1$sensitivities)
plot(r2, add = T, col =2, lty = 2);points(r2$specificities, r2$sensitivities, col = 2)
plot(r3, add = T, col =3, lty = 3);points(r3$specificities, r3$sensitivities, col = 3)

# Perfect separation AUC.CI -----------------------------------------------
#r2 had perfect sep.

ci.auc(r2)

#r3 has only one case, but not separated

ci.auc(r3)
auc(r3)

r1$direction
r1$controls;x[y1 == 0]
r1$cases;x[y1 == 1]
length(r1$thresholds)
x_mid
tr <- r1$thresholds
se <- r1$sensitivities
sp <- r1$specificities

SE_SP <- function(x,y = y1, thr = 1.475){
  SE <- sum(y[x < thr])/sum(y == 1)
  SP <- sum(y[x > thr] == 0)/sum(y == 0)
  c("SE" = SE, "SP" = SP)
}

my_se_sp <- sapply(X = r1$thresholds, FUN = function(t) SE_SP(x = x, y = y1, thr =t))
points(my_se_sp["SE",] ~ my_se_sp["SP",], pch = 3, col = 1)

my_se_sp_2 <- sapply(X = x_mid, FUN = function(t) SE_SP(x = x, y = y2, thr =t))
points(my_se_sp_2["SE",] ~ my_se_sp_2["SP",], pch = 3, col = 2)


# Same ROC Curve for pred vs log reg --------------------------------------
tb <- tibble(x = rnorm(100),
             y = rbinom(n = 100, size = 1, prob = pnorm(x)))
plot(y ~ x, data  = tb, col = y+1)

roc_ <- roc(y ~ x, data = tb, col = tb$y + 1)
plot(roc_, print.thres = T, print.auc = T, print.auc.x = 0.2, print.auc.y = 0.1, col = 2)

roc_$percent

roc_$auc

logreg_ <- glm(y ~ x, family = "binomial", data = tb)
summary(logreg_)
roc_link <- roc(response = logreg_$data$y, 
                predictor =  predict(logreg_))
plot(roc_link, add = T , col = 2)
library(tidyverse)
install.packages("tidyverse")


# roc curves for given sub-populations  --------------------------------------------------------
rr <- roc(y ~ x, data = tb)
plot(rr,print.auc = T,  print.auc.x = 1, print.auc.y = 1)



roc_fun <- function(mean1 = 0, mean2 = 1, sd1 = 1, sd2 = 1, n1 = 100, n2 = 100){
  #Control; population 1
  x1 <- rnorm(n = n1, mean = mean1, sd = sd1)
  #Cases; population 2
  x2 <- rnorm(n = n2, mean = mean2, sd = sd2)
  data <- data.frame(x = c(x1,x2), 
                     y = c(rep(0, n1), rep(1, n2)))
  #roc 
  roc <- pROC::roc(formula = y ~ x, data = data)
  youden <- coords(
    roc = roc,
    x = 'best',
    transpose = TRUE
  )
  
  youden <- if(is.null(dim(youden))) youden else youden[,1]
  
  youden
  par(mfrow = c(2,1))
  plot(roc, print.auc = T, print.thres = T, main = paste0("m1 = ", mean1, ". m2 = ", mean2))
  points( x = youden["specificity"], y = youden["sensitivity"], pch = 2, col = 2 )
  
  # % of cases having x > threshols  and rev
  SE <- nrow( data[data$y == 1 & data$x > as.numeric(youden["threshold"]),] ) / 
          nrow(data[data$y == 1,])
  omSE <- nrow( data[data$y == 1 & data$x < as.numeric(youden["threshold"]),] ) / 
    nrow(data[data$y == 1,])
  # % of controls having x < threshold and rev
  SP <- nrow( data[data$y == 0 & data$x < as.numeric(youden["threshold"]),] ) / 
    nrow(data[data$y == 0,])
  omSP <- nrow( data[data$y == 0 & data$x > as.numeric(youden["threshold"]),] ) / 
    nrow(data[data$y == 0,])
  
  plot(y ~ x, data = data, col = y+1, 
       main = paste("Number of 1's having x > threshols:", "(",youden["threshold"] %>% round(3), ")" , "is:", SE, "\n",
                    "Number of 0's having x < threshols:", "(",youden["threshold"] %>% round(3), ")" , "is:", SP, "\n")
       )
  abline( v = youden["threshold"])

}
roc_fun(mean2 = 1, sd1 = 0.1)
