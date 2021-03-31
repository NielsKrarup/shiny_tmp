library(tidyverse)
n1 <- 100
mean1 <- -1
sd1 <- 1

n2 <- 120
mean2 <- 1
sd2 <- 2

line <- 0.33


tb <- rbind(
        tibble(x = c(rnorm(n1, mean = mean1, sd = sd1)),
              response = rbinom(n = n1, size = 1, prob = pnorm(x))),
        tibble(x = c(rnorm(n2, mean = mean2, sd = sd2)),
               response = rbinom(n = n2, size = 1, prob = pnorm(x)))
        )
#tb$response <- factor(tb$response)
Colors <- c("1" = "grey", "0" = "white")
names(Colors)  
Colors[names(Colors)=="0"]

tb %>% mutate(predict = as.factor(x > line))

rep(c(1,2), each = c(10,2))
rep(1:3,times = 10)
rep(1:3,each = 10)
library("pROC")
tb <- tibble(x = rnorm(n = n1 + n2, mean = c(rep(mean1, n1), rep(mean2, n2)), sd = c(rep(sd1, n1), rep(sd2,n2))),
       response = rbinom(n = n1+n2, size = 1, prob = pnorm(x)),
       predict  = as.integer(1))

roc_obj <- pROC::roc(response ~ x, data = tb)
roc_obj2 <- pROC::roc(response ~ x, data = tb, direction = ">")

str(roc_obj)
par(mfrow = c(2,1))
plot(roc_obj)
plot(roc_obj2)
eval(1,  roc_obj$direction, 3)


eval(quote("1<2"))
substitute(paste0("1", roc_obj$thresholds, "3"))
plot(roc_obj)
roc_obj$direction
line <- -1
tb
x
tb %>% mutate(class = eval(parse(text = paste0("x", roc_obj$direction, line))))

#tb$response <- factor(tb$response)
Colors <- c("1" = "grey", "0" = "white")
names(Colors)  
Colors[names(Colors)=="0"]

p <- tb %>% ggplot(aes(x = x, fill = factor(response))) + geom_density(alpha = 0.3) + 
  scale_fill_manual(values = Colors) + 
  geom_vline(aes(xintercept=line), linetype="dashed")
p
d <- ggplot_build(p)$data[[1]]

p + geom_area(data = subset(d, x > line & fill == Colors[names(Colors)=="0"]), 
              aes(x=x, y=y), fill="red", alpha = 0.6) +
  geom_area(data = subset(d, x < line & fill == Colors[names(Colors)=="1"]), 
            aes(x=x, y=y), fill="orange", alpha = 0.6) 
p
