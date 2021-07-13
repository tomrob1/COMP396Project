source('framework/data.R')
#source('notes/cointegrationzscore.R')

library(ggplot2)
library(grid)
library(gridExtra)
library(tseries)
library(xts)
library(egcm)
library(urca)
######################################################################################################################################
# data
######################################################################################################################################
dataList <- getData("PART1")
dataList2 <- getData("PART2")
dataList3 <- getData("PART3")
  series <- lapply(dataList[1:10], function(x) x$Open)
  series2 <- lapply(dataList2[1:10], function(x) x$Open)
  series3 <- lapply(dataList3[1:10], function(x) x$Open)
  drawdown <- lapply(series3, function(x) x[1:900])
modeldata <- cbind(series[[1]],series[[9]])

x <- series[[1]]
y <- series[[9]]
z <- series[[5]]

x2 <- series2[[1]]
y2 <- series2[[9]]
z2 <- series2[[5]]

x3 <- series3[[1]]
y3 <- series3[[9]]
z3 <- series3[[5]]

xd <- drawdown[[1]]
yd <- drawdown[[9]]

xALL <- c(x,x2,x3)
yALL <- c(y,y2,y3)
zALL <- c(z,z2,z3)

######################################################################################################################################
# ADF
######################################################################################################################################
adf.test(x)
ur.df(x, selectlags="AIC")
ur.df(x, selectlags="BIC")
summary(ur.df(x, selectlags="AIC"))
summary(ur.df(x, selectlags="BIC"))

adf.test(y)
ur.df(y, selectlags="AIC")
ur.df(y, selectlags="BIC")
summary(ur.df(y, selectlags="AIC"))
summary(ur.df(y, selectlags="BIC"))
######################################################################################################################################
# PP
######################################################################################################################################
summary(ur.pp(x, type="Z-tau", model = "trend", lags="short"))
summary(ur.pp(y, type="Z-tau", model = "trend", lags="short"))

pp.test(x)
pp.test(y)

######################################################################################################################################
# Regression
######################################################################################################################################
#Part 1  
  #Paul Teetor Method
  m <- lm(series[[1]] ~ series[[9]] + 0) # + 0?
  beta <- m$coefficients[1]
  cat ("Assumed hedge ratio is ", beta, "\n")
  sprd <- series[[1]] - beta * series[[9]]
  adf.test(sprd, alternative = 'stationary', k=0)$p.value #0.6612547
  adf.test(sprd, alternative = 'stationary') #0.5972
  
  m <- lm(series[[1]] ~ series[[9]] + 1)
  beta <- m$coefficients[1]
  cat ("Assumed hedge ratio is ", beta, "\n")
  sprd <- series[[1]] - beta * series[[9]]
  plot(sprd)
  #plot(rollma(sprd,60,5))
  adf.test(sprd, alternative = 'stationary', k=0)$p.value #0.6191655
  adf.test(sprd, alternative = 'stationary')$p.value #0.6053626
  
    m <- lm(series[[1]] ~ series[[9]] + 1)
    beta <- m$coefficients[2]
    cat ("Assumed hedge ratio is ", beta, "\n")
    sprd <- series[[1]] - beta * series[[9]]
    plot(sprd)
    #plot(rollma(sprd,60,5))
    adf.test(sprd, alternative = 'stationary', k=0)$p.value #0.3946879
    adf.test(sprd, alternative = 'stationary')$p.value #0.3257905
  
  model <- lm(series[[1]] ~ series[[9]])
  b <- model$coefficients[2]
  spreadp1 <- series[[1]] - b*series[[9]]
  plot(spreadp1)
  adf.test(spreadp1, k=0)$p.value #0.3946879
  adf.test(spreadp1)$p.value #0.3257905
  summary(ur.df(spreadp1, selectlags="AIC"))
  
    model <- lm(series[[1]] ~ series[[9]])
    b <- model$coefficients[1]
    spreadp1 <- series[[1]] - b*series[[9]]
    adf.test(spreadp1, k=0)$p.value #0.6191655
    adf.test(spreadp1)$p.value #0.6053626
  
  ##
  '9 ~1'
  ## +0 = 1 coefficient
  m <- lm(series[[9]] ~ series[[1]] + 0)
  beta <- m$coefficients[1]
  cat ("Assumed hedge ratio is ", beta, "\n")
  sprd <- series[[9]] - beta * series[[1]]
  plot(sprd)
  plot(rollma(sprd,60,5))
  adf.test(sprd, alternative = 'stationary', k=0)$p.value #0.6647128
  adf.test(sprd, alternative = 'stationary')$p.value #0.6058386
  
  m <- lm(series[[9]] ~ series[[1]] + 1)
  beta <- m$coefficients[1]
  cat ("Assumed hedge ratio is ", beta, "\n")
  sprd <- series[[9]] - beta * series[[1]]
  plot(sprd)
  #plot(rollma(sprd,60,5))
  adf.test(sprd, alternative = 'stationary', k=0)$p.value #0.5656023
  adf.test(sprd, alternative = 'stationary')$p.value #0.4772168
  
    m <- lm(series[[9]] ~ series[[1]] + 1)
    beta <- m$coefficients[2]
    cat ("Assumed hedge ratio is ", beta, "\n")
    sprd <- series[[9]] - beta * series[[1]]
    plot(sprd)
    #plot(rollma(sprd,60,5))
    adf.test(sprd, alternative = 'stationary', k=0)$p.value #0.4339312
    adf.test(sprd, alternative = 'stationary')$p.value #0.4151982
  
  model <- lm(series[[9]] ~ series[[1]])
  b <- model$coefficients[2]
  spreadp1 <- series[[9]] - b*series[[1]]
  plot(spreadp1)
  plot(rollma(spreadp1,60,5))
  adf.test(spreadp1, k=0)$p.value # 0.4339312
  adf.test(spreadp1, k=1)$p.value # 0.442621
  adf.test(spreadp1)$p.value # 0.4151982
  summary(ur.df(spreadp1, selectlags="AIC"))
  
    model <- lm(series[[9]] ~ series[[1]])
    b <- model$coefficients[1]
    spreadp1 <- series[[9]] - b*series[[1]]
    plot(spreadp1)
    plot(rollma(spreadp1,60,5))
    adf.test(spreadp1, k=0)$p.value # 0.5656023
    adf.test(spreadp1, k=1)$p.value # 0.5888712
    adf.test(spreadp1)$p.value # 0.4772168

#Part 2
  #Paul Teetor Method
  m2 <- lm(series2[[1]] ~ series2[[9]] + 0)
  beta2 <- m2$coefficients[1]
  cat ("Assumed hedge ratio is ", beta2, "\n")
  sprd2 <- series2[[1]] - beta2 * series2[[9]]
  adf.test(sprd2, alternative = 'stationary', k=0)$p.value
  
  model2 <- lm(series2[[1]] ~ series2[[9]])
  b2 <- model2$coefficients[2]
  spreadp2 <- series2[[1]] - b2*series2[[9]]
  adf.test(spreadp2, k=0)$p.value #0.02146518
  adf.test(spreadp2)$p.value #0.035
  
  ##
  '9 ~1'
  ##
  #Paul Teetor Method
  m2 <- lm(series2[[9]] ~ series2[[1]] + 0)
  beta2 <- m2$coefficients[1]
  cat ("Assumed hedge ratio is ", beta2, "\n")
  sprd2 <- series2[[9]] - beta2 * series2[[1]]
  plot(sprd2)
  plot(rollma(sprd2,60,5))
  adf.test(sprd2, alternative = 'stationary', k=0)$p.value #0.2314047
  
  #Paul Teetor Method
  m2 <- lm(series2[[9]] ~ series2[[1]] + 1)
  beta2 <- m2$coefficients[1]
  cat ("Assumed hedge ratio is ", beta2, "\n")
  sprd2 <- series2[[9]] - beta2 * series2[[1]]
  plot(sprd2)
  plot(rollma(sprd2,60,5))
  adf.test(sprd2, alternative = 'stationary', k=0)$p.value #0.01201012
  
  model2 <- lm(series2[[9]] ~ series2[[1]])
  b2 <- model2$coefficients[2]
  spreadp2 <- series2[[9]] - b2*series2[[1]]
  plot(spreadp2)
  plot(rollma(spreadp2,60,5))
  adf.test(spreadp2, k=0)$p.value #0.3823455
  adf.test(spreadp2)$p.value #0.3604492
  
#Part 3
  #Paul Teetor Method
  m3 <- lm(series3[[1]] ~ series3[[9]] + 0)
  beta3 <- m3$coefficients[1]
  cat ("Assumed hedge ratio is ", beta, "\n")
  sprd3 <- series3[[1]] - beta3 * series3[[9]]
  adf.test(sprd3, alternative = 'stationary', k=0)$p.value # 0.5656023
  
  model3 <- lm(series3[[1]] ~ series3[[9]])
  b3 <- model3$coefficients[2]
  spreadp3 <- series3[[1]] - b3*series3[[9]]
  adf.test(spreadp3, k=0)$p.value #0.03839774
  
  ##
  '9 ~1'
  ##
  #Paul Teetor Method
  m3 <- lm(series3[[9]] ~ series3[[1]] + 0)
  beta3 <- m3$coefficients[1]
  cat ("Assumed hedge ratio is ", beta, "\n")
  sprd3 <- series3[[9]] - beta3 * series3[[1]]
  plot(sprd3)
  plot(rollma(sprd3,60,5))
  adf.test(sprd3, alternative = 'stationary', k=0)$p.value #0.5594751
  

  m3 <- lm(series3[[9]] ~ series3[[1]] + 1) #-1 doesnt change
  beta3 <- m3$coefficients[1]
  cat ("Assumed hedge ratio is ", beta, "\n")
  sprd3 <- series3[[9]] - beta3 * series3[[1]]
  plot(sprd3)
  plot(rollma(sprd3,60,5))
  adf.test(sprd3, alternative = 'stationary', k=0)$p.value #0.4086628
  
  model3 <- lm(series3[[9]] ~ series3[[1]])
  b3 <- model3$coefficients[2]
  spreadp3 <- series3[[9]] - b3*series3[[1]]
  plot(sprd3)
  plot(rollma(sprd2,60,5))
  adf.test(spreadp3, k=0)$p.value #0.02012326

########################################################################################
## Computes the Phillips-Ouliaris test for the null hypothesis that x is not cointegrated - Phillips Peron?
########################################################################################
tseries::po.test(modelyx$model)$p.value
tseries::po.test(modelxy$model)$p.value

########################################################################################
## EGCM Package
# Augmented Dickey Fuller (ADF)                          
# Phillips-Perron (PP)                                   
# Pantula, Gonzales-Farias and Fuller (PGFF)                
# Elliott, Rothenberg and Stock DF-GLS (ERSD)            
# Johansen's Trace Test (JOT)                           
# Schmidt and Phillips Rho (SPR)                          
########################################################################################
plot(egcm(x,y))
plot(egcm(x2,y2))
plot(egcm(x3,y3))
plot(egcm(xd,yd))

plot(egcm(y,x))
plot(egcm(y2,x2))
plot(egcm(y3,x3))
plot(egcm(yd,xd))

plot(egcm(xALL, yALL))
plot(egcm(yALL, xALL))

plot(egcm(xALL,zALL))
plot(egcm(zALL,xALL))

summary(egcm(yALL,xALL))

summary(egcm(x,y))
summary(egcm(y,x))
egcm(x,y)
egcm(y,x)

summary(egcm(x2,y2))
summary(egcm(y2,x2))
egcm(x2,y2)
egcm(y2,x2)

summary(egcm(x3,y3))
summary(egcm(y3,x3))
egcm(x3,y3)
egcm(y3,x3)

summary(egcm(xd,yd))
summary(egcm(yd,xd))
egcm(xd,yd)
egcm(yd,xd)
