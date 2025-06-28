# Installing Packages (For Quantil Regression)
install.packages("quantreg")
install.packages("ggplot2")
install.packages("caret")


# Loading the packages
library(quantreg)
library(dplyr)
library(ggplot2)
library(caret)
library(POT)
library(gPdtest)
library(MASS)
library(ismev)
library(extRemes)
library(fitdistrplus)

# Model: Quantile Regression
dataset2 <- read.csv('days4614001.csv')
taus1 <- c(.05,.1,.25,.75,.9,.95,.99,.999)
X_daily <- dataset2$X
Y_daily <- dataset2$Rain.mm
Quan_fit1 <- rq(Y_daily~X_daily, tau= taus1, data = dataset2)
Quan_fit1
# Plot
plot(Y_daily~X_daily, data = dataset2, pch = 16, main = "Days vs Daily Rainfall",  ylab = "Rainfall (mm)", xlab = "Days", col=rgb(red = 1, green=0, blue=0, alpha = 0.2), cex =.5)
abline(lm(Y_daily~X_daily, data = dataset), col = 1, lty = 2,)
abline(rq(Y_daily~X_daily,tau= .05, data = dataset2), col = 2, lty=2)
abline(rq(Y_daily~X_daily,tau= .1, data = dataset2), col = 3, lty = 2)
abline(rq(Y_daily~X_daily,tau= .25, data = dataset2), col = 4, lty = 2)
abline(rq(Y_daily~X_daily,tau= .75, data = dataset2), col = 5, lty = 2)
abline(rq(Y_daily~X_daily,tau= .9, data = dataset2), col = 6, lty = 2)
abline(rq(Y_daily~X_daily,tau= .95, data = dataset2), col = 7, lty = 2)
abline(rq(Y_daily~X_daily,tau=.99, data = dataset2), col= 8, lty = 2)
abline(rq(Y_daily~X_daily,tau=.999, data = dataset2), col= 9, lty = 2)

#plot with monthly rainfall
taus <- c(.05,.1,.25,.75,.9,.95)
Quan_fit <- rq(y ~ X1, tau= taus, data = dataset)
Quan_fit

# Summary of Model
summary(Quan_fit)

# Plot
plot(y ~ X1, data = dataset, pch = 16, main = "Month vs Monthly Rainfall",  ylab = "Rainfall (mm)", xlab = "Month" )
abline(lm(y ~ X1, data = dataset), col = "red", lty = 2)
abline(rq(y ~ X1,tau= .05, data = dataset), col = "blue", lty = 2)
abline(rq(y ~ X1,tau= .1, data = dataset), col = "green", lty = 2)
abline(rq(y ~ X1,tau= .25, data = dataset), col = "yellow", lty = 2)
abline(rq(y ~ X1,tau=.5, data - dataset),col = "purple", lty = 2)
abline(rq(y ~ X1,tau= .75, data = dataset), col = "black", lty = 2)
abline(rq(y ~ X1,tau= .9, data = dataset), col = "grey", lty = 2)
abline(rq(y ~ X1,tau= .95, data = dataset), col = "purple", lty = 2)

#bloxplot
boxplot(dataset$Rain.mm~dataset$Month, main = "Boxplot", ylab = "Rain (mm)", xlab = "Month")

#Bayesian Binary QR
library(Brq)
dataset <- read.csv('year4614001.csv')
head(dataset)
dataset <- subset(dataset, select = -c(X) )
head(dataset)
tail(dataset)
predictors <- subset(dataset, select = -c(Rain.mm) )
predictors
x <- data.matrix(predictors)
x
X1 <- data.matrix(predictors[,2])
X1
y <- data.matrix(dataset[,3])
y
fit = Brq(y~x,tau=c(0.1,0.3,0.5,0.7,0.9), method="BLBqr",runs=11000, burn=1000)
fit1 <- BLBqr(X1,y, tau = 0.5, runs = 11000, burn = 1000)
BLBqr(x,y, tau = 0.5, runs = 11000, burn = 1000)
fit1$coefficients

summary(fit1)
model(fit1)
summary(fit)
model(fit)

for (i in 1:5) {
  plot(x, plottype=c("hist", "trace", "ACF", "traceACF", "histACF","tracehist",
                     "traceACFhist"),Coefficients=fit$coefficients[,i],breaks=30,lwd=1,col1=0,col2=1,col3=1,col4=1)
}
boxplot(dataset$Rain.mm~dataset$Year)
boxplot(dataset$Rain.mm~dataset$Month)



fit$coefficients

# Quantile Regression

# Actual Rainfall data
dataset2014 <- dataset[dataset$Year == 2014,]
df1<- read.csv("2014_4717001.csv")
df2<- read.csv("2014_4923001.csv")
df3 <- read.csv("year4819027.csv")
df4<- read.csv("2014_5120025.csv")
df5<- read.csv("2014_5216001.csv")
df6<- read.csv("2014_5320038.csv")
df7<- read.csv("2014_5322044.csv")
df8<- read.csv("2014_5419036.csv")
df9<- read.csv("2014_5422046.csv")
df10<- read.csv("2014_5520001.csv")
df11<- read.csv("2014_5621051.csv")
df12<- read.csv("2014_5722057.csv")
df3 <- df3[df3$Year == 2014,]
df3

# GCM Prediction output 
GCM_data <- read.csv("GCM output.csv")
head(GCM_data)
GCM_data_Kelantan <- GCM_data[GCM_data$State == "Kelantan", ]
head(GCM_data_Kelantan)
GCM_data_Kelantan_2014 <- GCM_data_Kelantan[GCM_data_Kelantan$Year == 2014, ]
GCM_data_Kelantan_2014

# Plot GCM rainfall output with days
days <- seq(1,365,1)
GCM_data_Kelantan_2014$Days <- days
plot(GCM_data_Kelantan_2014$Rainfall..mm.~GCM_data_Kelantan_2014$Days, xlab = "Days", ylab = "Rainfall (mm)")
y <- GCM_data_Kelantan_2014$Rainfall..mm.
X <- GCM_data_Kelantan_2014$Days

# Heterodecasticity
lm_mod <- lm(y ~ X)
plot(lm_mod)
mod_resi <- lm_mod$residuals
data_new_Kelantan <- GCM_data_Kelantan_2014
data_new_Kelantan$resi <- mod_resi
ggplot(data = data_new_Kelantan, aes(y = resi, x = Days))+geom_point(col = 'blue')+
  geom_abline(slope = 0) + labs(y = "Residuals", x = "Days")

abline(rq(y ~ X,tau= .75), col = "blue", lty = 2)
abline(rq(y ~ X,tau= .90), col = "red", lty = 2)
abline(rq(y ~ X,tau= .95), col = "green", lty = 2)
summary(rq(y~X,tau=.75))
summary(rq(y~X,tau=.90))
summary(rq(y~X,tau=.95))


# GCM with months
month <- seq(1,12,1)
mon_matrix <- matrix(nrow = 1, ncol = 12)

for (mon in month) {
  
  rainfall <- GCM_data_Kelantan_2014[which(GCM_data_Kelantan_2014[,3] == mon), 5 ]
  sum_mon <- sum(rainfall)
  mon_matrix [1, mon] <- sum_mon
  sum_mon <- 0
  
}

mon_matrix
mon_matrix <- t(mon_matrix)
month <- t(month)
month
GCM_monthly <- data.frame(month, mon_matrix)
names(GCM_monthly) <- c("Month", "Rainfall(mm)")
View(GCM_monthly)



y_mon <- GCM_monthly$`Rainfall(mm)`
X_mon <- GCM_monthly$Month
plot(y_mon ~ X_mon)
abline(rq(y_mon ~ X_mon,tau= .75), col = "blue", lty = 2)
abline(rq(y_mon ~ X_mon,tau= .90), col = "red", lty = 2)
abline(rq(y_mon ~ X_mon,tau= .95), col = "green", lty = 2)

# Quantile model + predictions
model_1 <- rq(y ~ X, tau = .75)
model_2 <- rq(y ~ X, tau = .90)
model_3 <- rq(y ~ X, tau = .95)
forecast_1 <- predict(model_1)
forecast_2 <- predict(model_2)
forecast_3 <- predict(model_3)
predicts <- data.frame(days, forecast_1, forecast_2, forecast_3)
names(predicts) <- c("Days", "Q.75", "Q.90", "Q.95")
predicts$Month <- GCM_data_Kelantan_2014$Month

mon_quant <- matrix(nrow = 3, ncol = 12)
predicts
View(predicts)

quan_seq <- seq(2,4,1)

for (i in quan_seq) {
  
  for (mon in month) {
    rainfall <- predicts[which(predicts[,5] == mon), i]
    sum_mon <- sum(rainfall)
    mon_quant[i - 1, mon] <- sum_mon
    sum_mon <- 0
    
  }
  
}

# Plot the predictions + Actual in scatter plots
mon_quant
mon_quant <- t(mon_quant)
monthly_rainfall <- data.frame(df12$Rain.mm , mon_quant)
names(monthly_rainfall) <- c("Actual", "Q.75", "Q.90", "Q.95")
plot(monthly_rainfall$Actual, ylim = c(0,1700) )
points(monthly_rainfall$Q.75, col =2)
points(monthly_rainfall$Q.90, col = 3)
points(monthly_rainfall$Q.95, col = 4)
write.csv(monthly_rainfall, file = "Monthly_5722057.csv")

#================================================
monthly_rainfall_without_Actual <- subset(monthly_rainfall, select = -c(Actual))
monthly_rainfall_without_Actual <- t(monthly_rainfall_without_Actual)
boxplot(monthly_rainfall_without_Actual)
quantile(monthly_rainfall_without_Actual)

dataset_indays <- read.csv("Latest_New_Days_4614001.csv")
predicts_copy <- predicts
dataset2014_day <- dataset_indays[dataset_indays$Year == 2014,]
View(dataset2014_day)
dim(dataset2014_day)
predicts_copy$Actual <- dataset2014_day$Rain.mm

# Change to 0 and 1
predicts_copy <- predicts_copy %>% 
  mutate(Q.75_cat = case_when(predicts_copy$Q.75 < predicts_copy$Actual ~ 1,
                              predicts_copy$Q.75 >= predicts_copy$Actual ~ 0))
predicts_copy <- predicts_copy %>% 
  mutate(Q.90_cat = case_when(predicts_copy$Q.90 < predicts_copy$Actual ~ 1,
                              predicts_copy$Q.90 >= predicts_copy$Actual ~ 0))
predicts_copy <- predicts_copy %>% 
  mutate(Q.95_cat = case_when(predicts_copy$Q.95 < predicts_copy$Actual ~ 1,
                              predicts_copy$Q.95 >= predicts_copy$Actual ~ 0))
predicts_copy <- subset(predicts_copy, select = -c(Q.75, Q.90, Q.95))
predicts_copy
glm.fit <- glm(Q.75_cat ~ Days, data = predicts_copy, family = binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, type = "response")
glm.probs

# Fitting Quantile Regression on Actual Rainfall
X_monthly <- dataset$Month
Y_monthly <- dataset$Rain.mm
plot(Y_monthly ~ X_monthly)
abline(rq(Y_monthly  ~ X_monthly,tau= .75), col = "blue", lty = 2)
abline(rq(Y_monthly  ~ X_monthly,tau= .90), col = "red", lty = 2)
abline(rq(Y_monthly  ~ X_monthly,tau= .95), col = "green", lty = 2)

# Predictions
model_a <- rq(Y_monthly  ~ X_monthly, tau = .75)
model_b <- rq(Y_monthly  ~ X_monthly, tau = .90)
model_c <- rq(Y_monthly  ~ X_monthly, tau = .95)
forecast_a <- predict(model_a)
forecast_b <- predict(model_b)
forecast_c <- predict(model_c)
predicts_actual <- data.frame(dataset$Month, dataset$Rain.mm, forecast_a, forecast_b, forecast_c)
names(predicts_actual) <- c("Month", "Rain (mm)", "Q.75", "Q.90", "Q.95")
predicts_actual

# Change to 0 and 1
predicts_actual <- predicts_actual %>% 
  mutate(Q.75_cat = case_when(predicts_actual$Q.75 <= predicts_actual$`Rain (mm)` ~ 1,
                              predicts_actual$Q.75 > predicts_actual$`Rain (mm)` ~ 0))
predicts_actual <- predicts_actual %>% 
  mutate(Q.90_cat = case_when(predicts_actual$Q.90 <= predicts_actual$`Rain (mm)` ~ 1,
                              predicts_actual$Q.90 > predicts_actual$`Rain (mm)` ~ 0))
predicts_actual <- predicts_actual %>% 
  mutate(Q.95_cat = case_when(predicts_actual$Q.95 <= predicts_actual$`Rain (mm)` ~ 1,
                              predicts_actual$Q.95 > predicts_actual$`Rain (mm)` ~ 0))
predicts_actual <- subset(predicts_actual, select = -c(Q.75, Q.90, Q.95))
predicts_actual

# Fit and Predictions for Logistic Reg
glm.fit75 <- glm(Q.75_cat ~ Month, data = predicts_actual, family = binomial)
glm.probs75 <- predict(glm.fit75, type = "response")
glm.fit90 <- glm(Q.90_cat ~ Month, data = predicts_actual, family = binomial)
glm.probs90 <- predict(glm.fit90, type = "response")
glm.fit95 <- glm(Q.95_cat ~ Month, data = predicts_actual, family = binomial)
glm.probs95 <- predict(glm.fit95, type = "response")

summary(glm.fit)

# Check for quantile in actual rainfall data
quantile(dataset$Rain.mm, probs = c(0.75, .90, .95))
# 0.75 = 249.30, 0.90 = 338.00, 0.95 = 382.71
actual_copy <- dataset
actual_copy <- actual_copy %>% 
  mutate(Q.75_cat = case_when(249.30 <= actual_copy$Rain.mm ~ 1,
                              249.30 > actual_copy$Rain.mm ~ 0))
actual_copy <- actual_copy %>% 
  mutate(Q.90_cat = case_when(338.00 <= actual_copy$Rain.mm ~ 1,
                              338.00 > actual_copy$Rain.mm ~ 0))
actual_copy <- actual_copy %>% 
  mutate(Q.95_cat = case_when(382.71 <= actual_copy$Rain.mm ~ 1,
                              382.71 > actual_copy$Rain.mm ~ 0))
actual_copy
actual_copy <- subset(actual_copy, select = -c(X, Year))
actual_copy$Month <- as.factor(actual_copy$Month)
actual_copy

month
month_data <- as.data.frame(month)
names(month_data) <- c("Month")
month_data$Month <- as.factor(month_data$Month)

#Fit into logistic Reg
glm.fit75 <- glm(Q.75_cat ~ Month, data = actual_copy, family = binomial)
glm.probs75 <- predict(glm.fit75, type = "response", newdata = month_data)
glm.fit90 <- glm(Q.90_cat ~ Month, data = actual_copy, family = binomial)
glm.probs90 <- predict(glm.fit90, type = "response", newdata = month_data)
glm.fit95 <- glm(Q.95_cat ~ Month, data = actual_copy, family = binomial)
glm.probs95 <- predict(glm.fit95, type = "response", newdata = month_data)

glm.probs75
glm.probs90
glm.probs95
?as.factor

# Use Gua Musang dataset
dataset_GM <- read.csv('year4819027.csv')
dataset2014_GM <- dataset_GM[dataset_GM$Year == 2014,]

plot(dataset2014_GM$Rain.mm ~ dataset2014_GM$Month, ylim = c(0,1000))
points(monthly_rainfall$Q.75, col =2)
points(monthly_rainfall$Q.90, col = 3)
points(monthly_rainfall$Q.95, col = 4)

quantile(dataset_GM$Rain.mm, probs = c(0.75, .90, .95))
#0.75 = 263.70, 0.90 = 337.16, 0.95 = 385.90

actual_copy_GM <- dataset_GM
actual_copy_GM <- actual_copy_GM %>% 
  mutate(Q.75_cat = case_when(263.70 <= actual_copy_GM$Rain.mm ~ 1,
                              263.70 > actual_copy_GM$Rain.mm ~ 0))
actual_copy_GM <- actual_copy_GM %>% 
  mutate(Q.90_cat = case_when(337.16 <= actual_copy_GM$Rain.mm ~ 1,
                              337.16 > actual_copy_GM$Rain.mm ~ 0))
actual_copy_GM <- actual_copy_GM %>% 
  mutate(Q.95_cat = case_when(385.90 <= actual_copy_GM$Rain.mm ~ 1,
                              385.90> actual_copy_GM$Rain.mm ~ 0))
actual_copy_GM
actual_copy_GM <- subset(actual_copy_GM, select = -c(X, Year))
actual_copy_GM$Month <- as.factor(actual_copy_GM$Month)
plot(dataset_GM$Rain.mm~dataset_GM$Month)


month_data <- as.data.frame(month)
names(month_data) <- c("Month")
month_data$Month <- as.factor(month_data$Month)

#Fit into logistic Reg
glm.fit75_GM <- glm(Q.75_cat ~ Month, data = actual_copy_GM, family = binomial)
glm.probs75_GM <- predict(glm.fit75_GM, type = "response", newdata = month_data)
glm.fit90_GM <- glm(Q.90_cat ~ Month, data = actual_copy_GM, family = binomial)
glm.probs90_GM <- predict(glm.fit90_GM, type = "response", newdata = month_data)
glm.fit95_GM <- glm(Q.95_cat ~ Month, data = actual_copy_GM, family = binomial)
glm.probs95_GM <- predict(glm.fit95_GM, type = "response", newdata = month_data)

glm.probs75_GM
glm.probs90_GM
glm.probs95_GM

# Add Gua Musang dataset into brook
dataset_merge <- dataset
dataset_GM_B <- rbind(dataset_merge, dataset_GM)
dataset_GM_B
plot(dataset_GM_B$Rain.mm~dataset_GM_B$Month)
quantile(dataset_GM_B$Rain.mm, probs = c(0.75, .90, .95))
#.75 = 256.3, .90 = 337.2, .95 = 383.9

copy_GM_B <- dataset_GM_B
copy_GM_B <- copy_GM_B %>% 
  mutate(Q.75_cat = case_when(256.30 <= copy_GM_B$Rain.mm ~ 1,
                              256.30 > copy_GM_B$Rain.mm ~ 0))
copy_GM_B <- copy_GM_B %>% 
  mutate(Q.90_cat = case_when(337.20 <= copy_GM_B$Rain.mm ~ 1,
                              337.20 > copy_GM_B$Rain.mm ~ 0))
copy_GM_B <- copy_GM_B %>% 
  mutate(Q.95_cat = case_when(383.90 <= copy_GM_B$Rain.mm ~ 1,
                              383.90> copy_GM_B$Rain.mm ~ 0))
copy_GM_B
copy_GM_B <- subset(copy_GM_B, select = -c(X, Year))
copy_GM_B$Month <- as.factor(copy_GM_B$Month)

glm.fit75_GM_B <- glm(Q.75_cat ~ Month, data = copy_GM_B, family = binomial)
glm.probs75_GM_B <- predict(glm.fit75_GM_B, type = "response", newdata = month_data)
glm.fit90_GM_B <- glm(Q.90_cat ~ Month, data = copy_GM_B, family = binomial)
glm.probs90_GM_B <- predict(glm.fit90_GM_B, type = "response", newdata = month_data)
glm.fit95_GM_B <- glm(Q.95_cat ~ Month, data = copy_GM_B, family = binomial)
glm.probs95_GM_B <- predict(glm.fit95_GM_B, type = "response", newdata = month_data)

glm.probs75_GM_B
glm.probs90_GM_B
glm.probs95_GM_B

dataset_all_2014 <- read.csv("all_2014.csv")
quantile(copy_2014$Rain.mm, probs = c(0.75, .9, .95))
copy_2014
copy_2014 <- rbind(dataset2014, dataset2014_GM,dataset_all_2014)

copy_2014 <- copy_2014 %>% 
  mutate(Q.75_cat = case_when(289.05 <= copy_2014$Rain.mm ~ 1,
                              289.05 > copy_2014$Rain.mm ~ 0))
copy_2014 <- copy_2014 %>% 
  mutate(Q.90_cat = case_when(387.40 <= copy_2014$Rain.mm ~ 1,
                              387.40 > copy_2014$Rain.mm ~ 0))
copy_2014 <- copy_2014 %>% 
  mutate(Q.95_cat = case_when(1104.125 <= copy_2014$Rain.mm ~ 1,
                              1104.125 > copy_2014$Rain.mm ~ 0))

copy_2014 <- subset(copy_2014, select = -c(X, Year))
copy_2014$Month <- as.factor(copy_2014$Month)

glm.fit75_2014 <- glm(Q.75_cat ~ Month, data = copy_2014, family = binomial)
glm.probs75_2014 <- predict(glm.fit75_2014, type = "response", newdata = month_data)
glm.fit90_2014 <- glm(Q.90_cat ~ Month, data = copy_2014, family = binomial)
glm.probs90_2014 <- predict(glm.fit90_2014, type = "response", newdata = month_data)
glm.fit95_2014 <- glm(Q.95_cat ~ Month, data = copy_2014, family = binomial)
glm.probs95_2014 <- predict(glm.fit95_2014, type = "response", newdata = month_data)

glm.probs75_2014
glm.probs90_2014
glm.probs95_2014

# Extreme Rainfall analysis
x <- rgp(20, shape = 1)
dataset2$Rain.mm
dataset <- subset(dataset2, select = -c(X))
plot(dataset$Rain.mm)
#gpdmodel <- gpd.fit(dataset$Rain.mm,method = "amle")
quantile(dataset$Rain.mm, probs = c(0.9, 0.95))
model_1 <- fitgpd(data = dataset$Rain.mm, threshold = 338, est = "mle")
model_1$exceed
fit_gev <- gev.fit(model_1$exceed)
fit_gev$mle
qqnorm(model_1$exceed, pch = 1, frame = FALSE)
qqline(model_1$exceed, lwd = 2)

# New Extreme Value Distribution
daily_max <- read.csv("Annual_DailyMax4614001.csv")
percipitate <- daily_max$Rain.mm
fit_mle <- fevd(daily_max$Rain.mm, type = "GEV", method = "MLE")
plot(fit_mle)
fit_mle$results

fit_lmom <- fevd(daily_max$Rain.mm, type = "GEV", method = "Lmoments")
plot(fit_lmom)
fit_lmom$results

fit_GP <- fevd(as.vector(percipitate), type = "Gumbel", method = "MLE")
plot(fit_GP)
fit_GP$results

fit_gamma <- fitdist(percipitate, distr = "Beta", method = "mle")
plot(fit_gamma)

fit_weibull <- fitdist(percipitate, distr = "weibull", method = "mle")
plot(fit_weibull)

fit_lnorm <- fitdist(percipitate, distr = "lnorm", method = "mle")
plot(fit_lnorm)

fit_gnorm <- fitdist(percipitate, distr = "norm", method = "mle")
plot(fit_gnorm)
?fitdist
?d

write.csv(monthly_rainfall, file = "Monthly Rainfall.csv")
