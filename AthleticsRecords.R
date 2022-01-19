data = read.table("WR_2021.txt",header = T)
men = data[1:7,]
women = data[8:14,]

# Exploratory stats
mean(men$time)
mean(women$time)
var(men$time)
var(women$time)
(var(women$time)-var(men$time))/var(women$time)*100
mae(var(men$time),var(women$time))
# Include an appropriate plot of data grouped on gender.
par(mfrow=c(1,1))
boxplot(data$time ~ data$sex,col=2:3, ylab = "Time (s)", xlab = "Gender", main = "Boxplots of world record times" )
par(mfrow=c(1,2))
plot(time ~ distance, group=sex,data=men, main = "Time vs Distance (Men)",xlab="distance (m)", ylab="time (s)", col=3,lwd=3)
plot(time ~ distance, group=sex,data=women, main = "Time vs Distance (Women)",xlab="distance (m)", ylab="time (s)", col=2,lwd=3)


# Linear Regression model
library(ggplot2)
ggplot(data) +
  aes(y = time, x = distance, color = sex) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm") +ggtitle("Interaction between gender") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(x=1900, y=370, label="0.1678*x (women) ",col=1) + geom_text(x=2400, y=300, label="0.1515*x (men) ",col=1)+  ggtitle("Linear Regression Models with 95% conf. interval") + theme(plot.title = element_text(hjust = 0.5))


# Interaction plot
interaction.plot(x.factor=data$distance,trace.factor = data$sex, response = data$time, trace.label = "Gender", col=1:2, main="Interactions beetween gender",xlab="distance",ylab="mean of the time")

L1 = lm(time ~ distance,data=data)
L2 = lm(time ~ distance*sex,data=data)
summary(L1)
anova(L1)
summary(L2)
anova(L2)

# Linear Regression Models

# Linear Regression men
L_men = lm(time ~ distance , data=men)
L_men
# Predictions men
preds_men = predict(L_men)
# Mean squared prediction error (men)
MSE_men = mean((men$time-preds_men)^2)
MSE_men


# Linear Regression women
L_women = lm(time ~ distance, data=women)
L_women
# Predictions for women
preds_women = predict(L_women)
# Mean squared prediction error (women)
MSE_women = mean((women$time-preds_women)^2);MSE_women

#Predict 600m

p = as.data.frame(600)
colnames(p) = "distance"
predict(L_men, p,interval="confidence",level=0.95)
predict(L_women,p,interval="confidence",level=0.95)


# Testing assumptions behind the LR model

qqnorm(data$time)
qqline(data$time)
plot(L_men$fitted.values, L_men$residuals, pch=19, cex=0.5,
     xlab="Fitted values", ylab="Residuals", main = "Residuals vs Fitted Values (Men)")
# Bootstrapping
time_sim_men = rnorm(1000, mean(men$time), sd(men$time))
dist_sim_men = rnorm(1000, mean(men$distance), sd(men$distance))
L_sim_men = lm(time_sim_men ~ dist_sim_men)

time_sim_women = rnorm(1000, mean(women$time), sd(women$time))
dist_sim_women = rnorm(1000, mean(women$distance), sd(women$distance))
L_sim_women = lm(time_sim_women ~ dist_sim_women)

par(mfrow=c(1,2))
qqnorm(L_sim_men$residuals, pch=19, cex=0.5, main = "Q-Q Plot (Men)")
qqline(L_sim_men$residuals)
plot(L_sim_men$fitted.values, L_sim_men$residuals, pch=19, cex=0.5,
     xlab="Fitted values", ylab="Residuals", main = "Residuals vs Fitted Values (Men)")

par(mfrow=c(1,2))
qqnorm(L_sim_women$residuals, pch=19, cex=0.5, main = "Q-Q Plot (Women)")
qqline(L_sim_women$residuals)
plot(L_sim_women$fitted.values, L_sim_women$residuals, pch=19, cex=0.5,
     xlab="Fitted values", ylab="Residuals", main = "Residuals vs Fitted Values (Women)")

