library(ggplot2)
library(corrplot)
library(gplots)
library(gains)
library(leaps)
library(caret)
library(MASS)

#Read the HR Dataset
hr.df <- read.csv("HR.csv", header = TRUE)


####################Prediction Model analysis####################

# Will the employee leave the company(Running GLM and LDA)

#Dataset for Logistic Regression
hr.logit <- hr.df[,5:30]

set.seed(13)
#Partitioning data into training (60%) and validation(40%) for logistic regression
train.index <- createDataPartition(hr.logit$left_Company , p = 0.6, list = FALSE)
train.df <-hr.logit[train.index,]
valid.df <- hr.logit[-train.index,]

#Logistic Regression for Leaving the company
lc<- glm(left_Company ~ ., data = train.df, family = "binomial")
options(scipen=999)
summary(lc)

#calculate e to the power coefficients
exp(coef(lc))

### Evaluate Performance of the Logit Model
### Predict propensities

pred <- predict(lc, valid.df[, -15], type = "response")

#Gains 
gain <- gains(valid.df$left_Company , pred, groups = 10)
gain

#Lift
plot(c(0,gain$cume.pct.of.total*sum(pred))~c(0,gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", main = "", type = "l")
lines(c(0,sum(pred))~c(0, dim(valid.df)[1]), lty = 5)

#decile chart and values
heights <- gain$mean.resp/mean(valid.df$left_Company)
midpoints <- barplot(heights, names.arg = gain$depth,  ylim = c(0,9), col = "blue",  
                     xlab = "Percentile", ylab = "Decile lift", 
                     main = "Decile-chart")
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

pred.scale <- ifelse( pred > 0.5, 1,0)

#Confusion Matrix
#confusionMatrix(data = pred.scale, reference = valid.df$left_Company)
confusiontable <- table(Predicted = as.numeric(pred.scale) , Actual =as.numeric(valid.df$left_Company))
confusiontable

#Accuracy of Logistic Regression on predicting if the employee will leave the company
mean(pred.scale==valid.df$left_Company)*100

#### --------------------------linear discriminant Regression------------------------------


lda1 <- lda(left_Company~., data = train.df, family="binomial")

# predict values
predict1 = predict(lda1, valid.df[,-c(15)], type = "response")
names(predict1)

# model Accuracy
table(predict=predict1$class, actual=valid.df$left_Company)
mean(predict1$class == valid.df$left_Company) 

#gains lift and decile
gain1 <- gains(valid.df$left_Company, predict1$posterior[,2], groups = 10)

### Plot Lift Chart
plot(c(0,gain1$cume.pct.of.total*sum(valid.df$left_Company))~c(0,gain1$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", main = "", type = "l")
lines(c(0,sum(valid.df$left_Company))~c(0, dim(valid.df)[1]), lty = 5)

### Plot decile-wise chart
Decile <- gain1$mean.resp/mean(valid.df$left_Company)
Decile1 <- barplot(Decile, names.arg = gain1$depth,  ylim = c(0,5), col = "red",  
                   xlab = "%ile", ylab = "Decile lift", 
                   main = "Decile Chart")
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

#--------------------------------------------B) What is the likelihood of employees getting a promotion?-----------------------------------------------


# Convert Category values to Factors
hr.df$salary <- factor(hr.df$salary, levels = c("high", "low", "medium"), 
                       labels = c(1, 3, 2))
hr.df$Gender <- factor(hr.df$Gender, levels = c("F", "M"), 
                       labels = c(0, 1))
hr.df$Role <- factor(hr.df$Role, levels = c("Director","Level 1","Level 2-4","Manager","Senior Director","Senior Manager","VP"),
                     labels = c(3,7,6,5,2,4,1))

#Convert Factors into Numeric
hr.df$salary = as.numeric(paste(hr.df$salary))
hr.df$Gender = as.numeric(paste(hr.df$Gender))
hr.df$Role = as.numeric(paste(hr.df$Role))

hrform.df <- hr.df[,-c(1,2,3,4,11)]


#Partitioning data into training (60%) and validation(40%) for linear regression on "Rising_Star"
train.lm.rs.index <- createDataPartition(hrform.df$Rising_Star , p= 0.6, list = FALSE)
train.linear.rs <-hrform.df[train.lm.rs.index,]
valid.linear.rs <- hrform.df[-train.lm.rs.index,]

# Linear Regression for Rising Star
hr.rise <- lm(Rising_Star ~ ., data = train.linear.rs)
summary(hr.rise)

pred.linear.rs <- predict(hr.rise, valid.linear.rs)

#Gains 
gain.linear.rs <- gains(valid.linear.rs$Rising_Star , pred.linear.rs, groups = 10)
gain.linear.rs

#Lift
plot(c(0,gain.linear.rs$cume.pct.of.total*sum(pred.linear.rs))~c(0,gain.linear.rs$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", main = "", type = "l")
lines(c(0,sum(pred.linear.rs))~c(0, dim(valid.linear.rs)[1]), lty = 5)

#decile chart and values
heights <- gain.linear.rs$mean.resp/mean(valid.linear.rs$Rising_Star)
midpoints <- barplot(heights, names.arg = gain.linear.rs$depth,  ylim = c(0,9), col = "blue",  
                     xlab = "Percentile", ylab = "Decile lift", 
                     main = "Decile-chart")
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

pred.linear.rs.round <- round(pred.linear.rs,0)

#Confusion Matrix
confusiontable.linear.rs <- table(Predicted = pred.linear.rs.round , Actual = valid.linear.rs$Rising_Star )
confusiontable.linear.rs

#Accuracy
mean(pred.linear.rs.round==valid.linear.rs$Rising_Star)


#-------------------------------------------C)	How much time will the employee spend in company?------------------------------------------------

#Linear Regression for time spend in company
set.seed(123)
#Partitioning data into training (60%) and validation(40%) for linear regression
train.lm.ts.index <- createDataPartition(hrform.df$time_spend_company , p= 0.6, list = FALSE)
train.linear.ts <-hrform.df[train.lm.ts.index,]
valid.linear.ts <- hrform.df[-train.lm.ts.index,]

hr_time.lm <- lm(time_spend_company ~ ., data = train.linear.ts )
summary(hr_time.lm)

pred.linear.ts <- predict(hr_time.lm, valid.linear.ts)

#gains
gain.linear.ts <- gains(valid.linear.ts$time_spend_company , pred.linear.ts, groups = 10)
gain.linear.ts

#Lift
plot(c(0,gain.linear.ts$cume.pct.of.total*sum(pred.linear.ts))~c(0,gain.linear.ts$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", main = "", type = "l")
lines(c(0,sum(pred.linear.ts))~c(0, dim(valid.linear.ts)[1]), lty = 5)

#decile chart and values
heights <- gain.linear.ts$mean.resp/mean(valid.linear.ts$time_spend_company)
midpoints <- barplot(heights, names.arg = gain.linear.ts$depth,  ylim = c(0,9), col = "blue",  
                     xlab = "Percentile", ylab = "Decile lift", 
                     main = "Decile-chart")
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

pred.linear.ts.round <- round(pred.linear.ts,0)

#Accuracy
mean(pred.linear.ts.round==valid.linear.ts$time_spend_company)

#---------------------------------------------D) How satisfied are the employees in company?----------------------------------------------
#Linear Regression for Employee Satisfaction
set.seed(123)
#Partitioning data into training (60%) and validation(40%) for linear regression
train.lm.es.index <- createDataPartition(hrform.df$Emp_Satisfaction , p= 0.6, list = FALSE)
train.linear.es <-hrform.df[train.lm.es.index,]
valid.linear.es <- hrform.df[-train.lm.es.index,]

hr_emp_sat.lm <- lm(Emp_Satisfaction ~ ., data = train.linear.es )
summary(hr_emp_sat.lm)

pred.linear.es <- predict(hr_emp_sat.lm, valid.linear.es)

#gains
gain.linear.es <- gains(valid.linear.es$Emp_Satisfaction , pred.linear.es, groups = 10)
gain.linear.es

#Lift
plot(c(0,gain.linear.es$cume.pct.of.total*sum(pred.linear.es))~c(0,gain.linear.es$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", main = "", type = "l")
lines(c(0,sum(pred.linear.es))~c(0, dim(valid.linear.es)[1]), lty = 5)

#decile chart and values
heights <- gain.linear.es$mean.resp/mean(valid.linear.es$Emp_Satisfaction)
midpoints <- barplot(heights, names.arg = gain.linear.es$depth,  ylim = c(0,9), col = "blue",  
                     xlab = "Percentile", ylab = "Decile lift", 
                     main = "Decile-chart")
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

pred.linear.es.round <- round(pred.linear.es,0)

#Accuracy
mean(pred.linear.es.round==valid.linear.es$Emp_Satisfaction)
mean(hrform.df$Emp_Satisfaction)

