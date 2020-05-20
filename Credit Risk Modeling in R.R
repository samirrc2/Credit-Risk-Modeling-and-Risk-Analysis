loan<- loan_data_ch1

library(tidyverse)
library(dplyr)
library(gmodels) 
library(ggthemes)
library(ggplot2) 
library(readxl)
library(rpart.plot)
library(pROC)
library(gmodels) 

# understanding the data
tail(loan)
names(loan)
dim(loan)
str(loan)
summary(loan)

#EDA
ggplot() + geom_histogram(aes(x = loan$int_rate,fill=int_rate), binwidth=1,addMeanLine=TRUE,alpha=0.5,removePanelGrid=TRUE, fill = "red")+geom_col()
hist(loan$loan_amnt, ylim = c(0,8000),col=heat.colors(3))
barplot(table(loan$grade),ylim = c(0,9000),col=heat.colors(6))
par(mfrow=c(3,3))
boxplot(loan$loan_status,col = "red")
boxplot(loan$loan_amnt,col = "red")
boxplot(loan$emp_length,col = "red")
boxplot(loan$int_rate,col = "red")
boxplot(loan$annual_inc,col = "red")
boxplot(loan$age,col = "red")

ggplot(loan,aes(grade,int_rate))+geom_boxplot(aes(fill=grade))+theme(axis.text.x = element_blank())
ggplot(loan,aes(home_ownership,int_rate))+geom_boxplot(aes(fill=home_ownership))+theme(axis.text.x = element_blank())
ggplot(loan,aes(int_rate,fill=grade))+geom_density()+facet_grid(grade ~ .)

#Cross Table to Understand Data
CrossTable(loan$loan_status) #11% are loan defaulters
CrossTable(loan$grade, loan$loan_status, prop.r = TRUE, 
           prop.c = F, prop.t = F, prop.chisq = F)

#Spotting outliers using histogram and scatterplots
plot.new()
par(mfrow=c(1,1))
hist_1 <- hist(loan$loan_amnt)
hist_1$breaks
hist_2 <- hist(loan$loan_amnt, breaks = 200, xlab = "Loan Amount", 
               main = "Histogram of the loan amount")
plot(loan$age, ylab = "Age")

# Remove Outlier from the data

# Age
outlier_index <- which(loan$age > 122) 
loan2 <- loan[-outlier_index,]
plot(loan2$age)

# Annual Income
plot(loan$age, loan$annual_inc, xlab = "Age", ylab = "Annual Income")
outlier_index_ai <- which(loan$annual_inc == 6000000)
loan <- loan[-outlier_index_ai, ]
plot(loan$annual_inc)

#Missing Values replced with Median values 
summary(loan$int_rate)
na_index <- which(is.na(loan$int_rate))
#loan2 <- loan[-na_index, ]
#sum(is.na(loan2$int_rate))


median_ir <- median(loan$int_rate, na.rm = TRUE)
loan$int_rate[na_index] <- median_ir
summary(loan$int_rate)

summary(loan$emp_length)
na_index_emp <- which(is.na(loan$emp_length))
median_emp <- median(loan$emp_length, na.rm = TRUE)
loan$emp_length[na_index_emp] <- median_emp
summary(loan$emp_length)

#Correlation and Heat Map
cor(loan[sapply(loan, function(x) !is.factor(x))])

#data <- as.matrix(loan)
#heatmap(data)


#Apply Logistic Regression
set.seed(42)
index_train <- sample(1:nrow(loan), 2/3 * nrow(loan)) #2/3 of dataset
loan_train <- loan[index_train, ]
loan_test <- loan[-index_train, ]

lr_loan <- glm(loan_status~ age + int_rate + grade + loan_amnt +
                 annual_inc, family = "binomial", data = loan_train )
summary(lr_loan)

# Result Analysis
loan_predict <- predict(lr_loan, loan_test, type = "response")
range(loan_predict,na.rm = T)

lr_cutoff <- ifelse(loan_predict > 0.35, 1, 0)
tab_cm <- table(loan_test$loan_status, lr_cutoff)
tab_cm

# Testing Accuracy 
acc_logit <- sum(diag(tab_cm)) / nrow(loan_test)
acc_logit

#ROC curve and AUC 
roc_logit <- roc(loan_test$loan_status, loan_predict)
plot(roc_logit)
auc(roc_logit)

#Fancy Plots
ggplot(loan_test, aes(age, as.numeric(loan_status), color=home_ownership))  +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Age") + ylab("Pr (survived)")

graph <- ggplot(loan_test, aes(x=int_rate, y=loan_status)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE) +
  labs(x="Interest Rate", y="Percentage")+
  expand_limits(x=20)
graph
