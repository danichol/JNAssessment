##########################################################################################

install.packages("dplyr")
install.packages("caTools") ##This package allows us to do the logistic regression#
install.packages("ROCR") ##This package allows us to evaluate the model ##
install.packages("ggplot2") #for data visualization
install.packages("writexl") ##writing the table to an xlsx file


library(dplyr)
library(readxl)
library(caTools)
library(ROCR)
library(ggplot2)
library(car)
library(cowplot)
library(pROC) #ROC and AUC curves
library(writexl)
library(corrplot)
library(RColorBrewer)
library(tidyr)

#import the training data to do do the logistic regression#

cs_training <- read_excel("C:/Users/danee/Dropbox/My PC (LAPTOP-ERU4CNJH)/Documents/Interview Assignment  Data Scientist - JN Group/cs-training.xlsx", 
                          sheet = "cs-training", col_types = c("numeric", 
                          "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "numeric", 
                          "numeric", "numeric"))

##################### EXPLORING THE DATA #####################
names(cs_training)
summary(cs_training)


#Checking data values for missing information
sum(is.na(cs_training)) #shows the total # of rows with missing data

#Replacing the NAs in Monthly Income using Median Imputation
MI_na_index <- which(is.na(cs_training$MonthlyIncome))
med_imptMI <- median(cs_training$MonthlyIncome, na.rm=TRUE)

training_noNA <- cs_training #copying the data set before changing anything

training_noNA$MonthlyIncome[MI_na_index] <- med_imptMI
summary(training_noNA$MonthlyIncome)

#Replacing the NAs in Number of Dependents with 0 assuming that it was left blank because respondents had no dependents
training_noNA["NumberOfDependents"][is.na(training_noNA["NumberOfDependents"])] <- 0


##################### VISUALIZE THE DATA #####################

#histograms

par(mfrow=c(2,2))
hist(training_noNA$age, col="#072AC8", main="Histogram of Age", xlab='Age (years)')
hist(training_noNA$NumberOfDependents, col="#072AC8", main="Histogram of Number of Dependents", xlab = "Number of Dependents")
hist(training_noNA$DebtRatio,col="#072AC8", main="Histogram of Debt Ratio", xlab="Debt Ratio (percentage)")
hist(training_noNA$MonthlyIncome, col="#072AC8", main="Histogram of Monthly Income", xlab = "Monthly Income (dollars)")

par(mfrow=c(1,3))
hist(training_noNA$RevolvingUtilizationOfUnsecuredLines, col="#072AC8", main="Histogram of Revolving Utilization of Unsecured Lines", xlab='Revolving Utilization of Unsecured Lines (percentage)')
hist(training_noNA$NumberOfOpenCreditLinesAndLoans, col="#072AC8", main="Histogram of Number of Open Credit Lines and Loans", xlab='Number of Open Credit Lines and Loans')
hist(training_noNA$NumberRealEstateLoansOrLines, col="#072AC8", main="Histogram of Number of Real Estate Loans or Credit Lines", xlab='Number of Real Estate Loans or Credit Lines')

par(mfrow=c(1,3))
hist(training_noNA$`NumberOfTime30-59DaysPastDueNotWorse`, col="#072AC8", main="Histogram of Number of Times Borrower has been between 30-59 Days Past Due ", xlab='Number of Times')
hist(training_noNA$`NumberOfTime60-89DaysPastDueNotWorse`, col="#072AC8", main="Histogram of Number of Times Borrower has been between 60 -89 Days Past Due ", xlab='Number of Times')
hist(training_noNA$NumberOfTimes90DaysLate, col="#072AC8", main="Histogram of Number Of Times Borrower Has Been 90 Days or More Past Due ", xlab='Number of Times')


#boxplots 
par(mfrow=c(1,2))
boxplot(training_noNA$age, main="Age", boxwex=0.5,col="#FFEE32")
boxplot(training_noNA$NumberOfDependents, main="Number of Dependents", boxwex=0.5, col="#FFEE32")

par(mfrow=c(1,2))
boxplot(training_noNA$DebtRatio, main= "Debt Ratio", boxwex=0.5, col="#FFEE32")
boxplot(training_noNA$MonthlyIncome, main = "Monthly Income", boxwex=0.5, col="#FFEE32")

par(mfrow=c(1,3))
boxplot(training_noNA$RevolvingUtilizationOfUnsecuredLines, main="Revolving Utilization", boxwex=0.5, col="#FFEE32")
boxplot(training_noNA$NumberOfOpenCreditLinesAndLoans, main = "Number of Credit Lines or Loans", boxwex=0.5, col="#FFEE32")
boxplot(training_noNA$NumberRealEstateLoansOrLines, main ="Number of Real Estate Lines or Loans", boxwex=0.5, col="#FFEE32")

par(mfrow=c(1,3))
boxplot(training_noNA$`NumberOfTime30-59DaysPastDueNotWorse`, main="30-59 Days Past Due", boxwex=0.5, col="#FFEE32")
boxplot(training_noNA$`NumberOfTime60-89DaysPastDueNotWorse`, main="60-89 Days Past Due", boxwex=0.5, col="#FFEE32")
boxplot(training_noNA$NumberOfTimes90DaysLate, main="90+ Days Past Due", boxwex=0.5, col="#FFEE32")


#Now we have to deal with the outliers#

##Debt Ratio##
DR_outlier_cutoff <- quantile(training_noNA$DebtRatio,0.75)+1.75*IQR(training_noNA$DebtRatio)
DR_index_outlier <- which(training_noNA$DebtRatio > DR_outlier_cutoff)
no_out_D <- training_noNA[-DR_index_outlier, ]

##DPD30##
D3_index_outlier <- which(no_out_D$`NumberOfTime30-59DaysPastDueNotWorse` > 25)
no_out_D3 <- no_out_D[-D3_index_outlier, ]


##DPD90##
D9_index_outlier <- which(no_out_D3$NumberOfTimes90DaysLate > 9)
no_out_D39 <- no_out_D3[-D9_index_outlier, ]

##Monthly Income##
MI_outlier_cutoff <- quantile(no_out_D39$MonthlyIncome,0.75)+1.75*IQR(no_out_D39$MonthlyIncome)
MI_index_outlier <- which(no_out_D39$MonthlyIncome > MI_outlier_cutoff)
no_out_D39M <- no_out_D39[-MI_index_outlier, ]

##Revolving Utilization##
RU_outlier_cutoff <- quantile(no_out_D39M$RevolvingUtilizationOfUnsecuredLines,0.75)+1.75*IQR(no_out_D39M$RevolvingUtilizationOfUnsecuredLines)
RU_index_outlier <- which(no_out_D39M$RevolvingUtilizationOfUnsecuredLines > RU_outlier_cutoff)
no_out_D39MR <- no_out_D39M[-RU_index_outlier, ]


training_no_outliers <- no_out_D39MR

#Renaming columns
colnames(training_no_outliers)[1]="ID"
colnames(training_no_outliers)[2]="def"
colnames(training_no_outliers)[3]="revutil"
colnames(training_no_outliers)[5]="dpd30"
colnames(training_no_outliers)[8]="credit_lines_loans"
colnames(training_no_outliers)[9]="dpd90"
colnames(training_no_outliers)[10]="real_estate_lines"
colnames(training_no_outliers)[11]="dpd60"
colnames(training_no_outliers)[12]="dependents"

attach(training_no_outliers)

#### NEW PLOTS NO OUTLIERS ####
#Histograms 
par(mfrow=c(2,2))
hist(training_no_outliers$age, col="#072AC8", main="Histogram of Age", xlab='Age (years)')
hist(training_no_outliers$dependents, col="#072AC8", main="Histogram of Number of Dependents", xlab = "Number of Dependents")
hist(training_no_outliers$DebtRatio,col="#072AC8", main="Histogram of Debt Ratio", xlab="Debt Ratio (percentage)")
hist(training_no_outliers$MonthlyIncome, col="#072AC8", main="Histogram of Monthly Income", xlab = "Monthly Income (dollars)")


par(mfrow=c(1,3))
hist(training_no_outliers$revutil, col="#072AC8", main="Histogram of Revolving Utilization of Unsecured Lines", xlab='Revolving Utilization of Unsecured Lines (percentage)')
hist(training_no_outliers$credit_lines_loans, col="#072AC8", main="Histogram of Number of Open Credit Lines and Loans", xlab='Number of Open Credit Lines and Loans')
hist(training_no_outliers$real_estate_lines, col="#072AC8", main="Histogram of Number of Real Estate Loans or Credit Lines", xlab='Number of Real Estate Loans or Credit Lines')

par(mfrow=c(1,3))
hist(training_no_outliers$dpd30, col="#072AC8", main="Histogram of Number of Times Borrower has been between 30-59 Days Past Due ", xlab='Number of Times')
hist(training_no_outliers$dpd60, col="#072AC8", main="Histogram of Number of Times Borrower has been between 60 -89 Days Past Due ", xlab='Number of Times')
hist(training_no_outliers$dpd90, col="#072AC8", main="Histogram of Number Of Times Borrower Has Been 90 Days or More Past Due ", xlab='Number of Times')

###boxplots

par(mfrow=c(1,2))
boxplot(training_no_outliers$age, main="Age", boxwex=0.5,col="#FFEE32")
boxplot(training_no_outliers$dependents, main="Number of Dependents", boxwex=0.5, col="#FFEE32")

par(mfrow=c(1,2))
boxplot(training_no_outliers$DebtRatio, main= "Debt Ratio", boxwex=0.5, col="#FFEE32")
boxplot(training_no_outliers$MonthlyIncome, main = "Monthly Income", boxwex=0.5, col="#FFEE32")

par(mfrow=c(1,3))
boxplot(training_no_outliers$revutil, main="Revolving Utilization", boxwex=0.5, col="#FFEE32")
boxplot(training_no_outliers$credit_lines_loans, main = "Number of Credit Lines or Loans", boxwex=0.5, col="#FFEE32")
boxplot(training_no_outliers$real_estate_lines, main ="Number of Real Estate Lines or Loans", boxwex=0.5, col="#FFEE32")

par(mfrow=c(1,3))
boxplot(training_no_outliers$dpd30, main="30-59 Days Past Due", boxwex=0.5, col="#FFEE32")
boxplot(training_no_outliers$dpd60, main="60-89 Days Past Due", boxwex=0.5, col="#FFEE32")
boxplot(training_no_outliers$dpd90, main="90+ Days Past Due", boxwex=0.5, col="#FFEE32")


###Correlation Plot######

corrplot(cor(training_no_outliers), method ="number",
         order = "alphabet", type="upper", col=brewer.pal(n=8, name="GnBu"), diag=FALSE)



####### CONVERTING DPDs into factor variables #####
ttf <- training_no_outliers

##changing over the dpd30 to if the customer did it or not, rather than the amount of times
ttf$dpd30 <- ifelse(ttf$dpd30 !=0 & ttf$dpd60==0 & ttf$dpd90 ==0, 1,0)
ttf$dpd60 <- ifelse(ttf$dpd60 !=0 & ttf$dpd90 ==0, 2,0)
ttf$dpd90 <- ifelse(ttf$dpd90 !=0, 3,0)

ttf$dpdfactor <- ttf$dpd30 + ttf$dpd60 +ttf$dpd90
ttf$dpdfactor <- as.factor(ttf$dpdfactor)

str(ttf)
table(ttf$dpdfactor)

plot(ttf$dpdfactor,main="Levels of 'dpdfactor'", xlab='dpdfactor',col="#072AC8")

##################### RUN THE MODEL #####################

## Logistic model ##

#model with dpd as a factor with levels#

logreg <- glm(def ~ revutil + age+ DebtRatio + MonthlyIncome + 
              credit_lines_loans  + real_estate_lines + dpdfactor + dependents, 
              data=ttf, family ="binomial")
summary(logreg)
confint(logreg)

logreg$coefficients #log odds
exp(logreg$coefficients) #odds

##################### PREDICT ON THE TEST DATA #####################

test_data <- read_excel("Interview Assignment  Data Scientist - JN Group/cs-test.xlsx", 
                       sheet = "cs-test", col_types = c("numeric", 
                       "numeric", "numeric", "numeric", 
                       "numeric", "numeric", "numeric", 
                       "numeric", "numeric", "numeric", 
                       "numeric", "numeric"))

summary(cs_test)

cs_test <- test_data

#Renaming columns to ensure test & training data have the same name#
colnames(cs_test)[1]="ID"
colnames(cs_test)[2]="def"
colnames(cs_test)[3]="revutil"
colnames(cs_test)[5]="dpd30"
colnames(cs_test)[8]="credit_lines_loans"
colnames(cs_test)[9]="dpd90"
colnames(cs_test)[10]="real_estate_lines"
colnames(cs_test)[11]="dpd60"
colnames(cs_test)[12]="dependents"

#Checking data values for missing information
sum(is.na(cs_test)) #shows the total # of rows with missing data

#Replacing the NAs in Monthly Income using Median Imputation
test_MI_na_index <- which(is.na(cs_test$MonthlyIncome))
test_med_imptMI <- median(cs_test$MonthlyIncome, na.rm=TRUE)

#test_noNA <- cs_test #copying the dataset before changing anything

cs_test$MonthlyIncome[test_MI_na_index] <- test_med_imptMI
summary(cs_test$MonthlyIncome)

#Replacing the NAs in Number of Dependents with 0 assuming that it was left blank because respondents had no dependents
cs_test["NumberOfDependents"][is.na(cs_test["NumberOfDependents"])] <- 0


#dpd factors as with training data
cs_test$dpd30 <- ifelse(cs_test$dpd30 !=0 & cs_test$dpd60==0 & cs_test$dpd90 ==0, 1,0)
cs_test$dpd60 <- ifelse(cs_test$dpd60 !=0 & cs_test$dpd90 ==0, 2,0)
cs_test$dpd90 <- ifelse(cs_test$dpd90 !=0, 3,0)

cs_test$dpdfactor <- cs_test$dpd30 + cs_test$dpd60 +cs_test$dpd90
cs_test$dpdfactor <- as.factor(cs_test$dpdfactor)

str(cs_test)


#run the predicted model to predict the pds

pred_data <- logreg %>% predict(cs_test, type = "response")


##########ROC CURVE

roc(ttf$def,logreg$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, 
    print.auc=TRUE, col="#213a5c", lwd=4)


#Bonus question
cs_test$def <- pred_data
##save bonus
write_xlsx(cs_test,"C:/Users/danee/Dropbox/My PC (LAPTOP-ERU4CNJH)/Documents/Interview Assignment  Data Scientist - JN Group/cs_test_bonus.xlsx")


### graph of predicted probabilities
predicted.data <- data.frame(prob_of_default=logreg$fitted.values, def=ttf$def)
predicted.data <- predicted.data[order(predicted.data$prob_of_default,decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)


pd <- ggplot(data=predicted.data, aes(x=rank, y=prob_of_default)) + geom_point(aes(color=ttf$def), alpha=1, stroke=2)
pd + labs(x="Index") +labs(y="predicted probabilities") + labs(title = "Graphing the Predicted Probabilites of Default")


#Linearity between predictors & logit ###Assumption of LR###

logit<- exp(logreg$fitted.values)
predictors <- logreg$linear.predictors

plot(logit,logodds, main="Testing the Linearity between Predictors and Logit",col="#072AC8") ##basic


