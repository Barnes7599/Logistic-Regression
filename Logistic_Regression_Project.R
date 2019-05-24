library(tidyverse)
library(Amelia)
library(ggthemes)
library(caTools)
adult <- read.csv("adult_sal.csv")

head(adult)

adult <- adult %>% 
    select(-X)

str(adult)
summary(adult)

table(adult$type_employer)
#there  1836 nulls in type_employer and the smallest groups never-worked and without-pay

adult$type_employer <- as.character(adult$type_employer)

unemp <- function(job){
    if (job == 'Never-worked' | job == 'Without-pay') {
        return('Unemployed')
    }else {
        return(job)
    }
}

adult$type_employer <- sapply(adult$type_employer,unemp)

table(adult$type_employer)

group_emp <- function(job){
    if (job == 'Local-gov' | job == 'State-gov'){
        return('SL-gov')
    }else if (job == 'Self-emp-inc' | job == 'Self-emp-not-inc'){
        return('self-emp')
    }else {
        return(job)
    }
}

adult$type_employer <- sapply(adult$type_employer, group_emp)    

table(adult$type_employer)

table(adult$marital)


group_marital <- function(mar){
    mar <- as.character(mar)
    
    # Not-Married
    if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
        return('Not-Married')
        
        # Never-Married   
    }else if(mar=='Never-married'){
        return(mar)
        
        #Married
    }else{
        return('Married')
    }
}
adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)

table(adult$country)


Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
    if (ctry %in% Asia){
        return('Asia')
    }else if (ctry %in% North.America){
        return('North.America')
    }else if (ctry %in% Europe){
        return('Europe')
    }else if (ctry %in% Latin.and.South.America){
        return('Latin.and.South.America')
    }else {
        return('Other')
    }
}

adult$country <- sapply(adult$country, group_country)
table(adult$country)
str(adult)

# Turn the columns we changed back to factors
adult$country <- as.factor(adult$country)
# Or... adult$country <-  sapply(adult$country,factor)
adult$marital <- as.factor(adult$marital)
adult$type_employer <- as.factor(adult$type_employer)


# Lets deal with the missing data, first convert all the ? to NA
adult[adult == '?'] <- NA
table(adult$type_employer)

missmap(adult, col = c('yellow', 'black'), legend = TRUE, main = 'Missingness Map', y.at = c(1), y.labels = c(''))

# remove NAs now remember this may not always be the best decision

adult <- na.omit(adult)
str(adult)

# check to see if rows with the missing values have been removed
missmap(adult, col = c('yellow', 'black'), legend = TRUE, main = 'Missingness Map', y.at = c(1), y.labels = c(''))

ggplot(adult, aes(age)) +
    geom_histogram(aes( fill = income), color = 'black', binwidth = 1) + 
    theme_bw()

ggplot(adult, aes(hr_per_week)) + 
    geom_histogram() +
    theme_clean()

ggplot(adult, aes(region)) +
    geom_bar(aes(fill = income)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme_clean()

###############################
## LOGISTIC REGRESSION MODEL ##
###############################

head(adult)
adult <- rename(adult, region = country)

# TRAIN TEST SPLIT 
set.seed(101)
# CREATE SAMPLE
sample <- sample.split(adult$income, SplitRatio = 0.7)
# SPLIT TRAINING DATA
train <- subset(adult, sample == TRUE)
# SPLIT TEST DATA
test <- subset(adult, sample == FALSE)

### MODEL ###
model <- glm(income ~ ., family = binomial('logit'), data = train)

summary(model)
#### STEP function tries to eliminate varibales that are not contributing significance to the model. 
new.step.model <- step(model)
summary(new.step.model)


test$predicted.income <- predict(model, newdata = test, type = 'response')

table(test$income, test$predicted.income > 0.5)
# TN 6372
# FN 872
# FP 548
# TP 1423
#Actual Negative = TN + FP
#Actual Positive = FN + TP
#Predicted Negative = TN + FN
#Predicted Positive = FP + TP

### Recal vs Accuracy vs Percision
#https://towardsdatascience.com/accuracy-precision-recall-or-f1-331fb37c5cb9
#accuracy
#https://cdn-images-1.medium.com/max/800/1*7J08ekAwupLBegeUI8muHA.png
acc <- (6372 + 1423)/(6372 + 548 + 872 + 1423)
acc
# Recall: Recall actually calculates how many of the Actual Positives our model capture through labeling it as Positive (True Positive). Applying the same understanding, we know that Recall shall be the model metric we use to select our best model when there is a high cost associated with False Negative. For instance, in fraud detection or sick patient detection. If a fraudulent transaction (Actual Positive) is predicted as non-fraudulent (Predicted Negative), the consequence can be very bad for the bank. Similarly, in sick patient detection. If a sick patient (Actual Positive) goes through the test and predicted as not sick (Predicted Negative). The cost associated with False Negative will be extremely high if the sickness is contagious. https://cdn-images-1.medium.com/max/800/1*BBhWQC-m0CLN4sVJ0h5fJQ.jpeg

recall <- 6372/(6372 + 548)
# Precision: Precision talks about how precise/accurate your model is out of those predicted positive, how many of them are actual positive. Precision is a good measure to determine, when the costs of False Positive is high. For instance, email spam detection. In email spam detection, a false positive means that an email that is non-spam (actual negative) has been identified as spam (predicted spam). The email user might lose important emails if the precision is not high for the spam detection model. https://cdn-images-1.medium.com/max/800/1*PULzWEven_XAZjiMNizDCg.png
precision <- 6372/(6372 + 782)

# f1: F1 Score is needed when you want to seek a balance between Precision and Recall. Right…so what is the difference between F1 Score and Accuracy then? We have previously seen that accuracy can be largely contributed by a large number of True Negatives which in most business circumstances, we do not focus on much whereas False Negative and False Positive usually has business costs (tangible & intangible) thus F1 Score might be a better measure to use if we need to seek a balance between Precision and Recall AND there is an uneven class distribution (large number of Actual Negatives). https://cdn-images-1.medium.com/max/800/1*T6kVUKxG_Z4V5Fm1UXhEIw.png

f1 <- (precision * recall) / (precision + recall)
recall
precision
f1
