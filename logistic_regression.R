# Part 1 - Logistic Regression
library(tidyverse)
library(ggthemes)
library(plotly)
library(Amelia)

# Bring in you data
df.train <- read.csv("titanic_train.csv")
df.test <-  read.csv("titanic_test.csv")

# you could have split the data using the below
#Split data into train and test sets
#set.seed(101) (so you can also replicate same results)
# Split the sample on the column you are trying to perdict
#70% training data 30% for testing
#sample <- sample.split(df$column, SplitRatio = 0.7)
# sample.split just assigns TRUE / FALSE to the ratio
# TRUE == 70% of data (Train)
#train <- subset(df, sample == TRUE)
# FALSE == 30% of data (Test)
#test <- subset(df, sample == FALSE)

#Explore the data
head(df.train)
str(df.train)

help("missmap")
#determine how much of the data is missing or NA
missmap(df.train, main = 'Missing Map', col = c('yellow', 'black'), legend = TRUE)

ggplot(df.train, aes(Survived)) + 
    geom_bar()
# 0 == people that did not survive 1 == people that did survive

ggplot(df.train, aes(Pclass)) + 
    geom_bar(aes(fill = factor(Pclass)))

ggplot(df.train, aes(Sex)) +
    geom_bar(aes(fill = factor(Sex)))

ggplot(df.train, aes(Age)) + 
    geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')

ggplot(df.train, aes(SibSp)) + 
    geom_bar()

ggplot(df.train, aes(Fare)) +
    geom_histogram(fill = 'green', color = 'black', alpha = 0.5)

# Fill in missing age by using average age by passanger class
pl <- ggplot(df.train, aes(Pclass, Age))
pl <- pl + geom_boxplot(aes(group = Pclass, fill = factor(Pclass), alpha = 0.4))
pl + scale_y_continuous(breaks = seq(min(0), max(80),2)) + theme_bw()

#now that we have visualized the ages lets build a function to create imputation of age based on class

impute_age <- function(age, class){
    out <- age
    for (i in 1:length(age)){
        if (is.na(age[i])){
          if (class[i] == 1){
                out[i] <- 37
            }else if (class[i] == 2){
                out[i] <- 29
            }else{
                out[i] <- 24
            }
        }else{
            out[i] <-  age[i]
        }
      }
    return(out)
}

# now pass in impute ages to fixed.ages function
fixed.ages <-  impute_age(df.train$Age, df.train$Pclass)

df.train$Age <- fixed.ages

#check for any na data again
missmap(df.train, main = 'Imputation Check', col = c('yellow', 'black'),legend = FALSE)

# Look the data one more time to determine what columns you want to use in the model
str(df.train)

# remove the columns you do not want
df.train <- df.train %>% 
    select(-PassengerId, -Name, -Ticket, -Cabin, -Parch)

head(df.train)
df.train$Survived <- as.factor(df.train$Survived)
df.train$Pclass <- as.factor(df.train$Pclass)
#df.train$Parch <- as.factor(df.train$Parch)
df.train$SibSp <- as.factor(df.train$SibSp)


str(df.train)

log.model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = df.train)

?glm

summary(log.model)
#*** low probablity of the varible not being significant

# Lets perdict off the train set by spliting 70/30

library(caTools)
set.seed(101)
split <- sample.split(df.train$Survived, SplitRatio = 0.7)
final.train <- subset(df.train, split == TRUE)
final.test <- subset(df.train, split == FALSE)

final.log.model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = final.train)
summary(final.log.model)

fitted.probabilities <-  predict(final.log.model, final.test, type = 'response') # type is equal to response because we are trying to predict whether the lived or died or 0 and 1

fitted.results <- ifelse(fitted.probabilities > 0.5, 1, 0) # short hand if statement, all this says is if fitted.probabilities is greater than .5 then assign it a one else assign it a zero. 

missClassError <-  mean(fitted.results != final.test$Survived)

#check the accuracy
accuaracy <- 1 - missClassError

#confusion matrix
table(final.test$Survived, fitted.probabilities > 0.5)



