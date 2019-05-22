library(tidyverse)

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
adult$marital <- as.factor(adult$marital)
adult$type_employer <- as.factor(adult$type_employer)

