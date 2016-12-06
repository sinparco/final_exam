#### Run Libraries / Load Data ####
library(tidyverse)
library(psych) 
library(haven)
library(apaTables)
library(pwr)
library(MBESS) 

#### Part 2 ####

## Cleaning Data
my.data <- read_csv("exam_data_f16.csv")

analytic.data <- my.data

#Labelling Data
categorical_variables <- select(analytic.data, gender)
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1, "Female"=2)
age <- analytic.data$age

#Creating Item Scales
agreeableness_items <- select (analytic.data, A1, A2, A3, A4, A5)
cont_items <- select (analytic.data, C1, C2, C3, C4, C5)
perf_items <- select (analytic.data, JP1, JP2, JP3, JP4, JP5)

#Fixing Bad Values
is_bad_value <- agreeableness_items<1 | agreeableness_items>6
agreeableness_items[is_bad_value] <- NA
is_bad_value <- cont_items<1 | cont_items>6
cont_items[is_bad_value] <- NA
is_bad_value <- perf_items<1 | perf_items>6
perf_items[is_bad_value] <- NA

#Fixing Inverted Items
agreeableness_items <- mutate(agreeableness_items, A1=7-A1)
cont_items <- mutate(cont_items, C4=7-C4)
cont_items <- mutate(cont_items, C5=7-C5)
perf_items <- mutate(perf_items, JP1=7-JP1)
perf_items <- mutate(perf_items, JP2=7-JP2)

#Obtaining Scale Scores
agreeableness <- psych::alpha(as.data.frame(agreeableness_items), check.keys=FALSE)$scores
conscientiousness <- psych::alpha(as.data.frame(cont_items), check.keys=FALSE)$scores
performance <- psych::alpha(as.data.frame(perf_items), check.keys=FALSE)$scores

#Combine into analytic_data
analytic_data <- cbind(categorical_variables, age, agreeableness, conscientiousness, performance)

#Saving .RData, CSV, .SAV 
save(analytic_data,file="final_analytic_data.RData")
write_csv(analytic_data,path="final_analytic_data.csv")



#### Correlation Table #### 

#Cor Table
apa.cor.table(analytic_data, filename="Table1_APA.doc", table.number=1)

# Confidence Intervals for Reliability

psych::alpha(as.data.frame(agreeableness_items), check.keys=FALSE)
psych::alpha(as.data.frame(cont_items), check.keys=FALSE)
psych::alpha(as.data.frame(perf_items), check.keys=FALSE)

#Testing Linearity 

pairs.panels(as.data.frame(analytic_data), lm=TRUE)

#### Multiple Regression ###

# Setting up subsets of data
final_men <- analytic_data %>% filter(gender=="Male") %>% select(-gender)
final_women <- analytic_data %>% filter(gender=="Female") %>% select(-gender)

# Run Regression
reg1 <- lm(performance ~ agreeableness + conscientiousness, data=analytic_data);summary(reg1)
apa.reg.table(reg1, filename="Table2_APA.doc", table.number=2)

reg2 <- lm(performance ~ agreeableness + conscientiousness, data=final_men);summary(reg2)
apa.reg.table(reg2, filename="Table3_APA.doc", table.number=3)

reg3 <- lm(performance ~ agreeableness + conscientiousness, data=final_women);summary(reg3)
apa.reg.table(reg3, filename="Table4_APA.doc", table.number=4)


