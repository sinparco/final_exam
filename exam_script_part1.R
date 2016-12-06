#### Run Libraries / Load Data ####
library(tidyverse)
library(psych) 
library(haven)
library(apaTables)
library(pwr)
library(MBESS)  

my.data <- read_csv("exam_data_f16.csv")

#### Part 1 ####

#Incremental prediction (i.e., power analysis for interaction WITH agreeableness and contcientiousness)
## Assume sr2 = .02, assume R2 = .20
## f2 <- sr2 / (1-R2)

f2 <- .10 / (1 - .20) #F2 = 0.125
pwr.f2.test(u=2, f2=0.125, power=.85)

N = 2 + 72 + 2
print(N) #n=76




