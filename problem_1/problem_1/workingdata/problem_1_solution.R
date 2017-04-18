## Chong Xing
## 2017-04-14
## Problem 1 Solution

setwd("/home/chong/GIT/Ticket-658-GRA_Training/exercises/problem_1/problem_1/workingdata")

## Reading in the Merged Working Data
dat_merged<- read.csv(file = "data_06-10.csv", header = TRUE)
head(dat_merged)
dim(dat_merged) # 344 by 125

## Reading in Separate Data
setwd("/home/chong/GIT/Ticket-658-GRA_Training/exercises/problem_1/problem_1/launch")

library(foreign)

dat_0607 <- read.spss(file = "CLT_1016Launch 06-07 PreK AllUSD.sav", use.value.labels = TRUE, to.data.frame = TRUE)
dim(dat_0607) # 60 by 110

dat_0708 <- read.spss(file = "CLT_1016Launch 07-08 PreK AllUSD.sav", use.value.labels = TRUE, to.data.frame = TRUE)
dim(dat_0708) # 60 by 110

dat_0809 <- read.spss(file = "CLT_1016Launch 08-09 PreK AllUSD.sav", use.value.labels = TRUE, to.data.frame = TRUE)
dim(dat_0809) # 76 by 110

dat_1011 <- read.spss(file = "CLT_1016Launch 10-11 PreK AllUSD.sav", use.value.labels = TRUE, to.data.frame = TRUE)
dim(dat_1011) # 149 by 111

## Stacking the Separate Data into One (Wickham, 2014, p.29)
dat_solu <- plyr::rbind.fill(dat_0607, dat_0708, dat_0809, dat_1011)
dim(dat_solu) # 345 by 111
summary(dat_solu)

## A multmerge() Function
multmerge = function(mypath) {
    filenames = list.files(path = mypath, full.names = TRUE)
    datalist = lapply(filenames, function(x){foreign::read.spss(file = x,
                      use.value.labels = TRUE, to.data.frame = TRUE)})
    Reduce(function(x, y) {plyr::rbind.fill(x, y)}, datalist)
}


