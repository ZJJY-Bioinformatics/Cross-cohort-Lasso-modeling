#Lasso-leave-one-out #
setwd("C:/Users/86156/Desktop/CRC_BGC")
library(tidyverse)
library(readxl)
#library(writexl)
library("glmnet", warn.conflicts = F, quietly=T)
library("caret", warn.conflicts = F, quietly=T)
library("vip", warn.conflicts = F, quietly=T)
library("pROC", warn.conflicts = F, quietly = T)

file.list = list.files(pattern = "*.xlsx")
for(i in file.list) {
  assign(sub(".xlsx", "", i), read_excel(i))
}   # 读入并命名

#定义cohort#
list.cohort <- rbind(ccAustria,ccChina,ccUSA)
names<-c("ccAustria","ccChina","ccUSA")

#定义n-fold#
n.fold <- 10

#Preallocate local results collectors across cv folds
c.res <- list()
c.res$predict <- c.res$coef <- c.res$metric <- c.res$roc <- tibble()

for (ncohort in names) {   
  cohort=list.cohort[list.cohort$cohort != ncohort,]
  resp <- cohort [ ,3]%>% names()
  BGC<-cohort[,4:ncol(cohort)]%>% names()
  list.var <- list(BGC=BGC)
  
  for (k in 1:10) { 
    if (str_detect(resp, "frac")) {
      family <- "gaussian"
    } else {
      family <- "binomial"
    }
    
    #Roll the dice, stratified by target response variable
    
    if (str_detect(resp, "frac")) {
      cv.folds <- createFolds(cohort[[resp]], k = 10)
    } else {
      cv.folds <- createFolds(as.factor(cohort[[resp]]), k = 10)
    }
    
    
    for (fold in 1:n.fold) {  
      #Get current training and test sets
      c.train <- cohort[unlist(cv.folds[1:n.fold != fold]),] 
      c.test <- list.cohort[list.cohort$cohort==ncohort,]
      
      for (var.name in names(list.var)) {
        try(c.res <- run.lasso(
          c.vars=list.var[[var.name]],
          c.response=resp,
          set.train = c.train,
          set.test = c.test,
          family=family,
          var.name= var.name,
          fold = (k-1) * n.fold + fold,
          cohort=ncohort, 
          mcohort=ncohort,
          c.res = c.res
        ))
      }   #循环var#
    }     #循环n-fold#
  }      #循环迭代次数k#
}        #循环cohort#

#Store data
for (var.name in names(c.res)) {
  write.csv(c.res[[var.name]],paste('lasso_leave_one_out/',var.name,'.csv',sep=''))
}