setwd("C:/Users/86156/Desktop/CRC_BGC")
library(tidyverse)
library(readxl)
#library(writexl)
library("glmnet", warn.conflicts = F, quietly=T)
library("caret", warn.conflicts = F, quietly=T)
library("vip", warn.conflicts = F, quietly=T)
library("pROC", warn.conflicts = F, quietly = T)
file.list <- list.files(pattern = "*.xlsx")
for(i in file.list) {
  assign(sub(".xlsx", "", i), read_excel(i))
}   # read the file and name


#Lasso-self validation#
#定义cohort#
list.cohort <- list(ccAustria=ccAustria,ccChina=ccChina,ccUSA=ccUSA)

#定义n-fold#
n.fold <- 10

#Preallocate local results collectors across cv folds
c.res <- list()
c.res$predict <- c.res$coef <- c.res$metric <- c.res$roc <- tibble()
for (ncohort in names(list.cohort)) {   
  cohort=list.cohort[[ncohort]]
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
    
    for (fold in 1:n.fold) {  #循环n-fold#
      #Get current training and test sets
      c.test <- cohort[cv.folds[[fold]], ]
      c.train <- cohort[unlist(cv.folds[1:n.fold != fold]), ]
      
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
      }   #recycle var
    }     #recycle n-fold
  }      #recycle iteration k
}        #recycle cohort

#Lasso-cross-cohort validation#
for (ncohort in names(list.cohort)) {   
  cohort=list.cohort[[ncohort]]
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
      c.train <- cohort[unlist(cv.folds[1:n.fold != fold]), ]
      
      for (mcohort in names(list.cohort)[names(list.cohort)!=ncohort]) {                      #循环Cross-cohort#
        c.test <- list.cohort[[mcohort]]
        
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
            mcohort=mcohort,
            
            c.res = c.res
          ))
        }   #recycle var
      }   #recycle mcohort
    }     #recycle n-fold
  }      #recycle iteration k
}        #recycle cohort

#Store data
for (var.name in names(c.res)) {
  write.csv(c.res[[var.name]],paste('lasso_out_crc_cross/',var.name,'.csv',sep=''))
}