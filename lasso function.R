#Define function to run LASSO regression and return eval results#
run.lasso <- function(
    c.vars,   #select variable x
    c.response,    #select variable y
    set.train = c.train,    #select train
    set.test = c.test,          #select test
    family = "binomial",        #define y type
    var.name,                   #output x category
    fold = fold,                #output fold and iteration
    cohort=cohort,              #output train cohort
    mcohort=mcohort,            #output test cohort
    c.res = c.res               #store result
) {
  #Get data
  c.train.X <- set.train %>% dplyr::select(c.vars) %>% mutate_all(~replace_na(., min(., na.rm=T))) %>% as.matrix.data.frame()
  c.test.X <- set.test %>% dplyr::select(c.vars) %>% mutate_all(~replace_na(., min(., na.rm=T))) %>% as.matrix.data.frame()
  resp.train <- set.train %>% pull(c.response)
  resp.test <- set.test %>% pull(c.response)
  
  #Run cv'ed LASSO regression
  c.lasso <- cv.glmnet(c.train.X, resp.train, alpha = 1, family = family)
  
  #Get standardised coefficients at lambda.1se using the Agresti method
  sds <- apply(c.train.X, 2, sd)
  cs <- as.matrix(coef(c.lasso, s = "lambda.min")) #"lambda.min", "lambda.1se"#
  std_coefs <- cs[-1, 1] * sds
  
  #Prepare output
  c.res[["coef"]] <- bind_rows(c.res[["coef"]], tibble(
    cohort= cohort,
    mcohort=mcohort,
    resp.var = c.response,
    cv.fold = fold,
    family = family,
    var.type = var.name,
    term = c.vars,
    coef = std_coefs
  ))
  
  #Evaluate model fit and pass outputs
  c.predict.test <- as.numeric(predict(c.lasso, c.test.X, "lambda.min"))
  c.res[["predict"]] <- bind_rows(c.res[["predict"]], tibble(
    cohort=cohort,
    mcohort=mcohort,
    resp.var = c.response,
    cv.fold = fold,
    family = family,
    var.type = var.name,
    response = resp.test,
    predicted = c.predict.test
  ))
  
  if (family == "binomial") { 
    c.res[["metric"]] <- bind_rows(c.res[["metric"]], tibble(
      cohort=cohort,
      mcohort=mcohort,
      resp.var = c.response,
      cv.fold = fold,
      family = family,
      var.type = var.name,
      metric = "auc",
      non.trivial = var(c.predict.test) != 0,  #avoid to all variables were predict to one type # 
      value = metric_auc(resp.test, c.predict.test)
    ))
    
    #Pass ROC curve
    tmp.roc <- invisible(roc(response = resp.test, predictor = as.numeric(c.predict.test), ret = "coords"))
    c.res[["roc"]] <- bind_rows(c.res[["roc"]], tibble(
      cohort=cohort,
      mcohort=mcohort,
      resp.var = c.response,
      cv.fold = fold,
      family = family,
      var.type = var.name,
      sensitivity = tmp.roc$sensitivities,
      specificity = tmp.roc$specificities
    ))
    
    #Pass
    return(c.res)
  } else {
    #Pass metric
    c.res[["metric"]] <- bind_rows(c.res[["metric"]], tibble(
      cohort=cohort,
      mcohort=mcohort,
      resp.var = c.response,
      cv.fold = fold,
      family = family,
      var.type = var.name,
      metric = c("rho", "rsq", "mse", "rmse"),
      non.trivial = var(c.predict.test) != 0,
      value = c(
        cor(resp.test, c.predict.test),
        metric_rsquared(resp.test, c.predict.test),
        metric_mse(resp.test, c.predict.test),
        metric_rmse(resp.test, c.predict.test)
      )
    ))
    
    #Pass
    return(c.res)
  }
}


