
selectWithBarsUI <- function(id, label = NULL, var_list = NULL)
{
  ns <- NS(id)
  tagList(
      selectInput(ns("sel"), label=label, var_list, selectize=TRUE),
      #textInput(ns("text"), label="Seloko leke",value="None"),
      uiOutput(ns("text"))
  )
}

median.impute = function(x){
  x = as.data.frame(x)
  for (i in 1:ncol(x)){
    x[which(x[,i]== -1),i] = NA
  }
  
  x = x %>% mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% as.data.table()
  return(x)
}

preproc <- function(data)
{
  median.impute(data)
}


lgb.normalizedgini = function(preds, dtrain){
  actual = getinfo(dtrain, "label")
  score  = NormalizedGini(preds,actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}


selectWithBarsModule <- function(input, output, session, df_train, df_test, perf = "target")
{
  df_train <- preproc(df_train)
  df_test <- preproc(df_test)
  
  df_test$target = NA
  data = rbind(df_train, df_test)
  
  varnames = setdiff(colnames(data), c("id", perf))
  
  train_sparse = Matrix(as.matrix(data[!is.na(target), varnames, with=F]), sparse=TRUE)
  test_sparse  = Matrix(as.matrix(data[is.na(target) , varnames, with=F]), sparse=TRUE)
  
  y_train  = data[!is.na(target),target]
  test_ids = data[is.na(target) ,id]
  
  lgb.train = lgb.Dataset(data=train_sparse, label=y_train)
  
  categoricals.vec = colnames(train)[c(grep("cat",colnames(train)))]
  
  lgb.grid = list(objective = "binary",
                  metric = "auc",
                  min_sum_hessian_in_leaf = 1,
                  feature_fraction = 0.7,
                  bagging_fraction = 0.7,
                  bagging_freq = 5,
                  min_data = 100,
                  max_bin = 50,
                  lambda_l1 = 8,
                  lambda_l2 = 1.3,
                  min_data_in_bin=100,
                  min_gain_to_split = 10,
                  min_data_in_leaf = 30,
                  is_unbalance = TRUE)
  
  # lgb.model.cv = lgb.cv(params = lgb.grid, 
  #                       data = lgb.train, 
  #                       learning_rate = 0.02, 
  #                       num_leaves = 25,
  #                       num_threads = 2 , 
  #                       nrounds = 7000, 
  #                       early_stopping_rounds = 50,
  #                       eval_freq = 20, 
  #                       eval = lgb.normalizedgini,
  #                       categorical_feature = categoricals.vec, 
  #                       nfold = 5, 
  #                       stratified = TRUE)
  # 
  # best.iter = lgb.model.cv$best_iter
  #best.iter = 525
  best.iter = 20
  
  # Train final model
  lgb.model = lgb.train(params = lgb.grid, 
                        data = lgb.train, 
                        learning_rate = 0.02,
                        num_leaves = 25, 
                        num_threads = 4 , 
                        nrounds = best.iter,
                        eval_freq = 20, 
                        eval = lgb.normalizedgini,
                        categorical_feature = categoricals.vec)
  
  tree_imp  <<- lgb.importance(lgb.model, percentage = TRUE)
  
  features <- sapply(tree_imp$Feature, tolower)
  
  gain <- tree_imp$Gain
  cover <- tree_imp$Cover
  frequency <- tree_imp$Frequency
  
  max_gain <- max(gain)
  
  #vars <- mapply(function(x, y) paste0(x," - ", as.character(round(y*100,2))), features, gain)
  
  createBar <- function(x, y)
  {
    paste0(#"<h5>",x,"</h5>",
           #create the bar
           "<span style=' width: 100%; display: flex;'> <span style='display: inline-block; border-radius: 3px; padding-right: 0; background-color: #00457C; width:", 
           as.character(round(y*12/max_gain,2)), 
           #set the with of the bar. I have an object with these proportions 
           "%; margin-right: 5px;'>&nbsp; </span> <span>", 
           as.character(round(y*100,1)), "% </span> </span>") #finally add the value behind the bar.
  }
  
  htmlify <- function(x){
    q <- quote(HTML(x))
    y <- parse(text=sub("x", deparse(x), deparse(q)))
    #print(y)
    y
  }
  
  observe({
    input$go
    #vars <- lapply(mapply(createBar, features, gain), htmlify)
    vars <- map(mapply(createBar, features, gain), htmlify)
    #print(vars[[3]])
    
    print("vars[1]")
    print(vars[1])
    print("vars[[1]]")
    print(vars[[1]])
    #print(apply(vars, eval)[1])
    
    updateSelectInput(session, "sel", 
                      choices = eval(vars[[1]]))  
  })
  
  output$text <- renderUI({
    input$go
    vars <-mapply(createBar, features, gain)
    HTML(as.character(vars[1]))
    #vars
  })
}